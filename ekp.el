;;; ekp.el --- Knuth-Plass line breaking with CJK support -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; Maintainer: Kinney Zhang <kinneyzhang666@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/Kinneyzhang/emacs-kp
;; Keywords: wp, text, typesetting, CJK
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of the Knuth-Plass optimal line breaking algorithm
;; with support for CJK text and hyphenation.
;;
;; Reference: Knuth & Plass, "Breaking Paragraphs into Lines" (1981)
;;
;; Usage:
;;   (ekp-pixel-justify "Your text here" 600)
;;   (ekp-pixel-range-justify "Text" 500 700)

;;; Code:

(require 'cl-lib)
(require 'ekp-utils)
(require 'ekp-hyphen)

;; Defined by the dynamic module (ekp_c/ekp.dylib | .so | .dll)
(declare-function ekp-c-set-penalties "ext:ekp")
(declare-function ekp-c-break-with-arrays "ext:ekp")
(declare-function ekp-c-break-batch "ext:ekp")

(defconst ekp--load-file (or load-file-name (buffer-file-name))
  "Path to this file, for locating dictionaries.")

(defgroup ekp nil
  "Knuth-Plass optimal line breaking with CJK support."
  :group 'text
  :prefix "ekp-"
  :link '(url-link "https://github.com/Kinneyzhang/emacs-kp"))

(defcustom ekp-latin-lang "en_US"
  "Language code for hyphenation (e.g., \"en_US\", \"de_DE\")."
  :type 'string
  :group 'ekp)

(defcustom ekp-use-c-module t
  "When non-nil, use C dynamic module for DP computation if available.
The C module provides significant performance improvement for large texts.
Set to nil to force pure Elisp implementation."
  :type 'boolean
  :group 'ekp)

;;;; Glue Parameters
;; Glue = flexible space between boxes (Knuth-Plass terminology)
;; lws = Latin Word Space, mws = Mixed (Latin-CJK), cws = CJK

(defvar ekp-lws-ideal-pixel nil "Ideal Latin word spacing (pixels).")
(defvar ekp-lws-stretch-pixel nil "Max stretch for Latin spacing.")
(defvar ekp-lws-shrink-pixel nil "Max shrink for Latin spacing.")
(defvar ekp-mws-ideal-pixel nil "Ideal mixed (Latin-CJK) spacing.")
(defvar ekp-mws-stretch-pixel nil "Max stretch for mixed spacing.")
(defvar ekp-mws-shrink-pixel nil "Max shrink for mixed spacing.")
(defvar ekp-cws-ideal-pixel nil "Ideal CJK character spacing.")
(defvar ekp-cws-stretch-pixel nil "Max stretch for CJK spacing.")
(defvar ekp-cws-shrink-pixel nil "Max shrink for CJK spacing.")

;; Derived limits (computed from above)
(defvar ekp-lws-max-pixel nil)
(defvar ekp-lws-min-pixel nil)
(defvar ekp-mws-max-pixel nil)
(defvar ekp-mws-min-pixel nil)
(defvar ekp-cws-max-pixel nil)
(defvar ekp-cws-min-pixel nil)

;;;; K-P Algorithm Parameters

(defcustom ekp-default-cws-stretch-pixel 2
  "Max stretched pixel of whitespace between CJK chars."
  :type 'natnum
  :group 'ekp)

(defcustom ekp-line-penalty 10
  "Penalty for each line break.  Higher = fewer lines.  Default 10."
  :type 'number
  :group 'ekp)

(defcustom ekp-hyphen-penalty 50
  "Penalty for hyphenated breaks.  Higher = avoid hyphenation.  Default 50.
Note: added to demerits as penalty², following the K-P formula."
  :type 'number
  :group 'ekp)

(defcustom ekp-adjacent-fitness-penalty 100
  "Penalty when adjacent lines differ in tightness by >1 class."
  :type 'number
  :group 'ekp)

(defcustom ekp-consecutive-hyphen-penalty 100
  "Base penalty multiplier for consecutive hyphenated lines.
Actual penalty = this × count², encouraging spread of hyphens."
  :type 'number
  :group 'ekp)

(defcustom ekp-last-line-short-penalty 50
  "Penalty multiplier for underfilled last lines.
Applied as: this × (1 - fill-ratio) when fill < `ekp-last-line-min-ratio'."
  :type 'number
  :group 'ekp)

(defcustom ekp-last-line-min-ratio 0.5
  "Minimum fill ratio for last line (0.0-1.0)."
  :type 'float
  :group 'ekp)

(defcustom ekp-alignment 'justify
  "Paragraph alignment mode.
`justify'      — flush both edges (default)
`ragged-right' — natural spacing, lines end ragged on the right
`ragged-left'  — natural spacing, lines start ragged on the left
`center'       — natural spacing, both edges share the leftover
Non-justify modes keep inter-word glue rigid; the K-P optimizer still
picks breaks that minimize raggedness within
`ekp-ragged-stretch-pixel' per line."
  :type '(choice (const :tag "Justify (flush both edges)" justify)
                 (const :tag "Ragged right" ragged-right)
                 (const :tag "Ragged left" ragged-left)
                 (const :tag "Center" center))
  :group 'ekp)

(defcustom ekp-ragged-stretch-pixel nil
  "Per-line end-of-line flexibility (pixels) for non-justify alignment.
This is what a ragged line may fall short of the target width without
badness reaching infinity (like \\raggedright with a finite \\rightskip
stretch).  nil derives 8× the Latin word-space ideal (≈2 em)."
  :type '(choice (const :tag "Auto (≈2 em)" nil) natnum)
  :group 'ekp)

(defcustom ekp-protrusion nil
  "Non-nil enables right-edge character protrusion (hanging punctuation).
A line ending in punctuation lets part of that glyph hang past the
flush edge, per `ekp-protrusion-ratios' — CLREQ line-end punctuation
squeeze and microtype-style hanging periods/hyphens in one mechanism.
Left-edge protrusion is not implemented: Emacs cannot render text
before the line origin.  When enabled, reserve the protrusion width
in the layout (see `ekp-region-protrusion-reserve')."
  :type 'boolean
  :group 'ekp)

(defcustom ekp-protrusion-ratios
  '((cjk-close . 0.5) (latin-close . 0.5) (hyphen . 1.0))
  "Alist CLASS → RATIO of the glyph width allowed to protrude.
`cjk-close': fullwidth closers (。、」); 0.5 hangs exactly the
whitespace half of the glyph — visually equivalent to CLREQ line-end
compression.  `latin-close': chars from `ekp--no-line-start-chars'
ending a word (period, comma, quotes).  `hyphen': the soft hyphen
inserted at a break."
  :type '(alist :key-type (choice (const cjk-close)
                                  (const latin-close)
                                  (const hyphen))
                :value-type float)
  :group 'ekp)

(defcustom ekp-parshape nil
  "Per-line layout, as a sequence of (INDENT . WIDTH) cons cells.
Line i (0-based) uses element i; lines beyond the last element reuse
it (like TeX \\parshape).  INDENT is the left offset in pixels,
WIDTH the text width — the rendered line occupies INDENT + WIDTH.
Line-number-dependent widths require the (position × line-count) DP,
so this is Elisp-only: the C module is bypassed while set."
  :type '(choice (const :tag "Off" nil) sexp)
  :group 'ekp)

(defcustom ekp-first-line-indent nil
  "First-line indentation: pixels, or t for 2 em of the paragraph font.
Sugar for the common CJK paragraph convention; ignored when
`ekp-parshape' is set."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "2 em" t)
                 natnum)
  :group 'ekp)

(defcustom ekp-looseness 0
  "Target line count offset: 0=optimal, +1=looser (more lines), -1=tighter.
When non-zero, a full (position × line-count) dynamic program is run
and the path whose line count is closest to (optimal + looseness) with
the lowest demerits is selected.  Only supported by the Elisp engine;
when non-zero the C module is bypassed automatically."
  :type 'integer
  :group 'ekp)

(defconst ekp--infinite-badness 10000
  "Badness value treated as infinitely bad (matches TeX).")

;;;; Paragraph Cache Structure
;;
;; All paragraph data is stored in a flat struct for O(1) access.

(cl-defstruct (ekp-para (:constructor ekp-para--create))
  "Preprocessed paragraph data."
  string latin-font cjk-font
  boxes boxes-widths boxes-types glues-types
  hyphen-pixel hyphen-positions
  ideal-prefixs min-prefixs max-prefixs
  ;; Per-position leading glue values (indexed by box, n elements)
  glue-ideals glue-shrinks glue-stretches
  ;; Prefix counts of each stretchable glue type (n+1 elements each);
  ;; entry i = number of that glue type among glue indices 0..i-1.
  lws-prefixs mws-prefixs cws-prefixs
  ;; Space-box run widths: lead-spaces[i] = total width of consecutive
  ;; space boxes starting at box i (forced to 0 at i=0 so that first-line
  ;; indentation is preserved); trail-spaces[k] = total width of
  ;; consecutive space boxes ending at box k-1.
  lead-spaces trail-spaces
  ;; Per-gap break permission: breaks-allowed[k] non-nil iff a line may
  ;; end after box k-1 (kinsoku, no-break spans).  n+1 bool-vector;
  ;; index n (paragraph end) is always allowed.  forbidden-positions is
  ;; the same information as a sparse int vector for the C bridge.
  breaks-allowed forbidden-positions
  ;; Right-edge protrusion: tail-protrudes[k] = pixels the last
  ;; non-space box before gap k may hang past the flush edge (all
  ;; zeros when `ekp-protrusion' is off); hyphen-protrude = same for
  ;; the soft hyphen at a hyphenated break.
  tail-protrudes hyphen-protrude
  ;; Lazily memoized (START . END) offsets of each box in the source
  ;; string (render-time lossless payloads); content-invariant.
  (box-offsets-memo nil)
  ;; Glue params snapshot at para creation time (plist)
  glue-params
  (dp-cache nil :type hash-table))

(defvar ekp--para-cache nil
  "Cache: equal-keyed table, content key → ekp-para struct.")

(defvar ekp--last-para nil
  "Fast path: (string-object lang para) of the most recent lookup.
One justification call resolves the same string object many times;
this avoids recomputing the full cache key each time.  Invalidated
by parameter changes, language changes and `ekp-clear-caches'.")

(defcustom ekp-para-cache-limit 256
  "Maximum number of cached paragraphs.
When exceeded, the whole paragraph cache is flushed (cheap to rebuild)."
  :type 'natnum
  :group 'ekp)

(defvar ekp--params-explicit nil
  "Non-nil after `ekp-param-set'; spacing params then persist until
`ekp-param-reset'.  When nil, defaults are derived from each string.")

;;;; Initialization
;; ekp-root-dir is provided by ekp-utils.el

(defun ekp--load-dicts ()
  "Register bundled hyphenation dictionaries, if the directory exists.
A missing directory (e.g., an incomplete install) only disables
hyphenation; it must not break loading the package."
  (let ((dir (expand-file-name "dictionaries" (ekp-root-dir))))
    (if (file-directory-p dir)
        (ekp-hyphen-load-languages dir)
      (lwarn 'ekp :warning
             "Dictionary directory %s not found; hyphenation disabled" dir))))

(ekp--load-dicts)

;;;; Parameter Management

(defun ekp--params-set-p ()
  "Return non-nil if all spacing parameters are set."
  (and ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
       ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
       ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))

(defun ekp--param-apply (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  "Set the nine spacing variables and derived limits (internal)."
  (setq ekp-lws-ideal-pixel lws-i ekp-lws-stretch-pixel lws-+
        ekp-lws-shrink-pixel lws-- ekp-mws-ideal-pixel mws-i
        ekp-mws-stretch-pixel mws-+ ekp-mws-shrink-pixel mws--
        ekp-cws-ideal-pixel cws-i ekp-cws-stretch-pixel cws-+
        ekp-cws-shrink-pixel cws--)
  (unless (ekp--params-set-p)
    (error "All spacing parameters must be non-nil"))
  (setq ekp-lws-max-pixel (+ lws-i lws-+) ekp-lws-min-pixel (- lws-i lws--)
        ekp-mws-max-pixel (+ mws-i mws-+) ekp-mws-min-pixel (- mws-i mws--)
        ekp-cws-max-pixel (+ cws-i cws-+) ekp-cws-min-pixel (- cws-i cws--))
  ;; Spacing changed: paragraphs must be re-resolved against it.
  (setq ekp--last-para nil))

(defun ekp-param-set (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  "Set all spacing parameters explicitly; they persist until `ekp-param-reset'.
LWS = Latin word space, MWS = mixed, CWS = CJK.
Each takes ideal, stretch (+), and shrink (-) values in pixels."
  (ekp--param-apply lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  (setq ekp--params-explicit t))

(defun ekp-param-set-default (string)
  "Compute and apply default spacing parameters based on STRING's font.
Does not mark parameters as explicit; each paragraph gets fresh defaults."
  (let* ((lws (max 1 (ekp-word-spacing-pixel string)))
         (mws (max 0 (- lws 1))))
    (ekp--param-apply lws (ceiling (/ (float lws) 2)) (ceiling (/ (float lws) 3))
                      mws (ceiling (/ (float mws) 2)) (ceiling (/ (float mws) 3))
                      0 ekp-default-cws-stretch-pixel 0)))

;;;###autoload
(defun ekp-param-reset ()
  "Clear explicit spacing parameters; defaults are derived per string again."
  (interactive)
  (setq ekp--params-explicit nil)
  (setq ekp-lws-ideal-pixel nil ekp-lws-stretch-pixel nil
        ekp-lws-shrink-pixel nil ekp-mws-ideal-pixel nil
        ekp-mws-stretch-pixel nil ekp-mws-shrink-pixel nil
        ekp-cws-ideal-pixel nil ekp-cws-stretch-pixel nil
        ekp-cws-shrink-pixel nil)
  (setq ekp--last-para nil))

;;;; Text Analysis

(defconst ekp--latin-regexp
  "[A-Za-z'\\-À-ÖØ-öø-ÿĀ-ɏḀ-ỿ]"
  "Regexp matching Latin characters including accented forms.")

(defconst ekp--word-left-punct "[({<„‚«‹¿¡*@\"'‘“"
  "Characters that may precede a hyphenatable Latin word.")

(defconst ekp--word-right-punct ")}>.,;:!?*\"'’”»›"
  "Characters that may follow a hyphenatable Latin word.
NB: `]' is included separately at the start of the character class.")

(defun ekp--split-with-hyphen (string)
  "Split STRING into boxes with hyphenation points marked.
Returns (boxes-vector . hyphen-positions-vector)."
  (let* ((boxes (ekp-split-to-boxes string))
         ;; NB: both punct sets are spliced into character classes;
         ;; they contain no chars that are special inside [...].
         (word-re (format "^\\([%s]*\\)\\(%s+\\)\\([]%s]*\\)$"
                          ekp--word-left-punct
                          ekp--latin-regexp
                          ekp--word-right-punct))
         ;; Resolved lazily on the first Latin word, at most once per
         ;; call; nil (no usable dictionary) just disables hyphenation.
         (hyphenator 'unset)
         (idx 0) new-boxes hyphen-idxs)
    (dolist (box (append boxes nil))
      (if (and (string-match word-re box)
               ;; Never hyphenate inside a no-break span (verbatim atoms)
               (null (text-property-not-all 0 (length box)
                                            'ekp-no-break nil box))
               (or (and (eq hyphenator 'unset)
                        (setq hyphenator
                              (condition-case nil
                                  (ekp-hyphen-create ekp-latin-lang)
                                (error nil))))
                   hyphenator))
          ;; Latin word: apply hyphenation
          (let* ((left (match-string 1 box))
                 (word (match-string 2 box))
                 (right (match-string 3 box))
                 (parts (ekp-hyphen-boxes hyphenator word))
                 (n (length parts)))
            (when (> (length left) 0)
              (setcar parts (concat left (car parts))))
            (when (> (length right) 0)
              (setcar (last parts)
                      (concat (car (last parts)) right)))
            (push parts new-boxes)
            (dotimes (i n)
              (when (< i (1- n)) (push idx hyphen-idxs))
              (cl-incf idx)))
        ;; Non-Latin: single box
        (push (list box) new-boxes)
        (cl-incf idx)))
    (cons (vconcat (apply #'append (nreverse new-boxes)))
          (vconcat (nreverse hyphen-idxs)))))

(defvar ekp--str-type-table (make-char-table 'ekp-str-type)
  "Per-character memo for `ekp--str-type' (a pure classification).")

(defun ekp--str-type (str)
  "Classify single-character string STR.
Returns one of `space', `latin', `cjk', `cjk-open', `cjk-close'.
`cjk-open' must not end a line; `cjk-close' must not start one
\(kinsoku) — enforced via `ekp-para-breaks-allowed'."
  (let ((c (aref str 0)))
    (or (aref ekp--str-type-table c)
        (aset ekp--str-type-table c (ekp--str-type-1 str)))))

(defun ekp--str-type-1 (str)
  "Uncached `ekp--str-type'."
  (cond
   ;; Whitespace or zero-width characters
   ((or (string-blank-p str) (= (string-width str) 0)) 'space)
   ;; a half-width cjk punct
   ((or (string= "“" str) (string= "”" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ;; Opening punctuation (Ps/Pi), e.g. 「『(《
   ((ekp-cjk-opening-punct-p str) 'cjk-open)
   ;; Closing/other full-width punctuation, e.g. 。、」!?
   ((ekp-cjk-fw-punct-p str) 'cjk-close)
   ;; double-width (or wider): CJK-like content, including emoji
   (t 'cjk)))

(defun ekp--box-edge-char (box from-end)
  "Return the first (or last, if FROM-END) visible char of BOX as a string.
Skips zero-width characters; falls back to the edge char."
  (let* ((len (length box))
         (idx (if from-end (1- len) 0))
         (step (if from-end -1 1)))
    (while (and (>= idx 0) (< idx len)
                (= (char-width (aref box idx)) 0))
      (setq idx (+ idx step)))
    (if (and (>= idx 0) (< idx len))
        (substring box idx (1+ idx))
      (substring box (if from-end (1- len) 0)
                 (if from-end len 1)))))

(defun ekp--box-type (box)
  "Return (START-TYPE . END-TYPE) for BOX, or nil for empty boxes."
  (unless (or (null box) (string-empty-p box))
    ;; Space/zero-width boxes: type is (space . space)
    (if (or (string-blank-p box) (= (string-width box) 0))
        '(space . space)
      (cons (ekp--str-type (ekp--box-edge-char box nil))
            (ekp--str-type (ekp--box-edge-char box t))))))

(defun ekp--glue-type (prev-box-type curr-box-type)
  "Glue type between boxes: `lws', `mws', `cws' or `nws'.
Lws means whitespace between latin words; cws between cjk chars;
mws between cjk and latin; nws means no whitespace.  Space boxes
\(preserved whitespace) need no additional glue."
  (let ((before (cdr prev-box-type))
        (after (car curr-box-type)))
    (if before
        (cond
         ;; Space boxes: no additional glue needed
         ((or (eq before 'space) (eq after 'space)) 'nws)
         ;; Punctuation hugs its content: no glue after an opener,
         ;; none before a closer (these gaps are also unbreakable).
         ((eq before 'cjk-open) 'nws)
         ((eq after 'cjk-close) 'nws)
         ((and (eq before 'latin) (eq after 'latin)) 'lws)
         ((and (eq before 'cjk) (eq after 'cjk)) 'cws)
         ((or (and (eq before 'cjk) (eq after 'latin))
              (and (eq before 'latin) (eq after 'cjk)))
          'mws)
         ;; Remaining punctuation adjacency (after a closer, or before
         ;; an opener): CJK spacing.
         ((or (eq before 'cjk-close) (eq after 'cjk-open)) 'cws))
      'nws)))

(defconst ekp--no-break-joiner-chars '(#x00A0 #x202F #x2007 #x2060 #xFEFF)
  "Characters that forbid a break between their neighbors.
NO-BREAK SPACE, NARROW NO-BREAK SPACE, FIGURE SPACE, WORD JOINER and
the deprecated ZWNBSP.  The zero-width ones attach to the preceding
box; the visible ones are boxes of their own whose adjacent gaps are
unbreakable and glue-free (the character supplies its own spacing).")

(defconst ekp--no-line-start-chars ".,;:!?)]}%’”»›…·"
  "Halfwidth/neutral punctuation that must not start a line.
Applies to boxes consisting solely of these characters (a lone comma
after a CJK char), never to words that merely begin with one
\(\".emacs\").  Fullwidth closers are covered by the `cjk-close'
class instead.")

(defconst ekp--no-line-end-chars "([{‘“«‹"
  "Halfwidth/neutral punctuation that must not end a line.
Same box-level rule as `ekp--no-line-start-chars'; fullwidth openers
are covered by the `cjk-open' class.")

(defconst ekp--no-line-start-char-list (append ekp--no-line-start-chars nil))
(defconst ekp--no-line-end-char-list (append ekp--no-line-end-chars nil))

(defun ekp--box-pure-set-p (box chars)
  "Non-nil when BOX is non-empty and every char is a member of CHARS."
  (let ((len (length box)) (i 0) (all t))
    (when (> len 0)
      (while (and all (< i len))
        (unless (memq (aref box i) chars)
          (setq all nil))
        (setq i (1+ i)))
      all)))

(defun ekp--box-no-line-start-p (box box-type)
  "Non-nil if BOX must not appear at the start of a line."
  (or (eq (car box-type) 'cjk-close)
      (ekp--box-pure-set-p box ekp--no-line-start-char-list)))

(defun ekp--box-no-line-end-p (box box-type)
  "Non-nil if BOX must not appear at the end of a line."
  (or (eq (cdr box-type) 'cjk-open)
      (ekp--box-pure-set-p box ekp--no-line-end-char-list)))

(defun ekp--compute-glue-types (boxes boxes-types hyphen-positions)
  "Compute glue types for BOXES. Positions after HYPHEN-POSITIONS are `nws'."
  (let* ((n (length boxes))
         (glues (make-vector n nil))
         prev-type)
    (dolist (i (append hyphen-positions nil))
      (aset glues (1+ i) 'nws))
    (dotimes (i n)
      (let ((curr-type (aref boxes-types i)))
        (unless (aref glues i)
          (aset glues i (ekp--glue-type prev-type curr-type)))
        (setq prev-type curr-type)))
    glues))

(defun ekp-glue-ideal-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-ideal-pixel)
        ((eq 'mws type) ekp-mws-ideal-pixel)
        ((eq 'cws type) ekp-cws-ideal-pixel)))

(defun ekp-glue-min-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-min-pixel)
        ((eq 'mws type) ekp-mws-min-pixel)
        ((eq 'cws type) ekp-cws-min-pixel)))

(defun ekp-glue-max-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-max-pixel)
        ((eq 'mws type) ekp-mws-max-pixel)
        ((eq 'cws type) ekp-cws-max-pixel)))

(defun ekp--para-glue-ideal (para type)
  "Get ideal glue pixel for TYPE using PARA's stored glue params."
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-ideal))
          ((eq 'mws type) (plist-get params :mws-ideal))
          ((eq 'cws type) (plist-get params :cws-ideal)))))

(defun ekp--para-glue-shrink (para type)
  "Get shrink amount for TYPE using PARA's stored glue params."
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-shrink))
          ((eq 'mws type) (plist-get params :mws-shrink))
          ((eq 'cws type) (plist-get params :cws-shrink)))))

(defun ekp--para-glue-stretch (para type)
  "Get stretch amount for TYPE using PARA's stored glue params."
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-stretch))
          ((eq 'mws type) (plist-get params :mws-stretch))
          ((eq 'cws type) (plist-get params :cws-stretch)))))

(defun ekp--para-glue-min (para type)
  "Get minimum glue pixel (ideal - shrink) for TYPE."
  (- (ekp--para-glue-ideal para type)
     (ekp--para-glue-shrink para type)))

(defun ekp--para-glue-max (para type)
  "Get maximum glue pixel (ideal + stretch) for TYPE."
  (+ (ekp--para-glue-ideal para type)
     (ekp--para-glue-stretch para type)))

;;; ============================================================
;;; Cache Implementation
;;; ============================================================

(defun ekp--para-key (string)
  "Compute cache key for STRING.
The key is a structure compared with `equal', so hash collisions
cannot alias two different paragraphs.  It covers: characters, text
properties, detected fonts, the hyphenation language, and the
effective spacing parameters \(or the symbol `auto' when defaults
are derived per string)."
  (let ((latin-font (ekp-latin-font string))
        (cjk-font (ekp-cjk-font string)))
    (list string
          (prin1-to-string (object-intervals string))
          latin-font cjk-font
          ekp-latin-lang
          ekp-alignment
          ekp-ragged-stretch-pixel
          (and ekp-protrusion ekp-protrusion-ratios)
          ekp-parshape
          ekp-first-line-indent
          (if (and ekp--params-explicit (ekp--params-set-p))
              (list ekp-lws-ideal-pixel ekp-lws-stretch-pixel
                    ekp-lws-shrink-pixel ekp-mws-ideal-pixel
                    ekp-mws-stretch-pixel ekp-mws-shrink-pixel
                    ekp-cws-ideal-pixel ekp-cws-stretch-pixel
                    ekp-cws-shrink-pixel)
            'auto))))

(defun ekp--measure-boxes (boxes uniform-props)
  "Measure pixel widths of BOXES, deduplicating identical boxes.
Identity = same characters AND same text properties.  When
UNIFORM-PROPS is non-nil (the whole paragraph carries at most one
property run), plain string equality suffices as the key.  For CJK
text where each character is a box, deduplication dramatically
reduces the number of `string-pixel-width' calls."
  (let* ((n (length boxes))
         (seen (make-hash-table :test 'equal :size n))
         (widths (make-vector n 0)))
    (dotimes (i n)
      (let* ((box (aref boxes i))
             (key (if uniform-props box
                    (cons box (object-intervals box))))
             (w (gethash key seen)))
        (unless w
          (setq w (string-pixel-width box))
          (puthash key w seen))
        (aset widths i w)))
    widths))

(defun ekp--hyphen-width-for (string)
  "Pixel width of the hyphen char, styled like STRING's first char."
  (let ((props (and (> (length string) 0) (text-properties-at 0 string))))
    (string-pixel-width (if props (apply #'propertize "-" props) "-"))))

(defun ekp--space-box-type-p (box-type)
  "Return non-nil if BOX-TYPE describes a whitespace box."
  (and box-type (eq (car box-type) 'space)))

(defun ekp--tail-protrude-pixel (box box-type)
  "Pixels the last visible char of BOX may protrude past the flush edge."
  (if (not ekp-protrusion)
      0
    (let* ((tail-type (cdr box-type))
           (last-str (substring box -1))
           (ratio (cond
                   ((eq tail-type 'cjk-close)
                    (alist-get 'cjk-close ekp-protrusion-ratios 0))
                   ((memq (aref box (1- (length box)))
                          ekp--no-line-start-char-list)
                    (alist-get 'latin-close ekp-protrusion-ratios 0))
                   (t 0))))
      (if (> ratio 0)
          (floor (* ratio (string-pixel-width last-str)))
        0))))

(defun ekp--line-edge-release (para _start end)
  "Pixels released at the right edge of the line [START, END).
The protrusion of the line's final glyph: the soft hyphen's when the
line breaks at a hyphenation point, otherwise the last non-space
box's.  0 when `ekp-protrusion' was off at paragraph build time."
  (if (ekp--hyphenate-p (ekp-para-hyphen-positions para) (1- end))
      (ekp-para-hyphen-protrude para)
    (aref (ekp-para-tail-protrudes para) end)))

(defun ekp--ragged-extra-stretch ()
  "Resolve the per-line flexibility for non-justify alignment."
  (or ekp-ragged-stretch-pixel
      (max 1 (* 8 (or ekp-lws-ideal-pixel 1)))))

(defun ekp--parshape-active-p ()
  "Non-nil when per-line widths are in effect (parshape or indent)."
  (or ekp-parshape ekp-first-line-indent))

(defun ekp--first-indent-pixel (para)
  "Resolve `ekp-first-line-indent' to pixels for PARA."
  (cond
   ((numberp ekp-first-line-indent) ekp-first-line-indent)
   (ekp-first-line-indent
    (* 2 (string-pixel-width
          (propertize "字" 'face
                      (list :family (ekp-para-cjk-font para))))))
   (t 0)))

(defun ekp--line-spec (para line-index measure)
  "Layout of LINE-INDEX (0-based) as (INDENT . WIDTH).
MEASURE is the paragraph measure passed to the justify call.
`ekp-parshape' takes precedence; its last entry repeats.  Otherwise
`ekp-first-line-indent' shifts line 0.  WIDTH never drops below 1."
  (cond
   (ekp-parshape
    (let* ((shape (if (vectorp ekp-parshape)
                      (append ekp-parshape nil)
                    ekp-parshape))
           (spec (or (nth line-index shape) (car (last shape)))))
      (cons (max 0 (car spec)) (max 1 (cdr spec)))))
   (ekp-first-line-indent
    (if (= line-index 0)
        (let ((indent (ekp--first-indent-pixel para)))
          (cons indent (max 1 (- measure indent))))
      (cons 0 measure)))
   (t (cons 0 measure))))

(defun ekp--make-para (string)
  "Create and fully initialize `ekp-para' struct for STRING.
Computes ALL data in one pass: text, params, and prefix arrays."
  ;; Ensure params: explicit params persist; otherwise derive defaults
  ;; from this string's font.
  (unless (and ekp--params-explicit (ekp--params-set-p))
    (ekp-param-set-default string))
  ;; Extract fonts
  (let* ((latin-font (ekp-latin-font string))
         (cjk-font (ekp-cjk-font string))
         ;; Split into boxes with hyphenation
         (split-result (ekp--split-with-hyphen string))
         (boxes (car split-result))
         (hyphen-positions (cdr split-result))
         (n (length boxes))
         ;; Compute box properties
         (boxes-widths (ekp--measure-boxes
                        boxes (null (cdr (object-intervals string)))))
         (boxes-types (vconcat (mapcar #'ekp--box-type boxes)))
         (glues-types (ekp--compute-glue-types
                       boxes boxes-types hyphen-positions))
         (hyphen-pixel (ekp--hyphen-width-for string))
         ;; Prefix arrays
         (ideal-prefixs (make-vector (1+ n) 0))
         (min-prefixs (make-vector (1+ n) 0))
         (max-prefixs (make-vector (1+ n) 0))
         (glue-ideals (make-vector n 0))
         (glue-shrinks (make-vector n 0))
         (glue-stretches (make-vector n 0))
         (lws-prefixs (make-vector (1+ n) 0))
         (mws-prefixs (make-vector (1+ n) 0))
         (cws-prefixs (make-vector (1+ n) 0))
         (lead-spaces (make-vector (1+ n) 0))
         (trail-spaces (make-vector (1+ n) 0))
         (breaks-allowed (make-bool-vector (1+ n) t))
         (forbidden nil)
         (tail-protrudes (make-vector (1+ n) 0))
         (hyphen-protrude
          (if ekp-protrusion
              (floor (* (alist-get 'hyphen ekp-protrusion-ratios 0)
                        hyphen-pixel))
            0)))
    ;; Break permissions.  A gap is unbreakable when:
    ;; - kinsoku: the line would end with an opener or start with a
    ;;   closer (full- and halfwidth alike),
    ;; - it lies strictly inside an `ekp-no-break' span, or
    ;; - a no-break joiner character (NBSP & friends) touches it.
    ;; Unbreakable gaps carry no glue: punctuation hugs its content,
    ;; atoms stay rigid, NBSP supplies its own spacing.
    (let ((k 1))
      (while (< k n)
        (let* ((prev-box (aref boxes (1- k)))
               (curr-box (aref boxes k))
               (prev-last (aref prev-box (1- (length prev-box))))
               (curr-first (aref curr-box 0)))
          (when (or (ekp--box-no-line-end-p prev-box
                                            (aref boxes-types (1- k)))
                    (ekp--box-no-line-start-p curr-box
                                              (aref boxes-types k))
                    (and (get-text-property (1- (length prev-box))
                                            'ekp-no-break prev-box)
                         (get-text-property 0 'ekp-no-break curr-box))
                    (memq prev-last ekp--no-break-joiner-chars)
                    (memq curr-first ekp--no-break-joiner-chars))
            (aset breaks-allowed k nil)
            (push k forbidden)
            (unless (eq (aref glues-types k) 'nws)
              (aset glues-types k 'nws))))
        (setq k (1+ k))))
    ;; Right-edge protrusion: tail-protrudes[k] = protrusion of the
    ;; last non-space box before gap k (renderer strips trailing
    ;; space boxes, so look through them).
    (when ekp-protrusion
      (let ((pro (make-vector (max n 1) 0)))
        (dotimes (b n)
          (aset pro b (ekp--tail-protrude-pixel (aref boxes b)
                                                (aref boxes-types b))))
        (let ((k 1))
          (while (<= k n)
            (aset tail-protrudes k
                  (if (ekp--space-box-type-p (aref boxes-types (1- k)))
                      (aref tail-protrudes (1- k))
                    (aref pro (1- k))))
            (setq k (1+ k))))))
    ;; Single loop for all prefix computations
    (dotimes (i n)
      (let* ((box-w (aref boxes-widths i))
             (glue-type (aref glues-types i))
             (g-ideal (ekp-glue-ideal-pixel glue-type))
             ;; Non-justify alignment: inter-word glue is rigid; the
             ;; per-line flexibility comes from :extra-stretch instead.
             (g-min (if (eq ekp-alignment 'justify)
                        (ekp-glue-min-pixel glue-type)
                      g-ideal))
             (g-max (if (eq ekp-alignment 'justify)
                        (ekp-glue-max-pixel glue-type)
                      g-ideal)))
        (aset glue-ideals i g-ideal)
        (aset glue-shrinks i (- g-ideal g-min))
        (aset glue-stretches i (- g-max g-ideal))
        (aset ideal-prefixs (1+ i) (+ (aref ideal-prefixs i) box-w g-ideal))
        (aset min-prefixs (1+ i) (+ (aref min-prefixs i) box-w g-min))
        (aset max-prefixs (1+ i) (+ (aref max-prefixs i) box-w g-max))
        (aset lws-prefixs (1+ i) (+ (aref lws-prefixs i)
                                    (if (eq glue-type 'lws) 1 0)))
        (aset mws-prefixs (1+ i) (+ (aref mws-prefixs i)
                                    (if (eq glue-type 'mws) 1 0)))
        (aset cws-prefixs (1+ i) (+ (aref cws-prefixs i)
                                    (if (eq glue-type 'cws) 1 0)))
        ;; trail-spaces[k]: width of space-box run ending at k-1
        (aset trail-spaces (1+ i)
              (if (ekp--space-box-type-p (aref boxes-types i))
                  (+ (aref trail-spaces i) box-w)
                0))))
    ;; lead-spaces[i]: width of space-box run starting at i (backwards pass).
    ;; Index 0 forced to 0: first-line leading spaces are indentation.
    (let ((i (1- n)))
      (while (>= i 0)
        (aset lead-spaces i
              (if (ekp--space-box-type-p (aref boxes-types i))
                  (+ (aref boxes-widths i) (aref lead-spaces (1+ i)))
                0))
        (setq i (1- i))))
    (aset lead-spaces 0 0)
    (ekp-para--create
     :string string
     :latin-font latin-font
     :cjk-font cjk-font
     :boxes boxes
     :boxes-widths boxes-widths
     :boxes-types boxes-types
     :glues-types glues-types
     :hyphen-pixel hyphen-pixel
     :hyphen-positions hyphen-positions
     :ideal-prefixs ideal-prefixs
     :min-prefixs min-prefixs
     :max-prefixs max-prefixs
     :glue-ideals glue-ideals
     :glue-shrinks glue-shrinks
     :glue-stretches glue-stretches
     :lws-prefixs lws-prefixs
     :mws-prefixs mws-prefixs
     :cws-prefixs cws-prefixs
     :lead-spaces lead-spaces
     :trail-spaces trail-spaces
     :breaks-allowed breaks-allowed
     :forbidden-positions (vconcat (nreverse forbidden))
     :tail-protrudes tail-protrudes
     :hyphen-protrude hyphen-protrude
     :glue-params (let ((justify (eq ekp-alignment 'justify)))
                    (list :lws-ideal ekp-lws-ideal-pixel
                          :lws-stretch (if justify ekp-lws-stretch-pixel 0)
                          :lws-shrink (if justify ekp-lws-shrink-pixel 0)
                          :mws-ideal ekp-mws-ideal-pixel
                          :mws-stretch (if justify ekp-mws-stretch-pixel 0)
                          :mws-shrink (if justify ekp-mws-shrink-pixel 0)
                          :cws-ideal ekp-cws-ideal-pixel
                          :cws-stretch (if justify ekp-cws-stretch-pixel 0)
                          :cws-shrink (if justify ekp-cws-shrink-pixel 0)
                          :alignment ekp-alignment
                          :extra-stretch (if justify 0
                                           (ekp--ragged-extra-stretch))))
     :dp-cache (make-hash-table :test 'eql :size 20))))

(defun ekp--get-para (string)
  "Get or create `ekp-para' struct for STRING.
This is the main entry point for cached paragraph data."
  (if (and ekp--last-para
           (eq (car ekp--last-para) string)
           (equal (nth 1 ekp--last-para) ekp-latin-lang))
      (nth 2 ekp--last-para)
    (unless ekp--para-cache
      (setq ekp--para-cache (make-hash-table :test 'equal :size 100)))
    (let* ((key (ekp--para-key string))
           (para (or (gethash key ekp--para-cache)
                     (progn
                       (when (>= (hash-table-count ekp--para-cache)
                                 ekp-para-cache-limit)
                         (clrhash ekp--para-cache))
                       ;; NB: in auto-params mode `ekp--make-para' updates
                       ;; the spacing variables, which invalidates
                       ;; `ekp--last-para'; set the fast path afterwards.
                       (let ((p (ekp--make-para string)))
                         (puthash key p ekp--para-cache)
                         p)))))
      (setq ekp--last-para (list string ekp-latin-lang para))
      para)))

;;;###autoload
(defun ekp-clear-caches ()
  "Clear all paragraph caches."
  (interactive)
  (setq ekp--para-cache nil)
  (setq ekp--last-para nil))

;;;; Paragraph Accessors

(defun ekp--boxes (string)
  (ekp-para-boxes (ekp--get-para string)))

(defun ekp--boxes-widths (string)
  (ekp-para-boxes-widths (ekp--get-para string)))

(defun ekp--glues-types (string)
  (ekp-para-glues-types (ekp--get-para string)))

(defun ekp--ideal-prefixs (string)
  (ekp-para-ideal-prefixs (ekp--get-para string)))

(defun ekp--min-prefixs (string)
  (ekp-para-min-prefixs (ekp--get-para string)))

(defun ekp--max-prefixs (string)
  (ekp-para-max-prefixs (ekp--get-para string)))

(defun ekp--hyphen-pixel (string)
  (ekp-para-hyphen-pixel (ekp--get-para string)))

(defun ekp--hyphen-positions (string)
  (ekp-para-hyphen-positions (ekp--get-para string)))

;;;; K-P Badness and Demerits
;;   demerits = (linepenalty + badness)² + penalty² + extras
;;
;; Fitness classes ensure visual consistency:
;;   0=tight, 1=decent, 2=loose, 3=very-loose
;;   Adjacent lines with class difference > 1 get extra penalty.

(defun ekp--compute-badness (adjustment-pixel flexibility-pixel)
  "Compute Knuth-Plass badness from ADJUSTMENT-PIXEL and FLEXIBILITY-PIXEL.
Returns 0 if no adjustment needed, 10000 (infinite) if impossible."
  (cond
   ((= adjustment-pixel 0) 0)
   ((<= flexibility-pixel 0) ekp--infinite-badness)
   (t (let ((ratio (/ (float adjustment-pixel) flexibility-pixel)))
        (min ekp--infinite-badness (* 100 (expt (abs ratio) 3)))))))

(defun ekp--compute-fitness-class (adjustment-pixel flexibility-pixel)
  "Classify line tightness into fitness class (0-3).
0=tight (shrunk), 1=decent, 2=loose, 3=very-loose."
  (if (<= flexibility-pixel 0)
      1  ; default to decent
    (let ((ratio (/ (float adjustment-pixel) flexibility-pixel)))
      (cond
       ((< ratio -0.5) 0)   ; tight (significantly shrunk)
       ((< ratio 0.5) 1)    ; decent (close to ideal)
       ((< ratio 1.0) 2)    ; loose
       (t 3)))))            ; very loose

(defun ekp--compute-demerits (badness penalty prev-fitness curr-fitness
                                      end-with-hyphenp prev-hyphen-count)
  "Compute K-P demerits for a line break.
BADNESS is the line badness, PENALTY is break penalty (e.g., hyphen).
PREV-FITNESS and CURR-FITNESS are fitness classes of adjacent lines.
Returns total demerits for this break."
  (let* (;; Base demerits: (linepenalty + badness)²
         (base (expt (+ ekp-line-penalty badness) 2))
         ;; Add break penalty
         (with-penalty (+ base (* penalty penalty)))
         ;; Fitness incompatibility penalty
         (fitness-delta (abs (- prev-fitness curr-fitness)))
         (with-fitness (if (> fitness-delta 1)
                           (+ with-penalty ekp-adjacent-fitness-penalty)
                         with-penalty))
         ;; Consecutive hyphen penalty (quadratic growth)
         (hyphen-count (if end-with-hyphenp (1+ prev-hyphen-count) 0))
         (with-hyphen (if end-with-hyphenp
                          (+ with-fitness (* ekp-consecutive-hyphen-penalty
                                             hyphen-count hyphen-count))
                        with-fitness)))
    with-hyphen))

(defun ekp--sorted-vector-member-p (vec n)
  "Return non-nil if N exists in sorted vector VEC.
Uses binary search for O(log n) lookup."
  (and vec
       (> (length vec) 0)
       (let ((lo 0)
             (hi (1- (length vec))))
         (while (< lo hi)
           (let ((mid (/ (+ lo hi) 2)))
             (if (< (aref vec mid) n)
                 (setq lo (1+ mid))
               (setq hi mid))))
         (= (aref vec lo) n))))

(defalias 'ekp--hyphenate-p #'ekp--sorted-vector-member-p
  "Return non-nil if position N in HYPHEN-POSITIONS ends with hyphenation.")

;;;; Shared Line Measurement (O(1) via prefix arrays)

(defun ekp--gaps-between (para i k)
  "Return (latin-gaps mix-gaps cjk-gaps) inside line I..K (exclusive glues).
Counts glue indices I+1 .. K-1 using precomputed prefix counts."
  (let ((lp (ekp-para-lws-prefixs para))
        (mp (ekp-para-mws-prefixs para))
        (cp (ekp-para-cws-prefixs para))
        (j (1+ i)))
    (list (- (aref lp k) (aref lp j))
          (- (aref mp k) (aref mp j))
          (- (aref cp k) (aref cp j)))))

(defun ekp--line-ideal-pixel (para i k)
  "Ideal width of line I..K: box+glue ideals, minus leading glue and
stripped space-box runs, plus hyphen width when the line hyphenates."
  (let* ((ip (ekp-para-ideal-prefixs para))
         (raw (- (aref ip k) (aref ip i)
                 (aref (ekp-para-glue-ideals para) i)))
         (space-w (min raw (+ (aref (ekp-para-lead-spaces para) i)
                              (aref (ekp-para-trail-spaces para) k))))
         (ideal (- raw space-w)))
    (if (ekp--hyphenate-p (ekp-para-hyphen-positions para) (1- k))
        (+ ideal (ekp-para-hyphen-pixel para))
      ideal)))

;;;; Dynamic Programming Line Breaking
;;
;; Design notes:
;; - All line metrics are O(1) via prefix arrays.
;; - Two-pass strategy: a strict Knuth-Plass pass runs first.  If the
;;   paragraph end is unreachable (some region admits no valid line,
;;   e.g. an unbreakable box wider than the line, or a rigid run that
;;   cannot stretch), a second pass permits "emergency" single-box
;;   breaks with huge demerits, guaranteeing that every input yields
;;   output.  The C engine implements the identical strategy.
;; - Emergency demerits = (line-penalty + 10000)² + rest², i.e. at
;;   least as bad as the worst regular line.

(defun ekp--dp-cache-elisp (para line-pixel)
  "Pure Elisp DP implementation. Returns and caches the dp-result plist.
Looseness and per-line widths (parshape/first-line indent) need the
\(position × line-count) DP."
  (if (or (/= ekp-looseness 0) (ekp--parshape-active-p))
      (ekp--dp-cache-elisp-loose para line-pixel)
    (let ((dp-result (or (ekp--dp-run-1d para line-pixel nil)
                         (ekp--dp-run-1d para line-pixel t))))
      (puthash line-pixel dp-result (ekp-para-dp-cache para))
      dp-result)))

(defun ekp--hyphen-flags (hyphen-positions n)
  "Return a bool-vector of length N flagging hyphenatable box indices."
  (let ((v (make-bool-vector (max n 1) nil)))
    (dotimes (j (length hyphen-positions))
      (aset v (aref hyphen-positions j) t))
    v))

(defun ekp--dp-run-1d (para line-pixel allow-emergency)
  "One strict (or emergency-permitting) K-P DP pass over PARA.
Returns the dp-result plist, or nil when the paragraph end is
unreachable (only possible when ALLOW-EMERGENCY is nil)."
  (let* ((boxes (ekp-para-boxes para))
         (n (length boxes))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (hyph-flags (ekp--hyphen-flags
                      (ekp-para-hyphen-positions para) n))
         (ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (glue-ideals (ekp-para-glue-ideals para))
         (glue-shrinks (ekp-para-glue-shrinks para))
         (glue-stretches (ekp-para-glue-stretches para))
         (lws-prefixs (ekp-para-lws-prefixs para))
         (mws-prefixs (ekp-para-mws-prefixs para))
         (cws-prefixs (ekp-para-cws-prefixs para))
         (lead-spaces (ekp-para-lead-spaces para))
         (trail-spaces (ekp-para-trail-spaces para))
         (breaks-ok (ekp-para-breaks-allowed para))
         (tail-protrudes (ekp-para-tail-protrudes para))
         (hyphen-protrude (ekp-para-hyphen-protrude para))
         (params (ekp-para-glue-params para))
         (lws-stretch (plist-get params :lws-stretch))
         (mws-stretch (plist-get params :mws-stretch))
         (cws-stretch (plist-get params :cws-stretch))
         (lws-shrink (plist-get params :lws-shrink))
         (mws-shrink (plist-get params :mws-shrink))
         (cws-shrink (plist-get params :cws-shrink))
         (extra-stretch (or (plist-get params :extra-stretch) 0))
         (backptrs (make-vector (1+ n) nil))
         (demerits (make-vector (1+ n) nil))
         (rests (make-vector (1+ n) nil))
         (gaps (make-vector (1+ n) nil))
         (hyphen-counts (make-vector (1+ n) 0))
         (fitness-classes (make-vector (1+ n) 1)))
    (aset demerits 0 0.0)
    (dotimes (i n)
      (when (aref demerits i)
        (let* ((prev-dem (aref demerits i))
               (prev-hyphen-count (aref hyphen-counts i))
               (prev-fitness (aref fitness-classes i))
               (ip-i (aref ideal-prefixs i))
               (mn-i (aref min-prefixs i))
               (mx-i (aref max-prefixs i))
               (lead-glue-ideal (aref glue-ideals i))
               (lead-glue-min (- lead-glue-ideal (aref glue-shrinks i)))
               (lead-glue-max (+ lead-glue-ideal (aref glue-stretches i)))
               (lead-space (aref lead-spaces i))
               (saw-allowed nil)
               (k (1+ i)))
          (catch 'break
            (while (<= k n)
              (if (not (or (= k n) (aref breaks-ok k)))
                  ;; Break forbidden here (kinsoku, no-break span):
                  ;; not a candidate; keep extending the line.
                  (setq k (1+ k))
              (let* ((is-last (= k n))
                     (single-box (= k (1+ i)))
                     ;; No permitted break strictly inside [i, k): the
                     ;; run is atomic and eligible for emergency
                     ;; handling, like a single box.
                     (atomic-run (not saw-allowed))
                     (end-with-hyphenp (aref hyph-flags (1- k)))
                     (hyph-w (if end-with-hyphenp hyphen-pixel 0))
                     ;; right-edge protrusion releases width at this k
                     (lw (+ line-pixel
                            (if end-with-hyphenp
                                hyphen-protrude
                              (aref tail-protrudes k))))
                     (raw-ideal (- (aref ideal-prefixs k) ip-i lead-glue-ideal))
                     (space-w (min raw-ideal
                                   (+ lead-space (aref trail-spaces k))))
                     (ideal (+ (- raw-ideal space-w) hyph-w))
                     (minw (+ (- (aref min-prefixs k) mn-i lead-glue-min
                                 space-w)
                              hyph-w))
                     (maxw (+ (- (aref max-prefixs k) mx-i lead-glue-max
                                 space-w)
                              hyph-w extra-stretch)))
                (cond
                 ;; Line already too long: emergency-record atomic run,
                 ;; then stop extending.
                 ((or (> minw lw)
                      (and is-last (> ideal lw)))
                  (when (and atomic-run allow-emergency)
                    (ekp--dp-relax-emergency
                     demerits backptrs rests gaps hyphen-counts
                     fitness-classes i k prev-dem
                     (- lw ideal) end-with-hyphenp
                     prev-hyphen-count
                     (unless single-box (ekp--gaps-between para i k))))
                  (throw 'break nil))
                 ;; Valid break point
                 ((or (<= minw lw maxw)
                      (and is-last (<= ideal lw)))
                  (let* ((adjustment (- lw ideal))
                         dem line-gaps fitness new-hyphen)
                    (cond
                     ;; Single box line: fixed flexibility of 1
                     (single-box
                      (let* ((badness (ekp--compute-badness adjustment 1))
                             (penalty (if end-with-hyphenp
                                          ekp-hyphen-penalty 0)))
                        (setq fitness 1
                              new-hyphen (if end-with-hyphenp
                                             (1+ prev-hyphen-count) 0)
                              line-gaps nil
                              dem (ekp--compute-demerits
                                   badness penalty prev-fitness fitness
                                   end-with-hyphenp prev-hyphen-count))))
                     ;; Last line: minimal demerits if reasonably filled
                     (is-last
                      (let* ((fill-ratio (/ (float ideal) lw))
                             (badness (if (< fill-ratio
                                             ekp-last-line-min-ratio)
                                          (* ekp-last-line-short-penalty
                                             (- 1.0 fill-ratio))
                                        0)))
                        (setq fitness 1 new-hyphen 0 line-gaps nil
                              dem (expt (+ ekp-line-penalty badness) 2))))
                     ;; Normal justified line
                     (t
                      (let* ((j (1+ i))
                             (lcnt (- (aref lws-prefixs k)
                                      (aref lws-prefixs j)))
                             (mcnt (- (aref mws-prefixs k)
                                      (aref mws-prefixs j)))
                             (ccnt (- (aref cws-prefixs k)
                                      (aref cws-prefixs j)))
                             (flexibility
                              (if (> adjustment 0)
                                  (+ (* lcnt lws-stretch)
                                     (* mcnt mws-stretch)
                                     (* ccnt cws-stretch)
                                     extra-stretch)
                                (+ (* lcnt lws-shrink)
                                   (* mcnt mws-shrink)
                                   (* ccnt cws-shrink))))
                             (badness (ekp--compute-badness
                                       adjustment flexibility))
                             (penalty (if end-with-hyphenp
                                          ekp-hyphen-penalty 0)))
                        (setq fitness (ekp--compute-fitness-class
                                       adjustment flexibility)
                              new-hyphen (if end-with-hyphenp
                                             (1+ prev-hyphen-count) 0)
                              line-gaps (list lcnt mcnt ccnt)
                              dem (ekp--compute-demerits
                                   badness penalty prev-fitness fitness
                                   end-with-hyphenp prev-hyphen-count)))))
                    (let ((total (+ prev-dem dem)))
                      (when (or (null (aref demerits k))
                                (< total (aref demerits k)))
                        (aset demerits k total)
                        (aset backptrs k i)
                        (aset rests k adjustment)
                        (aset gaps k line-gaps)
                        (aset fitness-classes k fitness)
                        (aset hyphen-counts k new-hyphen)))))
                 ;; Invalid atomic run (rigid underfull): emergency
                 ;; record so the DP cannot dead-end (2nd pass only).
                 ((and atomic-run allow-emergency)
                  (ekp--dp-relax-emergency
                   demerits backptrs rests gaps hyphen-counts
                   fitness-classes i k prev-dem
                   (- lw ideal) end-with-hyphenp
                   prev-hyphen-count
                   (unless single-box (ekp--gaps-between para i k)))))
                (setq saw-allowed t)
                (setq k (1+ k)))))))))
    ;; Extract solution (nil when end unreachable in the strict pass)
    (when (aref demerits n)
      (let ((breaks (ekp--dp-trace-breaks backptrs n)))
        (list :rests (mapcar (lambda (b) (aref rests b)) breaks)
              :gaps (mapcar (lambda (b) (aref gaps b)) breaks)
              :breaks breaks
              :cost (aref demerits n)
              :line-count (length breaks))))))

(defun ekp--dp-relax-emergency (demerits backptrs rests gaps hyphen-counts
                                         fitness-classes i k prev-dem rest
                                         end-with-hyphenp prev-hyphen-count
                                         &optional line-gaps)
  "Record an emergency (over/underfull atomic-run) break at K from I.
REST is line-pixel minus the line's ideal width (may be negative).
LINE-GAPS is the (lws mws cws) gap-count list for multi-box runs
\(nil for single boxes, which render via the single-box path).
Only replaces an existing entry when strictly better."
  (let ((total (+ prev-dem
                  (expt (+ ekp-line-penalty ekp--infinite-badness) 2)
                  (* (float rest) rest))))
    (when (or (null (aref demerits k))
              (< total (aref demerits k)))
      (aset demerits k total)
      (aset backptrs k i)
      (aset rests k rest)
      (aset gaps k line-gaps)
      (aset fitness-classes k 3)
      (aset hyphen-counts k
            (if end-with-hyphenp (1+ prev-hyphen-count) 0)))))

(defun ekp--dp-trace-breaks (backptrs n)
  "Trace optimal break points from BACKPTRS array."
  (let ((breaks (list n))
        (index n))
    (while (> index 0)
      (let ((prev (aref backptrs index)))
        (if prev
            (progn (when (> prev 0) (push prev breaks))
                   (setq index prev))
          ;; Defensive: should not happen (every position is reachable)
          (setq index (1- index)))))
    breaks))

;;;; Looseness: full (position × line-count) DP
;;
;; `ekp-looseness' asks for a paragraph with (optimal + looseness)
;; lines.  The 1D DP only keeps the single best path per position, so
;; alternative line counts are lost.  Here we keep the best path per
;; (position, line-count) state instead, then select the final state
;; whose line count is closest to the target.

(defun ekp--dp-cache-elisp-loose (para line-pixel)
  "Elisp DP tracking all line counts, for `ekp-looseness' support.
Two passes like the 1D engine: strict first, then with emergency
breaks when no valid layout exists."
  (let ((dp-result (or (ekp--dp-run-loose para line-pixel nil)
                       (ekp--dp-run-loose para line-pixel t))))
    (puthash line-pixel dp-result (ekp-para-dp-cache para))
    dp-result))

(defun ekp--dp-run-loose (para line-pixel allow-emergency)
  "One (position × line-count) DP pass.  Returns dp-result or nil."
  (let* ((boxes (ekp-para-boxes para))
         (n (length boxes))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (glue-ideals (ekp-para-glue-ideals para))
         (glue-shrinks (ekp-para-glue-shrinks para))
         (glue-stretches (ekp-para-glue-stretches para))
         (lead-spaces (ekp-para-lead-spaces para))
         (trail-spaces (ekp-para-trail-spaces para))
         (breaks-ok (ekp-para-breaks-allowed para))
         (tail-protrudes (ekp-para-tail-protrudes para))
         (hyphen-protrude (ekp-para-hyphen-protrude para))
         (params (ekp-para-glue-params para))
         (lws-stretch (plist-get params :lws-stretch))
         (mws-stretch (plist-get params :mws-stretch))
         (cws-stretch (plist-get params :cws-stretch))
         (lws-shrink (plist-get params :lws-shrink))
         (mws-shrink (plist-get params :mws-shrink))
         (cws-shrink (plist-get params :cws-shrink))
         (extra-stretch (or (plist-get params :extra-stretch) 0))
         ;; state: (pos . lines) -> [dem backptr fitness hyph rest gaps]
         (states (make-hash-table :test 'equal :size (* 4 (1+ n))))
         (counts-at (make-vector (1+ n) nil)))
    (puthash (cons 0 0) (vector 0.0 nil 1 0 nil nil) states)
    (push 0 (aref counts-at 0))
    (dotimes (i n)
      (dolist (lc (aref counts-at i))
        (let* ((st (gethash (cons i lc) states))
               ;; per-line layout: line LC (0-based) may have its own width
               (this-width (cdr (ekp--line-spec para lc line-pixel)))
               (prev-dem (aref st 0))
               (prev-fitness (aref st 2))
               (prev-hyphen-count (aref st 3))
               (ip-i (aref ideal-prefixs i))
               (mn-i (aref min-prefixs i))
               (mx-i (aref max-prefixs i))
               (lead-glue-ideal (aref glue-ideals i))
               (lead-glue-min (- lead-glue-ideal (aref glue-shrinks i)))
               (lead-glue-max (+ lead-glue-ideal (aref glue-stretches i)))
               (lead-space (aref lead-spaces i))
               (saw-allowed nil)
               (k (1+ i)))
          (catch 'break
            (while (<= k n)
              (if (not (or (= k n) (aref breaks-ok k)))
                  ;; Break forbidden here: keep extending the line.
                  (setq k (1+ k))
              (let* ((is-last (= k n))
                     (single-box (= k (1+ i)))
                     (atomic-run (not saw-allowed))
                     (end-with-hyphenp
                      (ekp--hyphenate-p hyphen-positions (1- k)))
                     (hyph-w (if end-with-hyphenp hyphen-pixel 0))
                     ;; right-edge protrusion releases width at this k
                     (lw (+ this-width
                            (if end-with-hyphenp
                                hyphen-protrude
                              (aref tail-protrudes k))))
                     (raw-ideal (- (aref ideal-prefixs k) ip-i lead-glue-ideal))
                     (space-w (min raw-ideal
                                   (+ lead-space (aref trail-spaces k))))
                     (ideal (+ (- raw-ideal space-w) hyph-w))
                     (minw (+ (- (aref min-prefixs k) mn-i lead-glue-min
                                 space-w)
                              hyph-w))
                     (maxw (+ (- (aref max-prefixs k) mx-i lead-glue-max
                                 space-w)
                              hyph-w extra-stretch))
                     (adjustment (- lw ideal))
                     candidate)
                (cond
                 ((or (> minw lw)
                      (and is-last (> ideal lw)))
                  (when (and atomic-run allow-emergency)
                    (setq candidate
                          (list (+ (expt (+ ekp-line-penalty
                                            ekp--infinite-badness) 2)
                                   (* (float adjustment) adjustment))
                                adjustment
                                (unless single-box
                                  (ekp--gaps-between para i k))
                                3
                                (if end-with-hyphenp
                                    (1+ prev-hyphen-count) 0)))
                    (ekp--dp-loose-relax states counts-at k (1+ lc) i
                                         prev-dem candidate))
                  (throw 'break nil))
                 ((or (<= minw lw maxw)
                      (and is-last (<= ideal lw)))
                  (setq candidate
                        (cond
                         (single-box
                          (let* ((badness (ekp--compute-badness adjustment 1))
                                 (penalty (if end-with-hyphenp
                                              ekp-hyphen-penalty 0))
                                 (nh (if end-with-hyphenp
                                         (1+ prev-hyphen-count) 0)))
                            (list (ekp--compute-demerits
                                   badness penalty prev-fitness 1
                                   end-with-hyphenp prev-hyphen-count)
                                  adjustment nil 1 nh)))
                         (is-last
                          (let* ((fill-ratio (/ (float ideal) lw))
                                 (badness (if (< fill-ratio
                                                 ekp-last-line-min-ratio)
                                              (* ekp-last-line-short-penalty
                                                 (- 1.0 fill-ratio))
                                            0)))
                            (list (expt (+ ekp-line-penalty badness) 2)
                                  adjustment nil 1 0)))
                         (t
                          (let* ((line-gaps (ekp--gaps-between para i k))
                                 (lcnt (nth 0 line-gaps))
                                 (mcnt (nth 1 line-gaps))
                                 (ccnt (nth 2 line-gaps))
                                 (flexibility
                                  (if (> adjustment 0)
                                      (+ (* lcnt lws-stretch)
                                         (* mcnt mws-stretch)
                                         (* ccnt cws-stretch)
                                         extra-stretch)
                                    (+ (* lcnt lws-shrink)
                                       (* mcnt mws-shrink)
                                       (* ccnt cws-shrink))))
                                 (badness (ekp--compute-badness
                                           adjustment flexibility))
                                 (fitness (ekp--compute-fitness-class
                                           adjustment flexibility))
                                 (penalty (if end-with-hyphenp
                                              ekp-hyphen-penalty 0))
                                 (nh (if end-with-hyphenp
                                         (1+ prev-hyphen-count) 0)))
                            (list (ekp--compute-demerits
                                   badness penalty prev-fitness fitness
                                   end-with-hyphenp prev-hyphen-count)
                                  adjustment line-gaps fitness nh)))))
                  (ekp--dp-loose-relax states counts-at k (1+ lc) i
                                       prev-dem candidate))
                 ((and atomic-run allow-emergency)
                  (setq candidate
                        (list (+ (expt (+ ekp-line-penalty
                                          ekp--infinite-badness) 2)
                                 (* (float adjustment) adjustment))
                              adjustment
                              (unless single-box
                                (ekp--gaps-between para i k))
                              3
                              (if end-with-hyphenp
                                  (1+ prev-hyphen-count) 0)))
                  (ekp--dp-loose-relax states counts-at k (1+ lc) i
                                       prev-dem candidate)))
                (setq saw-allowed t)
                (setq k (1+ k)))))))))
    ;; Select final state: line count closest to (optimal + looseness).
    ;; nil when the end is unreachable (strict pass only).
    (when-let* ((end-counts (aref counts-at n)))
      (let ((optimal-count nil) (optimal-dem nil))
        (dolist (c end-counts)
          (let ((dem (aref (gethash (cons n c) states) 0)))
            (when (or (null optimal-dem) (< dem optimal-dem))
              (setq optimal-dem dem optimal-count c))))
        (let* ((target (+ optimal-count ekp-looseness))
               (best-count nil) (best-diff nil) (best-dem nil))
          (dolist (c end-counts)
            (let ((diff (abs (- c target)))
                  (dem (aref (gethash (cons n c) states) 0)))
              (when (or (null best-count)
                        (< diff best-diff)
                        (and (= diff best-diff) (< dem best-dem)))
                (setq best-count c best-diff diff best-dem dem))))
          ;; Trace back through states
          (let ((breaks nil) (lines-rests nil) (lines-gaps nil)
                (pos n) (lc best-count))
            (while (> pos 0)
              (let ((st (gethash (cons pos lc) states)))
                (push pos breaks)
                (push (aref st 4) lines-rests)
                (push (aref st 5) lines-gaps)
                (setq pos (or (aref st 1) 0)
                      lc (1- lc))))
            (list :rests lines-rests
                  :gaps lines-gaps
                  :breaks breaks
                  :cost best-dem
                  :line-count (length breaks))))))))

(defun ekp--dp-loose-relax (states counts-at k lines i prev-dem candidate)
  "Relax state (K . LINES) with CANDIDATE from position I.
CANDIDATE is (DEM-DELTA REST GAPS FITNESS HYPHEN-COUNT)."
  (let* ((key (cons k lines))
         (total (+ prev-dem (nth 0 candidate)))
         (existing (gethash key states)))
    (when (or (null existing) (< total (aref existing 0)))
      (unless existing
        (push lines (aref counts-at k)))
      (puthash key (vector total i (nth 3 candidate) (nth 4 candidate)
                           (nth 1 candidate) (nth 2 candidate))
               states))))

;;;; DP Dispatch and C Module Integration

(defun ekp--dp-get-cached (para line-pixel)
  "Get cached DP result from PARA for LINE-PIXEL, or nil."
  (gethash line-pixel (ekp-para-dp-cache para)))

(defun ekp--c-available-p ()
  "Return non-nil when the C module can be used for DP."
  (and ekp-use-c-module
       (boundp 'ekp-c-module-loaded) ekp-c-module-loaded
       (fboundp 'ekp-c-break-with-arrays)
       ;; looseness and per-line widths need the (position × line-count)
       ;; DP, Elisp only
       (= ekp-looseness 0)
       (not (ekp--parshape-active-p))))

(defun ekp--c-sync-params ()
  "Push current K-P penalty settings to the C module."
  (when (fboundp 'ekp-c-set-penalties)
    (ekp-c-set-penalties ekp-line-penalty
                         ekp-hyphen-penalty
                         ekp-adjacent-fitness-penalty
                         (float ekp-last-line-min-ratio)
                         ekp-consecutive-hyphen-penalty
                         (float ekp-last-line-short-penalty)
                         (if (eq ekp-alignment 'justify)
                             0
                           (ekp--ragged-extra-stretch)))))

(defun ekp-dp-cache (string line-pixel)
  "Compute optimal line breaks for STRING at LINE-PIXEL width.
Uses Knuth-Plass dynamic programming with demerits.
If `ekp-use-c-module' is non-nil and the C module is available (and
`ekp-looseness' is 0), the C module computes the DP."
  (let* ((para (ekp--get-para string))
         (cached (ekp--dp-get-cached para line-pixel)))
    (cond
     (cached cached)
     ((ekp--c-available-p)
      (ekp--dp-cache-via-c para line-pixel))
     (t (ekp--dp-cache-elisp para line-pixel)))))

(defun ekp--lines-data-from-breaks (para line-pixel breaks)
  "Compute (RESTS . GAPS) lists for BREAKS, matching the DP's metrics."
  (let ((start 0) rests gapss)
    (dolist (end breaks)
      (push (- (+ line-pixel (ekp--line-edge-release para start end))
               (ekp--line-ideal-pixel para start end))
            rests)
      (push (if (or (= end (1+ start))
                    (= end (length (ekp-para-boxes para))))
                nil
              (ekp--gaps-between para start end))
            gapss)
      (setq start end))
    (cons (nreverse rests) (nreverse gapss))))

(defun ekp--store-c-result (para line-pixel breaks cost)
  "Store a C-module result (BREAKS, COST) into PARA's dp-cache."
  (let* ((data (ekp--lines-data-from-breaks para line-pixel breaks))
         (dp-result (list :rests (car data)
                          :gaps (cdr data)
                          :breaks breaks
                          :cost cost
                          :line-count (length breaks))))
    (puthash line-pixel dp-result (ekp-para-dp-cache para))
    dp-result))

(defun ekp--prepare-para-for-c (para line-pixel)
  "Prepare PARA data as a 14-element vector for the C batch API."
  (vector (ekp-para-ideal-prefixs para)
          (ekp-para-min-prefixs para)
          (ekp-para-max-prefixs para)
          (ekp-para-glue-ideals para)
          (ekp-para-glue-shrinks para)
          (ekp-para-glue-stretches para)
          (ekp-para-hyphen-positions para)
          (ekp-para-hyphen-pixel para)
          line-pixel
          (ekp-para-lead-spaces para)
          (ekp-para-trail-spaces para)
          (ekp-para-forbidden-positions para)
          (ekp-para-tail-protrudes para)
          (ekp-para-hyphen-protrude para)))

(defun ekp--dp-cache-via-c (para line-pixel)
  "Compute breaks using the C module with PARA's precomputed arrays.
The C module receives all font-dependent data from Elisp; it only
runs the pure DP.  Falls back to Elisp when the C call fails."
  (ekp--c-sync-params)
  (let* ((result (ekp-c-break-with-arrays
                  (ekp-para-ideal-prefixs para)
                  (ekp-para-min-prefixs para)
                  (ekp-para-max-prefixs para)
                  (ekp-para-glue-ideals para)
                  (ekp-para-glue-shrinks para)
                  (ekp-para-glue-stretches para)
                  (ekp-para-hyphen-positions para)
                  (ekp-para-hyphen-pixel para)
                  line-pixel
                  (ekp-para-lead-spaces para)
                  (ekp-para-trail-spaces para)
                  (ekp-para-forbidden-positions para)
                  (ekp-para-tail-protrudes para)
                  (ekp-para-hyphen-protrude para)))
         (c-breaks (car result))
         (c-cost (cdr result)))
    (if (null c-breaks)
        (ekp--dp-cache-elisp para line-pixel)
      (ekp--store-c-result para line-pixel c-breaks c-cost))))

(defun ekp--dp-cache-batch (strings line-pixel)
  "Compute DP for multiple STRINGS in parallel using the C batch API.
Returns list of dp-results in the same order as STRINGS.
Only computes strings that aren't already cached."
  (let* ((paras (mapcar #'ekp--get-para strings))
         (needs-compute '())  ; list of (index . para)
         (results (make-vector (length strings) nil)))
    (cl-loop for para in paras
             for i from 0
             for cached = (ekp--dp-get-cached para line-pixel)
             do (if cached
                    (aset results i cached)
                  (push (cons i para) needs-compute)))
    (if (null needs-compute)
        (append results nil)
      (ekp--c-sync-params)
      (let* ((needs-compute (nreverse needs-compute))
             (batch-input (vconcat
                           (mapcar (lambda (ip)
                                     (ekp--prepare-para-for-c (cdr ip) line-pixel))
                                   needs-compute)))
             (batch-results (ekp-c-break-batch batch-input)))
        (cl-loop for ip in needs-compute
                 for j from 0
                 for idx = (car ip)
                 for para = (cdr ip)
                 for res = (aref batch-results j)
                 for breaks = (car res)
                 for cost = (cdr res)
                 do (aset results idx
                          (if breaks
                              (ekp--store-c-result para line-pixel breaks cost)
                            ;; C failed, fallback to Elisp
                            (ekp--dp-cache-elisp para line-pixel)))))
      (append results nil))))

(defun ekp-dp-data (string line-pixel &optional key)
  "Return the dp cache plist for STRING at LINE-PIXEL.
If KEY is non-nil, return the value of KEY in the plist."
  (let ((data (ekp-dp-cache string line-pixel)))
    (if key
        (plist-get data key)
      data)))

(defun ekp-total-cost (string line-pixel)
  "Return the total demerits of the K-P solution."
  (ekp-dp-data string line-pixel :cost))

(defun ekp-line-breaks (string line-pixel)
  "Return the break points of the K-P solution."
  (ekp-dp-data string line-pixel :breaks))

;;; Line Glue Distribution
;; Distributes extra/deficit space across glues (gaps between boxes)
;; Priority: latin gaps → mixed gaps → CJK gaps

(defun ekp--distribute-gap-adjustment (para rest-pixel gaps-list stretch-p)
  "Distribute REST-PIXEL across GAPS-LIST using PARA's stored glue params.
STRETCH-P indicates stretch (t) or shrink (nil) mode.
Returns ((latin-adj . latin-extra) (mix-adj . mix-extra) (cjk-adj . cjk-extra))."
  (let* ((params (ekp-para-glue-params para))
         (latin-gaps (nth 0 gaps-list))
         (mix-gaps (nth 1 gaps-list))
         (cjk-gaps (nth 2 gaps-list))
         (remaining rest-pixel)
         ;; Per-gap adjustment values from para's stored params
         (latin-change (if stretch-p
                           (plist-get params :lws-stretch)
                         (plist-get params :lws-shrink)))
         (mix-change (if stretch-p
                         (plist-get params :mws-stretch)
                       (plist-get params :mws-shrink)))
         (cjk-change (if stretch-p
                         (plist-get params :cws-stretch)
                       (plist-get params :cws-shrink)))
         ;; Results
         (latin-adj 0) (latin-extra 0)
         (mix-adj 0) (mix-extra 0)
         (cjk-adj 0) (cjk-extra 0))
    ;; Distribute to latin gaps first
    (let ((latin-capacity (* latin-gaps latin-change)))
      (if (< remaining latin-capacity)
          (when (> latin-gaps 0)
            (setq latin-adj (/ remaining latin-gaps))
            (setq latin-extra (% remaining latin-gaps))
            (setq remaining 0))
        (setq latin-adj latin-change)
        (setq remaining (- remaining latin-capacity))))
    ;; Then to mixed gaps
    (when (> remaining 0)
      (let ((mix-capacity (* mix-gaps mix-change)))
        (if (< remaining mix-capacity)
            (when (> mix-gaps 0)
              (setq mix-adj (/ remaining mix-gaps))
              (setq mix-extra (% remaining mix-gaps))
              (setq remaining 0))
          (setq mix-adj mix-change)
          (setq remaining (- remaining mix-capacity)))))
    ;; Finally to CJK gaps.  When stretching, CJK gaps absorb any
    ;; leftover beyond their nominal capacity (emergency spreading);
    ;; when shrinking they never shrink below their limit.
    (when (> remaining 0)
      (if stretch-p
          (when (> cjk-gaps 0)
            (setq cjk-adj (/ remaining cjk-gaps))
            (setq cjk-extra (% remaining cjk-gaps)))
        (let ((cjk-capacity (* cjk-gaps cjk-change)))
          (if (< remaining cjk-capacity)
              (when (> cjk-gaps 0)
                (setq cjk-adj (/ remaining cjk-gaps))
                (setq cjk-extra (% remaining cjk-gaps)))
            (setq cjk-adj cjk-change)))))
    (list (cons latin-adj latin-extra)
          (cons mix-adj mix-extra)
          (cons cjk-adj cjk-extra))))

(defun ekp--compute-glue-pixels (para glues-types gaps-distribution stretch-p)
  "Compute actual glue pixels from GLUES-TYPES and GAPS-DISTRIBUTION.
Returns list of pixel values for each glue. Uses PARA's stored glue params."
  (let ((latin-adj (car (nth 0 gaps-distribution)))
        (latin-extra (cdr (nth 0 gaps-distribution)))
        (mix-adj (car (nth 1 gaps-distribution)))
        (mix-extra (cdr (nth 1 gaps-distribution)))
        (cjk-adj (car (nth 2 gaps-distribution)))
        (cjk-extra (cdr (nth 2 gaps-distribution)))
        (latin-idx -1) (mix-idx -1) (cjk-idx -1))
    (mapcar
     (lambda (type)
       (let* ((base (ekp--para-glue-ideal para type))
              (adj (pcase type
                     ('lws (cl-incf latin-idx)
                           (+ latin-adj (if (< latin-idx latin-extra) 1 0)))
                     ('mws (cl-incf mix-idx)
                           (+ mix-adj (if (< mix-idx mix-extra) 1 0)))
                     ('cws (cl-incf cjk-idx)
                           (+ cjk-adj (if (< cjk-idx cjk-extra) 1 0)))
                     (_ 0))))
         (max 0 (if stretch-p (+ base adj) (- base adj)))))
     glues-types)))

(defun ekp--line-glue-single-box (line-pixel box-width hyphen-p hyphen-pixel)
  "Compute glues for a single-box line.
The trailing filler is clamped at 0 for overfull boxes."
  (let ((trailing (- line-pixel box-width (if hyphen-p hyphen-pixel 0))))
    (list 0 (max 0 trailing))))

(defun ekp--line-glue-last-line (para glues-types ideal-pixel line-pixel)
  "Compute glues for last line (ragged right). Uses PARA's stored glue params."
  (append '(0)
          (mapcar (lambda (type) (ekp--para-glue-ideal para type)) glues-types)
          (list (max 0 (- line-pixel ideal-pixel)))))

(defun ekp--line-glue-normal (para glues-types rest-pixel gaps-list)
  "Compute glues for a normal (justified) line. Uses PARA's stored glue params."
  (if (= rest-pixel 0)
      (append '(0) (mapcar (lambda (type) (ekp--para-glue-ideal para type))
                           glues-types)
              '(0))
    (let* ((stretch-p (> rest-pixel 0))
           (distribution (ekp--distribute-gap-adjustment
                          para (abs rest-pixel) gaps-list stretch-p))
           (glue-pixels (ekp--compute-glue-pixels
                         para glues-types distribution stretch-p)))
      (append '(0) glue-pixels '(0)))))

(defun ekp-line-glues (string line-pixel)
  "Compute glue pixels for each line after breaking STRING at LINE-PIXEL.
Returns vector of vectors, each inner vector is glue pixels for one line.
Each line's glues: [0 glue1 glue2 ... trailing-space]."
  (let* ((para (ekp--get-para string))
         (boxes-num (length (ekp-para-boxes para)))
         (glues-types (ekp-para-glues-types para))
         (alignment (or (plist-get (ekp-para-glue-params para) :alignment)
                        'justify))
         (ragged (not (eq alignment 'justify)))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (breaks (ekp-line-breaks string line-pixel))
         (lines-rests (ekp-dp-data string line-pixel :rests))
         (lines-gaps (ekp-dp-data string line-pixel :gaps))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-glues-types (append (cl-subseq glues-types (1+ start) end)
                                       nil))
             (is-last (>= end boxes-num))
             (hyphen-p (ekp--hyphenate-p hyphen-positions (1- end)))
             ;; per-line layout (parshape / first-line indent)
             (line-spec (ekp--line-spec para i line-pixel))
             (line-indent (car line-spec))
             ;; right-edge protrusion widens this line's effective target
             (eff-pixel (+ (cdr line-spec)
                           (if hyphen-p
                               (ekp-para-hyphen-protrude para)
                             (aref (ekp-para-tail-protrudes para) end))))
             ;; DP-consistent metrics (space-box runs excluded, hyphen incl.)
             (ideal-pixel (ekp--line-ideal-pixel para start end))
             (max-pixel (let* ((mx (ekp-para-max-prefixs para))
                               (ip (ekp-para-ideal-prefixs para))
                               (raw-ideal (- (aref ip end) (aref ip start)
                                             (aref (ekp-para-glue-ideals para)
                                                   start)))
                               (space-w (min raw-ideal
                                             (+ (aref (ekp-para-lead-spaces para)
                                                      start)
                                                (aref (ekp-para-trail-spaces para)
                                                      end)))))
                          (+ (- (aref mx end) (aref mx start)
                                (+ (aref (ekp-para-glue-ideals para) start)
                                   (aref (ekp-para-glue-stretches para) start))
                                space-w)
                             (if hyphen-p hyphen-pixel 0))))
             glue-list)
        (setq glue-list
              (cond
               ;; Single box: just trailing space
               ((= 1 (- end start))
                (ekp--line-glue-single-box eff-pixel
                                           (- ideal-pixel
                                              (if hyphen-p hyphen-pixel 0))
                                           hyphen-p hyphen-pixel))
               ;; Last line, or any line under non-justify alignment:
               ;; natural glue widths plus a trailing filler.
               ((or is-last ragged)
                (ekp--line-glue-last-line
                 para line-glues-types ideal-pixel eff-pixel))
               ;; Emergency underfull line (can't stretch to width):
               ;; set glues to max and pad with trailing filler.
               ((< max-pixel eff-pixel)
                (append '(0)
                        (mapcar (lambda (type)
                                  (ekp--para-glue-max para type))
                                line-glues-types)
                        (list (max 0 (- eff-pixel max-pixel)))))
               ;; Normal justified line
               (t
                (ekp--line-glue-normal para line-glues-types
                                       (nth i lines-rests)
                                       (nth i lines-gaps)))))
        ;; Non-justify alignment: place the leftover per mode
        ;; (ragged-right keeps it trailing; center splits it; ragged-left
        ;; moves it to the head).
        (when (and ragged (>= (length glue-list) 2)
                   (memq alignment '(center ragged-left)))
          (let ((filler (car (last glue-list))))
            (setq glue-list
                  (if (eq alignment 'center)
                      (let ((lead (/ filler 2)))
                        (append (list lead)
                                (cdr (butlast glue-list))
                                (list (- filler lead))))
                    (append (list filler)
                            (cdr (butlast glue-list))
                            (list 0))))))
        ;; left indent renders as a leading spacer
        (when (> line-indent 0)
          (setq glue-list (cons (+ (car glue-list) line-indent)
                                (cdr glue-list))))
        (aset line-glues i (vconcat glue-list))
        (setq start end)))
    line-glues))

;;;; Rendering

(defun ekp--box-space-p (box)
  "Return non-nil if BOX is a whitespace-only box."
  (and box (not (string-empty-p box))
       (or (string-blank-p box) (= (string-width box) 0))))

(defun ekp--strip-line-spaces (line-boxes line-glues
                                          &optional strip-leading strip-trailing)
  "Strip leading/trailing space boxes from LINE-BOXES based on flags.
STRIP-LEADING / STRIP-TRAILING: strip space boxes at that edge.
LINE-GLUES is treated as an opaque list of n+1 glue values kept in
sync with the boxes.  Returns (kept-boxes kept-glues nlead ntrail)
where NLEAD / NTRAIL count the boxes stripped at each edge.

The stripped widths are NOT redistributed: the DP already excluded
these space-box runs from its line metrics, so the remaining boxes
plus distributed glues already fill the target width exactly."
  (let ((boxes (append line-boxes nil))
        (glues (append line-glues nil))
        (nlead 0) (ntrail 0))
    (when (> (length boxes) 0)
      ;; Strip trailing space boxes (if requested)
      (when strip-trailing
        (while (and boxes (ekp--box-space-p (car (last boxes))))
          (setq ntrail (1+ ntrail))
          (setq boxes (butlast boxes))
          ;; Remove second-to-last glue (the one before the trailing
          ;; space box); keep the last glue (line's trailing filler).
          (when (> (length glues) 1)
            (setq glues (append (butlast (butlast glues)) (last glues))))))
      ;; Strip leading space boxes (if requested)
      (when strip-leading
        (while (and boxes (ekp--box-space-p (car boxes)))
          (setq nlead (1+ nlead))
          (setq boxes (cdr boxes))
          ;; Remove the second glue (the one after the leading glue)
          (when (> (length glues) 1)
            (setq glues (cons (car glues) (cddr glues)))))))
    (list boxes glues nlead ntrail)))

(defun ekp--box-offsets (string boxes)
  "Locate each of BOXES in STRING; return a vector of (START . END).
Boxes are in order and separated only by characters the tokenizer
dropped (whitespace runs, zero-width breakers), so a sequential
leftmost scan aligns them unambiguously."
  (let ((offsets (make-vector (length boxes) nil))
        (p 0) (i 0))
    (dolist (box boxes)
      (let ((blen (length box)))
        (while (not (eq t (compare-strings string p (+ p blen) box 0 blen)))
          (setq p (1+ p)))
        (aset offsets i (cons p (+ p blen)))
        (setq p (+ p blen))
        (setq i (1+ i))))
    offsets))

(defun ekp--hide-string (string)
  "Return STRING marked `ekp-hidden' and displayed as nothing."
  (if (string-empty-p string)
      string
    (propertize string 'ekp-hidden t 'display "")))

(defvar ekp--glue-string-cache (make-hash-table :test 'eql)
  "PIXEL → shared glue string for empty payloads (pure, shareable).")

(defvar ekp--glue-space-string-cache (make-hash-table :test 'eql)
  "PIXEL → shared glue string for a plain single-space payload.")

(defun ekp--render-glue (pixel payload)
  "Render a glue of PIXEL width that replaced original text PAYLOAD.
Zero-width glue renders as the hidden PAYLOAD itself, so no original
character is ever dropped.  The common payloads (empty, plain space)
are interned per width: glue strings are immutable, so sharing is
safe and avoids re-allocating properties for every gap."
  (cond
   ((and (= pixel 0) (string-empty-p payload)) "")
   ((<= pixel 0) (ekp--hide-string payload))
   ((string-empty-p payload)
    (or (gethash pixel ekp--glue-string-cache)
        (puthash pixel
                 (propertize " " 'display `(space :width (,pixel))
                             'ekp-glue payload)
                 ekp--glue-string-cache)))
   ((and (string= payload " ") (null (object-intervals payload)))
    (or (gethash pixel ekp--glue-space-string-cache)
        (puthash pixel
                 (propertize " " 'display `(space :width (,pixel))
                             'ekp-glue payload)
                 ekp--glue-space-string-cache)))
   (t (propertize " " 'display `(space :width (,pixel)) 'ekp-glue payload))))

(defun ekp--hyphen-for-box (box)
  "Return a hyphen string styled like the end of BOX.
The `ekp-soft-hyphen' property marks it as synthesized, so
`ekp-unjustify-region' can strip it structurally."
  (let ((props (and (> (length box) 0)
                    (text-properties-at (1- (length box)) box))))
    (apply #'propertize "-" 'ekp-soft-hyphen t props)))

(defun ekp--pixel-justify (string line-pixel)
  "Justify single-paragraph STRING to LINE-PIXEL, with render caching.
The rendered string for a (paragraph, width) pair is deterministic,
so it is stored in the paragraph's dp-cache entry and reused — resize
sweeps that revisit a width pay nothing."
  (let* ((para (ekp--get-para string))
         (dp (ekp-dp-data string line-pixel))
         (hit (plist-get dp :rendered)))
    (or hit
        (let ((rendered (ekp--pixel-justify-1 string line-pixel))
              (cache (ekp-para-dp-cache para)))
          ;; keep memory bounded during long resize sessions
          (when (<= (hash-table-count cache) 64)
            (puthash line-pixel (plist-put dp :rendered rendered) cache))
          rendered))))

(defun ekp--pixel-justify-1 (string line-pixel)
  "Justify single-paragraph STRING to LINE-PIXEL.

The output is lossless with respect to STRING:
- synthesized spacing carries an `ekp-glue' property whose value is
  the original text it replaced (usually a whitespace run),
- soft line breaks are newlines whose `ekp-soft-break' property holds
  the original text swallowed around the break,
- original text outside any visible line (paragraph-edge whitespace)
  survives as zero-display `ekp-hidden' text,
- break hyphens carry `ekp-soft-hyphen'.
`ekp-unjustify-region' inverts all four structurally."
  (let* ((para (ekp--get-para string))
         (boxes (append (ekp-para-boxes para) nil))
         (offsets (or (ekp-para-box-offsets-memo para)
                      (setf (ekp-para-box-offsets-memo para)
                            (ekp--box-offsets string boxes))))
         (breaks (ekp-line-breaks string line-pixel))
         (num (length breaks))
         (lines-glues (ekp-line-glues string line-pixel))
         (hyphen-positions (ekp--hyphen-positions string))
         (start 0)
         ;; (rendered-text first-box-idx last-box-idx) per visible line
         (lines nil))
    (dotimes (i num)
      (let* ((end (nth i breaks))
             (line-boxes (cl-subseq boxes start end))
             (glue-pixels (append (aref lines-glues i) nil))
             ;; Strip space boxes:
             ;; - First line (i=0): keep leading spaces (indentation)
             ;; - Other lines: strip leading spaces (break artifacts)
             ;; - All lines: strip trailing spaces
             (is-first-line (= i 0))
             (stripped (ekp--strip-line-spaces line-boxes glue-pixels
                                               (not is-first-line)
                                               t))
             (kept (nth 0 stripped))
             (kept-glues (nth 1 stripped))
             (first-idx (+ start (nth 2 stripped)))
             ;; Check if last box of this line needs hyphen
             (need-hyphen
              (and (< i (1- num))  ; not last line
                   (ekp--hyphenate-p hyphen-positions (1- end)))))
        (when kept
          (let ((parts nil) (idx first-idx) (glues kept-glues) (n 0))
            (dolist (box kept)
              (push (ekp--render-glue
                     (pop glues)
                     (if (> idx first-idx)
                         (substring string
                                    (cdr (aref offsets (1- idx)))
                                    (car (aref offsets idx)))
                       ;; leading glue of a line is always 0px and
                       ;; replaces nothing; edge text is handled by
                       ;; soft breaks / hidden runs below
                       ""))
                    parts)
              (push box parts)
              (setq idx (1+ idx) n (1+ n)))
            (when need-hyphen
              (push (ekp--hyphen-for-box (car (last kept))) parts))
            ;; trailing filler glue (synthesized, replaces nothing)
            (push (ekp--render-glue (car glues) "") parts)
            (push (list (apply #'concat (nreverse parts))
                        first-idx (+ first-idx n -1))
                  lines)))
        (setq start end)))
    (setq lines (nreverse lines))
    (if (null lines)
        ;; Defensive: no visible box at all (blank paragraphs are
        ;; filtered before this function).
        (ekp--hide-string string)
      (let* ((first-line (car lines))
             (last-line (car (last lines)))
             (parts (list (ekp--hide-string
                           (substring string 0
                                      (car (aref offsets (nth 1 first-line)))))))
             (prev nil))
        (dolist (line lines)
          (when prev
            (push (propertize "\n" 'ekp-soft-break
                              (substring string
                                         (cdr (aref offsets (nth 2 prev)))
                                         (car (aref offsets (nth 1 line)))))
                  parts))
          (push (nth 0 line) parts)
          (setq prev line))
        (push (ekp--hide-string
               (substring string (cdr (aref offsets (nth 2 last-line)))))
              parts)
        (apply #'concat (nreverse parts))))))

(defun ekp--validate-width (line-pixel)
  "Signal a user error unless LINE-PIXEL is a positive integer."
  (unless (and (integerp line-pixel) (> line-pixel 0))
    (user-error "ekp: line width must be a positive integer, got %S"
                line-pixel)))

(defun ekp-pixel-justify (string line-pixel)
  "Justify multiline STRING to LINE-PIXEL pixels.
Each newline-separated segment is treated as one paragraph.
When the C module is available, paragraphs are computed in parallel."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (ekp--validate-width line-pixel)
  (let* ((strs (split-string string "\n"))
         (non-blank-strs (cl-remove-if #'string-blank-p strs))
         (use-batch (and (ekp--c-available-p)
                         (fboundp 'ekp-c-break-batch)
                         (> (length non-blank-strs) 1))))
    ;; Pre-compute all DP results in parallel if using batch
    (when use-batch
      (ekp--dp-cache-batch non-blank-strs line-pixel))
    ;; Now process each string (DP results are cached)
    (mapconcat (lambda (str)
                 (if (string-blank-p str)
                     ""
                   (ekp--pixel-justify str line-pixel)))
               strs "\n")))

;;; Optimal Width Search
;;
;; Ternary search over average demerits, refined with a local scan.
;; Note: cost as a function of width is not strictly unimodal (line
;; count changes cause jumps), so the result is a good local optimum;
;; the final neighborhood scan smooths out small non-unimodalities.

(defun ekp--compute-avg-cost (strings pixel)
  "Compute average cost for STRINGS at PIXEL width."
  ;; Batch all paragraphs through the C module in one call if possible.
  (when (and (ekp--c-available-p) (fboundp 'ekp-c-break-batch))
    (let ((non-blank (cl-remove-if #'string-blank-p strings)))
      (when (> (length non-blank) 1)
        (ekp--dp-cache-batch non-blank pixel))))
  (let ((total-cost 0)
        (count 0))
    (dolist (s strings)
      (unless (string-blank-p s)
        (cl-incf total-cost (abs (ekp-total-cost s pixel)))
        (cl-incf count)))
    (if (> count 0)
        (/ (float total-cost) count)
      most-positive-fixnum)))

(defun ekp--ternary-search-optimal-width (strings min-pixel max-pixel)
  "Find optimal width in [MIN-PIXEL, MAX-PIXEL] using ternary search.
Returns the pixel width with minimum average cost."
  (let ((lo min-pixel)
        (hi max-pixel))
    ;; Ternary search: O(log n) iterations
    (while (> (- hi lo) 2)
      (let* ((mid1 (+ lo (/ (- hi lo) 3)))
             (mid2 (- hi (/ (- hi lo) 3)))
             (cost1 (ekp--compute-avg-cost strings mid1))
             (cost2 (ekp--compute-avg-cost strings mid2)))
        (if (< cost1 cost2)
            (setq hi mid2)
          (setq lo mid1))))
    ;; Local scan around the ternary result to escape small
    ;; non-unimodalities (cost jumps when the line count changes).
    (let* ((center (/ (+ lo hi) 2))
           (best-pixel nil)
           (best-cost nil))
      (cl-loop for p from (max min-pixel (- center 3))
               to (min max-pixel (+ center 3))
               for cost = (ekp--compute-avg-cost strings p)
               when (or (null best-cost) (< cost best-cost))
               do (setq best-cost cost best-pixel p))
      best-pixel)))

(defun ekp-pixel-range-justify (string min-pixel max-pixel)
  "Find optimal width for STRING between MIN-PIXEL and MAX-PIXEL.
Returns (justified-text . optimal-pixel)."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (ekp--validate-width min-pixel)
  (ekp--validate-width max-pixel)
  (when (> min-pixel max-pixel)
    (user-error "ekp: min-pixel (%d) must be <= max-pixel (%d)"
                min-pixel max-pixel))
  (let* ((strings (split-string string "\n"))
         ;; Pre-warm caches
         (_ (dolist (s strings)
              (unless (string-blank-p s)
                (ekp--get-para s))))
         (best-pixel (ekp--ternary-search-optimal-width
                      strings min-pixel max-pixel)))
    (cons (ekp-pixel-justify string best-pixel) best-pixel)))

(provide 'ekp)

;;; ekp.el ends here
