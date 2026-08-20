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
(require 'seq)
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

(defcustom ekp-inline-code-policy 'no-hyphen
  "Default break policy for automatic inline code spans.
`normal' uses ordinary breaks and hyphenation.  `no-hyphen' keeps
ordinary legal breaks but suppresses discretionary dictionary
hyphens.  `no-break' makes fitting automatic spans rigid; overwide
automatic spans downgrade to `no-hyphen'.  This option does not
affect explicit `ekp-no-break' regions."
  :type '(choice (const normal) (const no-hyphen) (const no-break))
  :safe #'ekp--safe-break-policy-value-p
  :group 'ekp)

(defcustom ekp-hyphenation 'auto
  "Global discretionary hyphenation policy.
`auto' and `on' use the configured dictionary when available;
missing dictionaries disable hyphenation without signaling.
`off' disables discretionary hyphenation."
  :type '(choice (const auto) (const on) (const off))
  :safe #'ekp--safe-hyphenation-value-p
  :group 'ekp)

(defcustom ekp-token-break-policies
  '((url . no-hyphen)
    (path . no-hyphen)
    (identifier . no-hyphen)
    (number-unit . no-break))
  "Break policies for bounded automatic token classifiers."
  :type '(alist :key-type (choice (const url) (const path)
                                  (const identifier) (const number-unit))
                :value-type (choice (const normal) (const no-hyphen)
                                    (const no-break)))
  :safe #'ekp--safe-token-break-policies-p
  :group 'ekp)

(defcustom ekp-number-unit-suffixes
  '("%" "‰" "°C" "°F" "px" "pt" "pc" "em" "rem" "ms" "s" "min" "h"
    "Hz" "kHz" "MHz" "GHz" "B" "KB" "MB" "GB" "TB" "KiB" "MiB"
    "GiB" "TiB" "μm" "mm" "cm" "m" "km" "mg" "g" "kg")
  "Exact suffixes recognized by the compact number-unit classifier."
  :type '(repeat string)
  :safe (lambda (value)
          (and (proper-list-p value) (seq-every-p #'stringp value)))
  :group 'ekp)

(defcustom ekp-kinsoku-profile 'common
  "Kinsoku profile used when compiling paragraph break permissions."
  :type '(choice (const common) (const zh) (const ja)
                 (const off) (const custom))
  :safe (lambda (value) (memq value '(common zh ja off custom)))
  :group 'ekp)

(defcustom ekp-overlong-token-policy 'emergency
  "Policy for ordinary non-CJK tokens wider than the measure."
  :type '(choice (const emergency) (const overflow) (const natural))
  :safe (lambda (value) (memq value '(emergency overflow natural)))
  :group 'ekp)

(defun ekp--safe-break-policy-value-p (value)
  "Return non-nil when VALUE is a documented break-policy value."
  (memq value '(normal no-hyphen no-break)))

(defun ekp--safe-hyphenation-value-p (value)
  "Return non-nil when VALUE is a documented hyphenation value."
  (memq value '(auto on off)))

(defun ekp--safe-token-break-policies-p (value)
  "Return non-nil when VALUE is a safe token-policy alist."
  (and (proper-list-p value)
       (seq-every-p
        (lambda (entry)
          (and (consp entry)
               (memq (car entry) '(url path identifier number-unit))
               (ekp--safe-break-policy-value-p (cdr entry))))
        value)))

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

(defvar ekp--allow-native-live-append nil
  "Non-nil when auto live append may use a loaded native DP module.")

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

(defun ekp--safe-emergency-stretch-pixel-p (value)
  "Return non-nil when VALUE is a safe emergency-stretch setting."
  (or (null value) (and (integerp value) (>= value 0))))

(defcustom ekp-emergency-stretch-pixel nil
  "Fixed final-pass emergency stretch budget in pixels.
nil derives a display-context-local value from the current font metrics.
The value is a paragraph setting, not a fraction of the candidate line
width; the strict DP pass uses zero and the final pass uses this fixed
budget with ordinary badness, fitness, and demerits."
  :type '(choice (const :tag "Auto (3 em)" nil) natnum)
  :safe #'ekp--safe-emergency-stretch-pixel-p
  :group 'ekp)

(defcustom ekp-protrusion nil
  "Non-nil enables right-edge character protrusion (hanging punctuation).
A line ending in punctuation lets part of that glyph hang past the
flush edge, per `ekp-protrusion-ratios' — CLREQ line-end punctuation
squeeze and microtype-style hanging periods/hyphens in one mechanism.
Left-edge protrusion is not implemented: Emacs cannot render text
before the line origin.  Buffer integration reserves the protrusion
width in its layout when enabled."
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
  ;; Natural pixel width of each source gap (indexed by right box).
  ;; Width-independent projection geometry is measured once per paragraph.
  (gap-naturals-memo nil)
  ;; Structural policy intervals compiled into hyphenation and breaks.
  (resolved-policies nil)
  ;; Glue params snapshot at para creation time (plist)
  glue-params
  (layout-plan-cache nil :type hash-table)
  (dp-cache nil :type hash-table))

(cl-defstruct (ekp--policy-interval (:constructor ekp--policy-interval-create))
  "Resolved private policy interval before DP compilation."
  start end break-policy hyphenation literal-spacing provenance category)

(cl-defstruct (ekp-layout-gap (:constructor ekp-layout-gap--create))
  "One planned glue between two source boxes."
  kind left-box right-box source-start source-end natural-pixel target-pixel)

(cl-defstruct (ekp-layout-line (:constructor ekp-layout-line--create))
  "One display line in an `ekp-layout-plan'."
  index box-start box-end source-start source-end glues gaps
  leading-pixel trailing-pixel hyphen-p break-kind
  break-source-start break-source-end signature)

(cl-defstruct (ekp-layout-plan (:constructor ekp-layout-plan--create))
  "Semantic KP layout independent of any output representation."
  string line-pixel context para boxes offsets lines
  (state 'planned) reason)

(define-error 'ekp-backend-contract-error
  "Malformed C backend result")

(defun ekp--copy-layout-context-value (value)
  "Return a consumer-owned copy of layout context VALUE."
  (cond
   ((stringp value)
    (copy-sequence value))
   ((vectorp value)
    (let ((copy (copy-sequence value)))
      (dotimes (i (length copy))
        (aset copy i (ekp--copy-layout-context-value (aref copy i))))
      copy))
   ((consp value)
    (cons (ekp--copy-layout-context-value (car value))
          (ekp--copy-layout-context-value (cdr value))))
   (t value)))

(defun ekp--copy-layout-line (line)
  "Return a consumer-owned copy of LINE and its mutable children."
  (let ((copy (copy-ekp-layout-line line)))
    (setf (ekp-layout-line-glues copy)
          (copy-sequence (ekp-layout-line-glues line)))
    (setf (ekp-layout-line-gaps copy)
          (vconcat (mapcar #'copy-ekp-layout-gap
                           (append (ekp-layout-line-gaps line) nil))))
    (setf (ekp-layout-line-signature copy)
          (copy-tree (ekp-layout-line-signature line)))
    copy))

(defun ekp--copy-layout-plan-for-consumer (plan)
  "Return a consumer-owned copy of cached PLAN.
`ekp-layout-plan-para' is intentionally shared: paragraph-cache
ownership predates this semantic-plan cache, and append planning
relies on that stable paragraph identity.  The plan and its mutable
plan-owned payloads are copied at this boundary."
  (let ((copy (copy-ekp-layout-plan plan)))
    (setf (ekp-layout-plan-string copy)
          (copy-sequence (ekp-layout-plan-string plan)))
    (setf (ekp-layout-plan-context copy)
          (ekp--copy-layout-context-value
           (ekp-layout-plan-context plan)))
    (setf (ekp-layout-plan-boxes copy)
          (vconcat (mapcar #'copy-sequence
                           (append (ekp-layout-plan-boxes plan) nil))))
    (setf (ekp-layout-plan-offsets copy)
          (vconcat (mapcar (lambda (offset)
                             (cons (car offset) (cdr offset)))
                           (append (ekp-layout-plan-offsets plan) nil))))
    (setf (ekp-layout-plan-lines copy)
          (vconcat (mapcar #'ekp--copy-layout-line
                           (append (ekp-layout-plan-lines plan) nil))))
    copy))

(defvar ekp--para-cache nil
  "Cache: equal-keyed table, content key → ekp-para struct.")

(defvar ekp--policy-analysis-cache nil
  "Cache: base paragraph policy identity → width-tiered policy analysis.")

(defvar ekp--policy-analysis-sensitive-p nil
  "Non-nil when the most recent policy analysis depends on measure.")

(defvar ekp--last-para nil
  "Fast path for the most recently resolved paragraph.
The value is (STRING KEY PARA), where KEY is the same complete
structural key used by `ekp--para-cache'.  One justification call
resolves the same string object many times; this avoids its repeated
hash-table lookup without creating a second identity rule.")

(defcustom ekp-para-cache-limit 256
  "Maximum number of cached paragraphs.
When exceeded, the whole paragraph cache is flushed (cheap to rebuild)."
  :type 'natnum
  :group 'ekp)

(defvar ekp--params-explicit nil
  "Non-nil after `ekp-param-set' has been called.
Spacing parameters then persist until `ekp-param-reset'; when nil,
defaults are derived from each string.")

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

(defun ekp--spacing-signature ()
  "Return the spacing inputs that determine paragraph preprocessing."
  (if (and ekp--params-explicit (ekp--params-set-p))
      (list ekp-lws-ideal-pixel ekp-lws-stretch-pixel
            ekp-lws-shrink-pixel ekp-mws-ideal-pixel
            ekp-mws-stretch-pixel ekp-mws-shrink-pixel
            ekp-cws-ideal-pixel ekp-cws-stretch-pixel
            ekp-cws-shrink-pixel)
    (cons 'auto ekp-default-cws-stretch-pixel)))

(defun ekp--param-apply (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  "Set the nine spacing variables and derived limits (internal).
The nine pixel arguments are ideal/stretch/shrink per glue class:
LWS-I LWS-+ LWS-- for Latin word space, MWS-I MWS-+ MWS-- for mixed
Latin-CJK, and CWS-I CWS-+ CWS-- for CJK."
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
  "Set all spacing parameters explicitly; persist until `ekp-param-reset'.
The nine pixel values, in order, are LWS-I LWS-+ LWS-- MWS-I MWS-+
MWS-- CWS-I CWS-+ CWS--: ideal, stretch (+) and shrink (-) for the
Latin (LWS), mixed (MWS) and CJK (CWS) word spaces."
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
      (let ((parts nil)
            (hyphenated-p nil))
        (cond
         ((and (text-property-not-all 0 (length box) 'ekp--no-hyphen nil box)
               (setq parts (ekp--split-no-hyphen-box box)))
          nil)
         ((and (string-match word-re box)
                   ;; Never hyphenate inside a no-break span
                   (null (text-property-not-all 0 (length box)
                                                'ekp-no-break nil box))
                   (null (text-property-not-all 0 (length box)
                                                'ekp--no-hyphen nil box))
                   (or (memq ekp-hyphenation '(auto on))
                       (text-property-not-all
                        0 (length box) 'ekp--hyphenation nil box)))
          ;; Extract the groups BEFORE resolving the hyphenator:
          ;; dictionary compilation runs regexps of its own and
          ;; clobbers the match data.
          (let ((left (match-string 1 box))
                (word (match-string 2 box))
                (right (match-string 3 box)))
            (when (eq hyphenator 'unset)
              (setq hyphenator
                    (condition-case nil
                        (ekp-hyphen-create ekp-latin-lang)
                      (ekp-hyphen-dictionary-not-found nil))))
            (when hyphenator
              (setq hyphenated-p t)
              (setq parts (ekp-hyphen-boxes hyphenator word))
              (when (> (length left) 0)
                (setcar parts (concat left (car parts))))
              (when (> (length right) 0)
                (setcar (last parts)
                        (concat (car (last parts)) right)))))))
        (if parts
            ;; Latin word: hyphenated into syllable boxes
            (let ((n (length parts)))
              (push parts new-boxes)
              (dotimes (i n)
                (when (and hyphenated-p (< i (1- n)))
                  (push idx hyphen-idxs))
                (cl-incf idx)))
          ;; Non-Latin box, or hyphenation unavailable
          (push (list box) new-boxes)
          (cl-incf idx))))
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
  "Uncached `ekp--str-type' computation for STR."
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
  "Return the glue type between PREV-BOX-TYPE and CURR-BOX-TYPE.
It is `lws', `mws', `cws' or `nws'.  Lws means whitespace between
latin words; cws between cjk chars; mws between cjk and latin; nws
means no whitespace.  Space boxes (preserved whitespace) need no
additional glue."
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

(defconst ekp--private-policy-properties
  '(ekp--break-policy ekp--hyphenation ekp--literal-spacing
    ekp--policy-provenance ekp--automatic-no-break ekp--resolved-policy
    ekp--no-hyphen ekp--token-category ekp--downgraded-no-break
    ekp--face-break-policy)
  "Implementation-private properties used only during analysis.")

(defvar ekp--policy-measure nil
  "Current measure available to width-dependent policy compilation.")

(defvar ekp-cjk-no-line-start-extra)
(defvar ekp-cjk-no-line-end-extra)

(defun ekp--policy-signature ()
  "Return public policy inputs that affect paragraph construction."
  (ekp--copy-layout-context-value
   (list ekp-inline-code-policy ekp-hyphenation
         ekp-token-break-policies ekp-number-unit-suffixes
         ekp-kinsoku-profile ekp-cjk-no-line-start-extra
         ekp-cjk-no-line-end-extra ekp-overlong-token-policy)))

(defun ekp--strip-private-policy-properties (string)
  "Remove implementation-private policy properties from STRING."
  (remove-text-properties
   0 (length string)
   (apply #'append (mapcar (lambda (prop) (list prop nil))
                           ekp--private-policy-properties))
   string)
  string)

(defun ekp--private-policy-properties-p (string)
  "Return non-nil when STRING carries implementation-private policy properties."
  (catch 'found
    (dolist (interval (object-intervals string))
      (let ((properties (nth 2 interval)))
        (dolist (property ekp--private-policy-properties)
          (when (plist-member properties property)
            (throw 'found t)))))
    nil))

(defun ekp--clean-private-policy-source (string)
  "Return STRING without private policy properties, copying only when needed."
  (if (ekp--private-policy-properties-p string)
      (ekp--strip-private-policy-properties (copy-sequence string))
    string))

(defun ekp--latin-like-token-p (token)
  "Return non-nil when TOKEN is a bounded non-whitespace Latin-like token."
  (and (> (length token) 0)
       (not (string-match-p "\\s-" token))
       (seq-every-p (lambda (char) (<= (char-width char) 1)) token)))

(defun ekp--number-unit-token-p (token)
  "Return non-nil when TOKEN is compact number plus configured unit."
  (and (> (length token) 1)
       (<= ?0 (aref token 0) ?9)
       (seq-some
        (lambda (unit)
          (and (string-suffix-p unit token)
               (string-match-p
                "\\`[0-9]+\\(?:[.,][0-9]+\\)?\\'"
                (substring token 0 (- (length token) (length unit))))))
        ekp-number-unit-suffixes)))

(defun ekp--identifier-char-p (char)
  "Return non-nil when CHAR is a bounded identifier constituent."
  (or (and (<= ?A char) (<= char ?Z))
      (and (<= ?a char) (<= char ?z))
      (and (<= ?0 char) (<= char ?9))))

(defun ekp--identifier-start-char-p (char)
  "Return non-nil when CHAR may start an automatic identifier token."
  (or (and (<= ?A char) (<= char ?Z))
      (and (<= ?a char) (<= char ?z))))

(defun ekp--identifier-separator-end (token index)
  "Return separator end index in TOKEN at INDEX, or nil."
  (pcase (aref token index)
    ((or ?_ ?.) (1+ index))
    (?: (and (< (1+ index) (length token))
             (= (aref token (1+ index)) ?:) (+ index 2)))
    (?- (and (< (1+ index) (length token))
             (= (aref token (1+ index)) ?>) (+ index 2)))))

(defun ekp--identifier-break-indexes (token)
  "Return legal identifier break indexes for TOKEN, or nil if invalid."
  (let ((i 0) breaks valid)
    (when (and (> (length token) 1)
               (ekp--identifier-start-char-p (aref token 0))
               (ekp--identifier-char-p (aref token (1- (length token)))))
      (setq valid t)
      (while (and valid (< i (length token)))
        (let ((char (aref token i)))
          (cond
           ((ekp--identifier-char-p char)
            (when (and (> i 0)
                       (let ((prev (aref token (1- i))))
                         (or (and (<= ?a prev ?z) (<= ?A char ?Z))
                             (and (or (<= ?A prev ?Z) (<= ?a prev ?z))
                                  (<= ?0 char ?9)))))
              (push i breaks))
            (setq i (1+ i)))
           ((let ((end (ekp--identifier-separator-end token i)))
              (if (and end (> i 0) (< end (length token))
                       (ekp--identifier-char-p (aref token (1- i)))
                       (ekp--identifier-char-p (aref token end)))
                  (setq breaks (cons end breaks) i end)
                (setq valid nil)))))))
      (and valid breaks (nreverse breaks)))))

(defun ekp--identifier-token-p (token)
  "Return non-nil when TOKEN matches the bounded identifier grammar."
  (not (null (ekp--identifier-break-indexes token))))

(defun ekp--classify-token (token)
  "Return TOKEN's automatic category, or nil."
  (when (ekp--latin-like-token-p token)
    (cond
     ((or (string-prefix-p "www." token)
          (string-match-p "\\`[[:alpha:]][[:alnum:].+-]*://" token))
      'url)
     ((or (string-match-p ".+/.+" token)
          (string-match-p ".+\\\\.+" token)) 'path)
     ((ekp--number-unit-token-p token) 'number-unit)
     ((ekp--identifier-token-p token) 'identifier))))

(defun ekp--token-policy (category)
  "Return configured policy for token CATEGORY."
  (or (cdr (assq category ekp-token-break-policies)) 'normal))

(defun ekp--ordinary-overlong-token-p (string line-pixel)
  "Return non-nil if STRING has an ordinary token over LINE-PIXEL."
  (let ((pos 0) found)
    (while (and (not found) (string-match "\\S-+" string pos))
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (token (match-string 0 string)))
        (setq found
              (and (ekp--latin-like-token-p token)
                   (not (text-property-not-all
                         start end 'ekp-no-break nil string))
                   (> (ekp--measured-width (substring string start end))
                      line-pixel))))
      (setq pos (match-end 0)))
    found))

(defun ekp--natural-overlong-token-p (string line-pixel)
  "Return non-nil when STRING should bypass KP at LINE-PIXEL."
  (and (eq ekp-overlong-token-policy 'natural)
       (ekp--ordinary-overlong-token-p string line-pixel)))

(defun ekp--put-analysis-policy (string start end policy provenance)
  "Attach POLICY with PROVENANCE to STRING from START to END."
  (put-text-property start end 'ekp--break-policy policy string)
  (put-text-property start end 'ekp--policy-provenance provenance string)
  (pcase policy
    ('no-hyphen (put-text-property start end 'ekp--no-hyphen t string))
    ('no-break (put-text-property start end 'ekp--automatic-no-break t string))))

(defun ekp--clear-automatic-policy-properties (string start end)
  "Clear automatic private policy markers in STRING from START to END."
  (remove-text-properties
   start end
   '(ekp--break-policy nil ekp--hyphenation nil ekp--literal-spacing nil
     ekp--policy-provenance nil ekp--automatic-no-break nil
     ekp--resolved-policy nil ekp--no-hyphen nil ekp--token-category nil
     ekp--downgraded-no-break nil)
   string))

(defun ekp--break-policy-rank (policy)
  "Return restrictiveness rank for POLICY."
  (pcase policy
    ('no-break 3)
    ('no-hyphen 2)
    ('normal 1)
    (_ 0)))

(defun ekp--stricter-break-policy (left right)
  "Return the stricter automatic policy from LEFT and RIGHT."
  (if (> (ekp--break-policy-rank right)
         (ekp--break-policy-rank left))
      right
    left))

(defun ekp--token-width-if-needed (string start end token policy)
  "Return TOKEN width in STRING from START to END when needed."
  (when (and ekp--policy-measure
             (ekp--latin-like-token-p token)
             (not (text-property-not-all start end 'ekp-no-break nil string))
             (or (eq ekp-overlong-token-policy 'overflow)
                 (eq policy 'no-break)))
    (ekp--measured-width (substring string start end))))

(defun ekp--token-policy-specs (string)
  "Return automatic policy specs for STRING from a single token scan."
  (let ((pos 0) specs no-break-token-p latin-token-p)
    (while (string-match "\\S-+" string pos)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (token (match-string 0 string))
             (latin-p (and (ekp--latin-like-token-p token)
                           (not (text-property-not-all
                                 start end 'ekp-no-break nil string))))
             (category (ekp--classify-token token))
             (policy (and category (ekp--token-policy category)))
             (width (ekp--token-width-if-needed
                     string start end token policy))
             (overwide (and width (> width ekp--policy-measure)))
             downgraded)
        (when latin-p
          (setq latin-token-p t))
        (when (eq policy 'no-break)
          (setq no-break-token-p t))
        (when (and (eq ekp-overlong-token-policy 'overflow) overwide)
          (setq policy 'no-break))
        (when (and policy (not (eq policy 'normal)))
          (when (and (eq policy 'no-break)
                     (not (eq ekp-overlong-token-policy 'overflow))
                     overwide)
            (setq policy 'no-hyphen)
            (setq downgraded t))
          (push (list start end policy category downgraded) specs)))
      (setq pos (match-end 0)))
    (list (nreverse specs) no-break-token-p latin-token-p)))

(defun ekp--apply-token-spec (string spec)
  "Apply automatic token SPEC to STRING and return its interval."
  (pcase-let ((`(,start ,end ,policy ,category ,downgraded) spec))
    (ekp--put-analysis-policy string start end policy 'token)
    (put-text-property start end 'ekp--token-category category string)
    (when downgraded
      (put-text-property start end 'ekp--downgraded-no-break t string))
    (ekp--policy-interval-create
     :start start :end end :break-policy policy
     :hyphenation (if (eq policy 'no-hyphen) 'off nil)
     :literal-spacing nil :provenance 'token :category category)))

(defun ekp--face-policy-ranges (string)
  "Return automatic inline-face policy ranges in STRING."
  (let ((pos 0)
        (length (length string))
        raw-no-break-p
        ranges)
    (while (< pos length)
      (let* ((end (or (next-single-property-change
                       pos 'ekp--face-break-policy string length)
                      length))
             (policy (get-text-property
                      pos 'ekp--face-break-policy string)))
        (when (eq policy 'no-break)
          (setq raw-no-break-p t))
        (when (and (eq policy 'no-break) ekp--policy-measure
                   (> (ekp--measured-width
                       (ekp--clean-private-policy-source
                        (substring string pos end)))
                      ekp--policy-measure))
          (setq policy 'no-hyphen))
        (when (and (ekp--safe-break-policy-value-p policy)
                   (not (eq policy 'normal)))
          (push (list pos end policy) ranges))
        (setq pos end)))
    (list (nreverse ranges) raw-no-break-p)))

(defun ekp--region-policy-ranges (string)
  "Return explicit break-policy ranges in STRING."
  (seq-filter
   #'identity
   (mapcar
    (lambda (iv)
      (let ((policy (plist-get (nth 2 iv) 'ekp-break-policy)))
        (and (memq policy '(normal hyphenate no-hyphen))
             (list (nth 0 iv) (nth 1 iv) policy))))
    (object-intervals string))))

(defun ekp--policy-boundaries (length token-intervals face-ranges region-ranges)
  "Return sorted boundaries for LENGTH.
TOKEN-INTERVALS, FACE-RANGES, and REGION-RANGES supply policy spans."
  (let ((points (list 0 length)))
    (dolist (interval token-intervals)
      (push (ekp--policy-interval-start interval) points)
      (push (ekp--policy-interval-end interval) points))
    (dolist (range (append face-ranges region-ranges))
      (push (nth 0 range) points)
      (push (nth 1 range) points))
    (sort (delete-dups points) #'<)))

(defun ekp--policy-at (position ranges)
  "Return policy in RANGES active at POSITION."
  (seq-some
   (lambda (range)
     (and (<= (nth 0 range) position)
          (< position (nth 1 range))
          (nth 2 range)))
   ranges))

(defun ekp--token-interval-at (position intervals)
  "Return token interval from INTERVALS active at POSITION."
  (seq-find
   (lambda (interval)
     (and (<= (ekp--policy-interval-start interval) position)
          (< position (ekp--policy-interval-end interval))))
   intervals))

(defun ekp--downgraded-ranges (string)
  "Return ranges in STRING carrying downgraded automatic no-break markers."
  (seq-filter
   #'identity
   (mapcar
    (lambda (iv)
      (and (plist-get (nth 2 iv) 'ekp--downgraded-no-break)
           (list (nth 0 iv) (nth 1 iv))))
    (object-intervals string))))

(defun ekp--range-active-p (position ranges)
  "Return non-nil when POSITION is inside one of RANGES."
  (seq-some
   (lambda (range)
     (and (<= (car range) position) (< position (cadr range))))
   ranges))

(defun ekp--apply-effective-policy
    (string start end policy provenance category &optional downgraded)
  "Apply resolved POLICY to STRING from START to END."
  (ekp--put-analysis-policy string start end policy provenance)
  (when (eq provenance 'face)
    (put-text-property start end 'ekp--literal-spacing t string))
  (when category
    (put-text-property start end 'ekp--token-category category string))
  (when downgraded
    (put-text-property start end 'ekp--downgraded-no-break t string))
  (ekp--policy-interval-create
   :start start :end end :break-policy policy
   :hyphenation (if (eq policy 'no-hyphen) 'off nil)
   :literal-spacing (eq provenance 'face)
   :provenance provenance :category category))

(defun ekp--apply-merged-policies
    (string token-intervals face-ranges region-ranges downgraded-ranges)
  "Apply TOKEN-INTERVALS, FACE-RANGES, and REGION-RANGES to STRING."
  (let ((boundaries (ekp--policy-boundaries
                     (length string) token-intervals
                     face-ranges region-ranges))
        intervals
        region-intervals)
    (cl-loop for start in boundaries
             for end in (cdr boundaries)
             when (< start end)
             do
             (let* ((region (ekp--policy-at start region-ranges))
                    (token (ekp--token-interval-at start token-intervals))
                    (token-policy
                     (and token
                          (ekp--policy-interval-break-policy token)))
                    (face (ekp--policy-at start face-ranges))
                    (policy (or region
                                (ekp--stricter-break-policy
                                 token-policy face)))
                    (provenance
                     (cond (region 'region)
                           ((and face
                                 (>= (ekp--break-policy-rank face)
                                     (ekp--break-policy-rank token-policy)))
                            'face)
                           (token 'token)))
                    (category (and (not region) token
                                   (ekp--policy-interval-category token)))
                    (downgraded
                     (and token
                          (ekp--range-active-p start downgraded-ranges))))
               (if region
                   (progn
                     (put-text-property
                      start end 'ekp--policy-provenance 'region string)
                     (pcase region
                       ('hyphenate
                        (put-text-property
                         start end 'ekp--hyphenation 'on string)
                        (push (ekp--policy-interval-create
                               :start start :end end
                               :break-policy region :hyphenation 'on
                               :literal-spacing nil
                               :provenance 'region :category nil)
                              region-intervals))
                       ('normal
                        (push (ekp--policy-interval-create
                               :start start :end end
                               :break-policy region :hyphenation nil
                               :literal-spacing nil
                               :provenance 'region :category nil)
                              region-intervals))
                       ('no-hyphen
                        (push (ekp--apply-effective-policy
                               string start end region 'region nil)
                              region-intervals))))
                 (when (and policy (not (eq policy 'normal)))
                   (push (ekp--apply-effective-policy
                          string start end policy provenance category
                          downgraded)
                         intervals)))))
    (append (nreverse intervals) (nreverse region-intervals))))

(defun ekp--analyze-policies (string)
  "Return (ANALYSIS . INTERVALS) for STRING."
  (let* ((token-data (ekp--token-policy-specs string))
         (face-data (ekp--face-policy-ranges string))
         (token-specs (nth 0 token-data))
         (face-ranges (nth 0 face-data))
         (region-ranges (ekp--region-policy-ranges string)))
    (setq ekp--policy-analysis-sensitive-p
          (or (nth 1 face-data)
              (nth 1 token-data)
              (and (eq ekp-overlong-token-policy 'overflow)
                   (nth 2 token-data))))
    (if (and (null token-specs) (null face-ranges) (null region-ranges))
      (cons string nil)
    (let* ((analysis (copy-sequence string))
           (token-intervals (mapcar
                             (lambda (spec)
                               (ekp--apply-token-spec analysis spec))
                             token-specs))
           (downgraded-ranges (ekp--downgraded-ranges analysis))
           (intervals (ekp--apply-merged-policies
                       (progn
                         (ekp--clear-automatic-policy-properties
                          analysis 0 (length analysis))
                         analysis)
                       token-intervals face-ranges region-ranges
                       downgraded-ranges)))
      (cons analysis intervals)))))

(defun ekp--policy-analysis-base-key (string)
  "Return measure-independent policy-analysis cache key for STRING."
  (let ((source (ekp--clean-private-policy-source string))
        key-source copied)
    (setq key-source source)
    (dolist (iv (object-intervals string))
      (let ((plist (nth 2 iv)))
        (when (plist-member plist 'ekp--face-break-policy)
          (unless copied
            (setq key-source (copy-sequence source)
                  copied t))
          (put-text-property
           (nth 0 iv) (nth 1 iv) 'ekp--face-break-policy
           (plist-get plist 'ekp--face-break-policy) key-source))))
    (list key-source
          (prin1-to-string (ekp--key-intervals key-source))
          (ekp--policy-signature)
          (ekp--width-context))))

(defun ekp--cached-policy-analysis (string)
  "Return cached full policy analysis for STRING at current measure."
  (let* ((base-key (ekp--policy-analysis-base-key string))
         (entry (and ekp--policy-analysis-cache
                     (gethash base-key ekp--policy-analysis-cache)))
         (measure ekp--policy-measure))
    (cond
     ((and entry (not (car entry))) (cdr entry))
     ((and entry (gethash measure (cdr entry))))
     (t
      (let* ((ekp--policy-analysis-sensitive-p nil)
             (analysis (ekp--analyze-policies string))
             (sensitive ekp--policy-analysis-sensitive-p))
        (unless ekp--policy-analysis-cache
          (setq ekp--policy-analysis-cache
                (make-hash-table :test 'equal :size 100)))
        (when (>= (hash-table-count ekp--policy-analysis-cache)
                  ekp-para-cache-limit)
          (clrhash ekp--policy-analysis-cache))
        (if sensitive
            (let ((table (if (and entry (car entry))
                             (cdr entry)
                           (make-hash-table :test 'equal :size 4))))
              (puthash measure analysis table)
              (puthash base-key (cons t table) ekp--policy-analysis-cache))
          (puthash base-key (cons nil analysis) ekp--policy-analysis-cache))
        analysis)))))

(defun ekp--split-at-indexes (string indexes)
  "Split STRING at sorted character INDEXES, preserving properties."
  (let ((start 0) parts)
    (dolist (end indexes)
      (when (> end start)
        (push (substring string start end) parts))
      (setq start end))
    (when (< start (length string))
      (push (substring string start) parts))
    (nreverse parts)))

(defun ekp--path-break-indexes (string &optional skip-first)
  "Return legal path break indexes for STRING.
When SKIP-FIRST is non-nil, do not break after the first separator."
  (let (indexes seen)
    (dotimes (i (length string))
      (when (memq (aref string i) '(?/ ?\\))
        (if (and skip-first (not seen))
            (setq seen t)
          (push (1+ i) indexes))))
    (nreverse indexes)))

(defun ekp--split-no-hyphen-box (box)
  "Return no-hyphen BOX split at legal token boundaries, or nil."
  (pcase (get-text-property 0 'ekp--token-category box)
    ('path (ekp--split-at-indexes
            box (ekp--path-break-indexes
                 box (get-text-property 0 'ekp--downgraded-no-break box))))
    ('url (ekp--split-at-indexes box (ekp--path-break-indexes box)))
    ('identifier
     (ekp--split-at-indexes box (ekp--identifier-break-indexes box)))
    (_ nil)))

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

(defconst ekp--ja-no-line-start-extra
  (concat "ぁぃぅぇぉっゃゅょゎゕゖァィゥェォッャュョヮヵヶ"
          "ㇰㇱㇲㇳㇴㇵㇶㇷㇸㇹㇺㇻㇼㇽㇾㇿ"
          "ーゝゞヽヾ々〻")
  "Immutable Japanese letters that must not start a line.")

(defcustom ekp-cjk-no-line-start-extra ""
  "Custom/Japanese CJK letters that must not start a line.
Used only by the `custom' kinsoku profile.  The `ja' profile uses
an immutable built-in Japanese addition set."
  :type 'string
  :safe #'stringp
  :group 'ekp)

(defcustom ekp-cjk-no-line-end-extra ""
  "Custom CJK characters that must not end a line.
Used only by the `custom' kinsoku profile."
  :type 'string
  :safe #'stringp
  :group 'ekp)

(defun ekp--char-in-string-p (char string)
  "Return non-nil when CHAR occurs in STRING."
  (and (stringp string) (memq char (append string nil))))

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
  "Non-nil if BOX must not appear at the start of a line.
BOX-TYPE is BOX's (start . end) type pair from `ekp--box-type'."
  (and (not (eq ekp-kinsoku-profile 'off))
       (or (eq (car box-type) 'cjk-close)
           (ekp--box-pure-set-p box ekp--no-line-start-char-list)
           (and (> (length box) 0)
                (eq ekp-kinsoku-profile 'ja)
                (ekp--char-in-string-p
                 (aref box 0) ekp--ja-no-line-start-extra))
           (and (> (length box) 0)
                (eq ekp-kinsoku-profile 'custom)
                (ekp--char-in-string-p
                 (aref box 0) ekp-cjk-no-line-start-extra)))))

(defun ekp--box-no-line-end-p (box box-type)
  "Non-nil if BOX must not appear at the end of a line.
BOX-TYPE is BOX's (start . end) type pair from `ekp--box-type'."
  (and (not (eq ekp-kinsoku-profile 'off))
       (or (eq (cdr box-type) 'cjk-open)
           (ekp--box-pure-set-p box ekp--no-line-end-char-list)
           (and (> (length box) 0)
                (eq ekp-kinsoku-profile 'custom)
                (ekp--char-in-string-p
                 (aref box (1- (length box)))
                 ekp-cjk-no-line-end-extra)))))

(defun ekp--compute-glue-types (boxes boxes-types hyphen-positions)
  "Compute the glue-type vector for BOXES using BOXES-TYPES.
Positions right after HYPHEN-POSITIONS are forced to `nws'."
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
  "Return the ideal glue pixel width for glue TYPE."
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-ideal-pixel)
        ((eq 'mws type) ekp-mws-ideal-pixel)
        ((eq 'cws type) ekp-cws-ideal-pixel)))

(defun ekp-glue-min-pixel (type)
  "Return the minimum glue pixel width for glue TYPE."
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-min-pixel)
        ((eq 'mws type) ekp-mws-min-pixel)
        ((eq 'cws type) ekp-cws-min-pixel)))

(defun ekp-glue-max-pixel (type)
  "Return the maximum glue pixel width for glue TYPE."
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

(defun ekp--para-glue-stretch (para type)
  "Get stretch amount for TYPE using PARA's stored glue params."
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-stretch))
          ((eq 'mws type) (plist-get params :mws-stretch))
          ((eq 'cws type) (plist-get params :cws-stretch)))))

(defun ekp--para-glue-max (para type)
  "Return the maximum glue pixel (ideal + stretch) for TYPE in PARA."
  (+ (ekp--para-glue-ideal para type)
     (ekp--para-glue-stretch para type)))

;;; ============================================================
;;; Cache Implementation
;;; ============================================================

(defconst ekp--key-ignored-props '(fontified jit-lock-defer-multiline)
  "Text properties that never affect layout and churn constantly.
Font-lock flips `fontified' as text scrolls into view; keeping it in
cache keys would alias one paragraph into several entries and halve
the hit rate in fontified buffers.")

(defun ekp--key-intervals (string)
  "Property intervals of STRING with volatile bookkeeping removed.
Like `object-intervals', minus `ekp--key-ignored-props'; intervals
left with no properties are dropped entirely."
  (let (out)
    (dolist (iv (object-intervals string))
      (let ((plist (nth 2 iv)) filtered)
        (while plist
          (unless (memq (car plist) ekp--key-ignored-props)
            (push (car plist) filtered)
            (push (cadr plist) filtered))
          (setq plist (cddr plist)))
        (when filtered
          (let ((start (nth 0 iv))
                (end (nth 1 iv))
                (props (nreverse filtered)))
            (if (and out (= (nth 1 (car out)) start)
                     (equal (nth 2 (car out)) props))
                (setcar (cdr (car out)) end)
              (push (list start end props) out))))))
    (nreverse out)))

(defvar ekp--box-width-cache (make-hash-table :test 'equal :size 4096)
  "Global measurement cache: box key → pixel width.
Keys are the bare string for property-free boxes, else
\(STRING . FILTERED-INTERVALS).  Cross-paragraph: the same character
or word is measured once per Emacs session, not once per paragraph
\(CJK text repeats a small alphabet of glyphs constantly).  Flushed
by `ekp-clear-caches' — required after font or theme changes, as
before.")

(defvar ekp--box-width-cache-limit 65536
  "Entry cap for `ekp--box-width-cache'; the cache is flushed beyond it.")

(defun ekp--string-pixel-width (string)
  "Pixel width of STRING as it will render in the current buffer.
Like `string-pixel-width', but honors the current buffer's
`face-remapping-alist' — which is where `text-scale-mode', themes
and mode-specific font tweaks live.  Plain `string-pixel-width'
measures in a bare hidden buffer, so in any buffer with remapped
faces it reports the wrong font's metrics and every \"pixel-exact\"
line comes out wrong on screen (Emacs 31 grew a BUFFER argument for
exactly this; this is the 29/30-compatible equivalent)."
  (if (null face-remapping-alist)
      (string-pixel-width string)
    (let ((remap face-remapping-alist))
      (with-current-buffer (get-buffer-create " *ekp-pixel-width*" t)
        (setq-local face-remapping-alist remap)
        (delete-region (point-min) (point-max))
        ;; Keep line-affecting context out, like string-pixel-width.
        (setq-local line-prefix nil wrap-prefix nil)
        (insert string)
        (prog1 (car (buffer-text-pixel-size nil nil t))
          (delete-region (point-min) (point-max)))))))

(defun ekp--width-context ()
  "The display context that box measurement depends on.
nil in an unremapped buffer (the common case); otherwise the
buffer's `face-remapping-alist', which changes glyph metrics and
therefore must key every measurement and paragraph cache entry."
  face-remapping-alist)

(defun ekp--measured-width (str)
  "Pixel width of STR in the current display context, cached."
  (let* ((source (ekp--clean-private-policy-source str))
         (ivs (ekp--key-intervals source))
         (ctx (ekp--width-context))
         (key (cond ((and (null ivs) (null ctx)) source)
                    ((null ctx) (cons source ivs))
                    (t (list source ivs ctx)))))
    (or (gethash key ekp--box-width-cache)
        (progn
          (when (>= (hash-table-count ekp--box-width-cache)
                    ekp--box-width-cache-limit)
            (clrhash ekp--box-width-cache))
          (puthash key (ekp--string-pixel-width source)
                   ekp--box-width-cache)))))

(defun ekp--resolved-emergency-stretch-pixel ()
  "Return the fixed final-pass emergency stretch budget in pixels."
  (or ekp-emergency-stretch-pixel
      (* 3 (max 1 (ekp--measured-width "M")))))

(defun ekp--para-key (string &optional policy-analysis)
  "Compute cache key for STRING and optional POLICY-ANALYSIS.
The key is a structure compared with `equal', so hash collisions
cannot alias two different paragraphs.  It covers: characters, text
properties, detected fonts, the hyphenation language, and the
effective policy intervals, and the effective spacing signature \(nine
explicit values or the auto CJK stretch default when the other defaults
are derived per string)."
  (let* ((source (ekp--clean-private-policy-source string))
         (latin-font (ekp-latin-font source))
         (cjk-font (ekp-cjk-font source))
        (policy-analysis (or policy-analysis
                             (ekp--analyze-policies string))))
    (list source
          (prin1-to-string (ekp--key-intervals source))
          latin-font cjk-font
          ;; Buffers with remapped faces (text-scale, themes) render
          ;; — and therefore measure — differently: never alias their
          ;; paragraphs with an unremapped buffer's.
          (ekp--width-context)
          ekp-latin-lang
          ekp-alignment
          ekp-ragged-stretch-pixel
          (and ekp-protrusion ekp-protrusion-ratios)
          ekp-parshape
          ekp-first-line-indent
          (ekp--policy-signature)
          (cdr policy-analysis)
          (ekp--spacing-signature))))

(defun ekp--measure-boxes (boxes uniform-props)
  "Measure pixel widths of BOXES, deduplicating identical boxes.
Identity = same characters AND same text properties.  When
UNIFORM-PROPS is non-nil (the whole paragraph carries at most one
property run), plain string equality suffices as the paragraph-local
key.  Misses fall through to the session-global width cache, so a
glyph shared across paragraphs is measured only once."
  (let* ((n (length boxes))
         (seen (make-hash-table :test 'equal :size n))
         (widths (make-vector n 0)))
    (dotimes (i n)
      (let* ((box (aref boxes i))
             (key (if uniform-props box
                    (cons box (object-intervals box))))
             (w (gethash key seen)))
        (unless w
          (setq w (ekp--measured-width box))
          (puthash key w seen))
        (aset widths i w)))
    widths))

(defun ekp--hyphen-width-for (string)
  "Pixel width of the hyphen char, styled like STRING's first char."
  (let ((props (and (> (length string) 0) (text-properties-at 0 string))))
    (ekp--measured-width (if props (apply #'propertize "-" props) "-"))))

(defun ekp--space-box-type-p (box-type)
  "Return non-nil if BOX-TYPE describes a whitespace box."
  (and box-type (eq (car box-type) 'space)))

(defun ekp--tail-protrude-pixel (box box-type)
  "Pixels the last visible char of BOX may protrude past the flush edge.
BOX-TYPE is BOX's (start . end) type pair from `ekp--box-type'."
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
          (floor (* ratio (ekp--measured-width last-str)))
        0))))

(defun ekp--line-edge-release (para _start end)
  "Pixels released at the right edge of PARA's line [START, END).
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

(defun ekp--first-indent-pixel (para)
  "Resolve `ekp-first-line-indent' to pixels for PARA.
Goes through the width cache: this runs for every rendered line."
  (cond
   ((numberp ekp-first-line-indent) ekp-first-line-indent)
   (ekp-first-line-indent
    (* 2 (ekp--measured-width
          (propertize "字" 'face
                      (list :family (ekp-para-cjk-font para))))))
   (t 0)))

(defun ekp--line-spec (para line-index measure)
  "Layout of PARA's LINE-INDEX (0-based) as (INDENT . WIDTH).
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

(defun ekp--glue-params-snapshot ()
  "Return current paragraph glue parameters as a stable plist."
  (let ((justify (eq ekp-alignment 'justify)))
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
          :extra-stretch (if justify 0 (ekp--ragged-extra-stretch)))))

(defun ekp--make-para (string &optional policy-analysis)
  "Create and fully initialize `ekp-para' for STRING.
POLICY-ANALYSIS is a precomputed result from `ekp--analyze-policies'."
  ;; Ensure params: explicit params persist; otherwise derive defaults
  ;; from this string's font.
  (unless (and ekp--params-explicit (ekp--params-set-p))
    (ekp-param-set-default string))
  ;; Extract fonts
  (let* ((source (ekp--clean-private-policy-source string))
         (policy-analysis (or policy-analysis
                              (ekp--analyze-policies string)))
         (analysis-string (car policy-analysis))
         (resolved-policies (cdr policy-analysis))
         (latin-font (ekp-latin-font source))
         (cjk-font (ekp-cjk-font source))
         ;; Split into boxes with hyphenation
         (split-result (ekp--split-with-hyphen analysis-string))
         (boxes (car split-result))
         (hyphen-positions (cdr split-result))
         (n (length boxes))
         ;; Compute box properties
         (boxes-widths (ekp--measure-boxes
                        boxes (null (cdr (object-intervals source)))))
         (boxes-types (vconcat (mapcar #'ekp--box-type boxes)))
         (glues-types (ekp--compute-glue-types
                       boxes boxes-types hyphen-positions))
         (hyphen-pixel (ekp--hyphen-width-for source))
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
    ;; - it lies strictly inside an `ekp-no-break' span,
    ;; - it would move a literal source-space box to line start, or
    ;; - a no-break joiner character (NBSP & friends) touches it.
    ;; Unbreakable gaps carry no glue: punctuation hugs its content,
    ;; atoms stay rigid, NBSP supplies its own spacing.
    (let ((k 1))
      (while (< k n)
        (let* ((prev-box (aref boxes (1- k)))
               (curr-box (aref boxes k))
               (prev-last (aref prev-box (1- (length prev-box))))
               (curr-first (aref curr-box 0))
               (literal-gap
                (and (get-text-property (1- (length prev-box))
                                        'ekp--literal-spacing prev-box)
                     (get-text-property 0 'ekp--literal-spacing
                                        curr-box)))
               (literal-line-start-space
                (and literal-gap (ekp--box-space-p curr-box))))
          (when (or (ekp--box-no-line-end-p prev-box
                                            (aref boxes-types (1- k)))
                    (ekp--box-no-line-start-p curr-box
                                              (aref boxes-types k))
                    (and (get-text-property (1- (length prev-box))
                                            'ekp-no-break prev-box)
                         (get-text-property 0 'ekp-no-break curr-box))
                    (and (get-text-property (1- (length prev-box))
                                            'ekp--automatic-no-break prev-box)
                         (get-text-property 0 'ekp--automatic-no-break
                                            curr-box))
                    literal-line-start-space
                    (memq prev-last ekp--no-break-joiner-chars)
                    (memq curr-first ekp--no-break-joiner-chars))
            (aset breaks-allowed k nil)
            (push k forbidden)
            (unless (or literal-gap (eq (aref glues-types k) 'nws))
              (aset glues-types k 'nws))))
        (setq k (1+ k))))
    ;; Remove private analysis markers after they have been compiled
    ;; into hyphen positions and break permissions.
    (dotimes (i n)
      (aset boxes i (ekp--strip-private-policy-properties
                     (copy-sequence (aref boxes i)))))
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
     :string source
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
     :resolved-policies resolved-policies
     :glue-params (ekp--glue-params-snapshot)
     :dp-cache (make-hash-table :test 'equal :size 20))))

(defun ekp--append-prefix-vector (prefix count suffix)
  "Return PREFIX through COUNT followed by SUFFIX as a vector."
  (vconcat (cl-subseq prefix 0 count) suffix))

(defun ekp--append-hyphen-positions (para stable tail-positions)
  "Merge PARA hyphen positions before STABLE with TAIL-POSITIONS."
  (vconcat
   (seq-filter
    (lambda (position) (< position stable))
    (ekp-para-hyphen-positions para))
   (mapcar
    (lambda (position) (+ stable position))
    (append tail-positions nil))))

(defun ekp--append-offsets (prefix stable tail-offsets cutoff)
  "Merge PREFIX offsets before STABLE with TAIL-OFFSETS at CUTOFF."
  (vconcat
   (cl-subseq prefix 0 stable)
   (mapcar
    (lambda (range)
      (cons (+ cutoff (car range)) (+ cutoff (cdr range))))
    (append tail-offsets nil))))

(defun ekp--gap-natural-at (string boxes offsets right)
  "Return STRING's natural gap width before box RIGHT in BOXES and OFFSETS."
  (let* ((left (1- right))
         (start (cdr (aref offsets left)))
         (end (car (aref offsets right)))
         (source (if (< start end)
                     (substring string start end)
                   (car (last (string-glyph-split
                               (aref boxes left)))))))
    (ekp--measured-width source)))

(defun ekp--append-gap-naturals (para string boxes offsets stable)
  "Reuse PARA gaps before STABLE and measure STRING's BOXES via OFFSETS."
  (let* ((old (or (ekp-para-gap-naturals-memo para)
                  (ekp--gap-natural-pixels
                   para (ekp-para-box-offsets-memo para))))
         (naturals
          (vconcat (cl-subseq old 0 stable)
                   (make-vector (- (length boxes) stable) 0))))
    (cl-loop for right from (max 1 stable) below (length boxes)
             do (aset naturals right
                      (ekp--gap-natural-at string boxes offsets right)))
    naturals))

(defun ekp--append-cutoff (old string)
  "Return an append-safe source cutoff from OLD into STRING, or nil."
  (when (and (< (length old) (length string))
             (null (ekp--key-intervals old))
             (null (ekp--key-intervals string))
             (not (string-match-p "[\n\t]" string))
             (string-prefix-p old string))
    (let ((tail (1- (length old))))
      (while (and (>= tail 0) (= (aref old tail) ?\s))
        (setq tail (1- tail)))
      (when (>= tail 0)
        (when-let ((space
                    (cl-position ?\s old :from-end t :end (1+ tail))))
          (1+ space))))))

(defun ekp--para-has-box-type-p (para type)
  "Return non-nil when PARA has a box with TYPE at either edge."
  (seq-some
   (lambda (box-type)
     (if (eq type 'cjk)
         (or (memq (car box-type) '(cjk cjk-open cjk-close))
             (memq (cdr box-type) '(cjk cjk-open cjk-close)))
       (or (eq type (car box-type))
           (eq type (cdr box-type)))))
   (append (ekp-para-boxes-types para) nil)))

(defun ekp--append-stable-box-count (offsets cutoff)
  "Return the box index in OFFSETS beginning at CUTOFF."
  (let ((position (1- (length offsets)))
        found)
    (while (and (>= position 0) (not found))
      (if (= (car (aref offsets position)) cutoff)
          (setq found position)
        (setq position (1- position))))
    found))

(defun ekp--copy-vector-prefix (source length count initial)
  "Return LENGTH vector initialized from SOURCE's first COUNT entries."
  (vconcat (cl-subseq (if (bool-vector-p source)
                          (vconcat source)
                        source)
                      0 count)
           (make-vector (- length count) initial)))

(defun ekp--append-glue-types (para boxes types hyphens stable)
  "Extend PARA glue types for BOXES from STABLE using TYPES and HYPHENS."
  (let ((glues (ekp--copy-vector-prefix
                (ekp-para-glues-types para) (length boxes) stable nil)))
    (dolist (position (append hyphens nil))
      (when (>= position (1- stable))
        (aset glues (1+ position) 'nws)))
    (cl-loop for index from stable below (length boxes)
             unless (aref glues index)
             do (aset glues index
                      (ekp--glue-type
                       (and (> index 0) (aref types (1- index)))
                       (aref types index))))
    (cl-loop for position from stable below (length boxes)
             when (ekp--append-break-forbidden-p boxes types position)
             do (aset glues position 'nws))
    glues))

(defun ekp--append-prefix-data (para stable widths types glues)
  "Extend PARA prefix data from STABLE using WIDTHS, TYPES, and GLUES."
  (let* ((n (length widths))
         (prefix-count (1+ stable))
         (ideal (ekp--copy-vector-prefix
                 (ekp-para-ideal-prefixs para) (1+ n) prefix-count 0))
         (minimum (ekp--copy-vector-prefix
                   (ekp-para-min-prefixs para) (1+ n) prefix-count 0))
         (maximum (ekp--copy-vector-prefix
                   (ekp-para-max-prefixs para) (1+ n) prefix-count 0))
         (g-ideal (ekp--copy-vector-prefix
                   (ekp-para-glue-ideals para) n stable 0))
         (g-shrink (ekp--copy-vector-prefix
                    (ekp-para-glue-shrinks para) n stable 0))
         (g-stretch (ekp--copy-vector-prefix
                     (ekp-para-glue-stretches para) n stable 0))
         (lws (ekp--copy-vector-prefix
               (ekp-para-lws-prefixs para) (1+ n) prefix-count 0))
         (mws (ekp--copy-vector-prefix
               (ekp-para-mws-prefixs para) (1+ n) prefix-count 0))
         (cws (ekp--copy-vector-prefix
               (ekp-para-cws-prefixs para) (1+ n) prefix-count 0))
         (lead (ekp--copy-vector-prefix
                (ekp-para-lead-spaces para) (1+ n) stable 0))
         (trail (ekp--copy-vector-prefix
                 (ekp-para-trail-spaces para) (1+ n) prefix-count 0)))
    (cl-loop for index from stable below n do
             (let* ((width (aref widths index))
                    (type (aref glues index))
                    (gi (ekp-glue-ideal-pixel type))
                    (gmin (if (eq ekp-alignment 'justify)
                              (ekp-glue-min-pixel type) gi))
                    (gmax (if (eq ekp-alignment 'justify)
                              (ekp-glue-max-pixel type) gi)))
               (aset g-ideal index gi)
               (aset g-shrink index (- gi gmin))
               (aset g-stretch index (- gmax gi))
               (aset ideal (1+ index)
                     (+ (aref ideal index) width gi))
               (aset minimum (1+ index)
                     (+ (aref minimum index) width gmin))
               (aset maximum (1+ index)
                     (+ (aref maximum index) width gmax))
               (aset lws (1+ index)
                     (+ (aref lws index) (if (eq type 'lws) 1 0)))
               (aset mws (1+ index)
                     (+ (aref mws index) (if (eq type 'mws) 1 0)))
               (aset cws (1+ index)
                     (+ (aref cws index) (if (eq type 'cws) 1 0)))
               (aset trail (1+ index)
                     (if (ekp--space-box-type-p (aref types index))
                         (+ (aref trail index) width) 0))))
    (cl-loop for index downfrom (1- n) to stable
             do (aset lead index
                      (if (ekp--space-box-type-p (aref types index))
                          (+ (aref widths index) (aref lead (1+ index)))
                        0)))
    (aset lead 0 0)
    (vector ideal minimum maximum g-ideal g-shrink g-stretch
            lws mws cws lead trail)))

(defun ekp--append-break-forbidden-p (boxes types position)
  "Return non-nil when BOXES of TYPES may not break at POSITION."
  (let* ((previous (aref boxes (1- position)))
         (current (aref boxes position))
         (previous-last (aref previous (1- (length previous))))
         (current-first (aref current 0)))
    (or (ekp--box-no-line-end-p previous (aref types (1- position)))
        (ekp--box-no-line-start-p current (aref types position))
        (memq previous-last ekp--no-break-joiner-chars)
        (memq current-first ekp--no-break-joiner-chars))))

(defun ekp--append-break-data (para boxes types stable)
  "Extend PARA break permissions for BOXES of TYPES from STABLE."
  (let* ((n (length boxes))
         (breaks (ekp--copy-vector-prefix
                  (ekp-para-breaks-allowed para)
                  (1+ n) stable t))
         (forbidden
          (seq-filter
           (lambda (position) (< position stable))
           (ekp-para-forbidden-positions para))))
    (cl-loop for position from stable below n
             when (ekp--append-break-forbidden-p boxes types position)
             do (aset breaks position nil)
             and do (push position forbidden))
    (cons breaks (vconcat (sort (append forbidden nil) #'<)))))

(defun ekp--append-tail-protrudes (para boxes types stable)
  "Extend PARA tail protrusions for BOXES of TYPES from STABLE."
  (let* ((n (length boxes))
         (tail (ekp--copy-vector-prefix
                (ekp-para-tail-protrudes para) (1+ n) (1+ stable) 0)))
    (when ekp-protrusion
      (cl-loop for position from (1+ stable) to n
               for index = (1- position)
               do (aset tail position
                        (if (ekp--space-box-type-p (aref types index))
                            (aref tail (1- position))
                          (ekp--tail-protrude-pixel
                           (aref boxes index) (aref types index))))))
    tail))

(defun ekp--append-para-record
    (para string stable boxes widths types glues hyphens offsets gaps)
  "Extend PARA with STRING after STABLE.
Use BOXES, WIDTHS, TYPES, GLUES, HYPHENS, OFFSETS, and GAPS."
  (let* ((prefix (ekp--append-prefix-data
                  para stable widths types glues))
         (breaks (ekp--append-break-data para boxes types stable))
         (extended (copy-ekp-para para)))
    (setf (ekp-para-string extended) string
          (ekp-para-boxes extended) boxes
          (ekp-para-boxes-widths extended) widths
          (ekp-para-boxes-types extended) types
          (ekp-para-glues-types extended) glues
          (ekp-para-hyphen-positions extended) hyphens
          (ekp-para-ideal-prefixs extended) (aref prefix 0)
          (ekp-para-min-prefixs extended) (aref prefix 1)
          (ekp-para-max-prefixs extended) (aref prefix 2)
          (ekp-para-glue-ideals extended) (aref prefix 3)
          (ekp-para-glue-shrinks extended) (aref prefix 4)
          (ekp-para-glue-stretches extended) (aref prefix 5)
          (ekp-para-lws-prefixs extended) (aref prefix 6)
          (ekp-para-mws-prefixs extended) (aref prefix 7)
          (ekp-para-cws-prefixs extended) (aref prefix 8)
          (ekp-para-lead-spaces extended) (aref prefix 9)
          (ekp-para-trail-spaces extended) (aref prefix 10)
          (ekp-para-breaks-allowed extended) (car breaks)
          (ekp-para-forbidden-positions extended) (cdr breaks)
          (ekp-para-tail-protrudes extended)
          (ekp--append-tail-protrudes para boxes types stable)
          (ekp-para-box-offsets-memo extended) offsets
          (ekp-para-gap-naturals-memo extended) gaps
          (ekp-para-dp-cache extended)
          (make-hash-table :test 'equal :size 20))
    extended))

(defun ekp--append-para (para string)
  "Return (NEW-PARA . STABLE-BOXES) for plain STRING appended to PARA.
Return nil when the tokenizer prefix cannot be reused exactly."
  (let* ((old (ekp-para-string para))
         (cutoff (and (> (length old) 0)
                      (ekp--append-cutoff old string)))
         (tail (and cutoff (substring string cutoff)))
         (latin-font
          (and tail
               (if (ekp--para-has-box-type-p para 'latin)
                   (ekp-para-latin-font para)
                 (ekp-latin-font tail))))
         (cjk-font
          (and tail
               (if (ekp--para-has-box-type-p para 'cjk)
                   (ekp-para-cjk-font para)
                 (ekp-cjk-font tail))))
         (fonts-stable
          (and cutoff
               (equal (ekp-para-latin-font para) latin-font)
               (equal (ekp-para-cjk-font para) cjk-font)))
         (old-offsets (ekp-para-box-offsets-memo para))
         (stable (and fonts-stable old-offsets
                      (ekp--append-stable-box-count old-offsets cutoff))))
    (when (and stable (> stable 0))
      (let* ((split (ekp--split-with-hyphen tail))
             (tail-boxes (car split))
             (boxes (ekp--append-prefix-vector
                     (ekp-para-boxes para) stable tail-boxes))
             (widths (ekp--append-prefix-vector
                      (ekp-para-boxes-widths para) stable
                      (ekp--measure-boxes tail-boxes t)))
             (types (ekp--append-prefix-vector
                     (ekp-para-boxes-types para) stable
                     (vconcat (mapcar #'ekp--box-type tail-boxes))))
             (hyphens (ekp--append-hyphen-positions para stable (cdr split)))
             (glues (ekp--append-glue-types
                     para boxes types hyphens stable))
             (offsets (ekp--append-offsets
                       old-offsets stable
                       (ekp--box-offsets tail (append tail-boxes nil))
                       cutoff))
             (gaps (ekp--append-gap-naturals
                    para string boxes offsets stable)))
        (cons (ekp--append-para-record
               para string stable boxes widths types glues
               hyphens offsets gaps)
              stable)))))

(defun ekp--get-para (string)
  "Get or create `ekp-para' struct for STRING.
This is the main entry point for cached paragraph data."
  (let* ((policy-analysis (ekp--cached-policy-analysis string))
         (key (ekp--para-key string policy-analysis))
         (source (car key)))
    (if (and ekp--last-para
             (eq (car ekp--last-para) source)
             (equal (nth 1 ekp--last-para) key))
        (nth 2 ekp--last-para)
      (unless ekp--para-cache
        (setq ekp--para-cache (make-hash-table :test 'equal :size 100)))
      (let ((para (or (gethash key ekp--para-cache)
                      (progn
                        (when (>= (hash-table-count ekp--para-cache)
                                  ekp-para-cache-limit)
                          (clrhash ekp--para-cache))
                        (let ((new-para
                               (ekp--make-para string policy-analysis)))
                          (puthash key new-para ekp--para-cache)
                          new-para)))))
        (setq ekp--last-para (list source key para))
        para))))

;;;###autoload
(defun ekp-clear-caches ()
  "Clear all paragraph and measurement caches.
Run after font or theme changes that affect glyph widths."
  (interactive)
  (setq ekp--para-cache nil)
  (setq ekp--policy-analysis-cache nil)
  (setq ekp--last-para nil)
  (clrhash ekp--box-width-cache))

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
  "Classify line tightness from ADJUSTMENT-PIXEL and FLEXIBILITY-PIXEL.
Return the fitness class 0-3: 0=tight (shrunk), 1=decent, 2=loose,
3=very-loose."
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
END-WITH-HYPHENP is non-nil when the line ends at a hyphen point, and
PREV-HYPHEN-COUNT counts the consecutive hyphenated lines before it.
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

(defsubst ekp--line-stripped-space-pixel
    (raw-pixel start end lead-spaces trail-spaces)
  "Return edge-space width excluded from RAW-PIXEL for line START..END.
LEAD-SPACES and TRAIL-SPACES are the paragraph's precomputed run vectors."
  (min raw-pixel
       (+ (aref lead-spaces start) (aref trail-spaces end))))

(defun ekp--gaps-between (para i k)
  "Return (latin-gaps mix-gaps cjk-gaps) for PARA inside line I..K.
Counts glue indices I+1 .. K-1 using precomputed prefix counts."
  (let ((lp (ekp-para-lws-prefixs para))
        (mp (ekp-para-mws-prefixs para))
        (cp (ekp-para-cws-prefixs para))
        (j (1+ i)))
    (list (- (aref lp k) (aref lp j))
          (- (aref mp k) (aref mp j))
          (- (aref cp k) (aref cp j)))))

(defun ekp--line-ideal-pixel (para i k)
  "Return the ideal width of line I..K in PARA.
It sums box and glue ideals, subtracts leading glue and stripped
space-box runs, and adds the hyphen width when the line hyphenates."
  (let* ((ip (ekp-para-ideal-prefixs para))
         (raw (- (aref ip k) (aref ip i)
                 (aref (ekp-para-glue-ideals para) i)))
         (space-w
          (ekp--line-stripped-space-pixel
           raw i k (ekp-para-lead-spaces para)
           (ekp-para-trail-spaces para)))
         (ideal (- raw space-w)))
    (if (ekp--hyphenate-p (ekp-para-hyphen-positions para) (1- k))
        (+ ideal (ekp-para-hyphen-pixel para))
      ideal)))

;;;; Dynamic Programming Line Breaking
;;
;; Design notes:
;; - All line metrics are O(1) via prefix arrays.
;; - Two-pass strategy: a strict Knuth-Plass pass runs first.  If the
;;   paragraph end is unreachable, the final pass adds finite background
;;   emergency stretch to every underfull candidate and still scores it
;;   through the normal badness/demerits path.  If an overfull candidate
;;   would otherwise remove the last surviving path, the final pass records
;;   TeX's zero-increment artificial demerits break.  The C engine implements
;;   the identical strategy.

(defsubst ekp--dp-key (line-pixel)
  "Return the complete DP cache signature for LINE-PIXEL.
The paragraph owns width-independent layout data; this key captures
every remaining runtime input read by the Elisp and C DP engines."
  (list line-pixel
        ekp-looseness
        ekp-line-penalty
        ekp-hyphen-penalty
        ekp-adjacent-fitness-penalty
        ekp-consecutive-hyphen-penalty
        ekp-last-line-short-penalty
        ekp-last-line-min-ratio
        (ekp--resolved-emergency-stretch-pixel)))

(defun ekp--dp-cache-elisp (para line-pixel)
  "Return and cache the dp-result plist for PARA at LINE-PIXEL.
This pure-Elisp DP path handles looseness and parshape via the
position-by-line-count DP; a plain first-line indent uses the 1D
pass, where line 0 starts at box 0."
  (if (or (/= ekp-looseness 0) ekp-parshape)
      (ekp--dp-cache-elisp-loose para line-pixel)
    (let ((dp-result (or (ekp--dp-run-1d para line-pixel nil)
                         (ekp--dp-run-1d para line-pixel t))))
      (puthash (ekp--dp-key line-pixel) dp-result (ekp-para-dp-cache para))
      dp-result)))

(defun ekp--hyphen-flags (hyphen-positions n)
  "Return a `bool-vector' of length N flagging the HYPHEN-POSITIONS indices."
  (let ((v (make-bool-vector (max n 1) nil)))
    (dotimes (j (length hyphen-positions))
      (aset v (aref hyphen-positions j) t))
    v))

(defun ekp--dp-state-array (length previous stable index initial)
  "Return LENGTH array reusing PREVIOUS INDEX through STABLE."
  (if previous
      (ekp--copy-vector-prefix
       (aref previous index) length (1+ stable) initial)
    (make-vector length initial)))

(defun ekp--dp-first-new-break (para stable)
  "Return PARA's first permitted break after STABLE."
  (let ((breaks (ekp-para-breaks-allowed para))
        (position (1+ stable))
        (end (length (ekp-para-boxes para))))
    (while (and (< position end) (not (aref breaks position)))
      (setq position (1+ position)))
    position))

(defun ekp--dp-line-too-long-p (para start end line-pixel)
  "Return non-nil when PARA's START..END cannot fit LINE-PIXEL."
  (let* ((hyphen-p (ekp--hyphenate-p
                    (ekp-para-hyphen-positions para) (1- end)))
         (hyphen-width (if hyphen-p (ekp-para-hyphen-pixel para) 0))
         (ideal (ekp--line-ideal-pixel para start end))
         (minimum-prefix (ekp-para-min-prefixs para))
         (glue-ideal (aref (ekp-para-glue-ideals para) start))
         (glue-min (- glue-ideal
                      (aref (ekp-para-glue-shrinks para) start)))
         (raw-ideal (- (aref (ekp-para-ideal-prefixs para) end)
                       (aref (ekp-para-ideal-prefixs para) start)
                       glue-ideal))
         (space-width (ekp--line-stripped-space-pixel
                       raw-ideal start end
                       (ekp-para-lead-spaces para)
                       (ekp-para-trail-spaces para)))
         (minimum (+ (- (aref minimum-prefix end)
                        (aref minimum-prefix start)
                        glue-min space-width)
                     hyphen-width))
         (target (+ (if (= start 0)
                        (cdr (ekp--line-spec para 0 line-pixel))
                      line-pixel)
                    (if hyphen-p
                        (ekp-para-hyphen-protrude para)
                      (aref (ekp-para-tail-protrudes para) end)))))
    (or (> minimum target)
        (and (= end (length (ekp-para-boxes para)))
             (> ideal target)))))

(defun ekp--dp-reused-start (para stable line-pixel)
  "Return PARA's earliest state before STABLE reaching LINE-PIXEL's tail."
  (let ((end (ekp--dp-first-new-break para stable))
        (low 0)
        (high stable))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (ekp--dp-line-too-long-p
             para middle end line-pixel)
            (setq low (1+ middle))
          (setq high middle))))
    low))

(defun ekp--dp-run-1d
    (para line-pixel allow-emergency &optional previous-state stable-end)
  "One strict (or emergency-permitting) K-P DP pass over PARA at LINE-PIXEL.
Returns the dp-result plist, or nil when the paragraph end is
unreachable (only possible when ALLOW-EMERGENCY is nil).
PREVIOUS-STATE may reuse exact states through STABLE-END."
  (let* ((boxes (ekp-para-boxes para))
         (n (length boxes))
         (reuse (and previous-state stable-end
                     (eq (aref previous-state 6) allow-emergency)))
         (stable (if reuse (min stable-end n) 0))
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
         ;; First-line indent shrinks line 0 only; a line starts at
         ;; box 0 exactly when i = 0, so the 1D DP handles it without
         ;; the (position × line-count) state (parshape still needs it).
         (first-line-pixel (cdr (ekp--line-spec para 0 line-pixel)))
         (params (ekp-para-glue-params para))
         (lws-stretch (plist-get params :lws-stretch))
         (mws-stretch (plist-get params :mws-stretch))
         (cws-stretch (plist-get params :cws-stretch))
         (lws-shrink (plist-get params :lws-shrink))
         (mws-shrink (plist-get params :mws-shrink))
         (cws-shrink (plist-get params :cws-shrink))
         (extra-stretch (or (plist-get params :extra-stretch) 0))
         (emergency-stretch
          (if allow-emergency
              (ekp--resolved-emergency-stretch-pixel)
            0))
         (reused-state (and reuse previous-state))
         (backptrs (ekp--dp-state-array
                    (1+ n) reused-state stable 0 nil))
         (demerits (ekp--dp-state-array
                    (1+ n) reused-state stable 1 nil))
         (rests (ekp--dp-state-array
                 (1+ n) reused-state stable 2 nil))
         (gaps (ekp--dp-state-array
                (1+ n) reused-state stable 3 nil))
         (hyphen-counts (ekp--dp-state-array
                         (1+ n) reused-state stable 4 0))
         (fitness-classes (ekp--dp-state-array
                           (1+ n) reused-state stable 5 1))
         (artificial-candidates
          (and allow-emergency (make-vector (1+ n) nil)))
         (surviving-candidates
          (and allow-emergency (make-bool-vector (1+ n) nil))))
    (aset demerits 0 0.0)
    (let ((iteration-start
           (if reuse
               (ekp--dp-reused-start para stable line-pixel)
             0)))
      (cl-loop for i from iteration-start below n do
        (when (and allow-emergency
                   (null (aref demerits i))
                   (not (aref surviving-candidates i))
                   (aref artificial-candidates i))
          (ekp--dp-install-artificial
           (aref artificial-candidates i) i
           demerits backptrs rests gaps hyphen-counts fitness-classes))
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
               (k (if (and reuse (< i stable))
                      (1+ stable)
                    (1+ i))))
          (catch 'break
            (while (<= k n)
              (if (not (or (= k n) (aref breaks-ok k)))
                  ;; Break forbidden here (kinsoku, no-break span):
                  ;; not a candidate; keep extending the line.
                  (setq k (1+ k))
              (let* ((is-last (= k n))
                     (single-box (= k (1+ i)))
                     (end-with-hyphenp (aref hyph-flags (1- k)))
                     (hyph-w (if end-with-hyphenp hyphen-pixel 0))
                     ;; right-edge protrusion releases width at this k
                     (lw (+ (if (= i 0) first-line-pixel line-pixel)
                            (if end-with-hyphenp
                                hyphen-protrude
                              (aref tail-protrudes k))))
                     (raw-ideal (- (aref ideal-prefixs k) ip-i lead-glue-ideal))
                     (space-w
                      (ekp--line-stripped-space-pixel
                       raw-ideal i k lead-spaces trail-spaces))
                     (ideal (+ (- raw-ideal space-w) hyph-w))
                     (minw (+ (- (aref min-prefixs k) mn-i lead-glue-min
                                 space-w)
                              hyph-w))
                     (maxw (+ (- (aref max-prefixs k) mx-i lead-glue-max
                                 space-w)
                              hyph-w extra-stretch))
                     (effective-maxw (+ maxw emergency-stretch)))
                (cond
                 ;; Remember a TeX-style zero-increment break in case every
                 ;; active path would be lost at this overfull breakpoint.
                 ((or (> minw lw)
                      (and is-last (> ideal lw)))
                  (when allow-emergency
                    (let ((current (aref artificial-candidates k)))
                      (when (or (null current)
                                (< prev-dem (aref current 0)))
                        (aset artificial-candidates k
                              (vector prev-dem i (- lw ideal)
                                      (unless single-box
                                        (ekp--gaps-between para i k))
                                      end-with-hyphenp
                                      prev-hyphen-count)))))
                  (throw 'break nil))
                 ;; Valid break point
                 ((or (<= minw lw effective-maxw)
                      (and is-last (<= ideal lw)))
                  (when allow-emergency
                    (aset surviving-candidates k t))
                  (let* ((adjustment (- lw ideal))
                         dem line-gaps fitness new-hyphen)
                    (cond
                     ;; Single box line: fixed flexibility in the strict pass;
                     ;; final pass uses finite background emergency stretch.
                     (single-box
                      (let* ((flexibility
                              (if (and allow-emergency (> adjustment 0))
                                  emergency-stretch
                                1))
                             (badness (ekp--compute-badness
                                       adjustment flexibility))
                             (penalty (if end-with-hyphenp
                                          ekp-hyphen-penalty 0)))
                        (setq fitness
                              (if (and allow-emergency (> adjustment 0))
                                  (ekp--compute-fitness-class
                                   adjustment flexibility)
                                1)
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
                                  (let ((stretch (+ (* lcnt lws-stretch)
                                                    (* mcnt mws-stretch)
                                                    (* ccnt cws-stretch)
                                                    extra-stretch)))
                                    (if allow-emergency
                                        (+ stretch emergency-stretch)
                                      stretch))
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
                        (aset hyphen-counts k new-hyphen)))
                    nil))
                 ;; Underfull candidates remain active even when their
                 ;; badness is above this pass's finite fit threshold.
                 (allow-emergency
                  (aset surviving-candidates k t)))
                (setq k (1+ k))))))))))
    (when (and allow-emergency
               (null (aref demerits n))
               (not (aref surviving-candidates n))
               (aref artificial-candidates n))
      (ekp--dp-install-artificial
       (aref artificial-candidates n) n
       demerits backptrs rests gaps hyphen-counts fitness-classes))
    ;; Extract solution (nil when end unreachable in the strict pass)
    (when (aref demerits n)
      (let ((breaks (ekp--dp-trace-breaks backptrs n)))
        (list :rests (mapcar (lambda (b) (aref rests b)) breaks)
              :gaps (mapcar (lambda (b) (aref gaps b)) breaks)
              :breaks breaks
              :cost (aref demerits n)
              :line-count (length breaks)
              :state (vector backptrs demerits rests gaps
                             hyphen-counts fitness-classes
                             allow-emergency))))))

(defun ekp--dp-install-artificial
    (candidate k demerits backptrs rests gaps hyphen-counts fitness-classes)
  "Install TeX final-pass CANDIDATE at break K.
CANDIDATE stores prior demerits, start, rest, gaps, hyphen flag, and
prior hyphen count.  Artificial demerits add zero to the prior path;
the overfull line keeps the tight fitness class computed by TeX.
Update DEMERITS, BACKPTRS, RESTS, GAPS, HYPHEN-COUNTS, and
FITNESS-CLASSES in place."
  (aset demerits k (aref candidate 0))
  (aset backptrs k (aref candidate 1))
  (aset rests k (aref candidate 2))
  (aset gaps k (aref candidate 3))
  (aset fitness-classes k 0)
  (aset hyphen-counts k
        (if (aref candidate 4) (1+ (aref candidate 5)) 0)))

(defun ekp--dp-trace-breaks (backptrs n)
  "Trace optimal break points back from N using the BACKPTRS array."
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
  "Run the DP over PARA at LINE-PIXEL, tracking all line counts.
For `ekp-looseness' and parshape support.  Two passes like the 1D
engine: strict first, then emergency breaks when no layout is valid."
  (let ((dp-result (or (ekp--dp-run-loose para line-pixel nil)
                       (ekp--dp-run-loose para line-pixel t))))
    (puthash (ekp--dp-key line-pixel) dp-result (ekp-para-dp-cache para))
    dp-result))

(defun ekp--dp-run-loose (para line-pixel allow-emergency)
  "One (position × line-count) DP pass over PARA at LINE-PIXEL.
Return the dp-result plist, or nil when the paragraph end is
unreachable (only possible when ALLOW-EMERGENCY is nil)."
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
         (emergency-stretch
          (if allow-emergency
              (ekp--resolved-emergency-stretch-pixel)
            0))
         ;; state: (pos . lines) -> [dem backptr fitness hyph rest gaps]
         (states (make-hash-table :test 'equal :size (* 4 (1+ n))))
         (counts-at (make-vector (1+ n) nil))
         (artificial-candidates
          (and allow-emergency (make-vector (1+ n) nil)))
         (surviving-candidates
          (and allow-emergency (make-bool-vector (1+ n) nil))))
    (puthash (cons 0 0) (vector 0.0 nil 1 0 nil nil) states)
    (push 0 (aref counts-at 0))
    (dotimes (i n)
      (when (and allow-emergency
                 (null (aref counts-at i))
                 (not (aref surviving-candidates i))
                 (aref artificial-candidates i))
        (ekp--dp-loose-install-artificial
         (aref artificial-candidates i) i states counts-at))
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
               (k (1+ i)))
          (catch 'break
            (while (<= k n)
              (if (not (or (= k n) (aref breaks-ok k)))
                  ;; Break forbidden here: keep extending the line.
                  (setq k (1+ k))
              (let* ((is-last (= k n))
                     (single-box (= k (1+ i)))
                     (end-with-hyphenp
                      (ekp--hyphenate-p hyphen-positions (1- k)))
                     (hyph-w (if end-with-hyphenp hyphen-pixel 0))
                     ;; right-edge protrusion releases width at this k
                     (lw (+ this-width
                            (if end-with-hyphenp
                                hyphen-protrude
                              (aref tail-protrudes k))))
                     (raw-ideal (- (aref ideal-prefixs k) ip-i lead-glue-ideal))
                     (space-w
                      (ekp--line-stripped-space-pixel
                       raw-ideal i k lead-spaces trail-spaces))
                     (ideal (+ (- raw-ideal space-w) hyph-w))
                     (minw (+ (- (aref min-prefixs k) mn-i lead-glue-min
                                 space-w)
                              hyph-w))
                     (maxw (+ (- (aref max-prefixs k) mx-i lead-glue-max
                                 space-w)
                              hyph-w extra-stretch))
                     (effective-maxw (+ maxw emergency-stretch))
                     (adjustment (- lw ideal))
                     candidate)
                (cond
                 ((or (> minw lw)
                      (and is-last (> ideal lw)))
                  (when allow-emergency
                    (let ((current (aref artificial-candidates k)))
                      (when (or (null current)
                                (< prev-dem (aref current 0)))
                        (aset artificial-candidates k
                              (vector prev-dem i lc adjustment
                                      (unless single-box
                                        (ekp--gaps-between para i k))
                                      end-with-hyphenp
                                      prev-hyphen-count)))))
                  (throw 'break nil))
                 ((or (<= minw lw effective-maxw)
                      (and is-last (<= ideal lw)))
                  (when allow-emergency
                    (aset surviving-candidates k t))
                  (setq candidate
                        (cond
                         (single-box
                          (let* ((flexibility
                                  (if (and allow-emergency (> adjustment 0))
                                      emergency-stretch
                                    1))
                                 (badness (ekp--compute-badness
                                           adjustment flexibility))
                                 (penalty (if end-with-hyphenp
                                              ekp-hyphen-penalty 0))
                                 (fitness
                                  (if (and allow-emergency (> adjustment 0))
                                      (ekp--compute-fitness-class
                                       adjustment flexibility)
                                    1))
                                 (nh (if end-with-hyphenp
                                         (1+ prev-hyphen-count) 0)))
                            (list (ekp--compute-demerits
                                   badness penalty prev-fitness fitness
                                   end-with-hyphenp prev-hyphen-count)
                                  adjustment nil fitness nh)))
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
                                      (let ((stretch (+ (* lcnt lws-stretch)
                                                        (* mcnt mws-stretch)
                                                        (* ccnt cws-stretch)
                                                        extra-stretch)))
                                        (if allow-emergency
                                            (+ stretch emergency-stretch)
                                          stretch))
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
                                       prev-dem candidate)
                  nil)
                 (allow-emergency
                  (aset surviving-candidates k t)))
                (setq k (1+ k)))))))))
    (when (and allow-emergency
               (null (aref counts-at n))
               (not (aref surviving-candidates n))
               (aref artificial-candidates n))
      (ekp--dp-loose-install-artificial
       (aref artificial-candidates n) n states counts-at))
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

(defun ekp--dp-loose-install-artificial (candidate k states counts-at)
  "Install TeX final-pass CANDIDATE at loose-DP break K.
Update the STATES table and COUNTS-AT index in place."
  (let* ((lines (1+ (aref candidate 2)))
         (key (cons k lines)))
    (unless (gethash key states)
      (push lines (aref counts-at k))
      (puthash key
               (vector (aref candidate 0)
                       (aref candidate 1)
                       0
                       (if (aref candidate 5)
                           (1+ (aref candidate 6)) 0)
                       (aref candidate 3)
                       (aref candidate 4))
               states))))

(defun ekp--dp-loose-relax (states counts-at k lines i prev-dem candidate)
  "Relax state (K . LINES) with CANDIDATE from position I.
STATES maps each (position . line-count) to its best vector; COUNTS-AT
tracks the line counts reached at each position.  PREV-DEM is the
demerits up to I.  CANDIDATE is (DEM-DELTA REST GAPS FITNESS
HYPHEN-COUNT)."
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
  (gethash (ekp--dp-key line-pixel) (ekp-para-dp-cache para)))

(defun ekp--c-module-ready-p ()
  "Return non-nil when the loaded C module exposes the DP entry point."
  (and (boundp 'ekp-c-module-loaded)
       ekp-c-module-loaded
       (fboundp 'ekp-c-break-with-arrays)))

(defun ekp--c-available-p ()
  "Return non-nil when the C module can be used for ordinary DP."
  (and ekp-use-c-module
       (ekp--c-module-ready-p)
       ;; looseness and parshape need the (position × line-count) DP,
       ;; Elisp only; first-line indent is a scalar the C engine takes
       (= ekp-looseness 0)
       (not ekp-parshape)))

(defun ekp--c-append-available-p ()
  "Return non-nil when live append may use the native 1D DP path."
  (and (= ekp-looseness 0)
       (not ekp-parshape)
       (or (ekp--c-available-p)
           (and ekp--allow-native-live-append
                (bound-and-true-p ekp-auto-justify-native-append)
                (ekp--c-module-ready-p)))))

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
                           (ekp--ragged-extra-stretch))
                         (ekp--resolved-emergency-stretch-pixel))))

(defun ekp--dp-cache-para (para line-pixel)
  "Return PARA's DP result at LINE-PIXEL, computing it when absent."
  (or (ekp--dp-get-cached para line-pixel)
      (if (ekp--c-available-p)
          (ekp--dp-cache-via-c para line-pixel)
        (ekp--dp-cache-elisp para line-pixel))))

(defun ekp--dp-cache-append (para previous stable line-pixel)
  "Compute PARA at LINE-PIXEL reusing PREVIOUS states through STABLE.
Automatic live append may use the loaded native 1D DP even when the
ordinary full-layout engine is explicitly set to Elisp."
  (if (ekp--c-append-available-p)
      (ekp--dp-cache-via-c para line-pixel)
    (let* ((old (ekp--dp-get-cached previous line-pixel))
           (state (and old (plist-get old :state)))
           (result
            (if (and state (not (aref state 6)))
                (or (ekp--dp-run-1d
                     para line-pixel nil state stable)
                    (ekp--dp-run-1d para line-pixel t))
              (or (ekp--dp-run-1d para line-pixel nil)
                  (ekp--dp-run-1d para line-pixel t)))))
      (puthash (ekp--dp-key line-pixel) result (ekp-para-dp-cache para))
      result)))

(defun ekp-dp-cache (string line-pixel)
  "Compute optimal line breaks for STRING at LINE-PIXEL width.
Uses Knuth-Plass dynamic programming with demerits.
If `ekp-use-c-module' is non-nil and the C module is available (and
`ekp-looseness' is 0), the C module computes the DP."
  (let ((ekp--policy-measure line-pixel))
    (ekp--dp-cache-para (ekp--get-para string) line-pixel)))

(defun ekp--lines-data-from-breaks (para line-pixel breaks)
  "Compute (RESTS . GAPS) lists for BREAKS of PARA at LINE-PIXEL.
Per-line widths (first-line indent) must mirror the DP exactly, or
the reconstructed rests overfill the indented line."
  (let ((start 0) (idx 0) rests gapss)
    (dolist (end breaks)
      (push (- (+ (cdr (ekp--line-spec para idx line-pixel))
                  (ekp--line-edge-release para start end))
               (ekp--line-ideal-pixel para start end))
            rests)
      (push (if (or (= end (1+ start))
                    (= end (length (ekp-para-boxes para))))
                nil
              (ekp--gaps-between para start end))
            gapss)
      (setq start end
            idx (1+ idx)))
    (cons (nreverse rests) (nreverse gapss))))

(defun ekp--store-c-result (para line-pixel breaks cost)
  "Store C-module (BREAKS, COST) for LINE-PIXEL in PARA's dp-cache."
  (let* ((data (ekp--lines-data-from-breaks para line-pixel breaks))
         (dp-result (list :rests (car data)
                          :gaps (cdr data)
                          :breaks breaks
                          :cost cost
                          :line-count (length breaks))))
    (puthash (ekp--dp-key line-pixel) dp-result (ekp-para-dp-cache para))
    dp-result))

(defun ekp--c-breaks-valid-p (para breaks)
  "Return non-nil when BREAKS are in range and increasing for PARA."
  (and (proper-list-p breaks)
       (let ((limit (length (ekp-para-boxes para)))
             (previous 0)
             (valid t))
         (dolist (break breaks)
           (unless (and (integerp break) (< previous break) (<= break limit))
             (setq valid nil))
           (when (integerp break)
             (setq previous break)))
         (and valid (= previous limit)))))

(defun ekp--signal-backend-contract-error (detail result)
  "Signal an explicit backend contract error for DETAIL and RESULT."
  (signal 'ekp-backend-contract-error (list detail result)))

(defun ekp--valid-c-result-or-signal (para result)
  "Return (BREAKS . COST), :fallback, or signal for PARA C RESULT.
Nil RESULT and nil breaks are documented soft failures and keep the
Elisp fallback.  Any non-nil malformed result is a backend contract
violation."
  (cond
   ((null result) :fallback)
   ((not (consp result))
    (ekp--signal-backend-contract-error 'malformed-result result))
   ((null (car result)) :fallback)
   ((not (numberp (cdr result)))
    (ekp--signal-backend-contract-error 'nonnumeric-cost result))
   ((not (ekp--c-breaks-valid-p para (car result)))
    (ekp--signal-backend-contract-error 'malformed-breaks result))
   (t result)))

(defun ekp--prepare-para-for-c (para line-pixel)
  "Prepare PARA at LINE-PIXEL as a 15-element vector for the C batch API."
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
          (ekp-para-hyphen-protrude para)
          (cdr (ekp--line-spec para 0 line-pixel))))

(defun ekp--dp-cache-via-c (para line-pixel)
  "Compute breaks at LINE-PIXEL using the C module and PARA's arrays.
The C module receives all font-dependent data from Elisp; it only
runs the pure DP.  A nil result falls back to Elisp; module errors
propagate because they indicate a broken backend contract."
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
                  (ekp-para-hyphen-protrude para)
                  (cdr (ekp--line-spec para 0 line-pixel))))
         (checked (ekp--valid-c-result-or-signal para result)))
    (if (eq checked :fallback)
        (ekp--dp-cache-elisp para line-pixel)
      (ekp--store-c-result para line-pixel (car checked) (cdr checked)))))

(defun ekp--dp-cache-batch (strings line-pixel)
  "Compute DP at LINE-PIXEL for multiple STRINGS via the C batch API.
Returns list of dp-results in the same order as STRINGS.
Only computes strings that aren't already cached."
  (let* ((ekp--policy-measure line-pixel)
         (paras (mapcar #'ekp--get-para strings))
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
             ;; A nil whole-batch result falls back per paragraph.
             ;; Signals propagate as broken backend contracts.
             (batch-results (ekp-c-break-batch batch-input)))
        (when (and batch-results
                   (or (not (vectorp batch-results))
                       (/= (length batch-results)
                           (length needs-compute))))
          (ekp--signal-backend-contract-error
           'malformed-batch-results batch-results))
        (cl-loop for ip in needs-compute
                 for j from 0
                 for idx = (car ip)
                 for para = (cdr ip)
                 for res = (and batch-results (aref batch-results j))
                 for checked = (ekp--valid-c-result-or-signal para res)
                 do (aset results idx
                          (if (eq checked :fallback)
                            ;; C returned no result; fallback to Elisp.
                              (ekp--dp-cache-elisp para line-pixel)
                            (ekp--store-c-result para line-pixel
                                                 (car checked)
                                                 (cdr checked))))))
      (append results nil))))

(defun ekp-dp-data (string line-pixel &optional key)
  "Return the dp cache plist for STRING at LINE-PIXEL.
If KEY is non-nil, return the value of KEY in the plist."
  (let ((data (ekp-dp-cache string line-pixel)))
    (if key
        (plist-get data key)
      data)))

(defun ekp-total-cost (string line-pixel)
  "Return the total demerits of the K-P solution for STRING at LINE-PIXEL."
  (ekp-dp-data string line-pixel :cost))

(defun ekp-line-breaks (string line-pixel)
  "Return the break points of the K-P solution for STRING at LINE-PIXEL."
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

(defun ekp--distribute-emergency-stretch (para rest-pixel gaps-list)
  "Distribute REST-PIXEL for PARA over stretchable GAPS-LIST.
Use TeX glue-set proportions from PARA's actual stretch capacities."
  (let* ((params (ekp-para-glue-params para))
         (changes (mapcar (lambda (key) (plist-get params key))
                          '(:lws-stretch :mws-stretch :cws-stretch)))
         (weights (cl-mapcar #'* gaps-list changes))
         (total (apply #'+ weights))
         (amounts (make-vector 3 0)))
    (when (> total 0)
      (let ((remainders nil) (used 0) (index 0))
        (dolist (weight weights)
          (let* ((numerator (* rest-pixel weight))
                 (base (/ numerator total)))
            (aset amounts index base)
            (cl-incf used base)
            (push (cons (% numerator total) index) remainders)
            (cl-incf index)))
        (dolist (entry (seq-take
                        (sort remainders
                              (lambda (a b)
                                (if (= (car a) (car b))
                                    (< (cdr a) (cdr b))
                                  (> (car a) (car b)))))
                        (- rest-pixel used)))
          (cl-incf (aref amounts (cdr entry))))))
    (cl-loop for amount across amounts
             for count in gaps-list
             collect (if (> count 0)
                         (cons (/ amount count) (% amount count))
                       (cons 0 0)))))

(defun ekp--fixed-line-glues (para types start end maximum trailing)
  "Return fixed glue pixels for PARA TYPES from START to END.
Use maximum widths when MAXIMUM is non-nil and finish with TRAILING."
  (let* ((params (ekp-para-glue-params para))
         (pixels (make-vector (1+ (- end start)) 0)))
    (cl-loop for position from (1+ start) below end
             for output from 1
             for type = (aref types position)
             for ideal = (pcase type
                           ('lws (plist-get params :lws-ideal))
                           ('mws (plist-get params :mws-ideal))
                           ('cws (plist-get params :cws-ideal))
                           (_ 0))
             for stretch = (if maximum
                               (pcase type
                                 ('lws (plist-get params :lws-stretch))
                                 ('mws (plist-get params :mws-stretch))
                                 ('cws (plist-get params :cws-stretch))
                                 (_ 0))
                             0)
             do (aset pixels output (+ ideal stretch)))
    (aset pixels (1- (length pixels)) trailing)
    pixels))

(defun ekp--adjusted-line-glues
    (para types start end rest-pixel gaps-list &optional emergency-stretch)
  "Distribute REST-PIXEL over GAPS-LIST for PARA TYPES from START to END."
  (if (= rest-pixel 0)
      (ekp--fixed-line-glues para types start end nil 0)
    (let* ((stretch-p (> rest-pixel 0))
           (distribution (if emergency-stretch
                             (ekp--distribute-emergency-stretch
                              para rest-pixel gaps-list)
                           (ekp--distribute-gap-adjustment
                            para (abs rest-pixel) gaps-list stretch-p)))
           (shares (vconcat distribution))
           (params (ekp-para-glue-params para))
           (pixels (make-vector (1+ (- end start)) 0))
           (indices (vector -1 -1 -1)))
      (cl-loop for position from (1+ start) below end
               for output from 1
               for type = (aref types position)
               for slot = (pcase type ('lws 0) ('mws 1) ('cws 2) (_ nil))
               for ideal = (pcase type
                             ('lws (plist-get params :lws-ideal))
                             ('mws (plist-get params :mws-ideal))
                             ('cws (plist-get params :cws-ideal))
                             (_ 0))
               do
               (let ((adjustment 0))
                 (when slot
                   (cl-incf (aref indices slot))
                   (let ((share (aref shares slot)))
                     (setq adjustment
                           (+ (car share)
                              (if (< (aref indices slot) (cdr share))
                                  1 0)))))
                 (aset pixels output
                       (max 0 (if stretch-p
                                  (+ ideal adjustment)
                                (- ideal adjustment))))))
      pixels)))

(defun ekp--line-glues-from-data
    (para line-pixel dp &optional previous-lines common)
  "Compute glue vectors from prepared PARA at LINE-PIXEL using DP.
Reuse COMMON entries from PREVIOUS-LINES when provided."
  (let* ((boxes-num (length (ekp-para-boxes para)))
         (glues-types (ekp-para-glues-types para))
         (alignment (or (plist-get (ekp-para-glue-params para) :alignment)
                        'justify))
         (ragged (not (eq alignment 'justify)))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (breaks (plist-get dp :breaks))
         (lines-rests (plist-get dp :rests))
         (lines-gaps (plist-get dp :gaps))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (line-glues (make-vector (length breaks) nil))
         (start (if (> (or common 0) 0)
                    (nth (1- common) breaks)
                  0)))
    (dotimes (index (or common 0))
      (aset line-glues index
            (ekp-layout-line-glues (aref previous-lines index))))
    (cl-loop for i from (or common 0) below (length breaks) do
      (let* ((end (nth i breaks))
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
                               (space-w
                                (ekp--line-stripped-space-pixel
                                 raw-ideal start end
                                 (ekp-para-lead-spaces para)
                                 (ekp-para-trail-spaces para))))
                          (+ (- (aref mx end) (aref mx start)
                                (+ (aref (ekp-para-glue-ideals para) start)
                                   (aref (ekp-para-glue-stretches para) start))
                                space-w)
                             (if hyphen-p hyphen-pixel 0))))
             glue-vector)
        (setq glue-vector
              (cond
               ;; Single box: just trailing space
               ((= 1 (- end start))
                (vector 0 (max 0 (- eff-pixel ideal-pixel))))
               ;; Last line, or any line under non-justify alignment:
               ;; natural glue widths plus a trailing filler.
               ((or is-last ragged)
                (ekp--fixed-line-glues
                 para glues-types start end nil
                 (max 0 (- eff-pixel ideal-pixel))))
               ;; Emergency underfull line (can't stretch to width):
               ;; final pass may stretch real glue past nominal max.
               ((< max-pixel eff-pixel)
                (let* ((line-gaps (nth i lines-gaps))
                       (params (ekp-para-glue-params para))
                       (stretch-capacity
                        (and line-gaps
                             (+ (* (nth 0 line-gaps)
                                   (plist-get params :lws-stretch))
                                (* (nth 1 line-gaps)
                                   (plist-get params :mws-stretch))
                                (* (nth 2 line-gaps)
                                   (plist-get params :cws-stretch))))))
                  (if (and stretch-capacity (> stretch-capacity 0))
                      (ekp--adjusted-line-glues
                       para glues-types start end
                       (nth i lines-rests) line-gaps t)
                    (ekp--fixed-line-glues
                     para glues-types start end t
                     (max 0 (- eff-pixel max-pixel))))))
               ;; Normal justified line
               (t
                (ekp--adjusted-line-glues
                 para glues-types start end
                 (nth i lines-rests) (nth i lines-gaps)))))
        ;; Non-justify alignment: place the leftover per mode
        ;; (ragged-right keeps it trailing; center splits it; ragged-left
        ;; moves it to the head).
        (when (and ragged (>= (length glue-vector) 2)
                   (memq alignment '(center ragged-left)))
          (let* ((last (1- (length glue-vector)))
                 (filler (aref glue-vector last))
                 (lead (if (eq alignment 'center)
                           (/ filler 2)
                         filler)))
            (aset glue-vector 0 lead)
            (aset glue-vector last
                  (if (eq alignment 'center)
                      (- filler lead)
                    0))))
        ;; left indent renders as a leading spacer
        (when (> line-indent 0)
          (aset glue-vector 0 (+ (aref glue-vector 0) line-indent)))
        (aset line-glues i glue-vector)
        (setq start end)))
    line-glues))

(defun ekp-line-glues (string line-pixel)
  "Compute glue pixels for each line after breaking STRING at LINE-PIXEL.
Returns a vector of per-line glue vectors."
  (let ((para (ekp--get-para string)))
    (ekp--line-glues-from-data
     para line-pixel (ekp--dp-cache-para para line-pixel))))

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

(defun ekp--gap-natural-pixels (para offsets)
  "Return memoized natural gap widths for PARA at OFFSETS."
  (or (ekp-para-gap-naturals-memo para)
      (let* ((string (ekp-para-string para))
             (boxes (ekp-para-boxes para))
             (naturals (make-vector (length boxes) 0)))
        (cl-loop
         for right from 1 below (length boxes)
         for left = (1- right)
         for start = (cdr (aref offsets left))
         for end = (car (aref offsets right))
         for source = (if (< start end)
                          (substring string start end)
                        (car (last (string-glyph-split
                                    (aref boxes left)))))
         do (aset naturals right (ekp--measured-width source)))
        (setf (ekp-para-gap-naturals-memo para) naturals))))

(defun ekp--layout-line-gaps
    (para offsets naturals box-start box-end glues)
  "Build semantic gaps for one line of PARA.
OFFSETS maps source boxes and NATURALS their measured gap widths.
BOX-START and BOX-END delimit the kept boxes; GLUES contains the
corresponding leading/interior/trailing pixel widths."
  (let ((types (ekp-para-glues-types para))
        (glue-index 1)
        gaps)
    (cl-loop for right from (1+ box-start) below box-end do
             (let* ((left (1- right))
                    (source-start (cdr (aref offsets left)))
                    (source-end (car (aref offsets right)))
                    (natural (aref naturals right))
                    (target (aref glues glue-index)))
               (when (or (< source-start source-end)
                         (> target 0))
                 (push (ekp-layout-gap--create
                        :kind (aref types right)
                        :left-box left
                        :right-box right
                        :source-start source-start
                        :source-end source-end
                        :natural-pixel natural
                        :target-pixel target)
                       gaps))
               (setq glue-index (1+ glue-index))))
    (vconcat (nreverse gaps))))

(defun ekp--layout-line-signature (line)
  "Return the stable layout signature for LINE."
  (list (ekp-layout-line-source-start line)
        (ekp-layout-line-source-end line)
        (ekp-layout-line-break-kind line)
        (append (ekp-layout-line-glues line) nil)))

(defun ekp--layout-break-kind (string line next-line)
  "Classify the visual break from LINE to NEXT-LINE in STRING."
  (cond
   ((ekp-layout-line-hyphen-p line) 'hyphen)
   ((string-blank-p
     (substring string
                (ekp-layout-line-source-end line)
                (ekp-layout-line-source-start next-line)))
    'space)
   (t 'cjk)))

(defun ekp--finalize-layout-breaks (string lines)
  "Add source break ranges, kinds, and signatures to LINES for STRING."
  (dotimes (i (length lines))
    (let* ((line (aref lines i))
           (next (and (< i (1- (length lines))) (aref lines (1+ i))))
           (start (ekp-layout-line-source-end line))
           (end (if next (ekp-layout-line-source-start next) start)))
      (setf (ekp-layout-line-break-source-start line) start
            (ekp-layout-line-break-source-end line) end
            (ekp-layout-line-break-kind line)
            (and next (ekp--layout-break-kind string line next))
            (ekp-layout-line-signature line)
            (ekp--layout-line-signature line))))
  lines)

(defun ekp--make-layout-line
    (para offsets naturals index start end line-glues last-line-p)
  "Build one semantic layout line from PARA's DP slice START through END.
OFFSETS maps boxes to source positions and NATURALS stores gap widths.
INDEX is the line number, and LINE-GLUES holds its pixel widths.
LAST-LINE-P suppresses a terminal discretionary hyphen."
  (let* ((boxes (ekp-para-boxes para))
         (stripped (ekp--strip-line-spaces
                    (cl-subseq boxes start end) line-glues
                    (> index 0) t))
         (kept (nth 0 stripped))
         (glues (vconcat (nth 1 stripped)))
         (box-start (+ start (nth 2 stripped)))
         (box-end (+ box-start (length kept)))
         (hyphen-p (and (not last-line-p)
                        (ekp--hyphenate-p
                         (ekp-para-hyphen-positions para) (1- end)))))
    (when kept
      (ekp-layout-line--create
       :index index :box-start box-start :box-end box-end
       :source-start (car (aref offsets box-start))
       :source-end (cdr (aref offsets (1- box-end)))
       :glues glues
       :gaps (ekp--layout-line-gaps
              para offsets naturals box-start box-end glues)
       :leading-pixel (aref glues 0)
       :trailing-pixel (aref glues (1- (length glues)))
       :hyphen-p hyphen-p))))

(defun ekp--common-layout-line-count (previous breaks stable)
  "Return the break prefix shared by PREVIOUS and BREAKS before STABLE."
  (let ((lines (and previous (ekp-layout-plan-lines previous)))
        (count 0))
    (while (and lines
                (< count (length lines))
                (< count (length breaks))
                (<= (ekp-layout-line-box-end (aref lines count)) stable)
                (= (ekp-layout-line-box-end (aref lines count))
                   (nth count breaks)))
      (cl-incf count))
    count))

(defun ekp--layout-context-snapshot (line-pixel)
  "Return every non-text input to a plan at LINE-PIXEL."
  (ekp--copy-layout-context-value
   (list (ekp--dp-key line-pixel)
         (ekp--width-context)
         ekp-latin-lang
         ekp-alignment
         ekp-ragged-stretch-pixel
         (and ekp-protrusion ekp-protrusion-ratios)
         ekp-parshape
         ekp-first-line-indent
         (ekp--policy-signature)
         (ekp--spacing-signature))))

(defun ekp--layout-plan-from-para
    (string line-pixel para dp &optional previous stable)
  "Build STRING's LINE-PIXEL plan from PARA and DP.
Reuse PREVIOUS lines that end before STABLE when both are non-nil."
  (let* ((boxes (ekp-para-boxes para))
         (offsets (or (ekp-para-box-offsets-memo para)
                      (setf (ekp-para-box-offsets-memo para)
                            (ekp--box-offsets string (append boxes nil)))))
         (naturals (ekp--gap-natural-pixels para offsets))
         (breaks (plist-get dp :breaks))
         (previous-lines (and previous (ekp-layout-plan-lines previous)))
         (common (if previous
                     (ekp--common-layout-line-count
                      previous breaks stable)
                   0))
         (line-glues (ekp--line-glues-from-data
                      para line-pixel dp previous-lines common))
         (start (if (> common 0) (nth (1- common) breaks) 0))
         (lines (reverse
                 (cl-subseq (append previous-lines nil) 0 common))))
    (cl-loop for i from common below (length breaks) do
      (let* ((end (nth i breaks))
             (line (ekp--make-layout-line
                    para offsets naturals i start end (aref line-glues i)
                    (= i (1- (length breaks))))))
        (when line (push line lines))
        (setq start end)))
    (setq lines (ekp--finalize-layout-breaks
                 string (vconcat (nreverse lines))))
    (ekp-layout-plan--create
     :string string :line-pixel line-pixel
     :context (ekp--layout-context-snapshot line-pixel)
     :para para :boxes boxes :offsets offsets :lines lines)))

(defun ekp-layout-plan (string line-pixel)
  "Return a semantic KP layout plan for STRING at LINE-PIXEL.
The plan records source offsets, glue targets, breaks, indentation,
and discretionary hyphens without choosing a display representation."
  (let ((ekp--policy-measure line-pixel)
        (source (ekp--clean-private-policy-source string)))
    (if (ekp--natural-overlong-token-p string line-pixel)
        (ekp-layout-plan--create
         :string source :line-pixel line-pixel
         :context (ekp--layout-context-snapshot line-pixel)
         :para nil :boxes [] :offsets [] :lines []
         :state 'natural :reason 'overlong-token)
      (let* ((para (ekp--get-para string))
             (dp (ekp--dp-cache-para para line-pixel))
             (key (ekp--layout-context-snapshot line-pixel))
             (cache (or (ekp-para-layout-plan-cache para)
                        (setf (ekp-para-layout-plan-cache para)
                              (make-hash-table :test 'equal :size 8))))
             (hit (gethash key cache)))
        (if hit
            (ekp--copy-layout-plan-for-consumer hit)
          (when (>= (hash-table-count cache) 8)
            (clrhash cache))
          (let ((plan (ekp--layout-plan-from-para
                       source line-pixel para dp)))
            (puthash key plan cache)
            (ekp--copy-layout-plan-for-consumer plan)))))))

(defun ekp-layout-plan-append (previous string line-pixel)
  "Return STRING's exact append plan by extending PREVIOUS, or nil.
Only property-free, context-stable 1D layouts take this fast path."
  (let ((old-para (and previous (ekp-layout-plan-para previous)))
        (ekp--policy-measure line-pixel))
    (when (and old-para
               (null (ekp-para-resolved-policies old-para))
               (equal (ekp-layout-plan-context previous)
                      (ekp--layout-context-snapshot line-pixel))
               (= ekp-looseness 0)
               (not ekp-parshape)
               (equal (ekp-para-glue-params old-para)
                      (ekp--glue-params-snapshot)))
      (when-let* ((append (ekp--append-para old-para string))
                  (para (car append))
                  (stable (cdr append))
                  (_ (null (ekp-para-resolved-policies para)))
                  (dp (ekp--dp-cache-append
                       para old-para stable line-pixel)))
        (ekp--layout-plan-from-para
         string line-pixel para dp previous stable)))))

(defconst ekp--layout-marker-properties
  '(ekp-glue ekp-soft-break ekp-soft-hyphen ekp-hidden ekp-justified)
  "Text properties owned by the lossless render/inversion protocol.")

;; Text typed next to a marker character must never inherit the
;; marker: a self-inserted char inheriting `ekp-glue' would be treated
;; as a synthesized space by the next unjustification and deleted.
(dolist (prop ekp--layout-marker-properties)
  (setf (alist-get prop text-property-default-nonsticky) t))

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

(defun ekp--render-layout-line-string (plan line)
  "Render LINE from PLAN using the reversible string marker protocol."
  (let* ((string (ekp-layout-plan-string plan))
         (boxes (ekp-layout-plan-boxes plan))
         (offsets (ekp-layout-plan-offsets plan))
         (start (ekp-layout-line-box-start line))
         (end (ekp-layout-line-box-end line))
         (glues (ekp-layout-line-glues line))
         parts)
    (cl-loop for box-index from start below end
             for glue-index from 0 do
             (push (ekp--render-glue
                    (aref glues glue-index)
                    (if (= box-index start) ""
                      (substring string
                                 (cdr (aref offsets (1- box-index)))
                                 (car (aref offsets box-index)))))
                   parts)
             (push (aref boxes box-index) parts))
    (when (ekp-layout-line-hyphen-p line)
      (push (ekp--hyphen-for-box (aref boxes (1- end))) parts))
    (push (ekp--render-glue (aref glues (1- (length glues))) "") parts)
    (apply #'concat (nreverse parts))))

(defun ekp-render-layout-string (plan)
  "Render PLAN as the public reversible justified string."
  (let* ((string (ekp-layout-plan-string plan))
         (lines (ekp-layout-plan-lines plan)))
    (if (eq (ekp-layout-plan-state plan) 'natural)
        string
      (if (= (length lines) 0)
        (ekp--hide-string string)
      (let ((parts (list (ekp--hide-string
                          (substring string 0
                                     (ekp-layout-line-source-start
                                      (aref lines 0)))))))
        (dotimes (i (length lines))
          (let ((line (aref lines i)))
            (when (> i 0)
              (let ((prev (aref lines (1- i))))
                (push (propertize
                       "\n" 'ekp-soft-break
                       (substring string
                                  (ekp-layout-line-source-end prev)
                                  (ekp-layout-line-source-start line)))
                      parts)))
            (push (ekp--render-layout-line-string plan line) parts)))
        (let ((last (aref lines (1- (length lines)))))
          (push (ekp--hide-string
                 (substring string (ekp-layout-line-source-end last)))
                parts))
        (apply #'concat (nreverse parts)))))))

(defun ekp--pixel-justify (string line-pixel)
  "Justify single-paragraph STRING to LINE-PIXEL, with render caching.
The rendered string for a (paragraph, width) pair is deterministic,
so it is stored in the paragraph's dp-cache entry and reused — resize
sweeps that revisit a width pay nothing."
  (if (ekp--natural-overlong-token-p string line-pixel)
      string
    (let* ((ekp--policy-measure line-pixel)
           (para (ekp--get-para string))
           (dp (ekp-dp-data string line-pixel))
           (hit (plist-get dp :rendered)))
      (or hit
          (let ((rendered (ekp--pixel-justify-1 string line-pixel))
                (cache (ekp-para-dp-cache para)))
            ;; keep memory bounded during long resize sessions
            (when (<= (hash-table-count cache) 64)
              (puthash (ekp--dp-key line-pixel)
                       (plist-put dp :rendered rendered) cache))
            rendered)))))

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
  (ekp-render-layout-string (ekp-layout-plan string line-pixel)))

(defun ekp--validate-width (line-pixel)
  "Signal a user error unless LINE-PIXEL is a positive integer."
  (unless (and (integerp line-pixel) (> line-pixel 0))
    (user-error "Line width must be a positive integer, got %S"
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
  "Find the optimal width for STRINGS in [MIN-PIXEL, MAX-PIXEL].
Use ternary search; return the pixel width with minimum average cost."
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
    (user-error "Min-pixel (%d) must be <= max-pixel (%d)"
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
