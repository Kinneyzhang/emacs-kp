;;; ekp.el --- Knuth-Plass line breaking for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: emacs-kp contributors
;; Keywords: text, typesetting, CJK
;; Package-Requires: ((emacs "27.1"))

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

(defconst ekp--load-file (or load-file-name (buffer-file-name))
  "Path to this file, for locating dictionaries.")

(defvar ekp-latin-lang "en_US"
  "Language code for hyphenation (e.g., 'en_US', 'de_DE').")

(defvar ekp-use-c-module t
  "When non-nil, use C dynamic module for DP computation if available.
The C module provides significant performance improvement for large texts.
Set to nil to force pure Elisp implementation.")

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

(defvar ekp-default-cws-stretch-pixel 2
  "Max stretched pixel of whitespace between CJK chars.")

(defvar ekp-line-penalty 10
  "Penalty for each line break. Higher = fewer lines. Default 10.")

(defvar ekp-hyphen-penalty 50
  "Penalty for hyphenated breaks. Higher = avoid hyphenation. Default 50.")

(defvar ekp-adjacent-fitness-penalty 100
  "Penalty when adjacent lines differ in tightness by >1 class.")

(defvar ekp-consecutive-hyphen-penalty 100
  "Base penalty multiplier for consecutive hyphenated lines.
Actual penalty = this × count², encouraging spread of hyphens.")

(defvar ekp-forced-break-penalty 10000
  "Base penalty for forced breaks where no valid break exists.
High value ensures forced breaks are last resort.")

(defvar ekp-last-line-short-penalty 50
  "Penalty multiplier for underfilled last lines.
Applied as: this × (1 - fill-ratio) when fill < ekp-last-line-min-ratio.")

(defvar ekp-last-line-min-ratio 0.5
  "Minimum fill ratio for last line (0.0-1.0).")

(defvar ekp-looseness 0
  "Target line count offset: 0=optimal, +1=looser (more lines), -1=tighter (fewer lines).
When non-zero, the algorithm tracks multiple paths and selects the one
whose line count is closest to (optimal + looseness).")

(defvar ekp-threshold-factor 0
  "Threshold factor for early pruning (0 = disabled).
When > 0, breakpoints with demerits > best × (1 + factor) are skipped.
Typical value: 2.0 for moderate pruning, 5.0 for aggressive pruning.
Reduces computation time for long paragraphs at slight quality cost.")

(defvar ekp-flagged-penalty -10000
  "Penalty for flagged (forced) breaks.
Negative value means this break is preferred (mandatory).
When a box ends with a forced break marker, it will be selected.
Used for explicit line breaks in poetry, code blocks, etc.")

;;;; Paragraph Cache Structure
;;
;; All paragraph data is stored in a flat struct for O(1) access.
;; Cache key: sxhash of (string, fonts, spacing params)

(cl-defstruct (ekp-para (:constructor ekp-para--create))
  "Preprocessed paragraph data."
  string latin-font cjk-font
  boxes boxes-widths boxes-types glues-types
  hyphen-pixel hyphen-positions
  flagged-positions  ; vector of indices for forced line breaks
  ideal-prefixs min-prefixs max-prefixs
  ;; Store glue params at para creation time for consistent C module calls
  glue-params  ; plist (:lws-ideal :lws-shrink :lws-stretch :mws-* :cws-*)
  (dp-cache nil :type hash-table))

(defvar ekp--para-cache nil
  "Cache: hash-key → ekp-para struct.")

(defvar ekp--use-default-params t
  "Internal flag for parameter initialization.")

;;;; Initialization

(defun ekp-root-dir ()
  "Return directory containing ekp.el."
  (when ekp--load-file
    (file-name-directory ekp--load-file)))

(defun ekp--load-dicts ()
  "Load hyphenation dictionaries."
  (ekp-hyphen-load-languages
   (expand-file-name "dictionaries" (ekp-root-dir))))

(ekp--load-dicts)

;;;; Parameter Management

(defun ekp--params-set-p ()
  "Return non-nil if all spacing parameters are set."
  (and ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
       ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
       ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))

(defun ekp-param-set-default (string)
  "Set default spacing parameters based on STRING's font."
  (let* ((lws (ekp-word-spacing-pixel string))
         (mws (- lws 1)))
    (ekp-param-set lws (ceiling (/ (float lws) 2)) (ceiling (/ (float lws) 3))
                   mws (ceiling (/ (float mws) 2)) (ceiling (/ (float mws) 3))
                   0 ekp-default-cws-stretch-pixel 0)))

(defun ekp-param-set (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  "Set all spacing parameters.
LWS = Latin word space, MWS = mixed, CWS = CJK.
Each takes ideal, stretch (+), and shrink (-) values."
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
  (setq ekp--use-default-params nil))

;;;; Text Analysis

(defconst ekp--latin-regexp
  "[A-Za-z'\\-\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u024F\u1E00-\u1EFF]"
  "Regexp matching Latin characters including accented forms.")

(defun ekp--split-with-hyphen (string)
  "Split STRING into boxes with hyphenation points marked.
Returns (boxes-vector . hyphen-positions-vector)."
  (let* ((boxes (ekp-split-to-boxes string))
         (idx 0) new-boxes hyphen-idxs)
    (dolist (box (append boxes nil))
      (if (string-match
           (format "^\\([[{<„‚¿¡*@\"']*\\)\\(%s+\\)\\([]}>.,*?\"']*\\)$"
                   ekp--latin-regexp)
           box)
          ;; Latin word: apply hyphenation
          (let* ((left (match-string 1 box))
                 (word (match-string 2 box))
                 (right (match-string 3 box))
                 (parts (ekp-hyphen-boxes
                         (ekp-hyphen-create ekp-latin-lang) word))
                 (n (length parts)))
            (when left (setcar parts (concat left (car parts))))
            (when right (setcar (last parts)
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

(defun ekp--str-type (str)
  "STR should be single letter string."
  (cond
   ;; Whitespace (space, tab, etc.) or zero-width characters
   ((or (string-blank-p str) (= (string-width str) 0)) 'space)
   ;; a half-width cjk punct
   ((or (string= "“" str) (string= "”" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ((= (string-width str) 2)
    (if (ekp-cjk-fw-punct-p str)
        'cjk-punct
      'cjk))
   (t (error "Abnormal string width %s for %s"
             (string-width str) str))))

(defun ekp--box-type (box)
  (unless (or (null box) (string-empty-p box))
    ;; Space/zero-width boxes: type is (space . space)
    (if (or (string-blank-p box) (= (string-width box) 0))
        '(space . space)
      (cons (ekp--str-type (substring box 0 1))
            (ekp--str-type (substring box -1))))))

(defun ekp--glue-type (prev-box-type curr-box-type)
  "Lws means whitespace between latin words; cws means
whitespace between cjk words; mws means whitespace between
cjk and latin words; nws means no whitespace.
Space boxes (preserved whitespace) need no additional glue."
  (let ((before (cdr prev-box-type))
        (after (car curr-box-type)))
    (if before
        (cond
         ;; Space boxes: no additional glue needed
         ((or (eq before 'space) (eq after 'space)) 'nws)
         ((and (eq before 'latin) (eq after 'latin)) 'lws)
         ((and (eq before 'cjk) (eq after 'cjk)) 'cws)
         ((or (and (eq before 'cjk) (eq after 'latin))
              (and (eq before 'latin) (eq after 'cjk)))
          'mws)
         ((or (eq before 'cjk-punct) (eq after 'cjk-punct)) 'cws))
      'nws)))

(defun ekp--compute-glue-types (boxes boxes-types hyphen-positions)
  "Compute glue types for BOXES. Positions after HYPHEN-POSITIONS are 'nws."
  (let* ((n (length boxes))
         (glues (make-vector n nil))
         prev-type)
    (dolist (i (append hyphen-positions nil))
      (aset glues (1+ i) 'nws))
    (dotimes (i n)
      (unless (aref glues i)
        (let ((curr-type (aref boxes-types i)))
          (aset glues i (ekp--glue-type prev-type curr-type))
          (setq prev-type curr-type))))
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
  "Get minimum glue pixel (ideal - shrink) for TYPE using PARA's stored params."
  (- (ekp--para-glue-ideal para type)
     (ekp--para-glue-shrink para type)))

(defun ekp--para-glue-max (para type)
  "Get maximum glue pixel (ideal + stretch) for TYPE using PARA's stored params."
  (+ (ekp--para-glue-ideal para type)
     (ekp--para-glue-stretch para type)))

;;; ============================================================
;;; Cache Implementation: Fast Hash + Flat Structure
;;; ============================================================

(defun ekp--para-hash (string)
  "Compute fast hash key for STRING.
Uses sxhash on serialized representation to correctly handle text properties."
  (let ((latin-font (ekp-latin-font string))
        (cjk-font (ekp-cjk-font string)))
    ;; Combine: string content + text properties + fonts + spacing params
    ;; Use prin1-to-string on intervals to ensure property values are hashed
    (sxhash
     (list (sxhash string)
           (sxhash (prin1-to-string (object-intervals string)))
           latin-font cjk-font
           ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
           ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
           ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))))

(defun ekp--make-para (string)
  "Create and fully initialize ekp-para struct for STRING.
Computes ALL data in one pass: text, params, and prefix arrays."
  ;; Ensure params are set
  (when (or ekp--use-default-params (null (ekp--params-set-p)))
    (ekp-param-set-default string))
  (setq ekp--use-default-params t)
  ;; Extract fonts
  (let* ((latin-font (ekp-latin-font string))
         (cjk-font (ekp-cjk-font string))
         ;; Split into boxes with hyphenation
         (split-result (ekp--split-with-hyphen string))
         (boxes (car split-result))
         (hyphen-positions (cdr split-result))
         (n (length boxes))
         ;; Compute box properties
         (boxes-widths (vconcat (mapcar #'string-pixel-width boxes)))
         (boxes-types (vconcat (mapcar #'ekp--box-type boxes)))
         (glues-types (ekp--compute-glue-types
                       boxes boxes-types hyphen-positions))
         (hyphen-pixel (string-pixel-width "-"))
         ;; Compute prefix arrays in one pass
         (ideal-prefixs (make-vector (1+ n) 0))
         (min-prefixs (make-vector (1+ n) 0))
         (max-prefixs (make-vector (1+ n) 0)))
    ;; Single loop for all prefix computations
    (dotimes (i n)
      (let ((box-w (aref boxes-widths i))
            (glue-type (aref glues-types i)))
        (aset ideal-prefixs (1+ i)
              (+ (aref ideal-prefixs i) box-w
                 (ekp-glue-ideal-pixel glue-type)))
        (aset min-prefixs (1+ i)
              (+ (aref min-prefixs i) box-w
                 (ekp-glue-min-pixel glue-type)))
        (aset max-prefixs (1+ i)
              (+ (aref max-prefixs i) box-w
                 (ekp-glue-max-pixel glue-type)))))
    ;; Create struct with all data, including glue params at creation time
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
     :glue-params (list :lws-ideal ekp-lws-ideal-pixel
                        :lws-stretch ekp-lws-stretch-pixel
                        :lws-shrink ekp-lws-shrink-pixel
                        :mws-ideal ekp-mws-ideal-pixel
                        :mws-stretch ekp-mws-stretch-pixel
                        :mws-shrink ekp-mws-shrink-pixel
                        :cws-ideal ekp-cws-ideal-pixel
                        :cws-stretch ekp-cws-stretch-pixel
                        :cws-shrink ekp-cws-shrink-pixel)
     :dp-cache (make-hash-table :test 'eql :size 20))))

(defun ekp--get-para (string)
  "Get or create ekp-para struct for STRING.
This is the main entry point for cached paragraph data."
  (unless ekp--para-cache
    (setq ekp--para-cache (make-hash-table :test 'eql :size 100)))
  (let ((key (ekp--para-hash string)))
    (or (gethash key ekp--para-cache)
        (let ((para (ekp--make-para string)))
          (puthash key para ekp--para-cache)
          para))))

(defun ekp-clear-caches ()
  "Clear all paragraph caches."
  (interactive)
  (setq ekp--para-cache nil))

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

(defun ekp--hyphen-str (_string)
  "Return hyphen character."
  "-")

;;;; K-P Badness and Demerits
;;   demerits = (linepenalty + badness)² + penalties
;;
;; Fitness classes ensure visual consistency:
;;   0=tight, 1=decent, 2=loose, 3=very-loose
;;   Adjacent lines with class difference > 1 get extra penalty.

(defun ekp--compute-badness (adjustment-pixel flexibility-pixel)
  "Compute Knuth-Plass badness from ADJUSTMENT-PIXEL and FLEXIBILITY-PIXEL.
Returns 0 if no adjustment needed, 10000 (infinite) if impossible."
  (cond
   ((= adjustment-pixel 0) 0)
   ((<= flexibility-pixel 0) 10000)
   (t (let ((ratio (/ (float adjustment-pixel) flexibility-pixel)))
        (min 10000 (* 100 (expt (abs ratio) 3)))))))

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

(defun ekp--gaps-list (glues-types)
  "Count gaps by type: (latin-gaps mix-gaps cjk-gaps)."
  (list (seq-count (lambda (it) (eq 'lws it)) glues-types)
        (seq-count (lambda (it) (eq 'mws it)) glues-types)
        (seq-count (lambda (it) (eq 'cws it)) glues-types)))

(defun ekp--compute-stretch-capacity (para gaps-list)
  "Return total stretchable pixels for GAPS-LIST using PARA's stored params."
  (let ((params (ekp-para-glue-params para)))
    (+ (* (nth 0 gaps-list) (plist-get params :lws-stretch))
       (* (nth 1 gaps-list) (plist-get params :mws-stretch))
       (* (nth 2 gaps-list) (plist-get params :cws-stretch)))))

(defun ekp--compute-shrink-capacity (para gaps-list)
  "Return total shrinkable pixels for GAPS-LIST using PARA's stored params.
CJK gaps don't shrink."
  (let ((params (ekp-para-glue-params para)))
    (+ (* (nth 0 gaps-list) (plist-get params :lws-shrink))
       (* (nth 1 gaps-list) (plist-get params :mws-shrink)))))

(defun ekp--line-badness-and-fitness (para ideal-pixel line-pixel glues-types)
  "Compute badness, fitness class, and gaps for a line.
Uses PARA's stored glue params for consistent capacity calculation.
Returns (:badness NUM :fitness NUM :gaps LIST :adjustment NUM :flexibility NUM)."
  (let* ((glues-types (seq-drop glues-types 1))
         (gaps-list (ekp--gaps-list glues-types))
         (adjustment (- line-pixel ideal-pixel))
         (flexibility (if (> adjustment 0)
                          (ekp--compute-stretch-capacity para gaps-list)
                        (ekp--compute-shrink-capacity para gaps-list)))
         (badness (ekp--compute-badness adjustment flexibility))
         (fitness (ekp--compute-fitness-class adjustment flexibility)))
    (list :badness badness
          :fitness fitness
          :gaps gaps-list
          :adjustment adjustment
          :flexibility flexibility)))

(defun ekp--hyphenate-p (hyphen-positions n)
  "Return non-nil if position N ends with hyphenation.
HYPHEN-POSITIONS is a sorted vector of indices where hyphenation can occur.
Uses binary search for O(log n) lookup instead of O(n) linear search."
  (and hyphen-positions
       (> (length hyphen-positions) 0)
       (let ((lo 0)
             (hi (1- (length hyphen-positions))))
         (while (< lo hi)
           (let ((mid (/ (+ lo hi) 2)))
             (if (< (aref hyphen-positions mid) n)
                 (setq lo (1+ mid))
               (setq hi mid))))
         (= (aref hyphen-positions lo) n))))

(defun ekp--flagged-p (flagged-positions n)
  "Return non-nil if position N is a flagged (forced) break.
FLAGGED-POSITIONS is a sorted vector of indices where forced breaks occur.
Uses binary search for O(log n) lookup."
  (and flagged-positions
       (> (length flagged-positions) 0)
       (let ((lo 0)
             (hi (1- (length flagged-positions))))
         (while (< lo hi)
           (let ((mid (/ (+ lo hi) 2)))
             (if (< (aref flagged-positions mid) n)
                 (setq lo (1+ mid))
               (setq hi mid))))
         (= (aref flagged-positions lo) n))))

;;;; Dynamic Programming Line Breaking

(defun ekp--dp-init-arrays (n)
  "Initialize DP arrays for N boxes.
Returns (backptrs demerits rests gaps hyphen-counts fitness-classes line-counts alt-paths).
When looseness != 0, alt-paths tracks alternative paths by (position . line-count)."
  (let ((backptrs (make-vector (1+ n) nil))
        (demerits (make-vector (1+ n) nil))
        (rests (make-vector (1+ n) nil))
        (gaps (make-vector (1+ n) nil))
        (hyphen-counts (make-vector (1+ n) 0))
        (fitness-classes (make-vector (1+ n) 1))  ; default: decent
        (line-counts (make-vector (1+ n) 0))      ; for looseness
        ;; alt-paths: hash (position . line-count) -> (backptr . demerits)
        ;; Size based on estimated paths: n positions × ~10 possible line counts
        (alt-paths (when (/= ekp-looseness 0)
                     (make-hash-table :test 'equal :size (min 1000 (* n 10))))))
    (aset demerits 0 0.0)
    ;; Initialize alt-paths for position 0
    (when alt-paths
      (puthash (cons 0 0) (cons nil 0.0) alt-paths))
    (list backptrs demerits rests gaps
          hyphen-counts fitness-classes line-counts alt-paths)))

(defun ekp--leading-space-width (i boxes-types boxes-widths)
  "Compute total width of leading space boxes starting at position I.
Returns 0 if box at I is not a space box."
  (let ((n (length boxes-types))
        (width 0)
        (pos i))
    (while (and (< pos n)
                (let ((box-type (aref boxes-types pos)))
                  (and box-type (eq (car box-type) 'space))))
      (cl-incf width (aref boxes-widths pos))
      (cl-incf pos))
    width))

(defun ekp--trailing-space-width (k boxes-types boxes-widths)
  "Compute total width of trailing space boxes ending before position K.
K is the exclusive end position (break point).
Returns 0 if box at K-1 is not a space box."
  (let ((width 0)
        (pos (1- k)))
    (while (and (>= pos 0)
                (let ((box-type (aref boxes-types pos)))
                  (and box-type (eq (car box-type) 'space))))
      (cl-incf width (aref boxes-widths pos))
      (cl-decf pos))
    width))

(defun ekp--dp-line-metrics (para i k glues-types ideal-prefixs min-prefixs max-prefixs)
  "Compute line metrics for boxes I to K using PARA's stored glue params.
Returns (ideal-pixel min-pixel max-pixel) excluding leading glue.
For first line (i=0): includes leading space widths (paragraph indentation).
For non-first lines (i>0): excludes leading space widths (line-break artifacts).
Always excludes trailing space widths."
  (let* ((leading-glue-type (aref glues-types i))
         (boxes-types (ekp-para-boxes-types para))
         (boxes-widths (ekp-para-boxes-widths para))
         ;; For non-first lines, exclude leading space width (will be stripped)
         ;; First line (i=0) keeps leading spaces for paragraph indentation
         (leading-space-w (if (> i 0)
                              (ekp--leading-space-width i boxes-types boxes-widths)
                            0))
         ;; Always exclude trailing space width (always stripped)
         (trailing-space-w (ekp--trailing-space-width k boxes-types boxes-widths))
         (space-w (+ leading-space-w trailing-space-w)))
    (list (- (aref ideal-prefixs k) (aref ideal-prefixs i)
             (ekp--para-glue-ideal para leading-glue-type)
             space-w)
          (- (aref min-prefixs k) (aref min-prefixs i)
             (ekp--para-glue-min para leading-glue-type)
             space-w)
          (- (aref max-prefixs k) (aref max-prefixs i)
             (ekp--para-glue-max para leading-glue-type)
             space-w))))

(defun ekp--dp-force-break (para i k arrays glues-types hyphen-positions ideal-prefixs hyphen-pixel line-pixel)
  "Force a break at K-1 when no valid break found. Update ARRAYS.
Uses PARA's stored glue params for consistency."
  (let* ((backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (break-pos (1- k))
         (hyphenate-p (ekp--hyphenate-p hyphen-positions break-pos))
         (boxes-types (ekp-para-boxes-types para))
         (boxes-widths (ekp-para-boxes-widths para))
         ;; For non-first lines, exclude leading space width
         (leading-space-w (if (> i 0)
                              (ekp--leading-space-width i boxes-types boxes-widths)
                            0))
         ;; Always exclude trailing space width
         (trailing-space-w (ekp--trailing-space-width k boxes-types boxes-widths))
         (space-w (+ leading-space-w trailing-space-w))
         (ideal-pixel (- (aref ideal-prefixs break-pos)
                         (aref ideal-prefixs i)
                         (ekp--para-glue-ideal para (aref glues-types i))
                         space-w))
         (rest-pixel (- line-pixel ideal-pixel)))
    (when hyphenate-p (cl-incf ideal-pixel hyphen-pixel))
    ;; Force break with high demerits
    (aset demerits break-pos (+ ekp-forced-break-penalty (expt rest-pixel 2)))
    (aset rests break-pos rest-pixel)
    (aset backptrs break-pos i)
    (aset fitness-classes break-pos 3)  ; very loose
    (aset line-counts break-pos (1+ (aref line-counts i)))
    (aset gaps break-pos
          (ekp--gaps-list (seq-drop (cl-subseq glues-types i break-pos) 1)))))

(defun ekp--dp-compute-line-demerits (para j is-last end-with-hyphenp
                                        ideal-pixel line-pixel
                                        glues-types i k
                                        prev-hyphen-count prev-fitness
                                        &optional end-with-flaggedp)
  "Compute line demerits using full K-P formula.
Uses PARA's stored glue params for consistent badness calculation.
END-WITH-FLAGGEDP indicates a forced break (very low/negative demerits).
Returns (demerits gaps fitness new-hyphen-count)."
  (cond
   ;; Flagged (forced) break: use negative penalty to ensure selection
   (end-with-flaggedp
    (let* ((result (ekp--line-badness-and-fitness
                    para ideal-pixel line-pixel
                    (seq-subseq glues-types i k)))
           (line-gaps (plist-get result :gaps)))
      ;; Use flagged penalty (negative = preferred)
      (list ekp-flagged-penalty line-gaps 1 0)))
   ;; Single word line
   ((= j 0)
    (let* ((badness (ekp--compute-badness (- line-pixel ideal-pixel) 1))
           (fitness 1)  ; decent
           (penalty (if end-with-hyphenp ekp-hyphen-penalty 0))
           (new-hyphen (if end-with-hyphenp 1 0))
           (dem (ekp--compute-demerits badness penalty prev-fitness fitness
                                       end-with-hyphenp prev-hyphen-count)))
      (list dem nil fitness new-hyphen)))
   ;; Last line: minimal demerits if reasonably filled
   (is-last
    (let* ((fill-ratio (/ (float ideal-pixel) line-pixel))
           ;; Penalize if last line is too short
           (badness (if (< fill-ratio ekp-last-line-min-ratio)
                        (* ekp-last-line-short-penalty (- 1.0 fill-ratio))
                      0))
           (dem (expt (+ ekp-line-penalty badness) 2)))
      (list dem nil 1 0)))
   ;; Normal line
   (t
    (let* ((result (ekp--line-badness-and-fitness
                    para ideal-pixel line-pixel
                    (seq-subseq glues-types i k)))
           (badness (plist-get result :badness))
           (fitness (plist-get result :fitness))
           (line-gaps (plist-get result :gaps))
           (penalty (if end-with-hyphenp ekp-hyphen-penalty 0))
           (new-hyphen (if end-with-hyphenp (1+ prev-hyphen-count) 0))
           (dem (ekp--compute-demerits badness penalty prev-fitness fitness
                                       end-with-hyphenp prev-hyphen-count)))
      (list dem line-gaps fitness new-hyphen)))))

(defun ekp--dp-trace-breaks (backptrs n)
  "Trace optimal break points from BACKPTRS array."
  (let ((breaks (list n))
        (index n))
    (while (> index 0)
      (let ((prev (aref backptrs index)))
        (if prev
            (progn (push prev breaks)
                   (setq index prev))
          (setq index (1- index)))))
    (cdr breaks)))

(defun ekp--dp-trace-breaks-with-looseness (backptrs line-counts n target-lines
                                                     &optional alt-paths)
  "Trace breaks, preferring paths with TARGET-LINES line count.
Used for looseness parameter support.
ALT-PATHS is a hash table mapping (position . line-count) to (backptr . demerits)
for alternative paths when looseness != 0."
  (if (or (= ekp-looseness 0) (null alt-paths))
      (ekp--dp-trace-breaks backptrs n)
    ;; Find path closest to target line count
    (let* ((optimal-lines (aref line-counts n))
           (target (+ optimal-lines ekp-looseness))
           (best-path nil)
           (best-diff most-positive-fixnum))
      ;; Search alt-paths for best match at position n
      (maphash
       (lambda (key value)
         (when (= (car key) n)  ; position = n (end)
           (let* ((line-count (cdr key))
                  (diff (abs (- line-count target))))
             (when (< diff best-diff)
               (setq best-diff diff)
               (setq best-path (cons line-count (car value)))))))  ; (line-count . backptr)
       alt-paths)
      (if best-path
          ;; Trace back using alt-paths
          (ekp--dp-trace-alt-path alt-paths n (car best-path))
        ;; Fallback to optimal path
        (ekp--dp-trace-breaks backptrs n)))))

(defun ekp--dp-trace-alt-path (alt-paths n target-lines)
  "Trace alternative path from ALT-PATHS ending at N with TARGET-LINES."
  (let ((breaks (list n))
        (index n)
        (lines target-lines)
        (max-iterations (* n 2)))  ; Safety limit to prevent infinite loop
    (while (and (> index 0) (> max-iterations 0))
      (let* ((key (cons index lines))
             (entry (gethash key alt-paths)))
        (if entry
            (let ((prev (car entry)))
              (when (> prev 0) (push prev breaks))
              (setq index prev)
              (cl-decf lines))
          ;; No entry found at current line count, give up
          (setq index 0)))
      (cl-decf max-iterations))
    (cdr breaks)))

(defun ekp--dp-store-cache (string line-pixel dp-result)
  "Store DP-RESULT for STRING at LINE-PIXEL in para's dp-cache."
  (let ((para (ekp--get-para string)))
    (puthash line-pixel dp-result (ekp-para-dp-cache para))))

(defun ekp--dp-get-cached (para line-pixel)
  "Get cached DP result from PARA for LINE-PIXEL, or nil."
  (gethash line-pixel (ekp-para-dp-cache para)))

(defun ekp-dp-cache (string line-pixel)
  "Compute optimal line breaks for STRING at LINE-PIXEL width.
Uses Knuth-Plass dynamic programming with demerits.
If `ekp-use-c-module' is non-nil and C module is available, uses it."
  (let* ((para (ekp--get-para string))
         (cached (ekp--dp-get-cached para line-pixel)))
    (if cached
        cached
      ;; Try C module first (if enabled and available)
      (if (and ekp-use-c-module
               (boundp 'ekp-c-module-loaded) ekp-c-module-loaded
               (fboundp 'ekp-c-break-with-arrays))
          (ekp--dp-cache-via-c para string line-pixel)
        ;; Fallback to Elisp implementation
        (ekp--dp-cache-elisp para string line-pixel)))))

(defun ekp--glue-type-to-int (type)
  "Convert glue TYPE symbol to integer for C module.
0=nws, 1=lws, 2=mws, 3=cws."
  (pcase type
    ('lws 1)
    ('mws 2)
    ('cws 3)
    (_ 0)))  ; nws or nil

(defun ekp--prepare-para-for-batch (para line-pixel)
  "Prepare PARA data as vector for batch API at LINE-PIXEL.
Returns [ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks
         glue-stretches hyphen-positions hyphen-width line-width].
Uses PARA's stored glue-params to ensure consistency with prefix arrays."
  (let* ((ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (glues-types (ekp-para-glues-types para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (n (length (ekp-para-boxes para)))
         (glue-ideals (make-vector n 0))
         (glue-shrinks (make-vector n 0))
         (glue-stretches (make-vector n 0)))
    ;; Use para's stored glue params, not global variables
    (dotimes (i n)
      (let ((type (aref glues-types i)))
        (aset glue-ideals i (ekp--para-glue-ideal para type))
        (aset glue-shrinks i (ekp--para-glue-shrink para type))
        (aset glue-stretches i (ekp--para-glue-stretch para type))))
    (vector ideal-prefixs min-prefixs max-prefixs
            glue-ideals glue-shrinks glue-stretches
            hyphen-positions hyphen-pixel line-pixel)))

(defun ekp--store-batch-result (para line-pixel breaks cost)
  "Store batch result (BREAKS, COST) into PARA's dp-cache for LINE-PIXEL.
Computes rests and gaps from breaks using PARA's stored glue params."
  (let* ((glues-types (ekp-para-glues-types para))
         (ideal-prefixs (ekp-para-ideal-prefixs para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (start 0)
         lines-rests lines-gaps)
    (dolist (end breaks)
      (let* ((leading-glue-type (aref glues-types start))
             (end-with-hyphenp (ekp--hyphenate-p hyphen-positions (1- end)))
             (ideal-pixel (- (aref ideal-prefixs end)
                             (aref ideal-prefixs start)
                             (ekp--para-glue-ideal para leading-glue-type))))
        (when end-with-hyphenp
          (cl-incf ideal-pixel hyphen-pixel))
        (push (- line-pixel ideal-pixel) lines-rests)
        (push (ekp--gaps-list
               (seq-drop (cl-subseq glues-types start end) 1))
              lines-gaps)
        (setq start end)))
    (let ((dp-result (list :rests (nreverse lines-rests)
                           :gaps (nreverse lines-gaps)
                           :breaks breaks
                           :cost cost
                           :line-count (length breaks))))
      (puthash line-pixel dp-result (ekp-para-dp-cache para))
      dp-result)))

(defun ekp--dp-cache-batch (strings line-pixel)
  "Compute DP for multiple STRINGS in parallel using C batch API.
Returns list of dp-results in same order as STRINGS.
Only processes strings that aren't already cached."
  (let* ((paras (mapcar #'ekp--get-para strings))
         (needs-compute '())  ; list of (index . para)
         (results (make-vector (length strings) nil)))
    ;; Check which paras need computation
    (cl-loop for para in paras
             for i from 0
             for cached = (ekp--dp-get-cached para line-pixel)
             do (if cached
                    (aset results i cached)
                  (push (cons i para) needs-compute)))
    ;; If all cached, return immediately
    (if (null needs-compute)
        (append results nil)
      ;; Prepare batch input for uncached paras
      (let* ((needs-compute (nreverse needs-compute))
             (batch-input (vconcat
                           (mapcar (lambda (ip)
                                     (ekp--prepare-para-for-batch (cdr ip) line-pixel))
                                   needs-compute)))
             (batch-results (ekp-c-break-batch batch-input)))
        ;; Process results
        (cl-loop for ip in needs-compute
                 for j from 0
                 for idx = (car ip)
                 for para = (cdr ip)
                 for res = (aref batch-results j)
                 for breaks = (car res)
                 for cost = (cdr res)
                 do (if breaks
                        (aset results idx
                              (ekp--store-batch-result para line-pixel breaks cost))
                      ;; C failed, fallback to Elisp
                      (aset results idx
                            (ekp--dp-cache-elisp para (ekp-para-string para) line-pixel)))))
      (append results nil))))

(defun ekp--dp-cache-via-c (para string line-pixel)
  "Compute breaks using C module with Elisp's pre-computed prefix arrays.
C module receives ALL font-dependent data from Elisp's para struct:
prefix sums, glue values, hyphen info. C only does pure DP.
Uses PARA's stored glue-params for consistency with cached prefix arrays."
  (ignore string)  ; Use para's data instead
  (let* ((ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (glues-types (ekp-para-glues-types para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (n (length (ekp-para-boxes para)))
         ;; Build glue value arrays for C using para's stored params
         (glue-ideals (make-vector n 0))
         (glue-shrinks (make-vector n 0))
         (glue-stretches (make-vector n 0)))
    ;; Extract glue values from para's stored params, not global variables
    (dotimes (i n)
      (let ((type (aref glues-types i)))
        (aset glue-ideals i (ekp--para-glue-ideal para type))
        (aset glue-shrinks i (ekp--para-glue-shrink para type))
        (aset glue-stretches i (ekp--para-glue-stretch para type))))
    ;; Call C module with all Elisp-computed arrays
    (let* ((result (ekp-c-break-with-arrays
                    ideal-prefixs
                    min-prefixs
                    max-prefixs
                    glue-ideals
                    glue-shrinks
                    glue-stretches
                    hyphen-positions
                    hyphen-pixel
                    line-pixel))
           (c-breaks (car result))
           (c-cost (cdr result)))
      (if (null c-breaks)
          ;; C module failed, fallback to Elisp
          (ekp--dp-cache-elisp para string line-pixel)
        ;; C module succeeded: compute rests and gaps from breaks
        (let* ((breaks c-breaks)
               (start 0)
               lines-rests lines-gaps)
          ;; Compute rests and gaps for each line using para's stored params
          (dolist (end breaks)
            (let* ((leading-glue-type (aref glues-types start))
                   (end-with-hyphenp (ekp--hyphenate-p hyphen-positions (1- end)))
                   (ideal-pixel (- (aref ideal-prefixs end)
                                   (aref ideal-prefixs start)
                                   (ekp--para-glue-ideal para leading-glue-type))))
              (when end-with-hyphenp
                (cl-incf ideal-pixel hyphen-pixel))
              (push (- line-pixel ideal-pixel) lines-rests)
              (push (ekp--gaps-list
                     (seq-drop (cl-subseq glues-types start end) 1))
                    lines-gaps)
              (setq start end)))
          (let ((dp-result (list :rests (nreverse lines-rests)
                                 :gaps (nreverse lines-gaps)
                                 :breaks breaks
                                 :cost c-cost
                                 :line-count (length breaks))))
            (puthash line-pixel dp-result (ekp-para-dp-cache para))
            dp-result))))))

(defun ekp--dp-cache-elisp (para string line-pixel)
  "Pure Elisp DP implementation."
  (ignore string)  ; para already contains all needed data
  ;; Get data directly from struct (O(1) access)
  (let* ((glues-types (ekp-para-glues-types para))
         (boxes (ekp-para-boxes para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (flagged-positions (ekp-para-flagged-positions para))
         (n (length boxes))
         (ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (arrays (ekp--dp-init-arrays n))
         (backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (hyphen-counts (nth 4 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (alt-paths (nth 7 arrays))  ; for looseness support
         ;; Track best demerits at end for threshold pruning
         (best-end-demerits nil))
    ;; Main DP loop: for each reachable position i
    (dotimes (i (1+ n))
      (when (aref demerits i)
        ;; Threshold pruning: skip if demerits already too high
        (let ((should-process
               (or (<= ekp-threshold-factor 0)
                   (null best-end-demerits)
                   (<= (aref demerits i)
                       (* best-end-demerits (1+ ekp-threshold-factor))))))
          (when should-process
            (let ((prev-hyphen-count (aref hyphen-counts i))
                  (prev-fitness (aref fitness-classes i))
                  (prev-line-count (aref line-counts i)))
              (catch 'break
                ;; Try extending line to each position k > i
                (dotimes (j (- n i))
                  (let* ((k (+ i j 1))
                         (is-last (= k n))
                         ;; k is the break position (exclusive), k-1 is the last box index
                         (end-with-hyphenp
                          (ekp--hyphenate-p hyphen-positions (1- k)))
                         (end-with-flaggedp
                          (ekp--flagged-p flagged-positions (1- k)))
                       (metrics (ekp--dp-line-metrics
                                 para i k glues-types
                                 ideal-prefixs min-prefixs max-prefixs))
                       (ideal-pixel (nth 0 metrics))
                       (min-pixel (nth 1 metrics))
                       (max-pixel (nth 2 metrics)))
                  ;; Add hyphen width if line ends with hyphen
                  (when end-with-hyphenp
                    (cl-incf ideal-pixel hyphen-pixel)
                    (cl-incf max-pixel hyphen-pixel)
                    (cl-incf min-pixel hyphen-pixel))
                  ;; Check if line is too long (but allow flagged breaks anyway)
                  (when (and (not end-with-flaggedp)
                             (or (> min-pixel line-pixel)
                                 (and is-last (> ideal-pixel line-pixel))))
                    (when (null (aref demerits (1- k)))
                      (ekp--dp-force-break
                       para i k arrays glues-types hyphen-positions
                       ideal-prefixs hyphen-pixel line-pixel))
                    (throw 'break nil))
                  ;; Valid break point: compute demerits
                  ;; Flagged breaks are always valid
                  (when (or end-with-flaggedp
                            (<= min-pixel line-pixel max-pixel)
                            (and is-last (<= ideal-pixel line-pixel)))
                    (pcase-let ((`(,dem ,line-gaps ,fitness ,new-hyphen)
                                 (ekp--dp-compute-line-demerits
                                  para j is-last end-with-hyphenp
                                  ideal-pixel line-pixel glues-types i k
                                  prev-hyphen-count prev-fitness
                                  end-with-flaggedp)))
                      (let ((total-dem (+ (aref demerits i) dem))
                            (new-line-count (1+ prev-line-count)))
                        ;; Update optimal path (always)
                        (when (or (null (aref demerits k))
                                  (< total-dem (aref demerits k)))
                          (aset rests k (- line-pixel ideal-pixel))
                          (aset gaps k line-gaps)
                          (aset demerits k total-dem)
                          (aset backptrs k i)
                          (aset fitness-classes k fitness)
                          (aset hyphen-counts k new-hyphen)
                          (aset line-counts k new-line-count)
                          ;; Update best end demerits for threshold pruning
                          (when (= k n)
                            (when (or (null best-end-demerits)
                                      (< total-dem best-end-demerits))
                              (setq best-end-demerits total-dem))))
                        ;; Track alternative paths for looseness (if enabled)
                        (when alt-paths
                          (let* ((key (cons k new-line-count))
                                 (existing (gethash key alt-paths)))
                            (when (or (null existing)
                                      (< total-dem (cdr existing)))
                              (puthash key (cons i total-dem) alt-paths)))))))))))))))
    ;; Extract optimal solution
    (let* ((breaks (ekp--dp-trace-breaks-with-looseness
                    backptrs line-counts n (aref line-counts n) alt-paths))
           (lines-rests (mapcar (lambda (i) (aref rests i)) breaks))
           (lines-gaps (mapcar (lambda (i) (aref gaps i)) breaks))
           (dp-result (list :rests lines-rests
                            :gaps lines-gaps
                            :breaks breaks
                            :cost (aref demerits n)
                            :line-count (aref line-counts n))))
      (puthash line-pixel dp-result (ekp-para-dp-cache para))
      dp-result)))

(defun ekp-dp-data (string line-pixel &optional key)
  "Return the data plist of dp cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (ekp-dp-cache string line-pixel)))
    (if key
        (plist-get data key)
      data)))

(defun ekp-total-cost (string line-pixel)
  "Return the COST of kp algorithm."
  (ekp-dp-data string line-pixel :cost))

(defun ekp-line-breaks (string line-pixel)
  "Return the break points of kp algorithm."
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
         (cjk-change (if stretch-p (plist-get params :cws-stretch) 0))
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
    ;; Finally to CJK gaps
    (when (and (> remaining 0) (> cjk-gaps 0))
      (setq cjk-adj (/ remaining cjk-gaps))
      (setq cjk-extra (% remaining cjk-gaps)))
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
                     ('nws 0)
                     (_ 0))))
         (if stretch-p (+ base adj) (- base adj))))
     glues-types)))

(defun ekp--line-glue-single-box (line-pixel box-width hyphen-p hyphen-pixel)
  "Compute glues for a single-box line."
  (let ((trailing (- line-pixel box-width (if hyphen-p hyphen-pixel 0))))
    (list 0 trailing)))

(defun ekp--line-glue-last-line (para glues-types ideal-pixel line-pixel)
  "Compute glues for last line (ragged right). Uses PARA's stored glue params."
  (append '(0)
          (mapcar (lambda (type) (ekp--para-glue-ideal para type)) glues-types)
          (list (- line-pixel ideal-pixel))))

(defun ekp--line-glue-normal (para glues-types rest-pixel gaps-list)
  "Compute glues for a normal (justified) line. Uses PARA's stored glue params."
  (if (= rest-pixel 0)
      (append '(0) (mapcar (lambda (type) (ekp--para-glue-ideal para type)) glues-types) '(0))
    (let* ((stretch-p (> rest-pixel 0))
           (distribution (ekp--distribute-gap-adjustment
                          para (abs rest-pixel) gaps-list stretch-p))
           (glue-pixels (ekp--compute-glue-pixels para glues-types distribution stretch-p)))
      (append '(0) glue-pixels '(0)))))

(defun ekp-line-glues (string line-pixel)
  "Compute glue pixels for each line after breaking STRING at LINE-PIXEL.
Returns vector of vectors, each inner vector is glue pixels for one line.
Each line's glues: [0 glue1 glue2 ... trailing-space].
Uses the cached para's stored glue params for consistency."
  (let* ((para (ekp--get-para string))
         (boxes-widths (ekp--boxes-widths string))
         (boxes-num (length (ekp--boxes string)))
         (glues-types (ekp--glues-types string))
         (hyphen-positions (ekp--hyphen-positions string))
         (ideal-prefixs (ekp--ideal-prefixs string))
         (max-prefixs (ekp--max-prefixs string))
         (breaks (ekp-line-breaks string line-pixel))
         (lines-rests (ekp-dp-data string line-pixel :rests))
         (lines-gaps (ekp-dp-data string line-pixel :gaps))
         (hyphen-pixel (ekp--hyphen-pixel string))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-types (seq-drop (cl-subseq glues-types start end) 1))
             (is-last (>= end boxes-num))
             ;; end is exclusive, end-1 is the last box index
             (hyphen-p (ekp--hyphenate-p hyphen-positions (1- end)))
             (ideal-pixel (- (aref ideal-prefixs end)
                             (aref ideal-prefixs start)
                             (ekp--para-glue-ideal para (aref glues-types start))))
             (max-pixel (+ (- (aref max-prefixs end)
                              (aref max-prefixs start)
                              (ekp--para-glue-max para (aref glues-types start)))
                           (if hyphen-p hyphen-pixel 0)))
             glue-list)
        (setq glue-list
              (cond
               ;; Single box: just trailing space
               ((= 1 (length line-boxes-widths))
                (ekp--line-glue-single-box line-pixel
                                           (aref line-boxes-widths 0)
                                           hyphen-p hyphen-pixel))
               ;; Last line: ragged right
               (is-last
                (ekp--line-glue-last-line
                 para line-glues-types ideal-pixel line-pixel))
               ;; Forced break (line too short even at max stretch)
               ((< max-pixel line-pixel)
                (append '(0)
                        (mapcar (lambda (type)
                                  (ekp--para-glue-max para type))
                                line-glues-types)
                        (list (- line-pixel max-pixel))))
               ;; Normal justified line
               (t
                (ekp--line-glue-normal para line-glues-types
                                       (nth i lines-rests)
                                       (nth i lines-gaps)))))
        (aset line-glues i (vconcat glue-list))
        (setq start end)))
    line-glues))

(defun ekp--box-space-p (box)
  "Return non-nil if BOX is a whitespace-only box."
  (and box (not (string-empty-p box))
       (or (string-blank-p box) (= (string-width box) 0))))

(defun ekp--interleave (list1 list2)
  "Interleave elements of LIST1 and LIST2."
  (let (result)
    (while (or list1 list2)
      (when list1 (push (pop list1) result))
      (when list2 (push (pop list2) result)))
    (nreverse result)))

(defun ekp--combine-glues-and-boxes (glues boxes)
  "Combine GLUES (n+1 elements) and BOXES (n elements) into string."
  (let* ((glues (append glues nil))
         (last-glue (car (last glues)))
         (glues (butlast glues))
         (boxes (append boxes nil)))
    (if (= (length glues) (length boxes))
        (string-join (append (ekp--interleave glues boxes)
                             (list last-glue)))
      (error "Glues count (%d) must equal boxes count (%d) + 1"
             (1+ (length glues)) (length boxes)))))

(defun ekp--pixel-spacing-width (spacing)
  "Extract pixel width from a SPACING created by `ekp-pixel-spacing'."
  (if (string-empty-p spacing)
      0
    (let ((display (get-text-property 0 'display spacing)))
      (if (and display (eq (car display) 'space))
          (let ((width-spec (plist-get (cdr display) :width)))
            (if (listp width-spec) (car width-spec) (or width-spec 0)))
        0))))

(defun ekp--redistribute-extra-width (glues extra-width)
  "Redistribute EXTRA-WIDTH across GLUES proportionally.
GLUES is a list of pixel spacing strings. Returns adjusted list.
The extra width is distributed to all glues except leading (first) glue."
  (when (and glues (> extra-width 0))
    (let* ((inner-glues (butlast (cdr glues)))  ; glues between boxes (not leading/trailing)
           (n (length inner-glues)))
      (if (= n 0)
          ;; No inner glues, add all to trailing
          (let* ((trailing (car (last glues)))
                 (old-width (ekp--pixel-spacing-width trailing))
                 (new-width (+ old-width extra-width)))
            (setf (car (last glues)) (ekp-pixel-spacing new-width)))
        ;; Distribute across inner glues
        (let ((per-glue (/ extra-width n))
              (remainder (% extra-width n))
              (idx 0))
          (setq glues
                (cons (car glues)  ; leading glue unchanged
                      (append
                       (mapcar
                        (lambda (g)
                          (let* ((old-w (ekp--pixel-spacing-width g))
                                 (extra (+ per-glue (if (< idx remainder) 1 0)))
                                 (new-w (+ old-w extra)))
                            (cl-incf idx)
                            (ekp-pixel-spacing new-w)))
                        inner-glues)
                       (last glues))))))))  ; trailing glue unchanged
  glues)

(defun ekp--strip-line-spaces (line-boxes line-glues line-boxes-widths
                                &optional strip-leading strip-trailing)
  "Strip leading/trailing space boxes from LINE-BOXES based on flags.
STRIP-LEADING: if non-nil, strip leading space boxes (default: nil = keep).
STRIP-TRAILING: if non-nil, strip trailing space boxes (default: nil = keep).
Returns (stripped-boxes . adjusted-glues) with extra width redistributed.
LINE-BOXES-WIDTHS is the pixel widths corresponding to LINE-BOXES.
The removed space width is redistributed to remaining glues for proper justification."
  (let* ((boxes (append line-boxes nil))
         (glues (append line-glues nil))
         (widths (append line-boxes-widths nil))
         (removed-width 0))  ; Track total width of removed space boxes
    (when (> (length boxes) 0)
      ;; Strip trailing space boxes (if requested)
      (when strip-trailing
        (while (and boxes (ekp--box-space-p (car (last boxes))))
          ;; Accumulate width of removed space box
          (cl-incf removed-width (car (last widths)))
          (setq boxes (butlast boxes))
          (setq widths (butlast widths))
          ;; Remove second-to-last glue (the one before the trailing space box)
          ;; Keep the last glue which is trailing space for the line
          (when (> (length glues) 1)
            (setq glues (append (butlast (butlast glues)) (last glues))))))
      ;; Strip leading space boxes (if requested)
      (when strip-leading
        (while (and boxes (ekp--box-space-p (car boxes)))
          ;; Accumulate width of removed space box
          (cl-incf removed-width (car widths))
          (setq boxes (cdr boxes))
          (setq widths (cdr widths))
          ;; Remove the second glue (the one after the leading glue)
          (when (> (length glues) 1)
            (setq glues (cons (car glues) (cddr glues)))))))
    ;; Redistribute removed width to remaining glues for proper justification
    (when (> removed-width 0)
      (setq glues (ekp--redistribute-extra-width glues removed-width)))
    (cons (vconcat boxes) glues)))

(defun ekp--pixel-justify (string line-pixel)
  "Justify single STRING to LINE-PIXEL."
  (let* ((boxes (ekp--boxes string))
         (boxes-widths (ekp--boxes-widths string))
         (hyphen (ekp--hyphen-str string))
         (breaks (ekp-line-breaks string line-pixel))
         (num (length breaks))
         (lines-glues (ekp-line-glues string line-pixel))
         (hyphen-positions (ekp--hyphen-positions string))
         (start 0) strings)
    (dotimes (i num)
      (let* ((end (nth i breaks))
             (line-boxes (cl-subseq boxes start end))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-raw (mapcar #'ekp-pixel-spacing
                                     (aref lines-glues i)))
             ;; Strip space boxes:
             ;; - First line (i=0): keep leading spaces (paragraph indentation)
             ;; - Other lines: strip leading spaces (line-break artifacts)
             ;; - All lines: strip trailing spaces
             (is-first-line (= i 0))
             (stripped (ekp--strip-line-spaces line-boxes line-glues-raw
                                               line-boxes-widths
                                               (not is-first-line)  ; strip-leading
                                               t))                   ; strip-trailing
             (line-boxes (car stripped))
             (line-glues (cdr stripped))
             ;; Check if last box of this line needs hyphen
             (last-box-idx (1- end))
             (need-hyphen
              (and (< i (1- num))  ; not last line
                   (ekp--hyphenate-p hyphen-positions last-box-idx))))
        (when (and need-hyphen (> (length line-boxes) 0))
          (setf (aref line-boxes (1- (length line-boxes)))
                (concat (aref line-boxes (1- (length line-boxes))) hyphen)))
        (when (> (length line-boxes) 0)
          (push (ekp--combine-glues-and-boxes line-glues line-boxes)
                strings))
        (setq start end)))
    (mapconcat 'identity (nreverse strings) "\n")))

(defun ekp-pixel-justify (string line-pixel)
  "Justify multiline STRING to LINE-PIXEL.
When C module is available and enabled, uses parallel batch processing."
  (let* ((strs (split-string string "\n"))
         (non-blank-strs (cl-remove-if #'string-blank-p strs))
         (use-batch (and ekp-use-c-module
                         (boundp 'ekp-c-module-loaded) ekp-c-module-loaded
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
;; Uses ternary search with aggressive caching.
;; The key optimization: reuse box/glue preprocessing across all widths.

(defun ekp--compute-avg-cost (strings pixel)
  "Compute average cost for STRINGS at PIXEL width."
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
    ;; Final linear scan over remaining 3 candidates
    (let ((best-pixel lo)
          (best-cost (ekp--compute-avg-cost strings lo)))
      (dolist (p (list (1+ lo) hi))
        (when (<= p max-pixel)
          (let ((cost (ekp--compute-avg-cost strings p)))
            (when (< cost best-cost)
              (setq best-cost cost
                    best-pixel p)))))
      best-pixel)))

(defun ekp-pixel-range-justify (string min-pixel max-pixel)
  "Find optimal width for STRING between MIN-PIXEL and MAX-PIXEL.
Returns (justified-text . optimal-pixel).
Uses ternary search for O(log n) width evaluations.
All preprocessing is cached via ekp--para-cache."
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
