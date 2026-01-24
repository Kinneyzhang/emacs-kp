;; -*- lexical-binding: t; -*-

(require 'ekp-utils)
(require 'ekp-hyphen)

(defconst ekp-load-file-name (or load-file-name (buffer-file-name)))

(defvar ekp-latin-lang "en_US")

(defvar ekp-param-use-default-p t
  "Used in internal, you should not modify it!")

(defvar ekp-lws-ideal-pixel nil
  "The ideal pixel of whitespace between latin words.")

(defvar ekp-lws-stretch-pixel nil
  "The stretch pixel of whitespace between latin words.")

(defvar ekp-lws-shrink-pixel nil
  "The shrink pixel of whitespace between latin words.")

(defvar ekp-mws-ideal-pixel nil
  "The ideal pixel of whitespace between latin word and cjk char.")

(defvar ekp-mws-stretch-pixel nil
  "The stretch pixel of whitespace between latin word and cjk char.")

(defvar ekp-mws-shrink-pixel nil
  "The shrink pixel of whitespace between latin word and cjk char.")

(defvar ekp-cws-ideal-pixel nil
  "The ideal pixel of non-whitespace, such between cjk chars.")

(defvar ekp-cws-stretch-pixel nil
  "The stretch pixel of non-whitespace, such between cjk chars.")

(defvar ekp-cws-shrink-pixel nil
  "The shrink pixel of non-whitespace, such between cjk chars.")

(defvar ekp-lws-max-pixel nil)

(defvar ekp-lws-min-pixel nil)

(defvar ekp-mws-max-pixel nil)

(defvar ekp-mws-min-pixel nil)

(defvar ekp-cws-max-pixel nil)

(defvar ekp-cws-min-pixel nil)

;;; Knuth-Plass Algorithm Parameters
;; These control the trade-offs in line breaking optimization.
;; See: Knuth & Plass, "Breaking Paragraphs into Lines" (1981)

(defvar ekp-line-penalty 10
  "Penalty added for each line break (K-P: linepenalty).
Higher values prefer fewer lines with more stretching.
Typical range: 0-100. Default 10.")

(defvar ekp-hyphen-penalty 50
  "Penalty for breaking a word with hyphen (K-P: hyphenpenalty).
Higher values avoid hyphenation. Default 50.")

(defvar ekp-adjacent-fitness-penalty 100
  "Penalty when adjacent lines differ in fitness class by > 1.
Ensures visual consistency. Default 100.")

(defvar ekp-last-line-min-ratio 0.5
  "Minimum fill ratio for last line (0.0-1.0).
Avoids orphaned words. Default 0.5 = at least half width.")

(defvar ekp-looseness 0
  "Target line count adjustment from optimal.
0 = optimal, +1 = one more line (looser), -1 = one fewer line (tighter).
Useful for fitting text to specific space.")

(defvar ekp-caches
  (make-hash-table
   :test 'equal :size 100 :rehash-size 1.5 :weakness nil)
  "Key of ekp-caches is the hash of string.")

(defun ekp-root-dir ()
  (when ekp-load-file-name
    (file-name-directory ekp-load-file-name)))

(defun ekp-load-dicts ()
  (ekp-hyphen-load-languages
   (expand-file-name "./dictionaries" (ekp-root-dir))))

(ekp-load-dicts)

(defun ekp-param-check ()
  "Check whether all params are set."
  (and ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
       ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
       ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))

(defun ekp-param-set-default (string)
  (let* ((lws-pixel (ekp-word-spacing-pixel string))
         (mws-pixel (- lws-pixel 2)))
    (ekp-param-set
     lws-pixel (round (* lws-pixel 0.5)) (round (* lws-pixel 0.333))
     mws-pixel (round (* mws-pixel 0.5)) (round (* mws-pixel 0.333))
     0 2 0)))

(defun ekp-param-set ( lws-ideal lws-stretch lws-shrink
                       mws-ideal mws-stretch mws-shrink
                       cws-ideal cws-stretch cws-shrink)
  (setq ekp-lws-ideal-pixel lws-ideal)
  (setq ekp-lws-stretch-pixel lws-stretch)
  (setq ekp-lws-shrink-pixel lws-shrink)
  (setq ekp-mws-ideal-pixel mws-ideal)
  (setq ekp-mws-stretch-pixel mws-stretch)
  (setq ekp-mws-shrink-pixel mws-shrink)
  (setq ekp-cws-ideal-pixel cws-ideal)
  (setq ekp-cws-stretch-pixel cws-stretch)
  (setq ekp-cws-shrink-pixel cws-shrink)
  (unless (ekp-param-check)
    (error "all pixel args should not be nil!"))
  (setq ekp-lws-max-pixel (+ ekp-lws-ideal-pixel ekp-lws-stretch-pixel))
  (setq ekp-lws-min-pixel (- ekp-lws-ideal-pixel ekp-lws-shrink-pixel))
  (setq ekp-mws-max-pixel (+ ekp-mws-ideal-pixel ekp-mws-stretch-pixel))
  (setq ekp-mws-min-pixel (- ekp-mws-ideal-pixel ekp-mws-shrink-pixel))
  (setq ekp-cws-max-pixel (+ ekp-cws-ideal-pixel ekp-cws-stretch-pixel))
  (setq ekp-cws-min-pixel (- ekp-cws-ideal-pixel ekp-cws-shrink-pixel))
  (setq ekp-param-use-default-p nil))

(defun ekp-param-fmtstr ()
  (format "%s-%s-%s-%s-%s-%s-%s-%s-%s"
          ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
          ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
          ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 正确版本：包含完整字符集和组合标记
(defvar ekp-latin-regexp
  (concat
   "["                               ; 开始字符集
   "A-Za-z'-"                          ; 基础拉丁字母
   "\300-\326\330-\366\370-\417"     ; ISO-8859-1补充
   "\u00C0-\u00D6\u00D8-\u00F6"      ; Unicode基本补充
   "\u00F8-\u00FF\u0100-\u024F"      ; 扩展A/B
   "\u1E00-\u1EFF"                   ; 扩展附加
   "\uA780-\uA7F9"                   ; 拉丁扩展-D
   "]"                               ; 闭合字符集
   )
  "正则表达式匹配所有拉丁字符及其变体，包括组合变音符")

(defun ekp-split-string (string)
  ;; return (boxes-vector . hyphen-positions-vector)
  (let* ((boxes (ekp-split-to-boxes string))
         (idx 0)
         new-boxes idxs)
    (dolist (box (append boxes nil))
      (save-match-data
        (if (string-match
             (format
              "^\\([[{<„‚¿¡*@\"']*\\)\\(%s+\\)\\([]}>.,*?\"']*\\)$"
              ekp-latin-regexp)
             box)
            (let* ((pure-word (match-string 2 box))
                   (left-punct (match-string 1 box))
                   (right-punct (match-string 3 box))
                   (word-lst (ekp-hyphen-boxes
                              (ekp-hyphen-create ekp-latin-lang)
                              pure-word))
                   (num (length word-lst)))
              (when left-punct
                (setf (car word-lst) (concat left-punct
                                             (car word-lst))))
              (when right-punct
                (setf (car (last word-lst))
                      (concat (car (last word-lst))
                              right-punct)))
              (push word-lst new-boxes)
              (dotimes (i num)
                (when (< i (1- num))
                  (push idx idxs))
                (cl-incf idx)))
          (push (list box) new-boxes)
          (cl-incf idx))))
    (let ((boxes-lst (apply #'append (nreverse new-boxes))))
      (cons (vconcat boxes-lst)
            (vconcat (nreverse idxs))))))

(defun ekp-str-type (str)
  "STR should be single letter string."
  (cond
   ;; a half-width cjk punct
   ((or (string= "“" str) (string= "”" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ((= (string-width str) 2)
    (if (ekp-cjk-fw-punct-p str)
        'cjk-punct
      'cjk))
   (t (error "Abnormal string width %s for %s"
             (string-width str) str))))

(defun ekp-box-type (box)
  (unless (or (null box) (string-empty-p box))
    (cons (ekp-str-type (substring box 0 1))
          (ekp-str-type (substring box -1)))))

(defun ekp-glue-type (prev-box-type curr-box-type)
  "Lws means whitespace between latin words; cws means
whitespace between cjk words; mws means whitespace between
cjk and latin words; nws means no whitespace."
  (let ((before (cdr prev-box-type))
        (after (car curr-box-type)))
    (if before
        (cond
         ((and (eq before 'latin) (eq after 'latin)) 'lws)
         ((and (eq before 'cjk) (eq after 'cjk)) 'cws)
         ((or (and (eq before 'cjk) (eq after 'latin))
              (and (eq before 'latin) (eq after 'cjk)))
          'mws)
         ((or (eq before 'cjk-punct) (eq after 'cjk-punct)) 'cws))
      'nws)))

(defun ekp--glues-types (boxes boxes-types hyphen_positions)
  "Set type of all glue in boxes using `ekp-glue-type',
set type to 'nws for each glue after position in hyphen_positions."
  ;; IMPORTANT!
  (let* ((num (length boxes))
         (glues-types (make-vector num nil))
         prev-box-type curr-box-type)
    ;; set hyphen position to 'nws
    (dolist (i (append hyphen_positions nil))
      (aset glues-types (1+ i) 'nws))
    (dotimes (i num)
      (unless (aref glues-types i)
        (let ((curr-box-type (aref boxes-types i)))
          (aset glues-types
                i (ekp-glue-type prev-box-type curr-box-type))
          (setq prev-box-type curr-box-type))))
    glues-types))

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

(defun ekp-text-hash (string)
  (let ((latin-font (ekp-latin-font string))
        (cjk-font (ekp-cjk-font string))
        (print-text-properties t))
    (secure-hash
     'md5 (format "%s|%s|%s" latin-font cjk-font (prin1-to-string string)))))

(defun ekp-text-cache (string)
  ;; consider font
  ;; (text-data . param-cache(param-data . dp-cache(dp-data)))
  "Return the text cache of STRING. Text cache consists of
(data . param-cache). Data is a plist (:boxes boxes :boxes-widths
boxes-widths :glues-types glues-types)."
  (let ((text-hash (ekp-text-hash string)))
    (if-let ((_ ekp-caches)
             (cache (gethash text-hash ekp-caches)))
        cache
      (when (or ekp-param-use-default-p
                (null (ekp-param-check)))
        (ekp-param-set-default string))
      (setq ekp-param-use-default-p t)
      (let* ((cjk-font (ekp-cjk-font string))
             (latin-font (ekp-latin-font string))
             (cons (ekp-split-string string))
             (boxes (car cons))
             (hyphen_after_positions (cdr cons))
             (boxes-widths (vconcat
                            (mapcar #'string-pixel-width boxes)))
             (boxes-types (vconcat (mapcar #'ekp-box-type boxes)))
             (glues-types (ekp--glues-types boxes boxes-types
                                            hyphen_after_positions))
             (plist (list :boxes boxes
                          :latin-font latin-font
                          :cjk-font cjk-font
                          :boxes-widths boxes-widths
                          :boxes-types boxes-types
                          :glues-types glues-types))
             (cache (cons plist nil)))
        (unless ekp-caches
          (setq ekp-caches (make-hash-table
                            :test 'equal :size 100
                            :rehash-size 1.5 :weakness nil)))
        (puthash text-hash cache ekp-caches)
        cache))))

(defun ekp-text-data (string &optional key)
  "Return the data plist of text cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (car (ekp-text-cache string))))
    (if key
        (plist-get data key)
      data)))

(defun ekp-boxes (string)
  (ekp-text-data string :boxes))

(defun ekp-hyphen-str (string)
  "-"
  ;; (propertize
  ;;  "-"
  ;;  'face `(:family ,(ekp-text-data string :latin-font)))
  )

(defun ekp-hyphen-pixel (string)
  (string-pixel-width (ekp-hyphen-str string)))

(defun ekp-boxes-widths (string)
  (ekp-text-data string :boxes-widths))

(defun ekp-boxes-types (string)
  (ekp-text-data string :boxes-types))

(defun ekp-glues-types (string)
  (ekp-text-data string :glues-types))

(defun ekp-param-cache (string)
  ;; (param-data . dp-cache(dp-data))
  "Return a plist of ideal-prefixs, min-prefixs and max-prefixs
by caculating with string and other params."
  (if-let* ((param-record (cdr (ekp-text-cache string)))
            (param-cache (gethash (ekp-param-fmtstr) param-record)))
      param-cache
    (let* ((boxes-num (length (ekp-boxes string)))
           (boxes-widths (ekp-boxes-widths string))
           (glues-types (ekp-glues-types string))
           (ideal-prefixs (make-vector (1+ boxes-num) 0))
           (min-prefixs (make-vector (1+ boxes-num) 0))
           (max-prefixs (make-vector (1+ boxes-num) 0)))
      (dotimes (i boxes-num)
        (aset ideal-prefixs (1+ i)
              (+ (aref ideal-prefixs i) (aref boxes-widths i)
                 (ekp-glue-ideal-pixel (aref glues-types i))))
        (aset min-prefixs (1+ i)
              (+ (aref min-prefixs i) (aref boxes-widths i)
                 (ekp-glue-min-pixel (aref glues-types i))))
        (aset max-prefixs (1+ i)
              (+ (aref max-prefixs i) (aref boxes-widths i)
                 (ekp-glue-max-pixel (aref glues-types i)))))
      (let* ((param-data (list :ideal-prefixs ideal-prefixs
                               :min-prefixs min-prefixs
                               :max-prefixs max-prefixs))
             (param-cache (cons param-data nil)))
        (if-let ((param-record (cdr (ekp-text-cache string))))
            (puthash (ekp-param-fmtstr) param-cache param-record)
          (let ((param-record (make-hash-table
                               :test 'equal :size 100
                               :rehash-size 1.5 :weakness nil)))
            (puthash (ekp-param-fmtstr) param-cache param-record)
            (puthash (ekp-text-hash string)
                     (cons (ekp-text-data string) param-record)
                     ekp-caches)))
        param-cache))))

(defun ekp-param-data (string &optional key)
  "Return the data plist of param cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (car (ekp-param-cache string))))
    (if key
        (plist-get data key)
      data)))

(defun ekp-ideal-prefixs (string)
  (ekp-param-data string :ideal-prefixs))

(defun ekp-min-prefixs (string)
  (ekp-param-data string :min-prefixs))

(defun ekp-max-prefixs (string)
  (ekp-param-data string :max-prefixs))

;;; Knuth-Plass Badness and Demerits
;; 
;; K-P defines badness as how much a line deviates from ideal:
;;   badness = 100 * |r|³  where r = adjustment / flexibility
;;
;; Demerits combine badness with penalties to rank line breaks:
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
                          (+ with-fitness (* 100 hyphen-count hyphen-count))
                        with-fitness)))
    with-hyphen))

(defun ekp--gaps-list (glues-types)
  "Count gaps by type: (latin-gaps mix-gaps cjk-gaps)."
  (list (seq-count (lambda (it) (eq 'lws it)) glues-types)
        (seq-count (lambda (it) (eq 'mws it)) glues-types)
        (seq-count (lambda (it) (eq 'cws it)) glues-types)))

(defun ekp--compute-stretch-capacity (gaps-list)
  "Return total stretchable pixels for GAPS-LIST."
  (+ (* (nth 0 gaps-list) ekp-lws-stretch-pixel)
     (* (nth 1 gaps-list) ekp-mws-stretch-pixel)
     (* (nth 2 gaps-list) ekp-cws-stretch-pixel)))

(defun ekp--compute-shrink-capacity (gaps-list)
  "Return total shrinkable pixels for GAPS-LIST (CJK gaps don't shrink)."
  (+ (* (nth 0 gaps-list) ekp-lws-shrink-pixel)
     (* (nth 1 gaps-list) ekp-mws-shrink-pixel)))

(defun ekp--line-badness-and-fitness (ideal-pixel line-pixel glues-types)
  "Compute badness, fitness class, and gaps for a line.
Returns (:badness NUM :fitness NUM :gaps LIST :adjustment NUM :flexibility NUM)."
  (let* ((glues-types (seq-drop glues-types 1))
         (gaps-list (ekp--gaps-list glues-types))
         (adjustment (- line-pixel ideal-pixel))
         (flexibility (if (> adjustment 0)
                          (ekp--compute-stretch-capacity gaps-list)
                        (ekp--compute-shrink-capacity gaps-list)))
         (badness (ekp--compute-badness adjustment flexibility))
         (fitness (ekp--compute-fitness-class adjustment flexibility)))
    (list :badness badness
          :fitness fitness
          :gaps gaps-list
          :adjustment adjustment
          :flexibility flexibility)))

;; Keep old function for compatibility
(defun ekp--line-cost-and-gaps (ideal-pixel line-pixel glues-types)
  "Compute badness cost for a line using Knuth-Plass formula.
IDEAL-PIXEL is natural width, LINE-PIXEL is target width.
Returns (:cost NUMBER :gaps GAPS-LIST)."
  (let* ((result (ekp--line-badness-and-fitness ideal-pixel line-pixel glues-types)))
    (list :cost (plist-get result :badness)
          :gaps (plist-get result :gaps))))

(defun ekp-hyphenate-p (glues-types n)
  "Return non-nil if position N ends with hyphenation."
  (and (< n (length glues-types))
       (eq 'nws (aref glues-types n))))

;;; Dynamic Programming Line Breaking Algorithm
;; Implements optimal line breaking using Knuth-Plass algorithm.
;;
;; Key data structures:
;;   - demerits[i]: minimum demerits to reach position i
;;   - backptrs[i]: previous break point for optimal path
;;   - fitness[i]: fitness class at break i (for adjacent penalty)
;;   - rests[i]: adjustment pixels at break i
;;   - gaps[i]: gap counts by type
;;   - hyphen-counts[i]: consecutive hyphen count

(defun ekp--dp-init-arrays (n)
  "Initialize DP arrays for N boxes.
Returns (backptrs demerits rests gaps hyphen-counts fitness-classes line-counts)."
  (let ((backptrs (make-vector (1+ n) nil))
        (demerits (make-vector (1+ n) nil))
        (rests (make-vector (1+ n) nil))
        (gaps (make-vector (1+ n) nil))
        (hyphen-counts (make-vector (1+ n) 0))
        (fitness-classes (make-vector (1+ n) 1))  ; default: decent
        (line-counts (make-vector (1+ n) 0)))     ; for looseness
    (aset demerits 0 0.0)
    (list backptrs demerits rests gaps hyphen-counts fitness-classes line-counts)))

(defun ekp--dp-line-metrics (i k glues-types ideal-prefixs min-prefixs max-prefixs)
  "Compute line metrics for boxes I to K.
Returns (ideal-pixel min-pixel max-pixel) excluding leading glue."
  (let ((leading-glue-type (aref glues-types i)))
    (list (- (aref ideal-prefixs k) (aref ideal-prefixs i)
             (ekp-glue-ideal-pixel leading-glue-type))
          (- (aref min-prefixs k) (aref min-prefixs i)
             (ekp-glue-min-pixel leading-glue-type))
          (- (aref max-prefixs k) (aref max-prefixs i)
             (ekp-glue-max-pixel leading-glue-type)))))

(defun ekp--dp-force-break (i k arrays glues-types ideal-prefixs hyphen-pixel line-pixel)
  "Force a break at K-1 when no valid break found. Update ARRAYS."
  (let* ((backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (break-pos (1- k))
         (hyphenate-p (ekp-hyphenate-p glues-types break-pos))
         (ideal-pixel (- (aref ideal-prefixs break-pos)
                         (aref ideal-prefixs i)
                         (ekp-glue-ideal-pixel (aref glues-types i))))
         (rest-pixel (- line-pixel ideal-pixel)))
    (when hyphenate-p (cl-incf ideal-pixel hyphen-pixel))
    ;; Force break with high demerits
    (aset demerits break-pos (+ 10000 (expt rest-pixel 2)))
    (aset rests break-pos rest-pixel)
    (aset backptrs break-pos i)
    (aset fitness-classes break-pos 3)  ; very loose
    (aset line-counts break-pos (1+ (aref line-counts i)))
    (aset gaps break-pos
          (ekp--gaps-list (seq-drop (cl-subseq glues-types i break-pos) 1)))))

(defun ekp--dp-compute-line-demerits (j is-last end-with-hyphenp
                                        ideal-pixel line-pixel
                                        glues-types i k
                                        prev-hyphen-count prev-fitness)
  "Compute line demerits using full K-P formula.
Returns (demerits gaps fitness new-hyphen-count)."
  (cond
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
                        (* 50 (- 1.0 fill-ratio))
                      0))
           (dem (expt (+ ekp-line-penalty badness) 2)))
      (list dem nil 1 0)))
   ;; Normal line
   (t
    (let* ((result (ekp--line-badness-and-fitness ideal-pixel line-pixel
                                                  (seq-subseq glues-types i k)))
           (badness (plist-get result :badness))
           (fitness (plist-get result :fitness))
           (line-gaps (plist-get result :gaps))
           (penalty (if end-with-hyphenp ekp-hyphen-penalty 0))
           (new-hyphen (if end-with-hyphenp (1+ prev-hyphen-count) 0))
           (dem (ekp--compute-demerits badness penalty prev-fitness fitness
                                       end-with-hyphenp prev-hyphen-count)))
      (list dem line-gaps fitness new-hyphen)))))

;; Unused but kept for reference
(defun ekp--dp-update-best (k arrays line-demerits line-gaps fitness
                              ideal-pixel line-pixel base-demerits new-hyphen line-num)
  "Update ARRAYS at position K if this break is better."
  (let* ((backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (hyphen-counts (nth 4 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (total-demerits (+ base-demerits line-demerits)))
    (when (or (null (aref demerits k))
              (< total-demerits (aref demerits k)))
      (aset rests k (- line-pixel ideal-pixel))
      (aset gaps k line-gaps)
      (aset demerits k total-demerits)
      (aset backptrs k (aref backptrs k))  ; will be set by caller
      (aset fitness-classes k fitness)
      (aset hyphen-counts k new-hyphen)
      (aset line-counts k line-num)
      t)))

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

(defun ekp--dp-trace-breaks-with-looseness (backptrs line-counts n target-lines)
  "Trace breaks, preferring paths with TARGET-LINES line count.
Used for looseness parameter support."
  (if (= ekp-looseness 0)
      (ekp--dp-trace-breaks backptrs n)
    ;; Find path closest to target line count
    (let ((optimal-lines (aref line-counts n))
          (target (+ optimal-lines ekp-looseness)))
      ;; For now, just use optimal path
      ;; Full looseness would require tracking multiple paths
      (ekp--dp-trace-breaks backptrs n))))

(defun ekp--dp-store-cache (string line-pixel dp-cache)
  "Store DP-CACHE for STRING at LINE-PIXEL."
  (if-let ((dp-record (cdr (ekp-param-cache string))))
      (puthash line-pixel dp-cache dp-record)
    (let ((dp-record (make-hash-table :test 'equal :size 100
                                      :rehash-size 1.5 :weakness nil)))
      (puthash line-pixel dp-cache dp-record)
      (puthash (ekp-param-fmtstr)
               (cons (ekp-param-data string) dp-record)
               (cdr (ekp-text-cache string))))))

(defun ekp-dp-cache (string line-pixel)
  "Compute optimal line breaks for STRING at LINE-PIXEL width.
Uses Knuth-Plass dynamic programming with demerits."
  (if-let* ((dp-record (cdr (ekp-param-cache string)))
            (cached (gethash line-pixel dp-record)))
      cached
    ;; Gather input data
    (let* ((glues-types (ekp-glues-types string))
           (boxes (ekp-boxes string))
           (hyphen-pixel (ekp-hyphen-pixel string))
           (n (length boxes))
           (ideal-prefixs (ekp-ideal-prefixs string))
           (min-prefixs (ekp-min-prefixs string))
           (max-prefixs (ekp-max-prefixs string))
           (arrays (ekp--dp-init-arrays n))
           (backptrs (nth 0 arrays))
           (demerits (nth 1 arrays))
           (rests (nth 2 arrays))
           (gaps (nth 3 arrays))
           (hyphen-counts (nth 4 arrays))
           (fitness-classes (nth 5 arrays))
           (line-counts (nth 6 arrays)))
      ;; Main DP loop: for each reachable position i
      (dotimes (i (1+ n))
        (when (aref demerits i)
          (let ((prev-hyphen-count (aref hyphen-counts i))
                (prev-fitness (aref fitness-classes i))
                (prev-line-count (aref line-counts i)))
            (catch 'break
              ;; Try extending line to each position k > i
              (dotimes (j (- n i))
                (let* ((k (+ i j 1))
                       (is-last (= k n))
                       (end-with-hyphenp (ekp-hyphenate-p glues-types k))
                       (metrics (ekp--dp-line-metrics
                                 i k glues-types ideal-prefixs min-prefixs max-prefixs))
                       (ideal-pixel (nth 0 metrics))
                       (min-pixel (nth 1 metrics))
                       (max-pixel (nth 2 metrics)))
                  ;; Add hyphen width if line ends with hyphen
                  (when end-with-hyphenp
                    (cl-incf ideal-pixel hyphen-pixel)
                    (cl-incf max-pixel hyphen-pixel)
                    (cl-incf min-pixel hyphen-pixel))
                  ;; Check if line is too long
                  (when (or (> min-pixel line-pixel)
                            (and is-last (> ideal-pixel line-pixel)))
                    (when (null (aref demerits (1- k)))
                      (ekp--dp-force-break i k arrays glues-types
                                           ideal-prefixs hyphen-pixel line-pixel))
                    (throw 'break nil))
                  ;; Valid break point: compute demerits
                  (when (or (<= min-pixel line-pixel max-pixel)
                            (and is-last (<= ideal-pixel line-pixel)))
                    (pcase-let ((`(,dem ,line-gaps ,fitness ,new-hyphen)
                                 (ekp--dp-compute-line-demerits
                                  j is-last end-with-hyphenp
                                  ideal-pixel line-pixel glues-types i k
                                  prev-hyphen-count prev-fitness)))
                      (let ((total-dem (+ (aref demerits i) dem)))
                        (when (or (null (aref demerits k))
                                  (< total-dem (aref demerits k)))
                          (aset rests k (- line-pixel ideal-pixel))
                          (aset gaps k line-gaps)
                          (aset demerits k total-dem)
                          (aset backptrs k i)
                          (aset fitness-classes k fitness)
                          (aset hyphen-counts k new-hyphen)
                          (aset line-counts k (1+ prev-line-count))))))))))))
      ;; Extract optimal solution
      (let* ((breaks (ekp--dp-trace-breaks-with-looseness
                      backptrs line-counts n (aref line-counts n)))
             (lines-rests (mapcar (lambda (i) (aref rests i)) breaks))
             (lines-gaps (mapcar (lambda (i) (aref gaps i)) breaks))
             (dp-cache (list :rests lines-rests
                             :gaps lines-gaps
                             :breaks breaks
                             :cost (aref demerits n)
                             :line-count (aref line-counts n))))
        (ekp--dp-store-cache string line-pixel dp-cache)
        dp-cache))))

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

(defun ekp--distribute-gap-adjustment (rest-pixel gaps-list stretch-p)
  "Distribute REST-PIXEL across GAPS-LIST.
STRETCH-P indicates stretch (t) or shrink (nil) mode.
Returns ((latin-adj . latin-extra) (mix-adj . mix-extra) (cjk-adj . cjk-extra))."
  (let* ((latin-gaps (nth 0 gaps-list))
         (mix-gaps (nth 1 gaps-list))
         (cjk-gaps (nth 2 gaps-list))
         (remaining rest-pixel)
         ;; Per-gap adjustment values
         (latin-change (if stretch-p ekp-lws-stretch-pixel ekp-lws-shrink-pixel))
         (mix-change (if stretch-p ekp-mws-stretch-pixel ekp-mws-shrink-pixel))
         (cjk-change (if stretch-p ekp-cws-stretch-pixel 0))
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

(defun ekp--compute-glue-pixels (glues-types gaps-distribution stretch-p)
  "Compute actual glue pixels from GLUES-TYPES and GAPS-DISTRIBUTION.
Returns list of pixel values for each glue."
  (let ((latin-adj (car (nth 0 gaps-distribution)))
        (latin-extra (cdr (nth 0 gaps-distribution)))
        (mix-adj (car (nth 1 gaps-distribution)))
        (mix-extra (cdr (nth 1 gaps-distribution)))
        (cjk-adj (car (nth 2 gaps-distribution)))
        (cjk-extra (cdr (nth 2 gaps-distribution)))
        (latin-idx -1) (mix-idx -1) (cjk-idx -1))
    (mapcar
     (lambda (type)
       (let* ((base (ekp-glue-ideal-pixel type))
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

(defun ekp--line-glue-last-line (glues-types ideal-pixel line-pixel)
  "Compute glues for last line (ragged right)."
  (append '(0)
          (mapcar #'ekp-glue-ideal-pixel glues-types)
          (list (- line-pixel ideal-pixel))))

(defun ekp--line-glue-normal (glues-types rest-pixel gaps-list)
  "Compute glues for a normal (justified) line."
  (if (= rest-pixel 0)
      (append '(0) (mapcar #'ekp-glue-ideal-pixel glues-types) '(0))
    (let* ((stretch-p (> rest-pixel 0))
           (distribution (ekp--distribute-gap-adjustment
                          (abs rest-pixel) gaps-list stretch-p))
           (glue-pixels (ekp--compute-glue-pixels glues-types distribution stretch-p)))
      (append '(0) glue-pixels '(0)))))

(defun ekp-line-glues (string line-pixel)
  "Compute glue pixels for each line after breaking STRING at LINE-PIXEL.
Returns vector of vectors, each inner vector is glue pixels for one line.
Each line's glues: [0 glue1 glue2 ... trailing-space]."
  (let* ((boxes-widths (ekp-boxes-widths string))
         (boxes-num (length (ekp-boxes string)))
         (glues-types (ekp-glues-types string))
         (ideal-prefixs (ekp-ideal-prefixs string))
         (max-prefixs (ekp-max-prefixs string))
         (breaks (ekp-line-breaks string line-pixel))
         (lines-rests (ekp-dp-data string line-pixel :rests))
         (lines-gaps (ekp-dp-data string line-pixel :gaps))
         (hyphen-pixel (ekp-hyphen-pixel string))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-types (seq-drop (cl-subseq glues-types start end) 1))
             (is-last (>= end boxes-num))
             (hyphen-p (ekp-hyphenate-p glues-types end))
             (ideal-pixel (- (aref ideal-prefixs end)
                             (aref ideal-prefixs start)
                             (ekp-glue-ideal-pixel (aref glues-types start))))
             (max-pixel (+ (- (aref max-prefixs end)
                              (aref max-prefixs start)
                              (ekp-glue-max-pixel (aref glues-types start)))
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
                (ekp--line-glue-last-line line-glues-types ideal-pixel line-pixel))
               ;; Forced break (line too short even at max stretch)
               ((< max-pixel line-pixel)
                (append '(0)
                        (mapcar #'ekp-glue-max-pixel line-glues-types)
                        (list (- line-pixel max-pixel))))
               ;; Normal justified line
               (t
                (ekp--line-glue-normal line-glues-types
                                       (nth i lines-rests)
                                       (nth i lines-gaps)))))
        (aset line-glues i (vconcat glue-list))
        (setq start end)))
    line-glues))

(defun ekp-combine-glues-and-boxes (glues boxes)
  (let* ((glues (append glues nil))
         (last-glue (car (last glues)))
         (glues (-drop-last 1 glues))
         (boxes (append boxes nil)))
    (if (= (length glues) (length boxes))
        (string-join (append (-interleave glues boxes)
                             (list last-glue)))
      (error "(length glues) + 1 != (length boxes)"))))

(defun ekp--pixel-justify (string line-pixel)
  "Justify single STRING to LINE-PIXEL."
  (let* ((boxes (ekp-boxes string))
         (hyphen (ekp-hyphen-str string))
         (breaks (ekp-line-breaks string line-pixel))
         (num (length breaks))
         (lines-glues (ekp-line-glues string line-pixel))
         (glues-types (ekp-glues-types string))
         (start 0) strings)
    (dotimes (i num)
      (let* ((end (nth i breaks))
             (line-boxes (cl-subseq boxes start end))
             (line-glues (mapcar #'ekp-pixel-spacing
                                 (aref lines-glues i))))
        ;; not last line and glue is 'nws, should add hyphen
        (when (ekp-hyphenate-p glues-types end)
          (setf (aref line-boxes (- end start 1))
                (concat (aref line-boxes (- end start 1)) hyphen)))
        (push (ekp-combine-glues-and-boxes line-glues line-boxes)
              strings)
        (setq start end)))
    (mapconcat 'identity (nreverse strings) "\n")))

(defun ekp-pixel-justify (string line-pixel &optional use-cache)
  "Justify multiline STRING to LINE-PIXEL.
When USE-CACHE is non-nil, use the cache for performance.
Default is nil, meaning cache is not used."
  (let ((ekp-caches (if use-cache
                        ekp-caches
                      (make-hash-table
                       :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))
        (strs (split-string string "\n")))
    (mapconcat (lambda (str)
                 (if (string-blank-p str)
                     ""
                   (ekp--pixel-justify str line-pixel)))
               strs "\n")))

;;; Optimal Width Search
;; Uses ternary search instead of linear scan.
;; Cost function is roughly unimodal: too narrow = many breaks = high cost,
;; too wide = overstretched lines = high cost.

(defun ekp--compute-avg-cost (strings pixel)
  "Compute average cost for STRINGS at PIXEL width."
  (let ((costs (mapcar (lambda (s)
                         (if (string-blank-p s) 0
                           (abs (ekp-total-cost s pixel))))
                       strings)))
    (/ (float (apply #'+ costs)) (max 1 (length costs)))))

(defun ekp--ternary-search-optimal-width (strings min-pixel max-pixel)
  "Find optimal width in [MIN-PIXEL, MAX-PIXEL] using ternary search.
Returns the pixel width with minimum average cost."
  (let ((lo min-pixel)
        (hi max-pixel))
    ;; Ternary search: O(log n) instead of O(n)
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

(defun ekp-pixel-range-justify (string min-pixel max-pixel &optional use-cache)
  "Find optimal width for STRING between MIN-PIXEL and MAX-PIXEL.
Returns (justified-text . optimal-pixel).
Uses ternary search for O(log n) complexity instead of O(n)."
  (let* ((ekp-caches (if use-cache
                         ekp-caches
                       (make-hash-table
                        :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))
         (strings (split-string string "\n"))
         (best-pixel (ekp--ternary-search-optimal-width strings min-pixel max-pixel)))
    (cons (ekp-pixel-justify string best-pixel use-cache) best-pixel)))

(provide 'ekp)
