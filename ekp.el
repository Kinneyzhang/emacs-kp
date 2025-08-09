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
    (let* ((boxes-lst (apply #'append (nreverse new-boxes)))
           (boxes-lst
            (seq-map (lambda (box)
                       (propertize
                        box 'face `(:family ,(ekp-latin-font string))))
                     boxes-lst)))
      (cons (vconcat boxes-lst)
            (vconcat (nreverse idxs))))))

(defun ekp-str-type (str)
  "STR should be single letter string."
  (cond
   ;; a half-width cjk punct
   ((or (string= "“" str) (string= "”" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ((= (string-width str) 2)
    (if (ekp-cjk-fw-punct-p (string-to-char str))
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
        (cjk-font (ekp-cjk-font string)))
    (secure-hash
     'md5 (format "%s|%s|%s" latin-font cjk-font string))))

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
             (boxes-widths (vconcat (mapcar #'string-pixel-width boxes)))
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
  (propertize
   "-" 'face `(:family ,(ekp-text-data string :latin-font))))

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

(defun ekp--gaps-list (glues-types)
  (list (seq-count (lambda (it) (eq 'lws it)) glues-types)
        (seq-count (lambda (it) (eq 'mws it)) glues-types)
        (seq-count (lambda (it) (eq 'cws it)) glues-types)))

(defun ekp--line-cost-and-gaps (ideal-pixel line-pixel glues-types)
  "Return the cost ratio of WORDS limited to LINE-PIXEL."
  (let* ((glues-types (seq-drop glues-types 1))
         (gaps-list (ekp--gaps-list glues-types))
         (latin-gaps (nth 0 gaps-list))
         (mix-gaps (nth 1 gaps-list))
         (cjk-gaps (nth 2 gaps-list))
         (rest-pixel (- line-pixel ideal-pixel))
         ratio)
    (if (> rest-pixel 0)
        ;; should stretch
        (setq ratio
              (/ rest-pixel
                 (float (+ (* latin-gaps ekp-lws-stretch-pixel)
                           (* cjk-gaps ekp-cws-stretch-pixel)
                           (* mix-gaps ekp-mws-stretch-pixel)))))
      ;; should shrink
      (setq ratio
            (/ rest-pixel
               (float (+ (* latin-gaps ekp-lws-shrink-pixel)
                         (* mix-gaps ekp-mws-shrink-pixel))))))
    (list :cost (* 100 (expt ratio 3)) :gaps gaps-list)))

(defun ekp-hyphenate-p (glues-types n)
  (and (< n (length glues-types))
       (eq 'nws (aref glues-types n))))

(defun ekp-dp-cache (string line-pixel)
  (if-let* ((dp-record (cdr (ekp-param-cache string)))
            (dp-cache (gethash line-pixel dp-record)))
      dp-cache
    (let* ((glues-types (ekp-glues-types string))
           (boxes (ekp-boxes string))
           (hyphen-pixel (ekp-hyphen-pixel string))
           (n (length boxes))
           (ideal-prefixs (ekp-ideal-prefixs string))
           (min-prefixs (ekp-min-prefixs string))
           (max-prefixs (ekp-max-prefixs string))
           (backptrs (make-vector (1+ n) nil))
           (costs (make-vector (1+ n) nil))
           ;; rest pixel = line-pixel - ideal-pixel
           (rests (make-vector (1+ n) nil))
           (gaps (make-vector (1+ n) nil))
           ;; 连续行 hyphen 结尾计数
           (hyphen-line-count 0))
      (dotimes (i (1+ n))
        (aset costs i (if (= i 0) 0.0 nil)))
      (dotimes (i (1+ n))
        (when (aref costs i)
          (setq hyphen-line-count 0)
          (catch 'break
            (dotimes (j (- n i))
              (let* ((k (+ i j 1)) ;; k: end word index (exclusive)
                     (is-last (= k n))
                     (end-with-hyphenp (ekp-hyphenate-p glues-types k))
                     (ideal-pixel (- (aref ideal-prefixs k)
                                     (aref ideal-prefixs i)
                                     (ekp-glue-ideal-pixel
                                      (aref glues-types i))))
                     (max-pixel (- (aref max-prefixs k)
                                   (aref max-prefixs i)
                                   (ekp-glue-max-pixel
                                    (aref glues-types i))))
                     (min-pixel (- (aref min-prefixs k)
                                   (aref min-prefixs i)
                                   (ekp-glue-min-pixel
                                    (aref glues-types i)))))

                ;; ends with hyphen, plus the pixel of hyphen
                (when end-with-hyphenp
                  (cl-incf ideal-pixel hyphen-pixel)
                  (cl-incf max-pixel hyphen-pixel)
                  (cl-incf min-pixel hyphen-pixel))

                ;; back to last word
                (when (or (> min-pixel line-pixel)
                          (and is-last (> ideal-pixel line-pixel)))
                  (when (null (aref costs (1- k)))
                    ;; can not find a proper line break,
                    ;; break line at prev box
                    (let* ((hyphenate-p (ekp-hyphenate-p glues-types (1- k)))
                           (ideal-pixel (- (aref ideal-prefixs (1- k))
                                           (aref ideal-prefixs i)
                                           (ekp-glue-ideal-pixel
                                            (aref glues-types i))))
                           (rest-pixel (- line-pixel ideal-pixel)))
                      (when hyphenate-p (cl-incf ideal-pixel hyphen-pixel))
                      (aset costs (1- k) (+ 100 (expt rest-pixel 3)))
                      (aset rests (1- k) rest-pixel)
                      (aset backptrs (1- k) i)
                      (aset gaps (1- k)
                            (ekp--gaps-list
                             (seq-drop (cl-subseq glues-types i (1- k)) 1)))
                      ;; (elog-debug "1-k:%s; rests:%S" (1- k)
                      ;;             (aref rests (1- k)))
                      ))
                  (throw 'break nil))
                
                (when (or (<= min-pixel line-pixel max-pixel)
                          (and is-last (<= ideal-pixel line-pixel)))
                  (let* ((line-gaps)
                         (line-cost
                          (cond
                           ;; only has one word
                           ((= j 0)
                            (expt (- ideal-pixel line-pixel) 3))
                           (is-last 0.0)
                           ;; has more than one word
                           (t (let* ((cost-and-gaps
                                      (ekp--line-cost-and-gaps
                                       ideal-pixel line-pixel
                                       (seq-subseq glues-types i k)))
                                     (cost (plist-get cost-and-gaps :cost))
                                     (gaps (plist-get cost-and-gaps :gaps)))
                                (setq line-gaps gaps)
                                (if end-with-hyphenp
                                    (progn
                                      ;; add extra cost of hypen
                                      (cl-incf hyphen-line-count)
                                      (+ cost (* 1000 hyphen-line-count)))
                                  (setq hyphen-line-count 0)
                                  cost)))))
                         (total-cost (+ (aref costs i) line-cost)))
                    ;; (message "total cost:%S" total-cost)
                    ;; (message "hyphen-line-count:%S" hyphen-line-count)
                    (when (or (null (aref costs k))
                              (< (abs total-cost) (abs (aref costs k))))
                      ;; set all for cost is smaller!
                      (aset rests k (- line-pixel ideal-pixel))
                      (aset gaps k line-gaps)
                      (aset costs k total-cost)
                      ;; 断点设置为 当前行的起点 = 上一行的和结束点
                      (aset backptrs k i)))))))))
      (let ((breaks (list n))
            (index n))
        (while (> index 0)
          (let ((prev (aref backptrs index)))
            (if prev (progn (push prev breaks)
                            (setq index prev))
              (setq index (1- index)))))
        (let* ((breaks (cdr breaks))
               lines-rests lines-gaps dp-cache)
          (dolist (i breaks)
            (push (aref rests i) lines-rests)
            (push (aref gaps i) lines-gaps))
          ;; set dp cache
          (setq dp-cache (list :rests (nreverse lines-rests)
                               :gaps (nreverse lines-gaps)
                               :breaks breaks
                               :cost (aref costs (length boxes))))
          ;; update param cache
          (if-let ((dp-record (cdr (ekp-param-cache string))))
              (puthash line-pixel dp-cache dp-record)
            (let ((dp-record (make-hash-table
                              :test 'equal :size 100
                              :rehash-size 1.5 :weakness nil)))
              (puthash line-pixel dp-cache dp-record)
              (puthash (ekp-param-fmtstr)
                       (cons (ekp-param-data string) dp-record)
                       (cdr (ekp-text-cache string)))))
          dp-cache)))))

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

(defun ekp-line-glues (string line-pixel)
  "Line glues include glues before first box and after last box.
So the length of line glues is: line-boxes-num + 1"
  (let* ((boxes-widths (ekp-boxes-widths string))
         (boxes-num (length (ekp-boxes string)))
         (glues-types (ekp-glues-types string))
         (ideal-prefixs (ekp-ideal-prefixs string))
         (max-prefixs (ekp-max-prefixs string))
         (breaks (ekp-line-breaks string line-pixel))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-types
              ;; exclude glue before word at the start of line
              (seq-drop (cl-subseq glues-types start end) 1))
             (is-last (>= end boxes-num))
             line-glue)
        (setq line-glue
              (cond
               ((= 1 (length line-boxes-widths))
                (list 0 (- line-pixel (aref line-boxes-widths 0))))
               (is-last
                (append '(0)
                        (mapcar #'ekp-glue-ideal-pixel line-glues-types)
                        (list
                         (- line-pixel (- (aref ideal-prefixs end)
                                          (aref ideal-prefixs start)
                                          (ekp-glue-ideal-pixel
                                           (aref glues-types start)))))))
               (t
                ;; (elog-debug "-----------------")
                ;; (elog-debug "glues-types:%s" line-glues-types)
                (let ((max-pixel (- (aref max-prefixs end)
                                    (aref max-prefixs start)
                                    (ekp-glue-max-pixel
                                     (aref glues-types start)))))
                  ;; ends with hyphen
                  (when (ekp-hyphenate-p glues-types end)
                    (cl-incf max-pixel (ekp-hyphen-pixel string)))
                  ;; (elog-debug "start:%s; end:%s; max:%s" start end max-pixel)
                  (if (< max-pixel line-pixel)
                      (progn
                        ;; 行尾直接断行的情况
                        ;; (elog-debug "暴力断行 i:%s pixel:%s"
                        ;;             i (- line-pixel max-pixel))
                        (append '(0)
                                (mapcar #'ekp-glue-max-pixel line-glues-types)
                                (list (- line-pixel max-pixel))))
                    ;; 正常情况
                    (let ((ideal-pixel (- (aref ideal-prefixs end)
                                          (aref ideal-prefixs start)
                                          (ekp-glue-ideal-pixel
                                           (aref glues-types start)))))
                      ;; ideal-pixel 包含第一个box之前的glue
                      ;; (elog-debug "boxes:%S" (cl-subseq boxes start end))
                      ;; (elog-debug "line:%s; ideal:%s; rest:%s; stored-rest:%s"
                      ;;             line-pixel ideal-pixel (- line-pixel ideal-pixel)
                      ;;             (nth i (ekp-dp-data string line-pixel :rests)))
                      )
                    (let* ((lines-rests (ekp-dp-data string line-pixel :rests))
                           (curr-rest-pixel (nth i lines-rests)))
                      ;; (elog-debug "curr-rest-pixel:%s" curr-rest-pixel)
                      (cond
                       ((= curr-rest-pixel 0)
                        (append '(0) (mapcar #'ekp-glue-ideal-pixel
                                             line-glues-types)
                                '(0)))
                       (t
                        (let* ((lines-gaps (ekp-dp-data string line-pixel :gaps))
                               (line-gaps (nth i lines-gaps))
                               (latin-gaps (nth 0 line-gaps))
                               (mix-gaps (nth 1 line-gaps))
                               (cjk-gaps (nth 2 line-gaps))
                               (latin-gap-pixel 0) (latin-extra-gaps 0)
                               (mix-gap-pixel 0) (mix-extra-gaps 0)
                               (cjk-gap-pixel 0) (cjk-extra-gaps 0)
                               (line-rest-pixel (abs curr-rest-pixel)))
                          ;; max-latin-change include stretch or shrink
                          (let* ((latin-gap-change (if (> curr-rest-pixel 0)
                                                       ekp-lws-stretch-pixel
                                                     ekp-lws-shrink-pixel))
                                 (latin-max-pixel (* latin-gaps latin-gap-change)))
                            (if (< (- line-rest-pixel latin-max-pixel) 0)
                                (when (> latin-gaps 0)
                                  ;; only stretch latin glues
                                  ;; (elog-debug "latin-gap-pixel:%s"
                                  ;;             (/ line-rest-pixel latin-gaps))
                                  ;; (elog-debug "latin-extra-gaps:%s"
                                  ;;             (% line-rest-pixel latin-gaps))
                                  (setq latin-gap-pixel (/ line-rest-pixel latin-gaps))
                                  (setq latin-extra-gaps (% line-rest-pixel latin-gaps)))
                              ;; stretch latin glues max and continue to stretch mix glues
                              (setq latin-gap-pixel latin-gap-change)
                              (setq line-rest-pixel (- line-rest-pixel latin-max-pixel))
                              (let* ((mix-gap-change (if (> curr-rest-pixel 0)
                                                         ekp-mws-stretch-pixel
                                                       ekp-mws-shrink-pixel))
                                     (mix-max-pixel (* mix-gaps mix-gap-change)))
                                (if (< (- line-rest-pixel mix-max-pixel) 0)
                                    (when (> mix-gaps 0)
                                      ;; only stretch mix glues
                                      (setq mix-gap-pixel (/ line-rest-pixel mix-gaps))
                                      (setq mix-extra-gaps (% line-rest-pixel mix-gaps)))
                                  ;; stretch mix glues max and continue to stretch cjk glues
                                  (setq mix-gap-pixel mix-gap-change)
                                  (setq line-rest-pixel (- line-rest-pixel mix-max-pixel))
                                  (when (> cjk-gaps 0)
                                    (setq cjk-gap-pixel (/ line-rest-pixel cjk-gaps))
                                    (setq cjk-extra-gaps (% line-rest-pixel cjk-gaps)))))))
                          (let* ((latin-extra-index -1)
                                 (mix-extra-index -1)
                                 (cjk-extra-index -1)
                                 (glue-pixel-lst
                                  (mapcar
                                   (lambda (type)
                                     (let ((pixel (cond
                                                   ((eq type 'lws)
                                                    (cl-incf latin-extra-index)
                                                    (if (< latin-extra-index latin-extra-gaps)
                                                        (1+ latin-gap-pixel)
                                                      latin-gap-pixel))
                                                   ((eq type 'mws)
                                                    (cl-incf mix-extra-index)
                                                    (if (< mix-extra-index mix-extra-gaps)
                                                        (1+ mix-gap-pixel)
                                                      mix-gap-pixel))
                                                   ((eq type 'cws)
                                                    (cl-incf cjk-extra-index)
                                                    (if (< cjk-extra-index cjk-extra-gaps)
                                                        (1+ cjk-gap-pixel)
                                                      cjk-gap-pixel))
                                                   ((eq type 'nws) 0))))
                                       (if (> curr-rest-pixel 0)
                                           ;; stretch
                                           (+ (ekp-glue-ideal-pixel type) pixel)
                                         ;; shrink
                                         (- (ekp-glue-ideal-pixel type) pixel))))
                                   line-glues-types)))
                            ;; (elog-debug "glue-pixel-lst:%S" glue-pixel-lst)
                            ;; (elog-debug "--------------")
                            (append '(0) glue-pixel-lst '(0))))))))))))
        (aset line-glues i (vconcat line-glue nil))
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

(defun ekp-pixel-justify (string line-pixel)
  "Justify multiline STRING to LINE-PIXEL."
  (let ((strs (split-string string "\n")))
    (mapconcat (lambda (str)
                 (if (string-blank-p str)
                     ""
                   (ekp--pixel-justify str line-pixel)))
               strs "\n")))

(defun ekp-pixel-range-justify (string min-pixel max-pixel)
  "Find the optimal breakpoint for STRING typesetting between
a MIN-PIXEL and MAX-PIXEL width and return a cons-cell. The car
of it is the ​​typeset tex and cdr is the best pixel."
  (let* ((strings (split-string string "\n"))
         (best-pixel max-pixel)
         (best-cost-lst
          (mapcar (lambda (string)
                    (if (string-blank-p string)
                        0
                      (abs (ekp-total-cost string max-pixel))))
                  strings))
         ;; get average cost of all lines' costs
         (best-cost (/ (float (apply #'+ best-cost-lst))
                       (length best-cost-lst)))
         (curr-pixel max-pixel))
    (while (>= curr-pixel min-pixel)
      (let* ((cost-lst
              (mapcar (lambda (string)
                        (if (string-blank-p string)
                            0
                          (abs (ekp-total-cost string curr-pixel))))
                      strings))
             (curr-cost (/ (float (apply #'+ cost-lst))
                           (length cost-lst))))
        (when (< curr-cost best-cost)
          (progn
            (setq best-cost curr-cost)
            (setq best-pixel curr-pixel)))
        (cl-decf curr-pixel 1)))
    (cons (ekp-pixel-justify string best-pixel) best-pixel)))

(provide 'ekp)
