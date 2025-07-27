;;; ekp-hyphen.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar ekp-hyphen--hdcache (make-hash-table :test 'equal))

(defvar ekp-hyphen--languages (make-hash-table :test 'equal))

(defvar ekp-hyphen--languages-lowercase (make-hash-table :test 'equal))

(defconst ekp-hyphen--ignored
  '("%" "#" "LEFTHYPHENMIN" "RIGHTHYPHENMIN"
    "COMPOUNDLEFTHYPHENMIN" "COMPOUNDRIGHTHYPHENMIN"))

(cl-defstruct (ekp-hyphen--datint
               (:constructor ekp-hyphen--make-datint))
  value data)

(cl-defstruct (ekp-hyphen--altparser
               (:constructor ekp-hyphen--make-altparser))
  change index cut)

(cl-defstruct (ekp-hyphen (:constructor ekp-hyphen--make))
  hd left right)

(cl-defstruct (ekp-hyphen--hyphdict
               (:constructor ekp-hyphen--make-hyphdict))
  patterns cache maxlen)

(defun ekp-hyphen--parse-hex (s)
  "Replace ^^hh with the corresponding char in S."
  (replace-regexp-in-string
   "\\^\\^\\([0-9a-fA-F][0-9a-fA-F]\\)"
   (lambda (m) (string (string-to-number (match-string 1 m) 16)))
   s))

(defun ekp-hyphen--parse (pat)
  "Parse pattern string PAT to list of (digit string, non-digit string)."
  (let ((pos 0)
        (len (length pat))
        res)
    (while (< pos len)
      (let* ((digit (if (and (< pos len) (>= (aref pat pos) ?0)
                             (<= (aref pat pos) ?9))
                        (prog1 (string (aref pat pos)) (cl-incf pos))
                      ""))
             (ndigit (if (and (< pos len) (or (< (aref pat pos) ?0)
                                              (> (aref pat pos) ?9)))
                         (prog1 (string (aref pat pos)) (cl-incf pos))
                       "")))
        (push (list digit ndigit) res)))
    (nreverse res)))

(defun ekp-hyphen--language-path (language)
  "Get a fallback language available in our dictionaries for string LANGUAGE."
  (let* ((parts (split-string
                 (downcase (replace-regexp-in-string "-" "_" language)) "_"))
         (found nil))
    (or (gethash language ekp-hyphen--languages)
        (progn
          (while (and parts (not found))
            (let ((lang (mapconcat #'identity parts "_")))
              (setq found (gethash lang ekp-hyphen--languages-lowercase))
              (pop parts)))
          found))))

(defun ekp-hyphen--altparser-create (pattern alternative)
  "Create an AlternativeParser struct from PATTERN and ALTERNATIVE."
  (let* ((alt (split-string alternative ","))
         (change (nth 0 alt))
         (index (string-to-number (nth 1 alt)))
         (cut (string-to-number (nth 2 alt))))
    (if (string-prefix-p "." pattern)
        (cl-incf index))
    (ekp-hyphen--make-altparser :change change :index index :cut cut)))

(defun ekp-hyphen--altparser-call (altparser val)
  "Call ALTPARSER with value VAL."
  (let ((index (cl-decf (ekp-hyphen--altparser-index altparser)))
        (v (string-to-number val)))
    (if (cl-oddp v)
        (ekp-hyphen--make-datint
         :value v
         :data (list (ekp-hyphen--altparser-change altparser)
                     index
                     (ekp-hyphen--altparser-cut altparser)))
      v)))

(defun ekp-hyphen--read-dic-file (path)
  "Read a .dic file from PATH. Return (encoding . lines-list)."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((encoding (buffer-substring-no-properties
                     (point) (line-end-position))))
      (forward-line 1)
      (cons encoding
            (split-string (buffer-substring-no-properties
                           (point) (point-max))
                          "\n" t)))))

(defun ekp-hyphen--make-hyphdict-from-path (path)
  "Build a HyphDict structure from a .dic file at PATH."
  (let* ((file (ekp-hyphen--read-dic-file path))
         (encoding (car file))
         (lines (cdr file))
         (patterns (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let* ((p (string-trim line)))
        (unless (or (string-empty-p p)
                    (cl-some (lambda (ig) (string-prefix-p ig p))
                             ekp-hyphen--ignored))
          (setq p (ekp-hyphen--parse-hex p))
          (let* ((factory
                  (if (and (string-match "/" p) (string-match "=" p))
                      (let* ((split (split-string p "/" t))
                             (pattern (car split))
                             (alternative (cadr split)))
                        (lambda (i)
                          (ekp-hyphen--altparser-call
                           (ekp-hyphen--altparser-create pattern alternative)
                           (or i "0"))))
                    #'string-to-number))
                 (pattern (if (and (string-match "/" p) (string-match "=" p))
                              (car (split-string p "/" t))
                            p))
                 (tags-values
                  (mapcar (lambda (pr) (list (cadr pr)
                                             (funcall factory (car pr))))
                          (ekp-hyphen--parse pattern))))
            (let ((tags (mapcar #'car tags-values))
                  (values (mapcar #'cadr tags-values)))
              (unless (= (apply #'max (mapcar
                                       (lambda (v)
                                         (if (integerp v) v
                                           (ekp-hyphen--datint-value v)))
                                       values))
                         0)
                (let ((start 0) (end (length values)))
                  (while (and (< start end) (equal (elt values start) 0))
                    (cl-incf start))
                  (while (and (> end start) (equal (elt values (1- end)) 0))
                    (cl-decf end))
                  (puthash (apply #'concat tags)
                           (cons start (cl-subseq values start end))
                           patterns))))))))
    (let* ((maxlen (apply #'max (mapcar #'length
                                        (hash-table-keys patterns)))))
      (ekp-hyphen--make-hyphdict :patterns patterns
                                 :cache (make-hash-table :test 'equal)
                                 :maxlen maxlen))))

(defun ekp-hyphen--hyphdict-positions (hd word)
  "Get a list of positions where WORD can be hyphenated, using HyphDict HD.
Returns a list of ekp-hyphen--datint objects or ints."
  (let* ((w (downcase word))
         (cache (ekp-hyphen--hyphdict-cache hd))
         (points (gethash w cache)))
    (unless points
      (let* ((pointed-word (concat "." w "."))
             (references (make-list (+ (length pointed-word) 1) 0)))
        (cl-loop
         for i from 0 below (1- (length pointed-word)) do
         (let ((stop (min (+ i (ekp-hyphen--hyphdict-maxlen hd))
                          (length pointed-word))))
           (cl-loop
            for j from (1+ i) to stop do
            (let ((pattern
                   (gethash (substring pointed-word i j)
                            (ekp-hyphen--hyphdict-patterns hd))))
              (when pattern
                (let* ((offset (car pattern))
                       (vals (cdr pattern))
                       (slice-start (+ i offset))
                       (slice-end (+ i offset (length vals))))
                  (cl-loop for k from slice-start below slice-end
                           for v in vals
                           do (when (and (<= 0 k)
                                         (< k (length references)))
                                (setf (nth k references)
                                      (max v (nth k references)))))))))))
        (let ((res nil))
          (cl-loop for i from 0 below (length references)
                   for reference in references
                   when (cl-oddp (if (ekp-hyphen--datint-p reference)
                                     (ekp-hyphen--datint-value reference)
                                   reference))
                   do (push (if (ekp-hyphen--datint-p reference)
                                reference
                              (ekp-hyphen--make-datint :value (- i 1)))
                            res))
          (setq points (nreverse res))
          (puthash w points cache))))
    points))

(defun ekp-hyphen-load-languages (dict-dir)
  "Scan DICT-DIR for hyphenation dictionaries and populate
 `ekp-hyphen--languages'."
  (dolist (file (directory-files dict-dir t "\\.dic\\'"))
    (let ((name (replace-regexp-in-string "\\(^hyph_\\|\\.dic$\\)" ""
                                          (file-name-nondirectory file))))
      (puthash name file ekp-hyphen--languages)
      (let ((short (car (split-string name "_"))))
        (unless (gethash short ekp-hyphen--languages-lowercase)
          (puthash short file ekp-hyphen--languages-lowercase))))))

(defun ekp-hyphen-create (&optional lang filename left right cache)
  "Create a ekp-hyphen object. Prefer LANG, otherwise FILENAME.
LEFT and RIGHT are minimum first/last syllable chars. CACHE t/nil."
  (let* ((left (or left 2))
         (right (or right 2))
         (cache (if (null cache) t cache))
         (path (cond (lang (ekp-hyphen--language-path lang))
                     (filename filename))))
    (unless path
      (error "No dictionary found for language or filename"))
    (let ((hd (or (and cache (gethash path ekp-hyphen--hdcache))
                  (let ((dict (ekp-hyphen--make-hyphdict-from-path path)))
                    (puthash path dict ekp-hyphen--hdcache)
                    dict))))
      (ekp-hyphen--make :hd hd :left left :right right))))

(defun ekp-hyphen-positions (ekp-hyphen word)
  "Get hyphenation positions for WORD, using EKP-HYPHEN."
  (let* ((hd (ekp-hyphen-hd ekp-hyphen))
         (left (ekp-hyphen-left ekp-hyphen))
         (right (- (length word) (ekp-hyphen-right ekp-hyphen))))
    (cl-remove-if-not (lambda (i)
                        (and (<= left (ekp-hyphen--datint-value i))
                             (<= (ekp-hyphen--datint-value i) right)))
                      (ekp-hyphen--hyphdict-positions hd word))))

(defun ekp-hyphen-inserted (ekp-hyphen word &optional hyphen)
  "Return WORD with all possible hyphens inserted."
  (let ((hyphen (or hyphen "-"))
        (letters (string-to-list word)))
    (dolist (pos (reverse (ekp-hyphen-positions ekp-hyphen word)))
      (let ((idx (ekp-hyphen--datint-value pos)))
        (if (ekp-hyphen--datint-data pos)
            (let* ((data (ekp-hyphen--datint-data pos))
                   (change (nth 0 data))
                   (index (+ (nth 1 data) idx))
                   (cut (nth 2 data))
                   (changestr (replace-regexp-in-string "=" hyphen change)))
              (setq letters (append (cl-subseq letters 0 index)
                                    (string-to-list changestr)
                                    (cl-subseq letters (+ index cut)))))
          (setq letters (append (cl-subseq letters 0 idx)
                                (string-to-list hyphen)
                                (cl-subseq letters idx))))))
    (concat "" (mapconcat #'char-to-string letters ""))))

(defun ekp-hyphen-boxes (ekp-hyphen word)
  (split-string (ekp-hyphen-inserted ekp-hyphen word " ") " "))

(provide 'ekp-hyphen)

;; (defun ekp-hyphen-iterate (ekp-hyphen word)
;;   "Yield all hyphenation possibilities for WORD, longest first."
;;   (let ((positions (reverse (ekp-hyphen-positions ekp-hyphen word)))
;;         res)
;;     (dolist (pos positions)
;;       (let ((idx (ekp-hyphen--datint-value pos)))
;;         (if (ekp-hyphen--datint-data pos)
;;             (let* ((data (ekp-hyphen--datint-data pos))
;;                    (change (nth 0 data))
;;                    (index (+ (nth 1 data) idx))
;;                    (cut (nth 2 data))
;;                    (wordstr (if (string= word (upcase word))
;;                                 (upcase change)
;;                               change))
;;                    (c1 (car (split-string wordstr "=")))
;;                    (c2 (cadr (split-string wordstr "="))))
;;               (push (cons (concat (substring word 0 index) c1)
;;                           (concat c2 (substring word (+ index cut))))
;;                     res))
;;           (push (cons (substring word 0 idx)
;;                       (substring word idx))
;;                 res))))
;;     res))

;; (defun ekp-hyphen-wrap (ekp-hyphen word width &optional hyphen)
;;   "Return (first-part . last-part) for WORD, where first-part
;; is <= WIDTH with hyphen."
;;   (let ((hyphen (or hyphen "-"))
;;         (poss (ekp-hyphen-iterate ekp-hyphen word)))
;;     (setq width (- width (length hyphen)))
;;     (catch 'found
;;       (dolist (pair poss)
;;         (when (<= (length (car pair)) width)
;;           (throw 'found (cons (concat (car pair) hyphen)
;;                               (cdr pair))))))))
