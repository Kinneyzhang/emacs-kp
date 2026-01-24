;;; ekp-hyphen.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)  ; for hash-table-keys

;; Cache: dictionary path -> compiled HyphDict
(defvar ekp-hyphen--hdcache (make-hash-table :test 'equal))

;; Language registry: "en_US" -> dictionary file path
(defvar ekp-hyphen--languages (make-hash-table :test 'equal))

;; Fallback registry: "en" -> dictionary file path (first match)
(defvar ekp-hyphen--languages-lowercase (make-hash-table :test 'equal))

;; Lines in .dic files starting with these are metadata, not patterns
(defconst ekp-hyphen--ignored
  '("%" "#" "LEFTHYPHENMIN" "RIGHTHYPHENMIN"
    "COMPOUNDLEFTHYPHENMIN" "COMPOUNDRIGHTHYPHENMIN"))

;; Data structures for hyphenation algorithm
;; See: Liang, F.M. "Word Hy-phen-a-tion by Com-put-er" (1983)

(cl-defstruct (ekp-hyphen--datint
               (:constructor ekp-hyphen--make-datint))
  "Integer with optional replacement data for special hyphenations."
  value  ; hyphenation priority (odd = break allowed)
  data)  ; (change index cut) for non-standard breaks like "ff" -> "f-f"

(cl-defstruct (ekp-hyphen--altparser
               (:constructor ekp-hyphen--make-altparser))
  "Parser for alternative hyphenation patterns (e.g., German ck -> k-k)."
  change  ; replacement string with "=" marking break point
  index   ; position in word
  cut)    ; characters to remove

(cl-defstruct (ekp-hyphen (:constructor ekp-hyphen--make))
  "User-facing hyphenator object."
  hd      ; compiled HyphDict
  left    ; minimum chars before first break (default 2)
  right)  ; minimum chars after last break (default 2)

(cl-defstruct (ekp-hyphen--hyphdict
               (:constructor ekp-hyphen--make-hyphdict))
  "Compiled hyphenation dictionary."
  patterns  ; hash: pattern-string -> (offset . values)
  cache     ; hash: word -> positions (memoization)
  maxlen)   ; longest pattern length (optimization)

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

(defun ekp-hyphen--hyphdict-positions (hyphdict word)
  "Find all hyphenation positions in WORD using HYPHDICT.
Returns list of ekp-hyphen--datint objects (odd value = break allowed)."
  (let* ((word-lower (downcase word))
         (cache (ekp-hyphen--hyphdict-cache hyphdict))
         (cached-result (gethash word-lower cache)))
    (or cached-result
        (let ((points (ekp-hyphen--compute-positions hyphdict word-lower)))
          (puthash word-lower points cache)
          points))))

(defun ekp-hyphen--compute-positions (hyphdict word)
  "Compute hyphenation positions for WORD (internal, no caching)."
  (let* ((pointed-word (concat "." word "."))
         (word-len (length pointed-word))
         (max-pattern-len (ekp-hyphen--hyphdict-maxlen hyphdict))
         (patterns (ekp-hyphen--hyphdict-patterns hyphdict))
         ;; Priority array: index i = position before char i
         (priorities (make-list (1+ word-len) 0)))
    ;; Scan all substrings and apply matching patterns
    (dotimes (start (1- word-len))
      (let ((end-limit (min (+ start max-pattern-len) word-len)))
        (cl-loop for end from (1+ start) to end-limit do
                 (when-let ((pattern (gethash (substring pointed-word start end)
                                              patterns)))
                   (ekp-hyphen--apply-pattern priorities pattern start)))))
    ;; Extract positions where priority is odd (= hyphenation allowed)
    (ekp-hyphen--extract-break-positions priorities)))

(defun ekp-hyphen--apply-pattern (priorities pattern start)
  "Apply PATTERN values to PRIORITIES array starting at START."
  (let ((offset (car pattern))
        (values (cdr pattern)))
    (cl-loop for idx from (+ start offset)
             for val in values
             when (and (<= 0 idx) (< idx (length priorities)))
             do (setf (nth idx priorities)
                      (max val (nth idx priorities))))))

(defun ekp-hyphen--extract-break-positions (priorities)
  "Extract break positions from PRIORITIES array.
Returns list of ekp-hyphen--datint objects for odd-valued positions."
  (let (result)
    (cl-loop for idx from 0 below (length priorities)
             for priority in priorities
             when (cl-oddp (if (ekp-hyphen--datint-p priority)
                               (ekp-hyphen--datint-value priority)
                             priority))
             do (push (if (ekp-hyphen--datint-p priority)
                          priority
                        (ekp-hyphen--make-datint :value (- idx 1)))
                      result))
    (nreverse result)))

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

;; (defun ekp-hyphen-inserted (ekp-hyphen word &optional hyphen)
;;   "Return WORD with all possible hyphens inserted."
;;   (let ((hyphen (or hyphen "-"))
;;         (letters (string-to-list word)))
;;     (dolist (pos (reverse (ekp-hyphen-positions ekp-hyphen word)))
;;       (let ((idx (ekp-hyphen--datint-value pos)))
;;         (if (ekp-hyphen--datint-data pos)
;;             (let* ((data (ekp-hyphen--datint-data pos))
;;                    (change (nth 0 data))
;;                    (index (+ (nth 1 data) idx))
;;                    (cut (nth 2 data))
;;                    (changestr (replace-regexp-in-string "=" hyphen change)))
;;               (setq letters (append (cl-subseq letters 0 index)
;;                                     (string-to-list changestr)
;;                                     (cl-subseq letters (+ index cut)))))
;;           (setq letters (append (cl-subseq letters 0 idx)
;;                                 (string-to-list hyphen)
;;                                 (cl-subseq letters idx))))))
;;     (concat "" (mapconcat #'char-to-string letters ""))))

(defun ekp-hyphen-inserted (ekp-hyphen word &optional hyphen)
  "Return WORD with all possible hyphens inserted, preserving
text properties."
  (let ((hyphen (or hyphen "-"))
        (result word))
    (dolist (pos (reverse (ekp-hyphen-positions ekp-hyphen word)))
      (let ((idx (ekp-hyphen--datint-value pos)))
        (if (ekp-hyphen--datint-data pos)
            (let* ((data (ekp-hyphen--datint-data pos))
                   (change (nth 0 data))
                   (index (+ (nth 1 data) idx))
                   (cut (nth 2 data))
                   (changestr (replace-regexp-in-string "=" hyphen change)))
              (setq result (concat (substring result 0 index)
                                   changestr
                                   (substring result (+ index cut)))))
          (setq result (concat (substring result 0 idx)
                               hyphen
                               (substring result idx))))))
    result))

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
