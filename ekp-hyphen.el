;;; ekp-hyphen.el --- Liang hyphenation algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: emacs-kp contributors
;; Keywords: text, hyphenation, typesetting
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Implementation of Frank Liang's hyphenation algorithm.
;; See: Liang, F.M. "Word Hy-phen-a-tion by Com-put-er" (1983)
;;
;; Usage:
;;   (ekp-hyphen-load-languages "/path/to/dictionaries")
;;   (setq h (ekp-hyphen-create "en_US"))
;;   (ekp-hyphen-inserted h "hyphenation")  ; => "hy-phen-ation"
;;   (ekp-hyphen-boxes h "hyphenation")     ; => ("hy" "phen" "ation")

;;; Code:

(require 'cl-lib)

;;; Data Structure
;; Single struct holds everything: patterns, cache, and margin constraints.

(cl-defstruct (ekp-hyphen (:constructor ekp-hyphen--create))
  "Hyphenator object.
PATTERNS: hash-table, pattern-string -> (offset . priority-values).
CACHE: hash-table, word -> list of break positions.
MAXLEN: longest pattern length (bounds substring search).
LEFT/RIGHT: minimum chars before first / after last break."
  patterns cache maxlen left right)

;;; Global State

(defvar ekp-hyphen--cache (make-hash-table :test 'equal)
  "Cache: dictionary path -> compiled ekp-hyphen.")

(defvar ekp-hyphen--langs (make-hash-table :test 'equal)
  "Registry: language code -> dictionary file path.")

(defvar ekp-hyphen--langs-short (make-hash-table :test 'equal)
  "Fallback: short code (e.g., 'en') -> first matching dict path.")

;;; Dictionary Loading

(defun ekp-hyphen-load-languages (dir)
  "Scan DIR for .dic files, populate language registry."
  (dolist (file (directory-files dir t "\\.dic\\'"))
    (let* ((name (file-name-nondirectory file))
           (lang (replace-regexp-in-string "\\(^hyph_\\|\\.dic$\\)" "" name))
           (short (car (split-string lang "_"))))
      (puthash lang file ekp-hyphen--langs)
      (unless (gethash short ekp-hyphen--langs-short)
        (puthash short file ekp-hyphen--langs-short)))))

(defun ekp-hyphen--resolve-lang (lang)
  "Resolve LANG to dictionary path, trying exact then short forms."
  (or (gethash lang ekp-hyphen--langs)
      (let* ((norm (downcase (replace-regexp-in-string "-" "_" lang)))
             (parts (split-string norm "_"))
             found)
        (while (and parts (not found))
          (setq found (gethash (string-join parts "_")
                               ekp-hyphen--langs-short)
                parts (butlast parts)))
        found)))

;;; Pattern Compilation

(defun ekp-hyphen--parse-pattern (pat)
  "Parse PAT like 'hy3ph' into (letters offset . values).
Values array has length = letters + 1 (position after last letter).
E.g., 'a1bc2' -> letters='abc', values=(0 1 0 2)."
  (let ((pos 0) (len (length pat)) letters values)
    (while (< pos len)
      ;; Read optional digit (priority before next letter or at end)
      (let ((digit 0))
        (when (and (< pos len)
                   (>= (aref pat pos) ?0)
                   (<= (aref pat pos) ?9))
          (setq digit (- (aref pat pos) ?0))
          (cl-incf pos))
        (push digit values)
        ;; Read letter if present
        (when (and (< pos len)
                   (or (< (aref pat pos) ?0) (> (aref pat pos) ?9)))
          (push (aref pat pos) letters)
          (cl-incf pos))))
    (setq letters (apply #'string (nreverse letters))
          values (vconcat (nreverse values)))
    ;; Trim leading/trailing zeros
    (let ((start 0) (end (length values)))
      (while (and (< start end) (= (aref values start) 0)) (cl-incf start))
      (while (and (> end start) (= (aref values (1- end)) 0)) (cl-decf end))
      (when (> end start)
        (list letters start (cl-subseq values start end))))))

(defun ekp-hyphen--compile (path)
  "Compile dictionary at PATH into ekp-hyphen struct."
  (let ((patterns (make-hash-table :test 'equal))
        (maxlen 0))
    (with-temp-buffer
      (insert-file-contents path)
      (forward-line 1)  ; skip encoding line
      (while (not (eobp))
        (let* ((line (string-trim (buffer-substring-no-properties
                                   (point) (line-end-position))))
               (skip (or (string-empty-p line)
                         (string-match-p "^[%#]\\|HYPHENMIN" line)
                         (string-match-p "/" line))))  ; skip alt patterns
          (unless skip
            ;; Handle ^^XX hex escapes
            (setq line (replace-regexp-in-string
                        "\\^\\^\\([0-9a-fA-F]\\{2\\}\\)"
                        (lambda (m) (string (string-to-number
                                             (match-string 1 m) 16)))
                        line))
            (when-let ((parsed (ekp-hyphen--parse-pattern line)))
              (puthash (car parsed) (cdr parsed) patterns)
              (setq maxlen (max maxlen (length (car parsed)))))))
        (forward-line 1)))
    (ekp-hyphen--create :patterns patterns
                        :cache (make-hash-table :test 'equal)
                        :maxlen maxlen
                        :left 2 :right 2)))

;;; Hyphenation Algorithm

(defun ekp-hyphen--compute (h word)
  "Compute break positions for WORD using hyphenator H."
  (let* ((padded (concat "." (downcase word) "."))
         (len (length padded))
         (maxlen (ekp-hyphen-maxlen h))
         (patterns (ekp-hyphen-patterns h))
         (prio (make-vector (1+ len) 0)))
    ;; Apply matching patterns
    (dotimes (i (1- len))
      (cl-loop for j from (1+ i) to (min (+ i maxlen) len)
               for pat = (gethash (substring padded i j) patterns)
               when pat do
               (let ((off (car pat)) (vals (cadr pat)))
                 (dotimes (k (length vals))
                   (let ((pos (+ i off k)))
                     (when (< pos (length prio))
                       (aset prio pos (max (aref prio pos)
                                           (aref vals k)))))))))
    ;; Collect odd positions (subtract 1 for padding offset)
    (let (result)
      (dotimes (i (length prio))
        (when (cl-oddp (aref prio i))
          (push (1- i) result)))
      (nreverse result))))

(defun ekp-hyphen--positions (h word)
  "Get cached break positions for WORD."
  (let* ((key (downcase word))
         (cache (ekp-hyphen-cache h)))
    (or (gethash key cache)
        (puthash key (ekp-hyphen--compute h word) cache))))

;;; Public API

(defun ekp-hyphen-create (&optional lang file left right)
  "Create hyphenator for LANG or dictionary FILE.
LEFT/RIGHT: min chars before/after breaks (default 2)."
  (let ((path (or (and lang (ekp-hyphen--resolve-lang lang)) file)))
    (unless path (error "No dictionary for: %s" lang))
    (let ((h (or (gethash path ekp-hyphen--cache)
                 (puthash path (ekp-hyphen--compile path)
                          ekp-hyphen--cache))))
      (if (or left right)
          (ekp-hyphen--create :patterns (ekp-hyphen-patterns h)
                              :cache (ekp-hyphen-cache h)
                              :maxlen (ekp-hyphen-maxlen h)
                              :left (or left 2) :right (or right 2))
        h))))

(defun ekp-hyphen-positions (h word)
  "Return valid break positions in WORD, respecting margins."
  (let ((left (ekp-hyphen-left h))
        (right (- (length word) (ekp-hyphen-right h))))
    (cl-remove-if-not (lambda (p) (and (>= p left) (<= p right)))
                      (ekp-hyphen--positions h word))))

(defun ekp-hyphen-inserted (h word &optional hyphen)
  "Return WORD with HYPHEN inserted at break points."
  (let ((hyphen (or hyphen "-")) (result word) (off 0))
    (dolist (pos (ekp-hyphen-positions h word))
      (setq result (concat (substring result 0 (+ pos off))
                           hyphen
                           (substring result (+ pos off)))
            off (+ off (length hyphen))))
    result))

(defun ekp-hyphen-boxes (h word)
  "Split WORD into syllables at break points."
  (split-string (ekp-hyphen-inserted h word " ") " "))

(provide 'ekp-hyphen)

;;; ekp-hyphen.el ends here
