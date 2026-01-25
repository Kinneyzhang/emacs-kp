;;; ekp-utils.el --- Utility functions for EKP -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: emacs-kp contributors

;;; Commentary:

;; Utilities for the Emacs Knuth-Plass (EKP) typesetting package.

;;; Code:

(defconst ekp-utils--load-file (or load-file-name (buffer-file-name))
  "Path to this file, for locating module directories.")

(defun ekp-root-dir ()
  "Return directory containing ekp files."
  (when ekp-utils--load-file
    (file-name-directory ekp-utils--load-file)))

;;;; Font Detection

(defsubst ekp-cjk-char-p (char)
  "Return non-nil if CHAR is a CJK character."
  (let ((entry (aref (category-table) char)))
    ;; Use ‘describe-categories’ for a full list of categories.
    ;; Another way is to use ‘char-script-table’ (see
    ;; ‘script-representative-chars’ for possible scripts), which is
    ;; not as convenient.
    (or (aref entry ?c) ; Chinese
        (aref entry ?h) ; Korean
        (aref entry ?j) ; Japanese
        )))

(defun ekp-font-family (string &optional position)
  (format "%s" (font-get (font-at (or position 0) nil string) :family)))

(defun ekp-font-monospace-p (font-family)
  (let* ((font (find-font (font-spec :family font-family)))
         (font-name (font-xlfd-name font))
         (type (nth 10 (split-string font-name "-" t))))
    ;; 'c' used in terminal
    (or (or (string= "m" type) (string= "c" type))
        (let ((info (font-info font-name)))
          (and info (> (length info) 4) 
               ;; 等宽字体的核心标志: 最大宽度等于平均宽度
               (= (aref info 7) (aref info 11)))))))

(defun ekp-get-latin-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let ((char (char-after)))
                  (not (or (and (>= char ?a) (<= char ?z))
                           (and (>= char ?A) (<= char ?Z))))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun ekp-get-cjk-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let* ((char (char-after))
                       (width (char-width char)))
                  (or (or (= 1 width) (= 0 width))
                      (not (ekp-cjk-char-p char)))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun ekp-monospace-p (string)
  "判断字符串中的拉丁字母的字体是否等宽，返回字体名称"
  (if-let* ((letter (ekp-get-latin-letter string))
            (font-family (ekp-font-family letter)))
      ;; return monospace font family
      (when (ekp-font-monospace-p font-family)
        font-family)
    ;; no latin letter in string, use default
    (face-attribute 'default :family)))

(defun ekp-word-spacing-pixel (string)
  ;; font is monospace, use the pixel of blank
  ;; as word spacing pixel
  (if-let ((font-family (ekp-monospace-p string)))
      (string-pixel-width
       (propertize " " 'face `(:family ,font-family)))
    (let* ((letter (ekp-get-latin-letter string))
           (font-family (ekp-font-family letter)))
      (string-pixel-width
       (propertize
        " " 'face `(:family ,font-family))))))

(defun ekp-latin-font (string)
  (if-let ((letter (ekp-get-latin-letter string)))
      (ekp-font-family letter)
    (face-attribute 'default :family)))

(defun ekp-cjk-font (string)
  (if-let ((letter (ekp-get-cjk-letter string)))
      (ekp-font-family letter)
    (ekp-font-family "牛")))

(defun ekp-pixel-spacing (pixel)
  "Return a pixel spacing with a PIXEL pixel width."
  (if (= pixel 0)
      ""
    (propertize " " 'display `(space :width (,pixel)))))

(defun ekp-cjk-fw-punct-p (str)
  "Return if CHAR is CJK full-width punctuation."
  (let ((char (seq-first str)))
    (or (equal (char-syntax char) ?.)
        (and (>= char #x3000) (<= char #x303F))
        (and (>= char #xFF00) (<= char #xFF60)))))

(defun ekp--flush-latin-word (word boxes)
  "Push latin WORD to BOXES if non-nil. Return updated boxes."
  (if word (cons word boxes) boxes))

(defun ekp--flush-cjk-char (char boxes)
  "Push CJK CHAR to BOXES if non-nil. Return updated boxes."
  (if char (cons char boxes) boxes))

(defun ekp--flush-spaces (spaces boxes prev-state next-width)
  "Push SPACES to BOXES based on context.
PREV-STATE: 1=latin, 2=CJK (previous content type).
NEXT-WIDTH: width of next character (1=latin, 2=CJK).
Rules:
- Latin-Latin: single space handled by glue, multiple preserves all but last
- CJK involved (prev or next is CJK): preserve ALL spaces"
  (when (and spaces (not (string-empty-p spaces)))
    (let ((cjk-involved (or (= prev-state 2) (= next-width 2))))
      (cond
       ;; CJK involved: preserve all spaces
       (cjk-involved
        (setq boxes (cons spaces boxes)))
       ;; Latin-Latin with multiple spaces: preserve all but last
       ((> (length spaces) 1)
        (setq boxes (cons (substring spaces 0 -1) boxes)))
       ;; Latin-Latin with single space: let glue handle it
       (t nil))))
  boxes)

(defun ekp--flush-trailing-spaces (spaces boxes)
  "Push all trailing SPACES to BOXES (for end of string)."
  (if (and spaces (not (string-empty-p spaces)))
      (cons spaces boxes)
    boxes))

(defun ekp--handle-latin-char (str state latin-word cjk-char boxes)
  "Handle a latin (width=1) character.
Return (new-state new-latin-word new-cjk-char new-boxes)."
  (if (= state 1)
      ;; Already in latin mode: accumulate
      (list 1 (concat latin-word str) nil boxes)
    ;; Was in CJK mode: flush CJK char, switch to latin
    (list 1 str nil (ekp--flush-cjk-char cjk-char boxes))))

(defun ekp--handle-cjk-char (str state latin-word cjk-char boxes)
  "Handle a CJK (width=2) character.
Return (new-state new-latin-word new-cjk-char new-boxes)."
  (if (= state 1)
      ;; Was in latin mode: flush latin word, push CJK directly
      (list 2 nil nil (cons str (ekp--flush-latin-word latin-word boxes)))
    ;; Already in CJK mode
    (if (ekp-cjk-fw-punct-p str)
        ;; Punctuation attaches to previous CJK char
        (list 2 nil nil (cons (concat cjk-char str) boxes))
      ;; Regular CJK char: flush previous, hold current
      (list 2 nil str (ekp--flush-cjk-char cjk-char boxes)))))

(defun ekp-split-to-boxes (string)
  "Split STRING into typographic boxes.
Latin words become single boxes; CJK chars are individual boxes.
Whitespace runs are preserved as separate boxes; CJK punctuation attaches to preceding char."
  (if (string-blank-p string)
      (vector string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((state (char-width (seq-first string)))  ; 1=latin, 2=CJK
            (prev-state 1)  ; track previous content state for space handling
            latin-word   ; accumulator for latin characters
            cjk-char     ; holds previous CJK char (for punct attachment)
            spaces       ; accumulator for whitespace runs
            boxes)       ; result list (built in reverse)
        (while (not (eobp))
          (let* ((str (buffer-substring (point) (1+ (point))))
                 (width (string-width str)))
            (cond
             ;; Whitespace or zero-width: flush content, accumulate spaces
             ((or (string-blank-p str) (= 0 width))
              (setq boxes (ekp--flush-cjk-char cjk-char boxes))
              (when cjk-char (setq prev-state 2))
              (setq cjk-char nil)
              (setq boxes (ekp--flush-latin-word latin-word boxes))
              (when latin-word (setq prev-state 1))
              (setq latin-word nil)
              (setq spaces (concat spaces str)))
             ;; Non-whitespace: flush spaces first, then handle char
             (t
              (setq boxes (ekp--flush-spaces spaces boxes prev-state width))
              (setq spaces nil)
              (cond
               ;; Latin character (width = 1)
               ((= 1 width)
                (pcase-let ((`(,s ,lw ,cc ,bx)
                             (ekp--handle-latin-char
                              str state latin-word cjk-char boxes)))
                  (setq state s latin-word lw cjk-char cc boxes bx)))
               ;; CJK character (width = 2)
               ((= 2 width)
                (pcase-let ((`(,s ,lw ,cc ,bx)
                             (ekp--handle-cjk-char
                              str state latin-word cjk-char boxes)))
                  (setq state s latin-word lw cjk-char cc boxes bx)))))))
          (forward-char 1))
        ;; Flush remaining content
        (setq boxes (ekp--flush-cjk-char cjk-char boxes))
        (setq boxes (ekp--flush-latin-word latin-word boxes))
        (setq boxes (ekp--flush-trailing-spaces spaces boxes))
        (vconcat (nreverse boxes))))))

(defun ekp-clear-caches ()
  (interactive)
  (setq ekp-caches
        (make-hash-table
         :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ekp-start-process-with-callback
    (process-name command-args callback
                  &optional output-buffer)
  "执行命令（带参数）并在完成后调用回调"
  (let* ((buffer-name (generate-new-buffer-name
                       (or output-buffer "*EKP Process Output*")))
         (process (apply #'start-process process-name
                         buffer-name command-args)))
    (set-process-sentinel
     process
     `(lambda (proc event)
        (if (string-match-p "finished" event)
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  (funcall ',callback proc (process-buffer proc))
                (when (buffer-live-p (process-buffer proc))
                  (kill-buffer (process-buffer proc)))))
          (message "%s, please check %s" (string-trim event)
                   ,buffer-name))))
    process))

(defun ekp-rust-module-reload (module)
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module))))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(defun ekp-module-dir ()
  (when-let ((root-dir (ekp-root-dir)))
    (expand-file-name "ekp_rust" root-dir)))

(defun ekp-module-file ()
  (when-let* ((module-dir (ekp-module-dir))
              (filename (cond ((eq system-type 'darwin) "libekp.dylib")
                              ((eq system-type 'windows-nt) "ekp.dll")
                              (t "libekp.so"))))
    (expand-file-name (concat "target/release/" filename) module-dir)))

(defun ekp-module-load ()
  "Load rust module of ekp."
  (if (executable-find "cargo")
      (let ((file (ekp-module-file)))
        (if file
            (ekp-rust-module-reload file)
          (ekp-module-build)))
    (error "Please install cargo and add it to executable path!")))

(defun ekp-module-build ()
  "Reload ekp rust module."
  (interactive)
  (if (executable-find "cargo")
      (ekp-start-process-with-callback
       "ekp-build"
       (cond
        ((eq system-type 'windows-nt)
         `("cmd.exe" "/c" ,(format "cd %s && cargo build -r"
                                   (ekp-module-dir))))
        (t `("zsh" "-c" ,(format "cd %s && cargo build -r"
                                 (ekp-module-dir)))))
       (lambda (proc buffer)
         (ekp-rust-module-reload (ekp-module-file))
         (message "ekp rust module reload success!")))
    (error "Please install cargo and add it to executable path!")))

;;; C Module Support
;; Parallel C implementation using pthreads

(defvar ekp-c-module-loaded nil
  "Non-nil if C module is loaded.")

(defvar ekp-c-hyphenator-index nil
  "Index of the loaded hyphenator in C module.")

(defun ekp-c-module-dir ()
  "Return the C module directory."
  (when-let ((root-dir (ekp-root-dir)))
    (expand-file-name "ekp_c" root-dir)))

(defun ekp-c-module-file ()
  "Return path to compiled C module."
  (when-let* ((module-dir (ekp-c-module-dir))
              (filename (cond ((eq system-type 'darwin) "ekp.dylib")
                              ((eq system-type 'windows-nt) "ekp.dll")
                              (t "ekp.so"))))
    (expand-file-name filename module-dir)))

(defun ekp-c-module-reload (module)
  "Load MODULE from a temp copy to allow rebuilding."
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module))))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(defun ekp-c-module-load ()
  "Load EKP C module if available."
  (interactive)
  (let ((file (ekp-c-module-file)))
    (if (and file (file-exists-p file))
        (progn
          (ekp-c-module-reload file)
          (when (fboundp 'ekp-c-init)
            (ekp-c-init)
            (setq ekp-c-module-loaded t)
            (message "ekp-c module loaded (version %s, %d threads)"
                     (ekp-c-version) (ekp-c-thread-count))))
      (message "C module not found. Run 'make' in ekp_c/ directory."))))

(defun ekp-c-load-dictionary (lang)
  "Load hyphenation dictionary for LANG into C module."
  (when ekp-c-module-loaded
    (let* ((root-dir (ekp-root-dir))
           (dict-file (expand-file-name
                       (format "dictionaries/hyph_%s.dic" lang)
                       root-dir)))
      (when (file-exists-p dict-file)
        (setq ekp-c-hyphenator-index
              (ekp-c-load-hyphenator dict-file))
        (when ekp-c-hyphenator-index
          (message "Loaded hyphenator for %s (index %d)"
                   lang ekp-c-hyphenator-index))))))

(defun ekp-c-module-build ()
  "Build the C module using make."
  (interactive)
  (let ((module-dir (ekp-c-module-dir)))
    (if (and module-dir (file-exists-p
                         (expand-file-name "Makefile" module-dir)))
        (ekp-start-process-with-callback
         "ekp-c-build"
         (cond
          ((eq system-type 'windows-nt)
           `("cmd.exe" "/c" ,(format "cd %s && make" module-dir)))
          (t `("zsh" "-c" ,(format "cd %s && make" module-dir))))
         (lambda (proc buffer)
           (ekp-c-module-load)
           (message "ekp C module build success!")))
      (error "Makefile not found in ekp_c/ directory"))))

(provide 'ekp-utils)

;;; ekp-utils.el ends here
