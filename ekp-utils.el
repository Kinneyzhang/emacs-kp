;; -*- lexical-binding: t; -*-

;;; related to font

;; (defun ekp-cjk-char-p (char)
;;   "Return if char CHAR is cjk."
;;   (or
;;    ;; CJK统一表意文字（基本区）
;;    (<= #x4E00 char #x9FFF)
;;    ;; CJK扩展A区
;;    (<= #x3400 char #x4DBF)
;;    ;; CJK扩展B区（注意：超出16位范围）
;;    (and (<= #x20000 char) (<= char #x2A6DF))
;;    ;; CJK兼容/部首扩展等
;;    ;; CJK符号和标点
;;    (<= #x3000 char #x303F)
;;    ;; 日文假名
;;    (<= #x3040 char #x30FF)
;;    ;; 韩文谚文
;;    (<= #xAC00 char #xD7AF)
;;    ;; CJK兼容表意文字
;;    (<= #xF900 char #xFAFF)))

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

(defmacro ekp-setq (sym val)
  "Set the value of symbol SYM to VAL. If VAL is nil, set to
the value of [SYM]-default."
  `(setq ,sym (or ,val ,(intern (concat (symbol-name sym)
                                        "-default")))))

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
Whitespace separates boxes; CJK punctuation attaches to preceding char."
  (if (string-blank-p string)
      (vector string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((state (char-width (seq-first string)))  ; 1=latin, 2=CJK
            latin-word   ; accumulator for latin characters
            cjk-char     ; holds previous CJK char (for punct attachment)
            boxes)       ; result list (built in reverse)
        (while (not (eobp))
          (let* ((str (buffer-substring (point) (1+ (point))))
                 (width (string-width str)))
            (cond
             ;; Whitespace or zero-width: flush latin word, start new box
             ((or (string-blank-p str) (= 0 width))
              (setq boxes (ekp--flush-latin-word latin-word boxes))
              (setq latin-word nil))
             ;; Latin character (width = 1)
             ((= 1 width)
              (pcase-let ((`(,s ,lw ,cc ,bx)
                           (ekp--handle-latin-char str state latin-word cjk-char boxes)))
                (setq state s latin-word lw cjk-char cc boxes bx)))
             ;; CJK character (width = 2)
             ((= 2 width)
              (pcase-let ((`(,s ,lw ,cc ,bx)
                           (ekp--handle-cjk-char str state latin-word cjk-char boxes)))
                (setq state s latin-word lw cjk-char cc boxes bx)))))
          (forward-char 1))
        ;; Flush remaining content
        (setq boxes (ekp--flush-cjk-char cjk-char boxes))
        (setq boxes (ekp--flush-latin-word latin-word boxes))
        (vconcat (nreverse boxes))))))

(defun ekp-clear-caches ()
  (interactive)
  (setq ekp-caches
        (make-hash-table
         :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))

;; (defun ekp-split-to-boxes (string)
;;   "Split STRING into a vector: English by word, Chinese
;; by character, punctuation attached to previous unit."
;;   (with-temp-buffer
;;     (insert string)
;;     (goto-char (point-min))
;;     (let (words (index 0))
;;       (while (not (eobp))
;;         ;; skip whitespace
;;         (skip-syntax-forward "-")
;;         (unless (eobp)
;;           (let ((start (point)))
;;             (if (ekp-cjk-char-p (char-after))
;;                 (forward-char 1)
;;               (forward-word 1)
;;               ;; process float number
;;               (when-let* ((char1 (char-after (point)))
;;                           (char2 (char-after (1+ (point))))
;;                           (_ (and (eq char1 ?.) (<= ?0 char2 ?9))))
;;                 (forward-word 1))
;;               ;; process hyphen and contraction
;;               (while (or (eq (char-after (point)) ?-)
;;                          (eq (char-after (point)) ?')
;;                          (eq (char-after (point)) ?/))
;;                 (forward-word 1)))
;;             ;; attach punctuation
;;             (while (and (not (eobp))
;;                         (with-syntax-table (standard-syntax-table)
;;                           (eq (char-syntax (char-after)) ?.)))
;;               ;; (message "(char-after):%s" (char-after))
;;               (forward-char 1))
;;             (push (buffer-substring start (point)) words))))
;;       (vconcat (nreverse words)))))

;; (defvar ekp-par-num 8)
;; (defun ekp-strings (string n)
;;   "Split STRING to average N parts but don't split in a word."
;;   ;; used for rust
;;   (let* ((size (length string))
;;          (each-size (/ size n))
;;          (str-ends (--map (if (= it n) size (* it each-size))
;;                           (number-sequence 1 n)))
;;          regions)
;;     (with-temp-buffer
;;       (insert string)
;;       (goto-char (point-min))
;;       (let ((str-start 0)
;;             (prev-end 0))
;;         (dolist (str-end str-ends)
;;           (when (> str-end prev-end)
;;             (goto-char (1+ str-end))
;;             (while (and (not (eobp))
;;                         (not (eq ?  (char-after)))
;;                         (< (char-width (char-after)) 2))
;;               (forward-char 1))
;;             (setq str-end (1- (point)))
;;             (push (cons str-start str-end) regions)
;;             (setq prev-end str-end)
;;             (setq str-start str-end)))))
;;     (vconcat (--map
;;               (substring string (car it) (cdr it))
;;               (nreverse regions)))))

(provide 'ekp-utils)
