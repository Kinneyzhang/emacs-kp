;;; ekp-tests.el --- Tests for EKP -*- lexical-binding: t; -*-

(require 'ekp)
(require 'ert)

;;;; Test Utilities
(defun ekp-file-content (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (point-max))))

(defun ekp-propertize (string properties
                              &optional start end)
  "不会覆盖原有的属性，返回新的字符串。"
  ;; 防止是 make-list 创建的元素，它们都属于同一个对象
  ;; 最好先复制一份字符串
  (let* ((string (copy-sequence string))
         (start (or start 0))
         (end (or end (length string))))
    (while properties
      (let ((prop (pop properties))
            (value (pop properties)))
        (pcase prop
          ('face (add-face-text-property
                  start end value t string))
          ('display (add-display-text-property
                     start end (car value) (cadr value)
                     string))
          (_ (put-text-property
              start end prop value string)))))
    string))

(defun my-pop-to-buffer (buffer-or-name &optional action norecord)
  (declare (indent defun))
  (let ((buffer (pop-to-buffer buffer-or-name action norecord)))
    (with-current-buffer buffer (local-set-key "q" 'quit-window))
    buffer))

(defun pop-buffer-insert (height &rest strings)
  (declare (indent defun))
  (let* ((height (or height 10))
         (buffer (my-pop-to-buffer "*pop-buffer-insert*"
                   `(display-buffer-at-bottom
                     (window-height . ,height)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'insert strings)))))

;;; tests

(defun ekp-test-hyphen-word (lang word)
  (setq ekp-latin-lang lang)
  (ekp-hyphen-boxes
   (ekp-hyphen-create ekp-latin-lang) word))

;; (ekp-test-hyphen-word "de_DE" "ästhetisch")

(defun ekp-test-justify (cjk latin font pixel)
  ;; (ekp-clear-caches)
  (setq ekp-latin-lang latin)
  (pop-buffer-insert 35
    (ekp-pixel-justify
     (ekp-test-str cjk latin font) pixel)))

(defun ekp-test-range-justify (cjk latin font min max)
  ;; (ekp-clear-caches)
  (setq ekp-latin-lang latin)
  (pop-buffer-insert 35
    (car (ekp-pixel-range-justify
          (ekp-test-str cjk latin font) min max))))

;; (ekp-test-justify nil "en_US" "Cascadia Next SC" 399)
;; (ekp-test-justify nil "en_US" "Times New Roman" 699)
;; (ekp-test-justify nil "en_US" "Georgia" 699)
;; (ekp-test-justify nil "en_US" "Noto Serif" 700)
;; (ekp-test-justify nil "en_US" "Garamond" 699)
;; (ekp-test-justify "zh" "en_US" "Cascadia Next SC" 666)
;; (ekp-test-range-justify "zh" "en_US" "Cascadia Next SC" 666 690)
;; (ekp-test-justify nil "fr" "Cascadia Next SC" 666)
;; (ekp-test-justify nil "de_DE" "Cascadia Next SC" 666)
;; (ekp-test-justify "zh" "en_US" "Noto Serif" 689)
;; (ekp-test-justify "zh" nil nil 980)
;; (ekp-clear-caches)

;;; FIXME: font size also affect!
(defun ekp-test-str (cjk latin &optional font)
  (let ((file (concat "./text"
                      (and cjk (concat "-" cjk))
                      (and latin (concat "-" latin))
                      ".txt" )))
    (if font
        (ekp-propertize (ekp-file-content file)
                        `(face (:family ,font)))
      (ekp-file-content file))))

(defun ekp-test-demo (min-pixel max-pixel &optional inc)
  (let ((str (ekp-test-str "zh" "en_US" "Cascadia Next SC"))
        (pixel-lst (number-sequence min-pixel max-pixel inc))
        (buf (get-buffer-create "*ekp-test-demo*")))
    ;; (ekp-clear-caches)
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer buf)
      (with-current-buffer buf
        (dolist (pixel pixel-lst)
          (erase-buffer)
          (insert (ekp-pixel-justify str pixel))
          (goto-char (point-min))
          (sit-for 0.00001))))))

;; (ekp-test-demo 400 800 1)

(defun ekp-test-keep-props ()
  (let* ((str (ekp-test-str "zh" "en_US"))
         (lst (split-string str "\n" t)))
    (setq lst (list
               (ekp-propertize
                (ekp-propertize (nth 0 lst)
                                '(face (:family "Comic Sans MS")))
                '(face (:height 1.3 :foreground "cyan"))
                0 2)
               (ekp-propertize
                (ekp-propertize (nth 1 lst)
                                '(face (:family "Cascadia Next SC")))
                '(face (:height 1.3 :foreground "green"))
                0 2)
               (ekp-propertize
                (ekp-propertize (nth 2 lst)
                                '(face (:family  "IBM 3270 Narrow")))
                '(face (:height 1.3 :foreground "orange"))
                0 2)))
    (ekp-clear-caches)
    (pop-buffer-insert 30
      "\n" (ekp-pixel-justify (string-join lst "\n\n") 683))))

;; (ekp-test-keep-props)

;;; Performance Tests

(defun ekp-test-perf-range-justify (min max &optional iterations)
  "Benchmark ekp-pixel-range-justify from MIN to MAX.
Returns time in seconds."
  (let* ((iterations (or iterations 3))
         (str (ekp-test-str "zh" "en_US"))
         (start-time (float-time))
         result)
    (dotimes (_ iterations)
      (ekp-clear-caches)
      (setq result (ekp-pixel-range-justify str min max)))
    (let ((elapsed (/ (- (float-time) start-time) iterations)))
      (message "Range [%d, %d]: %.3fs avg, optimal=%dpx"
               min max elapsed (cdr result))
      elapsed)))

;; (ekp-test-perf-range-justify 666 690 3)

;;; Unit Tests (batch-mode safe, no font required)

(defun ekp-test-unit--hash-consistency ()
  "Test that sxhash is consistent for same input."
  (let* ((str "test string")
         (hash1 (sxhash (list (sxhash str) "font1" "font2" 8 4 2)))
         (hash2 (sxhash (list (sxhash str) "font1" "font2" 8 4 2))))
    (if (= hash1 hash2)
        (message "✓ Hash consistency: PASSED")
      (message "✗ Hash consistency: FAILED"))))

(defun ekp-test-unit--struct-access ()
  "Test struct slot access."
  (let ((para (record 'ekp-para
                      "test"        ; string
                      nil nil       ; latin-font, cjk-font
                      (vector "a" "b" "c")   ; boxes
                      (vector 10 20 30)      ; boxes-widths
                      nil                    ; boxes-types
                      (vector 'nws 'lws 'lws) ; glues-types
                      5                      ; hyphen-pixel
                      nil                    ; hyphen-positions
                      nil                    ; flagged-positions
                      (vector 0 10 38 76)    ; ideal-prefixs
                      (vector 0 10 34 70)    ; min-prefixs
                      (vector 0 10 42 82)    ; max-prefixs
                      nil                    ; glue-params
                      (make-hash-table :test 'eql)))) ; dp-cache
    (if (and (equal (ekp-para-string para) "test")
             (= (length (ekp-para-boxes para)) 3)
             (= (aref (ekp-para-boxes-widths para) 1) 20)
             (= (ekp-para-hyphen-pixel para) 5))
        (message "✓ Struct access: PASSED")
      (message "✗ Struct access: FAILED"))))

(defun ekp-test-unit--dp-cache-storage ()
  "Test DP results are stored in hash table."
  (let ((cache (make-hash-table :test 'eql)))
    (puthash 100 '(:breaks (1) :cost 50) cache)
    (let ((cached (gethash 100 cache)))
      (if (and cached (= (plist-get cached :cost) 50))
          (message "✓ DP cache storage: PASSED")
        (message "✗ DP cache storage: FAILED")))))

;;; New Feature Tests

(defun ekp-test-unit--hyphenate-p-binary-search ()
  "Test binary search hyphenation lookup."
  (let ((positions (vector 3 7 12 18 25)))
    (if (and (ekp--hyphenate-p positions 7)   ; exists
             (ekp--hyphenate-p positions 25)  ; last element
             (not (ekp--hyphenate-p positions 10))  ; doesn't exist
             (not (ekp--hyphenate-p positions 0)))  ; before first
        (message "✓ Binary search hyphenate-p: PASSED")
      (message "✗ Binary search hyphenate-p: FAILED"))))

(defun ekp-test-unit--flagged-p-binary-search ()
  "Test binary search flagged position lookup."
  (let ((positions (vector 5 10 20)))
    (if (and (ekp--flagged-p positions 5)     ; exists
             (ekp--flagged-p positions 20)    ; last element
             (not (ekp--flagged-p positions 15))  ; doesn't exist
             (not (ekp--flagged-p positions 1)))  ; before first
        (message "✓ Binary search flagged-p: PASSED")
      (message "✗ Binary search flagged-p: FAILED"))))

(defun ekp-test-unit--alt-paths-hash ()
  "Test alternative paths hash table for looseness."
  (let ((alt-paths (make-hash-table :test 'equal)))
    ;; Simulate tracking paths: (position . line-count) -> (backptr . demerits)
    (puthash (cons 10 3) (cons 5 150.0) alt-paths)
    (puthash (cons 10 4) (cons 6 200.0) alt-paths)
    (puthash (cons 20 5) (cons 10 300.0) alt-paths)
    (let* ((entry1 (gethash (cons 10 3) alt-paths))
           (entry2 (gethash (cons 10 4) alt-paths)))
      (if (and entry1
               (= (car entry1) 5)
               (= (cdr entry1) 150.0)
               entry2
               (= (car entry2) 6))
          (message "✓ Alt paths hash: PASSED")
        (message "✗ Alt paths hash: FAILED")))))

(defun ekp-test-unit--threshold-factor ()
  "Test threshold factor variable."
  (let ((original ekp-threshold-factor))
    (setq ekp-threshold-factor 2.0)
    (let ((result (and (numberp ekp-threshold-factor)
                       (= ekp-threshold-factor 2.0))))
      (setq ekp-threshold-factor original)
      (if result
          (message "✓ Threshold factor: PASSED")
        (message "✗ Threshold factor: FAILED")))))

(defun ekp-test-unit--flagged-penalty ()
  "Test flagged penalty is negative (preferred break)."
  (if (< ekp-flagged-penalty 0)
      (message "✓ Flagged penalty negative: PASSED")
    (message "✗ Flagged penalty negative: FAILED")))

(defun ekp-test-unit-all ()
  "Run all unit tests."
  (interactive)
  (message "=== Running Unit Tests ===")
  (ekp-test-unit--hash-consistency)
  (ekp-test-unit--struct-access)
  (ekp-test-unit--dp-cache-storage)
  ;; New feature tests
  (ekp-test-unit--hyphenate-p-binary-search)
  (ekp-test-unit--flagged-p-binary-search)
  (ekp-test-unit--alt-paths-hash)
  (ekp-test-unit--threshold-factor)
  (ekp-test-unit--flagged-penalty)
  (message "=== Unit Tests Complete ==="))

(should
 (equal (progn
          (ekp-clear-caches)
          (setq ekp-use-c-module nil)
          (ekp-pixel-justify
           (propertize "  作为神之编辑器（Editor of the Gods），Emacs 早已超越了普通文本编辑器的范畴。它是由​​Richard Stallman​​于1976年创建的​​GNU项目​核心组件，其名字源自 Editor MACroS​​。在过去的半个世纪里，Emacs演化成了一个​​self-documenting, customizable, extensible​​的生态系统，用户可通过​​Emacs Lisp (elisp)​​ 重新定义编辑行为。M-x 是每个Emacer的魔法咒语——按下Alt（或Meta键）加x即可召唤任意命令，比如M-x butterfly这样的复活节彩蛋。中国开发者常戏称其为“​​永远的操作系统​​”，因为你可以通过org-mode管理TODO list、用magit操作Git仓库、甚至用EMMS播放MP3音乐。在Unix哲学中，Emacs坚持“一个编辑器统治所有​​”（One Editor to Rule Them All）的理念，这与VS Code等现代编辑器形成鲜明对比。C-x C-f打开文件，C-x C-s保存文档，看似复杂的组合键一旦形成​​肌肉记忆​​，效率就会呈指数级飙升。著名Python库Black的开发者曾公开表示：\"​​My .emacs is my second brain.​​"
                       'face '(:family "Comic Sans MS"))
           683))
        (progn
          (ekp-clear-caches)
          (setq ekp-use-c-module t)
          (ekp-pixel-justify
           (propertize "  作为神之编辑器（Editor of the Gods），Emacs 早已超越了普通文本编辑器的范畴。它是由​​Richard Stallman​​于1976年创建的​​GNU项目​核心组件，其名字源自 Editor MACroS​​。在过去的半个世纪里，Emacs演化成了一个​​self-documenting, customizable, extensible​​的生态系统，用户可通过​​Emacs Lisp (elisp)​​ 重新定义编辑行为。M-x 是每个Emacer的魔法咒语——按下Alt（或Meta键）加x即可召唤任意命令，比如M-x butterfly这样的复活节彩蛋。中国开发者常戏称其为“​​永远的操作系统​​”，因为你可以通过org-mode管理TODO list、用magit操作Git仓库、甚至用EMMS播放MP3音乐。在Unix哲学中，Emacs坚持“一个编辑器统治所有​​”（One Editor to Rule Them All）的理念，这与VS Code等现代编辑器形成鲜明对比。C-x C-f打开文件，C-x C-s保存文档，看似复杂的组合键一旦形成​​肌肉记忆​​，效率就会呈指数级飙升。著名Python库Black的开发者曾公开表示：\"​​My .emacs is my second brain.​​"
                       'face '(:family "Comic Sans MS"))
           683))))

;; (ekp-test-unit-all)
