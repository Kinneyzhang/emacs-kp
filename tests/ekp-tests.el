;;; ekp-tests.el --- Tests for EKP -*- lexical-binding: t; -*-

(require 'ekp)

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
                      (vector 0 10 38 76)    ; ideal-prefixs
                      (vector 0 10 34 70)    ; min-prefixs
                      (vector 0 10 42 82)    ; max-prefixs
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

(defun ekp-test-unit-all ()
  "Run all unit tests."
  (interactive)
  (message "=== Running Unit Tests ===")
  (ekp-test-unit--hash-consistency)
  (ekp-test-unit--struct-access)
  (ekp-test-unit--dp-cache-storage)
  (message "=== Unit Tests Complete ==="))

;; (ekp-test-unit-all)
