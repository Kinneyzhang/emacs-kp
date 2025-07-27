;; -*- lexical-binding: t; -*-

;;; utils
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

;; (ekp-test-justify "zh" "en_US" "Cascadia Next SC" 666)
;; (ekp-test-justify nil "en_US" "Cascadia Next SC" 699)
;; (ekp-test-justify nil "en_US" "Times New Roman" 699)
;; (ekp-test-justify nil "en_US" "Georgia" 699)
;; (ekp-test-justify nil "en_US" "Garamond" 699)
;; (ekp-test-range-justify "zh" "en_US" "Cascadia Next SC" 666 690)
;; (ekp-test-justify nil "fr" "Cascadia Next SC" 666)
;; (ekp-test-justify nil "de_DE" "Cascadia Next SC" 666)
;; (ekp-test-justify "zh" "en_US" "Noto Serif" 689)
;; (ekp-test-justify "zh" nil nil 250)

;;; FIXME: font size also affect!

(defun ekp-test-str (cjk latin font)
  (let ((file (concat "./text"
                      (and cjk (concat "-" cjk))
                      (and latin (concat "-" latin))
                      ".txt" )))
    (propertize (file-content file)
                'face `(:family ,font))))

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
