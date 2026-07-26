;;; ekp-demo.el --- Interactive demos for EKP -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive, GUI-only demonstrations.  Evaluate the file in a
;; graphical Emacs session, then run the forms in the comments.
;; Automated tests live in ekp-tests.el.

;;; Code:

(require 'ekp)

(defun ekp-demo--file-content (file)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name file (expand-file-name "tests" (ekp-root-dir))))
    (buffer-substring (point-min) (point-max))))

(defun ekp-demo-propertize (string properties &optional start end)
  "Add PROPERTIES to a copy of STRING without clobbering existing ones."
  (let* ((string (copy-sequence string))
         (start (or start 0))
         (end (or end (length string))))
    (while properties
      (let ((prop (pop properties))
            (value (pop properties)))
        (pcase prop
          ('face (add-face-text-property start end value t string))
          ('display (add-display-text-property
                     start end (car value) (cadr value) string))
          (_ (put-text-property start end prop value string)))))
    string))

(defun ekp-demo--pop-buffer (height &rest strings)
  (declare (indent defun))
  (let ((buffer (pop-to-buffer "*ekp-demo*"
                               `(display-buffer-at-bottom
                                 (window-height . ,(or height 10))))))
    (with-current-buffer buffer
      (local-set-key "q" 'quit-window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'insert strings)))
    buffer))

(defun ekp-demo-str (cjk latin &optional font)
  "Load a test text (see tests/text-*.txt) with optional FONT face."
  (let ((file (concat "text"
                      (and cjk (concat "-" cjk))
                      (and latin (concat "-" latin))
                      ".txt")))
    (if font
        (ekp-demo-propertize (ekp-demo--file-content file)
                             `(face (:family ,font)))
      (ekp-demo--file-content file))))

(defun ekp-demo-justify (cjk latin font pixel)
  "Justify a sample text at PIXEL width and show it."
  (setq ekp-latin-lang (or latin "en_US"))
  (ekp-demo--pop-buffer 35
    (ekp-pixel-justify (ekp-demo-str cjk latin font) pixel)))

(defun ekp-demo-range-justify (cjk latin font min max)
  "Find the optimal width in [MIN, MAX] and show the result."
  (setq ekp-latin-lang (or latin "en_US"))
  (ekp-demo--pop-buffer 35
    (car (ekp-pixel-range-justify (ekp-demo-str cjk latin font) min max))))

;; (ekp-demo-justify nil "en_US" "Times New Roman" 699)
;; (ekp-demo-justify "zh" "en_US" "Cascadia Next SC" 666)
;; (ekp-demo-justify nil "de_DE" "Georgia" 666)
;; (ekp-demo-range-justify "zh" "en_US" nil 666 690)

(defun ekp-demo-animate (min-pixel max-pixel &optional inc)
  "Animate justification from MIN-PIXEL to MAX-PIXEL."
  (let ((str (ekp-demo-str "zh" "en_US"))
        (pixel-lst (number-sequence min-pixel max-pixel (or inc 1)))
        (buf (get-buffer-create "*ekp-demo-animate*")))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer buf)
      (with-current-buffer buf
        (buffer-disable-undo)
        (dolist (pixel pixel-lst)
          (erase-buffer)
          (insert (ekp-pixel-justify str pixel))
          (goto-char (point-min))
          (sit-for 0.00001))))))

;; (ekp-demo-animate 400 800 1)

(defun ekp-demo-mixed-faces ()
  "Show that per-paragraph fonts and faces survive justification."
  (let* ((str (ekp-demo-str "zh" "en_US"))
         (lst (split-string str "\n" t)))
    (setq lst (list
               (ekp-demo-propertize
                (ekp-demo-propertize (nth 0 lst)
                                     '(face (:family "Comic Sans MS")))
                '(face (:height 1.3 :foreground "cyan")) 0 2)
               (ekp-demo-propertize
                (ekp-demo-propertize (nth 1 lst)
                                     '(face (:family "Cascadia Next SC")))
                '(face (:height 1.3 :foreground "green")) 0 2)))
    (ekp-clear-caches)
    (ekp-demo--pop-buffer 30
      "\n" (ekp-pixel-justify (string-join lst "\n\n") 683))))

;; (ekp-demo-mixed-faces)

(provide 'ekp-demo)

;;; ekp-demo.el ends here
