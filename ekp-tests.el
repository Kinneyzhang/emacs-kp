;; -*- lexical-binding: t; -*-
;; (ekp-clear-caches)
;; (ekp-pixel-init 6 2 1 4 2 1 0 1 0)
;; (ekp-param-fmtstr)
;; (ekp-text-cache ekp-test-str1)
;; (ekp-param-cache ekp-test-str1)
;; (ekp-dp-cache ekp-test-str1 888)
;; (ekp-dp-cache ekp-test-str1 888)
;; (ekp-total-cost ekp-test-str1 888)
;; (ekp-line-breaks ekp-test-str1 444)
;; (ekp-line-glues ekp-test-str1 888)

(progn
  (defvar ekp-string-1
    (propertize (file-content "./text1.txt")
                'face 'ekp-latin-face))
  ;; (ekp-clear-caches)
  ;; (ekp-pixel-init 6 2 1 4 2 1 0 1 0)
  (pop-buffer-do 25
    (let ((cons (ekp-pixel-range-justify ekp-string-1 100 1500)))
      (insert (car cons)))))

;; "Cascadia Next SC"
(defvar ekp-string-1
  (propertize (file-content "./text1.txt")
              'face '(:family "Cascadia Next SC")
              ;; 'face 'ekp-latin-face
              ))

(ekp-param-fmtstr)
(progn
  (setq elog-level nil)
  ;; (elog-log-clear)
  ;; (ekp-clear-caches)
  ;; (ekp-param-set 6 2 6 4 3 1 0 1 0)
  ;; (ekp-param-set-default ekp-string-1)
  (pop-buffer-do 25
    (benchmark-progn
      (let ((str (ekp-pixel-justify ekp-string-1 277)))
        (insert str)))))

(defun ekp-test-justify ()
  (interactive)
  (let ((buf (get-buffer-create "ekp-test")))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (dotimes (i 1200)
        (erase-buffer)
        (elog-log-clear)
        (insert (ekp-pixel-justify ekp-string-1 (+ i 200)))
        (sit-for 0.000001)))))
