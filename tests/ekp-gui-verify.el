;;; ekp-gui-verify.el --- GUI pixel-fit verification for ekp -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Ground-truth verification that justified text really fits the
;; window, measured with `window-text-pixel-size' on the live display
;; — the one thing batch tests cannot check.
;;
;; Two ways to run:
;;
;;   M-x ekp-gui-verify      in ANY running GUI Emacs — including your
;;                           fully customized session.  Use this when
;;                           justified text looks truncated: it tells
;;                           you whether measurement matches rendering
;;                           under your fonts/remappings.
;;
;;   emacs -Q -L . -L tests -l tests/ekp-gui-verify.el \
;;         -f ekp-gui-verify-matrix
;;                           runs the full matrix (plain, text-scale
;;                           up/down, face remap, narrow+scale) and
;;                           prints a PASS/FAIL table.
;;
;; Criterion: every justified line's rendered width equals the target
;; width (± `ekp-region-margin-pixel').  Verbatim paragraphs
;; (`ekp-verbatim') are exempt — code blocks pass through unwrapped by
;; design and may exceed a narrow window, like any code line.

;;; Code:

(require 'ekp)
(require 'ekp-region)
(require 'ekp-showcase)

(defun ekp-gui-verify--scan (buffer)
  "Measure every line of BUFFER in its window; return a result plist."
  (with-current-buffer buffer
    (let* ((win (get-buffer-window buffer))
           (body (window-body-width win t))
           (target ekp-region--auto-width)
           (worst 0) (over 0) (n 0) (exempt 0))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (px (if (= bol eol) 0
                       (car (window-text-pixel-size win bol eol t)))))
            (when (> px 0)
              (if (text-property-not-all bol eol 'ekp-verbatim nil)
                  (setq exempt (1+ exempt))
                (setq n (1+ n))
                (when (> px worst) (setq worst px))
                (when (> px body) (setq over (1+ over))))))
          (forward-line 1)))
      (list :body body :target target :widest worst :over over
            :lines n :exempt exempt
            :pass (and (= over 0)
                       (<= (abs (- worst target))
                           (max 2 ekp-region-margin-pixel)))))))

;;;###autoload
(defun ekp-gui-verify ()
  "Verify pixel-exact justification against this session's display.
Opens the ekp showcase, enables follow-window justification, and
checks with `window-text-pixel-size' that every justified line
renders at exactly the window's text width — under YOUR fonts,
themes, remappings and text-scale.  Reports PASS or FAIL."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (ekp-showcase)
  (redisplay t)
  (with-current-buffer "*ekp-showcase*"
    (ekp-auto-justify-mode 1)
    (when (timerp ekp-region--resize-timer)
      (cancel-timer ekp-region--resize-timer))
    (ekp-region--reflow (current-buffer) (ekp-region--effective-width))
    (redisplay t)
    (let* ((r (ekp-gui-verify--scan (current-buffer)))
           (msg (format
                 "ekp-gui-verify: %s — %d lines, widest %dpx vs target %dpx (window %dpx)%s"
                 (if (plist-get r :pass) "PASS" "FAIL")
                 (plist-get r :lines) (plist-get r :widest)
                 (plist-get r :target) (plist-get r :body)
                 (if (> (plist-get r :exempt) 0)
                     (format ", %d verbatim lines exempt"
                             (plist-get r :exempt))
                   ""))))
      (message "%s" msg)
      r)))

(defun ekp-gui-verify--case (name setup)
  "Run one matrix case NAME with buffer SETUP; return its result plist."
  ;; Leftover debounce timers from the previous case must not fire
  ;; into this case's fresh buffer.
  (dolist (fn (list #'ekp-region--reflow
                    #'ekp-region--flush-dirty
                    #'ekp-region--process-chunk))
    (cancel-function-timers fn))
  (when (get-buffer "*ekp-showcase*")
    (kill-buffer "*ekp-showcase*"))
  (ekp-showcase)
  (redisplay t)
  (with-current-buffer "*ekp-showcase*"
    (funcall setup)
    (redisplay t)
    (ekp-auto-justify-mode 1)
    (when (timerp ekp-region--resize-timer)
      (cancel-timer ekp-region--resize-timer))
    (ekp-region--reflow (current-buffer) (ekp-region--effective-width))
    (redisplay t)
    (let ((r (ekp-gui-verify--scan (current-buffer))))
      (prog1 (append (list :name name) r)
        (ekp-auto-justify-mode -1)))))

(defun ekp-gui-verify--format-result (result)
  "Format one matrix RESULT plist as a report line."
  (format "%-22s body=%4d target=%4d widest=%4d over=%d/%d  %s"
          (plist-get result :name)
          (plist-get result :body)
          (plist-get result :target)
          (plist-get result :widest)
          (plist-get result :over)
          (plist-get result :lines)
          (if (plist-get result :pass) "PASS" "FAIL")))

(defun ekp-gui-verify--report (results)
  "Report matrix RESULTS and return their formatted table.
In batch mode, terminate with status 1 when any result fails."
  (let ((table (mapconcat #'ekp-gui-verify--format-result results "\n"))
        (passed t))
    (dolist (result results)
      (unless (plist-get result :pass)
        (setq passed nil)))
    (if noninteractive
        (princ (concat table "\n"))
      (with-current-buffer (get-buffer-create "*ekp-gui-verify*")
        (erase-buffer)
        (insert table "\n")
        (display-buffer (current-buffer))))
    (when (and noninteractive (not passed))
      (kill-emacs 1))
    table))

;;;###autoload
(defun ekp-gui-verify-matrix ()
  "Run the display-context matrix and print a PASS/FAIL table.
Covers: plain, text-scale up/down, family+height face remap, and a
narrow frame with scaling.  Intended for `emacs -Q'; in a customized
session prefer `ekp-gui-verify'."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (save-current-buffer
    (ekp-gui-verify--matrix-1)))

(defun ekp-gui-verify--matrix-1 ()
  "Run the matrix cases; caller guards the current buffer."
  (let (results)
    (set-frame-size (selected-frame) 190 40)
    (push (ekp-gui-verify--case "base" #'ignore) results)
    (push (ekp-gui-verify--case "text-scale +3"
                                (lambda () (text-scale-set 3)))
          results)
    (push (ekp-gui-verify--case "text-scale -2"
                                (lambda () (text-scale-set -2)))
          results)
    (push (ekp-gui-verify--case "remap family+height"
                                (lambda ()
                                  (face-remap-add-relative
                                   'default :height 1.15)))
          results)
    (push (ekp-gui-verify--case "no fringes"
                                (lambda ()
                                  (set-window-fringes
                                   (get-buffer-window (current-buffer))
                                   0 0)))
          results)
    (push (ekp-gui-verify--case "no fringes + scale +2"
                                (lambda ()
                                  (set-window-fringes
                                   (get-buffer-window (current-buffer))
                                   0 0)
                                  (text-scale-set 2)))
          results)
    (set-frame-size (selected-frame) 70 40)
    (push (ekp-gui-verify--case "narrow + scale +2"
                                (lambda () (text-scale-set 2)))
          results)
    (ekp-gui-verify--report (nreverse results))))

(provide 'ekp-gui-verify)

;;; ekp-gui-verify.el ends here
