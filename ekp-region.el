;;; ekp-region.el --- Buffer-level justification for ekp -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; Keywords: wp, convenience

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

;; Interactive layer over the ekp string API.
;;
;; - `ekp-justify-region' / `ekp-unjustify-region': justify buffer text
;;   in place.  Unjustification is a pure structural transform driven by
;;   the text properties the renderer leaves behind (`ekp-glue',
;;   `ekp-soft-break', `ekp-soft-hyphen', `ekp-soft-trail'), so the
;;   original text — including whitespace runs stripped at line breaks —
;;   is recovered exactly, even after the justified text was edited.
;;
;; - `ekp-auto-justify-mode': keeps the whole buffer justified to the
;;   window width.  Re-flows (debounced) when the window width changes,
;;   and incrementally re-justifies only the edited paragraphs after
;;   edits, so large buffers stay responsive (unchanged paragraphs hit
;;   the ekp paragraph cache).

;;; Code:

(require 'ekp)
(require 'cl-lib)

(defvar ekp-auto-justify-mode)

(defgroup ekp-region nil
  "Buffer-level justification built on ekp."
  :group 'text
  :prefix "ekp-")

(defcustom ekp-region-margin-pixel 2
  "Pixels subtracted from the window body width when justifying.
A small safety margin that keeps justified lines from being wrapped
by the display engine due to rounding."
  :type 'natnum)

(defcustom ekp-auto-justify-resize-delay 0.15
  "Seconds to debounce window-resize re-flows in `ekp-auto-justify-mode'."
  :type 'number)

(defcustom ekp-auto-justify-edit-delay 0.3
  "Idle seconds before edited paragraphs are re-justified."
  :type 'number)

(defcustom ekp-auto-justify-lazy-threshold 20000
  "Buffer size (characters) beyond which re-flows go visible-first.
Below it a window-width change re-justifies the whole buffer at
once; above it the visible portion is done synchronously and the
rest follows in idle background chunks."
  :type 'natnum)

(defcustom ekp-auto-justify-chunk-size 10
  "Paragraphs re-justified per background tick in lazy re-flows."
  :type 'natnum)

(defconst ekp-region-org-skip-faces
  '(org-block org-block-begin-line org-block-end-line org-code
    org-verbatim org-table org-meta-line)
  "Reasonable `ekp-region-skip-faces' preset for Org buffers.")

(defconst ekp-region-markdown-skip-faces
  '(markdown-code-face markdown-inline-code-face markdown-pre-face
    markdown-table-face)
  "Reasonable `ekp-region-skip-faces' preset for Markdown buffers.")

(defcustom ekp-region-skip-faces nil
  "Faces whose paragraphs are never justified (kept verbatim).
Point major-mode faces here — e.g. `org-block' and `org-code' for
Org, `markdown-code-face' for Markdown — and code blocks pass
through untouched.  Checked against the `face' property of each
paragraph, symbol or list."
  :type '(repeat face))

(defvar-local ekp-region-skip-predicate nil
  "When non-nil, a function called with a paragraph string.
Return non-nil to keep that paragraph verbatim (no justification).
The general escape hatch for mode-specific block detection; prefer
the `ekp-verbatim' text property or `ekp-region-skip-faces' when
they suffice.")

(defvar ekp-region--inhibit nil
  "Non-nil while ekp-region is modifying the buffer itself.")

(defvar-local ekp-region--auto-width nil
  "Pixel width the buffer is currently auto-justified to.")

(defvar-local ekp-region--resize-timer nil)
(defvar-local ekp-region--edit-timer nil)
(defvar-local ekp-region--dirty nil
  "Pending edited regions, as a list of (BEG-MARKER . END-MARKER).")

(defvar-local ekp-region--pending nil
  "Lazy re-flow state: (WIDTH . CHUNKS), CHUNKS = ((BEG-M . END-M)...).")

(defvar-local ekp-region--chunk-timer nil)

;;;; Width

(defun ekp-region-protrusion-reserve ()
  "Pixels reserved at the right margin for hanging punctuation.
Non-zero only while `ekp-protrusion' is enabled: protruding glyphs
extend past the flush edge, so the layout width must leave room."
  (if ekp-protrusion
      (max 2 (ceiling (* (alist-get 'cjk-close ekp-protrusion-ratios 0.5)
                         (string-pixel-width "。"))))
    0))

(defun ekp-region--window-pixel (&optional window)
  "Usable text width in pixels of WINDOW (default: selected window)."
  (max 1 (- (window-body-width window t)
            ekp-region-margin-pixel
            (ekp-region-protrusion-reserve))))

;;;; Pure string transforms

(defun ekp-region--split-hard (string)
  "Split justified STRING on hard newlines (those without `ekp-soft-break')."
  (let ((parts nil) (start 0) (i 0) (len (length string)))
    (while (< i len)
      (when (and (eq (aref string i) ?\n)
                 (not (get-text-property i 'ekp-soft-break string)))
        (push (substring string start i) parts)
        (setq start (1+ i)))
      (setq i (1+ i)))
    (push (substring string start) parts)
    (nreverse parts)))

(defun ekp-region--face-hit-p (string)
  "Non-nil when STRING carries any face from `ekp-region-skip-faces'."
  (let ((pos 0) (len (length string)) hit)
    (while (and (not hit) (< pos len))
      (let ((f (get-text-property pos 'face string)))
        (when (if (listp f)
                  (seq-intersection f ekp-region-skip-faces)
                (memq f ekp-region-skip-faces))
          (setq hit t))
        (setq pos (or (next-single-property-change pos 'face string len)
                      len))))
    hit))

(defun ekp-region--skip-para-p (para)
  "Non-nil when the paragraph string PARA must stay verbatim.
Code blocks and other protected text: marked with the `ekp-verbatim'
property, matching `ekp-region-skip-faces', or accepted by
`ekp-region-skip-predicate'."
  (or (text-property-not-all 0 (length para) 'ekp-verbatim nil para)
      (and ekp-region-skip-faces (ekp-region--face-hit-p para))
      (and ekp-region-skip-predicate
           (funcall ekp-region-skip-predicate para))))

(defun ekp-region--justify-string (text pixel)
  "Return TEXT justified to PIXEL with exact-recovery markers.
Hard newlines are preserved one-to-one.  Whitespace-only paragraphs
(which the string API would empty out) survive as hidden text;
verbatim paragraphs (see `ekp-region--skip-para-p') pass through
untouched."
  (let* ((paras (split-string text "\n"))
         (skips (mapcar #'ekp-region--skip-para-p paras))
         (cores (cl-loop for p in paras for s in skips
                         unless (or s (string-blank-p p)) collect p))
         (out (and cores
                   (ekp-region--split-hard
                    (ekp-pixel-justify (string-join cores "\n") pixel)))))
    (unless (= (length out) (length cores))
      (error "ekp-region: paragraph count mismatch (%d vs %d)"
             (length out) (length cores)))
    (string-join
     (cl-loop for p in paras for s in skips
              collect (cond (s p)
                            ((string-blank-p p) (ekp--hide-string p))
                            (t (pop out))))
     "\n")))

(defun ekp-region--pos-for-offset (string offset)
  "Physical position in justified STRING for logical OFFSET.
Glue characters count for the length of the original text they
replaced (their `ekp-glue' value), soft breaks for their payload,
soft hyphens for nothing; everything else (including hidden text)
is one logical character."
  (let ((i 0) (len (length string)))
    (while (and (< i len) (> offset 0))
      (let ((glue (get-text-property i 'ekp-glue string)))
        (cond
         (glue
          (setq offset (- offset (length glue))))
         ((get-text-property i 'ekp-soft-hyphen string))
         ((and (eq (aref string i) ?\n)
               (get-text-property i 'ekp-soft-break string))
          (setq offset (- offset (length (get-text-property
                                          i 'ekp-soft-break string)))))
         (t (setq offset (1- offset)))))
      (setq i (1+ i)))
    i))

;;;; Commands

;;;###autoload
(defun ekp-justify-region (beg end &optional pixel)
  "Justify the text between BEG and END to PIXEL width.
PIXEL defaults to the window text width (see `ekp-region-margin-pixel');
interactively, a numeric prefix argument supplies it explicitly.
Already-justified text is unjustified first, so the command is
idempotent and can re-flow to a new width."
  (interactive
   (list (region-beginning) (region-end)
         (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (setq pixel (or pixel (ekp-region--window-pixel)))
  (let ((beg (copy-marker (min beg end)))
        (end (copy-marker (max beg end) t))
        (ekp-region--inhibit t)
        (inhibit-read-only t))
    (unwind-protect
        (atomic-change-group
          ;; Re-flow support: strip previous justification first.
          (when (text-property-not-all beg end 'ekp-justified nil)
            (ekp-unjustify-region beg end))
          (let* ((text (buffer-substring beg end))
                 (justified (ekp-region--justify-string text pixel))
                 (point-offset (and (>= (point) beg) (< (point) end)
                                    (- (point) beg))))
            (unless (equal-including-properties text justified)
              (goto-char beg)
              (delete-region beg end)
              (insert justified)
              (when point-offset
                (goto-char (+ beg (ekp-region--pos-for-offset
                                   justified point-offset)))))
            (add-text-properties beg end (list 'ekp-justified pixel))))
      (set-marker beg nil)
      (set-marker end nil))))

;;;###autoload
(defun ekp-unjustify-region (beg end)
  "Restore the logical text between BEG and END.
Removes synthesized glue and soft hyphens, replaces soft line breaks
with the whitespace they swallowed, and re-exposes hidden paragraph
tails.  Text the user typed into the justified region is preserved."
  (interactive "r")
  (let ((end-m (copy-marker (max beg end) t))
        (ekp-region--inhibit t)
        (inhibit-read-only t))
    (unwind-protect
        (save-excursion
          (goto-char (min beg end))
          (while (< (point) end-m)
            (let* ((pos (point))
                   (glue (get-text-property pos 'ekp-glue)))
              (cond
               (glue
                (delete-region pos (1+ pos))
                (when (stringp glue) (insert glue)))
               ((get-text-property pos 'ekp-soft-hyphen)
                (delete-region pos (1+ pos)))
               ((and (eq (char-after pos) ?\n)
                     (get-text-property pos 'ekp-soft-break))
                (let ((payload (get-text-property pos 'ekp-soft-break)))
                  (delete-region pos (1+ pos))
                  (insert payload)))
               ((get-text-property pos 'ekp-hidden)
                (remove-text-properties pos (1+ pos)
                                        '(ekp-hidden nil display nil))
                (forward-char 1))
               ;; Plain text: our markers are sparse, so hop straight
               ;; to the next property boundary instead of stepping
               ;; char by char.
               (t (goto-char (min (marker-position end-m)
                                  (next-property-change pos nil
                                                        (marker-position
                                                         end-m))))))))
          (remove-text-properties (min beg end) end-m '(ekp-justified nil)))
      (set-marker end-m nil))))

;;;###autoload
(defun ekp-no-break-region (beg end)
  "Mark the region as an unbreakable typesetting atom.
Justification treats it as one rigid unit: no line break inside, no
hyphenation, spacing stays literal (inline code, product names,
numbers with units)."
  (interactive "r")
  (add-text-properties beg end '(ekp-no-break t)))

;;;###autoload
(defun ekp-allow-break-region (beg end)
  "Remove `ekp-no-break' marking from the region."
  (interactive "r")
  (remove-text-properties beg end '(ekp-no-break nil)))

;;;###autoload
(defun ekp-verbatim-region (beg end)
  "Protect the region's paragraphs from justification (code blocks).
Whole paragraphs carrying the `ekp-verbatim' property pass through
`ekp-justify-region' and `ekp-auto-justify-mode' untouched.  For an
unbreakable span inside prose, use `ekp-no-break-region' instead."
  (interactive "r")
  (add-text-properties beg end '(ekp-verbatim t)))

;;;###autoload
(defun ekp-clear-verbatim-region (beg end)
  "Remove `ekp-verbatim' protection from the region."
  (interactive "r")
  (remove-text-properties beg end '(ekp-verbatim nil)))

;;;; Auto-justify minor mode

(defun ekp-region--para-bounds (marker-pair)
  "Hard-paragraph bounds containing MARKER-PAIR, as (BEG . END)."
  (let ((b (let ((x (car marker-pair)))
             (if (markerp x) (marker-position x) x)))
        (e (let ((x (cdr marker-pair)))
             (if (markerp x) (marker-position x) x))))
    (save-excursion
      (goto-char (max (point-min) (min b (point-max))))
      (while (and (> (point) (point-min))
                  (let ((prev (1- (point))))
                    (not (and (eq (char-after prev) ?\n)
                              (not (get-text-property prev 'ekp-soft-break))))))
        (forward-char -1))
      (setq b (point))
      (goto-char (max (point-min) (min e (point-max))))
      (while (and (< (point) (point-max))
                  (not (and (eq (char-after) ?\n)
                            (not (get-text-property (point) 'ekp-soft-break)))))
        (forward-char 1))
      (cons b (point)))))

(defun ekp-region--merge-regions (regions)
  "Merge overlapping or adjacent (BEG . END) REGIONS."
  (let ((sorted (sort regions (lambda (a b) (< (car a) (car b)))))
        merged)
    (dolist (r sorted)
      (if (and merged (<= (car r) (cdr (car merged))))
          (setcdr (car merged) (max (cdr (car merged)) (cdr r)))
        (push (cons (car r) (cdr r)) merged)))
    (nreverse merged)))

(defun ekp-region--after-change (beg end _len)
  "Record the edit between BEG and END for incremental re-justification."
  (when (and ekp-auto-justify-mode (not ekp-region--inhibit))
    (push (cons (copy-marker beg) (copy-marker end)) ekp-region--dirty)
    (when (timerp ekp-region--edit-timer)
      (cancel-timer ekp-region--edit-timer))
    (setq ekp-region--edit-timer
          (run-with-idle-timer ekp-auto-justify-edit-delay nil
                               #'ekp-region--flush-dirty (current-buffer)))))

(defun ekp-region--flush-dirty (buffer)
  "Re-justify the paragraphs of BUFFER touched by recent edits."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and ekp-auto-justify-mode ekp-region--dirty ekp-region--auto-width)
        (let* ((pairs (prog1 ekp-region--dirty (setq ekp-region--dirty nil)))
               ;; Convert all bounds to markers before the first
               ;; re-justification shifts later positions.
               (regions (mapcar (lambda (r)
                                  (cons (copy-marker (car r))
                                        (copy-marker (cdr r) t)))
                                (ekp-region--merge-regions
                                 (mapcar #'ekp-region--para-bounds pairs)))))
          (dolist (r regions)
            (ekp-justify-region (car r) (cdr r) ekp-region--auto-width)
            (set-marker (car r) nil)
            (set-marker (cdr r) nil))
          (dolist (p pairs)
            (set-marker (car p) nil)
            (set-marker (cdr p) nil)))))))

(defun ekp-region--on-resize (window-or-frame)
  "Debounced re-flow after WINDOW-OR-FRAME changed size.
Buffer-local members of `window-size-change-functions' receive the
window showing the buffer — and are not guaranteed to run with that
buffer current — so resolve both explicitly."
  (let ((win (cond ((windowp window-or-frame) window-or-frame)
                   ((framep window-or-frame)
                    (get-buffer-window (current-buffer) window-or-frame))
                   (t (get-buffer-window (current-buffer))))))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (when ekp-auto-justify-mode
          (let ((w (ekp-region--window-pixel win)))
            (when (and ekp-region--auto-width (/= w ekp-region--auto-width))
              (when (timerp ekp-region--resize-timer)
                (cancel-timer ekp-region--resize-timer))
              (setq ekp-region--resize-timer
                    (run-with-timer ekp-auto-justify-resize-delay nil
                                    #'ekp-region--reflow
                                    (current-buffer) w)))))))))

(defun ekp-region--cancel-pending ()
  "Drop any queued lazy re-flow chunks."
  (when (timerp ekp-region--chunk-timer)
    (cancel-timer ekp-region--chunk-timer))
  (setq ekp-region--chunk-timer nil)
  (dolist (c (cdr ekp-region--pending))
    (set-marker (car c) nil)
    (set-marker (cdr c) nil))
  (setq ekp-region--pending nil))

(defun ekp-region--make-chunks (beg end)
  "Split [BEG, END) into marker-pair chunks of whole hard paragraphs."
  (let ((chunks nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((cbeg (point)) (paras 0))
          (while (and (< (point) end)
                      (< paras ekp-auto-justify-chunk-size))
            (if (search-forward "\n" end 'move)
                (unless (get-text-property (match-beginning 0)
                                           'ekp-soft-break)
                  (setq paras (1+ paras)))
              nil))
          (when (> (point) cbeg)
            ;; BEG has insertion-type t: the previous chunk's re-insert
            ;; happens exactly at this boundary, and the marker must
            ;; end up after that text, not before it.
            (push (cons (copy-marker cbeg t) (copy-marker (point) t))
                  chunks)))))
    (nreverse chunks)))

(defun ekp-region--visible-span ()
  "Visible portion of the current buffer, as (BEG . END)."
  (let ((win (get-buffer-window (current-buffer))))
    (if win
        (cons (window-start win) (or (window-end win t) (point-max)))
      (cons (point-min) (point-max)))))

(defun ekp-region--process-chunk (buffer)
  "Re-justify the next queued chunk of BUFFER, then reschedule."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ekp-region--chunk-timer nil)
      (cond
       ((or (not ekp-auto-justify-mode) (null ekp-region--pending))
        (ekp-region--cancel-pending))
       ;; a newer re-flow superseded this queue
       ((not (eql (car ekp-region--pending) ekp-region--auto-width))
        (ekp-region--cancel-pending))
       ;; be polite: yield to pending input, try again shortly
       ((input-pending-p)
        (setq ekp-region--chunk-timer
              (run-with-timer 0.1 nil #'ekp-region--process-chunk buffer)))
       (t
        (let* ((width (car ekp-region--pending))
               (chunk (pop (cdr ekp-region--pending))))
          (when chunk
            (ekp-justify-region (car chunk) (cdr chunk) width)
            (set-marker (car chunk) nil)
            (set-marker (cdr chunk) nil))
          (if (cdr ekp-region--pending)
              (setq ekp-region--chunk-timer
                    (run-with-timer 0.02 nil
                                    #'ekp-region--process-chunk buffer))
            (setq ekp-region--pending nil))))))))

(defun ekp-region--reflow (buffer width)
  "Re-justify BUFFER to WIDTH — whole buffer, or visible-first when large."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when ekp-auto-justify-mode
        (setq ekp-region--auto-width width)
        (ekp-region--cancel-pending)
        (if (< (- (point-max) (point-min))
               ekp-auto-justify-lazy-threshold)
            (ekp-justify-region (point-min) (point-max) width)
          ;; visible part now, the rest in background chunks
          (pcase-let* ((`(,vbeg . ,vend) (ekp-region--visible-span))
                       (`(,pbeg . ,pend)
                        (ekp-region--para-bounds (cons vbeg vend))))
            (ekp-justify-region pbeg pend width)
            (let ((chunks (nconc
                           ;; start at PEND so the hard newline there
                           ;; gets its ekp-justified property too
                           (ekp-region--make-chunks pend (point-max))
                           (ekp-region--make-chunks (point-min) pbeg))))
              (when chunks
                (setq ekp-region--pending (cons width chunks))
                (setq ekp-region--chunk-timer
                      (run-with-timer 0.02 nil
                                      #'ekp-region--process-chunk
                                      buffer))))))))))

;;;###autoload
(define-minor-mode ekp-auto-justify-mode
  "Keep the buffer pixel-justified to the window width.
Re-flows when the window width changes and re-justifies edited
paragraphs incrementally.  Designed for reading and previewing;
the buffer text is restored exactly when the mode is turned off."
  :lighter " EKP"
  (if ekp-auto-justify-mode
      (progn
        (setq ekp-region--auto-width
              (ekp-region--window-pixel (get-buffer-window)))
        (ekp-region--reflow (current-buffer) ekp-region--auto-width)
        (add-hook 'window-size-change-functions #'ekp-region--on-resize nil t)
        (add-hook 'after-change-functions #'ekp-region--after-change nil t))
    (remove-hook 'window-size-change-functions #'ekp-region--on-resize t)
    (remove-hook 'after-change-functions #'ekp-region--after-change t)
    (when (timerp ekp-region--resize-timer)
      (cancel-timer ekp-region--resize-timer))
    (when (timerp ekp-region--edit-timer)
      (cancel-timer ekp-region--edit-timer))
    (ekp-region--cancel-pending)
    (setq ekp-region--resize-timer nil
          ekp-region--edit-timer nil
          ekp-region--dirty nil
          ekp-region--auto-width nil)
    (ekp-unjustify-region (point-min) (point-max))))

(provide 'ekp-region)

;;; ekp-region.el ends here
