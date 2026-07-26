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
;;   `ekp-soft-break', `ekp-soft-hyphen', `ekp-hidden'), so the
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
  "Paragraphs re-justified per background chunk in lazy re-flows."
  :type 'natnum)

(defcustom ekp-auto-justify-tick-budget 0.005
  "Seconds of work per background tick in lazy re-flows.
Each tick processes chunks until the budget is exhausted (at least
one), then yields back to the command loop."
  :type 'number)

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

;;;###autoload
(defun ekp-org-setup ()
  "Configure the current (Org) buffer for ekp justification.
Protects source blocks, tables and meta lines from justification.
Typical use: (add-hook \\='org-mode-hook #\\='ekp-org-setup)."
  (setq-local ekp-region-skip-faces ekp-region-org-skip-faces))

;;;###autoload
(defun ekp-markdown-setup ()
  "Configure the current (Markdown) buffer for ekp justification.
Protects code faces from justification, and stops markdown-mode's
font-lock from managing the `display' property — refontification
would otherwise strip the pixel-glue display specs and wreck the
layout.  The cost: markdown's own display-based decorations (URL
hiding) are no longer cleaned up by refontification here.
Typical use: (add-hook \\='markdown-mode-hook #\\='ekp-markdown-setup)."
  (setq-local ekp-region-skip-faces ekp-region-markdown-skip-faces)
  (when (boundp 'font-lock-extra-managed-props)
    (setq-local font-lock-extra-managed-props
                (remq 'display font-lock-extra-managed-props))))

(defvar-local ekp-region-skip-predicate nil
  "When non-nil, a function called with a paragraph string.
Return non-nil to keep that paragraph verbatim (no justification).
The general escape hatch for mode-specific block detection; prefer
the `ekp-verbatim' text property or `ekp-region-skip-faces' when
they suffice.")

(defvar ekp-region--inhibit nil
  "Non-nil while ekp-region is modifying the buffer itself.")

(defvar-local ekp-region--save-state nil
  "Spans unjustified for saving: list of (BEG-MARKER END-MARKER WIDTH).
Set by `ekp-region--before-save', consumed by `ekp-region--after-save'.")

(defmacro ekp-region--preserving-modified (&rest body)
  "Run BODY, keeping the buffer unmodified if it was unmodified.
Justification is a reversible re-layout of the same logical text, so
it must not flip `buffer-modified-p' on its own — that would create
lock files, trigger auto-saves and \"buffer modified\" prompts for
buffers the user never edited."
  (declare (indent 0) (debug t))
  `(let ((ekp-region--modified-was (buffer-modified-p)))
     (prog1 (progn ,@body)
       (unless ekp-region--modified-was
         (restore-buffer-modified-p nil)))))

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

(defun ekp-region--effective-width (&optional buffer)
  "Justification width for BUFFER: the narrowest window showing it.
With the buffer in several windows only one width can be laid out;
the narrowest keeps every window free of overflow-wrapped lines.
Falls back to the selected window when the buffer is not displayed."
  (let ((wins (get-buffer-window-list (or buffer (current-buffer)) nil t)))
    (if wins
        (apply #'min (mapcar #'ekp-region--window-pixel wins))
      (ekp-region--window-pixel))))

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
      ;; Structured buffer text (comint/eshell prompts, forms) must
      ;; never be re-written: fields and read-only spans stay put.
      (text-property-not-all 0 (length para) 'field nil para)
      (text-property-not-all 0 (length para) 'read-only nil para)
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

(defun ekp-region--dwim-bounds ()
  "Region bounds when the region is active, else the paragraph at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (ekp-region--para-bounds (cons (point) (point)))))

;;;###autoload
(defun ekp-justify-region (beg end &optional pixel)
  "Justify the text between BEG and END to PIXEL width.
PIXEL defaults to the window text width (see `ekp-region-margin-pixel');
interactively, a numeric prefix argument supplies it explicitly.
Already-justified text is unjustified first, so the command is
idempotent and can re-flow to a new width."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (pcase-let ((`(,beg . ,end) (ekp-region--dwim-bounds)))
       (list beg end
             (and current-prefix-arg
                  (prefix-numeric-value current-prefix-arg))))))
  (setq pixel (or pixel (ekp-region--window-pixel)))
  (when (and font-lock-mode
             (or ekp-region-skip-faces ekp-region-skip-predicate))
    ;; Face-based verbatim detection needs real faces: parts of the
    ;; region jit-lock never displayed are not fontified yet.
    (font-lock-ensure (min beg end) (max beg end)))
  (let ((beg (copy-marker (min beg end)))
        (end (copy-marker (max beg end) t))
        (ekp-region--inhibit t)
        (inhibit-read-only t))
    (unwind-protect
        (ekp-region--preserving-modified
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
          (ekp-region--install-integrations))
      (set-marker beg nil)
      (set-marker end nil))))

(defun ekp-region--install-integrations ()
  "Install the buffer-local hooks justified text depends on.
Idempotent; added by `ekp-justify-region' and `ekp-auto-justify-mode'."
  ;; Saving a justified buffer must write the logical text.
  (add-hook 'before-save-hook #'ekp-region--before-save nil t)
  (add-hook 'after-save-hook #'ekp-region--after-save nil t)
  ;; Isearch searches the logical text.
  (add-hook 'isearch-mode-hook #'ekp-region--isearch-begin nil t)
  (add-hook 'isearch-mode-end-hook #'ekp-region--isearch-end nil t)
  ;; The kill ring receives the logical text.
  (setq-local filter-buffer-substring-function
              #'ekp-region--filter-buffer-substring))

(defun ekp-region--remove-integrations ()
  "Remove the hooks installed by `ekp-region--install-integrations'."
  (remove-hook 'before-save-hook #'ekp-region--before-save t)
  (remove-hook 'after-save-hook #'ekp-region--after-save t)
  (remove-hook 'isearch-mode-hook #'ekp-region--isearch-begin t)
  (remove-hook 'isearch-mode-end-hook #'ekp-region--isearch-end t)
  (when (eq filter-buffer-substring-function
            #'ekp-region--filter-buffer-substring)
    (kill-local-variable 'filter-buffer-substring-function)))

;;;###autoload
(defun ekp-unjustify-region (beg end)
  "Restore the logical text between BEG and END.
Removes synthesized glue and soft hyphens, replaces soft line breaks
with the whitespace they swallowed, and re-exposes hidden paragraph
tails.  Text the user typed into the justified region is preserved."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (pcase-let ((`(,beg . ,end) (ekp-region--dwim-bounds)))
       (list beg end))))
  (let ((end-m (copy-marker (max beg end) t))
        (ekp-region--inhibit t)
        (inhibit-read-only t))
    (unwind-protect
        (ekp-region--preserving-modified
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
           (remove-text-properties (min beg end) end-m '(ekp-justified nil))))
      (set-marker end-m nil))))

;;;; Saving: the file always receives the logical text

(defun ekp-region--justified-spans ()
  "Return justified spans of the buffer as a list of (BEG END WIDTH).
BEG/END are positions; WIDTH is the span's `ekp-justified' value."
  (let ((pos (point-min)) spans)
    (while (< pos (point-max))
      (let ((w (get-text-property pos 'ekp-justified))
            (next (next-single-property-change pos 'ekp-justified
                                               nil (point-max))))
        (when w (push (list pos next w) spans))
        (setq pos next)))
    (nreverse spans)))

(defun ekp-region--before-save ()
  "Restore the logical text before the buffer is written to disk.
Saving a justified buffer must never persist soft line breaks, glue
spaces or break hyphens: they are layout, not content.  The spans are
remembered (as markers) and re-justified by `ekp-region--after-save',
so the user never sees the buffer un-justified."
  (let ((spans (ekp-region--justified-spans)))
    (when spans
      ;; The unjustify+rejustify pair is deterministic and cancels out
      ;; exactly, so keep it off the undo history.
      (let ((buffer-undo-list t))
        ;; Marker-ize every span before the first unjustification
        ;; shifts the positions of the spans after it.
        (setq ekp-region--save-state
              (mapcar (pcase-lambda (`(,beg ,end ,width))
                        (list (copy-marker beg) (copy-marker end t) width))
                      spans))
        (pcase-dolist (`(,beg ,end ,_width) ekp-region--save-state)
          (ekp-unjustify-region beg end))))))

(defun ekp-region--after-save ()
  "Re-justify the spans un-done by `ekp-region--before-save'."
  (when ekp-region--save-state
    (let ((buffer-undo-list t))
      (pcase-dolist (`(,beg ,end ,width) ekp-region--save-state)
        (when (and (marker-position beg) (marker-position end))
          (ekp-justify-region beg end width))
        (set-marker beg nil)
        (set-marker end nil)))
    (setq ekp-region--save-state nil)
    ;; The file on disk holds exactly this buffer's logical text.
    (set-buffer-modified-p nil)))

;;;; Isearch: search the logical text

(defvar-local ekp-region--isearch-state nil
  "Spans unjustified while isearch is active: ((BEG-M END-M WIDTH)...).")

(defun ekp-region--isearch-begin ()
  "Show the logical text while searching.
Justified layout injects real space characters between CJK glyphs and
splits words across soft breaks and hyphens, so searching the layout
finds almost nothing.  The buffer is un-justified for the duration of
the search and restored by `ekp-region--isearch-end'."
  (let ((spans (and (null ekp-region--isearch-state)
                    (ekp-region--justified-spans))))
    (when spans
      (let ((buffer-undo-list t))
        (setq ekp-region--isearch-state
              (mapcar (pcase-lambda (`(,beg ,end ,width))
                        (list (copy-marker beg) (copy-marker end t) width))
                      spans))
        (pcase-dolist (`(,beg ,end ,_w) ekp-region--isearch-state)
          (ekp-unjustify-region beg end))))))

(defun ekp-region--isearch-end ()
  "Restore the justified layout after isearch."
  (when ekp-region--isearch-state
    (let ((buffer-undo-list t))
      (pcase-dolist (`(,beg ,end ,width) ekp-region--isearch-state)
        (when (and (marker-position beg) (marker-position end))
          (ekp-justify-region beg end width))
        (set-marker beg nil)
        (set-marker end nil)))
    (setq ekp-region--isearch-state nil)))

;;;; Kill/yank: the kill ring receives the logical text

(defun ekp-region--logical-string (string)
  "Return STRING with any ekp layout markers structurally inverted.
Non-justified strings are returned unchanged (same object)."
  (if (cl-some (lambda (prop)
                 (text-property-not-all 0 (length string) prop nil string))
               '(ekp-glue ekp-soft-break ekp-soft-hyphen
                 ekp-hidden ekp-justified))
      (with-temp-buffer
        (insert string)
        (ekp-unjustify-region (point-min) (point-max))
        (buffer-string))
    string))

(defun ekp-region--filter-buffer-substring (beg end &optional delete)
  "Extract BEG..END for the kill ring as logical text.
Killing justified text and yanking it elsewhere must transport the
words, not the pixel layout of the source window (DELETE as in
`filter-buffer-substring-function')."
  (ekp-region--logical-string (buffer-substring--filter beg end delete)))

;;;###autoload
(defun ekp-justify-buffer (&optional pixel)
  "Justify the whole accessible portion of the buffer to PIXEL width.
PIXEL defaults to the window text width; interactively, a numeric
prefix argument supplies it explicitly."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (and current-prefix-arg
                (prefix-numeric-value current-prefix-arg)))))
  (ekp-justify-region (point-min) (point-max) pixel))

;;;###autoload
(defun ekp-unjustify-buffer ()
  "Restore the logical text of the whole accessible portion."
  (interactive "*")
  (ekp-unjustify-region (point-min) (point-max)))

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
  "Record the edit between BEG and END for incremental re-justification.
Changes applied by undo are not re-flowed: re-justifying behind the
user's back would fight the undo sequence (and immediately dirty what
undo just restored).  The next real edit or resize re-flows normally."
  (when (and ekp-auto-justify-mode (not ekp-region--inhibit)
             (not undo-in-progress))
    (push (cons (copy-marker beg) (copy-marker end)) ekp-region--dirty)
    (when (timerp ekp-region--edit-timer)
      (cancel-timer ekp-region--edit-timer))
    (setq ekp-region--edit-timer
          (run-with-idle-timer ekp-auto-justify-edit-delay nil
                               #'ekp-region--flush-dirty (current-buffer)))))

(defun ekp-region--composing-p ()
  "Non-nil while an input method composition (quail preedit) is active.
Re-flowing the buffer under a live preedit overlay corrupts the
composition the user is still typing."
  (and (bound-and-true-p quail-overlay)
       (overlayp quail-overlay)
       (overlay-buffer quail-overlay)))

(defun ekp-region--flush-dirty (buffer)
  "Re-justify the paragraphs of BUFFER touched by recent edits."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (ekp-region--composing-p)
          ;; Let the user finish composing; try again after the delay.
          (setq ekp-region--edit-timer
                (run-with-idle-timer ekp-auto-justify-edit-delay nil
                                     #'ekp-region--flush-dirty buffer))
        (when (and ekp-auto-justify-mode ekp-region--dirty ekp-region--auto-width)
          (let* ((pairs (prog1 ekp-region--dirty (setq ekp-region--dirty nil)))
                 ;; Convert all bounds to markers before the first
                 ;; re-justification shifts later positions.
                 (regions (mapcar (lambda (r)
                                    (cons (copy-marker (car r))
                                          (copy-marker (cdr r) t)))
                                  (ekp-region--merge-regions
                                   (mapcar #'ekp-region--para-bounds pairs))))
                 (total (cl-reduce #'+ regions
                                   :key (lambda (r) (- (cdr r) (car r)))
                                   :initial-value 0)))
            (if (> total ekp-auto-justify-lazy-threshold)
                ;; A huge dirty area (big paste, revert): chunk it like
                ;; a lazy re-flow instead of freezing the command loop.
                (progn
                  (ekp-region--enqueue-chunks
                   (mapcan (lambda (r)
                             (prog1 (ekp-region--make-chunks (car r) (cdr r))
                               (set-marker (car r) nil)
                               (set-marker (cdr r) nil)))
                           regions))
                  (ekp-region--prioritize-visible))
              (dolist (r regions)
                (ekp-justify-region (car r) (cdr r) ekp-region--auto-width)
                (set-marker (car r) nil)
                (set-marker (cdr r) nil)))
            (dolist (p pairs)
              (set-marker (car p) nil)
              (set-marker (cdr p) nil))))))))

(defun ekp-region--enqueue-chunks (chunks)
  "Queue CHUNKS for background processing at the current auto width.
Prepends to an existing queue at the same width (edits win over the
tail of a resize re-flow); anything queued for a stale width was
already superseded and is dropped."
  (when chunks
    (if (and ekp-region--pending
             (eql (car ekp-region--pending) ekp-region--auto-width))
        (setcdr ekp-region--pending
                (nconc chunks (cdr ekp-region--pending)))
      (ekp-region--cancel-pending)
      (setq ekp-region--pending (cons ekp-region--auto-width chunks)))
    (unless (timerp ekp-region--chunk-timer)
      (setq ekp-region--chunk-timer
            (run-with-timer 0.02 nil #'ekp-region--process-chunk
                            (current-buffer))))))

(defun ekp-region--prioritize-visible ()
  "Move queued chunks that intersect the visible span to the front.
Scrolling into an unprocessed area should not have to wait for the
whole queue."
  (when (cdr ekp-region--pending)
    (pcase-let ((`(,vbeg . ,vend) (ekp-region--visible-span)))
      (let* ((chunks (cdr ekp-region--pending))
             (vis (cl-remove-if-not
                   (lambda (c) (and (< (car c) vend) (> (cdr c) vbeg)))
                   chunks))
             (rest (cl-remove-if
                    (lambda (c) (memq c vis))
                    chunks)))
        (setcdr ekp-region--pending (nconc vis rest))))))

(defun ekp-region--on-scroll (window _start)
  "Re-prioritize the lazy queue after WINDOW scrolled.
Runs off a zero timer: inside `window-scroll-functions' the window's
final extent is not known yet."
  (let ((buf (window-buffer window)))
    (when (buffer-live-p buf)
      (run-with-timer
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and ekp-auto-justify-mode ekp-region--pending)
               (ekp-region--prioritize-visible)))))))))

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
        (ekp-region--schedule-reflow)))))

(defun ekp-region--on-window-change ()
  "Re-check the layout width after the window configuration changed.
Catches the buffer becoming displayed (possibly for the first time),
window splits, and deletions of the narrowest window."
  (ekp-region--schedule-reflow))

(defun ekp-region--schedule-reflow ()
  "Debounce a re-flow of the current buffer to its effective width."
  (when ekp-auto-justify-mode
    (let ((w (ekp-region--effective-width)))
      (when (and ekp-region--auto-width (/= w ekp-region--auto-width))
        (when (timerp ekp-region--resize-timer)
          (cancel-timer ekp-region--resize-timer))
        (setq ekp-region--resize-timer
              (run-with-timer ekp-auto-justify-resize-delay nil
                              #'ekp-region--reflow
                              (current-buffer) w))))))

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
       ;; be polite: yield to pending input and live compositions
       ((or (input-pending-p) (ekp-region--composing-p))
        (setq ekp-region--chunk-timer
              (run-with-timer 0.1 nil #'ekp-region--process-chunk buffer)))
       (t
        (let ((width (car ekp-region--pending))
              (deadline (+ (float-time) ekp-auto-justify-tick-budget))
              (first t))
          ;; Work until the tick budget runs out — at least one chunk,
          ;; never with input waiting.  Peek-then-pop: an abort inside
          ;; justification must not lose the chunk.
          (while (and (cdr ekp-region--pending)
                      (or first
                          (and (< (float-time) deadline)
                               (not (input-pending-p)))))
            (setq first nil)
            (let ((chunk (cadr ekp-region--pending)))
              (ekp-justify-region (car chunk) (cdr chunk) width)
              (setcdr ekp-region--pending (cddr ekp-region--pending))
              (set-marker (car chunk) nil)
              (set-marker (cdr chunk) nil)))
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

(defun ekp-refill-paragraph ()
  "Re-justify the hard paragraph at point (ekp's `fill-paragraph').
Bound to \\[fill-paragraph] while `ekp-auto-justify-mode' is on:
plain `fill-paragraph' would treat glue spaces and soft breaks as
content and destroy the original whitespace."
  (interactive "*")
  (pcase-let ((`(,beg . ,end)
               (ekp-region--para-bounds (cons (point) (point)))))
    (ekp-justify-region beg end (or ekp-region--auto-width
                                    (ekp-region--window-pixel)))))

(defvar-keymap ekp-auto-justify-mode-map
  :doc "Keymap for `ekp-auto-justify-mode'."
  "<remap> <fill-paragraph>" #'ekp-refill-paragraph)

;;;###autoload
(define-minor-mode ekp-auto-justify-mode
  "Keep the buffer pixel-justified to the window width.
Re-flows when the window width changes and re-justifies edited
paragraphs incrementally.  Designed for reading and previewing;
the buffer text is restored exactly when the mode is turned off."
  :lighter " EKP"
  :keymap ekp-auto-justify-mode-map
  (if ekp-auto-justify-mode
      (progn
        ;; Out-of-the-box protection for the common markup modes,
        ;; unless the user configured their own.
        (unless (or ekp-region-skip-faces ekp-region-skip-predicate)
          (cond ((derived-mode-p 'org-mode) (ekp-org-setup))
                ((derived-mode-p 'markdown-mode) (ekp-markdown-setup))))
        (setq ekp-region--auto-width (ekp-region--effective-width))
        (ekp-region--reflow (current-buffer) ekp-region--auto-width)
        (add-hook 'window-size-change-functions #'ekp-region--on-resize nil t)
        (add-hook 'window-configuration-change-hook
                  #'ekp-region--on-window-change nil t)
        (add-hook 'window-scroll-functions #'ekp-region--on-scroll nil t)
        (add-hook 'after-change-functions #'ekp-region--after-change nil t)
        (ekp-region--install-integrations)
        ;; Turning the major mode off/over kills local hooks silently;
        ;; the buffer must get its logical text back first.
        (add-hook 'change-major-mode-hook #'ekp-region--teardown nil t))
    (remove-hook 'window-size-change-functions #'ekp-region--on-resize t)
    (remove-hook 'window-configuration-change-hook
                 #'ekp-region--on-window-change t)
    (remove-hook 'window-scroll-functions #'ekp-region--on-scroll t)
    (remove-hook 'after-change-functions #'ekp-region--after-change t)
    (remove-hook 'change-major-mode-hook #'ekp-region--teardown t)
    (ekp-region--teardown)
    ;; The teardown removed all justified text; a later
    ;; ekp-justify-region re-installs what it needs.
    (ekp-region--remove-integrations)))

(defun ekp-region--teardown ()
  "Cancel timers and restore the whole buffer's logical text.
Runs when `ekp-auto-justify-mode' is turned off and, via
`change-major-mode-hook', when a major-mode switch is about to
discard the mode silently."
  (when (timerp ekp-region--resize-timer)
    (cancel-timer ekp-region--resize-timer))
  (when (timerp ekp-region--edit-timer)
    (cancel-timer ekp-region--edit-timer))
  (ekp-region--cancel-pending)
  (dolist (p ekp-region--dirty)
    (set-marker (car p) nil)
    (set-marker (cdr p) nil))
  (setq ekp-region--resize-timer nil
        ekp-region--edit-timer nil
        ekp-region--dirty nil
        ekp-region--auto-width nil)
  ;; Narrowing must not leave justified orphans outside the visible
  ;; region.
  (save-restriction
    (widen)
    (ekp-unjustify-region (point-min) (point-max))))

(provide 'ekp-region)

;;; ekp-region.el ends here
