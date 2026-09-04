;;; ekp-utils.el --- Utility functions for EKP -*- lexical-binding: t; -*-

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
  "Return font family name used to display STRING at POSITION.
Falls back to the default face family when no window-system font
information is available (batch mode, tty frames)."
  (if-let* ((font (and (display-multi-font-p)
                       (ignore-errors (font-at (or position 0) nil string)))))
      (format "%s" (font-get font :family))
    (let ((family (face-attribute 'default :family)))
      (if (stringp family) family (format "%s" family)))))

;; GUI-only C function; absent in non-window-system builds (emacs-nox).
;; Call sites are guarded by `display-multi-font-p'.
(declare-function font-info "font.c" (name &optional frame))

(defun ekp-font-monospace-p (font-family)
  "Return non-nil if FONT-FAMILY appears to be monospace.
Returns nil (unknown) when font information is unavailable."
  (when-let* ((font (and (display-multi-font-p)
                         (find-font (font-spec :family font-family))))
              (font-name (font-xlfd-name font)))
    (let ((type (nth 10 (split-string font-name "-" t))))
      ;; 'c' used in terminal
      (or (or (string= "m" type) (string= "c" type))
          (let ((info (font-info font-name)))
            (and info (> (length info) 4)
                 ;; 等宽字体的核心标志: 最大宽度等于平均宽度
                 (= (aref info 7) (aref info 11))))))))

(defun ekp-get-latin-letter (string)
  "Return the first Latin letter (a-z or A-Z) in STRING, or nil if none."
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
  "Return the first wide CJK character in STRING, or nil if none."
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
  "Return the font family of STRING's Latin letters when monospace.
Return nil when that font is not monospace, or the default face
family when STRING contains no Latin letter."
  (if-let* ((letter (ekp-get-latin-letter string))
            (font-family (ekp-font-family letter)))
      ;; return monospace font family
      (when (ekp-font-monospace-p font-family)
        font-family)
    ;; no latin letter in string, use default
    (face-attribute 'default :family)))

(declare-function ekp--measured-width "ekp")

(defun ekp-word-spacing-pixel (string)
  "Return the pixel width of an inter-word space for STRING.
Use the blank glyph of STRING's Latin font; for a monospace font
that width is the space's own advance."
  ;; font is monospace, use the pixel of blank
  ;; as word spacing pixel
  (if-let* ((font-family (ekp-monospace-p string)))
      (ekp--measured-width
       (propertize " " 'face `(:family ,font-family)))
    (let* ((letter (ekp-get-latin-letter string))
           (font-family (ekp-font-family letter)))
      (ekp--measured-width
       (propertize
        " " 'face `(:family ,font-family))))))

(defun ekp-latin-font (string)
  "Return the font family used for STRING's Latin letters.
Fall back to the default face family when STRING has no Latin letter."
  (if-let* ((letter (ekp-get-latin-letter string)))
      (ekp-font-family letter)
    (face-attribute 'default :family)))

(defun ekp-cjk-font (string)
  "Return the font family used for STRING's CJK characters.
Fall back to the family of a sample CJK glyph when STRING has none."
  (if-let* ((letter (ekp-get-cjk-letter string)))
      (ekp-font-family letter)
    (ekp-font-family "牛")))

(defun ekp-pixel-spacing (pixel)
  "Return a pixel spacing with a PIXEL pixel width."
  (if (= pixel 0)
      ""
    (propertize " " 'display `(space :width (,pixel)))))

(defun ekp-cjk-fw-punct-p (str)
  "Return non-nil if STR begins with a CJK full-width punctuation char.
Full-width alphanumerics (ＡＢＣ, １２３) are NOT punctuation."
  (let ((char (seq-first str)))
    (and
     ;; Exclude fullwidth Latin letters and digits (FF10-FF19,
     ;; FF21-FF3A, FF41-FF5A): they are content, not punctuation.
     (not (or (and (>= char #xFF10) (<= char #xFF19))
              (and (>= char #xFF21) (<= char #xFF3A))
              (and (>= char #xFF41) (<= char #xFF5A))))
     (or (equal (char-syntax char) ?.)
         (and (>= char #x3000) (<= char #x303F))
         (and (>= char #xFF00) (<= char #xFF60))))))

(defun ekp-cjk-opening-punct-p (str)
  "Return non-nil if STR ends with a CJK opening punctuation.
These characters must not appear at the end of a line (kinsoku rule).
When STR is held as cjk-char, this checks if it still needs attachment."
  (let ((char (aref str (1- (length str)))))
    (memq (get-char-code-property char 'general-category)
          '(Ps Pi))))

(defun ekp--flush-latin-word (parts boxes)
  "Join reversed Latin PARTS once and push the word to BOXES."
  (if parts (cons (apply #'concat (nreverse parts)) boxes) boxes))

(defun ekp--flush-cjk-char (parts boxes)
  "Join reversed CJK PARTS once and push the character to BOXES."
  (if parts (cons (apply #'concat (nreverse parts)) boxes) boxes))

(defun ekp--flush-spaces (parts boxes prev-state next-width)
  "Join reversed space PARTS and push them to BOXES based on context.
PREV-STATE: 1=latin, 2=CJK (previous content type).
NEXT-WIDTH: width of next character (1=latin, 2=CJK).
Rules:
- Leading spaces (boxes is nil): preserve all spaces
- CJK involved (prev or next is CJK): preserve all spaces
- Latin-Latin with single space: let glue handle it
- Latin-Latin with multiple spaces: preserve all but last"
  (when parts
    (let ((spaces (apply #'concat (nreverse parts)))
          (cjk-involved (or (= prev-state 2) (= next-width 2))))
      (cond
       ;; Leading spaces (no previous boxes): preserve all
       ((null boxes)
        (setq boxes (cons spaces boxes)))
       ;; CJK involved: preserve all spaces
       (cjk-involved
        (setq boxes (cons spaces boxes)))
       ;; Inside a no-break span: spacing is literal, glue would
       ;; stretch — preserve the run as a rigid space box.
       ((or (text-property-not-all
             0 (length spaces) 'ekp-no-break nil spaces)
            (text-property-not-all
             0 (length spaces) 'ekp--literal-spacing nil spaces))
        (setq boxes (cons spaces boxes)))
       ;; Latin-Latin with multiple spaces: preserve all but last
       ((> (length spaces) 1)
        (setq boxes (cons (substring spaces 0 -1) boxes)))
       ;; Latin-Latin with single space: let glue handle it
       (t nil))))
  boxes)

(defun ekp--flush-trailing-spaces (parts boxes)
  "Join reversed trailing space PARTS once and push them to BOXES."
  (if parts
      (cons (apply #'concat (nreverse parts)) boxes)
    boxes))

(defun ekp--zero-width-attaching-p (char)
  "Return non-nil if zero-width CHAR must attach to the preceding text.
Combining marks (Mn/Mc/Me), ZWJ/ZWNJ, CGJ, variation selectors and
the word joiner attach to the previous character; other zero-width
characters (such as zero-width space U+200B) are treated as
invisible break points."
  (or (memq (get-char-code-property char 'general-category) '(Mn Mc Me))
      (memq char '(#x200C #x200D #x034F #x2060 #xFEFF))
      (and (>= char #xFE00) (<= char #xFE0F))))

(defun ekp--handle-latin-char (str state latin-word cjk-char boxes)
  "Handle a latin (width=1) character STR.
STATE is the current mode; LATIN-WORD, CJK-CHAR and BOXES are the
accumulators.  Return (new-state new-latin-word new-cjk-char new-boxes)."
  (if (= state 1)
      ;; Already in latin mode: accumulate
      (list 1 (cons str latin-word) nil boxes)
    ;; Was in CJK mode: flush held CJK char, switch to latin
    (list 1 (list str) nil (ekp--flush-cjk-char cjk-char boxes))))

(defun ekp--handle-cjk-char (str state latin-word cjk-char boxes)
  "Handle a CJK (width=2) character STR.
STATE is the current mode; LATIN-WORD, CJK-CHAR and BOXES are the
accumulators.  Return (new-state new-latin-word new-cjk-char new-boxes).

Every CJK character — punctuation included — becomes its own box.
Kinsoku is enforced by the DP through per-gap break permissions
\(`ekp-para-breaks-allowed'), not by merging boxes."
  (if (= state 1)
      ;; Was in latin mode: flush latin word, hold current CJK char
      (list 2 nil (list str) (ekp--flush-latin-word latin-word boxes))
    ;; Already in CJK mode: flush held char, hold current
    (list 2 nil (list str) (ekp--flush-cjk-char cjk-char boxes))))

(defun ekp-split-to-boxes (string)
  "Split STRING into typographic boxes.
Latin words become single boxes; CJK chars — punctuation included —
are individual boxes.  Whitespace runs are preserved as separate
boxes.  Kinsoku is enforced later via per-gap break permissions, not
by merging boxes."
  (if (string-blank-p string)
      (vector string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((state (char-width (seq-first string)))  ; 1=latin, 2=CJK
            (prev-state 1)  ; track previous content state for space handling
            latin-word   ; reversed fragments for latin characters
            cjk-char     ; reversed fragments for one CJK character
            spaces       ; reversed fragments for a whitespace run
            boxes)       ; result list (built in reverse)
        (while (not (eobp))
          (let* ((str (buffer-substring (point) (1+ (point))))
                 (char (string-to-char str))
                 (width (string-width str)))
            (cond
             ;; Zero-width combining/joining chars: attach to preceding text
             ((and (= 0 width) (not (string-blank-p str))
                   (ekp--zero-width-attaching-p char))
              (cond
               (latin-word (push str latin-word))
               (cjk-char (push str cjk-char))
               (spaces (push str spaces))
               (boxes
                (if (= prev-state 2)
                    (setq cjk-char (list str (pop boxes)) state 2)
                  (setq latin-word (list str (pop boxes)) state 1)))
               ;; String starts with a combining char: start an accumulator
               (t (setq latin-word (list str) state 1))))
             ;; Whitespace or other zero-width: flush content, accumulate spaces
             ((or (string-blank-p str) (= 0 width))
              (setq boxes (ekp--flush-cjk-char cjk-char boxes))
              (when cjk-char (setq prev-state 2))
              (setq cjk-char nil)
              (setq boxes (ekp--flush-latin-word latin-word boxes))
              (when latin-word (setq prev-state 1))
              (setq latin-word nil)
              (push str spaces))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ekp--module-reload (module)
  "Load MODULE from a temp copy to allow rebuilding."
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module))))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

;;; C Module Support
;; Parallel C implementation using pthreads

;; Defined by the dynamic module (ekp_c/ekp.dylib | .so | .dll)
(declare-function ekp-c-init "ext:ekp")
(declare-function ekp-c-version "ext:ekp")
(declare-function ekp-c-thread-count "ext:ekp")

(defvar ekp-c-module-loaded nil
  "Non-nil if C module is loaded.")

(defun ekp-c-module-dir ()
  "Return the C module directory."
  (when-let* ((root-dir (ekp-root-dir)))
    (expand-file-name "ekp_c" root-dir)))

(defun ekp-c-module-file ()
  "Return path to compiled C module."
  (when-let* ((module-dir (ekp-c-module-dir))
              (filename (cond ((eq system-type 'darwin) "ekp.dylib")
                              ((eq system-type 'windows-nt) "ekp.dll")
                              (t "ekp.so"))))
    (expand-file-name filename module-dir)))

(defalias 'ekp-c-module-reload #'ekp--module-reload
  "Load MODULE from a temp copy to allow rebuilding.")

(defconst ekp-c-module-required-version "1.6"
  "Minimum C module version compatible with this Elisp code.")

(defun ekp--c-build-finished (profile process _event)
  "Handle completion of PROCESS building C PROFILE."
  (when (memq (process-status process) '(exit signal))
    (if (and (eq (process-status process) 'exit)
             (= (process-exit-status process) 0))
        (condition-case error-data
            (progn
              (ekp-c-module-load)
              (kill-buffer (process-buffer process))
              (message "EKP C %s build succeeded" profile))
          (error
           (display-buffer (process-buffer process))
           (message "EKP C build loaded unsuccessfully: %s"
                    (error-message-string error-data))))
      (display-buffer (process-buffer process))
      (message "EKP C %s build failed (status %d)"
               profile (process-exit-status process)))))

;;;###autoload
(defun ekp-c-module-load ()
  "Load EKP C module if available.
Refuses to enable a module older than
`ekp-c-module-required-version' (rebuild with make)."
  (interactive)
  (let ((file (ekp-c-module-file)))
    (if (and file (file-exists-p file))
        (progn
          (ekp-c-module-reload file)
          (when (fboundp 'ekp-c-init)
            (ekp-c-init)
            (if (version< (ekp-c-version) ekp-c-module-required-version)
                (progn
                  (setq ekp-c-module-loaded nil)
                  (message "ekp-c module version %s is too old (need %s+). \
Run 'make' in ekp_c/ to rebuild; falling back to Elisp."
                           (ekp-c-version) ekp-c-module-required-version))
              (setq ekp-c-module-loaded t)
              (message "ekp-c module loaded (version %s, %d threads)"
                       (ekp-c-version) (ekp-c-thread-count)))))
      (message "C module not found. Run 'make' in ekp_c/ directory."))))

;;;###autoload
(defun ekp-c-module-build (&optional profile)
  "Build the C module with make using PROFILE.
PROFILE is one of `portable', `native', `debug', or `sanitize';
the default is `portable'."
  (interactive
   (list
    (intern
     (completing-read "C build profile: "
                      '("portable" "native" "debug" "sanitize")
                      nil t nil nil "portable"))))
  (setq profile (or profile 'portable))
  (unless (memq profile '(portable native debug sanitize))
    (user-error "Unknown EKP C build profile: %S" profile))
  (let* ((module-dir (ekp-c-module-dir))
         (makefile (and module-dir
                        (expand-file-name "Makefile" module-dir)))
         (make (executable-find "make")))
    (unless (and makefile (file-exists-p makefile))
      (user-error "Makefile not found in ekp_c/ directory"))
    (unless make
      (user-error "The make executable is not available"))
    (let* ((default-directory (file-name-as-directory module-dir))
           (buffer (generate-new-buffer "*ekp-c-build*")))
      (make-process
       :name (generate-new-buffer-name "ekp-c-build")
       :buffer buffer
       :command (list make (format "PROFILE=%s" profile))
       :noquery t
       :sentinel (apply-partially #'ekp--c-build-finished profile)))))

(provide 'ekp-utils)

;;; ekp-utils.el ends here
