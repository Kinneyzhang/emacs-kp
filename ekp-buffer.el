;;; ekp-buffer.el --- Non-mutating buffer layout for ekp -*- lexical-binding: t; -*-

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

;; `ekp.el' computes semantic KP layout plans.  This module projects those
;; plans onto existing source characters with text properties only:
;;
;; - `space-width' plus `min-width' renders exact ASCII-space glue;
;; - `min-width' pads zero-source CJK and mixed gaps;
;; - `line-prefix' renders indentation and leading alignment;
;; - replacing display strings render chosen breaks and discretionary
;;   hyphens without inserting characters.
;;
;; The logical character stream never changes.  EKP creates no overlays.

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'ekp)

(defvar ekp-auto-justify-mode)

(defgroup ekp-buffer nil
  "Non-mutating buffer layout built on ekp."
  :group 'ekp
  :prefix "ekp-")

(defcustom ekp-buffer-margin-pixel 2
  "Pixels reserved inside the window body width."
  :type 'natnum)

(defcustom ekp-auto-justify-resize-delay 0.15
  "Seconds to debounce window resize reflows."
  :type 'number)

(defcustom ekp-auto-justify-composition-delay 0.05
  "Seconds before retrying a live layout deferred by IME composition."
  :type 'number)

(defcustom ekp-auto-justify-native-append t
  "Use the loaded native module for automatic live append DP.
This affects only the already prepared live append path.  Explicit string
layout and full buffer layout still obey `ekp-use-c-module' directly; when
the native module is unavailable, live append falls back to Elisp."
  :type 'boolean
  :group 'ekp-buffer)

(defcustom ekp-auto-justify-paragraph-limit 2048
  "Maximum hard-paragraph characters planned automatically.
Longer paragraphs stay natural so enabling the mode, pasting, and
ordinary editing cannot enter an unbounded paragraph-wide planning
operation.  `ekp-refill-paragraph' remains the explicit full-quality
command and is not limited by this value."
  :type 'natnum)

(defcustom ekp-auto-justify-lazy-threshold 20000
  "Buffer size beyond which whole-buffer reflows run visible-first."
  :type 'natnum)

(defcustom ekp-auto-justify-chunk-size 10
  "Hard paragraphs processed in one lazy reflow chunk."
  :type 'natnum)

(defcustom ekp-auto-justify-tick-budget 0.005
  "Seconds of work allowed in one lazy reflow tick."
  :type 'number)

(defconst ekp-buffer-org-block-faces
  '(org-block org-block-begin-line org-block-end-line
    org-table org-meta-line)
  "Org block faces whose paragraphs stay verbatim.")

(defconst ekp-buffer-org-inline-faces
  '(org-code org-verbatim)
  "Org inline faces that receive automatic inline break policy.")

(defconst ekp-buffer-org-skip-faces ekp-buffer-org-block-faces
  "Reasonable `ekp-buffer-skip-faces' preset for Org buffers.")

(defconst ekp-buffer-markdown-block-faces
  '(markdown-code-face markdown-pre-face markdown-table-face)
  "Markdown block faces whose paragraphs stay verbatim.")

(defconst ekp-buffer-markdown-inline-faces
  '(markdown-inline-code-face)
  "Markdown inline faces that receive automatic inline break policy.")

(defconst ekp-buffer-markdown-skip-faces ekp-buffer-markdown-block-faces
  "Reasonable `ekp-buffer-skip-faces' preset for Markdown buffers.")

(defun ekp-buffer--safe-symbol-list-p (value)
  "Return non-nil when VALUE is a closed list of symbols."
  (and (proper-list-p value) (seq-every-p #'symbolp value)))

(defun ekp-buffer--safe-measure-p (value)
  "Return non-nil when VALUE is a documented measure mode."
  (or (eq value 'narrowest-window)
      (and (integerp value) (> value 0))
      (and (consp value) (eq (car value) 'max)
           (integerp (cdr value)) (> (cdr value) 0))))

(defun ekp-buffer--safe-mode-policy-entry-p (entry)
  "Return non-nil when ENTRY is a closed mode profile entry."
  (and (consp entry)
       (symbolp (car entry))
       (proper-list-p (cdr entry))
       (seq-every-p
        (lambda (setting)
          (and (consp setting)
               (pcase (car setting)
                 ('ekp-inline-code-policy
                  (ekp--safe-break-policy-value-p (cdr setting)))
                 ('ekp-hyphenation
                  (ekp--safe-hyphenation-value-p (cdr setting)))
                 ('ekp-overlong-token-policy
                  (memq (cdr setting) '(emergency overflow natural)))
                 ('ekp-emergency-stretch-pixel
                  (ekp--safe-emergency-stretch-pixel-p (cdr setting)))
                 ('ekp-kinsoku-profile
                  (memq (cdr setting) '(common zh ja off custom)))
                 ('ekp-cjk-no-line-start-extra
                  (stringp (cdr setting)))
                 ('ekp-cjk-no-line-end-extra
                  (stringp (cdr setting)))
                 ('ekp-number-unit-suffixes
                  (and (proper-list-p (cdr setting))
                       (seq-every-p #'stringp (cdr setting))))
                 ('ekp-token-break-policies
                  (ekp--safe-token-break-policies-p (cdr setting)))
                 ('ekp-buffer-skip-faces
                  (ekp-buffer--safe-symbol-list-p (cdr setting)))
                 ('ekp-buffer-inline-faces
                  (ekp-buffer--safe-symbol-list-p (cdr setting)))
                 ('ekp-buffer-measure
                  (ekp-buffer--safe-measure-p (cdr setting)))
                 (_ nil))))
        (cdr entry))))

(defun ekp-buffer--safe-mode-policy-alist-p (value)
  "Return non-nil when VALUE is a safe mode policy alist."
  (and (proper-list-p value)
       (seq-every-p #'ekp-buffer--safe-mode-policy-entry-p value)))

(defcustom ekp-buffer-skip-faces nil
  "Faces whose paragraphs stay verbatim."
  :type '(repeat face)
  :safe #'ekp-buffer--safe-symbol-list-p)

(defcustom ekp-buffer-inline-faces nil
  "Faces whose exact spans use `ekp-inline-code-policy'."
  :type '(repeat face)
  :safe #'ekp-buffer--safe-symbol-list-p)

(defcustom ekp-buffer-mode-policy-alist
  `((org-mode . ((ekp-buffer-skip-faces . ,ekp-buffer-org-block-faces)
                 (ekp-buffer-inline-faces . ,ekp-buffer-org-inline-faces)))
    (markdown-mode
     . ((ekp-buffer-skip-faces . ,ekp-buffer-markdown-block-faces)
        (ekp-buffer-inline-faces . ,ekp-buffer-markdown-inline-faces))))
  "Mode profiles consulted by automatic and manual buffer layout."
  :type '(alist :key-type symbol :value-type sexp)
  :safe #'ekp-buffer--safe-mode-policy-alist-p)

(defcustom ekp-buffer-measure 'narrowest-window
  "Measure used for automatic and default manual buffer layout.
`narrowest-window' uses the narrowest live window displaying the
buffer.  A positive integer is a fixed pixel width.  `(max . N)'
caps the narrowest live window at N pixels."
  :type '(choice (const narrowest-window) (integer :tag "Fixed pixels")
                 (cons (const max) (integer :tag "Maximum pixels")))
  :safe #'ekp-buffer--safe-measure-p)

(defvar-local ekp-buffer-skip-predicate nil
  "Function called with a paragraph string that should stay verbatim.")

(defvar ekp-buffer--inhibit nil
  "Non-nil while EKP changes projection properties.")

(defvar ekp-buffer--filtering nil
  "Non-nil while EKP delegates substring extraction to a prior filter.")

(defvar ekp-buffer--automatic-pass nil
  "Non-nil while an automatic operation lays out stable paragraphs.")

(defvar-local ekp-buffer--previous-filter nil)
(defvar-local ekp-buffer--previous-filter-local-p nil)
(defvar-local ekp-buffer--filter-installed nil)
(defvar-local ekp-buffer--auto-width nil)
(defvar-local ekp-buffer--resize-timer nil)
(defvar-local ekp-buffer--composition-timer nil)
(defvar-local ekp-buffer--policy-reflow-timer nil)
(defvar-local ekp-buffer--policy-reflow-context nil)
(defvar-local ekp-buffer--policy-context-snapshot nil)
(defvar-local ekp-buffer--pending nil)
(defvar-local ekp-buffer--chunk-timer nil)
(defvar-local ekp-buffer--generation 0)
(defvar-local ekp-buffer--live-edit nil)
(defvar-local ekp-buffer--live-state nil)
(defvar-local ekp-buffer--live-plan-cache nil)
(defvar-local ekp-buffer--wrap-state nil
  "Prior soft-wrap variable values and local-binding ownership.")
(defvar-local ekp-buffer--spans nil
  "Installed projections as `ekp-buffer--span' records.")
(defvar-local ekp-buffer--conflicts nil
  "Most recent skipped paragraph conflicts as (BEG END REASON).")

(cl-defstruct (ekp-buffer--span (:constructor ekp-buffer--span-create))
  beg end width plan lines)

(cl-defstruct
    (ekp-buffer--projected-line
     (:constructor ekp-buffer--projected-line-create))
  base beg end break-end line)

(cl-defstruct
    (ekp-buffer--live-edit (:constructor ekp-buffer--live-edit-create))
  old-beg old-fragment
  baseline-source baseline-plan baseline-signatures baseline-spans
  baseline-key baseline-active-index baseline-prefix-end baseline-markers
  dirty-beg dirty-end dirty-start dirty-finish row-start edit-end)

(cl-defstruct
    (ekp-buffer--live-state (:constructor ekp-buffer--live-state-create))
  beg end width source key plan signatures active-index prefix-end spans)

(defconst ekp-buffer--owned-properties
  '(ekp-justified ekp-buffer--display ekp-buffer--line-prefix)
  "Text properties that identify EKP's buffer projection.")

(defconst ekp-buffer--non-ascii-whitespace-regexp
  "[\t\n\r\v\f\u0085\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]"
  "Unicode whitespace that cannot use the ASCII-space projection path.")

(dolist (property ekp-buffer--owned-properties)
  (setf (alist-get property text-property-default-nonsticky) t))

;;; Setup

(defconst ekp-buffer--policy-variables
  '(ekp-inline-code-policy ekp-hyphenation ekp-token-break-policies
    ekp-number-unit-suffixes
    ekp-overlong-token-policy ekp-kinsoku-profile
    ekp-emergency-stretch-pixel
    ekp-cjk-no-line-start-extra ekp-cjk-no-line-end-extra
    ekp-buffer-skip-faces ekp-buffer-inline-faces
    ekp-buffer-mode-policy-alist ekp-buffer-measure)
  "Variables that affect buffer layout policy or measure.")

(defun ekp-buffer--matching-mode-profile ()
  "Return the first matching mode policy profile for the current buffer."
  (seq-some
   (lambda (entry)
     (and (derived-mode-p (car entry))
          (cdr entry)))
   ekp-buffer-mode-policy-alist))

(defun ekp-buffer--profile-value (profile variable)
  "Return VARIABLE's value from PROFILE, or nil if absent."
  (assq variable profile))

(defun ekp-buffer--effective-scalar (profile variable)
  "Return VARIABLE for PROFILE after local, mode-profile, then global precedence."
  (if (local-variable-p variable)
      (symbol-value variable)
    (let ((current (symbol-value variable))
          (default (default-value variable))
          (profile-value (ekp-buffer--profile-value profile variable)))
      (cond
       ((not (equal current default)) current)
       (profile-value (cdr profile-value))
       (t default)))))

(defun ekp-buffer--merge-token-policies (profile)
  "Return effective token policies after merging global, PROFILE, and local."
  (let ((merged (copy-tree (default-value 'ekp-token-break-policies)))
        (current ekp-token-break-policies)
        (default (default-value 'ekp-token-break-policies)))
    (dolist (entry (cdr (ekp-buffer--profile-value
                         profile 'ekp-token-break-policies)))
      (setf (alist-get (car entry) merged) (cdr entry)))
    (when (or (local-variable-p 'ekp-token-break-policies)
              (not (equal current default)))
      (dolist (entry current)
        (setf (alist-get (car entry) merged) (cdr entry))))
    merged))

(defun ekp-buffer--policy-context (&optional width)
  "Return the effective policy context for the current buffer at WIDTH."
  (let* ((profile (ekp-buffer--matching-mode-profile))
         (emergency-stretch
          (ekp-buffer--effective-scalar
           profile 'ekp-emergency-stretch-pixel))
         (resolved-emergency-stretch
          (let ((ekp-emergency-stretch-pixel emergency-stretch))
            (ekp--resolved-emergency-stretch-pixel))))
    (ekp--copy-layout-context-value
     (list :inline-code-policy
           (ekp-buffer--effective-scalar profile 'ekp-inline-code-policy)
           :hyphenation
           (ekp-buffer--effective-scalar profile 'ekp-hyphenation)
           :token-break-policies (ekp-buffer--merge-token-policies profile)
           :number-unit-suffixes
           (ekp-buffer--effective-scalar profile 'ekp-number-unit-suffixes)
           :overlong-token-policy
           (ekp-buffer--effective-scalar profile 'ekp-overlong-token-policy)
           :emergency-stretch-pixel resolved-emergency-stretch
           :kinsoku-profile
           (ekp-buffer--effective-scalar profile 'ekp-kinsoku-profile)
           :cjk-no-line-start-extra
           (ekp-buffer--effective-scalar
            profile 'ekp-cjk-no-line-start-extra)
           :cjk-no-line-end-extra
           (ekp-buffer--effective-scalar profile 'ekp-cjk-no-line-end-extra)
           :block-faces
           (ekp-buffer--effective-scalar profile 'ekp-buffer-skip-faces)
           :inline-faces
           (ekp-buffer--effective-scalar profile 'ekp-buffer-inline-faces)
           :measure
           (ekp-buffer--effective-scalar profile 'ekp-buffer-measure)
           :width width))))

(defmacro ekp-buffer--with-policy-context (context &rest body)
  "Run BODY with CONTEXT bound as EKP's core policy environment."
  (declare (indent 1))
  `(let ((ekp-inline-code-policy
          (plist-get ,context :inline-code-policy))
         (ekp-hyphenation (plist-get ,context :hyphenation))
         (ekp-token-break-policies
          (plist-get ,context :token-break-policies))
         (ekp-number-unit-suffixes
          (plist-get ,context :number-unit-suffixes))
         (ekp-overlong-token-policy
          (plist-get ,context :overlong-token-policy))
         (ekp-emergency-stretch-pixel
          (plist-get ,context :emergency-stretch-pixel))
         (ekp-kinsoku-profile (plist-get ,context :kinsoku-profile))
         (ekp-cjk-no-line-start-extra
          (plist-get ,context :cjk-no-line-start-extra))
         (ekp-cjk-no-line-end-extra
          (plist-get ,context :cjk-no-line-end-extra)))
     ,@body))

(defun ekp-buffer--face-member-p (face faces)
  "Return non-nil when FACE intersects FACES."
  (if (listp face)
      (seq-intersection face faces)
    (memq face faces)))

;;;###autoload
(defun ekp-org-setup ()
  "Protect common Org structural faces in the current buffer."
  (setq-local ekp-buffer-skip-faces ekp-buffer-org-block-faces)
  (setq-local ekp-buffer-inline-faces ekp-buffer-org-inline-faces))

;;;###autoload
(defun ekp-markdown-setup ()
  "Protect common Markdown code faces in the current buffer."
  (setq-local ekp-buffer-skip-faces ekp-buffer-markdown-block-faces)
  (setq-local ekp-buffer-inline-faces ekp-buffer-markdown-inline-faces))

;;; Width

(defun ekp-buffer--protrusion-reserve ()
  "Return the right-edge pixel reserve for hanging punctuation."
  (if ekp-protrusion
      (max 2 (ceiling (* (alist-get 'cjk-close ekp-protrusion-ratios 0.5)
                         (ekp--measured-width "。"))))
    0))

(defun ekp-buffer--indicator-reserve (&optional window)
  "Return pixels consumed by the continuation indicator in WINDOW."
  (let ((win (or window (selected-window))))
    (if (and (display-graphic-p (window-frame win))
             (> (or (cadr (window-fringes win)) 0) 0))
        0
      (frame-char-width (window-frame win)))))

(defun ekp-buffer--window-pixel (&optional window)
  "Return usable text width in pixels for WINDOW."
  (max 1 (- (window-body-width window t)
            ekp-buffer-margin-pixel
            (ekp-buffer--indicator-reserve window)
            (ekp-buffer--protrusion-reserve))))

(defun ekp-buffer--authoritative-window ()
  "Return the narrowest live window displaying the current buffer."
  (car
   (sort (seq-filter
          #'window-live-p
          (get-buffer-window-list (current-buffer) nil t))
         (lambda (left right)
           (< (ekp-buffer--window-pixel left)
              (ekp-buffer--window-pixel right))))))

(defun ekp-buffer--native-row-start (position)
  "Return native visual-row start containing POSITION, or nil in batch."
  (when-let* ((window (ekp-buffer--authoritative-window)))
    (save-excursion
      (goto-char position)
      (vertical-motion 0 window)
      (point))))

(defun ekp-buffer--effective-width (&optional buffer)
  "Return BUFFER's effective `ekp-buffer-measure' width.
The default uses the narrowest live window; fixed widths and `(max . N)'
come from the active buffer policy."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((windows (get-buffer-window-list (current-buffer) nil t))
           (narrowest (if windows
                          (apply #'min
                                 (mapcar #'ekp-buffer--window-pixel windows))
                        (ekp-buffer--window-pixel)))
           (measure (plist-get (ekp-buffer--policy-context) :measure)))
      (cond
       ((eq measure 'narrowest-window) narrowest)
       ((and (integerp measure) (> measure 0)) measure)
       ((and (consp measure) (eq (car measure) 'max)
             (integerp (cdr measure)) (> (cdr measure) 0))
        (min narrowest (cdr measure)))
       (t (error "Invalid ekp-buffer-measure: %S" measure))))))

(defun ekp-buffer--measure-report (&optional buffer)
  "Return measure diagnostics for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((windows (get-buffer-window-list (current-buffer) nil t))
           (narrowest (if windows
                          (apply #'min
                                 (mapcar #'ekp-buffer--window-pixel windows))
                        (ekp-buffer--window-pixel)))
           (context (ekp-buffer--policy-context))
           (requested (plist-get context :measure))
           (effective (ekp-buffer--effective-width)))
      (list :requested requested
            :narrowest narrowest
            :effective effective
            :overflow-risk (and (integerp effective)
                                (> effective narrowest))))))

;;; Paragraphs and conflicts

(defun ekp-buffer--face-hit-p (string faces)
  "Return non-nil when STRING carries one of FACES."
  (let ((pos 0) (length (length string)) hit)
    (while (and (< pos length) (not hit))
      (let ((face (get-text-property pos 'face string)))
        (setq hit (ekp-buffer--face-member-p face faces))
        (setq pos (or (next-single-property-change
                       pos 'face string length)
                      length))))
    hit))

(defun ekp-buffer--skip-paragraph-p (paragraph &optional context)
  "Return non-nil when PARAGRAPH must stay verbatim under CONTEXT."
  (let* ((context (or context (ekp-buffer--policy-context)))
         (faces (plist-get context :block-faces)))
    (or (string-blank-p paragraph)
        (text-property-not-all 0 (length paragraph)
                               'ekp-verbatim nil paragraph)
        (text-property-not-all 0 (length paragraph) 'field nil paragraph)
        (text-property-not-all 0 (length paragraph) 'read-only nil paragraph)
        (and faces (ekp-buffer--face-hit-p paragraph faces))
        (and ekp-buffer-skip-predicate
             (funcall ekp-buffer-skip-predicate paragraph)))))

(defun ekp-buffer--foreign-property-at-p (position property)
  "Return non-nil when PROPERTY at POSITION is not owned by EKP."
  (let ((value (get-text-property position property)))
    (and value
         (pcase property
           ('display
            (not (eq value
                     (get-text-property
                      position 'ekp-buffer--display))))
           ('line-prefix
            (not (eq value
                     (get-text-property
                      position 'ekp-buffer--line-prefix))))
           (_ t)))))

(defun ekp-buffer--foreign-property (beg end)
  "Return the first uncomposable foreign property in BEG through END."
  (seq-find
   (lambda (property)
     (let ((position beg)
           found)
       (while (and (< position end) (not found))
         (setq found
               (ekp-buffer--foreign-property-at-p position property)
               position
               (or (next-single-property-change
                    position property nil end)
                   end)))
       found))
   '(display line-prefix wrap-prefix composition invisible)))

(defun ekp-buffer--ascii-space-range-p (beg end)
  "Return non-nil when BEG through END contain only ASCII spaces."
  (and (< beg end)
       (let ((position beg))
         (while (and (< position end)
                     (= (char-after position) ?\s))
           (setq position (1+ position)))
         (= position end))))

(defun ekp-buffer--unsupported-gap (plan _base)
  "Return the first gap in PLAN that cannot be projected."
  (let ((string (ekp-layout-plan-string plan)))
    (when (string-match-p ekp-buffer--non-ascii-whitespace-regexp string)
      (catch 'unsupported
        (cl-loop
         for line across (ekp-layout-plan-lines plan)
         do
         (cl-loop
          for gap across (ekp-layout-line-gaps line)
          for start = (ekp-layout-gap-source-start gap)
          for end = (ekp-layout-gap-source-end gap)
          when (and (< start end)
                    (< (ekp-layout-gap-target-pixel gap)
                       (ekp-layout-gap-natural-pixel gap)))
          do
          (while (and (< start end) (= (aref string start) ?\s))
            (setq start (1+ start)))
          (when (< start end)
            (throw 'unsupported gap))))))))

(defun ekp-buffer--record-conflict (beg end reason)
  "Record that BEG through END stayed verbatim because of REASON."
  (push (list beg end reason) ekp-buffer--conflicts))

(defun ekp-buffer--projectable-p (plan base beg end)
  "Return non-nil when PLAN can be installed at BASE from BEG to END."
  (if-let* ((property (ekp-buffer--foreign-property beg end)))
      (progn
        (ekp-buffer--record-conflict beg end
                                     (format "foreign `%s' property" property))
        nil)
    (if (ekp-buffer--unsupported-gap plan base)
        (progn
          (ekp-buffer--record-conflict
           beg end "non-ASCII whitespace would require shrinking")
          nil)
      t)))

;;; Owned property operations

(defun ekp-buffer--put-owned
    (beg end property owner value)
  "Put PROPERTY VALUE on BEG through END and record it in OWNER."
  (when (< beg end)
    (add-text-properties beg end (list property value owner value))))

(defun ekp-buffer--remove-owned (beg end property owner)
  "Remove EKP-owned PROPERTY spans identified by OWNER from BEG to END."
  (let ((pos beg))
    (while (< pos end)
      (let* ((value (get-text-property pos owner))
             (next (or (next-single-property-change pos owner nil end) end)))
        (when value
          (remove-text-properties
           pos next
           (if (eq (get-text-property pos property) value)
               (list property nil owner nil)
             (list owner nil))))
        (setq pos next)))))

(defun ekp-buffer--put-display (beg end value)
  "Install EKP-owned display VALUE from BEG to END."
  (ekp-buffer--put-owned beg end 'display 'ekp-buffer--display value))

(defun ekp-buffer--put-line-prefix (beg end value)
  "Install EKP-owned line-prefix VALUE from BEG to END."
  (ekp-buffer--put-owned
   beg end 'line-prefix 'ekp-buffer--line-prefix value))

(defun ekp-buffer--remove-properties (beg end)
  "Remove only EKP-owned projection properties from BEG to END."
  (ekp-buffer--remove-owned beg end 'display 'ekp-buffer--display)
  (ekp-buffer--remove-owned
   beg end 'line-prefix 'ekp-buffer--line-prefix)
  (remove-text-properties beg end '(ekp-justified nil)))

;;; Display specifications

(defun ekp-buffer--min-width (pixel)
  "Return an independently identified absolute PIXEL `min-width' spec."
  (list 'min-width (list (list pixel))))

(defun ekp-buffer--space-display (natural target)
  "Return exact ASCII-space display for NATURAL and TARGET pixels."
  (if (= target 0)
      (make-string 0 0)
    (list (list 'space-width (/ (float target) (max 1 natural)))
          (ekp-buffer--min-width target))))

(defun ekp-buffer--prefix-display (pixel)
  "Return a display-only line prefix of PIXEL width."
  (propertize " " 'display `(space :width (,pixel))))

(defun ekp-buffer--tail-grapheme-range (plan box-index base)
  "Return buffer range of BOX-INDEX's final grapheme in PLAN at BASE."
  (let* ((offset (aref (ekp-layout-plan-offsets plan) box-index))
         (box (aref (ekp-layout-plan-boxes plan) box-index))
         (glyphs (string-glyph-split box))
         (length (length (car (last glyphs))))
         (end (+ base (cdr offset))))
    (cons (- end length) end)))

(defun ekp-buffer--clean-display-copy (beg end)
  "Return BEG through END for use inside a replacing display string."
  (let ((copy (buffer-substring beg end)))
    (remove-text-properties
     0 (length copy)
     '(display nil line-prefix nil wrap-prefix nil
       ekp-justified nil ekp-buffer--display nil
       ekp-buffer--line-prefix nil)
     copy)
    copy))

(defun ekp-buffer--break-display (beg end hyphen-p)
  "Return a display string for source grapheme BEG through END.
HYPHEN-P adds a discretionary hyphen before the visual newline."
  (let* ((grapheme (ekp-buffer--clean-display-copy beg end))
         (props (and (> (length grapheme) 0)
                     (text-properties-at (1- (length grapheme)) grapheme)))
         (hyphen (if hyphen-p (apply #'propertize "-" props) "")))
    (when (> (length grapheme) 0)
      (put-text-property 0 (length grapheme) 'cursor 1 grapheme))
    (concat grapheme hyphen "\n")))

;;; Plan projection

(defun ekp-buffer--project-source-gap (gap base)
  "Project a source-backed GAP at BASE."
  (let* ((beg (+ base (ekp-layout-gap-source-start gap)))
         (end (+ base (ekp-layout-gap-source-end gap)))
         (target (ekp-layout-gap-target-pixel gap))
         (natural (ekp-layout-gap-natural-pixel gap)))
    (cond
     ((ekp-buffer--ascii-space-range-p beg end)
      (ekp-buffer--put-display
       beg end (ekp-buffer--space-display natural target)))
     ((= target 0)
      (ekp-buffer--put-display beg end (make-string 0 0)))
     ((> target natural)
      (ekp-buffer--put-display
       beg end (ekp-buffer--min-width target))))))

(defun ekp-buffer--project-zero-source-gap (gap plan base)
  "Project a zero-source GAP from PLAN at BASE."
  (let ((target (ekp-layout-gap-target-pixel gap)))
    (when (> target 0)
      (pcase-let ((`(,beg . ,end)
                   (ekp-buffer--tail-grapheme-range
                    plan (ekp-layout-gap-left-box gap) base)))
        (ekp-buffer--put-display
         beg end
         (ekp-buffer--min-width
          (+ (ekp-layout-gap-natural-pixel gap) target)))))))

(defun ekp-buffer--project-gap (gap plan base)
  "Project GAP from PLAN at buffer BASE."
  (if (< (ekp-layout-gap-source-start gap)
         (ekp-layout-gap-source-end gap))
      (ekp-buffer--project-source-gap gap base)
    (ekp-buffer--project-zero-source-gap gap plan base)))

(defun ekp-buffer--project-break-space (beg end)
  "Project a chosen visual break over source characters BEG through END."
  (ekp-buffer--put-display beg (1+ beg) (copy-sequence "\n"))
  (when (< (1+ beg) end)
    (ekp-buffer--put-display (1+ beg) end (make-string 0 0))))

(defun ekp-buffer--project-break (line plan base)
  "Project LINE's chosen break from PLAN at BASE."
  (when (ekp-layout-line-break-kind line)
    (let ((beg (+ base (ekp-layout-line-break-source-start line)))
          (end (+ base (ekp-layout-line-break-source-end line))))
      (if (< beg end)
          (ekp-buffer--project-break-space beg end)
        (pcase-let ((`(,owner-beg . ,owner-end)
                     (ekp-buffer--tail-grapheme-range
                      plan (1- (ekp-layout-line-box-end line)) base)))
          (ekp-buffer--put-display
           owner-beg owner-end
           (ekp-buffer--break-display
            owner-beg owner-end (ekp-layout-line-hyphen-p line))))))))

(defun ekp-buffer--project-line (line plan base &optional natural-p)
  "Project one LINE from PLAN at BASE.
NATURAL-P leaves source gaps at their ordinary display widths."
  (let ((beg (+ base (ekp-layout-line-source-start line)))
        (end (+ base (ekp-layout-line-source-end line)))
        (indent (ekp-layout-line-leading-pixel line)))
    (when (> indent 0)
      (ekp-buffer--put-line-prefix
       beg end (ekp-buffer--prefix-display indent)))
    (unless natural-p
      (cl-loop for gap across (ekp-layout-line-gaps line)
               do (ekp-buffer--project-gap gap plan base)))
    (ekp-buffer--project-break line plan base)))

(defun ekp-buffer--project-edges (plan base)
  "Hide stripped paragraph edges from PLAN at BASE."
  (let ((lines (ekp-layout-plan-lines plan))
        (length (length (ekp-layout-plan-string plan))))
    (when (> (length lines) 0)
      (let ((first (aref lines 0))
            (last (aref lines (1- (length lines)))))
        (when (> (ekp-layout-line-source-start first) 0)
          (ekp-buffer--put-display
           base (+ base (ekp-layout-line-source-start first))
           (make-string 0 0)))
        (when (< (ekp-layout-line-source-end last) length)
          (ekp-buffer--put-display
           (+ base (ekp-layout-line-source-end last)) (+ base length)
           (make-string 0 0)))))))

(defun ekp-buffer--projected-line-from-plan
    (line base &optional effective-end)
  "Return a record for LINE projected from BASE.
EFFECTIVE-END includes the source boundary owned by the line."
  (ekp-buffer--projected-line-create
   :base (copy-marker base)
   :beg (copy-marker (+ base (ekp-layout-line-source-start line)))
   :end (copy-marker (+ base (ekp-layout-line-source-end line)) t)
   :break-end
   (copy-marker
    (or effective-end
        (+ base (ekp-layout-line-break-source-end line)))
    t)
   :line line))

(defun ekp-buffer--plan-projected-lines (plan base)
  "Return projected line records for PLAN at BASE."
  (let* ((lines (ekp-layout-plan-lines plan))
         (last (1- (length lines))))
    (cl-loop
     for line across lines
     for index from 0
     collect
     (ekp-buffer--projected-line-from-plan
      line base
      (and (= index last)
           (+ base (length (ekp-layout-plan-string plan))))))))

(defun ekp-buffer--register-span (beg end width plan)
  "Register a projection from BEG to END at WIDTH using PLAN."
  (push (ekp-buffer--span-create
         :beg (copy-marker beg)
         :end (copy-marker end t)
         :width width
         :plan plan
         :lines (ekp-buffer--plan-projected-lines plan beg))
        ekp-buffer--spans))

(defun ekp-buffer--install-plan (beg end width plan)
  "Install PLAN from BEG to END at WIDTH."
  (let ((base beg))
    (with-silent-modifications
      (ekp-buffer--project-edges plan base)
      (cl-loop for line across (ekp-layout-plan-lines plan)
               do (ekp-buffer--project-line line plan base))
      (put-text-property beg end 'ekp-justified width))
    (ekp-buffer--register-span beg end width plan)))

;;; Projection lifecycle

(defun ekp-buffer--span-overlaps-p (span beg end)
  "Return non-nil when SPAN intersects BEG through END."
  (let ((start (marker-position (ekp-buffer--span-beg span)))
        (finish (marker-position (ekp-buffer--span-end span))))
    (and start finish
         (if (= beg end)
             (<= start beg finish)
           (and (< start end) (> finish beg))))))

(defun ekp-buffer--discard-span (span)
  "Remove SPAN's markers."
  (dolist (line (ekp-buffer--span-lines span))
    (dolist (marker
             (list (ekp-buffer--projected-line-base line)
                   (ekp-buffer--projected-line-beg line)
                   (ekp-buffer--projected-line-end line)
                   (ekp-buffer--projected-line-break-end line)))
      (set-marker marker nil)))
  (set-marker (ekp-buffer--span-beg span) nil)
  (set-marker (ekp-buffer--span-end span) nil))

(defun ekp-buffer--clear-projection (beg end)
  "Clear complete projected spans intersecting BEG through END."
  (let ((targets (cl-remove-if-not
                  (lambda (span)
                    (ekp-buffer--span-overlaps-p span beg end))
                  ekp-buffer--spans)))
    (when targets
      (let ((ekp-buffer--inhibit t))
        (with-silent-modifications
          (dolist (span targets)
            (let ((start (marker-position (ekp-buffer--span-beg span)))
                  (finish (marker-position (ekp-buffer--span-end span))))
              (when (and start finish)
                (ekp-buffer--remove-properties start finish)))
            (setq ekp-buffer--spans (delq span ekp-buffer--spans))
            (ekp-buffer--discard-span span)))))))

(defun ekp-buffer--clear-all ()
  "Remove every EKP projection from the widened buffer."
  (save-restriction
    (widen)
    (ekp-buffer--clear-projection (point-min) (point-max))))

;;; Commands

(defun ekp-buffer--paragraph-ranges (beg end)
  "Return hard paragraph ranges inside BEG through END."
  (let (ranges)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((start (point))
              (finish (if (search-forward "\n" end t)
                          (1- (point))
                        end)))
          (push (cons start finish) ranges)
          (if (< finish end)
              (goto-char (1+ finish))
            (goto-char end)))))
    (nreverse ranges)))

(defun ekp-buffer--annotate-inline-faces (text context)
  "Annotate inline face spans on TEXT using CONTEXT."
  (let ((faces (plist-get context :inline-faces))
        (policy (plist-get context :inline-code-policy))
        (pos 0)
        (length (length text))
        annotated)
    (when (and faces (not (eq policy 'normal)))
      (while (< pos length)
        (let* ((face (get-text-property pos 'face text))
               (next (or (next-single-property-change pos 'face text length)
                         length)))
          (when (ekp-buffer--face-member-p face faces)
            (unless annotated
              (setq annotated (copy-sequence text)))
            (put-text-property
             pos next 'ekp--face-break-policy policy annotated))
          (setq pos next))))
    (or annotated text)))

(defun ekp-buffer--planning-text (text context)
  "Return TEXT annotated with transient inline policy from CONTEXT."
  (ekp-buffer--annotate-inline-faces text context))

(defun ekp-buffer--layout-plan (text pixel context)
  "Return layout plan for TEXT at PIXEL under CONTEXT."
  (let ((planning-text (ekp-buffer--planning-text text context)))
    (ekp-buffer--with-policy-context context
      (let ((plan (ekp-layout-plan planning-text pixel)))
        (setf (ekp-layout-plan-string plan) text)
        plan))))

(defun ekp-buffer--layout-paragraph (beg end pixel)
  "Project one hard paragraph from BEG to END at PIXEL."
  (if (and ekp-buffer--automatic-pass
           (> (- end beg) ekp-auto-justify-paragraph-limit))
      (ekp-buffer--record-conflict
       beg end "paragraph exceeds the automatic paragraph limit")
    (let* ((paragraph (buffer-substring beg end))
           (context (ekp-buffer--policy-context pixel)))
      (unless (ekp-buffer--skip-paragraph-p paragraph context)
        (let ((plan (ekp-buffer--layout-plan paragraph pixel context)))
          (if (eq (ekp-layout-plan-state plan) 'natural)
              (ekp-buffer--record-conflict
               beg end "overlong-token-natural")
            (when (ekp-buffer--projectable-p plan beg beg end)
              (ekp-buffer--install-plan beg end pixel plan))))))))

(defun ekp-buffer--dwim-bounds ()
  "Return active region bounds or the hard paragraph at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (ekp-buffer--para-bounds (cons (point) (point)))))

;;;###autoload
(defun ekp-justify-region (beg end &optional pixel)
  "Project BEG through END as KP layout at PIXEL without changing text."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (pcase-let ((`(,start . ,finish) (ekp-buffer--dwim-bounds)))
       (list start finish
             (and current-prefix-arg
                  (prefix-numeric-value current-prefix-arg))))))
  (setq pixel (or pixel (ekp-buffer--effective-width)))
  (ekp--validate-width pixel)
  (let ((start (min beg end))
        (finish (max beg end)))
    (setq beg start
          end finish))
  (when (and font-lock-mode
             (let ((context (ekp-buffer--policy-context pixel)))
               (or (plist-get context :block-faces)
                   (plist-get context :inline-faces)
                   ekp-buffer-skip-predicate)))
    (font-lock-ensure beg end))
  (let ((point-before (point))
        (mark-before (and (mark t) (copy-marker (mark t))))
        (mark-active-before mark-active)
        (ekp-buffer--inhibit t))
    (unwind-protect
        (progn
          (ekp-buffer--clear-projection beg end)
          (setq ekp-buffer--conflicts nil)
          (dolist (range (ekp-buffer--paragraph-ranges beg end))
            (ekp-buffer--layout-paragraph (car range) (cdr range) pixel))
          (if (or ekp-auto-justify-mode ekp-buffer--spans)
              (ekp-buffer--install-integrations)
            (ekp-buffer--remove-integrations))
          (goto-char point-before)
          (when mark-before
            (set-marker (mark-marker) (marker-position mark-before)))
          (setq mark-active mark-active-before))
      (when mark-before (set-marker mark-before nil)))))

;;;###autoload
(defun ekp-unjustify-region (beg end)
  "Remove EKP display projection intersecting BEG through END."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (pcase-let ((`(,start . ,finish) (ekp-buffer--dwim-bounds)))
       (list start finish))))
  (ekp-buffer--clear-projection (min beg end) (max beg end))
  (unless (or ekp-auto-justify-mode ekp-buffer--spans)
    (ekp-buffer--remove-integrations)))

;;;###autoload
(defun ekp-justify-buffer (&optional pixel)
  "Project the accessible buffer as KP layout at PIXEL."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (and current-prefix-arg
                (prefix-numeric-value current-prefix-arg)))))
  (ekp-justify-region (point-min) (point-max) pixel))

;;;###autoload
(defun ekp-unjustify-buffer ()
  "Remove EKP projection from the accessible buffer."
  (interactive "*")
  (ekp-unjustify-region (point-min) (point-max)))

;;; Copy integration

(defun ekp-buffer--strip-owned-from-string (string)
  "Remove EKP projection properties from copied STRING."
  (let ((pos 0) (length (length string)))
    (while (< pos length)
      (let ((next (or (next-property-change pos string length) length)))
        (set-text-properties
         pos next
         (ekp-buffer--clean-property-list
          (text-properties-at pos string))
         string)
        (setq pos next)))
    string))

(defun ekp-buffer--clean-property-list (properties)
  "Return PROPERTIES with EKP-owned projection properties removed."
  (let ((display (plist-get properties 'ekp-buffer--display))
        (prefix (plist-get properties 'ekp-buffer--line-prefix))
        clean)
    (while properties
      (let ((property (pop properties))
            (value (pop properties)))
        (unless (or (memq property ekp-buffer--owned-properties)
                    (and (eq property 'display) (eq value display))
                    (and (eq property 'line-prefix) (eq value prefix)))
          (setq clean (plist-put clean property value)))))
    clean))

(defun ekp-buffer--filter-buffer-substring (beg end &optional delete)
  "Extract logical BEG through END, optionally DELETE, without projection."
  (let (extracted)
    (let ((filter-buffer-substring-function ekp-buffer--previous-filter)
          (ekp-buffer--filtering t))
      (setq extracted (filter-buffer-substring beg end delete)))
    (when delete
      (ekp-buffer--after-layout-change))
    (ekp-buffer--strip-owned-from-string extracted)))

(defun ekp-buffer--install-integrations ()
  "Install integrations needed by manual spans or auto mode."
  (add-hook 'before-change-functions #'ekp-buffer--before-change nil t)
  (add-hook 'after-change-functions
            #'ekp-buffer--after-layout-change nil t)
  (unless ekp-buffer--filter-installed
    (setq ekp-buffer--previous-filter-local-p
          (local-variable-p 'filter-buffer-substring-function)
          ekp-buffer--previous-filter filter-buffer-substring-function
          ekp-buffer--filter-installed t)
    (setq-local filter-buffer-substring-function
                #'ekp-buffer--filter-buffer-substring)))

(defun ekp-buffer--remove-integrations ()
  "Remove EKP integrations and restore the prior substring filter."
  (remove-hook 'before-change-functions #'ekp-buffer--before-change t)
  (remove-hook 'after-change-functions
               #'ekp-buffer--after-layout-change t)
  (when ekp-buffer--filter-installed
    (when (eq filter-buffer-substring-function
              #'ekp-buffer--filter-buffer-substring)
      (if ekp-buffer--previous-filter-local-p
          (setq-local filter-buffer-substring-function
                      ekp-buffer--previous-filter)
        (kill-local-variable 'filter-buffer-substring-function)))
    (setq ekp-buffer--previous-filter nil
          ekp-buffer--previous-filter-local-p nil
          ekp-buffer--filter-installed nil)))

;;; Protection commands

(defvar ekp-break-policy nil
  "Text property controlling regional break policy.
`normal' clears automatic token or face restrictions, `hyphenate'
enables discretionary hyphenation, and `no-hyphen' suppresses it.
This property is not a hard no-break switch; rigid atoms remain
owned by the separate `ekp-no-break' text property.")

(defun ekp-buffer--interactive-protection-args ()
  "Return region arguments for an interactive protection command."
  (barf-if-buffer-read-only)
  (list (region-beginning) (region-end) t))

(defun ekp-buffer--after-region-policy-mutation (beg end)
  "Refresh projections between BEG and END after a region policy mutation."
  (if ekp-auto-justify-mode
      (ekp-buffer--reflow-for-policy-change (current-buffer) t)
    (ekp-buffer--clear-projection (min beg end) (max beg end))
    (unless ekp-buffer--spans
      (ekp-buffer--remove-integrations))))

(defun ekp-buffer--set-protection (beg end property enabled label announce)
  "Set PROPERTY to ENABLED from BEG to END and optionally ANNOUNCE LABEL."
  (let ((ekp-buffer--inhibit t))
    (if enabled
        (add-text-properties beg end (list property t))
      (remove-text-properties beg end (list property nil))))
  (ekp-buffer--after-region-policy-mutation beg end)
  (when announce
    (message "EKP: %s on %d characters; current buffer session only"
             label (- end beg))))

(defun ekp-buffer--set-break-policy (beg end policy label announce)
  "Set `ekp-break-policy' POLICY from BEG to END."
  (let ((ekp-buffer--inhibit t))
    (if policy
        (put-text-property beg end 'ekp-break-policy policy)
      (remove-text-properties beg end '(ekp-break-policy nil))))
  (ekp-buffer--after-region-policy-mutation beg end)
  (when announce
    (message "EKP: %s on %d characters; current buffer session only"
             label (- end beg))))

;;;###autoload
(defun ekp-normal-break-region (beg end &optional announce)
  "Mark BEG through END as ordinary break policy; ANNOUNCE reports it."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-break-policy
   beg end 'normal "Marked normal break policy" announce))

;;;###autoload
(defun ekp-enable-hyphenation-region (beg end &optional announce)
  "Enable discretionary hyphenation in BEG through END; ANNOUNCE reports it."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-break-policy
   beg end 'hyphenate "Enabled hyphenation" announce))

;;;###autoload
(defun ekp-disable-hyphenation-region (beg end &optional announce)
  "Disable discretionary hyphenation in BEG through END; ANNOUNCE reports it."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-break-policy
   beg end 'no-hyphen "Disabled hyphenation" announce))

;;;###autoload
(defun ekp-clear-break-policy-region (beg end &optional announce)
  "Clear `ekp-break-policy' from BEG through END; ANNOUNCE reports it."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-break-policy
   beg end nil "Cleared break policy" announce))

;;;###autoload
(defun ekp-no-break-region (beg end &optional announce)
  "Mark BEG through END as an unbreakable session-local atom.
ANNOUNCE requests interactive feedback."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-protection
   beg end 'ekp-no-break t "Marked no-break" announce))

;;;###autoload
(defun ekp-allow-break-region (beg end &optional announce)
  "Clear session-local `ekp-no-break' from BEG through END.
ANNOUNCE requests interactive feedback."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-protection
   beg end 'ekp-no-break nil "Cleared no-break" announce))

;;;###autoload
(defun ekp-verbatim-region (beg end &optional announce)
  "Keep paragraphs intersecting BEG through END verbatim this session.
ANNOUNCE requests interactive feedback."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-protection
   beg end 'ekp-verbatim t "Marked verbatim" announce))

;;;###autoload
(defun ekp-clear-verbatim-region (beg end &optional announce)
  "Clear session-local `ekp-verbatim' from BEG through END.
ANNOUNCE requests interactive feedback."
  (interactive (ekp-buffer--interactive-protection-args))
  (ekp-buffer--set-protection
   beg end 'ekp-verbatim nil "Cleared verbatim" announce))

;;; Auto mode

(defun ekp-buffer--para-bounds (marker-pair)
  "Return hard paragraph bounds containing MARKER-PAIR."
  (let ((beg (if (markerp (car marker-pair))
                 (marker-position (car marker-pair))
               (car marker-pair)))
        (end (if (markerp (cdr marker-pair))
                 (marker-position (cdr marker-pair))
               (cdr marker-pair))))
    (save-excursion
      (goto-char (max (point-min) (min beg (point-max))))
      (setq beg (line-beginning-position))
      (goto-char (max (point-min) (min end (point-max))))
      (setq end (line-end-position))
      (cons beg end))))

(defun ekp-buffer--span-at (beg end)
  "Return the projection span intersecting BEG through END."
  (seq-find
   (lambda (span) (ekp-buffer--span-overlaps-p span beg end))
   ekp-buffer--spans))

(defun ekp-buffer--release-live-edit ()
  "Detach markers owned by the current live edit transaction."
  (when ekp-buffer--live-edit
    (dolist (marker
             (delq nil
                   (list
                    (ekp-buffer--live-edit-old-beg ekp-buffer--live-edit)
                    (ekp-buffer--live-edit-dirty-beg ekp-buffer--live-edit)
                    (ekp-buffer--live-edit-dirty-end ekp-buffer--live-edit)
                    (ekp-buffer--live-edit-row-start ekp-buffer--live-edit)
                    (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit))))
      (set-marker marker nil))
    (setq ekp-buffer--live-edit nil)))

(defun ekp-buffer--detach-live-markers ()
  "Detach every marker owned by the current live state."
  (dolist (marker
           (list (ekp-buffer--live-state-beg ekp-buffer--live-state)
                 (ekp-buffer--live-state-end ekp-buffer--live-state)
                 (ekp-buffer--live-state-prefix-end
                  ekp-buffer--live-state)))
    (set-marker marker nil)))

(defun ekp-buffer--release-live-state ()
  "Detach markers and projection owned by the active live hard line."
  (when ekp-buffer--live-state
    (let ((beg (marker-position
                (ekp-buffer--live-state-beg ekp-buffer--live-state)))
          (end (marker-position
                (ekp-buffer--live-state-end ekp-buffer--live-state))))
      (when (and beg end)
        (ekp-buffer--clear-live-projection beg end)))
    (ekp-buffer--detach-live-markers)
    (setq ekp-buffer--live-state nil)))

(defun ekp-buffer--detach-live-state ()
  "Detach live state markers while preserving installed spans."
  (when ekp-buffer--live-state
    (ekp-buffer--detach-live-markers)
    (setq ekp-buffer--live-state nil)))

(defun ekp-buffer--live-state-contains-p (position)
  "Return non-nil if source POSITION is inside the live state."
  (when ekp-buffer--live-state
    (let ((beg (marker-position
                (ekp-buffer--live-state-beg ekp-buffer--live-state)))
          (end (marker-position
                (ekp-buffer--live-state-end ekp-buffer--live-state))))
      (and beg end (<= beg position end)))))

(defun ekp-buffer--activate-live-paragraph (beg end)
  "Make BEG through END the active hard line."
  (ekp-buffer--release-live-state)
  (ekp-buffer--clear-projection beg end)
  (setq ekp-buffer--live-state
        (ekp-buffer--live-state-create
         :beg (copy-marker beg)
         :end (copy-marker end t)
         :source (ekp-buffer--logical-substring beg end)
         :prefix-end (copy-marker beg))))

(defun ekp-buffer--commit-live-paragraph ()
  "Commit the active hard line as one complete global plan."
  (when ekp-buffer--live-state
    (let ((end (marker-position
                (ekp-buffer--live-state-end ekp-buffer--live-state))))
      (when end
        (ekp-buffer--publish-live-prefix end t)))
    (ekp-buffer--release-live-edit)))

(defun ekp-buffer--finalize-live-paragraph ()
  "Commit and detach the active hard line while preserving projection."
  (when ekp-buffer--live-state
    (ekp-buffer--commit-live-paragraph)
    (ekp-buffer--detach-live-state)))

(defun ekp-buffer--prepare-live-paragraph (beg end)
  "Ensure BEG through END is active with a stable committed baseline."
  (unless (and (ekp-buffer--live-state-contains-p beg)
               (ekp-buffer--live-state-contains-p end))
    (when ekp-buffer--live-state
      (save-restriction
        (widen)
        (ekp-buffer--finalize-live-paragraph)))
    (ekp-buffer--activate-live-paragraph beg end)
    (ekp-buffer--publish-live-prefix end t)))

(defun ekp-buffer--live-edit-contains-p (beg end)
  "Return non-nil when BEG through END stay inside the dirty island."
  (when ekp-buffer--live-edit
    (let ((start (marker-position
                  (ekp-buffer--live-edit-dirty-beg
                   ekp-buffer--live-edit)))
          (finish (marker-position
                   (ekp-buffer--live-edit-dirty-end
                    ekp-buffer--live-edit))))
      (and start finish (<= start beg end finish)))))

(defun ekp-buffer--live-span-edit-hit-p (span beg end line-end)
  "Return non-nil when SPAN owns the edit from BEG through END.
LINE-END lets insertion at the hard-line end belong to its last span."
  (let ((start (marker-position (ekp-buffer--span-beg span)))
        (finish (marker-position (ekp-buffer--span-end span))))
    (and start finish
         (if (= beg end)
             (and (<= start beg)
                  (or (< beg finish)
                      (and (= beg line-end) (= finish line-end))))
           (and (< start end) (> finish beg))))))

(defun ekp-buffer--live-dirty-range (beg end)
  "Return the smallest committed island owning BEG through END."
  (let* ((state ekp-buffer--live-state)
         (line-beg (marker-position (ekp-buffer--live-state-beg state)))
         (line-end (marker-position (ekp-buffer--live-state-end state)))
         (prefix-end
          (marker-position (ekp-buffer--live-state-prefix-end state)))
         (targets
          (cl-remove-if-not
           (lambda (span)
             (ekp-buffer--live-span-edit-hit-p span beg end line-end))
           (ekp-buffer--live-state-spans state))))
    (if targets
        (cons (marker-position (ekp-buffer--span-beg (car targets)))
              (marker-position
               (ekp-buffer--span-end (car (last targets)))))
      (if (>= beg prefix-end)
          (cons prefix-end line-end)
        (cons line-beg prefix-end)))))

(defun ekp-buffer--naturalize-live-range (beg end)
  "Remove EKP-owned display properties from BEG through END."
  (let ((ekp-buffer--inhibit t))
    (with-silent-modifications
      (save-restriction
        (widen)
        (ekp-buffer--remove-properties beg end)))))

(defun ekp-buffer--projected-line-markers (line)
  "Return every marker owned by projected LINE."
  (list (ekp-buffer--projected-line-base line)
        (ekp-buffer--projected-line-beg line)
        (ekp-buffer--projected-line-end line)
        (ekp-buffer--projected-line-break-end line)))

(defun ekp-buffer--live-state-owned-markers ()
  "Return all live-state and projection markers."
  (append
   (list (ekp-buffer--live-state-beg ekp-buffer--live-state)
         (ekp-buffer--live-state-end ekp-buffer--live-state)
         (ekp-buffer--live-state-prefix-end ekp-buffer--live-state))
   (cl-mapcan
    (lambda (span)
      (append
       (list (ekp-buffer--span-beg span)
             (ekp-buffer--span-end span))
       (cl-mapcan #'ekp-buffer--projected-line-markers
                  (ekp-buffer--span-lines span))))
    (ekp-buffer--live-state-spans ekp-buffer--live-state))))

(defun ekp-buffer--live-marker-snapshot (base)
  "Return live marker offsets relative to BASE."
  (delq nil
        (mapcar
   (lambda (marker)
     (when-let* ((position (marker-position marker)))
       (cons marker (- position base))))
   (ekp-buffer--live-state-owned-markers))))

(defun ekp-buffer--start-live-edit (beg end bounds)
  "Start a stable edit transaction for BEG through END in BOUNDS."
  (let* ((state ekp-buffer--live-state)
         (base (marker-position (ekp-buffer--live-state-beg state)))
         (finish (marker-position (ekp-buffer--live-state-end state)))
         (range (ekp-buffer--live-dirty-range beg end))
         (dirty-beg (car range))
         (dirty-end (cdr range))
         (baseline (buffer-substring base finish)))
    (ekp-buffer--naturalize-live-range dirty-beg dirty-end)
    (setq ekp-buffer--live-edit
          (ekp-buffer--live-edit-create
           :old-beg (copy-marker (car bounds))
           :baseline-source baseline
           :baseline-plan (ekp-buffer--live-state-plan state)
           :baseline-signatures (ekp-buffer--live-state-signatures state)
           :baseline-spans (ekp-buffer--live-state-spans state)
           :baseline-key (ekp-buffer--live-state-key state)
           :baseline-active-index
           (ekp-buffer--live-state-active-index state)
           :baseline-prefix-end
           (- (marker-position (ekp-buffer--live-state-prefix-end state))
              base)
           :baseline-markers (ekp-buffer--live-marker-snapshot base)
           :dirty-beg (copy-marker dirty-beg)
           :dirty-end (copy-marker dirty-end t)
           :dirty-start (- dirty-beg base)
           :dirty-finish (- dirty-end base)
           :row-start
           (copy-marker
            (or (ekp-buffer--native-row-start beg) dirty-beg))
           :edit-end (copy-marker end t)))))

(defun ekp-buffer--prepare-live-edit (beg end bounds)
  "Reuse or create the live transaction for BEG through END in BOUNDS."
  (unless (ekp-buffer--live-edit-contains-p beg end)
    (if (and ekp-buffer--live-edit
             (ekp-buffer--live-state-contains-p beg)
             (ekp-buffer--live-state-contains-p end))
        (ekp-buffer--commit-live-paragraph)
      (when ekp-buffer--live-edit
        (ekp-buffer--finalize-live-paragraph)))
    (ekp-buffer--prepare-live-paragraph (car bounds) (cdr bounds))
    (ekp-buffer--start-live-edit beg end bounds))
  (setf (ekp-buffer--live-edit-old-fragment ekp-buffer--live-edit)
        (buffer-substring-no-properties beg end)))

(defun ekp-buffer--before-change (beg end)
  "Prepare one stable dirty island for BEG through END."
  (unless ekp-buffer--inhibit
    (if ekp-auto-justify-mode
        (let ((bounds (ekp-buffer--para-bounds (cons beg end))))
          (ekp-buffer--cancel-pending)
          (ekp-buffer--prepare-live-edit beg end bounds)
          (cl-incf ekp-buffer--generation)
          (set-marker
           (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit)
           end))
      (when (ekp-buffer--span-at beg end)
        (ekp-buffer--clear-projection beg end)))))

(defun ekp-buffer--after-layout-change (&rest _ignored)
  "Release manual integrations after the final projection disappears."
  (unless (or ekp-buffer--inhibit
              ekp-buffer--filtering
              ekp-auto-justify-mode
              ekp-buffer--spans)
    (ekp-buffer--remove-integrations)))

(defun ekp-buffer--composing-p ()
  "Return non-nil while an input-method preedit is active."
  (or (and (bound-and-true-p quail-overlay)
           (overlayp quail-overlay)
           (overlay-buffer quail-overlay))
      (get-text-property (point) 'composition)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'composition))))

(defun ekp-buffer--release-marker-pairs (pairs)
  "Detach both markers in every element of PAIRS."
  (dolist (pair pairs)
    (set-marker (car pair) nil)
    (set-marker (cdr pair) nil)))

(defun ekp-buffer--logical-substring (beg end)
  "Return BEG through END with EKP projection properties removed."
  (ekp-buffer--strip-owned-from-string (buffer-substring beg end)))

(defun ekp-buffer--current-live-source ()
  "Return the current logical source by replacing only the dirty island."
  (let* ((state ekp-buffer--live-state)
         (edit ekp-buffer--live-edit)
         (source (ekp-buffer--live-state-source state))
         (start (ekp-buffer--live-edit-dirty-start edit))
         (finish (ekp-buffer--live-edit-dirty-finish edit))
         (dirty-beg
          (marker-position (ekp-buffer--live-edit-dirty-beg edit)))
         (dirty-end
          (marker-position (ekp-buffer--live-edit-dirty-end edit))))
    (concat (substring source 0 start)
            (buffer-substring dirty-beg dirty-end)
            (substring source finish))))

(defun ekp-buffer--live-context (width &optional context)
  "Return non-text layout context for a live plan at WIDTH.
Optional CONTEXT supplies a precomputed policy context."
  (let ((context (or context (ekp-buffer--policy-context width))))
    (ekp-buffer--with-policy-context context
      (ekp--copy-layout-context-value
       (list (ekp--dp-key width)
             (ekp--width-context)
             (mapcar (lambda (attribute)
                       (face-attribute 'default attribute nil t))
                     '(:family :height :width :weight :slant))
             ekp-latin-lang
             ekp-alignment
             ekp-ragged-stretch-pixel
             (and ekp-protrusion ekp-protrusion-ratios)
             ekp-parshape
             ekp-first-line-indent
             context
             (ekp--policy-signature)
             (ekp--spacing-signature))))))

(defun ekp-buffer--live-key-equal-p (left right)
  "Return non-nil when live cache keys LEFT and RIGHT are equivalent."
  (and (equal (cadr left) (cadr right))
       (equal-including-properties (car left) (car right))))

(defun ekp-buffer--live-cache-get (key)
  "Return cached plan for KEY and move it to the front."
  (when-let* ((cell
               (seq-find
                (lambda (entry)
                  (ekp-buffer--live-key-equal-p key (car entry)))
                ekp-buffer--live-plan-cache)))
    (setq ekp-buffer--live-plan-cache
          (cons cell (delq cell ekp-buffer--live-plan-cache)))
    (cdr cell)))

(defun ekp-buffer--live-cache-put (key plan)
  "Store PLAN under KEY in the bounded live cache."
  (setq ekp-buffer--live-plan-cache
        (cons (cons key plan)
              (cl-remove key ekp-buffer--live-plan-cache
                         :key #'car
                         :test #'ekp-buffer--live-key-equal-p)))
  (when (> (length ekp-buffer--live-plan-cache) 16)
    (setcdr (nthcdr 15 ekp-buffer--live-plan-cache) nil))
  plan)

(defun ekp-buffer--live-append-plan (key text width)
  "Extend the committed live plan for KEY to TEXT at WIDTH."
  (let ((old-key (and ekp-buffer--live-state
                      (ekp-buffer--live-state-key
                       ekp-buffer--live-state)))
        (old-plan (and ekp-buffer--live-state
                       (ekp-buffer--live-state-plan
                        ekp-buffer--live-state))))
    (when (and old-key old-plan
               (equal (cadr key) (cadr old-key)))
      (let* ((context (ekp-buffer--policy-context width))
             (planning-text (ekp-buffer--planning-text text context))
             (ekp--allow-native-live-append
              (and ekp-auto-justify-native-append
                   (bound-and-true-p ekp-c-module-loaded)))
             (plan (ekp-buffer--with-policy-context context
                     (ekp-layout-plan-append
                      old-plan planning-text width))))
        (when plan
          (setf (ekp-layout-plan-string plan) text))
        plan))))

(defun ekp-buffer--live-plan-entry (text width)
  "Return the cache entry for TEXT at WIDTH."
  (let* ((context (ekp-buffer--policy-context width))
         (key (list (copy-sequence text)
                    (ekp-buffer--live-context width context))))
    (cons key
          (or (ekp-buffer--live-cache-get key)
              (ekp-buffer--live-cache-put
               key (or (ekp-buffer--live-append-plan key text width)
                       (ekp-buffer--layout-plan
                        text width context)))))))

(defun ekp-buffer--single-line-live-p (text width)
  "Return non-nil if TEXT is conservatively known to fit WIDTH."
  (and (= ekp-looseness 0)
       (not ekp-parshape)
       (not ekp-first-line-indent)
       (not (string-match-p "[\t]" text))
       (<= (ekp--measured-width text) width)))

(defun ekp-buffer--line-break-end (line)
  "Return LINE's semantic ownership end."
  (max (ekp-layout-line-source-end line)
       (or (ekp-layout-line-break-source-end line)
           (ekp-layout-line-source-end line))))

(defun ekp-buffer--active-line-index (plan offset)
  "Return the semantic line index owning OFFSET in PLAN."
  (let ((lines (ekp-layout-plan-lines plan))
        found)
    (dotimes (index (length lines))
      (let ((line (aref lines index)))
        (when (and (not found)
                   (<= (ekp-layout-line-source-start line) offset)
                   (or (< offset (ekp-buffer--line-break-end line))
                       (and (= index (1- (length lines)))
                            (<= offset (ekp-buffer--line-break-end line)))))
          (setq found index))))
    (or found (max 0 (1- (length lines))))))

(defun ekp-buffer--live-line-signature (line text)
  "Return a buffer projection signature for LINE in TEXT."
  (let* ((start (ekp-layout-line-source-start line))
         (end (ekp-buffer--line-break-end line))
         (owned (substring text start end)))
    (list line owned (ekp--key-intervals owned))))

(defun ekp-buffer--live-prefix-signatures (plan active)
  "Return signatures for semantic lines before ACTIVE in PLAN."
  (let ((text (ekp-layout-plan-string plan))
        signatures)
    (dotimes (index active)
      (push (ekp-buffer--live-line-signature
             (aref (ekp-layout-plan-lines plan) index)
             text)
            signatures))
    (nreverse signatures)))

(defun ekp-buffer--make-live-line-span (base beg end width line)
  "Return one live LINE span from BASE through BEG and END at WIDTH."
  (let ((span (ekp-buffer--span-create
               :beg (copy-marker beg)
               :end (copy-marker end)
               :width width
               :plan nil
               :lines (list (ekp-buffer--projected-line-from-plan
                             line base end)))))
    (push span ekp-buffer--spans)
    span))

(defun ekp-buffer--common-prefix-length (left right)
  "Return the equal prefix length shared by LEFT and RIGHT."
  (let ((count 0))
    (while (and left right (equal (car left) (car right)))
      (setq count (1+ count)
            left (cdr left)
            right (cdr right)))
    count))

(defun ekp-buffer--discard-live-spans (spans)
  "Discard live SPANS and unregister them."
  (dolist (span spans)
    (setq ekp-buffer--spans (delq span ekp-buffer--spans))
    (ekp-buffer--discard-span span)))

(defun ekp-buffer--live-split-spans (count)
  "Keep COUNT live spans and return the dropped suffix."
  (let* ((spans (ekp-buffer--live-state-spans ekp-buffer--live-state))
         (keep-count (min count (length spans)))
         (keep (cl-subseq spans 0 keep-count))
         (drop (nthcdr keep-count spans)))
    (setf (ekp-buffer--live-state-spans ekp-buffer--live-state) keep)
    drop))

(defun ekp-buffer--live-prefix-end-after (beg count)
  "Return the buffer end of COUNT kept live lines, or BEG."
  (if (zerop count)
      beg
    (marker-position
     (ekp-buffer--span-end
      (nth (1- count)
           (ekp-buffer--live-state-spans ekp-buffer--live-state))))))

(defun ekp-buffer--live-span-index-at (position)
  "Return the live prefix span index owning POSITION, or nil."
  (let ((spans (ekp-buffer--live-state-spans ekp-buffer--live-state))
        found)
    (cl-loop for span in spans
             for index from 0
             until found
             do
             (let ((beg (marker-position (ekp-buffer--span-beg span)))
                   (end (marker-position (ekp-buffer--span-end span))))
               (when (and beg end (<= beg position) (< position end))
                 (setq found index))))
    found))

(defun ekp-buffer--clear-live-projection (beg end)
  "Clear the current live prefix from BEG through END."
  (ekp-buffer--discard-live-spans
   (ekp-buffer--live-state-spans ekp-buffer--live-state))
  (setf (ekp-buffer--live-state-spans ekp-buffer--live-state) nil)
  (let ((ekp-buffer--inhibit t))
    (with-silent-modifications
      (save-restriction
        (widen)
        (ekp-buffer--remove-properties beg end)))))

(defun ekp-buffer--live-prefix-present-p (beg active)
  "Return non-nil when ACTIVE prefix lines from BEG carry EKP ownership."
  (or (= active 0)
      (let ((prefix-end
             (marker-position
              (ekp-buffer--live-state-prefix-end ekp-buffer--live-state))))
        (and prefix-end
             (not (text-property-any
                   beg prefix-end 'ekp-justified nil))))))

(defun ekp-buffer--unsupported-live-text-p (text)
  "Return non-nil for TEXT with whitespace live layout cannot project."
  (string-match-p "[\t]" text))

(defun ekp-buffer--live-prefix-current-p (beg plan active signatures)
  "Return non-nil when PLAN at BEG has ACTIVE installed SIGNATURES."
  (and (equal signatures
              (ekp-buffer--live-state-signatures ekp-buffer--live-state))
       (eq plan (ekp-buffer--live-state-plan ekp-buffer--live-state))
       (= active (or (ekp-buffer--live-state-active-index
                      ekp-buffer--live-state)
                     -1))
       (ekp-buffer--live-prefix-present-p beg active)))

(defun ekp-buffer--commit-live-state
    (source key plan signatures active width prefix-end)
  "Commit SOURCE, KEY, PLAN, SIGNATURES, and ACTIVE at WIDTH through PREFIX-END."
  (setf (ekp-buffer--live-state-width ekp-buffer--live-state) width
        (ekp-buffer--live-state-source ekp-buffer--live-state) source
        (ekp-buffer--live-state-key ekp-buffer--live-state) key
        (ekp-buffer--live-state-plan ekp-buffer--live-state) plan
        (ekp-buffer--live-state-signatures ekp-buffer--live-state)
        signatures
        (ekp-buffer--live-state-active-index ekp-buffer--live-state)
        active)
  (set-marker
   (ekp-buffer--live-state-prefix-end ekp-buffer--live-state)
   prefix-end))

(defun ekp-buffer--replace-live-prefix
    (key plan beg end width active signatures)
  "Replace PLAN prefix at BEG through END and commit KEY.
WIDTH, ACTIVE, and SIGNATURES describe the replacement."
  (let* ((old (ekp-buffer--live-state-signatures ekp-buffer--live-state))
         (common (min active
                      (length (ekp-buffer--live-state-spans
                               ekp-buffer--live-state))
                      (ekp-buffer--common-prefix-length old signatures)))
         (clear-start (ekp-buffer--clear-live-suffix beg end common))
         (installed
          (ekp-buffer--install-live-prefix
           plan beg width common active)))
    (setf (ekp-buffer--live-state-spans ekp-buffer--live-state)
          (append (ekp-buffer--live-state-spans ekp-buffer--live-state)
                  (car installed)))
    (ekp-buffer--commit-live-state
     (ekp-layout-plan-string plan)
     key plan signatures active width (or (cdr installed) clear-start))))

(defun ekp-buffer--project-live-plan
    (key plan beg end width boundary complete)
  "Project PLAN before BOUNDARY and commit KEY.
When COMPLETE is non-nil, project every semantic line."
  (let* ((active
          (if complete
              (length (ekp-layout-plan-lines plan))
            (ekp-buffer--active-line-index plan (- boundary beg))))
         (signatures (ekp-buffer--live-prefix-signatures plan active)))
    (unless (ekp-buffer--live-prefix-current-p
             beg plan active signatures)
      (let ((ekp-buffer--inhibit t))
        (ekp-buffer--replace-live-prefix
         key plan beg end width active signatures)))))

(defun ekp-buffer--commit-natural-live-state (beg end width text)
  "Commit a natural live state for TEXT from BEG through END at WIDTH."
  (ekp-buffer--clear-live-projection beg end)
  (ekp-buffer--commit-live-state text nil nil nil 0 width beg))

(defun ekp-buffer--clear-live-suffix (beg end keep-count)
  "From BEG, keep KEEP-COUNT live lines and clear through END."
  (ekp-buffer--discard-live-spans
   (ekp-buffer--live-split-spans keep-count))
  (let ((clear-start (or (ekp-buffer--live-prefix-end-after beg keep-count)
                         beg))
        (ekp-buffer--inhibit t))
    (with-silent-modifications
      (save-restriction
        (widen)
        (ekp-buffer--remove-properties clear-start end)))
    clear-start))

(defun ekp-buffer--install-live-prefix (plan beg width start active)
  "Install PLAN semantic lines START through ACTIVE from BEG at WIDTH."
  (let ((lines (ekp-layout-plan-lines plan))
        spans
        prefix-end)
    (condition-case err
        (progn
          (with-silent-modifications
            (cl-loop for index from start below active
                     do
                     (let* ((line (aref lines index))
                            (line-beg
                             (+ beg (ekp-layout-line-source-start line)))
                            (finish
                             (+ beg (ekp-buffer--line-break-end line))))
                       (ekp-buffer--project-line line plan beg)
                       (put-text-property
                        line-beg finish 'ekp-justified width)
                       (push (ekp-buffer--make-live-line-span
                              beg line-beg finish width line)
                             spans)
                       (setq prefix-end finish))))
          (cons (nreverse spans) prefix-end))
      (error
       (ekp-buffer--discard-live-spans spans)
       (signal (car err) (cdr err))))))

(defun ekp-buffer--publish-semantic-prefix
    (beg end width text boundary complete)
  "Publish TEXT's stable prefix from BEG to END at WIDTH.
BOUNDARY identifies the native row left natural unless COMPLETE is non-nil."
  (let ((context (ekp-buffer--policy-context width)))
    (cond
     ((or (>= beg end) (ekp-buffer--skip-paragraph-p text context))
      (ekp-buffer--commit-natural-live-state beg end width text))
     ((> (- end beg) ekp-auto-justify-paragraph-limit)
      (ekp-buffer--record-conflict
       beg end "paragraph exceeds the automatic paragraph limit")
      (ekp-buffer--commit-natural-live-state beg end width text))
     ((and (not complete)
           (ekp-buffer--single-line-live-p text width))
      (ekp-buffer--commit-natural-live-state beg end width text))
     ((ekp-buffer--unsupported-live-text-p text)
      (ekp-buffer--record-conflict
       beg end "unsupported whitespace shrink")
      (ekp-buffer--commit-natural-live-state beg end width text))
     ((ekp-buffer--foreign-property beg end)
      (ekp-buffer--record-conflict beg end "foreign property in live line")
      (ekp-buffer--commit-natural-live-state beg end width text))
     (t
      (let* ((entry (ekp-buffer--live-plan-entry text width))
             (key (car entry))
             (plan (cdr entry)))
        (cond
         ((eq (ekp-layout-plan-state plan) 'natural)
          (ekp-buffer--record-conflict beg end "overlong-token-natural")
          (ekp-buffer--commit-natural-live-state beg end width text))
         ((ekp-buffer--unsupported-gap plan beg)
            (progn
              (ekp-buffer--record-conflict
               beg end "unsupported live whitespace shrink")
              (ekp-buffer--commit-natural-live-state beg end width text)))
         (t
          (ekp-buffer--project-live-plan
           key plan beg end width boundary complete))))))))

(defun ekp-buffer--publish-live-prefix (&optional boundary complete)
  "Publish one stable live plan through BOUNDARY.
Keep BOUNDARY's semantic row natural unless COMPLETE is non-nil."
  (when ekp-buffer--live-state
    (let* ((beg (marker-position
                 (ekp-buffer--live-state-beg ekp-buffer--live-state)))
           (end (marker-position
                 (ekp-buffer--live-state-end ekp-buffer--live-state)))
           (width ekp-buffer--auto-width)
           (boundary
            (max beg
                 (min (or boundary
                          (and ekp-buffer--live-edit
                               (marker-position
                                (ekp-buffer--live-edit-edit-end
                                 ekp-buffer--live-edit)))
                          (point))
                      end)))
           (text
            (and beg end
                 (if ekp-buffer--live-edit
                     (ekp-buffer--current-live-source)
                   (ekp-buffer--logical-substring beg end)))))
      (when (and beg end width text)
        (condition-case err
            (ekp-buffer--publish-semantic-prefix
             beg end width text boundary complete)
          (error
           (ekp-buffer--clear-live-projection beg end)
           (signal (car err) (cdr err))))))))

(defun ekp-buffer--apply-hard-boundary-edit (beg end)
  "Finalize hard paragraphs completed by the edit from BEG through END."
  (let* ((edit ekp-buffer--live-edit)
         (old-start (marker-position
                     (ekp-buffer--live-edit-old-beg edit)))
         (inserted-newline
          (string-match-p
           "\n" (buffer-substring-no-properties beg end)))
         (active-bounds
          (ekp-buffer--para-bounds (cons (point) (point))))
         (active-beg (car active-bounds))
         (active-end (cdr active-bounds)))
    (ekp-buffer--release-live-state)
    (ekp-buffer--clear-projection old-start active-end)
    (when inserted-newline
      (let ((ekp-buffer--automatic-pass t))
        (dolist (range (ekp-buffer--paragraph-ranges old-start active-beg))
          (ekp-buffer--layout-paragraph
           (car range) (cdr range) ekp-buffer--auto-width))))
    (ekp-buffer--release-live-edit)
    (ekp-buffer--activate-live-paragraph active-beg active-end)
    (ekp-buffer--publish-live-prefix end)))

(defun ekp-buffer--baseline-logical-source (&optional start finish)
  "Return the transaction baseline without EKP projection.
When START and FINISH are non-nil, copy only that source interval."
  (let ((source (ekp-buffer--live-edit-baseline-source
                 ekp-buffer--live-edit)))
    (ekp-buffer--strip-owned-from-string
     (if start
         (substring source start finish)
       (copy-sequence source)))))

(defun ekp-buffer--live-baseline-restored-p ()
  "Return non-nil when source and foreign properties match the baseline."
  (let* ((edit ekp-buffer--live-edit)
         (beg (marker-position (ekp-buffer--live-edit-dirty-beg edit)))
         (end (marker-position (ekp-buffer--live-edit-dirty-end edit)))
         (start (ekp-buffer--live-edit-dirty-start edit))
         (finish (ekp-buffer--live-edit-dirty-finish edit)))
    (and beg end
         (equal-including-properties
          (buffer-substring beg end)
          (ekp-buffer--baseline-logical-source start finish)))))

(defun ekp-buffer--baseline-owned-properties (position)
  "Return EKP-owned properties at baseline string POSITION."
  (let* ((source (ekp-buffer--live-edit-baseline-source
                  ekp-buffer--live-edit))
         (properties (text-properties-at position source))
         (display (plist-get properties 'ekp-buffer--display))
         (prefix (plist-get properties 'ekp-buffer--line-prefix))
         owned)
    (when (plist-member properties 'ekp-justified)
      (setq owned
            (list 'ekp-justified
                  (plist-get properties 'ekp-justified))))
    (when display
      (setq owned
            (append owned
                    (list 'ekp-buffer--display display
                          'display (plist-get properties 'display)))))
    (when prefix
      (setq owned
            (append owned
                    (list 'ekp-buffer--line-prefix prefix
                          'line-prefix
                          (plist-get properties 'line-prefix)))))
    owned))

(defun ekp-buffer--restore-baseline-properties (base start finish)
  "Restore baseline EKP properties at BASE from START through FINISH."
  (let ((source (ekp-buffer--live-edit-baseline-source
                 ekp-buffer--live-edit))
        (position start))
    (ekp-buffer--naturalize-live-range (+ base start) (+ base finish))
    (let ((ekp-buffer--inhibit t))
      (with-silent-modifications
        (while (< position finish)
          (let* ((next (or (next-property-change
                            position source finish)
                           finish))
                 (owned
                  (ekp-buffer--baseline-owned-properties position)))
            (when owned
              (add-text-properties
               (+ base position) (+ base next) owned))
            (setq position next)))))))

(defun ekp-buffer--restore-baseline-markers (base)
  "Restore every committed marker to its baseline offset from BASE."
  (dolist (entry
           (ekp-buffer--live-edit-baseline-markers
            ekp-buffer--live-edit))
    (set-marker (car entry) (+ base (cdr entry)))))

(defun ekp-buffer--restore-live-baseline ()
  "Restore the exact committed projection and close the transaction."
  (let* ((edit ekp-buffer--live-edit)
         (state ekp-buffer--live-state)
         (base (marker-position (ekp-buffer--live-state-beg state))))
    (ekp-buffer--restore-baseline-properties
     base
     (ekp-buffer--live-edit-dirty-start edit)
     (ekp-buffer--live-edit-dirty-finish edit))
    (ekp-buffer--restore-baseline-markers base)
    (setf (ekp-buffer--live-state-source state)
          (ekp-buffer--baseline-logical-source)
          (ekp-buffer--live-state-key state)
          (ekp-buffer--live-edit-baseline-key edit)
          (ekp-buffer--live-state-plan state)
          (ekp-buffer--live-edit-baseline-plan edit)
          (ekp-buffer--live-state-signatures state)
          (ekp-buffer--live-edit-baseline-signatures edit)
          (ekp-buffer--live-state-active-index state)
          (ekp-buffer--live-edit-baseline-active-index edit)
          (ekp-buffer--live-state-spans state)
          (ekp-buffer--live-edit-baseline-spans edit))
    (ekp-buffer--release-live-edit)))

(defun ekp-buffer--live-row-crossed-p ()
  "Return non-nil when the edit endpoint crossed its native visual row."
  (let* ((edit ekp-buffer--live-edit)
         (start (marker-position (ekp-buffer--live-edit-row-start edit)))
         (end (marker-position (ekp-buffer--live-edit-edit-end edit)))
         (native (and end (ekp-buffer--native-row-start end))))
    (and start end
         (if native
             (> native start)
           (and (< start end)
                (> (ekp--measured-width
                    (ekp-buffer--logical-substring start end))
                   ekp-buffer--auto-width))))))

(defun ekp-buffer--ordinary-live-edit-finished (end)
  "Advance or retain the current live transaction ending at END."
  (cond
   ((ekp-buffer--live-baseline-restored-p)
    (ekp-buffer--restore-live-baseline))
   ((ekp-buffer--live-row-crossed-p)
    (ekp-buffer--publish-live-prefix end)
    (ekp-buffer--release-live-edit))))

(defun ekp-buffer--retry-composition (buffer generation beg end)
  "Retry BUFFER's live edit GENERATION from BEG through END."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ekp-buffer--composition-timer nil)
      (when (and ekp-auto-justify-mode
                 (= generation ekp-buffer--generation)
                 ekp-buffer--live-edit)
        (if (ekp-buffer--composing-p)
            (ekp-buffer--defer-composition beg end)
          (ekp-buffer--finish-live-edit beg end))))))

(defun ekp-buffer--defer-composition (beg end)
  "Defer the current live transaction from BEG through END."
  (when (timerp ekp-buffer--composition-timer)
    (cancel-timer ekp-buffer--composition-timer))
  (setq ekp-buffer--composition-timer
        (run-with-timer
         ekp-auto-justify-composition-delay nil
         #'ekp-buffer--retry-composition
         (current-buffer) ekp-buffer--generation beg end)))

(defun ekp-buffer--after-change (beg end _old-length)
  "Advance the stable live transaction after BEG through END changed."
  (when (and ekp-auto-justify-mode
             (not ekp-buffer--inhibit)
             ekp-buffer--live-edit)
    (set-marker
     (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit)
     end)
    (let ((hard-p
           (or (string-match-p
                "\n"
                (ekp-buffer--live-edit-old-fragment
                 ekp-buffer--live-edit))
               (string-match-p
                "\n" (buffer-substring-no-properties beg end)))))
      (cond
       ((ekp-buffer--composing-p)
        (ekp-buffer--defer-composition beg end))
       (hard-p
        (ekp-buffer--apply-hard-boundary-edit beg end))
       (t
        (ekp-buffer--ordinary-live-edit-finished end))))))

(defun ekp-buffer--finish-live-edit (beg end)
  "Finish a deferred source edit from BEG through END."
  (let ((hard-p
         (or (string-match-p
              "\n"
              (ekp-buffer--live-edit-old-fragment ekp-buffer--live-edit))
             (string-match-p
              "\n" (buffer-substring-no-properties beg end)))))
    (if hard-p
        (ekp-buffer--apply-hard-boundary-edit beg end)
      (ekp-buffer--ordinary-live-edit-finished end))))

(defun ekp-buffer--make-chunks (beg end)
  "Split BEG through END into whole-paragraph marker chunks."
  (let (chunks)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((start (point))
              (count 0))
          (while (and (< (point) end)
                      (< count ekp-auto-justify-chunk-size))
            (if (search-forward "\n" end 'move)
                (setq count (1+ count))
              (goto-char end)))
          (when (< start (point))
            (push (cons (copy-marker start)
                        (copy-marker (point) t))
                  chunks)))))
    (nreverse chunks)))

(defun ekp-buffer--cancel-pending ()
  "Cancel and discard lazy reflow state."
  (when (timerp ekp-buffer--chunk-timer)
    (cancel-timer ekp-buffer--chunk-timer))
  (setq ekp-buffer--chunk-timer nil)
  (ekp-buffer--release-marker-pairs (cdr ekp-buffer--pending))
  (setq ekp-buffer--pending nil))

(defun ekp-buffer--visible-span ()
  "Return the visible buffer span."
  (let ((window (get-buffer-window (current-buffer))))
    (if window
        (cons (window-start window)
              (or (window-end window t) (point-max)))
      (cons (point-min) (point-max)))))

(defun ekp-buffer--prioritize-visible ()
  "Move pending chunks intersecting the visible span to the front."
  (when (cdr ekp-buffer--pending)
    (pcase-let ((`(,beg . ,end) (ekp-buffer--visible-span)))
      (let* ((chunks (cdr ekp-buffer--pending))
             (visible (cl-remove-if-not
                       (lambda (chunk)
                         (and (< (car chunk) end) (> (cdr chunk) beg)))
                       chunks)))
        (setcdr ekp-buffer--pending
                (nconc visible
                       (cl-set-difference chunks visible :test #'eq)))))))

(defun ekp-buffer--enqueue-chunks (chunks)
  "Queue CHUNKS for lazy processing at the authoritative width."
  (when chunks
    (ekp-buffer--cancel-pending)
    (setq ekp-buffer--pending
          (cons ekp-buffer--auto-width chunks))
    (setq ekp-buffer--chunk-timer
          (run-with-timer 0.02 nil
                          #'ekp-buffer--process-chunk (current-buffer)))))

(defun ekp-buffer--process-one-chunk ()
  "Project and remove the next pending chunk."
  (let ((chunk (cadr ekp-buffer--pending))
        (width (car ekp-buffer--pending)))
    (save-restriction
      (widen)
      (let ((ekp-buffer--automatic-pass t))
        (ekp-justify-region (car chunk) (cdr chunk) width)))
    (setcdr ekp-buffer--pending (cddr ekp-buffer--pending))
    (ekp-buffer--release-marker-pairs (list chunk))))

(defun ekp-buffer--process-chunk (buffer)
  "Process lazy reflow work for BUFFER within the tick budget."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ekp-buffer--chunk-timer nil)
      (if (or (not ekp-auto-justify-mode)
              (null ekp-buffer--pending)
              (not (eql (car ekp-buffer--pending)
                        ekp-buffer--auto-width)))
          (ekp-buffer--cancel-pending)
        (let ((deadline (+ (float-time) ekp-auto-justify-tick-budget))
              (first t))
          (while (and (cdr ekp-buffer--pending)
                      (or first
                          (and (< (float-time) deadline)
                               (not (input-pending-p)))))
            (setq first nil)
            (ekp-buffer--process-one-chunk))
          (if (cdr ekp-buffer--pending)
              (setq ekp-buffer--chunk-timer
                    (run-with-timer
                     0.02 nil #'ekp-buffer--process-chunk buffer))
            (setq ekp-buffer--pending nil)))))))

(defun ekp-buffer--refresh-live-prefix (&optional boundary complete)
  "Restore live state after reflow at BOUNDARY.
Use point only for initial activation.  COMPLETE commits every row."
  (let ((position (or boundary (point))))
    (pcase-let ((`(,beg . ,end)
                 (ekp-buffer--para-bounds
                  (cons position position))))
      (ekp-buffer--activate-live-paragraph beg end))
    (ekp-buffer--publish-live-prefix position complete)))

(defun ekp-buffer--layout-outside-active
    (beg end width active-beg active-end)
  "Project BEG through END at WIDTH, excluding the active hard line.
ACTIVE-BEG through ACTIVE-END is published once by the live owner."
  (let ((left-end (min end active-beg))
        (right-beg (max beg (if (< active-end (point-max))
                                (1+ active-end)
                              active-end))))
    (when (< beg left-end)
      (ekp-justify-region beg left-end width))
    (when (< right-beg end)
      (ekp-justify-region right-beg end width))))

(defun ekp-buffer--lazy-reflow (width active-beg active-end)
  "Reflow visible text at WIDTH and queue text outside the active paragraph.
ACTIVE-BEG through ACTIVE-END remains owned by the refreshed live state."
  (pcase-let* ((`(,visible-beg . ,visible-end)
                 (ekp-buffer--visible-span))
                (`(,beg . ,end)
                 (ekp-buffer--para-bounds
                  (cons visible-beg visible-end)))
                (active-after
                 (if (< active-end (point-max))
                     (1+ active-end)
                   active-end))
                (ranges (list (cons end (point-max))
                              (cons (point-min) beg)))
                (chunks nil))
    (ekp-buffer--layout-outside-active
     beg end width active-beg active-end)
    (dolist (range ranges)
      (let ((start (car range))
            (finish (cdr range)))
        (when (< start (min finish active-beg))
          (setq chunks
                (nconc chunks
                       (ekp-buffer--make-chunks
                        start (min finish active-beg)))))
        (when (< (max start active-after) finish)
          (setq chunks
                (nconc chunks
                       (ekp-buffer--make-chunks
                        (max start active-after) finish))))))
    (ekp-buffer--enqueue-chunks chunks)))

(defun ekp-buffer--reflow (buffer width &optional _generation)
  "Reflow BUFFER to authoritative WIDTH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ekp-buffer--resize-timer nil)
      (when ekp-auto-justify-mode
        (let* ((complete (and ekp-buffer--live-state t))
               (saved-boundary
                (or (and ekp-buffer--live-edit
                         (marker-position
                          (ekp-buffer--live-edit-edit-end
                           ekp-buffer--live-edit)))
                    (and ekp-buffer--live-state
                         (marker-position
                          (ekp-buffer--live-state-prefix-end
                           ekp-buffer--live-state)))
                    (point)))
               (boundary (max (point-min)
                              (min saved-boundary (point-max)))))
          (setq ekp-buffer--auto-width width)
          (ekp-buffer--cancel-pending)
          (ekp-buffer--release-live-edit)
          (ekp-buffer--detach-live-state)
          (save-restriction
            (widen)
            (pcase-let ((`(,active-beg . ,active-end)
                         (ekp-buffer--para-bounds
                          (cons boundary boundary))))
              (ekp-buffer--clear-all)
              (let ((ekp-buffer--automatic-pass t))
                (if (< (buffer-size) ekp-auto-justify-lazy-threshold)
                    (ekp-buffer--layout-outside-active
                     (point-min) (point-max) width active-beg active-end)
                  (ekp-buffer--lazy-reflow
                   width active-beg active-end)))))
          (setq ekp-buffer--policy-context-snapshot
                (copy-tree (ekp-buffer--policy-context)))
          (ekp-buffer--refresh-live-prefix boundary complete))))))

(defun ekp-buffer--run-scheduled-reflow (buffer)
  "Reflow BUFFER using the latest authoritative width."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ekp-buffer--resize-timer nil)
      (when ekp-auto-justify-mode
        (ekp-buffer--reflow buffer (ekp-buffer--effective-width buffer))))))

(defun ekp-buffer--schedule-reflow ()
  "Debounce reflow; compute width when the timer fires."
  (when ekp-auto-justify-mode
    (let ((width (ekp-buffer--effective-width)))
      (when (and ekp-buffer--auto-width
                 (/= width ekp-buffer--auto-width))
        (when (timerp ekp-buffer--resize-timer)
          (cancel-timer ekp-buffer--resize-timer))
        (setq ekp-buffer--resize-timer
              (run-with-timer
               ekp-auto-justify-resize-delay nil
               #'ekp-buffer--run-scheduled-reflow
               (current-buffer)))))))

(defun ekp-buffer--reflow-for-policy-change (buffer &optional force)
  "Bump BUFFER generation and reflow once after a policy change.
When FORCE is non-nil, reflow even if the global policy context is
unchanged; region text properties are paragraph policy too."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when ekp-auto-justify-mode
        (when (timerp ekp-buffer--policy-reflow-timer)
          (cancel-timer ekp-buffer--policy-reflow-timer))
        (let ((before-context ekp-buffer--policy-reflow-context)
              (after-context (ekp-buffer--policy-context)))
          (setq-local ekp-buffer--policy-reflow-timer nil)
          (setq-local ekp-buffer--policy-reflow-context nil)
          (if (and (not force) (equal before-context after-context))
              (setq-local ekp-buffer--policy-context-snapshot
                          (copy-tree after-context))
            (let ((point-before (point))
                  (mark-before (and (mark t) (copy-marker (mark t))))
                  (mark-active-before mark-active)
                  (modified-before (buffer-modified-p))
                  (undo-before buffer-undo-list))
              (unwind-protect
                  (progn
                    (cl-incf ekp-buffer--generation)
                    (setq ekp-buffer--live-plan-cache nil)
                    (ekp-buffer--reflow
                     buffer (ekp-buffer--effective-width buffer))
                    (goto-char (min point-before (point-max)))
                    (when mark-before
                      (set-marker (mark-marker)
                                  (marker-position mark-before)))
                    (setq mark-active mark-active-before)
                    (set-buffer-modified-p modified-before)
                    (setq buffer-undo-list undo-before))
                (when mark-before (set-marker mark-before nil))))))))))

(defun ekp-buffer--schedule-policy-reflow (buffer before-context)
  "Schedule one post-set policy reflow for BUFFER from BEFORE-CONTEXT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when ekp-auto-justify-mode
        (unless (timerp ekp-buffer--policy-reflow-timer)
          (setq-local ekp-buffer--policy-reflow-context
                      (copy-tree before-context)))
        (when (timerp ekp-buffer--policy-reflow-timer)
          (cancel-timer ekp-buffer--policy-reflow-timer))
        (setq-local
         ekp-buffer--policy-reflow-timer
         (run-with-timer
          0 nil #'ekp-buffer--reflow-for-policy-change buffer))))))

(defun ekp-buffer--watched-raw-noop-p (symbol new-value where)
  "Return non-nil when setting SYMBOL to NEW-VALUE in WHERE is a raw no-op."
  (if (bufferp where)
      nil
    (equal (default-value symbol) new-value)))

(defun ekp-buffer--policy-variable-changed (symbol new-value operation where)
  "Schedule reflow after SYMBOL is set to NEW-VALUE by OPERATION in WHERE."
  (when (memq operation '(set makunbound))
    (unless (ekp-buffer--watched-raw-noop-p symbol new-value where)
      (if (bufferp where)
          (with-current-buffer where
            (ekp-buffer--schedule-policy-reflow
             where (or ekp-buffer--policy-context-snapshot
                       (ekp-buffer--policy-context))))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (ekp-buffer--schedule-policy-reflow
             buffer (or ekp-buffer--policy-context-snapshot
                        (ekp-buffer--policy-context)))))))))

(dolist (variable ekp-buffer--policy-variables)
  (unless (memq #'ekp-buffer--policy-variable-changed
                (get-variable-watchers variable))
    (add-variable-watcher variable #'ekp-buffer--policy-variable-changed)))

(defun ekp-buffer--on-resize (window-or-frame)
  "Schedule reflow for the buffer shown by WINDOW-OR-FRAME."
  (let ((window (cond
                 ((windowp window-or-frame) window-or-frame)
                 ((framep window-or-frame)
                  (get-buffer-window
                   (current-buffer) window-or-frame))
                 (t (get-buffer-window (current-buffer))))))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (ekp-buffer--schedule-reflow)))))

(defun ekp-buffer--on-window-change ()
  "Recheck the authoritative width after a window configuration change."
  (ekp-buffer--schedule-reflow))

(defun ekp-buffer--on-text-scale ()
  "Reflow after a text-scale change invalidates glyph metrics."
  (when (and ekp-auto-justify-mode ekp-buffer--auto-width)
    (ekp-clear-caches)
    (ekp-buffer--reflow
     (current-buffer) (ekp-buffer--effective-width))))

(defun ekp-buffer--on-font-context-change (&optional _theme)
  "Rebuild active projections after a theme or frame-font change."
  (ekp-clear-caches)
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when ekp-auto-justify-mode
          (setq ekp-buffer--live-plan-cache nil)
          (ekp-buffer--reflow
           buffer (ekp-buffer--effective-width buffer)))))))

(dolist (hook '(enable-theme-functions disable-theme-functions
                after-setting-font-hook))
  (add-hook hook #'ekp-buffer--on-font-context-change))

(defun ekp-buffer--on-scroll (_window _start)
  "Prioritize visible lazy work after scrolling."
  (when ekp-buffer--pending
    (run-with-timer 0 nil
                    (lambda (buffer)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (ekp-buffer--prioritize-visible))))
                    (current-buffer))))

(defun ekp-buffer--enable-native-wrap ()
  "Make native soft wrapping an owned auto-mode precondition."
  (unless ekp-buffer--wrap-state
    (setq ekp-buffer--wrap-state
          (list (local-variable-p 'truncate-lines)
                truncate-lines
                (local-variable-p 'truncate-partial-width-windows)
                truncate-partial-width-windows)))
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil))

(defun ekp-buffer--restore-native-wrap ()
  "Restore display variables saved by `ekp-buffer--enable-native-wrap'."
  (when ekp-buffer--wrap-state
    (pcase-let ((`(,lines-local ,lines ,partial-local ,partial)
                 ekp-buffer--wrap-state))
      (if lines-local
          (setq-local truncate-lines lines)
        (kill-local-variable 'truncate-lines))
      (if partial-local
          (setq-local truncate-partial-width-windows partial)
        (kill-local-variable 'truncate-partial-width-windows)))
    (setq ekp-buffer--wrap-state nil)))

(defun ekp-refill-paragraph ()
  "Apply a complete KP layout to the hard paragraph at point."
  (interactive "*")
  (ekp-buffer--release-live-edit)
  (ekp-buffer--release-live-state)
  (pcase-let ((`(,beg . ,end)
               (ekp-buffer--para-bounds (cons (point) (point)))))
  (ekp-justify-region
   beg end (or ekp-buffer--auto-width
               (ekp-buffer--effective-width)))))

(defvar-keymap ekp-auto-justify-mode-map
  :doc "Keymap for `ekp-auto-justify-mode'."
  "<remap> <fill-paragraph>" #'ekp-refill-paragraph)

(easy-menu-define ekp-auto-justify-mode-menu ekp-auto-justify-mode-map
  "Menu for `ekp-auto-justify-mode'."
  '("EKP"
    ["Justify Region or Paragraph" ekp-justify-region t]
    ["Unjustify Region or Paragraph" ekp-unjustify-region t]
    ["Justify Buffer" ekp-justify-buffer t]
    ["Unjustify Buffer" ekp-unjustify-buffer t]
    "--"
    ["Mark Region Normal Break" ekp-normal-break-region (use-region-p)]
    ["Enable Region Hyphenation" ekp-enable-hyphenation-region (use-region-p)]
    ["Disable Region Hyphenation" ekp-disable-hyphenation-region (use-region-p)]
    ["Clear Region Break Policy" ekp-clear-break-policy-region (use-region-p)]
    "--"
    ["Mark Region No-Break" ekp-no-break-region (use-region-p)]
    ["Clear No-Break Region" ekp-allow-break-region (use-region-p)]
    ["Mark Region Verbatim" ekp-verbatim-region (use-region-p)]
    ["Clear Verbatim Region" ekp-clear-verbatim-region (use-region-p)]
    "--"
    ["Diagnose Layout" ekp-diagnose t]))

(defun ekp-buffer--set-auto-hooks (enable)
  "Install auto-mode hooks when ENABLE is non-nil; otherwise remove them."
  (dolist (entry
           '((window-size-change-functions . ekp-buffer--on-resize)
             (window-configuration-change-hook . ekp-buffer--on-window-change)
             (window-scroll-functions . ekp-buffer--on-scroll)
             (text-scale-mode-hook . ekp-buffer--on-text-scale)
             (after-change-functions . ekp-buffer--after-change)
             (change-major-mode-hook . ekp-buffer--teardown)))
    (if enable
        (add-hook (car entry) (cdr entry) nil t)
      (remove-hook (car entry) (cdr entry) t))))

(defun ekp-buffer--disable-auto-mode ()
  "Remove auto-mode hooks and restore the buffer's prior display state."
  (ekp-buffer--set-auto-hooks nil)
  (ekp-buffer--teardown))

(defun ekp-buffer--enable-auto-mode ()
  "Install the complete auto-mode lifecycle or roll it back on error."
  (let (enabled)
    (unwind-protect
        (progn
          (ekp-buffer--enable-native-wrap)
          (setq ekp-buffer--auto-width (ekp-buffer--effective-width))
          (ekp-buffer--reflow (current-buffer) ekp-buffer--auto-width)
          (ekp-buffer--set-auto-hooks t)
          (ekp-buffer--install-integrations)
          (setq enabled t))
      (unless enabled
        (setq ekp-auto-justify-mode nil)
        (ekp-buffer--disable-auto-mode)))))

;;;###autoload
(define-minor-mode ekp-auto-justify-mode
  "Maintain a non-mutating KP display projection.
Completed paragraphs use the effective `ekp-buffer-measure': narrowest
window by default, a fixed width, or a `(max . N)' cap.
Manual no-break and verbatim properties last only for the current buffer
session."
  :lighter " EKP"
  :keymap ekp-auto-justify-mode-map
  (if ekp-auto-justify-mode
      (ekp-buffer--enable-auto-mode)
    (ekp-buffer--disable-auto-mode)))

(defun ekp-buffer--teardown ()
  "Cancel asynchronous work and remove every EKP projection."
  (when (timerp ekp-buffer--resize-timer)
    (cancel-timer ekp-buffer--resize-timer))
  (when (timerp ekp-buffer--composition-timer)
    (cancel-timer ekp-buffer--composition-timer))
  (when (timerp ekp-buffer--policy-reflow-timer)
    (cancel-timer ekp-buffer--policy-reflow-timer))
  (ekp-buffer--cancel-pending)
  (ekp-buffer--release-live-edit)
  (ekp-buffer--release-live-state)
  (setq ekp-buffer--resize-timer nil
        ekp-buffer--composition-timer nil
        ekp-buffer--policy-reflow-timer nil
        ekp-buffer--policy-reflow-context nil
        ekp-buffer--policy-context-snapshot nil
        ekp-buffer--auto-width nil
        ekp-buffer--live-plan-cache nil)
  (ekp-buffer--clear-all)
  (ekp-buffer--remove-integrations)
  (ekp-buffer--restore-native-wrap))

;;; Diagnostics

;;;###autoload
(defun ekp-diagnose ()
  "Report the authoritative width and any skipped projection conflicts."
  (interactive)
  (let* ((measure (ekp-buffer--measure-report))
         (width (plist-get measure :effective))
         (conflicts (length ekp-buffer--conflicts))
         (policy (ekp-buffer--policy-context width)))
    (message
     (concat "EKP: requested %S, narrowest %d, effective %d, "
             "overflow risk %s, %d conflict%s; "
             "inline-code %S, hyphenation %S, kinsoku %S, overlong %S")
     (plist-get measure :requested)
     (plist-get measure :narrowest)
     width
     (if (plist-get measure :overflow-risk) "yes" "no")
     conflicts (if (= conflicts 1) "" "s")
     (plist-get policy :inline-code-policy)
     (plist-get policy :hyphenation)
     (plist-get policy :kinsoku-profile)
     (plist-get policy :overlong-token-policy))
    (append measure
            (list :width width
                  :policy policy
                  :conflicts ekp-buffer--conflicts))))

(provide 'ekp-buffer)

;;; ekp-buffer.el ends here
