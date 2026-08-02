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
;;   ${EMACS} -Q -L . -L tests -l tests/ekp-gui-verify.el \
;;         -f ekp-gui-verify-matrix
;;                           runs the full matrix (plain, text-scale
;;                           up/down, face remap, narrow+scale) and
;;                           prints a PASS/FAIL table.
;;
;; Criterion: every justified line's rendered width equals the target
;; width (± `ekp-buffer-margin-pixel').  Verbatim paragraphs
;; (`ekp-verbatim') are exempt — code blocks pass through unwrapped by
;; design and may exceed a narrow window, like any code line.

;;; Code:

(require 'ekp)
(require 'ekp-buffer)
(require 'ekp-showcase)
(require 'ert)
(require 'subr-x)

(defun ekp-gui-verify--line-width (window base line)
  "Measure planned LINE in WINDOW at paragraph BASE."
  (car (window-text-pixel-size
        window
        (+ base (ekp-layout-line-source-start line))
        (+ base (ekp-layout-line-source-end line))
        t)))

(defun ekp-gui-verify--scan-span (window span margin)
  "Return measurement counters for SPAN in WINDOW using MARGIN."
  (let* ((plan (ekp-buffer--span-plan span))
         (records (ekp-buffer--span-lines span))
         (target (ekp-buffer--span-width span))
         (widest 0) (over 0) (wrong 0))
    (dolist (record records)
      (let* ((line (ekp-buffer--projected-line-line record))
             (base (marker-position
                    (ekp-buffer--projected-line-base record)))
             (pixel (ekp-gui-verify--line-width window base line)))
        (setq widest (max widest pixel))
        (when (> pixel (+ target margin))
          (setq over (1+ over)))
        (when (and (> (length (ekp-layout-line-gaps line)) 0)
                   (ekp-layout-line-break-kind line)
                   (> (abs (- pixel target)) margin))
          (setq wrong (1+ wrong)))))
    (list :widest widest :over over :wrong wrong
          :lines (length records)
          :static (and plan t))))

(defun ekp-gui-verify--scan (buffer &optional skip-predicate)
  "Measure every projected display line of BUFFER.
When SKIP-PREDICATE is non-nil, omit spans for which it returns non-nil."
  (with-current-buffer buffer
    (let* ((window (get-buffer-window buffer))
           (body (window-body-width window t))
           (target ekp-buffer--auto-width)
           (margin (max 2 ekp-buffer-margin-pixel))
           (widest 0) (over 0) (wrong 0) (lines 0))
      (dolist (span ekp-buffer--spans)
        (unless (and skip-predicate (funcall skip-predicate span))
          (let ((result (ekp-gui-verify--scan-span window span margin)))
            (setq widest (max widest (plist-get result :widest))
                  over (+ over (plist-get result :over))
                  wrong (+ wrong (plist-get result :wrong))
                  lines (+ lines (plist-get result :lines))))))
      (list :body body :target target :widest widest :over over
            :wrong wrong :lines lines :exempt 0
            :source-clean
            (and (not (text-property-not-all
                       (point-min) (point-max) 'ekp-soft-break nil))
                 (not (text-property-not-all
                       (point-min) (point-max) 'ekp-glue nil))
                 (null (overlays-in (point-min) (point-max))))
            :pass (and (= over 0) (= wrong 0) (> lines 0))))))

(defun ekp-gui-verify--atom-context ()
  "Return the protected showcase atom and its projected plan context."
  (let* ((atom-beg (text-property-any
                    (point-min) (point-max) 'ekp-no-break t))
         (atom-end (and atom-beg
                        (next-single-property-change
                         atom-beg 'ekp-no-break nil (point-max))))
         (span (and atom-beg
                    (seq-find
                     (lambda (candidate)
                       (and (<= (marker-position
                                  (ekp-buffer--span-beg candidate))
                                atom-beg)
                            (< atom-beg
                               (marker-position
                                (ekp-buffer--span-end candidate)))))
                     ekp-buffer--spans))))
    (unless (and atom-beg atom-end span)
      (error "Showcase atom projection is unavailable"))
    (list atom-beg atom-end span (ekp-buffer--span-plan span))))

(defun ekp-gui-verify-atom-adapter ()
  "Return primitive evidence for the showcase rigid-atom regression."
  (pcase-let* ((`(,atom-beg ,atom-end ,span ,plan)
                 (ekp-gui-verify--atom-context))
                (base (marker-position (ekp-buffer--span-beg span)))
                (atom-start (- atom-beg base))
                (atom-finish (- atom-end base))
                (prefix-end (1- atom-start))
                (lines (ekp-layout-plan-lines plan))
                (first (aref lines 0))
                (atom-line
                 (seq-find
                  (lambda (line)
                    (and (<= (ekp-layout-line-source-start line) atom-start)
                         (>= (ekp-layout-line-source-end line) atom-finish)))
                  (append lines nil))))
    `((width . ,ekp-showcase--width)
      (first_line_end . ,(ekp-layout-line-source-end first))
      (prefix_end . ,prefix-end)
      (prefix_screen_lines
       . ,(count-screen-lines base (+ base prefix-end)))
      (atom_intact . ,(if atom-line t :false))
      (source_exact
       . ,(if (equal (buffer-substring-no-properties
                      (point-min) (point-max))
                     (substring-no-properties (ekp-showcase--sample)))
              t :false))
      (overlays . ,(length (overlays-in (point-min) (point-max))))
      (engine_c . ,(if (and ekp-use-c-module (ekp--c-available-p))
                       t :false))
      (reflow_ms . ,(or ekp-showcase--last-ms 0)))))

(defun ekp-gui-verify-atom-assertions ()
  "Return assertions for `ekp-gui-verify-atom-adapter'."
  (let* ((adapter (ekp-gui-verify-atom-adapter))
         (value (lambda (key) (cdr (assq key adapter)))))
    `(((name . "full-prefix-before-atom")
       (passed . ,(if (>= (funcall value 'first_line_end)
                           (funcall value 'prefix_end)) t :false)))
      ((name . "prefix-not-vertical")
       (passed . ,(if (<= (funcall value 'prefix_screen_lines) 2)
                      t :false)))
      ((name . "atom-stays-on-one-plan-line")
       (passed . ,(funcall value 'atom_intact)))
      ((name . "logical-source-exact")
       (passed . ,(funcall value 'source_exact)))
      ((name . "zero-overlays")
       (passed . ,(if (= (funcall value 'overlays) 0) t :false)))
      ((name . "c-engine-active")
       (passed . ,(funcall value 'engine_c))))))

(defun ekp-gui-verify--source-text (beg end)
  "Return source text from BEG through END without properties."
  (buffer-substring-no-properties beg end))

(defun ekp-gui-verify--range-end (beg property)
  "Return the end of PROPERTY's run starting at BEG."
  (next-single-property-change beg property nil (point-max)))

(defun ekp-gui-verify--face-has-showcase-inline-role-p (face)
  "Return non-nil when FACE carries the dedicated showcase inline role."
  (cond
   ((memq face '(ekp-showcase-inline-code
                 ekp-showcase-inline-code-face))
    t)
   ((consp face)
    (seq-some #'ekp-gui-verify--face-has-showcase-inline-role-p face))))

(defun ekp-gui-verify--inline-role-at-p (pos)
  "Return non-nil when POS is the automatic showcase inline role."
  (or (get-text-property pos 'ekp-showcase-inline-code)
      (ekp-gui-verify--face-has-showcase-inline-role-p
       (get-text-property pos 'face))))

(defun ekp-gui-verify--face-code-range ()
  "Return the automatic showcase inline-code role range."
  (let ((pos (point-min))
        hit)
    (while (and (< pos (point-max)) (not hit))
      (if (and (ekp-gui-verify--inline-role-at-p pos)
               (not (get-text-property pos 'ekp-verbatim))
               (not (get-text-property pos 'ekp-no-break)))
          (setq hit
                (cons pos
                      (min (or (next-single-property-change
                                pos 'ekp-showcase-inline-code
                                nil (point-max))
                               (point-max))
                           (ekp-gui-verify--range-end pos 'face)
                           (or (next-single-property-change
                                pos 'ekp-no-break nil (point-max))
                               (point-max))
                           (or (next-single-property-change
                                pos 'ekp-verbatim nil (point-max))
                               (point-max)))))
        (setq pos (1+ pos))))
    hit))

(defun ekp-gui-verify--projected-property-free-p (beg end)
  "Return non-nil when BEG through END has no EKP projection props."
  (seq-every-p
   (lambda (property)
     (not (text-property-not-all beg end property nil)))
   '(ekp-justified ekp-buffer--display ekp-buffer--line-prefix
     display line-prefix)))

(defun ekp-gui-verify--unplanned-span-count ()
  "Return the number of installed spans that have no layout plan yet."
  (let ((count 0))
    (dolist (span ekp-buffer--spans count)
      (unless (ekp-buffer--span-plan span)
        (setq count (1+ count))))))

(defun ekp-gui-verify--current-live-prefix-p ()
  "Return non-nil when the committed live prefix still matches the buffer."
  (if-let* ((state ekp-buffer--live-state)
            (beg (marker-position (ekp-buffer--live-state-beg state))))
      (ekp-buffer--live-prefix-current-p
       beg
       (ekp-buffer--live-state-plan state)
       (or (ekp-buffer--live-state-active-index state) 0)
       (ekp-buffer--live-state-signatures state))
    t))

(defun ekp-gui-verify--unplanned-span-counts ()
  "Return raw, live, and stale nil-plan span counts."
  (let ((raw 0)
        (live 0)
        (stale 0)
        (live-spans (and ekp-buffer--live-state
                         (ekp-buffer--live-state-spans
                          ekp-buffer--live-state)))
        (live-current (ekp-gui-verify--current-live-prefix-p)))
    (dolist (span ekp-buffer--spans)
      (unless (ekp-buffer--span-plan span)
        (setq raw (1+ raw))
        (if (and live-current (memq span live-spans))
            (setq live (1+ live))
          (setq stale (1+ stale)))))
    `((raw . ,raw)
      (live . ,live)
      (stale . ,stale)
      (live_state_current . ,(if live-current t :false)))))

(defun ekp-gui-verify--isolated-cjk-cascades ()
  "Return runs of three or more consecutive isolated CJK source lines."
  (let (hits)
    (dolist (span ekp-buffer--spans)
      (when-let* ((plan (ekp-buffer--span-plan span))
                  (source (ekp-layout-plan-string plan)))
        (let (run)
          (dolist (line (append (ekp-layout-plan-lines plan) nil))
            (if (ekp-gui-verify--isolated-cjk-line-p line source)
                (push (substring source
                                 (ekp-layout-line-source-start line)
                                 (ekp-layout-line-source-end line))
                      run)
              (when (>= (length run) 3)
                (push (nreverse run) hits))
              (setq run nil)))
          (when (>= (length run) 3)
            (push (nreverse run) hits)))))
    (nreverse hits)))

(defun ekp-gui-verify--hard-atom-range ()
  "Return the explicit hard no-break range in the showcase."
  (when-let* ((beg (text-property-any
                    (point-min) (point-max) 'ekp-no-break t)))
    (cons beg (ekp-gui-verify--range-end beg 'ekp-no-break))))

(defun ekp-gui-verify--verbatim-range ()
  "Return the explicit verbatim block range in the showcase."
  (when-let* ((beg (text-property-any
                    (point-min) (point-max) 'ekp-verbatim t)))
    (cons beg (ekp-gui-verify--range-end beg 'ekp-verbatim))))

(defun ekp-gui-verify--span-for-position (position)
  "Return the projected span containing POSITION."
  (seq-find
   (lambda (span)
     (and (<= (marker-position (ekp-buffer--span-beg span)) position)
          (< position (marker-position (ekp-buffer--span-end span)))))
   ekp-buffer--spans))

(defun ekp-gui-verify--range-lines (range)
  "Return layout lines intersecting RANGE."
  (when-let* ((span (and range
                         (ekp-gui-verify--span-for-position (car range))))
              (plan (ekp-buffer--span-plan span)))
    (let* ((base (marker-position (ekp-buffer--span-beg span)))
           (start (- (car range) base))
           (finish (- (cdr range) base)))
      (seq-filter
       (lambda (line)
         (and (< (ekp-layout-line-source-start line) finish)
              (< start (ekp-layout-line-source-end line))))
       (append (ekp-layout-plan-lines plan) nil)))))

(defun ekp-gui-verify--range-plan-complete-p (range)
  "Return non-nil when RANGE's plan is nonempty and covers its source."
  (when-let* ((span (and range
                         (ekp-gui-verify--span-for-position (car range))))
              (plan (ekp-buffer--span-plan span))
              (lines (append (ekp-layout-plan-lines plan) nil)))
    (and (= (ekp-layout-line-source-start (car lines)) 0)
         (= (ekp-layout-line-source-end (car (last lines)))
            (length (ekp-layout-plan-string plan))))))

(defun ekp-gui-verify--range-intact-p (range)
  "Return non-nil when RANGE is contained in one planned line."
  (seq-some
   (lambda (line)
     (let* ((span (ekp-gui-verify--span-for-position (car range)))
            (base (marker-position (ekp-buffer--span-beg span)))
            (start (- (car range) base))
            (finish (- (cdr range) base)))
       (and (<= (ekp-layout-line-source-start line) start)
            (>= (ekp-layout-line-source-end line) finish))))
   (ekp-gui-verify--range-lines range)))

(defun ekp-gui-verify--range-break-text (range line)
  "Return LINE's break source inside RANGE, or nil when outside."
  (let ((start (ekp-layout-line-break-source-start line))
        (end (ekp-layout-line-break-source-end line)))
    (when-let* ((span (and start end (< start end)
                           (ekp-gui-verify--span-for-position
                            (car range)))))
      (let* ((base (marker-position (ekp-buffer--span-beg span)))
             (range-start (- (car range) base))
             (range-end (- (cdr range) base)))
        (when (and (<= range-start start) (<= end range-end))
          (substring (ekp-gui-verify--source-text (car range) (cdr range))
                     (- start range-start)
                     (- end range-start)))))))

(defun ekp-gui-verify--inline-legal-breaks-p (range)
  "Return non-nil when RANGE uses only source-space breaks."
  (and range
       (seq-every-p
        (lambda (line)
          (or (not (ekp-layout-line-break-kind line))
              (let ((break-text
                     (ekp-gui-verify--range-break-text range line)))
                (or (not break-text)
                    (string-match-p
                     "\\`[[:space:]\n\r\t]+\\'" break-text)))))
        (ekp-gui-verify--range-lines range))))

(defun ekp-gui-verify--inline-hyphenated-p (range)
  "Return non-nil when RANGE has a discretionary hyphenated line."
  (seq-some #'ekp-layout-line-hyphen-p
            (or (ekp-gui-verify--range-lines range) nil)))

(defun ekp-gui-verify--inline-line-evidence (range)
  "Return planned line evidence for inline RANGE."
  (let* ((span (and range (ekp-gui-verify--span-for-position (car range))))
         (plan (and span (ekp-buffer--span-plan span)))
         (source (and plan (ekp-layout-plan-string plan)))
         (base (and span (marker-position (ekp-buffer--span-beg span))))
         (range-start (and base (- (car range) base)))
         (range-end (and base (- (cdr range) base)))
         (lines (or (ekp-gui-verify--range-lines range) nil))
         (line-count (length lines))
         break-map internal-breaks)
    (dolist (line lines)
      (let* ((start (ekp-layout-line-break-source-start line))
             (end (ekp-layout-line-break-source-end line))
             (kind (ekp-layout-line-break-kind line))
             (internal (and source start end (< start end)
                            range-start range-end
                            (< range-start start)
                            (<= end range-end)))
             (source-space
              (and source start end (< start end)
                   (string-match-p
                    "\\`[[:space:]\n\r\t]+\\'"
                    (substring source start end))))
             (hyphen (and internal (ekp-layout-line-hyphen-p line)))
             (entry
              `((start . ,(or start 0))
                (end . ,(or end 0))
                (kind . ,(ekp-gui-verify--json-policy-value kind))
                (internal . ,(if internal t :false))
                (source_space . ,(if source-space t :false))
                (hyphen . ,(if hyphen t :false)))))
        (when (or kind start end)
          (push entry break-map))
        (when internal
          (push entry internal-breaks))))
    `((automatic_inline_line_count . ,line-count)
      (automatic_inline_wrapped . ,(if (> line-count 1) t :false))
      (automatic_inline_breaks_source_spaces
       . ,(if (and (> line-count 1)
                   internal-breaks
                   (seq-every-p
                    (lambda (entry)
                      (and (eq (cdr (assq 'source_space entry)) t)
                           (eq (cdr (assq 'hyphen entry)) :false)))
                    internal-breaks))
              t :false))
      (automatic_inline_break_map . ,(nreverse break-map)))))

(defun ekp-gui-verify--isolated-cjk-line-p (line source)
  "Return non-nil when LINE projects a single CJK character from SOURCE."
  (let ((text (string-trim
               (substring source
                          (ekp-layout-line-source-start line)
                          (ekp-layout-line-source-end line)))))
    (and (= (length text) 1)
         (let ((char (aref text 0)))
           (and (<= #x4E00 char) (<= char #x9FFF))))))

(defun ekp-gui-verify--isolated-cjk-lines ()
  "Return isolated CJK planned lines in projected showcase spans."
  (let (hits)
    (dolist (span ekp-buffer--spans)
      (when-let* ((plan (ekp-buffer--span-plan span))
                  (source (ekp-layout-plan-string plan)))
        (dolist (line (append (ekp-layout-plan-lines plan) nil))
          (when (ekp-gui-verify--isolated-cjk-line-p line source)
            (push (substring source
                             (ekp-layout-line-source-start line)
                             (ekp-layout-line-source-end line))
                  hits)))))
    (nreverse hits)))

(defun ekp-gui-verify--refresh-showcase-width (width)
  "Open the showcase and refresh it at WIDTH."
  (when (get-buffer "*ekp-showcase*")
    (kill-buffer "*ekp-showcase*"))
  (ekp-showcase)
  (with-current-buffer "*ekp-showcase*"
    (ekp-showcase-set-width width)
    (redisplay t)
    (current-buffer)))

(defun ekp-gui-verify--showcase-policy-evidence ()
  "Return G004 policy evidence for the current showcase buffer."
  (let* ((auto (ekp-gui-verify--face-code-range))
         (manual (ekp-gui-verify--hard-atom-range))
         (block (ekp-gui-verify--verbatim-range))
         (auto-lines (ekp-gui-verify--inline-line-evidence auto))
         (sample (substring-no-properties (ekp-showcase--sample))))
    `((width . ,ekp-showcase--width)
      (automatic_inline_present . ,(if auto t :false))
      (automatic_inline_plan_complete
       . ,(if (ekp-gui-verify--range-plan-complete-p auto) t :false))
      (automatic_inline_explicit_no_break
       . ,(if (and auto (text-property-not-all
                         (car auto) (cdr auto) 'ekp-no-break nil))
              t :false))
      (automatic_inline_literal_source
       . ,(if (and auto
                   (string-match-p "[[:space:]][^[:space:]]+[[:space:]]"
                                   (ekp-gui-verify--source-text
                                    (car auto) (cdr auto))))
              t :false))
      (automatic_inline_legal_breaks
       . ,(if (and auto
                   (ekp-gui-verify--inline-legal-breaks-p auto))
              t :false))
      (automatic_inline_no_hyphen
       . ,(if (and auto
                   (not (ekp-gui-verify--inline-hyphenated-p auto)))
              t :false))
      ,@auto-lines
      (manual_hard_atom_present . ,(if manual t :false))
      (manual_hard_atom_distinct
       . ,(if (and auto manual (not (equal auto manual))) t :false))
      (manual_hard_atom_explicit
       . ,(if (and manual
                   (not (text-property-not-all
                         (car manual) (cdr manual) 'ekp-no-break t)))
              t :false))
      (manual_hard_atom_intact
       . ,(if (and manual
                   (ekp-gui-verify--range-intact-p manual))
              t :false))
      (block_verbatim_present . ,(if block t :false))
      (block_verbatim_intact
       . ,(if (and block
                   (ekp-gui-verify--projected-property-free-p
                    (car block) (cdr block)))
              t :false))
      (isolated_cjk_cascades . ,(ekp-gui-verify--isolated-cjk-cascades))
      (isolated_cjk_lines . ,(ekp-gui-verify--isolated-cjk-lines))
      (source_exact
       . ,(if (equal (buffer-substring-no-properties
                      (point-min) (point-max))
                     sample)
              t :false))
      (overlays . ,(length (overlays-in (point-min) (point-max))))
      (engine_c . ,(if (and ekp-use-c-module (ekp--c-available-p))
                       t :false)))))

(defun ekp-gui-verify-showcase-policy-adapter (&optional width)
  "Return G004 machine evidence for the showcase policy contract."
  (with-current-buffer (ekp-gui-verify--refresh-showcase-width
                        (or width 280))
    (ekp-gui-verify--showcase-policy-evidence)))

(defun ekp-gui-verify-showcase-parshape-policy-adapter (&optional width)
  "Return showcase policy evidence with parshape enabled at WIDTH."
  (with-current-buffer (ekp-gui-verify--refresh-showcase-width
                        (or width 280))
    (setq-local ekp-showcase--parshape-on t)
    (ekp-showcase--refresh)
    (redisplay t)
    (ekp-gui-verify--showcase-policy-evidence)))

(defun ekp-gui-verify-showcase-policy-assertions ()
  "Return G004 assertions for `ekp-gui-verify-showcase-policy-adapter'."
  (let* ((adapter (ekp-gui-verify-showcase-policy-adapter 280))
         (value (lambda (key) (cdr (assq key adapter)))))
    `(((name . "automatic-inline-face-present")
       (passed . ,(funcall value 'automatic_inline_present)))
      ((name . "automatic-inline-plan-complete")
       (passed . ,(funcall value 'automatic_inline_plan_complete)))
      ((name . "automatic-inline-has-no-explicit-no-break")
       (passed . ,(if (eq (funcall value
                                    'automatic_inline_explicit_no_break)
                          :false)
                      t :false)))
      ((name . "automatic-inline-keeps-literal-source")
       (passed . ,(funcall value 'automatic_inline_literal_source)))
      ((name . "automatic-inline-breaks-only-legally")
       (passed . ,(funcall value 'automatic_inline_legal_breaks)))
      ((name . "automatic-inline-never-discretionary-hyphenates")
       (passed . ,(funcall value 'automatic_inline_no_hyphen)))
      ((name . "manual-hard-atom-is-distinct")
       (passed . ,(funcall value 'manual_hard_atom_distinct)))
      ((name . "manual-hard-atom-remains-explicit")
       (passed . ,(funcall value 'manual_hard_atom_explicit)))
      ((name . "manual-hard-atom-remains-intact")
       (passed . ,(funcall value 'manual_hard_atom_intact)))
      ((name . "block-code-stays-verbatim")
       (passed . ,(if (and (funcall value 'block_verbatim_present)
                           (funcall value 'block_verbatim_intact))
                      t :false)))
      ((name . "narrow-rendering-has-no-isolated-cjk-cascade")
       (passed . ,(if (null (funcall value 'isolated_cjk_cascades))
                      t :false)))
      ((name . "narrow-rendering-has-no-isolated-cjk-lines")
       (passed . ,(if (null (funcall value 'isolated_cjk_lines))
                      t :false)))
      ((name . "logical-source-exact")
       (passed . ,(funcall value 'source_exact)))
      ((name . "zero-overlays")
       (passed . ,(if (= (funcall value 'overlays) 0) t :false)))
      ((name . "c-engine-active")
       (passed . ,(funcall value 'engine_c))))))

(ert-deftest ekp-gui-verify-g004-showcase-policy-contract ()
  "The showcase separates automatic inline code from manual hard atoms."
  (let ((assertions (ekp-gui-verify-showcase-policy-assertions)))
    (dolist (assertion assertions)
      (ert-info ((cdr (assq 'name assertion)))
        (should (eq (cdr (assq 'passed assertion)) t))))))

(ert-deftest ekp-gui-verify-showcase-1d-rejects-isolated-cjk-lines ()
  "The default 168px showcase oracle rejects isolated CJK rows."
  (let* ((adapter (ekp-gui-verify-showcase-policy-adapter 168))
         (isolated (cdr (assq 'isolated_cjk_lines adapter))))
    (should (eq (cdr (assq 'automatic_inline_plan_complete adapter)) t))
    (should-not isolated)))

(ert-deftest ekp-gui-verify-showcase-parshape-rejects-isolated-cjk-lines ()
  "The parshape-on 280px showcase oracle rejects isolated CJK rows."
  (let* ((adapter (ekp-gui-verify-showcase-parshape-policy-adapter 280))
         (isolated (cdr (assq 'isolated_cjk_lines adapter))))
    (should (eq (cdr (assq 'automatic_inline_plan_complete adapter)) t))
    (should-not isolated)))

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
    (when (timerp ekp-buffer--resize-timer)
      (cancel-timer ekp-buffer--resize-timer))
    (ekp-buffer--reflow (current-buffer) (ekp-buffer--effective-width))
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
  (dolist (fn (list #'ekp-buffer--reflow
                    #'ekp-buffer--process-chunk))
    (cancel-function-timers fn))
  (when (get-buffer "*ekp-showcase*")
    (kill-buffer "*ekp-showcase*"))
  (ekp-showcase)
  (redisplay t)
  (with-current-buffer "*ekp-showcase*"
    (funcall setup)
    (redisplay t)
    (ekp-auto-justify-mode 1)
    (when (timerp ekp-buffer--resize-timer)
      (cancel-timer ekp-buffer--resize-timer))
    (ekp-buffer--reflow (current-buffer) (ekp-buffer--effective-width))
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

(defun ekp-gui-verify--pixel-width (window beg end)
  "Return displayed width from BEG to END in WINDOW."
  (car (window-text-pixel-size window beg end t)))

(defun ekp-gui-verify--exact-ascii-glue (window)
  "Return failed 1–64px ASCII glue targets in WINDOW."
  (let ((natural (ekp-gui-verify--pixel-width window 2 3))
        failures)
    (dotimes (index 64)
      (let ((target (1+ index)))
        (with-silent-modifications
          (ekp-buffer--remove-properties 1 4)
          (ekp-buffer--put-display
           2 3 (ekp-buffer--space-display natural target)))
        (redisplay t)
        (let ((actual (- (ekp-gui-verify--pixel-width window 1 4)
                         (ekp-gui-verify--pixel-width window 1 2)
                         (ekp-gui-verify--pixel-width window 3 4))))
          (unless (= actual target)
            (push (cons target actual) failures)))))
    (nreverse failures)))

(defun ekp-gui-verify--exact-cjk-glue (window)
  "Return failed 1–64px zero-source CJK glue targets in WINDOW."
  (let ((first (ekp-gui-verify--pixel-width window 1 2))
        (second (ekp-gui-verify--pixel-width window 2 3))
        failures)
    (dotimes (index 64)
      (let ((target (1+ index)))
        (with-silent-modifications
          (ekp-buffer--remove-properties 1 3)
          (ekp-buffer--put-display
           1 2 (ekp-buffer--min-width (+ first target))))
        (redisplay t)
        (let ((actual (- (ekp-gui-verify--pixel-width window 1 3)
                         first second)))
          (unless (= actual target)
            (push (cons target actual) failures)))))
    (nreverse failures)))

(defun ekp-gui-verify--hyphen-width (text)
  "Return a width that chooses a discretionary hyphen in TEXT."
  (seq-find
   (lambda (width)
     (seq-some
      (lambda (line) (ekp-layout-line-hyphen-p line))
      (append (ekp-layout-plan-lines
               (ekp-layout-plan text width))
              nil)))
   (number-sequence 60 220 4)))

(defun ekp-gui-verify--hyphen-owner ()
  "Return the source position owning the displayed discretionary hyphen."
  (seq-find
   (lambda (position)
     (let ((display (get-text-property position 'display)))
       (and (stringp display)
            (string-match-p "-\n"
                            (substring-no-properties display)))))
   (number-sequence (point-min) (1- (point-max)))))

(defun ekp-gui-verify--editor-semantics (window owner text)
  "Verify point, region, and mouse semantics in WINDOW at OWNER for TEXT."
  (let ((point-visible
         (cl-loop
          for position from (point-min) to (point-max)
          always
          (progn
            (goto-char position)
            (redisplay t)
            (pos-visible-in-window-p position window t)))))
    (goto-char (point-min))
    (let ((vertical (vertical-motion 1 window)))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (redisplay t)
      (let* ((xy (pos-visible-in-window-p owner window t))
             (posn (and xy
                        (posn-at-x-y (car xy) (cadr xy) window)))
             (mouse (and posn (posn-point posn))))
        (list
         :point-visible point-visible
         :vertical-motion vertical
         :vertical-source-position (point)
         :region-source
         (and (use-region-p)
              (equal (buffer-substring-no-properties
                      (region-beginning) (region-end))
                     text))
         :mouse-source-position
         (and (integer-or-marker-p mouse)
              (<= (point-min) mouse (point-max))))))))

(defun ekp-gui-verify--hyphen-case (window text)
  "Return verification data for display-only hyphenation of TEXT in WINDOW."
  (let ((width (ekp-gui-verify--hyphen-width text)))
    (ekp-justify-region (point-min) (point-max) width)
    (redisplay t)
    (let ((owner (ekp-gui-verify--hyphen-owner)))
      (append
       (list
        :width width
        :source-clean
        (equal (substring-no-properties (buffer-string)) text)
        :source-lines (cl-count ?\n (buffer-string))
        :screen-lines (count-screen-lines (point-min) (point-max))
        :hyphen-display (and owner t)
        :overlays (length (overlays-in (point-min) (point-max)))
        :scan (ekp-gui-verify--scan (current-buffer))
        :window-width (window-body-width window t))
       (ekp-gui-verify--editor-semantics window owner text)))))

(defun ekp-gui-verify--display-properties-pass-p (result)
  "Return non-nil when display-property verification RESULT passes."
  (let ((hyphen (plist-get result :hyphen)))
    (and (null (plist-get result :ascii-failures))
         (null (plist-get result :cjk-failures))
         (plist-get hyphen :source-clean)
         (= (plist-get hyphen :source-lines) 0)
         (> (plist-get hyphen :screen-lines) 1)
         (plist-get hyphen :hyphen-display)
         (= (plist-get hyphen :overlays) 0)
         (plist-get hyphen :point-visible)
         (> (plist-get hyphen :vertical-motion) 0)
         (plist-get hyphen :region-source)
         (plist-get hyphen :mouse-source-position)
         (plist-get (plist-get hyphen :scan) :pass))))

;;;###autoload
(defun ekp-gui-verify-display-properties ()
  "Verify exact text-property glue and display-only hyphenation."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (let ((buffer (generate-new-buffer "*ekp-display-properties*"))
        result)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (delete-other-windows)
          (insert "a b")
          (redisplay t)
          (let ((ascii (ekp-gui-verify--exact-ascii-glue
                        (selected-window))))
            (erase-buffer)
            (insert "中文")
            (redisplay t)
            (let ((cjk (ekp-gui-verify--exact-cjk-glue
                        (selected-window))))
              (erase-buffer)
              (let ((text
                     "extraordinary hyphenation demonstration paragraph"))
                (insert text)
                (setq result
                      (list :ascii-failures ascii
                            :cjk-failures cjk
                            :hyphen
                            (ekp-gui-verify--hyphen-case
                             (selected-window) text)))
                (setq result
                      (plist-put
                       result :pass
                       (ekp-gui-verify--display-properties-pass-p
                        result)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (when (called-interactively-p 'interactive)
      (message "ekp display-property verification: %s"
               (if (plist-get result :pass) "PASS" "FAIL")))
    result))

(defvar-local ekp-gui-verify--live-original nil)
(defvar-local ekp-gui-verify--live-expected nil)
(defvar-local ekp-gui-verify--live-suffix " overflow")
(defvar-local ekp-gui-verify--live-yank " pasted 中文 semantic prefix")
(defvar-local ekp-gui-verify--live-original-signatures nil)
(defvar-local ekp-gui-verify--live-original-projection-hash nil)
(defvar-local ekp-gui-verify--live-original-active-index nil)
(defvar-local ekp-gui-verify--live-original-prefix-end nil)
(defvar-local ekp-gui-verify--live-original-width nil)
(defvar-local ekp-gui-verify--live-original-screen-lines nil)
(defvar-local ekp-gui-verify--live-post-crossing-state nil)
(defvar-local ekp-gui-verify--live-motion-generation nil)
(defvar-local ekp-gui-verify--live-motion-cache-size nil)
(defvar-local ekp-gui-verify--live-motion-state nil)
(defvar-local ekp-gui-verify--live-middle-position nil)
(defvar-local ekp-gui-verify--live-middle-later-beg nil)
(defvar-local ekp-gui-verify--live-middle-later-end nil)
(defvar-local ekp-gui-verify--live-middle-later-hash nil)
(defvar-local ekp-gui-verify--live-middle-plan nil)
(defvar-local ekp-gui-verify--live-middle-spans nil)
(defvar-local ekp-gui-verify--live-stage "setup")
(defvar-local ekp-gui-verify--showcase-temporal-source nil)
(defvar-local ekp-gui-verify--showcase-temporal-before-generation nil)
(defvar-local ekp-gui-verify--showcase-temporal-before-projection nil)

(defun ekp-gui-verify--timer-state (timer)
  "Return a JSON-compatible state for TIMER."
  (if (timerp timer) t :false))

(defun ekp-gui-verify--current-source-hash ()
  "Return the current buffer's logical source hash."
  (secure-hash 'sha256
               (buffer-substring-no-properties
                (point-min) (point-max))))

(defun ekp-gui-verify--json-policy-value (value)
  "Return VALUE as a JSON-compatible policy primitive."
  (if (symbolp value)
      (and value (symbol-name value))
    value))

(defun ekp-gui-verify--current-policy-context ()
  "Return a compact JSON-compatible policy context summary."
  (let ((context (ekp-buffer--policy-context ekp-buffer--auto-width)))
    `((inline_code . ,(ekp-gui-verify--json-policy-value
                       (plist-get context :inline-code-policy)))
      (hyphenation . ,(ekp-gui-verify--json-policy-value
                       (plist-get context :hyphenation)))
      (kinsoku . ,(ekp-gui-verify--json-policy-value
                   (plist-get context :kinsoku-profile)))
      (overlong . ,(ekp-gui-verify--json-policy-value
                    (plist-get context :overlong-token-policy)))
      (width . ,(or (plist-get context :width) 0)))))

(defun ekp-gui-verify-showcase-temporal-adapter
    (&optional stage completed)
  "Return current G004 temporal showcase state for STAGE."
  (let* ((policy (ekp-gui-verify--showcase-policy-evidence))
         (source-hash (ekp-gui-verify--current-source-hash))
         (unplanned (ekp-gui-verify--unplanned-span-counts))
         (raw-unplanned (cdr (assq 'raw unplanned)))
         (live-unplanned (cdr (assq 'live unplanned)))
         (stale-unplanned (cdr (assq 'stale unplanned)))
         (live-state-current (cdr (assq 'live_state_current unplanned)))
         (generation-delta
          (and ekp-gui-verify--showcase-temporal-before-generation
               (- ekp-buffer--generation
                  ekp-gui-verify--showcase-temporal-before-generation))))
    `((stage . ,(or stage "current"))
      (width . ,ekp-showcase--width)
      (selected_target_window
       . ,(if (and (= (length (seq-remove #'window-minibuffer-p
                                           (window-list)))
                      1)
                   (eq (window-buffer (selected-window))
                       (current-buffer)))
              t :false))
      (source_hash . ,source-hash)
      (source_unchanged
       . ,(if (or (not ekp-gui-verify--showcase-temporal-source)
                  (equal source-hash
                         ekp-gui-verify--showcase-temporal-source))
              t :false))
      (projection_hash . ,(ekp-gui-verify--projection-hash))
      (projection_published . ,(if ekp-buffer--spans t :false))
      (current_projection
       . ,(if (and ekp-buffer--spans
                   (= stale-unplanned 0)
                   (eq live-state-current t))
              t :false))
      (projection_changed
       . ,(if (and ekp-gui-verify--showcase-temporal-before-projection
                   (not (equal (ekp-gui-verify--projection-hash)
                               ekp-gui-verify--showcase-temporal-before-projection)))
              t :false))
      (generation . ,ekp-buffer--generation)
      (generation_delta . ,(or generation-delta 0))
      (resize_timer . ,(ekp-gui-verify--timer-state
                        ekp-buffer--resize-timer))
      (policy_timer . ,(ekp-gui-verify--timer-state
                        ekp-buffer--policy-reflow-timer))
      (unplanned_spans . ,raw-unplanned)
      (live_unplanned_spans . ,live-unplanned)
      (stale_unplanned_spans . ,stale-unplanned)
      (live_state_current . ,live-state-current)
      (timers_settled
       . ,(if (and (not (timerp ekp-buffer--resize-timer))
                   (not (timerp ekp-buffer--policy-reflow-timer)))
              t :false))
      (pending_chunks . ,(if ekp-buffer--pending t :false))
      (overlays . ,(cdr (assq 'overlays policy)))
      (engine_c . ,(cdr (assq 'engine_c policy)))
      (automatic_inline_plan_complete
       . ,(cdr (assq 'automatic_inline_plan_complete policy)))
      (automatic_inline_line_count
       . ,(cdr (assq 'automatic_inline_line_count policy)))
      (automatic_inline_wrapped
       . ,(cdr (assq 'automatic_inline_wrapped policy)))
      (automatic_inline_breaks_source_spaces
       . ,(cdr (assq 'automatic_inline_breaks_source_spaces policy)))
      (automatic_inline_break_map
       . ,(cdr (assq 'automatic_inline_break_map policy)))
      (isolated_cjk_cascades
       . ,(cdr (assq 'isolated_cjk_cascades policy)))
      (isolated_cjk_lines
       . ,(cdr (assq 'isolated_cjk_lines policy)))
      (policy . ,(ekp-gui-verify--current-policy-context))
      (completed . ,(if completed t :false)))))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-setup ()
  "Create a clean single-window showcase temporal verification buffer."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (cancel-function-timers #'ekp-buffer--reflow)
  (cancel-function-timers #'ekp-buffer--process-chunk)
  (cancel-function-timers #'ekp-buffer--reflow-for-policy-change)
  (when (get-buffer "*ekp-showcase*")
    (kill-buffer "*ekp-showcase*"))
  (delete-other-windows)
  (ekp-showcase)
  (delete-other-windows)
  (with-current-buffer "*ekp-showcase*"
    (when ekp-auto-justify-mode
      (ekp-auto-justify-mode -1))
    (setq ekp-gui-verify--showcase-temporal-source nil
          ekp-gui-verify--showcase-temporal-before-generation nil
          ekp-gui-verify--showcase-temporal-before-projection nil)
    (kill-local-variable 'ekp-inline-code-policy)
    (ekp-showcase-set-width 480)
    (redisplay t)
    (ekp-gui-verify-showcase-temporal-adapter "setup" nil)))

(defun ekp-gui-verify--temporal-set-width (width stage)
  "Set showcase WIDTH and record temporal STAGE."
  (with-current-buffer "*ekp-showcase*"
    (ekp-showcase-set-width width)
    (redisplay t)
    (ekp-gui-verify-showcase-temporal-adapter stage nil)))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-width-480 ()
  "Record manual showcase width 480 in the temporal scenario."
  (interactive)
  (ekp-gui-verify--temporal-set-width 480 "width-480"))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-width-280 ()
  "Record manual showcase width 280 in the temporal scenario."
  (interactive)
  (ekp-gui-verify--temporal-set-width 280 "width-280"))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-width-340 ()
  "Record manual showcase width 340 in the temporal scenario."
  (interactive)
  (ekp-gui-verify--temporal-set-width 340 "width-340"))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-policy-before ()
  "Enable auto layout and capture the policy-change before checkpoint."
  (interactive)
  (with-current-buffer "*ekp-showcase*"
    (ekp-auto-justify-mode 1)
    (let ((deadline (+ (float-time) 2.0)))
      (while (and (or (timerp ekp-buffer--resize-timer)
                      ekp-buffer--pending)
                  (< (float-time) deadline))
        (sit-for 0.05)))
    (setq ekp-gui-verify--showcase-temporal-source
          (ekp-gui-verify--current-source-hash)
          ekp-gui-verify--showcase-temporal-before-generation
          ekp-buffer--generation
          ekp-gui-verify--showcase-temporal-before-projection
          (ekp-gui-verify--projection-hash))
    (redisplay t)
    (ekp-gui-verify-showcase-temporal-adapter "policy-before" nil)))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-policy-immediate ()
  "Apply a local inline-code policy change and capture pending timer state."
  (interactive)
  (with-current-buffer "*ekp-showcase*"
    (setq-local ekp-inline-code-policy 'normal)
    (ekp-gui-verify-showcase-temporal-adapter "policy-immediate" nil)))

;;;###autoload
(defun ekp-gui-verify-showcase-temporal-policy-settled ()
  "Wait for the real policy timer and capture the settled checkpoint."
  (interactive)
  (with-current-buffer "*ekp-showcase*"
    (let ((deadline (+ (float-time) 2.0)))
      (while (and (or (timerp ekp-buffer--policy-reflow-timer)
                      (timerp ekp-buffer--resize-timer)
                      ekp-buffer--pending)
                  (< (float-time) deadline))
        (sit-for 0.05)))
    (redisplay t)
    (ekp-gui-verify-showcase-temporal-adapter "policy-settled" t)))

(defun ekp-gui-verify-showcase-temporal-assertions (state)
  "Return assertions for temporal showcase adapter STATE."
  (let* ((stage (cdr (assq 'stage state)))
         (immediate-p (equal stage "policy-immediate"))
         (manual-wrap-stage-p (member stage '("width-280" "width-340")))
         (completed-p (eq (cdr (assq 'completed state)) t))
         (policy-timer (cdr (assq 'policy_timer state)))
         (unplanned-spans (cdr (assq 'unplanned_spans state)))
         (stale-unplanned-spans
          (cdr (assq 'stale_unplanned_spans state)))
         (generation-delta (cdr (assq 'generation_delta state)))
         (immediate-observed-p
          (or (eq policy-timer t)
              (and (eq policy-timer :false)
                   (= (or generation-delta 0) 1))))
         (policy (cdr (assq 'policy state))))
    `(((name . "policy-timer-state-valid")
       (passed . ,(if (memq policy-timer '(t :false))
                      t :false)))
      ((name . "automatic-inline-plan-complete")
       (passed . ,(if (eq (cdr (assq 'automatic_inline_plan_complete state)) t)
                      t :false)))
      ((name . "immediate-policy-timer-pending")
       (passed . ,(if (or (not immediate-p)
                          immediate-observed-p)
                      t :false)))
      ((name . "immediate-unplanned-spans-only-while-policy-pending")
       (passed . ,(if (or (not immediate-p)
                          (= (or unplanned-spans 0) 0)
                          immediate-observed-p)
                      t :false)))
      ((name . "manual-width-automatic-inline-wrapped")
       (passed . ,(if (or (not manual-wrap-stage-p)
                          (eq (cdr (assq 'automatic_inline_wrapped state)) t))
                      t :false)))
      ((name . "manual-width-inline-breaks-use-source-spaces")
       (passed . ,(if (or (not manual-wrap-stage-p)
                          (eq (cdr (assq 'automatic_inline_breaks_source_spaces
                                          state))
                              t))
                      t :false)))
      ((name . "settled-policy-timer-cleared")
       (passed . ,(if (or (not completed-p)
                          (eq policy-timer :false))
                      t :false)))
      ((name . "settled-resize-timer-cleared")
       (passed . ,(if (or (not completed-p)
                          (eq (cdr (assq 'resize_timer state)) :false))
                      t :false)))
      ((name . "settled-no-stale-resize-or-policy-timers")
       (passed . ,(if (or (not completed-p)
                          (eq (cdr (assq 'timers_settled state)) t))
                      t :false)))
      ((name . "settled-exactly-one-generation-reflow")
       (passed . ,(if (or (not completed-p)
                          (= (or generation-delta 0) 1))
                      t :false)))
      ((name . "settled-source-hash-unchanged")
       (passed . ,(if (or (not completed-p)
                          (eq (cdr (assq 'source_unchanged state)) t))
                      t :false)))
      ((name . "settled-effective-inline-policy-normal")
       (passed . ,(if (or (not completed-p)
                          (equal (cdr (assq 'inline_code policy)) "normal"))
                      t :false)))
      ((name . "settled-zero-stale-unplanned-spans")
       (passed . ,(if (or (not completed-p)
                          (and (numberp stale-unplanned-spans)
                               (= stale-unplanned-spans 0)))
                      t :false)))
      ((name . "settled-current-projection")
       (passed . ,(if (or (not completed-p)
                          (eq (cdr (assq 'current_projection state)) t))
                      t :false)))
      ((name . "settled-zero-overlays")
       (passed . ,(if (= (cdr (assq 'overlays state)) 0) t :false)))
      ((name . "settled-projection-published")
       (passed . ,(if (eq (cdr (assq 'projection_published state)) t)
                      t :false)))
      ((name . "settled-c-engine-active")
       (passed . ,(if (eq (cdr (assq 'engine_c state)) t)
                      t :false)))
      ((name . "settled-no-isolated-cjk-cascade")
       (passed . ,(if (null (cdr (assq 'isolated_cjk_cascades state)))
                      t :false)))
      ((name . "settled-no-isolated-cjk-lines")
       (passed . ,(if (null (cdr (assq 'isolated_cjk_lines state)))
                      t :false)))
      ((name . "settled-one-selected-target-window")
       (passed . ,(if (eq (cdr (assq 'selected_target_window state)) t)
                      t :false)))
      ((name . "settled-no-pending-chunks")
       (passed . ,(if (or (not completed-p)
                          (eq (cdr (assq 'pending_chunks state)) :false))
                      t :false))))))

(ert-deftest ekp-gui-verify-g004-temporal-skips-unplanned-spans ()
  "Treat pending nil-plan spans as observable state, not verifier failure."
  (with-temp-buffer
    (insert "中文 abc")
    (let ((ekp-buffer--spans
           (list (ekp-buffer--span-create
                  :beg (copy-marker (point-min))
                  :end (copy-marker (point-max))
                  :width 10
                  :plan nil
                  :lines nil))))
      (should (= (ekp-gui-verify--unplanned-span-count) 1))
      (should (null (ekp-gui-verify--isolated-cjk-cascades)))
      (should (null (ekp-gui-verify--isolated-cjk-lines)))
      (should (null (ekp-gui-verify--range-lines
                     (cons (point-min) (point-max))))))))

(defun ekp-gui-verify--json-primitive-tree-p (value)
  "Return non-nil when VALUE contains only JSON-compatible primitives."
  (cond
   ((or (null value) (eq value t) (eq value :false)
        (stringp value) (numberp value))
    t)
   ((and (consp value) (symbolp (car value)))
    (and (symbolp (car value))
         (ekp-gui-verify--json-primitive-tree-p (cdr value))))
   ((listp value)
    (seq-every-p #'ekp-gui-verify--json-primitive-tree-p value))))

(ert-deftest ekp-gui-verify-g004-policy-context-is-json-primitive ()
  "Policy context exposes stable JSON primitive values."
  (let ((ekp-buffer--auto-width 280)
        (ekp-inline-code-policy 'no-hyphen)
        (ekp-hyphenation 'auto)
        (ekp-kinsoku-profile 'liang)
        (ekp-overlong-token-policy 'emergency))
    (let ((policy (ekp-gui-verify--current-policy-context)))
      (should (equal (cdr (assq 'inline_code policy)) "no-hyphen"))
      (should (equal (cdr (assq 'hyphenation policy)) "auto"))
      (should (equal (cdr (assq 'kinsoku policy)) "liang"))
      (should (equal (cdr (assq 'overlong policy)) "emergency"))
      (should (ekp-gui-verify--json-primitive-tree-p policy)))))

(ert-deftest ekp-gui-verify-g004-inline-break-map-ignores-external-break ()
  "Final external line breaks do not invalidate internal inline break proof."
  (with-temp-buffer
    (let* ((source (make-string 220 ?x))
           (range (cons (+ (point-min) 108) (+ (point-min) 198)))
           (lines
            (vector
             (ekp-layout-line--create
              :source-start 108 :source-end 136
              :break-kind 'space
              :break-source-start 136 :break-source-end 137)
             (ekp-layout-line--create
              :source-start 137 :source-end 173
              :break-kind 'space
              :break-source-start 173 :break-source-end 174)
             (ekp-layout-line--create
              :source-start 174 :source-end 198
              :break-kind 'space
              :break-source-start 198 :break-source-end 199)))
           (plan (ekp-layout-plan--create
                  :string source
                  :lines lines))
           ekp-buffer--spans)
      (insert source)
      (aset source 136 ?\s)
      (aset source 173 ?\s)
      (aset source 198 ?\s)
      (setq ekp-buffer--spans
            (list (ekp-buffer--span-create
                   :beg (copy-marker (point-min))
                   :end (copy-marker (point-max))
                   :width 280
                   :plan plan)))
      (let ((evidence (ekp-gui-verify--inline-line-evidence range)))
        (should (eq (cdr (assq 'automatic_inline_wrapped evidence)) t))
        (should (eq (cdr (assq 'automatic_inline_breaks_source_spaces
                               evidence))
                    t))
        (should (ekp-gui-verify--json-primitive-tree-p
                 (cdr (assq 'automatic_inline_break_map evidence))))
        (should (equal
                 (cdr (assq 'kind
                            (car (cdr (assq 'automatic_inline_break_map
                                            evidence)))))
                 "space"))))))

(ert-deftest ekp-gui-verify-g004-temporal-live-nil-plan-is-current ()
  "Settled temporal assertions allow current live nil-plan spans."
  (let* ((state '((stage . "policy-settled")
                  (width . 280)
                  (selected_target_window . t)
                  (source_unchanged . t)
                  (projection_published . t)
                  (generation_delta . 1)
                  (resize_timer . :false)
                  (policy_timer . :false)
                  (unplanned_spans . 1)
                  (live_unplanned_spans . 1)
                  (stale_unplanned_spans . 0)
                  (current_projection . t)
                  (timers_settled . t)
                  (pending_chunks . :false)
                  (overlays . 0)
                  (engine_c . t)
                  (automatic_inline_plan_complete . t)
                  (automatic_inline_line_count . 2)
                  (automatic_inline_wrapped . t)
                  (automatic_inline_breaks_source_spaces . t)
                  (isolated_cjk_cascades)
                  (isolated_cjk_lines)
                  (policy . ((inline_code . "normal")))
                  (completed . t)))
         (assertions (ekp-gui-verify-showcase-temporal-assertions state)))
    (dolist (name '("settled-zero-stale-unplanned-spans"
                    "settled-current-projection"))
      (let ((assertion
             (seq-find
              (lambda (candidate)
                (equal (cdr (assq 'name candidate)) name))
              assertions)))
        (should assertion)
        (should (eq (cdr (assq 'passed assertion)) t))))))

(ert-deftest ekp-gui-verify-g004-temporal-unplanned-span-policy ()
  "Allow unplanned spans only during the immediate pending policy stage."
  (let* ((base '((width . 280)
                 (selected_target_window . t)
                 (source_unchanged . t)
                 (projection_published . t)
                 (generation_delta . 1)
                 (resize_timer . :false)
                 (pending_chunks . :false)
                 (overlays . 0)
                 (engine_c . t)
                 (automatic_inline_plan_complete . t)
                 (live_unplanned_spans . 0)
                 (stale_unplanned_spans . 0)
                 (live_state_current . t)
                 (automatic_inline_line_count . 2)
                 (automatic_inline_wrapped . t)
                 (automatic_inline_breaks_source_spaces . t)
                 (automatic_inline_break_map)
                 (isolated_cjk_cascades)
                 (isolated_cjk_lines)
                 (policy . ((inline_code . "normal")))))
         (assertion-passed
          (lambda (state name)
            (let ((assertion
                   (seq-find
                    (lambda (candidate)
                      (equal (cdr (assq 'name candidate)) name))
                    (ekp-gui-verify-showcase-temporal-assertions state))))
              (cdr (assq 'passed assertion))))))
    (should
     (eq (funcall assertion-passed
                  (append '((stage . "policy-immediate")
                            (policy_timer . t)
                            (unplanned_spans . 2)
                            (current_projection . :false)
                            (timers_settled . :false)
                            (completed . :false))
                          base)
                  "immediate-unplanned-spans-only-while-policy-pending")
         t))
    (should
     (eq (funcall assertion-passed
                  (append '((stage . "policy-immediate")
                            (policy_timer . :false)
                            (unplanned_spans . 2)
                            (current_projection . :false)
                            (timers_settled . t)
                            (completed . :false))
                          base)
                  "immediate-unplanned-spans-only-while-policy-pending")
         t))
    (should
     (eq (funcall assertion-passed
                  (append '((stage . "policy-settled")
                            (policy_timer . :false)
                            (unplanned_spans . 0)
                            (live_unplanned_spans . 0)
                            (stale_unplanned_spans . 0)
                            (current_projection . t)
                            (timers_settled . t)
                            (completed . t))
                          base)
                  "settled-zero-stale-unplanned-spans")
         t))
    (should
     (eq (funcall assertion-passed
                  (append '((stage . "policy-settled")
                            (policy_timer . :false)
                            (unplanned_spans . 1)
                            (live_unplanned_spans . 0)
                            (stale_unplanned_spans . 1)
                            (current_projection . :false)
                            (timers_settled . t)
                            (completed . t))
                          base)
                  "settled-zero-stale-unplanned-spans")
         :false))
    (should
     (eq (funcall assertion-passed
                  (append '((stage . "policy-settled")
                            (policy_timer . :false)
                            (unplanned_spans . 1)
                            (live_unplanned_spans . 0)
                            (stale_unplanned_spans . 1)
                            (current_projection . :false)
                            (timers_settled . t)
                            (completed . t))
                          base)
                  "settled-current-projection")
         :false))))

(ert-deftest ekp-gui-verify-g004-temporal-width-requires-inline-wrap ()
  "Manual narrow checkpoints fail when automatic inline evidence is unwrapped."
  (let* ((state '((stage . "width-280")
                  (width . 280)
                  (selected_target_window . t)
                  (source_unchanged . t)
                  (projection_published . t)
                  (generation_delta . 0)
                  (resize_timer . :false)
                  (policy_timer . :false)
                  (unplanned_spans . 0)
                  (live_unplanned_spans . 0)
                  (stale_unplanned_spans . 0)
                  (live_state_current . t)
                  (current_projection . t)
                  (timers_settled . t)
                  (pending_chunks . :false)
                  (overlays . 0)
                  (engine_c . t)
                  (automatic_inline_plan_complete . t)
                  (automatic_inline_line_count . 1)
                  (automatic_inline_wrapped . :false)
                  (automatic_inline_breaks_source_spaces . :false)
                  (automatic_inline_break_map)
                  (isolated_cjk_cascades)
                  (isolated_cjk_lines)
                  (policy . ((inline_code . "no-hyphen")))
                  (completed . :false)))
         (assertions (ekp-gui-verify-showcase-temporal-assertions state)))
    (dolist (name '("manual-width-automatic-inline-wrapped"
                    "manual-width-inline-breaks-use-source-spaces"))
      (let ((assertion
             (seq-find
              (lambda (candidate)
                (equal (cdr (assq 'name candidate)) name))
              assertions)))
        (should assertion)
        (should (eq (cdr (assq 'passed assertion)) :false))))))

(defun ekp-gui-verify--near-overflow-text (target)
  "Return prose whose natural final line is nearly full at TARGET."
  (let ((words ["alpha" "中文" "beta" "排版" "gamma" "编辑"])
        (text "")
        (index 0)
        found)
    (while (and (< index 300) (not found))
      (setq text
            (concat text
                    (if (string-empty-p text) "" " ")
                    (aref words (% index (length words)))))
      (let* ((plan (ekp-layout-plan text target))
             (lines (ekp-layout-plan-lines plan)))
        (when (> (length lines) 2)
          (let* ((line (aref lines (1- (length lines))))
                 (tail (substring
                        text
                        (ekp-layout-line-source-start line)
                        (ekp-layout-line-source-end line)))
                 (remaining (- target (ekp--measured-width tail))))
            (setq found
                  (and (> remaining 0)
                       (< remaining
                          (ekp--measured-width
                           ekp-gui-verify--live-suffix)))))))
      (setq index (1+ index)))
    text))

(defun ekp-gui-verify--range-property-hash (beg end)
  "Return an exact text-property hash for BEG through END."
  (secure-hash
   'sha256
   (prin1-to-string (buffer-substring beg end))))

(defun ekp-gui-verify--configure-middle-edit ()
  "Record one projected space and a later stable anchor range."
  (let* ((spans (ekp-buffer--live-state-spans ekp-buffer--live-state))
         (dirty (car spans))
         (later (cadr spans))
         (dirty-beg (marker-position (ekp-buffer--span-beg dirty)))
         (dirty-end (marker-position (ekp-buffer--span-end dirty)))
         (space
          (save-excursion
            (goto-char dirty-beg)
            (search-forward " " dirty-end t))))
    (unless (and space later)
      (error "Live GUI setup needs one dirty row and one later anchor"))
    (setq ekp-gui-verify--live-middle-position (copy-marker (1- space))
          ekp-gui-verify--live-middle-later-beg
          (copy-marker (marker-position (ekp-buffer--span-beg later)))
          ekp-gui-verify--live-middle-later-end
          (copy-marker
           (marker-position (ekp-buffer--span-end (car (last spans)))) t)
          ekp-gui-verify--live-middle-later-hash
          (ekp-gui-verify--range-property-hash
           ekp-gui-verify--live-middle-later-beg
           ekp-gui-verify--live-middle-later-end)
          ekp-gui-verify--live-middle-plan
          (ekp-buffer--live-state-plan ekp-buffer--live-state)
          ekp-gui-verify--live-middle-spans spans)))

;;;###autoload
(defun ekp-gui-verify-live-setup ()
  "Create a deterministic GUI buffer for live typing verification."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (delete-other-windows)
  (let ((buffer (get-buffer-create "*EKP Live Layout*")))
    (switch-to-buffer buffer)
    (when ekp-auto-justify-mode
      (ekp-auto-justify-mode -1))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (text-mode)
      (setq-local truncate-lines nil)
      (redisplay t)
      (insert (ekp-gui-verify--near-overflow-text
               (ekp-buffer--effective-width)))
      (setq ekp-gui-verify--live-original
            (substring-no-properties (buffer-string))
            ekp-gui-verify--live-expected
            ekp-gui-verify--live-original
            ekp-gui-verify--live-post-crossing-state nil
            ekp-gui-verify--live-motion-generation nil
            ekp-gui-verify--live-motion-cache-size nil
            ekp-gui-verify--live-motion-state nil
            ekp-gui-verify--live-stage "setup")
      (ekp-auto-justify-mode 1)
      (goto-char (point-max))
      (run-hooks 'post-command-hook)
      (redisplay t)
      (setq ekp-gui-verify--live-original-signatures
            (copy-tree
             (ekp-buffer--live-state-signatures ekp-buffer--live-state))
            ekp-gui-verify--live-original-projection-hash
            (ekp-gui-verify--projection-hash)
            ekp-gui-verify--live-original-active-index
            (ekp-buffer--live-state-active-index ekp-buffer--live-state)
            ekp-gui-verify--live-original-prefix-end
            (marker-position
             (ekp-buffer--live-state-prefix-end ekp-buffer--live-state))
            ekp-gui-verify--live-original-width
            ekp-buffer--auto-width
            ekp-gui-verify--live-original-screen-lines
            (count-screen-lines (point-min) (point-max)))
      (ekp-gui-verify--configure-middle-edit))
    (ekp-gui-verify-live-adapter)))

(defun ekp-gui-verify--owned-break-positions (&optional beg end)
  "Return EKP-owned replacing-display newline positions from BEG to END."
  (let ((position (or beg (point-min)))
        (limit (or end (point-max)))
        positions)
    (while (< position limit)
      (let* ((display (get-text-property
                       position 'ekp-buffer--display))
             (next (or (next-single-property-change
                        position 'ekp-buffer--display nil limit)
                       limit)))
        (when (and (stringp display)
                   (string-match-p
                    "\n" (substring-no-properties display)))
          (push position positions))
        (setq position next)))
    (nreverse positions)))

(defun ekp-gui-verify--owned-property-runs ()
  "Return stable descriptions of all EKP-owned projection runs."
  (let ((position (point-min))
        runs)
    (while (< position (point-max))
      (let* ((next (or (next-property-change
                        position nil (point-max))
                       (point-max)))
             (display (get-text-property
                       position 'ekp-buffer--display))
             (prefix (get-text-property
                      position 'ekp-buffer--line-prefix))
             (justified (get-text-property position 'ekp-justified)))
        (when (or display prefix justified)
          (push (list position next display prefix justified) runs))
        (setq position next)))
    (nreverse runs)))

(defun ekp-gui-verify--projection-hash ()
  "Return a stable hash of the current EKP projection."
  (secure-hash
   'sha256 (prin1-to-string (ekp-gui-verify--owned-property-runs))))

(defun ekp-gui-verify--stable-live-state ()
  "Return the committed live projection state relevant to stability."
  (let ((state ekp-buffer--live-state))
    (list
     :active (ekp-buffer--live-state-active-index state)
     :prefix-end
     (marker-position (ekp-buffer--live-state-prefix-end state))
     :signatures (ekp-buffer--live-state-signatures state)
     :projection (ekp-gui-verify--projection-hash)
     :plan (ekp-buffer--live-state-plan state)
     :spans (ekp-buffer--live-state-spans state))))

(defun ekp-gui-verify--stable-live-state-matches-p (snapshot)
  "Return non-nil when the committed projection matches SNAPSHOT exactly."
  (let ((current (ekp-gui-verify--stable-live-state)))
    (and (eql (plist-get current :active)
              (plist-get snapshot :active))
         (eql (plist-get current :prefix-end)
              (plist-get snapshot :prefix-end))
         (eq (plist-get current :signatures)
             (plist-get snapshot :signatures))
         (equal (plist-get current :projection)
                (plist-get snapshot :projection))
         (eq (plist-get current :plan)
             (plist-get snapshot :plan))
         (eq (plist-get current :spans)
             (plist-get snapshot :spans)))))

(defun ekp-gui-verify--dirty-live-span-p (span)
  "Return non-nil when SPAN belongs to the current natural dirty island."
  (when-let* ((edit ekp-buffer--live-edit)
              (beg (marker-position
                    (ekp-buffer--live-edit-dirty-beg edit)))
              (end (marker-position
                    (ekp-buffer--live-edit-dirty-end edit))))
    (ekp-buffer--span-overlaps-p span beg end)))

(defun ekp-gui-verify--live-active-bounds ()
  "Return the active semantic source range, or nil."
  (if ekp-buffer--live-edit
      (let ((beg (marker-position
                  (ekp-buffer--live-edit-dirty-beg ekp-buffer--live-edit)))
            (end (marker-position
                  (ekp-buffer--live-edit-dirty-end ekp-buffer--live-edit))))
        (and beg end (<= beg end) (cons beg end)))
    (when-let* ((state ekp-buffer--live-state)
                (plan (ekp-buffer--live-state-plan state))
                (active (ekp-buffer--live-state-active-index state))
                (beg (marker-position (ekp-buffer--live-state-beg state)))
                (end (marker-position (ekp-buffer--live-state-end state))))
      (let ((lines (ekp-layout-plan-lines plan)))
        (when (< active (length lines))
          (let ((line (aref lines active)))
            (cons (+ beg (ekp-layout-line-source-start line)) end)))))))

(defun ekp-gui-verify--owned-layout-p (beg end)
  "Return non-nil when BEG through END carries EKP layout."
  (or (text-property-not-all beg end 'ekp-buffer--display nil)
      (text-property-not-all beg end 'ekp-buffer--line-prefix nil)
      (text-property-not-all beg end 'ekp-justified nil)))

(defun ekp-gui-verify--active-line-natural-p ()
  "Return non-nil when the active semantic line has no EKP layout."
  (when-let* ((bounds (ekp-gui-verify--live-active-bounds)))
    (not (ekp-gui-verify--owned-layout-p
          (car bounds) (cdr bounds)))))

(defun ekp-gui-verify--current-live-signatures ()
  "Return the current semantic prefix signatures."
  (and ekp-buffer--live-state
       (ekp-buffer--live-state-signatures ekp-buffer--live-state)))

(defun ekp-gui-verify--stage-check (stage predicate)
  "Return PREDICATE for STAGE, and true for every other stage."
  (if (equal ekp-gui-verify--live-stage stage)
      (if predicate t :false)
    t))

(defun ekp-gui-verify-live-adapter ()
  "Return JSON-compatible state for the live verification buffer."
  (let* ((live-beg
          (and ekp-buffer--live-state
               (marker-position
                (ekp-buffer--live-state-beg ekp-buffer--live-state))))
         (live-end
          (and ekp-buffer--live-state
               (marker-position
                (ekp-buffer--live-state-end ekp-buffer--live-state))))
         (scan (ekp-gui-verify--scan
                (current-buffer) #'ekp-gui-verify--dirty-live-span-p))
         (logical (substring-no-properties (buffer-string)))
         (breaks (ekp-gui-verify--owned-break-positions))
         (live-breaks
          (and live-beg live-end
               (ekp-gui-verify--owned-break-positions live-beg live-end)))
         (active (and ekp-buffer--live-state
                      (ekp-buffer--live-state-active-index
                       ekp-buffer--live-state)))
         (prefix-end
          (and ekp-buffer--live-state
               (marker-position
                (ekp-buffer--live-state-prefix-end
                 ekp-buffer--live-state))))
         (signatures (ekp-gui-verify--current-live-signatures))
         (projection-hash (ekp-gui-verify--projection-hash))
         (later-hash
          (and (markerp ekp-gui-verify--live-middle-later-beg)
               (marker-position ekp-gui-verify--live-middle-later-beg)
               (ekp-gui-verify--range-property-hash
                ekp-gui-verify--live-middle-later-beg
                ekp-gui-verify--live-middle-later-end)))
         (original-p
          (equal ekp-gui-verify--live-expected
                 ekp-gui-verify--live-original)))
    `((logical_length . ,(length logical))
      (logical_sha256 . ,(secure-hash 'sha256 logical))
      (expected_source . ,(if (equal logical
                                    ekp-gui-verify--live-expected)
                              t :false))
      (source_newlines . ,(cl-count ?\n logical))
      (owned_breaks . ,(length breaks))
      (live_owned_breaks . ,(length live-breaks))
      (stage . ,ekp-gui-verify--live-stage)
      (projection_sha256 . ,projection-hash)
      (active_index . ,(or active -1))
      (stable_boundary_position . ,(or prefix-end -1))
      (prefix_lines . ,(length signatures))
      (plan_lines
       . ,(if-let* ((plan (and ekp-buffer--live-state
                              (ekp-buffer--live-state-plan
                               ekp-buffer--live-state))))
              (length (ekp-layout-plan-lines plan))
            0))
      (middle_anchor_preserved
       . ,(ekp-gui-verify--stage-check
           "middle-dirty"
           (and ekp-buffer--live-edit
                (equal later-hash
                       ekp-gui-verify--live-middle-later-hash)
                (eq (ekp-buffer--live-state-plan ekp-buffer--live-state)
                    ekp-gui-verify--live-middle-plan)
                (eq (ekp-buffer--live-state-spans ekp-buffer--live-state)
                    ekp-gui-verify--live-middle-spans))))
      (middle_exact_restored
       . ,(ekp-gui-verify--stage-check
           "middle-restored"
           (and original-p
                (not ekp-buffer--live-edit)
                (equal projection-hash
                       ekp-gui-verify--live-original-projection-hash)
                (equal later-hash
                       ekp-gui-verify--live-middle-later-hash)
                (eq (ekp-buffer--live-state-plan ekp-buffer--live-state)
                    ekp-gui-verify--live-middle-plan)
                (eq (ekp-buffer--live-state-spans ekp-buffer--live-state)
                    ekp-gui-verify--live-middle-spans))))
      (prefix_replanned
       . ,(ekp-gui-verify--stage-check
           "typed"
           (and ekp-gui-verify--live-post-crossing-state
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-post-crossing-state)
                (not (equal signatures
                            ekp-gui-verify--live-original-signatures)))))
      (delete_preserved_committed_projection
       . ,(ekp-gui-verify--stage-check
           "suffix-deleted"
           (and original-p
                ekp-gui-verify--live-post-crossing-state
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-post-crossing-state))))
      (point_move_back_preserved_projection
       . ,(ekp-gui-verify--stage-check
           "moved-back"
           (and ekp-gui-verify--live-motion-generation
                (= ekp-buffer--generation
                   ekp-gui-verify--live-motion-generation)
                (= (length ekp-buffer--live-plan-cache)
                   ekp-gui-verify--live-motion-cache-size)
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-motion-state))))
      (point_move_forward_preserved_projection
       . ,(ekp-gui-verify--stage-check
           "moved-forward"
           (and ekp-gui-verify--live-motion-generation
                (= ekp-buffer--generation
                   ekp-gui-verify--live-motion-generation)
                (= (length ekp-buffer--live-plan-cache)
                   ekp-gui-verify--live-motion-cache-size)
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-motion-state))))
      (yank_kept_current_row_natural
       . ,(ekp-gui-verify--stage-check
           "yanked"
           (and ekp-buffer--live-edit
                ekp-gui-verify--live-motion-state
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-motion-state))))
      (undo_preserved_committed_projection
       . ,(ekp-gui-verify--stage-check
           "undone"
           (and original-p
                ekp-buffer--live-edit
                ekp-gui-verify--live-motion-state
                (ekp-gui-verify--stable-live-state-matches-p
                 ekp-gui-verify--live-motion-state))))
      (resize_reflowed
       . ,(ekp-gui-verify--stage-check
           "resized"
           (< ekp-buffer--auto-width
              ekp-gui-verify--live-original-width)))
      (resize_restored
       . ,(ekp-gui-verify--stage-check
           "resize-restored"
           (= ekp-buffer--auto-width
              ekp-gui-verify--live-original-width)))
      (completed_paragraph
       . ,(if (or (not (equal ekp-gui-verify--live-stage "completed"))
                  (and (= (cl-count ?\n logical) 1)
                       (get-text-property (point-min) 'ekp-justified)
                       live-beg live-end
                       (= live-beg live-end (point-max))))
              t :false))
      (screen_lines . ,(count-screen-lines (point-min) (point-max)))
      (overlays . ,(length (overlays-in (point-min) (point-max))))
      (hscroll . ,(window-hscroll))
      (active_paragraph . ,(if ekp-buffer--live-state t :false))
      (authoritative_width . ,(or ekp-buffer--auto-width 0))
      (active_line_natural
       . ,(if (or (member ekp-gui-verify--live-stage
                          '("completed" "resized" "resize-restored"))
                  (ekp-gui-verify--active-line-natural-p))
              t :false))
      (generation . ,ekp-buffer--generation)
      (pending_edit . ,(if ekp-buffer--live-edit t :false))
      (live_cache_size . ,(length ekp-buffer--live-plan-cache))
      (pixel_scan_pass
       . ,(if (or (= (or active 0) 0)
                  (plist-get scan :pass))
              t :false))
      (pixel_overflow_lines . ,(plist-get scan :over)))))

(defun ekp-gui-verify-live-assertions ()
  "Return adapter assertions for the live verification buffer."
  (let* ((adapter (ekp-gui-verify-live-adapter))
         (value (lambda (key) (cdr (assq key adapter)))))
    `(((name . "logical-source-exact")
       (passed . ,(funcall value 'expected_source)))
      ((name . "zero-overlays")
       (passed . ,(if (= (funcall value 'overlays) 0) t :false)))
      ((name . "zero-horizontal-scroll")
       (passed . ,(if (= (funcall value 'hscroll) 0) t :false)))
      ((name . "middle-edit-preserves-later-anchor")
       (passed . ,(funcall value 'middle_anchor_preserved)))
      ((name . "middle-reversal-restores-exact-projection")
       (passed . ,(funcall value 'middle_exact_restored)))
      ((name . "visual-row-crossing-commits-prefix")
       (passed . ,(funcall value 'prefix_replanned)))
      ((name . "delete-keeps-last-committed-projection")
       (passed . ,(funcall value
                           'delete_preserved_committed_projection)))
      ((name . "point-back-preserves-projection")
       (passed . ,(funcall value
                           'point_move_back_preserved_projection)))
      ((name . "point-forward-preserves-projection")
       (passed . ,(funcall value
                           'point_move_forward_preserved_projection)))
      ((name . "same-row-yank-keeps-projection")
       (passed . ,(funcall value 'yank_kept_current_row_natural)))
      ((name . "real-undo-keeps-committed-projection")
       (passed . ,(funcall value
                           'undo_preserved_committed_projection)))
      ((name . "window-resize-reflows-prefix")
       (passed . ,(funcall value 'resize_reflowed)))
      ((name . "window-width-restore-reflows-prefix")
       (passed . ,(funcall value 'resize_restored)))
      ((name . "hard-newline-completes-previous-paragraph")
       (passed . ,(funcall value 'completed_paragraph)))
      ((name . "active-semantic-line-natural")
       (passed . ,(funcall value 'active_line_natural)))
      ((name . "live-cache-bounded")
       (passed . ,(if (<= (funcall value 'live_cache_size) 16) t :false)))
      ((name . "no-pixel-overflow")
       (passed . ,(if (= (funcall value 'pixel_overflow_lines) 0)
                      t :false))))))

;;;###autoload
(defun ekp-gui-verify-live-delete-middle-space ()
  "Delete one projected middle-row space through the public command path."
  (interactive)
  (goto-char (1+ (marker-position
                  ekp-gui-verify--live-middle-position)))
  (call-interactively #'delete-backward-char)
  (let ((offset (- (marker-position
                    ekp-gui-verify--live-middle-position)
                   (point-min))))
    (setq ekp-gui-verify--live-expected
          (concat (substring ekp-gui-verify--live-original 0 offset)
                  (substring ekp-gui-verify--live-original (1+ offset)))
          ekp-gui-verify--live-stage "middle-dirty"))
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-restore-middle-space ()
  "Reinsert the deleted middle-row space and restore the exact baseline."
  (interactive)
  (goto-char ekp-gui-verify--live-middle-position)
  (let ((last-command-event ?\s))
    (call-interactively #'self-insert-command))
  (setq ekp-gui-verify--live-expected ekp-gui-verify--live-original
        ekp-gui-verify--live-stage "middle-restored")
  (goto-char (point-max))
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-type-suffix ()
  "Type the deterministic suffix through the command loop."
  (interactive)
  (setq ekp-gui-verify--live-stage "typed")
  (setq ekp-gui-verify--live-expected
        (concat ekp-gui-verify--live-original
                ekp-gui-verify--live-suffix))
  (execute-kbd-macro ekp-gui-verify--live-suffix)
  (redisplay t)
  (setq ekp-gui-verify--live-post-crossing-state
        (ekp-gui-verify--stable-live-state))
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-delete-suffix ()
  "Delete the suffix while retaining the last committed projection."
  (interactive)
  (execute-kbd-macro
   (vconcat
    (make-list (length ekp-gui-verify--live-suffix) 'backspace)))
  (setq ekp-gui-verify--live-expected
        ekp-gui-verify--live-original
        ekp-gui-verify--live-stage "suffix-deleted")
  (redisplay t)
  (setq ekp-gui-verify--live-motion-generation ekp-buffer--generation
        ekp-gui-verify--live-motion-cache-size
        (length ekp-buffer--live-plan-cache)
        ekp-gui-verify--live-motion-state
        (ekp-gui-verify--stable-live-state))
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-move-back ()
  "Move point into the first semantic line through the command loop."
  (interactive)
  (execute-kbd-macro (kbd "M-<"))
  (setq ekp-gui-verify--live-stage "moved-back")
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-move-forward ()
  "Move point back to the final semantic line through the command loop."
  (interactive)
  (execute-kbd-macro (kbd "M->"))
  (setq ekp-gui-verify--live-stage "moved-forward")
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-yank ()
  "Yank a multi-script suffix through the public command path."
  (interactive)
  (undo-boundary)
  (kill-new ekp-gui-verify--live-yank)
  (execute-kbd-macro (kbd "C-y"))
  (setq ekp-gui-verify--live-expected
        (concat ekp-gui-verify--live-original
                ekp-gui-verify--live-yank)
        ekp-gui-verify--live-stage "yanked")
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-undo-yank ()
  "Undo the verification yank through the public command path."
  (interactive)
  (execute-kbd-macro (kbd "C-/"))
  (setq ekp-gui-verify--live-expected
        ekp-gui-verify--live-original
        ekp-gui-verify--live-stage "undone")
  (redisplay t)
  (ekp-gui-verify-live-adapter))

(defun ekp-gui-verify--live-change-margin (columns stage)
  "Set the right margin to COLUMNS and record verification STAGE."
  (set-window-margins (selected-window) 0 columns)
  (run-hooks 'window-configuration-change-hook)
  (sit-for (+ ekp-auto-justify-resize-delay 0.2))
  (setq ekp-gui-verify--live-stage stage)
  (redisplay t)
  (ekp-gui-verify-live-adapter))

;;;###autoload
(defun ekp-gui-verify-live-resize-narrower ()
  "Narrow the live text area and wait for the real resize debounce."
  (interactive)
  (ekp-gui-verify--live-change-margin 24 "resized"))

;;;###autoload
(defun ekp-gui-verify-live-resize-restore ()
  "Restore the live text area and wait for the real resize debounce."
  (interactive)
  (ekp-gui-verify--live-change-margin nil "resize-restored"))

;;;###autoload
(defun ekp-gui-verify-live-complete-paragraph ()
  "Insert a hard newline and complete the previous paragraph."
  (interactive)
  (execute-kbd-macro "\n")
  (setq ekp-gui-verify--live-expected
        (concat ekp-gui-verify--live-original "\n")
        ekp-gui-verify--live-stage "completed")
  (redisplay t)
  (ekp-gui-verify-live-adapter))

(defconst ekp-gui-verify--split-text
  "soft wrap 中文 mixed editing stays natural across narrow side by side windows"
  "Text typed by the split-window soft-wrap verification.")

(defvar-local ekp-gui-verify--split-expected "")
(defvar-local ekp-gui-verify--split-stage "setup")
(defvar-local ekp-gui-verify--split-original-wrap nil)

;;;###autoload
(defun ekp-gui-verify-split-wrap-setup ()
  "Create an intentional narrow split for native soft-wrap verification."
  (interactive)
  (unless (display-graphic-p)
    (user-error "GUI verification needs a graphical frame"))
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (delete-other-windows)
  (redisplay t)
  (sit-for 0.2)
  (let* ((target (get-buffer-create "*EKP Split Soft Wrap*"))
         (control (get-buffer-create "*EKP Split Control*"))
         (left (selected-window))
         (right (split-window-right)))
    (set-window-buffer left target)
    (set-window-buffer right control)
    (window-resize left (- 44 (window-total-width left)) t)
    (select-window left)
    (with-current-buffer control
      (erase-buffer)
      (insert "Intentional control window for partial-width verification."))
    (with-current-buffer target
      (when ekp-auto-justify-mode
        (ekp-auto-justify-mode -1))
      (erase-buffer)
      (text-mode)
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'truncate-partial-width-windows)
      (setq ekp-gui-verify--split-expected ""
            ekp-gui-verify--split-stage "setup"
            ekp-gui-verify--split-original-wrap
            (list (local-variable-p 'truncate-lines)
                  truncate-lines
                  (local-variable-p 'truncate-partial-width-windows)
                  truncate-partial-width-windows))
      (ekp-auto-justify-mode 1)
      (goto-char (point-max)))
    (redisplay t)
    (ekp-gui-verify-split-wrap-adapter)))

(defun ekp-gui-verify--split-row-start ()
  "Return point's native visual-row start in the selected window."
  (save-excursion
    (vertical-motion 0 (selected-window))
    (point)))

(defun ekp-gui-verify--split-display-state-restored-p ()
  "Return non-nil when split verification restored its display state."
  (pcase-let ((`(,lines-local ,lines ,partial-local ,partial)
               ekp-gui-verify--split-original-wrap))
    (and (eq (local-variable-p 'truncate-lines) lines-local)
         (equal truncate-lines lines)
         (eq (local-variable-p 'truncate-partial-width-windows)
             partial-local)
         (equal truncate-partial-width-windows partial))))

(defun ekp-gui-verify-split-wrap-adapter ()
  "Return JSON-compatible state for split-window soft-wrap verification."
  (let* ((logical (substring-no-properties (buffer-string)))
         (typed-p (equal ekp-gui-verify--split-stage "typed"))
         (disabled-p (equal ekp-gui-verify--split-stage "disabled"))
         (breaks (ekp-gui-verify--owned-break-positions)))
    `((stage . ,ekp-gui-verify--split-stage)
      (expected_source
       . ,(if (equal logical ekp-gui-verify--split-expected) t :false))
      (logical_sha256 . ,(secure-hash 'sha256 logical))
      (window_count . ,(length (window-list nil 'no-minibuf)))
      (target_columns . ,(window-total-width))
      (auto_mode . ,(if ekp-auto-justify-mode t :false))
      (truncate_lines . ,(if truncate-lines t :false))
      (partial_truncation
       . ,(if truncate-partial-width-windows t :false))
      (wrap_bindings_local
       . ,(if (and (local-variable-p 'truncate-lines)
                   (local-variable-p 'truncate-partial-width-windows))
              t :false))
      (screen_lines . ,(count-screen-lines (point-min) (point-max)))
      (visual_row_advanced
       . ,(if (or (not typed-p)
                  (> (ekp-gui-verify--split-row-start) (point-min)))
              t :false))
      (semantic_prefix_lines
       . ,(if ekp-buffer--live-state
              (length
               (ekp-buffer--live-state-signatures
                ekp-buffer--live-state))
            0))
      (active_line_natural
       . ,(if (or (not typed-p)
                  disabled-p
                  (ekp-gui-verify--active-line-natural-p))
              t :false))
      (hscroll . ,(window-hscroll))
      (live_owned_breaks . ,(length breaks))
      (overlays . ,(length (overlays-in (point-min) (point-max))))
      (pending_edit . ,(if ekp-buffer--live-edit t :false))
      (display_state_restored
       . ,(if (or (not disabled-p)
                  (ekp-gui-verify--split-display-state-restored-p))
              t :false)))))

(defun ekp-gui-verify-split-wrap-assertions ()
  "Return assertions for split-window soft-wrap verification."
  (let* ((adapter (ekp-gui-verify-split-wrap-adapter))
         (value (lambda (key) (cdr (assq key adapter))))
         (disabled-p
          (equal (funcall value 'stage) "disabled")))
    `(((name . "logical-source-exact")
       (passed . ,(funcall value 'expected_source)))
      ((name . "intentional-two-window-layout")
       (passed . ,(if (= (funcall value 'window_count) 2) t :false)))
      ((name . "target-window-below-default-truncation-threshold")
       (passed . ,(if (< (funcall value 'target_columns) 50) t :false)))
      ((name . "auto-mode-owns-soft-wrap")
       (passed . ,(if (or disabled-p
                          (and (funcall value 'auto_mode)
                               (eq (funcall value 'truncate_lines) :false)
                               (eq (funcall value 'partial_truncation) :false)
                               (funcall value 'wrap_bindings_local)))
                      t :false)))
      ((name . "typing-crosses-visual-boundary")
       (passed . ,(funcall value 'visual_row_advanced)))
      ((name . "no-horizontal-scroll")
       (passed . ,(if (or disabled-p
                          (= (funcall value 'hscroll) 0))
                      t :false)))
      ((name . "semantic-prefix-publishes-complete-lines")
       (passed . ,(if (or (not (equal (funcall value 'stage) "typed"))
                          (and (> (funcall value 'semantic_prefix_lines) 0)
                               (> (funcall value 'live_owned_breaks) 0)))
                      t :false)))
      ((name . "active-semantic-line-natural")
       (passed . ,(funcall value 'active_line_natural)))
      ((name . "zero-overlays")
       (passed . ,(if (= (funcall value 'overlays) 0) t :false)))
      ((name . "no-pending-live-transaction")
       (passed . ,(if (eq (funcall value 'pending_edit) :false)
                      t :false)))
      ((name . "mode-disable-restores-display-state")
       (passed . ,(funcall value 'display_state_restored))))))

(defun ekp-gui-verify-split-generic-assertions
    (window buffer start end)
  "Return generic capture assertions for an intentional split layout."
  (list
   `((name . "selected-target-window-live")
     (passed . ,(if (and (window-live-p window)
                         (eq (window-buffer window) buffer))
                    t :false)))
   `((name . "intentional-two-window-capture")
     (passed . ,(if (= (length (window-list nil 'no-minibuf)) 2)
                    t :false)))
   `((name . "visible-range-valid")
     (passed . ,(if (and (integer-or-marker-p start)
                         (integer-or-marker-p end)
                         (<= start end)
                         (<= end (with-current-buffer buffer (point-max))))
                    t :false)))))

;;;###autoload
(defun ekp-gui-verify-split-wrap-type ()
  "Type across the native wrap boundary in the narrow target window."
  (interactive)
  (mapc
   (lambda (character)
     (let ((last-command-event character))
       (call-interactively #'self-insert-command))
     (redisplay t)
     (sit-for 0.02))
   (string-to-list ekp-gui-verify--split-text))
  (setq ekp-gui-verify--split-expected ekp-gui-verify--split-text
        ekp-gui-verify--split-stage "typed")
  (redisplay t)
  (ekp-gui-verify-split-wrap-adapter))

;;;###autoload
(defun ekp-gui-verify-split-wrap-disable ()
  "Disable auto mode and expose restored truncation ownership."
  (interactive)
  (ekp-auto-justify-mode -1)
  (setq ekp-gui-verify--split-stage "disabled")
  (redisplay t)
  (ekp-gui-verify-split-wrap-adapter))

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
