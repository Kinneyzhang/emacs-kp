;;; ekp-buffer-tests.el --- Tests for ekp-buffer.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe ERT tests for the buffer-level justification layer.
;; Widths are always passed explicitly, so no window is required.

;;; Code:

(require 'ert)
(require 'ekp-buffer)
(require 'ekp-showcase)

(defconst ekp-buffer-test--samples
  (list "简单的中文段落测试内容,排版效果应当良好稳定。"
        "The quick brown fox jumps over the lazy dog several times today."
        "Mixed 中英文 paragraph with  double  spaces inside and a tail   "
        "para one\n\npara two 混排 content here\nthird para"
        "  leading indent 段落内容 preserved intact"
        "短\n\n\n多个空段落之间的内容")
  "Logical texts covering CJK, Latin, mixed, blanks, indent, tails.")

(defconst ekp-buffer-test--widths '(30 80 200 400)
  "Pixel widths from emergency-narrow to comfortable.")

(defmacro ekp-buffer-test--with-text (text &rest body)
  "Run BODY in a temp buffer containing TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     ,@body))

;;;; Text-property projection invariants

(defun ekp-buffer-test--display-spec-p (name value)
  "Return non-nil when display VALUE contains a NAME specification."
  (or (and (consp value) (eq (car value) name))
      (and (listp value)
           (seq-some (lambda (item)
                       (and (consp item) (eq (car item) name)))
                     value))))

(defun ekp-buffer-test--strings-in-tree (tree)
  "Return all strings contained anywhere in TREE."
  (let (strings)
    (cl-labels ((walk (value)
                  (cond
                   ((stringp value) (push value strings))
                   ((consp value)
                    (walk (car value))
                    (walk (cdr value)))
                   ((vectorp value)
                    (dotimes (i (length value))
                      (walk (aref value i)))))))
      (walk tree))
    (nreverse strings)))

(ert-deftest ekp-buffer-test-layout-never-changes-source-characters ()
  "Layout changes display properties, never the logical character stream."
  (let ((text "中文排版 mixed words with spaces and extraordinary wrapping"))
    (ekp-buffer-test--with-text text
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil)
      (goto-char (+ (point-min) 7))
      (let ((point-before (point))
            (size-before (buffer-size))
            (chars-tick (buffer-chars-modified-tick))
            (undo-before buffer-undo-list))
        (ekp-justify-region (point-min) (point-max) 24)
        (should (equal (substring-no-properties (buffer-string)) text))
        (should (= (buffer-size) size-before))
        (should (= (point) point-before))
        (should (= (buffer-chars-modified-tick) chars-tick))
        (should (eq buffer-undo-list undo-before))
        (should-not (buffer-modified-p))
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest ekp-buffer-test-overlong-no-break-preserves-atom ()
  "A rigid overflow atom remains intact in the public projection."
  (let* ((prefix "行内原子演示:代码片段 ")
         (atom (propertize
                (concat (make-string 48 ?a) " b") 'ekp-no-break t))
         (text (concat prefix atom " 后文继续。"))
         (ekp-use-c-module nil))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 40)
      (let* ((breaks (ekp-buffer-test--display-newline-positions))
             (atom-beg (+ (point-min) (length prefix)))
             (atom-end (+ atom-beg (length atom))))
        (should breaks)
        (should-not
         (seq-some (lambda (position)
                     (and (> position atom-beg) (< position atom-end)))
                   breaks)))
      (should (equal (substring-no-properties (buffer-string))
                     (substring-no-properties text)))
      (should (get-text-property (+ (point-min) (length prefix))
                                 'ekp-no-break)))))

(ert-deftest ekp-buffer-test-projection-fires-no-external-change-hooks ()
  "Projection installation and removal are silent to external change hooks."
  (ekp-buffer-test--with-text
      "silent projection hooks 中文 mixed paragraph long enough"
    (let ((before 0)
          (after 0))
      (add-hook 'before-change-functions
                (lambda (&rest _) (setq before (1+ before))) nil t)
      (add-hook 'after-change-functions
                (lambda (&rest _) (setq after (1+ after))) nil t)
      (ekp-justify-region (point-min) (point-max) 20)
      (ekp-unjustify-region (point-min) (point-max))
      (should (= before 0))
      (should (= after 0))
      (goto-char (point-max))
      (insert "x")
      (should (= before 1))
      (should (= after 1)))))

(ert-deftest ekp-buffer-test-owned-property-publishes-once ()
  "A projection value and its owner marker are one property mutation."
  (ekp-buffer-test--with-text "owned"
    (let ((mutations 0)
          (put (symbol-function 'put-text-property))
          (add (symbol-function 'add-text-properties))
          (value '(space :width (4))))
      (cl-letf (((symbol-function 'put-text-property)
                 (lambda (beg end property new-value &optional object)
                   (setq mutations (1+ mutations))
                   (funcall put beg end property new-value object)))
                ((symbol-function 'add-text-properties)
                 (lambda (beg end properties &optional object)
                   (setq mutations (1+ mutations))
                   (funcall add beg end properties object))))
        (ekp-buffer--put-owned
         (point-min) (point-max) 'display 'ekp-buffer--display value))
      (should (= mutations 1))
      (should (equal (get-text-property (point-min) 'display) value))
      (should (equal (get-text-property
                      (point-min) 'ekp-buffer--display)
                     value)))))

(ert-deftest ekp-buffer-test-prepared-plan-installs-without-measuring ()
  "Projection must consume gap geometry already prepared by the plan."
  (let ((text "中文 mixed paragraph with several spaces and CJK gaps"))
    (ekp-buffer-test--with-text text
      (let ((plan (ekp-layout-plan
                   (buffer-substring (point-min) (point-max)) 18)))
        (cl-letf (((symbol-function 'ekp--measured-width)
                   (lambda (&rest _)
                     (error "projection measured prepared geometry"))))
          (should (ekp-buffer--projectable-p
                   plan (point-min) (point-min) (point-max)))
          (ekp-buffer--install-plan
           (point-min) (point-max) 18 plan))))))

(ert-deftest ekp-buffer-test-justify-accepts-reversed-bounds ()
  "A reversed region receives the same projection as an ordered region."
  (let ((text "中文排版 mixed words need several visual lines"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-max) (point-min) 24)
      (should ekp-buffer--spans)
      (should (equal (substring-no-properties (buffer-string)) text))
      (should-not (text-property-not-all
                   (point-min) (point-max) 'ekp-justified 24)))))

(ert-deftest ekp-buffer-test-ascii-glue-combines-space-and-min-width ()
  "Existing ASCII spaces use `space-width' plus an exact `min-width' floor."
  (ekp-buffer-test--with-text "aa bb cc dd ee ff"
    (ekp-justify-region (point-min) (point-max) 12)
    (goto-char (point-min))
    (should (search-forward " " nil t))
    (let ((display (get-text-property (1- (point)) 'display)))
      (pcase display
        (`((space-width ,factor) (min-width ((,target))))
         (should (numberp factor))
         (should (> factor 0))
         (should (integerp target))
         (should (> target 0)))
        (_ (ert-fail (format "Unexpected ASCII glue display: %S"
                             display)))))))

(ert-deftest ekp-buffer-test-cjk-zero-source-glue-uses-min-width ()
  "CJK glue pads an existing grapheme and inserts no source space."
  (let ((text "中文排版测试内容足够长"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 11)
      (should (equal (substring-no-properties (buffer-string)) text))
      (should
       (seq-some
        (lambda (pos)
          (pcase (get-text-property pos 'display)
            (`(min-width ((,target)))
             (and (integerp target) (> target 0)))))
        (number-sequence (point-min) (1- (point-max))))))))

(ert-deftest ekp-buffer-test-hyphen-and-break-are-display-only ()
  "A discretionary hyphen and newline live in `display', not source text."
  (let ((text "extraordinary hyphenation demonstration paragraph"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 16)
      (should (equal (substring-no-properties (buffer-string)) text))
      (should (= (cl-count ?\n (buffer-string)) 0))
      (should-not (string-match-p "-" (substring-no-properties
                                       (buffer-string))))
      (should
       (seq-some
        (lambda (pos)
          (let ((display (get-text-property pos 'display)))
            (and (stringp display)
                 (string-match-p "-\n" (substring-no-properties display)))))
        (number-sequence (point-min) (1- (point-max))))))))

(ert-deftest ekp-buffer-test-indentation-uses-line-prefix ()
  "First-line indentation is a `line-prefix' projection."
  (let ((ekp-first-line-indent 6))
    (ekp-buffer-test--with-text "首行缩进测试内容足够长会形成多行"
      (ekp-justify-region (point-min) (point-max) 18)
      (let ((prefix (get-text-property (point-min) 'line-prefix)))
        (should (stringp prefix))
        (should (equal (get-text-property 0 'display prefix)
                       '(space :width (6)))))
      (should (equal (substring-no-properties (buffer-string))
                     "首行缩进测试内容足够长会形成多行")))))

(ert-deftest ekp-buffer-test-foreign-display-conflict-is-preserved ()
  "EKP keeps a paragraph verbatim instead of stealing foreign display state."
  (ekp-buffer-test--with-text "foreign display paragraph must stay intact"
    (put-text-property 9 16 'display "VISIBLE")
    (let ((before (buffer-substring (point-min) (point-max))))
      (ekp-justify-region (point-min) (point-max) 12)
      (should (equal-including-properties
               (buffer-substring (point-min) (point-max)) before))
      (should ekp-buffer--conflicts))))

(ert-deftest ekp-buffer-test-foreign-projection-owners-stay-verbatim ()
  "Every uncomposable foreign projection owner remains exact."
  (dolist (entry '((line-prefix . "P")
                   (wrap-prefix . "W")
                   (composition . ((1)))
                   (invisible . foreign)))
    (ekp-buffer-test--with-text
        "foreign owner paragraph must stay property-identical"
      (put-text-property 4 8 (car entry) (cdr entry))
      (let ((before (buffer-substring (point-min) (point-max))))
        (ekp-justify-region (point-min) (point-max) 12)
        (should-not ekp-buffer--spans)
        (should (equal-including-properties
                 (buffer-substring (point-min) (point-max)) before))
        (should
         (string-match-p
          (format "foreign `%s' property" (car entry))
          (caddr (car ekp-buffer--conflicts))))))))

;;;; Roundtrip exactness

(ert-deftest ekp-buffer-test-roundtrip-exact ()
  "justify + unjustify restores text and properties exactly."
  (dolist (text ekp-buffer-test--samples)
    (dolist (w ekp-buffer-test--widths)
      (ekp-buffer-test--with-text text
        (ekp-justify-region (point-min) (point-max) w)
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-buffer-test-roundtrip-propertized ()
  "Roundtrip preserves user text properties."
  (let ((text (concat (propertize "加粗的中文开头内容" 'face 'bold)
                      " plain middle part "
                      (propertize "italic tail words" 'face 'italic))))
    (dolist (w '(60 250))
      (ekp-buffer-test--with-text text
        (ekp-justify-region (point-min) (point-max) w)
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-buffer-test-hard-newlines-preserved ()
  "Hard newline count survives justification."
  (ekp-buffer-test--with-text "a 段落 one\n\nb 段落 two\nc 段落 three"
    (ekp-justify-region (point-min) (point-max) 100)
    (let ((hard 0))
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (unless (get-text-property (match-beginning 0) 'ekp-soft-break)
          (setq hard (1+ hard))))
      (should (= hard 3)))))

;;;; Justified-state invariants

(ert-deftest ekp-buffer-test-justified-marked ()
  "Justified region carries the ekp-justified width property."
  (ekp-buffer-test--with-text "中文内容需要标记属性验证正确性"
    (ekp-justify-region (point-min) (point-max) 120)
    (should (eq (get-text-property (point-min) 'ekp-justified) 120))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'ekp-justified 120))))

(ert-deftest ekp-buffer-test-rejustify-idempotent ()
  "Justifying at a new width equals a fresh justification at that width."
  (let ((text "The idempotence check 中英混排 must hold across widths."))
    (let (fresh)
      (ekp-buffer-test--with-text text
        (ekp-justify-region (point-min) (point-max) 150)
        (setq fresh (buffer-string)))
      (ekp-buffer-test--with-text text
        (ekp-justify-region (point-min) (point-max) 300)
        (ekp-justify-region (point-min) (point-max) 150)
        (should (equal-including-properties (buffer-string) fresh))))))

;;;; Edit robustness

(ert-deftest ekp-buffer-test-edit-then-unjustify ()
  "Text typed into a justified buffer survives unjustification."
  (ekp-buffer-test--with-text "abcdef ghijkl 中文内容 mnopqr stuvwx"
    (ekp-justify-region (point-min) (point-max) 80)
    ;; Insert inside the first word: physical == logical there.
    (goto-char (+ (point-min) 2))
    (insert "XY")
    (ekp-unjustify-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "abXYcdef ghijkl 中文内容 mnopqr stuvwx"))))

(ert-deftest ekp-buffer-test-point-stable ()
  "Point returns to its logical position after a roundtrip."
  (ekp-buffer-test--with-text "abcdef ghijkl mnopqr stuvwx yzabcd"
    (goto-char (+ (point-min) 9))       ; inside "ghijkl"
    (ekp-justify-region (point-min) (point-max) 60)
    (ekp-unjustify-region (point-min) (point-max))
    (should (= (point) (+ (point-min) 9)))))

(ert-deftest ekp-buffer-test-inactive-mark-stays-inactive ()
  "Reprojection preserves an existing inactive mark without selecting text."
  (ekp-buffer-test--with-text
      "showcase width changes must not activate an old mark"
    (goto-char (+ (point-min) 12))
    (set-marker (mark-marker) (+ (point-min) 3))
    (setq mark-active nil)
    (let ((point-before (point))
          (mark-before (mark t)))
      (ekp-justify-region (point-min) (point-max) 40)
      (should (= (point) point-before))
      (should (= (mark t) mark-before))
      (should-not mark-active))))

(ert-deftest ekp-buffer-test-active-region-stays-active ()
  "Reprojection preserves an intentional active region."
  (ekp-buffer-test--with-text
      "an intentional region remains active across width changes"
    (set-mark (+ (point-min) 3))
    (goto-char (+ (point-min) 18))
    (activate-mark)
    (let ((point-before (point))
          (mark-before (mark t)))
      (ekp-justify-region (point-min) (point-max) 40)
      (should (= (point) point-before))
      (should (= (mark t) mark-before))
      (should mark-active)
      (should (use-region-p)))))

;;;; Auto-justify mode

(defmacro ekp-buffer-test--with-mode (text width &rest body)
  "Enable `ekp-auto-justify-mode' on TEXT at WIDTH, run BODY, disable."
  (declare (indent 2))
  `(ekp-buffer-test--with-text ,text
     (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                (lambda (&optional _) ,width)))
       (ekp-auto-justify-mode 1)
       (unwind-protect
           (progn ,@body)
         (ekp-auto-justify-mode -1)))))

(defun ekp-buffer-test--display-newline-positions ()
  "Return positions whose EKP-owned display value contains a newline."
  (let ((pos (point-min))
        positions)
    (while (< pos (point-max))
      (let* ((display (get-text-property
                       pos 'ekp-buffer--display))
             (next (or (next-single-property-change
                        pos 'ekp-buffer--display nil (point-max))
                       (point-max))))
        (when (and (stringp display)
                   (string-match-p "\n"
                                   (substring-no-properties display)))
          (push pos positions))
        (setq pos next)))
    (nreverse positions)))

(defun ekp-buffer-test--display-lines ()
  "Return source slices separated by EKP-owned display newlines."
  (let ((beg (point-min))
        (pos (point-min))
        lines)
    (while (< pos (point-max))
      (let ((display (get-text-property pos 'ekp-buffer--display)))
        (when (and (stringp display)
                   (string-match-p "\n"
                                   (substring-no-properties display)))
          (push (buffer-substring-no-properties beg (1+ pos)) lines)
          (setq beg (1+ pos))))
      (setq pos (1+ pos)))
    (push (buffer-substring-no-properties beg (point-max)) lines)
    (nreverse lines)))

(defun ekp-buffer-test--single-cjk-line-p (line)
  "Return non-nil when LINE is exactly one CJK source character."
  (let ((trimmed (replace-regexp-in-string
                  "\\`[[:space:]\n\r\t]+\\|[[:space:]\n\r\t]+\\'"
                  "" line)))
    (and (= (length trimmed) 1)
         (let ((char (aref trimmed 0)))
           (and (<= #x4E00 char) (<= char #x9FFF))))))

(defun ekp-buffer-test--display-hyphen-p ()
  "Return non-nil when an EKP-owned display break publishes a hyphen."
  (let ((pos (point-min))
        hit)
    (while (and (< pos (point-max)) (not hit))
      (let ((display (get-text-property pos 'ekp-buffer--display)))
        (setq hit
              (and (stringp display)
                   (string-match-p "-\n"
                                   (substring-no-properties display)))))
      (setq pos (1+ pos)))
    hit))

(defun ekp-buffer-test--type-string (string)
  "Insert STRING through the public self-insert command path."
  (mapc
   (lambda (character)
     (let ((last-command-event character))
       (call-interactively #'self-insert-command)))
   (string-to-list string)))

(defun ekp-buffer-test--semantic-plan (text width min-lines)
  "Return TEXT's plan at WIDTH, requiring at least MIN-LINES lines."
  (let* ((plan (ekp-layout-plan text width))
         (lines (ekp-layout-plan-lines plan)))
    (should (>= (length lines) min-lines))
    plan))

(defun ekp-buffer-test--line-at (plan index)
  "Return semantic line INDEX from PLAN."
  (aref (ekp-layout-plan-lines plan) index))

(defun ekp-buffer-test--any-owned-layout-p (beg end)
  "Return non-nil when BEG through END has EKP-owned projection."
  (and (< beg end)
       (or (text-property-not-all beg end 'ekp-justified nil)
           (text-property-not-all beg end 'ekp-buffer--display nil)
           (text-property-not-all beg end 'ekp-buffer--line-prefix nil))))

(defun ekp-buffer-test--assert-natural-range (beg end)
  "Assert BEG through END carries no EKP live projection properties."
  (when (< beg end)
    (dolist (property
             '(ekp-justified ekp-buffer--display ekp-buffer--line-prefix))
      (should-not (text-property-not-all beg end property nil)))))

(defun ekp-buffer-test--live-active-beg ()
  "Return the source beginning of the current natural semantic line."
  (let* ((state ekp-buffer--live-state)
         (plan (ekp-buffer--live-state-plan state))
         (active (ekp-buffer--live-state-active-index state))
         (base (marker-position (ekp-buffer--live-state-beg state)))
         (line (ekp-buffer-test--line-at plan active)))
    (+ base (ekp-layout-line-source-start line))))

(defun ekp-buffer-test--first-space-break-line (plan)
  "Return the first semantic line in PLAN whose break consumes spaces."
  (let ((lines (ekp-layout-plan-lines plan))
        found)
    (dotimes (index (length lines))
      (let ((line (aref lines index)))
        (when (and (not found)
                   (eq (ekp-layout-line-break-kind line) 'space)
                   (< (ekp-layout-line-break-source-start line)
                      (ekp-layout-line-break-source-end line)))
          (setq found line))))
    (should found)
    found))

(ert-deftest ekp-buffer-test-live-edge-whitespace-is-immediately-visible ()
  "One self-inserted edge whitespace stays natural on the active line."
  (dolist (case '(("alpha beta gamma" end ?\s)
                  ("alpha beta gamma" beginning ?\s)
                  ("alpha beta gamma" end ?\t)
                  ("中文排版测试" end ?\s)))
    (pcase-let ((`(,text ,where ,character) case))
      (ekp-buffer-test--with-mode text 16
        (goto-char (if (eq where 'end) (point-max) (point-min)))
        (let ((last-command-event character))
          (call-interactively #'self-insert-command))
        (should (= (char-before) character))
        (should-not (get-text-property
                     (1- (point)) 'ekp-buffer--display))
        (should-not (get-text-property (1- (point)) 'display))))))

(ert-deftest ekp-buffer-test-live-deletion-exposes-trailing-space ()
  "Deleting the final glyph leaves the preceding source space visible."
  (ekp-buffer-test--with-mode "alpha beta x" 16
    (goto-char (point-max))
    (delete-char -1)
    (should (eq (char-before) ?\s))
    (should-not (get-text-property
                 (1- (point)) 'ekp-buffer--display))
    (should-not (get-text-property (1- (point)) 'display))
    (should ekp-buffer--live-edit)))

(ert-deftest ekp-buffer-test-live-consecutive-spaces-survive-backspace ()
  "Backspacing a following glyph leaves both typed spaces visible."
  (ekp-buffer-test--with-mode "alpha beta" 16
    (goto-char (point-max))
    (dolist (character '(?\s ?\s ?x))
      (let ((last-command-event character))
        (call-interactively #'self-insert-command)))
    (call-interactively #'delete-backward-char)
    (should (string-suffix-p "  " (buffer-string)))
    (dolist (position (list (- (point) 2) (1- (point))))
      (should-not (get-text-property position 'ekp-buffer--display))
      (should-not (get-text-property position 'display)))
    (should ekp-buffer--live-edit)
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest ekp-buffer-test-live-yank-trailing-space-is-natural ()
  "Yanking text with a trailing space publishes it without hiding it."
  (let ((kill-ring nil)
        kill-ring-yank-pointer)
    (ekp-buffer-test--with-mode "alpha beta" 16
      (goto-char (point-max))
      (kill-new " pasted ")
      (call-interactively #'yank)
      (should (equal (substring-no-properties (buffer-string))
                     "alpha beta pasted "))
      (should-not (get-text-property
                   (1- (point)) 'ekp-buffer--display))
      (should-not (get-text-property (1- (point)) 'display))
      (should-not ekp-buffer--live-edit))))

(ert-deftest ekp-buffer-test-live-newline-then-space-is-natural ()
  "A space on a newly created hard line is visible immediately."
  (ekp-buffer-test--with-mode "alpha beta" 16
    (goto-char (point-max))
    (call-interactively #'newline)
    (let ((last-command-event ?\s))
      (call-interactively #'self-insert-command))
    (should (string-suffix-p "\n " (buffer-string)))
    (should-not (get-text-property
                 (1- (point)) 'ekp-buffer--display))
    (should-not (get-text-property (1- (point)) 'display))
    (should ekp-buffer--live-edit)))

(ert-deftest ekp-buffer-test-live-single-space-undo-restores-breaks ()
  "Undoing one edge space restores source and prior visual breaks."
  (ekp-buffer-test--with-mode "alpha beta gamma delta" 16
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((text (substring-no-properties (buffer-string)))
          (breaks (ekp-buffer-test--display-newline-positions)))
      (goto-char (point-max))
      (undo-boundary)
      (let ((last-command-event ?\s))
        (call-interactively #'self-insert-command))
      (undo-boundary)
      (undo-only 1)
      (should (equal (substring-no-properties (buffer-string)) text))
      (should (equal
               (ekp-buffer-test--display-newline-positions) breaks))
      (should-not ekp-buffer--live-edit)
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-live-single-line-quick-proof-does-no-layout-work ()
  "Typing a provably single semantic line neither plans nor projects."
  (ekp-buffer-test--with-mode "" 1000
    (let ((calls 0)
          (original (symbol-function 'ekp-layout-plan)))
      (cl-letf (((symbol-function 'ekp-layout-plan)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (apply original arguments))))
        (mapc
         (lambda (character)
           (let ((last-command-event character))
             (call-interactively #'self-insert-command)))
         (string-to-list "natural mixed 中文 editing"))
        (should (= calls 0))))
    (dolist (property
             '(ekp-justified ekp-buffer--display
               ekp-buffer--line-prefix))
      (should-not
       (text-property-not-all
        (point-min) (point-max) property nil)))
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest ekp-buffer-test-live-projects-prefix-before-last-plan-line ()
  "Point in the last semantic line projects only earlier plan lines."
  (let* ((text
          "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
         (width 20)
         (plan (ekp-buffer-test--semantic-plan text width 3))
         (lines (ekp-layout-plan-lines plan))
         (last-line (ekp-buffer-test--line-at plan (1- (length lines))))
         (active-beg (+ (point-min)
                        (ekp-layout-line-source-start last-line))))
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (should (ekp-buffer-test--any-owned-layout-p (point-min) active-beg))
      (ekp-buffer-test--assert-natural-range active-beg (point-max))
      (should-not (overlays-in (point-min) (point-max)))
      (should (equal (substring-no-properties (buffer-string)) text)))))

(ert-deftest ekp-buffer-test-live-middle-edit-preserves-unaffected-anchors ()
  "A middle-line edit naturalizes one island without clearing later anchors."
  (let* ((text
          (concat
           "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu "
           "nu xi omicron pi rho sigma tau upsilon phi chi psi omega"))
         (width 20)
         (plan (ekp-buffer-test--semantic-plan text width 4))
         (middle (ekp-buffer-test--line-at plan 1))
         (point-offset (1+ (ekp-layout-line-source-start middle)))
         (expected (concat (substring text 0 point-offset)
                           "X"
                           (substring text point-offset)))
         (calls 0)
         (original (symbol-function 'ekp-layout-plan)))
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (ekp-buffer--commit-live-paragraph)
      (let* ((spans (ekp-buffer--live-state-spans ekp-buffer--live-state))
             (dirty (nth 1 spans))
             (later (nth 2 spans))
             (later-beg (copy-marker
                         (marker-position (ekp-buffer--span-beg later))))
             (later-end (copy-marker
                         (marker-position
                          (ekp-buffer--span-end (car (last spans))))
                         t))
             (later-projection
              (buffer-substring later-beg later-end)))
        (unwind-protect
            (cl-letf (((symbol-function 'ekp-layout-plan)
                       (lambda (&rest arguments)
                         (setq calls (1+ calls))
                         (apply original arguments))))
              (goto-char (+ (point-min) point-offset))
              (let ((last-command-event ?X))
                (call-interactively #'self-insert-command))
              (should (= calls 0))
              (ekp-buffer-test--assert-natural-range
               (marker-position (ekp-buffer--span-beg dirty))
               (marker-position (ekp-buffer--span-end dirty)))
              (should (equal-including-properties
                       (buffer-substring later-beg later-end)
                       later-projection)))
          (set-marker later-beg nil)
          (set-marker later-end nil)))
      (should (equal (substring-no-properties (buffer-string)) expected)))))

(ert-deftest ekp-buffer-test-live-break-space-belongs-to-previous-owner-line ()
  "Editing break whitespace makes its semantic owner line natural."
  (let* ((text
          "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
         (width 20)
         (plan (ekp-buffer-test--semantic-plan text width 3))
         (line (ekp-buffer-test--first-space-break-line plan))
         (offset (ekp-layout-line-break-source-start line))
         (expected (concat (substring text 0 offset)
                           "X"
                           (substring text offset))))
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (should (ekp-buffer-test--any-owned-layout-p
               (point-min) (point-max)))
      (goto-char (+ (point-min) offset))
      (let ((last-command-event ?X))
        (call-interactively #'self-insert-command))
      (ekp-buffer-test--assert-natural-range
       (ekp-buffer-test--live-active-beg) (point-max))
      (should (equal (substring-no-properties (buffer-string)) expected)))))

(ert-deftest ekp-buffer-test-live-leading-space-belongs-to-first-line ()
  "Editing leading space naturalizes its row and keeps later anchors."
  (let ((text
         "   alpha beta gamma delta epsilon zeta eta theta iota kappa lambda")
        (width 20))
    (ekp-buffer-test--semantic-plan text width 3)
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (ekp-buffer--commit-live-paragraph)
      (should (ekp-buffer-test--any-owned-layout-p
               (point-min) (point-max)))
      (let* ((spans (ekp-buffer--live-state-spans ekp-buffer--live-state))
             (first (car spans))
             (later (cadr spans)))
        (goto-char (point-min))
        (let ((last-command-event ?X))
          (call-interactively #'self-insert-command))
        (should ekp-buffer--live-edit)
        (ekp-buffer-test--assert-natural-range
         (marker-position (ekp-buffer--span-beg first))
         (marker-position (ekp-buffer--span-end first)))
        (should (get-text-property
                 (marker-position (ekp-buffer--span-beg later))
                 'ekp-justified)))
      (should (equal (substring-no-properties (buffer-string))
                     (concat "X" text))))))

(ert-deftest ekp-buffer-test-live-earlier-breaks-are-not-frozen ()
  "Later edits can invalidate an earlier semantic line signature."
  (let* ((text
          "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
         (prefix "extraordinarysuperlong ")
         (width 20)
         (old-plan (ekp-buffer-test--semantic-plan text width 3))
         (new-plan (ekp-buffer-test--semantic-plan
                    (concat prefix text) width 3))
         (old-signature
          (ekp-layout-line-signature
           (ekp-buffer-test--line-at old-plan 0)))
         (new-signature
          (ekp-layout-line-signature
           (ekp-buffer-test--line-at new-plan 0))))
    (should-not (equal old-signature new-signature))
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (should (ekp-buffer-test--any-owned-layout-p
               (point-min) (point-max)))
      (let ((old-breaks (ekp-buffer-test--display-newline-positions)))
        (goto-char (point-min))
        (ekp-buffer-test--type-string prefix)
        (goto-char (point-max))
        (run-hooks 'post-command-hook)
        (should-not (equal
                     old-breaks
                     (ekp-buffer-test--display-newline-positions)))
        (should (equal (substring-no-properties (buffer-string))
                       (concat prefix text)))))))

(ert-deftest ekp-buffer-test-live-same-plan-does-not-rewrite-properties ()
  "Posting the same plan/prefix boundary performs zero owned writes."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (width 20)
        (writes 0)
        (put (symbol-function 'put-text-property))
        (remove (symbol-function 'remove-text-properties)))
    (ekp-buffer-test--semantic-plan text width 3)
    (ekp-buffer-test--with-mode "" width
      (ekp-buffer-test--type-string text)
      (should (ekp-buffer-test--any-owned-layout-p
               (point-min) (point-max)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (let ((chars-tick (buffer-chars-modified-tick))
            (undo-before buffer-undo-list))
        (cl-letf (((symbol-function 'put-text-property)
                   (lambda (beg end property value &optional object)
                     (when (memq property
                                 '(ekp-justified display line-prefix))
                       (setq writes (1+ writes)))
                     (funcall put beg end property value object)))
                  ((symbol-function 'remove-text-properties)
                   (lambda (beg end properties &optional object)
                     (when (seq-some
                            (lambda (property)
                              (memq property
                                    '(ekp-justified display line-prefix)))
                            properties)
                       (setq writes (1+ writes)))
                     (funcall remove beg end properties object))))
          (run-hooks 'post-command-hook))
        (should (= writes 0))
        (should (= (buffer-chars-modified-tick) chars-tick))
        (should-not (buffer-modified-p))
        (should (eq buffer-undo-list undo-before))))))

(ert-deftest ekp-buffer-test-live-same-row-edit-keeps-committed-projection ()
  "Ordinary same-row input neither replans nor rewrites the committed prefix."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (width 20)
        (calls 0)
        (original (symbol-function 'ekp-layout-plan)))
    (ekp-buffer-test--with-mode text width
      (let* ((state ekp-buffer--live-state)
             (prefix-end
              (copy-marker
               (marker-position (ekp-buffer--live-state-prefix-end state))))
             (projection (buffer-substring (point-min) prefix-end)))
        (unwind-protect
            (cl-letf (((symbol-function 'ekp-layout-plan)
                       (lambda (&rest arguments)
                         (setq calls (1+ calls))
                         (apply original arguments))))
              (goto-char
               (1+ (marker-position
                    (ekp-buffer--live-state-prefix-end state))))
              (let ((last-command-event ?x))
                (call-interactively #'self-insert-command))
              (should (= calls 0))
              (should (eq ekp-buffer--live-state state))
              (should ekp-buffer--live-edit)
              (should (equal-including-properties
                       (buffer-substring (point-min) prefix-end)
                       projection)))
          (set-marker prefix-end nil))))))

(ert-deftest ekp-buffer-test-live-wrap-crossing-is-one-atomic-commit ()
  "Typing across one native row boundary plans once, not after every key."
  (let ((calls 0)
        (original (symbol-function 'ekp-layout-plan)))
    (ekp-buffer-test--with-mode "" 20
      (cl-letf (((symbol-function 'ekp-layout-plan)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (apply original arguments))))
        (ekp-buffer-test--type-string "alpha beta gamma delt")
        (should (= calls 1))
        (should (ekp-buffer-test--any-owned-layout-p
                 (point-min) (point-max)))
        (ekp-buffer-test--type-string "a")
        (should (= calls 1))
        (should-not (get-text-property
                     (1- (point)) 'ekp-buffer--display))))))

(ert-deftest ekp-buffer-test-live-backward-wrap-crossing-stays-local ()
  "Deleting into the previous native row keeps the live transaction local."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (calls 0)
        (original (symbol-function 'ekp-layout-plan)))
    (ekp-buffer-test--with-mode text 20
      (goto-char (point-max))
      (insert " xyz")
      (ekp-buffer--commit-live-paragraph)
      (let ((size-before (buffer-size)))
        (cl-letf (((symbol-function 'ekp-buffer--native-row-start)
                   (lambda (_)
                     (if (= (buffer-size) size-before) 40 20)))
                  ((symbol-function 'ekp-layout-plan)
                   (lambda (&rest arguments)
                     (setq calls (1+ calls))
                     (apply original arguments))))
          (call-interactively #'delete-backward-char)
          (should (= calls 0))
          (should ekp-buffer--live-edit)
          (should (equal (substring-no-properties (buffer-string))
                         (concat text " xy"))))))))

(ert-deftest ekp-buffer-test-live-reversible-edit-restores-exact-baseline ()
  "Deleting and reinserting one space restores the exact committed state."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"))
    (ekp-buffer-test--with-mode text 20
      (let* ((state ekp-buffer--live-state)
             (spans (ekp-buffer--live-state-spans state))
             (middle (nth 1 spans))
             (beg (marker-position (ekp-buffer--span-beg middle)))
             (end (marker-position (ekp-buffer--span-end middle)))
             (space (save-excursion
                      (goto-char beg)
                      (search-forward " " end t)))
             (baseline (buffer-substring (point-min) (point-max)))
             (plan (ekp-buffer--live-state-plan state))
             (signatures (ekp-buffer--live-state-signatures state)))
        (should space)
        (goto-char (1- space))
        (delete-char 1)
        (should ekp-buffer--live-edit)
        (should (equal-including-properties
                 (ekp-buffer--live-edit-baseline-source
                  ekp-buffer--live-edit)
                 baseline))
        (should (eq (ekp-buffer--live-edit-baseline-plan
                     ekp-buffer--live-edit)
                    plan))
        (should (eq (ekp-buffer--live-edit-baseline-signatures
                     ekp-buffer--live-edit)
                    signatures))
        (should (eq (ekp-buffer--live-edit-baseline-spans
                     ekp-buffer--live-edit)
                    spans))
        (let ((last-command-event ?\s))
          (call-interactively #'self-insert-command))
        (should-not ekp-buffer--live-edit)
        (should (eq ekp-buffer--live-state state))
        (should (eq (ekp-buffer--live-state-plan state) plan))
        (should (eq (ekp-buffer--live-state-signatures state) signatures))
        (should (eq (ekp-buffer--live-state-spans state) spans))
        (should (equal-including-properties
                 (buffer-substring (point-min) (point-max))
                 baseline))))))

(ert-deftest ekp-buffer-test-live-source-rebuilds-only-dirty-island ()
  "A live source rebuild must not rescan the projected paragraph."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"))
    (ekp-buffer-test--with-mode text 20
      (let* ((middle
              (nth 1
                   (ekp-buffer--live-state-spans
                    ekp-buffer--live-state)))
             (beg (marker-position (ekp-buffer--span-beg middle)))
             (end (marker-position (ekp-buffer--span-end middle)))
             (space (save-excursion
                      (goto-char beg)
                      (search-forward " " end t)))
             (logical-substring
              (symbol-function 'ekp-buffer--logical-substring))
             (rescans 0))
        (should space)
        (goto-char (1- space))
        (delete-char 1)
        (should ekp-buffer--live-edit)
        (cl-letf (((symbol-function 'ekp-buffer--logical-substring)
                   (lambda (&rest arguments)
                     (setq rescans (1+ rescans))
                     (apply logical-substring arguments))))
          (should
           (equal-including-properties
            (ekp-buffer--current-live-source)
            (ekp-buffer--logical-substring
             (point-min) (point-max)))))
        (should (= rescans 1))))))

(ert-deftest ekp-buffer-test-point-leaving-paragraph-is-zero-work ()
  "Cursor motion across hard lines cannot commit or rewrite live state."
  (let ((text
         (concat
          "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"
          "\nsecond paragraph"))
        (calls 0)
        (writes 0)
        (original (symbol-function 'ekp-layout-plan))
        (put (symbol-function 'put-text-property))
        (remove (symbol-function 'remove-text-properties)))
    (ekp-buffer-test--with-mode text 20
      (goto-char (point-min))
      (let ((last-command-event ?x))
        (call-interactively #'self-insert-command))
      (let ((state ekp-buffer--live-state)
            (transaction ekp-buffer--live-edit)
            (projection (buffer-substring (point-min) (point-max))))
        (cl-letf (((symbol-function 'ekp-layout-plan)
                   (lambda (&rest arguments)
                     (setq calls (1+ calls))
                     (apply original arguments)))
                  ((symbol-function 'put-text-property)
                   (lambda (beg end property value &optional object)
                     (when (memq property
                                 '(ekp-justified display line-prefix))
                       (setq writes (1+ writes)))
                     (funcall put beg end property value object)))
                  ((symbol-function 'remove-text-properties)
                   (lambda (beg end properties &optional object)
                     (when (seq-some
                            (lambda (property)
                              (memq property
                                    '(ekp-justified display line-prefix)))
                            properties)
                       (setq writes (1+ writes)))
                     (funcall remove beg end properties object))))
          (goto-char (point-max))
          (run-hooks 'post-command-hook)
          (should (= calls 0))
          (should (= writes 0))
          (should (eq ekp-buffer--live-state state))
          (should (eq ekp-buffer--live-edit transaction))
          (should (equal-including-properties
                   (buffer-substring (point-min) (point-max))
                   projection)))))))

(ert-deftest ekp-buffer-test-live-point-motion-preserves-projection ()
  "Moving point within one hard line changes no live layout state."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (width 20)
        (calls 0)
        (key-calls 0)
        (cache-gets 0)
        (writes 0)
        (original (symbol-function 'ekp-layout-plan))
        (original-key (symbol-function 'ekp--para-key))
        (original-get (symbol-function 'ekp-buffer--live-cache-get))
        (put (symbol-function 'put-text-property))
        (remove (symbol-function 'remove-text-properties)))
    (let* ((plan (ekp-buffer-test--semantic-plan text width 3))
           (line (ekp-buffer-test--line-at plan 1))
           (target-offset (1+ (ekp-layout-line-source-start line))))
      (ekp-buffer-test--with-mode text width
        (goto-char
         (1+ (marker-position
              (ekp-buffer--live-state-prefix-end ekp-buffer--live-state))))
        (cl-letf (((symbol-function 'ekp-layout-plan)
                   (lambda (&rest args)
                     (setq calls (1+ calls))
                     (apply original args)))
                  ((symbol-function 'ekp--para-key)
                   (lambda (&rest args)
                     (setq key-calls (1+ key-calls))
                     (apply original-key args)))
                  ((symbol-function 'ekp-buffer--live-cache-get)
                   (lambda (key)
                     (setq cache-gets (1+ cache-gets))
                     (funcall original-get key)))
                  ((symbol-function 'put-text-property)
                   (lambda (beg end property value &optional object)
                     (when (memq property
                                 '(ekp-justified display line-prefix))
                       (setq writes (1+ writes)))
                     (funcall put beg end property value object)))
                  ((symbol-function 'remove-text-properties)
                   (lambda (beg end properties &optional object)
                     (when (seq-some
                            (lambda (property)
                              (memq property
                                    '(ekp-justified display line-prefix)))
                            properties)
                       (setq writes (1+ writes)))
                     (funcall remove beg end properties object))))
          (insert "x")
          (should (= calls 0))
          (run-hooks 'post-command-hook)
          (let* ((state ekp-buffer--live-state)
                 (transaction ekp-buffer--live-edit)
                 (projection (buffer-substring (point-min) (point-max)))
                 (active (ekp-buffer--live-state-active-index state))
                 (signatures (ekp-buffer--live-state-signatures state))
                 (spans (ekp-buffer--live-state-spans state))
                 (edit-end
                  (ekp-buffer--live-edit-edit-end transaction))
                 (edit-position (marker-position edit-end))
                 (prefix-end
                  (marker-position
                   (ekp-buffer--live-state-prefix-end state)))
                 (chars-tick (buffer-chars-modified-tick))
                 (undo-before buffer-undo-list))
            (setq calls 0 key-calls 0 cache-gets 0 writes 0)
            (dolist (position
                     (list (+ (point-min) target-offset) (point-max)))
              (goto-char position)
              (run-hooks 'post-command-hook)
              (should (eq ekp-buffer--live-state state))
              (should (equal-including-properties
                       (buffer-substring (point-min) (point-max))
                       projection))
              (should (= (ekp-buffer--live-state-active-index state)
                         active))
              (should (eq (ekp-buffer--live-state-signatures state)
                          signatures))
              (should (eq (ekp-buffer--live-state-spans state) spans))
              (should (eq ekp-buffer--live-edit transaction))
              (should (eq (ekp-buffer--live-edit-edit-end transaction)
                          edit-end))
              (should (= (marker-position edit-end) edit-position))
              (should (= (marker-position
                          (ekp-buffer--live-state-prefix-end state))
                         prefix-end)))
            (should (= calls 0))
            (should (= key-calls 0))
            (should (= cache-gets 0))
            (should (= writes 0))
            (should (= (buffer-chars-modified-tick) chars-tick))
            (should (eq buffer-undo-list undo-before))
            (should (= (length (substring-no-properties (buffer-string)))
                       (1+ (length text))))))))))

(ert-deftest ekp-buffer-test-live-reflow-commits-latest-edit-boundary ()
  "Width reflow commits the edit boundary, never transient point."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"))
    (ekp-buffer-test--with-mode text 20
      (goto-char
       (1+ (marker-position
            (ekp-buffer--live-state-prefix-end ekp-buffer--live-state))))
      (insert "x")
      (let ((edit-position
             (marker-position
              (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit))))
        (goto-char (point-min))
        (run-hooks 'post-command-hook)
        (ekp-buffer--reflow (current-buffer) 16)
        (let* ((state ekp-buffer--live-state)
               (plan (ekp-buffer--live-state-plan state))
               (line-count (length (ekp-layout-plan-lines plan))))
          (should-not ekp-buffer--live-edit)
          (should (= (ekp-buffer--live-state-active-index state)
                     line-count))
          (should (= (marker-position
                      (ekp-buffer--live-state-prefix-end state))
                     (marker-position (ekp-buffer--live-state-end state))))
          (should (ekp-buffer--live-state-contains-p edit-position))
          (should (= (point) (point-min))))))))

(ert-deftest ekp-buffer-test-live-plan-cache-is-bounded-and-contextual ()
  "Historical live plans reuse equal keys and bound retained history."
  (ekp-buffer-test--with-text ""
    (let ((calls 0)
          (original (symbol-function 'ekp-layout-plan))
          first)
      (setq ekp-buffer--live-plan-cache nil)
      (cl-letf (((symbol-function 'ekp-layout-plan)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (apply original arguments))))
        (setq first (ekp-buffer--live-plan-entry
                     "alpha beta gamma" 40))
        (should
         (eq (cdr first)
             (cdr (ekp-buffer--live-plan-entry
                   "alpha beta gamma" 40))))
        (should (= calls 1))
        (ekp-buffer--live-plan-entry "alpha beta gamma" 41)
        (let ((styled (copy-sequence "alpha beta gamma")))
          (put-text-property 0 5 'face 'bold styled)
          (ekp-buffer--live-plan-entry styled 40))
        (should (= calls 3))
        (let ((ekp-alignment 'center))
          (ekp-buffer--live-plan-entry "alpha beta gamma" 40))
        (should (= calls 4))
        (dotimes (index 20)
          (ekp-buffer--live-plan-entry
           (format "cache paragraph %02d alpha beta" index) 40))
        (should (= (length ekp-buffer--live-plan-cache) 16))
        (should-not
         (seq-find
          (lambda (entry)
            (equal-including-properties
             (caar entry) "alpha beta gamma"))
          ekp-buffer--live-plan-cache))))))

(ert-deftest ekp-buffer-test-live-yank-projects-semantic-prefix-only ()
  "One public yank projects completed semantic lines and keeps tail natural."
  (let* ((text
          "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
         (width 20)
         (plan (ekp-buffer-test--semantic-plan text width 3))
         (lines (ekp-layout-plan-lines plan))
         (active (aref lines (1- (length lines))))
         (active-beg (+ (point-min)
                        (ekp-layout-line-source-start active)))
         (kill-ring nil)
         kill-ring-yank-pointer)
    (ekp-buffer-test--with-mode "" width
      (kill-new text)
      (call-interactively #'yank)
      (should (ekp-buffer-test--any-owned-layout-p
               (point-min) active-beg))
      (ekp-buffer-test--assert-natural-range active-beg (point-max))
      (should (equal (substring-no-properties (buffer-string)) text))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-explicit-refill-survives-post-command ()
  "Explicit full refill is not downgraded until the next source edit."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (width 20))
    (ekp-buffer-test--with-mode text width
      (ekp-refill-paragraph)
      (should-not ekp-buffer--live-state)
      (let ((full-layout (buffer-substring (point-min) (point-max))))
        (run-hooks 'post-command-hook)
        (should-not ekp-buffer--live-state)
        (should (equal-including-properties
                 (buffer-substring (point-min) (point-max))
                 full-layout)))
      (goto-char (point-max))
      (insert "x")
      (should ekp-buffer--live-state))))

(ert-deftest ekp-buffer-test-live-conflict-pulls-whole-hard-line-native ()
  "A live foreign owner removes the entire affected hard-line prefix."
  (let ((text
         "first alpha beta gamma\nsecond delta epsilon zeta eta theta"))
    (ekp-buffer-test--with-mode text 20
      (goto-char (point-min))
      (search-forward "\n")
      (let* ((second-beg (point))
             (foreign (+ second-beg 7))
             (first-before
              (buffer-substring (point-min) (1- second-beg))))
        (let ((ekp-buffer--inhibit t))
          (with-silent-modifications
            (put-text-property foreign (1+ foreign)
                               'display "VISIBLE")))
        (goto-char (point-max))
        (insert "x")
        (ekp-buffer--commit-live-paragraph)
        (should (equal (get-text-property foreign 'display) "VISIBLE"))
        (ekp-buffer-test--assert-natural-range second-beg (point-max))
        (should (equal-including-properties
                 (buffer-substring (point-min) (1- second-beg))
                 first-before))
        (should ekp-buffer--conflicts)))))

(ert-deftest ekp-buffer-test-live-span-end-does-not-absorb-active-input ()
  "A live semantic span ends before text inserted at its boundary."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"))
    (ekp-buffer-test--with-mode "" 20
      (ekp-buffer-test--type-string text)
      (let* ((span (car (ekp-buffer--live-state-spans
                         ekp-buffer--live-state)))
             (end (ekp-buffer--span-end span)))
        (should (markerp end))
        (should-not (marker-insertion-type end))))))

(ert-deftest ekp-buffer-test-live-install-error-rolls-back-all-spans ()
  "A partial live install leaves no property or orphan span and re-signals."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        (calls 0)
        (original (symbol-function 'ekp-buffer--project-line)))
    (ekp-buffer-test--with-mode "" 20
      (ekp-buffer-test--type-string text)
      (ekp-buffer--clear-live-projection (point-min) (point-max))
      (setf (ekp-buffer--live-state-signatures ekp-buffer--live-state) nil
            (ekp-buffer--live-state-active-index ekp-buffer--live-state) nil)
      (cl-letf (((symbol-function 'ekp-buffer--project-line)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (when (= calls 2)
                     (error "forced live install failure"))
                   (apply original arguments))))
        (should-error (ekp-buffer--publish-live-prefix)
                      :type 'error))
      (should (= calls 2))
      (should-not ekp-buffer--spans)
      (should-not (ekp-buffer--live-state-spans ekp-buffer--live-state))
      (ekp-buffer-test--assert-natural-range (point-min) (point-max)))))

(ert-deftest ekp-buffer-test-live-crossing-size-limit-records-conflict ()
  "A hard line that grows past the live limit fails closed with diagnosis."
  (let ((ekp-auto-justify-paragraph-limit 20))
    (ekp-buffer-test--with-mode "" 1000
      (ekp-buffer-test--type-string (make-string 21 ?a))
      (ekp-buffer--commit-live-paragraph)
      (should-not ekp-buffer--spans)
      (ekp-buffer-test--assert-natural-range (point-min) (point-max))
      (should
       (string-match-p
        "automatic paragraph limit"
        (caddr (car ekp-buffer--conflicts)))))))

(ert-deftest ekp-buffer-test-live-teardown-releases-cache-and-markers ()
  "Disabling live mode detaches its markers, cache, and projection."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu")
        markers)
    (ekp-buffer-test--with-text text
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 20))
                ((symbol-function 'ekp-buffer--native-row-start)
                 (lambda (_) (point-min))))
        (ekp-auto-justify-mode 1)
        (goto-char (point-max))
        (insert "x")
        (setq markers
              (append
               (list (ekp-buffer--live-state-beg ekp-buffer--live-state)
                     (ekp-buffer--live-state-end ekp-buffer--live-state)
                     (ekp-buffer--live-state-prefix-end
                      ekp-buffer--live-state))
               (list
                (ekp-buffer--live-edit-old-beg ekp-buffer--live-edit)
                (ekp-buffer--live-edit-dirty-beg ekp-buffer--live-edit)
                (ekp-buffer--live-edit-dirty-end ekp-buffer--live-edit)
                (ekp-buffer--live-edit-row-start ekp-buffer--live-edit)
                (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit))))
        (should ekp-buffer--live-plan-cache)
        (ekp-auto-justify-mode -1))
      (should-not ekp-buffer--live-state)
      (should-not ekp-buffer--live-plan-cache)
      (dolist (marker markers)
        (should-not (marker-buffer marker)))
      (ekp-buffer-test--assert-natural-range (point-min) (point-max))
      (should (equal (substring-no-properties (buffer-string))
                     (concat text "x"))))))

(ert-deftest ekp-buffer-test-live-hard-newline-finalizes-previous-paragraph ()
  "A hard newline permits full KP only for the completed paragraph."
  (ekp-buffer-test--with-mode "alpha beta gamma" 80
    (goto-char (point-max))
    (call-interactively #'newline)
    (let ((newline (1- (point))))
      (should (get-text-property (point-min) 'ekp-justified))
      (should-not
       (text-property-not-all
        (1+ newline) (point-max) 'ekp-justified nil))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-live-edit-never-calls-whole-paragraph-command ()
  "An ordinary live edit must not dispatch through `ekp-justify-region'."
  (ekp-buffer-test--with-mode
      "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda"
      20
    (let ((called nil)
          (original (symbol-function 'ekp-justify-region)))
      (cl-letf (((symbol-function 'ekp-justify-region)
                 (lambda (&rest args)
                   (setq called args)
                   (apply original args))))
        (goto-char (point-max))
        (insert " x")
        (should-not called)
        (should ekp-buffer--live-edit)))))

(ert-deftest ekp-buffer-test-live-composition-defers-and-cancels-stale-generation ()
  "IME preedit defers projection and an older retry cannot publish."
  (let ((text "alpha beta gamma delta epsilon"))
    (ekp-buffer-test--with-mode text 16
      (let ((buffer (current-buffer))
            first-generation second-generation
            first-beg first-end second-beg second-end edit-end-marker)
        (cl-letf (((symbol-function 'ekp-buffer--composing-p)
                   (lambda () t)))
          (goto-char (point-max))
          (insert " x")
          (setq first-generation ekp-buffer--generation)
          (setq first-end (point)
                first-beg (- first-end 2))
          (should ekp-buffer--live-edit)
          (should (timerp ekp-buffer--composition-timer))
          (insert " y")
          (setq second-generation ekp-buffer--generation)
          (setq second-end (point)
                second-beg (- second-end 2)
                edit-end-marker
                (ekp-buffer--live-edit-edit-end ekp-buffer--live-edit))
          (should (> second-generation first-generation))
          (should (= (marker-position edit-end-marker) second-end))
          (when (timerp ekp-buffer--composition-timer)
            (cancel-timer ekp-buffer--composition-timer)
            (setq ekp-buffer--composition-timer nil)))
        (goto-char (point-min))
        (ekp-buffer--retry-composition
         buffer first-generation first-beg first-end)
        (should ekp-buffer--live-edit)
        (should (= (marker-position edit-end-marker) second-end))
        (ekp-buffer--retry-composition
         buffer second-generation second-beg second-end)
        (should-not ekp-buffer--live-edit)
        (should-not (marker-buffer edit-end-marker))
        (should (> (ekp-buffer--live-state-active-index
                    ekp-buffer--live-state)
                   0))
        (should (equal (substring-no-properties (buffer-string))
                       (concat text " x y")))))))

(ert-deftest ekp-buffer-test-live-next-edit-elsewhere-commits-quality-layout ()
  "The next source edit elsewhere commits and detaches the prior hard line."
  (let ((text
         "alpha beta gamma delta epsilon\nsecond paragraph stays here"))
    (ekp-buffer-test--with-mode text 16
      (cl-letf (((symbol-function 'ekp-buffer--native-row-start)
                 (lambda (_) (line-beginning-position))))
        (goto-char (1+ (point-min)))
        (insert "x")
        (let ((first-state ekp-buffer--live-state)
              (first-beg
               (ekp-buffer--live-state-beg ekp-buffer--live-state)))
          (forward-line 1)
          (run-hooks 'post-command-hook)
          (should (eq ekp-buffer--live-state first-state))
          (insert "y")
          (should-not (eq ekp-buffer--live-state first-state))
          (should-not (marker-buffer first-beg))
          (should (ekp-buffer--live-state-contains-p (point)))
          (should (get-text-property (point-min) 'ekp-justified)))))))

(ert-deftest ekp-buffer-test-live-foreign-display-abandons-only-its-paragraph ()
  "A live conflict keeps foreign display and leaves other paragraphs stable."
  (let ((text
         "first paragraph alpha beta gamma\nsecond paragraph delta epsilon"))
    (ekp-buffer-test--with-mode text 16
      (goto-char (point-min))
      (search-forward "\n")
      (let* ((second-beg (point))
             (first (buffer-substring (point-min) (1- second-beg)))
             (foreign (+ second-beg 7)))
        (let ((ekp-buffer--inhibit t))
          (with-silent-modifications
            (put-text-property foreign (1+ foreign)
                               'display "VISIBLE")))
        (goto-char (point-max))
        (insert " x")
        (should (equal (get-text-property foreign 'display) "VISIBLE"))
        (should (equal-including-properties
                 (buffer-substring (point-min) (1- second-beg))
                 first))
        (ekp-buffer--commit-live-paragraph)
        (should ekp-buffer--conflicts)
        (should-not (get-text-property second-beg 'ekp-justified))))))

(ert-deftest ekp-buffer-test-live-uses-one-narrowest-window-width ()
  "Every live span follows the buffer's narrowest displayed window."
  (ekp-buffer-test--with-text
      "alpha beta gamma delta epsilon zeta eta theta"
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(wide narrow)))
              ((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional window)
                 (if (eq window 'wide) 40 16))))
      (ekp-auto-justify-mode 1)
      (unwind-protect
          (progn
            (should (= ekp-buffer--auto-width 16))
            (goto-char (point-max))
            (insert " x")
            (should
             (cl-every
              (lambda (span)
                (= (ekp-buffer--span-width span) 16))
              ekp-buffer--spans)))
        (ekp-auto-justify-mode -1)))))

(ert-deftest ekp-buffer-test-live-narrowing-preserves-outside-source ()
  "A narrowed edit preserves outside source while committing prior live rows."
  (let ((text
         "first paragraph alpha beta\nmiddle paragraph gamma delta\nlast paragraph epsilon zeta"))
    (ekp-buffer-test--with-mode text 16
      (goto-char (point-min))
      (let* ((first-end (progn (search-forward "\n") (1- (point))))
             (middle-beg (point))
             (middle-end (progn (search-forward "\n") (1- (point))))
             (last-beg (point))
             (first-before
              (buffer-substring (point-min) first-end))
             (last-start (copy-marker last-beg))
             (last-end (copy-marker (point-max) t))
             (last-before (buffer-substring last-beg (point-max))))
        (narrow-to-region middle-beg middle-end)
        (goto-char (point-min))
        (search-forward "gamma")
        (insert "x")
        (save-restriction
          (widen)
          (should (equal-including-properties
                   (buffer-substring (point-min) first-end)
                   first-before))
          (should
           (equal
            (substring-no-properties
             (buffer-substring
              (marker-position last-start)
              (marker-position last-end)))
            (substring-no-properties last-before)))
          (should (get-text-property
                   (marker-position last-start) 'ekp-justified)))
        (set-marker last-start nil)
        (set-marker last-end nil)))))

(ert-deftest ekp-buffer-test-narrowed-reflow-reprojects-whole-buffer ()
  "A resize reflow preserves narrowing while updating outside paragraphs."
  (let ((text
         (concat
          "first alpha beta gamma delta epsilon\n"
          "middle zeta eta theta iota kappa lambda\n"
          "last mu nu xi omicron pi rho sigma")))
    (ekp-buffer-test--with-mode text 20
      (goto-char (point-min))
      (let* ((first (copy-marker (point-min)))
             (middle-beg (progn (forward-line 1) (point)))
             (middle-end (line-end-position))
             (last (progn (forward-line 1) (copy-marker (point)))))
        (narrow-to-region middle-beg middle-end)
        (ekp-buffer--reflow (current-buffer) 16)
        (should (= (point-min) middle-beg))
        (should (= (point-max) middle-end))
        (save-restriction
          (widen)
          (should (= (get-text-property first 'ekp-justified) 16))
          (should (= (get-text-property last 'ekp-justified) 16))
          (should (equal (substring-no-properties (buffer-string)) text)))
        (set-marker first nil)
        (set-marker last nil)))))

(ert-deftest ekp-buffer-test-live-resize-keeps-latest-effective-width ()
  "A stale resize callback cannot permanently lose the latest width."
  (let ((text "alpha beta gamma delta epsilon zeta"))
    (ekp-buffer-test--with-mode text 40
      (goto-char (point-max))
      (let ((scheduled-generation ekp-buffer--generation))
        (insert " x")
        (cl-letf (((symbol-function 'ekp-buffer--effective-width)
                   (lambda (&optional _) 12)))
          (ekp-buffer--reflow
           (current-buffer) 12 scheduled-generation))
        (should (= ekp-buffer--auto-width 12))
        (should (equal (substring-no-properties (buffer-string))
                       (concat text " x")))))))

(ert-deftest ekp-buffer-test-live-resize-publishes-active-paragraph-once ()
  "Resize must not install then replace a static active-paragraph plan."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda")
        (static-installs 0)
        (live-installs 0)
        (install-plan (symbol-function 'ekp-buffer--install-plan))
        (install-live (symbol-function 'ekp-buffer--install-live-prefix)))
    (ekp-buffer-test--with-mode text 40
      (cl-letf (((symbol-function 'ekp-buffer--install-plan)
                 (lambda (&rest arguments)
                   (setq static-installs (1+ static-installs))
                   (apply install-plan arguments)))
                ((symbol-function 'ekp-buffer--install-live-prefix)
                 (lambda (&rest arguments)
                   (setq live-installs (1+ live-installs))
                   (apply install-live arguments))))
        (ekp-buffer--reflow (current-buffer) 20))
      (should (= static-installs 0))
      (should (= live-installs 1))
      (should (equal (substring-no-properties (buffer-string)) text)))))

(ert-deftest ekp-buffer-test-font-context-change-invalidates-live-plan ()
  "Theme and frame-font changes rebuild every displayed auto buffer."
  (let ((text
         "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda")
        (calls 0)
        (original (symbol-function 'ekp-layout-plan)))
    (should
     (memq #'ekp-buffer--on-font-context-change enable-theme-functions))
    (should
     (memq #'ekp-buffer--on-font-context-change disable-theme-functions))
    (should
     (memq #'ekp-buffer--on-font-context-change after-setting-font-hook))
    (ekp-buffer-test--with-mode text 20
      (goto-char (point-max))
      (insert "x")
      (let ((old-plan (ekp-buffer--live-state-plan ekp-buffer--live-state)))
        (cl-letf (((symbol-function 'ekp-layout-plan)
                   (lambda (&rest arguments)
                     (setq calls (1+ calls))
                     (apply original arguments))))
          (run-hook-with-args 'enable-theme-functions 'ekp-test-theme))
        (should (> calls 0))
        (should-not
         (eq old-plan
             (ekp-buffer--live-state-plan ekp-buffer--live-state)))
        (should (equal (substring-no-properties (buffer-string))
                       (concat text "x")))))))

(ert-deftest ekp-buffer-test-mode-owns-and-restores-native-soft-wrap ()
  "Auto mode soft-wraps narrow windows and restores prior ownership."
  (ekp-buffer-test--with-text "alpha beta gamma delta"
    (let ((partial-default
           (default-value 'truncate-partial-width-windows)))
      (setq-local truncate-lines t)
      (kill-local-variable 'truncate-partial-width-windows)
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 80)))
        (ekp-auto-justify-mode 1)
        (unwind-protect
            (progn
              (should (local-variable-p 'truncate-lines))
              (should-not truncate-lines)
              (should
               (local-variable-p 'truncate-partial-width-windows))
              (should-not truncate-partial-width-windows))
          (ekp-auto-justify-mode -1)))
      (should (local-variable-p 'truncate-lines))
      (should truncate-lines)
      (should-not
       (local-variable-p 'truncate-partial-width-windows))
      (should
       (equal truncate-partial-width-windows partial-default)))))

(ert-deftest ekp-buffer-test-mode-enable-failure-restores-native-soft-wrap ()
  "A failed enable restores soft-wrap values, ownership, and mode state."
  (ekp-buffer-test--with-text "alpha beta gamma delta"
    (let ((partial-default
           (default-value 'truncate-partial-width-windows)))
      (setq-local truncate-lines t)
      (kill-local-variable 'truncate-partial-width-windows)
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 80))
                ((symbol-function 'ekp-buffer--reflow)
                 (lambda (&rest _) (error "forced reflow failure"))))
        (should-error (ekp-auto-justify-mode 1)
                      :type 'error))
      (should-not ekp-auto-justify-mode)
      (should-not ekp-buffer--wrap-state)
      (should (local-variable-p 'truncate-lines))
      (should truncate-lines)
      (should-not
       (local-variable-p 'truncate-partial-width-windows))
      (should
       (equal truncate-partial-width-windows partial-default)))))

(ert-deftest ekp-buffer-test-mode-roundtrip ()
  "Enabling then disabling the mode restores the buffer exactly."
  (let ((text "first paragraph 内容 aaa bbb ccc\nsecond paragraph 内容 ddd"))
    (ekp-buffer-test--with-mode text 150
      (should ekp-buffer--auto-width)
      (should (get-text-property (point-min) 'ekp-justified)))
    ;; body ran; with-mode disabled the mode on exit — verify restore
    (ekp-buffer-test--with-text text
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 150)))
        (ekp-auto-justify-mode 1)
        (ekp-auto-justify-mode -1)
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-buffer-test-mode-incremental-edit ()
  "A same-line live edit preserves the other hard line byte-for-byte."
  (let ((text "aaa bbb ccc ddd eee fff\nggg hhh iii jjj kkk lll")
        (calls nil))
    (ekp-buffer-test--with-text text
      (goto-char (point-min))
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 100)))
        (ekp-auto-justify-mode 1)
        (unwind-protect
            (let* ((hard-nl (line-end-position))
                   (second (buffer-substring (1+ hard-nl) (point-max)))
                   (orig (symbol-function 'ekp-justify-region)))
              (cl-letf (((symbol-function 'ekp-justify-region)
                         (lambda (beg end &optional width)
                           (push (list beg end width) calls)
                           (funcall orig beg end width))))
                (goto-char (+ (point-min) 4))
                (insert "zz")
                (should-not calls)
                (should ekp-buffer--live-edit)
                (should-not
                 (get-text-property (point-min) 'ekp-justified))
                (should (equal-including-properties
                         (buffer-substring
                          (+ hard-nl 3) (point-max))
                         second))))
          (ekp-auto-justify-mode -1)))
      (should (equal (buffer-string)
                     "aaa zzbbb ccc ddd eee fff\nggg hhh iii jjj kkk lll")))))

(ert-deftest ekp-buffer-test-verbatim-paragraph-skipped ()
  "A code-block paragraph stays byte-identical; prose around it justifies."
  (let* ((code (propertize "(defun foo (x)   (list 1     2))"
                           'ekp-verbatim t 'face 'font-lock-keyword-face))
         (text (concat "prose before with words enough to wrap lines\n"
                       code
                       "\nprose after also long enough to wrap lines")))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 20)
      ;; the code line is still there, character-exact, spacing intact
      (goto-char (point-min))
      (should (search-forward "(defun foo (x)   (list 1     2))" nil t))
      ;; prose received a projection while the code paragraph did not
      (should (get-text-property (point-min) 'ekp-justified))
      (goto-char (point-min))
      (search-forward "(defun")
      (should-not (get-text-property (match-beginning 0) 'ekp-justified))
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-skip-faces ()
  "Paragraphs wearing a skip face stay verbatim."
  (let* ((ekp-buffer-skip-faces '(font-lock-comment-face))
         (code (propertize ";; a  comment   line kept   as-is"
                           'face 'font-lock-comment-face))
         (text (concat "prose paragraph long enough to wrap\n" code)))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 15)
      (goto-char (point-min))
      (should (search-forward ";; a  comment   line kept   as-is" nil t))
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-org-inline-code-face-does-not-skip-paragraph ()
  "An Org inline-code face must not make prose stay native-wrapped."
  (let* ((code (propertize "(ekp-pixel-justify STR W)" 'face 'org-code))
         (text (concat "行内原子演示: 代码片段 " code
                       " 之后的正文仍然需要由 EKP 负责断行。")))
    (ekp-buffer-test--with-text text
      (ekp-org-setup)
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (goto-char (+ (point-min) 7))
      (set-mark (+ (point-min) 2))
      (let ((point-before (point))
            (mark-before (mark t))
            (chars-tick (buffer-chars-modified-tick))
            (undo-before buffer-undo-list)
            (overlays-before (length (overlays-in (point-min) (point-max)))))
        (ekp-justify-region (point-min) (point-max) 20)
        (should (get-text-property (point-min) 'ekp-justified))
        (should (equal (substring-no-properties (buffer-string))
                       (substring-no-properties text)))
        (should (= (point) point-before))
        (should (= (mark t) mark-before))
        (should (= (buffer-chars-modified-tick) chars-tick))
        (should (eq buffer-undo-list undo-before))
        (should-not (buffer-modified-p))
        (should (= (length (overlays-in (point-min) (point-max)))
                   overlays-before))
        (save-excursion
          (goto-char (point-min))
          (should (search-forward "(ekp-pixel-justify STR W)" nil t))
          (should (eq (get-text-property (match-beginning 0) 'face)
                      'org-code)))
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-buffer-test-org-block-face-stays-verbatim ()
  "An Org block face still protects its own paragraph verbatim."
  (let* ((block (propertize "#+begin_src emacs-lisp\n(+ 1   2)\n#+end_src"
                            'face 'org-block))
         (text (concat "prose before wraps with EKP projection\n"
                       block
                       "\nprose after wraps with EKP projection")))
    (ekp-buffer-test--with-text text
      (ekp-org-setup)
      (ekp-justify-region (point-min) (point-max) 20)
      (goto-char (point-min))
      (should (get-text-property (point) 'ekp-justified))
      (search-forward "(+ 1   2)")
      (should-not (get-text-property (match-beginning 0) 'ekp-justified))
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-inline-protection-has-no-single-cjk-lines ()
  "A narrow public layout must not publish isolated CJK rows near inline code."
  (let* ((atom (propertize "(ekp-pixel-justify STR W)"
                           'face 'org-code
                           'ekp-no-break t))
         (text (concat "行内原子演示: 代码片段 " atom
                       " 永不拆散、空格保持字面宽度；不间断空格让 "
                       "100_000 与 3.14 MB 这类数字单位锁在同一行。")))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 11)
      (let ((lines (ekp-buffer-test--display-lines)))
        (should (> (length lines) 1))
        (should-not
         (seq-find #'ekp-buffer-test--single-cjk-line-p lines)))
      (should (equal (substring-no-properties (buffer-string))
                     (substring-no-properties text)))
      (should-not (overlays-in (point-min) (point-max)))
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-showcase-1d-publishes-no-single-cjk-lines ()
  "The exact public 168px showcase sample must not publish isolated CJK rows."
  (let ((text (ekp-showcase--sample))
        (ekp-use-c-module nil))
    (ekp-buffer-test--with-text text
      (ekp-showcase-mode)
      (ekp-justify-region (point-min) (point-max) 168)
      (let* ((lines (ekp-buffer-test--display-lines))
             (single-cjk-lines
              (seq-filter #'ekp-buffer-test--single-cjk-line-p lines)))
        (should (> (length lines) 1))
        (should-not single-cjk-lines))
      (should (equal (substring-no-properties (buffer-string))
                     (substring-no-properties text)))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-showcase-parshape-publishes-no-single-cjk-lines ()
  "The exact parshape 280px showcase must not publish isolated CJK rows."
  (let ((text (ekp-showcase--sample))
        (ekp-use-c-module nil)
        (ekp-looseness 1))
    (ekp-buffer-test--with-text text
      (ekp-showcase-mode)
      (setq-local ekp-showcase--width 280
                  ekp-showcase--parshape-on t)
      (ekp-showcase--apply-parshape)
      (ekp-justify-region (point-min) (point-max) 280)
      (let* ((lines (ekp-buffer-test--display-lines))
             (single-cjk-lines
              (seq-filter #'ekp-buffer-test--single-cjk-line-p lines)))
        (should (> (length lines) 1))
        (should-not single-cjk-lines))
      (should (equal (substring-no-properties (buffer-string))
                     (substring-no-properties text)))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-break-policy-precedence-public-path ()
  "Public policy resolution is region, local, mode, then global."
  (dolist (option '(ekp-hyphenation
                    ekp-inline-code-policy
                    ekp-buffer-mode-policy-alist
                    ekp-buffer-inline-faces
                    ekp-overlong-token-policy))
    (should (boundp option)))
  (let ((original-hyphenation (default-value 'ekp-hyphenation))
        (original-inline-policy (default-value 'ekp-inline-code-policy))
        (original-inline-faces (default-value 'ekp-buffer-inline-faces))
        (original-mode-policy
         (default-value 'ekp-buffer-mode-policy-alist))
        (original-overlong (default-value 'ekp-overlong-token-policy)))
    (unwind-protect
        (let* ((word "internationalization")
               (auto-atom (propertize "auto inline atom" 'face 'org-code))
               (explicit-atom
                (propertize "manual no break atom with spaces"
                            'face 'org-code 'ekp-no-break t))
               (text (concat word " " word " " auto-atom " " explicit-atom))
               captures
               (original-layout (symbol-function 'ekp-buffer--layout-plan)))
          (set-default-toplevel-value 'ekp-hyphenation 'off)
          (set-default-toplevel-value 'ekp-inline-code-policy 'normal)
          (set-default-toplevel-value 'ekp-buffer-inline-faces nil)
          (set-default-toplevel-value
           'ekp-overlong-token-policy 'emergency)
          (set-default-toplevel-value
           'ekp-buffer-mode-policy-alist
           '((text-mode . ((ekp-hyphenation . on)
                           (ekp-inline-code-policy . no-break)
                           (ekp-buffer-inline-faces . (org-code))
                           (ekp-overlong-token-policy . natural)))))
          (ekp-buffer-test--with-text text
            (text-mode)
            (let ((region-beg (save-excursion
                                (goto-char (point-min))
                                (search-forward word)
                                (search-forward word)
                                (match-beginning 0)))
                  (explicit-beg (save-excursion
                                  (goto-char (point-min))
                                  (search-forward explicit-atom)
                                  (match-beginning 0)))
                  (explicit-end (save-excursion
                                  (goto-char (point-min))
                                  (search-forward explicit-atom)
                                  (match-end 0))))
              (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                         (lambda (source width context)
                           (push (list :source (copy-sequence source)
                                       :width width
                                       :context (copy-tree context))
                                 captures)
                           (funcall original-layout source width context))))
                ;; Mode profile beats real global defaults; no profile value is
                ;; copied into a buffer-local variable.
                (ekp-justify-region (point-min) (point-max) 80)
                (let ((context (plist-get (car captures) :context)))
                  (should (eq (plist-get context :hyphenation) 'on))
                  (should (eq (plist-get context :inline-code-policy)
                              'no-break))
                  (should (equal (plist-get context :inline-faces)
                                 '(org-code)))
                  (should (eq (plist-get context :overlong-token-policy)
                              'natural)))
                (should-not (local-variable-p 'ekp-hyphenation))
                (should-not (local-variable-p 'ekp-inline-code-policy))
                (should-not (local-variable-p 'ekp-buffer-inline-faces))
                (ekp-unjustify-region (point-min) (point-max))
                (setq captures nil)

                ;; Explicit buffer/file/dir-local values use the same public
                ;; variables and outrank the matching mode profile.
                (setq-local ekp-hyphenation 'off)
                (setq-local ekp-inline-code-policy 'normal)
                (setq-local ekp-buffer-inline-faces nil)
                (ekp-justify-region (point-min) (point-max) 80)
                (let ((context (plist-get (car captures) :context)))
                  (should (eq (plist-get context :hyphenation) 'off))
                  (should (eq (plist-get context :inline-code-policy)
                              'normal))
                  (should-not (plist-get context :inline-faces)))
                (ekp-unjustify-region (point-min) (point-max))
                (setq captures nil)

                ;; Exact region policy is passed only on the annotated source
                ;; range and outranks explicit locals in the core planner.
                (setq-local ekp-overlong-token-policy 'emergency)
                (put-text-property region-beg (+ region-beg (length word))
                                   'ekp-break-policy 'hyphenate)
                (put-text-property explicit-beg explicit-end
                                   'ekp-break-policy 'hyphenate)
                (ekp-justify-region (point-min) (point-max) 40)
                (should (get-text-property (point-min) 'ekp-justified))
                (let* ((source (plist-get (car captures) :source))
                       (region-offset (- region-beg (point-min)))
                       (region-end (+ region-offset (length word)))
                       (before-explicit (- explicit-beg (point-min)))
                       (explicit-offset before-explicit)
                       (explicit-finish (- explicit-end (point-min))))
                  (should
                   (eq (get-text-property region-offset 'ekp-break-policy
                                          source)
                       'hyphenate))
                  (should-not
                   (get-text-property (1- region-offset) 'ekp-break-policy
                                      source))
                  (should-not
                   (get-text-property region-end 'ekp-break-policy source))
                  (should
                   (eq (get-text-property explicit-offset 'ekp-break-policy
                                          source)
                       'hyphenate))
                  (should
                   (get-text-property explicit-offset 'ekp-no-break source))
                  (should-not
                   (seq-some
                    (lambda (pos)
                      (and (>= pos explicit-beg) (< pos explicit-end)))
                    (ekp-buffer-test--display-newline-positions)))
                  (should (< before-explicit explicit-finish)))
                (should (equal (substring-no-properties (buffer-string))
                               (substring-no-properties text)))
                (ekp-unjustify-region (point-min) (point-max))
                (should (equal (substring-no-properties (buffer-string))
                               (substring-no-properties text)))))))
      (set-default-toplevel-value 'ekp-hyphenation original-hyphenation)
      (set-default-toplevel-value
       'ekp-inline-code-policy original-inline-policy)
      (set-default-toplevel-value
       'ekp-buffer-inline-faces original-inline-faces)
      (set-default-toplevel-value
       'ekp-buffer-mode-policy-alist original-mode-policy)
      (set-default-toplevel-value
       'ekp-overlong-token-policy original-overlong))))

(ert-deftest ekp-buffer-test-skip-predicate ()
  "The paragraph predicate is the general escape hatch."
  (let ((text "keepme raw   spacing\nnormal prose that wraps around"))
    (ekp-buffer-test--with-text text
      (setq-local ekp-buffer-skip-predicate
                  (lambda (p) (string-prefix-p "keepme" p)))
      (ekp-justify-region (point-min) (point-max) 12)
      (goto-char (point-min))
      (should (looking-at-p "keepme raw   spacing$"))
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal (buffer-string) text)))))

(ert-deftest ekp-buffer-test-indent-roundtrip ()
  "First-line indent spacers vanish exactly on unjustify."
  (let ((ekp-first-line-indent 6)
        (text "首行缩进往返检查内容足够长断行几次"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 30)
      (ekp-unjustify-region (point-min) (point-max))
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-resize-hook-window-arg ()
  "The resize hook handles its WINDOW argument and foreign current buffer.
Regression: buffer-local `window-size-change-functions' members get
the displaying WINDOW, with an arbitrary buffer current."
  (let ((text "resize hook 检查 aaa bbb ccc ddd eee fff"))
    (ekp-buffer-test--with-mode text 200
      (let ((buf (current-buffer))
            (win (selected-window)))
        (set-window-buffer win buf)
        (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                   (lambda (&optional _) 120)))
          ;; simulate redisplay: window argument, unrelated buffer current
          (with-temp-buffer
            (ekp-buffer--on-resize win)))
        (with-current-buffer buf
          (should (timerp ekp-buffer--resize-timer))
          (cancel-timer ekp-buffer--resize-timer)
          ;; run what the timer would have run
          (ekp-buffer--reflow buf 120)
          (should (= ekp-buffer--auto-width 120)))))))

(ert-deftest ekp-buffer-test-mode-reflow-width ()
  "Reflow updates completed paragraphs and commits the active paragraph."
  (let ((text
         (concat "reflow 检查 aaa bbb ccc ddd eee fff ggg hhh\n"
                 "active tail remains natural while editing"))
        fresh)
    (ekp-buffer-test--with-text
        "reflow 检查 aaa bbb ccc ddd eee fff ggg hhh"
      (ekp-justify-region (point-min) (point-max) 90)
      (setq fresh (buffer-substring (point-min) (point-max))))
    (ekp-buffer-test--with-mode text 200
      (ekp-buffer--reflow (current-buffer) 90)
      (should (= ekp-buffer--auto-width 90))
      (let* ((newline (save-excursion
                        (goto-char (point-min))
                        (search-forward "\n")
                        (1- (point))))
             (got (buffer-substring (point-min) newline)))
        ;; ekp-justified was written at two widths; ignore that prop
        (remove-text-properties 0 (length got) '(ekp-justified nil) got)
        (remove-text-properties 0 (length fresh) '(ekp-justified nil) fresh)
        (should (equal-including-properties got fresh))
        (should-not
         (text-property-not-all
          (1+ newline) (point-max) 'ekp-justified 90))))))


(ert-deftest ekp-buffer-test-lazy-reflow-equals-oneshot ()
  "Visible-first chunked re-flow converges to the one-shot result."
  (let ((ekp-auto-justify-lazy-threshold 100)
        (ekp-auto-justify-chunk-size 3)
        (text (mapconcat #'identity
                         (make-list 12 "段落内容 some words 足够长会换行的样子")
                         "\n")))
    (ekp-buffer-test--with-text text
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 60))
                ((symbol-function 'ekp-buffer--visible-span)
                 (lambda () (cons (point-min) (min (point-max) 80)))))
        (ekp-auto-justify-mode 1)
        (ekp-buffer--reflow (current-buffer) 50)
        (should ekp-buffer--pending)
        ;; drain the background queue synchronously
        (let ((guard 0))
          (while (and ekp-buffer--pending (< guard 100))
            (when (timerp ekp-buffer--chunk-timer)
              (cancel-timer ekp-buffer--chunk-timer)
              (setq ekp-buffer--chunk-timer nil))
            (ekp-buffer--process-chunk (current-buffer))
            (setq guard (1+ guard))))
        (should-not ekp-buffer--pending)
        (let ((lazy (buffer-string)))
          (ekp-auto-justify-mode -1)
          (ekp-justify-region (point-min) (point-max) 50)
          (should (equal-including-properties (buffer-string) lazy)))))))

(ert-deftest ekp-buffer-test-lazy-reflow-preserves-active-live-spans ()
  "Pending lazy chunks never detach spans owned by the active paragraph."
  (let ((ekp-auto-justify-lazy-threshold 1)
        (ekp-auto-justify-chunk-size 1)
        (text
         (mapconcat
          (lambda (index)
            (format
             "para%02d alpha beta gamma delta epsilon zeta eta theta"
             index))
          (number-sequence 1 8)
          "\n")))
    (ekp-buffer-test--with-text text
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 20))
                ((symbol-function 'ekp-buffer--visible-span)
                 (lambda ()
                   (cons (point-min)
                         (save-excursion
                           (goto-char (point-min))
                           (line-end-position))))))
        (ekp-auto-justify-mode 1)
        (unwind-protect
            (progn
              (goto-char (point-max))
              (insert " x")
              (ekp-buffer--reflow (current-buffer) 16)
              (while (cdr ekp-buffer--pending)
                (ekp-buffer--process-one-chunk))
              (should-not
               (seq-find
                (lambda (span)
                  (not (marker-buffer (ekp-buffer--span-beg span))))
                (ekp-buffer--live-state-spans ekp-buffer--live-state)))
              (goto-char (point-max))
              (insert " y"))
          (ekp-auto-justify-mode -1))))))

;;;; Editor-state integrity (save / modified / undo / stickiness)

(ert-deftest ekp-buffer-test-typed-char-inherits-no-marker ()
  "Text typed beside a projection never inherits stale layout properties."
  (ekp-buffer-test--with-text "aaa bbb 中文 ccc"
    (ekp-justify-region (point-min) (point-max) 200)
    (let ((display-pos (text-property-not-all
                        (point-min) (point-max)
                        'ekp-buffer--display nil)))
      (should display-pos)
      (goto-char (1+ display-pos))
      (insert-and-inherit "X")
      (let ((x (1+ display-pos)))
        (should-not (get-text-property x 'ekp-justified))
        (should-not (get-text-property x 'ekp-buffer--display))
        (should-not (get-text-property x 'display))
        (should-not (get-text-property x 'line-prefix))))
    (ekp-unjustify-region (point-min) (point-max))
    (should (= 1 (cl-count ?X (buffer-string))))))

(ert-deftest ekp-buffer-test-save-writes-logical-text ()
  "Saving a justified file buffer writes the logical text to disk,
keeps the buffer justified, and leaves it unmodified."
  (let* ((file (make-temp-file "ekp-save-test"))
         (text "中文保存测试内容足够长会断行的样子,再加一句凑长度。")
         (make-backup-files nil)
         (create-lockfiles nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert text)
          (ekp-justify-region (point-min) (point-max) 20)
          (should (text-property-not-all
                   (point-min) (point-max)
                   'ekp-buffer--display nil))
          (should (= (cl-count ?\n (buffer-string)) 0))
          (save-buffer)
          ;; Disk: logical text only, no layout newlines.
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         text))
          ;; Buffer: still justified, and not "modified" vs its file.
          (should (get-text-property (point-min) 'ekp-justified))
          (should-not (buffer-modified-p))
          ;; And a second save still works (state was reset).
          (insert "x")
          (goto-char (point-min))
          (save-buffer)
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         (concat text "x")))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ekp-buffer-test-failed-save-preserves-layout ()
  "A filesystem save failure must not leave the buffer unformatted."
  (let* ((dir (make-temp-file "ekp-save-fail-" t))
         (file (expand-file-name "file.txt" dir))
         (text "保存失败以后屏幕仍然保持排版状态 and remains editable")
         (make-backup-files nil)
         (create-lockfiles nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (setq-local require-final-newline nil)
          (insert text)
          (ekp-justify-region (point-min) (point-max) 20)
          (let ((layout (buffer-substring (point-min) (point-max))))
            (delete-directory dir t)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) nil)))
              (should-error (save-buffer)))
            (should (equal-including-properties
                     (buffer-substring (point-min) (point-max)) layout))
            (should (get-text-property (point-min) 'ekp-justified))
            (should (buffer-modified-p)))
          (let ((kill-buffer-query-functions nil))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest ekp-buffer-test-interrupted-save-preserves-layout ()
  "A quit during writing must not leave the buffer unformatted."
  (let* ((file (make-temp-file "ekp-save-quit-"))
         (text "保存中断以后屏幕排版状态必须原样保留 with logical text")
         (make-backup-files nil)
         (create-lockfiles nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (setq-local require-final-newline nil)
          (insert text)
          (ekp-justify-region (point-min) (point-max) 20)
          (let ((layout (buffer-substring (point-min) (point-max))))
            (cl-letf (((symbol-function 'write-region)
                       (lambda (&rest _) (signal 'quit nil))))
              (should (condition-case nil
                          (progn (save-buffer) nil)
                        (quit t))))
            (should (equal-including-properties
                     (buffer-substring (point-min) (point-max)) layout))
            (should (buffer-modified-p)))
          (let ((kill-buffer-query-functions nil))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ekp-buffer-test-encoding-save-failure-is-retryable ()
  "An encoding failure must preserve layout and allow a clean retry."
  (let* ((file (make-temp-file "ekp-save-encoding-"))
         (text "编码失败以后仍然保持排版，重试写入 logical text")
         (make-backup-files nil)
         (create-lockfiles nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (setq-local require-final-newline nil)
          (insert text)
          (ekp-justify-region (point-min) (point-max) 20)
          (let ((layout (buffer-substring (point-min) (point-max))))
            (set-buffer-file-coding-system 'us-ascii-unix)
            (cl-letf (((symbol-function 'select-safe-coding-system)
                       (lambda (&rest _) (error "Forced encoding failure"))))
              (should-error (save-buffer)))
            (should (equal-including-properties
                     (buffer-substring (point-min) (point-max)) layout))
            (should (buffer-modified-p))
            (set-buffer-file-coding-system 'utf-8-unix)
            (save-buffer)
            (should (equal-including-properties
                     (buffer-substring (point-min) (point-max)) layout))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           text)))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ekp-buffer-test-justify-preserves-unmodified ()
  "Pure re-layout must not flip `buffer-modified-p'."
  (let* ((file (make-temp-file "ekp-mod-test"))
         (make-backup-files nil)
         (create-lockfiles nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "modified 标志保持检查内容足够长断行")
          (save-buffer)
          (should-not (buffer-modified-p))
          (ekp-justify-region (point-min) (point-max) 30)
          (should-not (buffer-modified-p))
          (ekp-unjustify-region (point-min) (point-max))
          (should-not (buffer-modified-p))
          ;; A real edit still marks the buffer modified.
          (insert "y")
          (should (buffer-modified-p))
          (let ((kill-buffer-query-functions nil))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ekp-buffer-test-live-undo-restores-without-pending-layout ()
  "Real undo restores logical text and finishes its live transaction."
  (let ((text "undo guard 检查内容 aaa bbb ccc"))
    (ekp-buffer-test--with-mode text 100
      (buffer-enable-undo)
      (undo-boundary)
      (goto-char (point-max))
      (insert " x")
      (undo-boundary)
      (undo-only 1)
      (should (equal (substring-no-properties (buffer-string)) text))
      (should-not ekp-buffer--live-edit)
      (should-not (timerp ekp-buffer--composition-timer))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ekp-buffer-test-live-kill-region-reflows-through-public-path ()
  "A real `kill-region' keeps live layout and copies no projection."
  (let ((text "alpha beta gamma delta epsilon zeta")
        (kill-ring nil)
        kill-ring-yank-pointer)
    (ekp-buffer-test--with-mode text 16
      (goto-char (point-min))
      (search-forward "gamma ")
      (kill-region (match-beginning 0) (match-end 0))
      (should (equal (substring-no-properties (buffer-string))
                     "alpha beta delta epsilon zeta"))
      (should (equal (current-kill 0 t) "gamma "))
      (should-not
       (text-property-not-all
        0 (length (current-kill 0 t))
        'ekp-buffer--display nil (current-kill 0 t)))
      (should ekp-buffer--live-state)
      (should ekp-buffer--live-edit)
      (ekp-buffer-test--assert-natural-range
       (marker-position
        (ekp-buffer--live-edit-dirty-beg ekp-buffer--live-edit))
       (marker-position
        (ekp-buffer--live-edit-dirty-end ekp-buffer--live-edit))))))

(ert-deftest ekp-buffer-test-live-newline-keeps-new-point-line-natural ()
  "A user hard break commits neighbors but keeps the new point line active."
  (let ((text "alpha beta gamma delta epsilon zeta"))
    (ekp-buffer-test--with-mode text 16
      (goto-char (point-min))
      (search-forward "gamma ")
      (insert "\n")
      (should (get-text-property (point-min) 'ekp-justified))
      (should ekp-buffer--live-state)
      (should (ekp-buffer--live-state-contains-p (point)))
      (should-not (get-text-property (point) 'ekp-justified))
      (should (equal (substring-no-properties (buffer-string))
                     "alpha beta gamma \ndelta epsilon zeta"))
      (should-not ekp-buffer--live-edit))))

(ert-deftest ekp-buffer-test-major-mode-change-restores ()
  "Switching major mode tears the justified state down cleanly."
  (let ((text "major mode 切换检查 aaa bbb ccc ddd"))
    (ekp-buffer-test--with-text text
      (let ((partial-default
             (default-value 'truncate-partial-width-windows))
            observed-wrap-state)
        (setq-local truncate-lines t)
        (kill-local-variable 'truncate-partial-width-windows)
        (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                   (lambda (&optional _) 80)))
          (ekp-auto-justify-mode 1)
          (should ekp-buffer--live-state)
          (add-hook
           'change-major-mode-hook
           (lambda ()
             (setq observed-wrap-state
                   (list (local-variable-p 'truncate-lines)
                         truncate-lines
                         (local-variable-p
                          'truncate-partial-width-windows)
                         truncate-partial-width-windows)))
           t t)
          (fundamental-mode)
          (should (equal observed-wrap-state
                         (list t t nil partial-default)))
          (should (equal (buffer-string) text))
          (should-not ekp-auto-justify-mode)
          (should (local-variable-p 'truncate-lines))
          (should truncate-lines)
          (should-not
           (local-variable-p 'truncate-partial-width-windows)))))))

;;;; Ecosystem compatibility (kill ring / isearch / fields / read-only)

(ert-deftest ekp-buffer-test-kill-ring-gets-logical-text ()
  "Copying projected text strips EKP properties from logical characters."
  (let ((text "中文复制检查内容足够长会断行 with some latin"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 20)
      (should (local-variable-p 'filter-buffer-substring-function))
      (should (equal (filter-buffer-substring (point-min) (point-max))
                     text)))))

(ert-deftest ekp-buffer-test-copy-filter-composes-and-restores ()
  "EKP must preserve an existing buffer-local substring filter."
  (let ((text "组合复制过滤器必须保留 logical text and prefix"))
    (ekp-buffer-test--with-text text
      (let ((prior (lambda (beg end &optional delete)
                     (let ((text (buffer-substring beg end)))
                       (when delete (delete-region beg end))
                       (concat "PRE:" text)))))
        (setq-local filter-buffer-substring-function prior)
        (ekp-justify-region (point-min) (point-max) 20)
        (should (equal (filter-buffer-substring (point-min) (point-max))
                       (concat "PRE:" text)))
        (ekp-unjustify-region (point-min) (point-max))
        (should (local-variable-p 'filter-buffer-substring-function))
        (should (eq filter-buffer-substring-function prior))))))

(ert-deftest ekp-buffer-test-kill-filter-composes-delete ()
  "Composed filtering must preserve DELETE and prior-filter semantics."
  (let ((text "组合 kill 过滤器删除源文本但返回 logical text"))
    (ekp-buffer-test--with-text text
      (let ((prior (lambda (beg end &optional delete)
                     (let ((text (buffer-substring beg end)))
                       (when delete (delete-region beg end))
                       (concat "PRE:" text)))))
        (setq-local filter-buffer-substring-function prior)
        (ekp-justify-region (point-min) (point-max) 20)
        (should (equal (filter-buffer-substring
                        (point-min) (point-max) t)
                       (concat "PRE:" text)))
        (should (= (point-min) (point-max)))
        (should (eq filter-buffer-substring-function prior))))))

(ert-deftest ekp-buffer-test-restores-inherited-copy-filter ()
  "Final unjustify must reveal an inherited substring filter again."
  (let ((prior (lambda (beg end &optional delete)
                 (prog1 (buffer-substring beg end)
                   (when delete (delete-region beg end))))))
    (let ((filter-buffer-substring-function prior))
      (ekp-buffer-test--with-text "继承 filter 恢复检查内容"
        (ekp-justify-region (point-min) (point-max) 20)
        (ekp-unjustify-region (point-min) (point-max))
        (should-not (local-variable-p 'filter-buffer-substring-function))
        (should (eq filter-buffer-substring-function prior))))))

(ert-deftest ekp-buffer-test-mode-disable-restores-copy-filter ()
  "Disabling auto mode must restore the previous local copy filter."
  (let ((prior (lambda (beg end &optional delete)
                 (prog1 (buffer-substring beg end)
                   (when delete (delete-region beg end))))))
    (ekp-buffer-test--with-text "关闭 mode 恢复已有 copy filter"
      (setq-local filter-buffer-substring-function prior)
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 20)))
        (ekp-auto-justify-mode 1)
        (goto-char (point-max))
        (insert " x")
        (let ((prefix-end
               (ekp-buffer--live-state-prefix-end
                ekp-buffer--live-state)))
          (should (markerp prefix-end))
          (ekp-auto-justify-mode -1)
          (should-not (marker-buffer prefix-end))))
      (should-not ekp-buffer--live-state)
      (should (local-variable-p 'filter-buffer-substring-function))
      (should (eq filter-buffer-substring-function prior)))))

(ert-deftest ekp-buffer-test-final-unjustify-removes-integrations ()
  "Removing the final layout span must remove unused integrations."
  (ekp-buffer-test--with-text "最后一个排版区间移除后清理集成 hooks"
    (ekp-justify-region (point-min) (point-max) 20)
    (should (memq #'ekp-buffer--before-change
                  before-change-functions))
    (should (local-variable-p 'filter-buffer-substring-function))
    (ekp-unjustify-region (point-min) (point-max))
    (should-not (local-variable-p 'filter-buffer-substring-function))
    (should-not (memq #'ekp-buffer--before-change
                      before-change-functions))))

(ert-deftest ekp-buffer-test-isearch-sees-logical-text ()
  "Search sees logical text directly without suspending the projection."
  (let ((text "跨行搜索的目标短语必须能找到 internationalization word"))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 20)
      (let ((justified (buffer-string)))
        (should (= (cl-count ?\n justified) 0))
        (goto-char (point-min))
        (should (search-forward "目标短语必须能找到" nil t))
        (goto-char (point-min))
        (should (search-forward "internationalization" nil t))
        (should (equal-including-properties (buffer-string) justified))))))

(ert-deftest ekp-buffer-test-field-paragraph-skipped ()
  "Paragraphs containing field or read-only text stay verbatim."
  (let* ((prompt (propertize "shell> " 'field 'output))
         (text (concat prompt "command output   here\n"
                       "prose paragraph long enough to wrap around")))
    (ekp-buffer-test--with-text text
      (ekp-justify-region (point-min) (point-max) 15)
      (goto-char (point-min))
      (should (search-forward "command output   here" nil t)))))

(ert-deftest ekp-buffer-test-read-only-command-barfs ()
  "Interactive justify on a read-only buffer signals, not corrupts."
  (ekp-buffer-test--with-text "read only 检查内容"
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (read-only-mode 1)
    (should-error (call-interactively #'ekp-justify-region)
                  :type 'buffer-read-only)))

(ert-deftest ekp-buffer-test-justify-invalid-width-preserves-projection ()
  "Invalid manual widths signal before clearing an existing projection."
  (ekp-buffer-test--with-text "invalid width must preserve this projection"
    (ekp-justify-region (point-min) (point-max) 20)
    (let ((before (buffer-substring (point-min) (point-max)))
          (spans ekp-buffer--spans))
      (dolist (width '(0 -1 1.5 "80"))
        (should-error (ekp-justify-region (point-min) (point-max) width)
                      :type 'user-error)
        (should (equal-including-properties
                 (buffer-substring (point-min) (point-max)) before))
        (should (eq ekp-buffer--spans spans))))))

(ert-deftest ekp-buffer-test-justify-empty-removes-unused-integrations ()
  "Manual justification does not retain hooks when no span was installed."
  (ekp-buffer-test--with-text ""
    (ekp-justify-region (point-min) (point-max) 20)
    (should-not ekp-buffer--spans)
    (should-not ekp-buffer--filter-installed)
    (should-not (memq #'ekp-buffer--before-change
                      before-change-functions))
    (should-not (memq #'ekp-buffer--after-layout-change
                      after-change-functions))))

(ert-deftest ekp-buffer-test-justify-foreign-only-removes-unused-integrations ()
  "A foreign-only paragraph does not retain manual EKP integrations."
  (ekp-buffer-test--with-text "foreign ownership keeps this paragraph natural"
    (put-text-property (point-min) (point-max) 'display "foreign")
    (ekp-justify-region (point-min) (point-max) 20)
    (should-not ekp-buffer--spans)
    (should-not ekp-buffer--filter-installed)
    (should-not (memq #'ekp-buffer--before-change
                      before-change-functions))
    (should-not (memq #'ekp-buffer--after-layout-change
                      after-change-functions))))

;;;; Commands and mode integration

(ert-deftest ekp-buffer-test-no-break-public-commands ()
  "Interactive no-break commands affect the public formatter and report scope."
  (ekp-buffer-test--with-text "prefix AA BB suffix words"
    (let (messages)
      (set-mark 8)
      (goto-char 13)
      (activate-mark)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (call-interactively #'ekp-no-break-region))
      (should (eq (get-text-property 8 'ekp-no-break) t))
      (ekp-justify-region (point-min) (point-max) 4)
      (should (string-match-p "AA BB" (buffer-string)))
      (ekp-unjustify-region (point-min) (point-max))
      (set-mark 8)
      (goto-char 13)
      (activate-mark)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (call-interactively #'ekp-allow-break-region))
      (should-not (get-text-property 8 'ekp-no-break))
      (should (= (length messages) 2))
      (should (cl-every
               (lambda (text)
                 (string-match-p "current buffer session" text))
               messages)))))

(ert-deftest ekp-buffer-test-verbatim-public-commands ()
  "Interactive verbatim commands protect the real region formatter."
  (ekp-buffer-test--with-text
      "literal block stays exactly here\nordinary prose wraps here"
    (goto-char (point-min))
    (let ((first-end (line-end-position)) messages)
      (set-mark (point-min))
      (goto-char first-end)
      (activate-mark)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (call-interactively #'ekp-verbatim-region))
      (ekp-justify-region (point-min) (point-max) 8)
      (should (equal (buffer-substring-no-properties
                      (point-min) (line-end-position))
                     "literal block stays exactly here"))
      (ekp-unjustify-region (point-min) (point-max))
      (set-mark (point-min))
      (goto-char first-end)
      (activate-mark)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (call-interactively #'ekp-clear-verbatim-region))
      (ekp-justify-region (point-min) first-end 8)
      (should (get-text-property (point-min) 'ekp-justified))
      (should (= (length messages) 2))
      (should (cl-every
               (lambda (text)
                 (string-match-p "current buffer session" text))
               messages)))))

(ert-deftest ekp-buffer-test-protection-workflows-discoverable ()
  "Mode help and menu expose the existing protection workflows."
  (should (string-match-p
           "current buffer[[:space:]]+session"
           (documentation #'ekp-auto-justify-mode)))
  (let ((menu (lookup-key ekp-auto-justify-mode-map [menu-bar ekp])))
    (should (keymapp menu))
    (should (where-is-internal
             #'ekp-no-break-region ekp-auto-justify-mode-map))
    (should (where-is-internal
             #'ekp-verbatim-region ekp-auto-justify-mode-map))))

(ert-deftest ekp-buffer-test-justify-buffer-roundtrip ()
  "ekp-justify-buffer / ekp-unjustify-buffer cover the whole buffer."
  (let ((text "第一段内容足够长断行\n\n第二段 also long enough to wrap"))
    (ekp-buffer-test--with-text text
      (ekp-justify-buffer 25)
      (should (get-text-property (point-min) 'ekp-justified))
      (ekp-unjustify-buffer)
      (should (equal-including-properties (buffer-string) text)))))

(ert-deftest ekp-buffer-test-justify-dwim-paragraph ()
  "Without an active region, the commands act on the paragraph at point."
  (ekp-buffer-test--with-text
      "para one short\npara two 目标段落内容足够长会断行几次\npara three"
    (goto-char (point-min))
    (search-forward "目标")
    (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional _) 20)))
      (call-interactively #'ekp-justify-region))
    ;; Only paragraph two is justified.
    (goto-char (point-min))
    (should-not (get-text-property (point) 'ekp-justified))
    (search-forward "目标")
    (should (get-text-property (match-beginning 0) 'ekp-justified))
    (goto-char (point-max))
    (should-not (get-text-property (1- (point)) 'ekp-justified))
    ;; And unjustify DWIM restores just as well.
    (goto-char (point-min))
    (search-forward "目标")
    (call-interactively #'ekp-unjustify-region)
    (should (equal (buffer-string)
                   "para one short\npara two 目标段落内容足够长会断行几次\npara three"))))

(ert-deftest ekp-buffer-test-refill-paragraph ()
  "`ekp-refill-paragraph' re-justifies the paragraph at point."
  (ekp-buffer-test--with-text "refill 检查内容足够长会断行几次的样子\nsecond para"
    (goto-char (point-min))
    (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional _) 20)))
      (ekp-refill-paragraph))
    (should (get-text-property (point-min) 'ekp-justified))
    (goto-char (point-max))
    (should-not (get-text-property (1- (point)) 'ekp-justified))))

(ert-deftest ekp-buffer-test-markdown-setup ()
  "Markdown setup preserves the mode's display-property ownership."
  (with-temp-buffer
    (setq-local font-lock-extra-managed-props '(display composition))
    (ekp-markdown-setup)
    (should (equal font-lock-extra-managed-props
                   '(display composition)))
    (should (local-variable-p 'ekp-buffer-skip-faces))
    (should (local-variable-p 'ekp-buffer-inline-faces))
    (should (equal ekp-buffer-skip-faces ekp-buffer-markdown-block-faces))
    (should (equal ekp-buffer-inline-faces
                   ekp-buffer-markdown-inline-faces))))

(ert-deftest ekp-buffer-test-org-auto-preset ()
  "Auto mode consults the Org profile without copying preset locals."
  (with-temp-buffer
    (org-mode)
    (insert "普通正文段落内容足够长断行几次的样子")
    (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional _) 100)))
      (ekp-auto-justify-mode 1)
      (unwind-protect
          (let ((context (ekp-buffer--policy-context)))
            (should-not (local-variable-p 'ekp-buffer-skip-faces))
            (should-not (local-variable-p 'ekp-buffer-inline-faces))
            (should (equal (plist-get context :block-faces)
                           ekp-buffer-org-block-faces))
            (should (equal (plist-get context :inline-faces)
                           ekp-buffer-org-inline-faces)))
        (ekp-auto-justify-mode -1)))))

;;;; Lazy re-flow scheduling

(ert-deftest ekp-buffer-test-live-unsupported-gap-stays-verbatim ()
  "Live layout stops instead of pretending to shrink non-ASCII whitespace."
  (let ((text "aa\tbb cc dd")
        (original (symbol-function 'ekp--measured-width)))
    (cl-letf (((symbol-function 'ekp--measured-width)
               (lambda (string)
                 (if (equal (substring-no-properties string) "\t")
                     5
                   (funcall original string)))))
      (ekp-buffer-test--with-mode text 100
        (setq ekp-buffer--conflicts nil)
        (goto-char (point-max))
        (insert " x")
        (ekp-buffer--commit-live-paragraph)
        (should-not ekp-buffer--spans)
        (should-not (text-property-not-all
                     (point-min) (point-max) 'ekp-justified nil))
        (should
         (string-match-p
          "unsupported whitespace shrink"
          (caddr (car ekp-buffer--conflicts))))
        (should (equal (substring-no-properties (buffer-string))
                       (concat text " x")))))))

(ert-deftest ekp-buffer-test-live-oversized-paragraph-never-blocks-on-planning ()
  "Automatic layout leaves an oversized hard paragraph natural."
  (let ((ekp-auto-justify-paragraph-limit 20)
        (text (make-string 60 ?a))
        calls)
    (ekp-buffer-test--with-text text
      (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional _) 12))
                ((symbol-function 'ekp-layout-plan)
                 (lambda (&rest _)
                   (setq calls (1+ (or calls 0)))
                   (error "oversized paragraph reached the planner"))))
        (ekp-auto-justify-mode 1)
        (unwind-protect
            (progn
              (goto-char (point-max))
              (insert "x")
              (should-not calls)
              (should-not ekp-buffer--spans)
              (should-not (text-property-not-all
                           (point-min) (point-max)
                           'ekp-justified nil))
              (should
               (string-match-p
                "automatic paragraph limit"
                (caddr (car ekp-buffer--conflicts))))
              (should (equal (substring-no-properties (buffer-string))
                             (concat text "x"))))
          (ekp-auto-justify-mode -1))))))

(ert-deftest ekp-buffer-test-prioritize-visible-chunks ()
  "Chunks intersecting the visible span move to the queue front."
  (ekp-buffer-test--with-text "abc"
    (setq ekp-buffer--auto-width 100)
    (let* ((mk (lambda (a b) (cons (copy-marker a) (copy-marker b))))
           (c1 (funcall mk 1 2))
           (c2 (funcall mk 2 3))
           (c3 (funcall mk 3 4)))
      (setq ekp-buffer--pending (cons 100 (list c1 c2 c3)))
      (cl-letf (((symbol-function 'ekp-buffer--visible-span)
                 (lambda () (cons 3 4))))
        (ekp-buffer--prioritize-visible))
      (should (eq (cadr ekp-buffer--pending) c3))
      (ekp-buffer--cancel-pending))))

(ert-deftest ekp-buffer-test-tick-budget-batches-chunks ()
  "A generous tick budget drains several chunks in one tick;
a zero budget still makes progress (exactly one chunk)."
  (let ((text (mapconcat #'identity
                         (make-list 6 "分块预算检查内容足够长")
                         "\n")))
    (ekp-buffer-test--with-text text
      (setq ekp-buffer--auto-width 40)
      (setq ekp-buffer--pending
            (cons 40 (ekp-buffer--make-chunks (point-min) (point-max))))
      (setq-local ekp-auto-justify-chunk-size 1)
      ;; zero budget: one chunk per tick
      (let ((ekp-auto-justify-tick-budget 0)
            (before (length (cdr ekp-buffer--pending))))
        (cl-letf (((symbol-function 'input-pending-p) #'ignore))
          (let ((ekp-auto-justify-mode t))
            (ekp-buffer--process-chunk (current-buffer))))
        (should (= (length (cdr ekp-buffer--pending)) (1- before))))
      (when (timerp ekp-buffer--chunk-timer)
        (cancel-timer ekp-buffer--chunk-timer)
        (setq ekp-buffer--chunk-timer nil))
      ;; big budget: the rest drains in one tick
      (let ((ekp-auto-justify-tick-budget 10.0))
        (cl-letf (((symbol-function 'input-pending-p) #'ignore))
          (let ((ekp-auto-justify-mode t))
            (ekp-buffer--process-chunk (current-buffer)))))
      (should-not ekp-buffer--pending))))

;;;; G003 configurable buffer policy contracts

(defun ekp-buffer-test--private-policy-property-present-p ()
  "Return non-nil when any private policy property leaked to the buffer."
  (let ((properties '(ekp--break-policy ekp--hyphenation ekp--literal-spacing
                      ekp--policy-provenance ekp--automatic-no-break
                      ekp--resolved-policy ekp--no-hyphen
                      ekp--token-category ekp--downgraded-no-break))
        (pos (point-min))
        hit)
    (while (and (< pos (point-max)) (not hit))
      (setq hit
            (seq-some
             (lambda (property)
               (get-text-property pos property))
             properties)
            pos (1+ pos)))
    hit))

(defun ekp-buffer-test--drain-policy-reflow ()
  "Run the scheduled zero-delay policy reflow for the current buffer."
  (let* ((timer ekp-buffer--policy-reflow-timer)
         (callback (timer--function timer))
         (arguments (timer--args timer)))
    (should (timerp timer))
    (should (eq callback #'ekp-buffer--reflow-for-policy-change))
    (apply callback arguments)
    (when (timerp ekp-buffer--policy-reflow-timer)
      (cancel-timer ekp-buffer--policy-reflow-timer)
      (setq ekp-buffer--policy-reflow-timer nil)))
  (should-not (timerp ekp-buffer--policy-reflow-timer)))

(ert-deftest ekp-buffer-test-g003-inline-profile-is-exact-span-only ()
  "Inline face lists affect exact spans; outside gaps remain breakable."
  (let* ((inline-text "aa  bb   cc")
         (inline (propertize inline-text 'face '(org-code bold)))
         (block (propertize "#+begin_src\n(+ 1   2)\n#+end_src"
                            'face 'org-block))
         (text (concat "prefix prose wraps before " inline
                       " after wraps more words with trailing prose\n"
                       block "\n"
                       "tail prose wraps normally")))
    (ekp-buffer-test--with-text text
      (ekp-org-setup)
      (let ((before (buffer-string))
            inline-beg inline-end)
        (goto-char (point-min))
        (search-forward inline-text)
        (setq inline-beg (match-beginning 0)
              inline-end (match-end 0))
        (ekp-justify-region (point-min) (point-max) 160)
        (should (get-text-property (point-min) 'ekp-justified))
        (should (equal (buffer-substring-no-properties inline-beg inline-end)
                       inline-text))
        (let* ((span (seq-find
                      (lambda (candidate)
                        (and (= (marker-position
                                 (ekp-buffer--span-beg candidate))
                                (point-min))
                             (< inline-end
                                (marker-position
                                 (ekp-buffer--span-end candidate)))))
                      ekp-buffer--spans))
               (plan (and span (ekp-buffer--span-plan span)))
               (source (and plan (ekp-layout-plan-string plan)))
               (base (and span
                          (marker-position (ekp-buffer--span-beg span))))
               (inline-start (- inline-beg base))
               (inline-finish (- inline-end base))
               literal-space-runs
               outside-gap-seen)
          (should span)
          (should (equal (substring source inline-start inline-finish)
                         inline-text))
          (cl-loop for box across (ekp-layout-plan-boxes plan)
                   for offset across (ekp-layout-plan-offsets plan)
                   when (and (<= inline-start (car offset))
                             (<= (cdr offset) inline-finish)
                             (string-match-p
                              "\\` +\\'" (substring-no-properties box)))
                   do (push (substring-no-properties box)
                            literal-space-runs))
          (cl-loop for line across (ekp-layout-plan-lines plan)
                   do (cl-loop
                       for gap across (ekp-layout-line-gaps line)
                       for start = (ekp-layout-gap-source-start gap)
                       for finish = (ekp-layout-gap-source-end gap)
                       when (and (or (<= finish inline-start)
                                     (<= inline-finish start))
                                 (string-match-p
                                  "\\` +\\'" (substring source start finish)))
                       do (setq outside-gap-seen t)
                       when (and (<= inline-start start)
                                 (<= finish inline-finish)
                                 (string-match-p
                                  "\\` +\\'" (substring source start finish)))
                       do
                       (should (= (ekp-layout-gap-target-pixel gap)
                                  (ekp-layout-gap-natural-pixel gap)))))
          (should (member "  " literal-space-runs))
          (should (member "   " literal-space-runs))
          (should outside-gap-seen))
        (should-not
         (cl-loop for pos from inline-beg below inline-end
                  thereis
                  (let ((display (get-text-property pos 'ekp-buffer--display)))
                    (and (stringp display)
                         (string-match-p "-\n"
                                         (substring-no-properties display))))))
        (should-not (ekp-buffer-test--private-policy-property-present-p))
        (goto-char (point-min))
        (search-forward "(+ 1   2)")
        (should-not (get-text-property (match-beginning 0) 'ekp-justified))
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) before))))))

(ert-deftest ekp-buffer-test-g004-inline-face-breaks-at-source-spaces ()
  "Inline faces use the public policy path without becoming hard atoms."
  (let* ((face 'ekp-buffer-test-inline-code-face)
         (inline-text
          "(ekp pixel justify STR W)  alpha   beta gamma delta epsilon zeta")
         (text (propertize inline-text 'face face))
         (ekp-buffer-inline-faces (list face))
         (ekp-inline-code-policy 'no-hyphen)
         (ekp-hyphenation 'off)
         (ekp-use-c-module nil))
    (ekp-buffer-test--with-text text
      (let ((before (buffer-string))
            (inline-beg (point-min))
            (inline-end (point-max)))
        (ekp-justify-region (point-min) (point-max) 20)
        (should-not (text-property-not-all
                     inline-beg inline-end 'ekp-no-break nil))
        (let* ((span (seq-find
                      (lambda (candidate)
                        (and (<= (marker-position
                                   (ekp-buffer--span-beg candidate))
                                 inline-beg)
                             (<= inline-end
                                 (marker-position
                                  (ekp-buffer--span-end candidate)))))
                      ekp-buffer--spans))
               (plan (and span (ekp-buffer--span-plan span)))
               (source (and plan (ekp-layout-plan-string plan)))
               (breaks-inside
                (and plan
                     (cl-loop
                      for line across (ekp-layout-plan-lines plan)
                      for start = (ekp-layout-line-break-source-start line)
                      for end = (ekp-layout-line-break-source-end line)
                      when (and start end (< start end)
                                (<= start (length inline-text))
                                (<= end (length inline-text)))
                      collect (substring source start end))))
               (hyphen-inside
                (and plan
                     (seq-some #'ekp-layout-line-hyphen-p
                               (append (ekp-layout-plan-lines plan) nil)))))
          (should span)
          (should plan)
          (should (> (length (ekp-layout-plan-lines plan)) 1))
          (should breaks-inside)
          (dolist (break breaks-inside)
            (should (string-match-p "\\`[[:space:]\n\r\t]+\\'"
                                    break)))
          (should-not hyphen-inside)
          (should (equal (substring-no-properties source)
                         inline-text))
          (should (equal (get-text-property 0 'face source) face)))
        (should (equal (substring-no-properties (buffer-string))
                       (substring-no-properties before)))
        (should (eq (get-text-property inline-beg 'face) face))
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest ekp-buffer-test-g003-profile-precedence-and-auto-consult-only ()
  "Auto profiles are consult-only; explicit locals and region policy win."
  (should (boundp 'ekp-buffer-mode-policy-alist))
  (let ((original-hyphenation (default-value 'ekp-hyphenation))
        (original-inline-policy (default-value 'ekp-inline-code-policy))
        (original-mode-policy
         (default-value 'ekp-buffer-mode-policy-alist)))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'ekp-hyphenation 'off)
          (set-default-toplevel-value 'ekp-inline-code-policy 'normal)
          (set-default-toplevel-value
           'ekp-buffer-mode-policy-alist
           '((org-mode . ((ekp-hyphenation . on)
                          (ekp-inline-code-policy . no-hyphen)
                          (ekp-buffer-measure . 12)))))
          (with-temp-buffer
            (org-mode)
            (insert "internationalization internationalization internationalization")
            (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                       (lambda (&optional _) 100)))
              (ekp-auto-justify-mode 1)
              (unwind-protect
                  (progn
                    ;; Mode profiles must be consulted without copying values
                    ;; into locals.  File and dir locals use these same public
                    ;; variables; no separate file/directory policy alists.
                    (should-not (local-variable-p 'ekp-buffer-skip-faces))
                    (should-not (local-variable-p 'ekp-inline-code-policy))
                    (should-not (local-variable-p 'ekp-hyphenation))
                    (should-not (local-variable-p 'ekp-buffer-measure))
                    (should (ekp-buffer-test--display-hyphen-p))
                    (ekp-unjustify-region (point-min) (point-max))
                    ;; Explicit locals are the public file/dir/buffer-local
                    ;; owner and outrank the mode profile without manual cache
                    ;; clearing.
                    (setq-local ekp-hyphenation 'off)
                    (ekp-justify-region (point-min) (point-max) 12)
                    (should-not (ekp-buffer-test--display-hyphen-p))
                    (ekp-unjustify-region (point-min) (point-max))
                    ;; Region policy is the final override over explicit
                    ;; locals.
                    (put-text-property (point-min) (+ (point-min) 20)
                                       'ekp-break-policy 'hyphenate)
                    (ekp-justify-region (point-min) (point-max) 12)
                    (should (ekp-buffer-test--display-hyphen-p)))
                (ekp-auto-justify-mode -1)))))
      (set-default-toplevel-value 'ekp-hyphenation original-hyphenation)
      (set-default-toplevel-value
       'ekp-inline-code-policy original-inline-policy)
      (set-default-toplevel-value
       'ekp-buffer-mode-policy-alist original-mode-policy))))

(ert-deftest ekp-buffer-test-g003-safe-local-values-are-closed ()
  "Documented locals are safe; malformed and executable values are rejected."
  (dolist (case '((ekp-inline-code-policy . normal)
                  (ekp-inline-code-policy . no-hyphen)
                  (ekp-inline-code-policy . no-break)
                  (ekp-hyphenation . auto)
                  (ekp-hyphenation . on)
                  (ekp-hyphenation . off)
                  (ekp-overlong-token-policy . emergency)
                  (ekp-overlong-token-policy . overflow)
                  (ekp-overlong-token-policy . natural)
                  (ekp-buffer-measure . narrowest-window)
                  (ekp-buffer-measure . (max . 24))
                  (ekp-buffer-measure . 30)
                  (ekp-emergency-stretch-pixel . nil)
                  (ekp-emergency-stretch-pixel . 0)
                  (ekp-emergency-stretch-pixel . 7)
                  (ekp-buffer-mode-policy-alist
                   . ((text-mode . ((ekp-emergency-stretch-pixel . 7)))))
                  (ekp-token-break-policies
                   . ((identifier . normal) (path . no-hyphen)))))
    (should (safe-local-variable-p (car case) (cdr case))))
  (dolist (case '((ekp-inline-code-policy . maybe)
                  (ekp-buffer-measure . window)
                  (ekp-buffer-measure . (max . "wide"))
                  (ekp-buffer-measure . 0)
                  (ekp-emergency-stretch-pixel . -1)
                  (ekp-emergency-stretch-pixel . 1.5)
                  (ekp-buffer-mode-policy-alist
                   . ((text-mode . ((ekp-emergency-stretch-pixel . -1)))))
                  (ekp-token-break-policies . ((identifier . execute)))
                  (ekp-buffer-skip-faces . ((lambda () t)))
                  (ekp-buffer-skip-predicate . ignore)
                  (ekp-buffer-skip-predicate . (lambda (_) t))))
    (should-not (safe-local-variable-p (car case) (cdr case)))))

(ert-deftest ekp-buffer-test-emergency-stretch-policy-context-and-reflow ()
  "Emergency stretch follows global/profile/local precedence and reflows once."
  (let ((original-stretch (default-value 'ekp-emergency-stretch-pixel))
        (original-profile (default-value 'ekp-buffer-mode-policy-alist)))
    (unwind-protect
        (progn
          (should (memq #'ekp-buffer--policy-variable-changed
                        (get-variable-watchers
                         'ekp-emergency-stretch-pixel)))
          (set-default-toplevel-value 'ekp-emergency-stretch-pixel 3)
          (set-default-toplevel-value
           'ekp-buffer-mode-policy-alist
           '((text-mode . ((ekp-emergency-stretch-pixel . 7)))))
          (ekp-buffer-test--with-text "emergency stretch context alpha beta"
            (text-mode)
            (should (= (plist-get (ekp-buffer--policy-context)
                                  :emergency-stretch-pixel)
                       7))
            (setq-local ekp-emergency-stretch-pixel 11)
            (should (= (plist-get (ekp-buffer--policy-context)
                                  :emergency-stretch-pixel)
                       11))
            (kill-local-variable 'ekp-emergency-stretch-pixel)
            (should (= (plist-get (ekp-buffer--policy-context)
                                  :emergency-stretch-pixel)
                       7))
            (setq-default ekp-buffer-mode-policy-alist nil)
            (should (= (plist-get (ekp-buffer--policy-context)
                                  :emergency-stretch-pixel)
                       3)))
          (ekp-buffer-test--with-text "emergency stretch watcher alpha beta"
            (text-mode)
            (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                       (lambda (&optional _) 30)))
              (ekp-auto-justify-mode 1)
              (unwind-protect
                  (let ((calls 0)
                        contexts
                        (original-layout
                         (symbol-function 'ekp-buffer--layout-plan))
                        (original-reflow
                         (symbol-function 'ekp-buffer--reflow)))
                    (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                               (lambda (source width context)
                                 (push (copy-tree context) contexts)
                                 (funcall original-layout
                                          source width context)))
                              ((symbol-function 'ekp-buffer--reflow)
                               (lambda (&rest args)
                                 (setq calls (1+ calls))
                                 (apply original-reflow args))))
                      (setq-default ekp-emergency-stretch-pixel 9)
                      (should (= calls 0))
                      (ekp-buffer-test--drain-policy-reflow)
                      (should (= calls 1))
                      (should (= (plist-get (car contexts)
                                            :emergency-stretch-pixel)
                                 9))))
                (ekp-auto-justify-mode -1)))))
      (set-default-toplevel-value 'ekp-emergency-stretch-pixel original-stretch)
      (set-default-toplevel-value
       'ekp-buffer-mode-policy-alist original-profile))))

(ert-deftest ekp-buffer-test-g003-break-policy-region-commands ()
  "Region commands write exact break-policy values and fail read-only first."
  (dolist (command '(ekp-normal-break-region
                    ekp-enable-hyphenation-region
                    ekp-disable-hyphenation-region
                    ekp-clear-break-policy-region))
    (should (fboundp command)))
  (ekp-buffer-test--with-text "alpha beta gamma"
    (let ((commands '((ekp-normal-break-region . normal)
                      (ekp-enable-hyphenation-region . hyphenate)
                      (ekp-disable-hyphenation-region . no-hyphen)
                      (ekp-clear-break-policy-region . nil)))
          messages)
      (dolist (entry commands)
        (set-mark (point-min))
        (goto-char (+ (point-min) 5))
        (activate-mark)
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (call-interactively (car entry)))
        (should (eq (get-text-property (point-min) 'ekp-break-policy)
                    (cdr entry))))
      (should (cl-every
               (lambda (text) (string-match-p "current buffer session" text))
               messages))))
  (ekp-buffer-test--with-text "read only"
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (read-only-mode 1)
    (should-error (call-interactively #'ekp-enable-hyphenation-region)
                  :type 'buffer-read-only)))

(defun ekp-buffer-test--manual-policy-command-clears-projection
    (command property expected)
  "Assert COMMAND invalidates a manual projection for PROPERTY."
  (ekp-buffer-test--with-text
      "alpha beta gamma delta epsilon zeta eta theta"
    (let ((region-beg (+ (point-min) 6))
          (region-end (+ (point-min) 16)))
      (ekp-justify-region (point-min) (point-max) 20)
      (should (= (length ekp-buffer--spans) 1))
      (should ekp-buffer--filter-installed)
      (let* ((span (car ekp-buffer--spans))
             (source (ekp-layout-plan-string (ekp-buffer--span-plan span)))
             (offset (- region-beg
                        (marker-position (ekp-buffer--span-beg span)))))
        (should-not (get-text-property offset property source)))
      (set-mark region-beg)
      (goto-char region-end)
      (activate-mark)
      (cl-letf (((symbol-function 'message) #'ignore))
        (call-interactively command))
      (should (eq (get-text-property region-beg property) expected))
      (when ekp-buffer--spans
        (let* ((span (car ekp-buffer--spans))
               (source (ekp-layout-plan-string (ekp-buffer--span-plan span)))
               (offset (- region-beg
                          (marker-position (ekp-buffer--span-beg span)))))
          (should (eq (get-text-property offset property source)
                      expected))))
      (should-not ekp-buffer--spans)
      (should-not ekp-buffer--filter-installed)
      (should-not (eq filter-buffer-substring-function
                      #'ekp-buffer--filter-buffer-substring)))))

(ert-deftest ekp-buffer-test-g003-manual-break-policy-invalidates-projection ()
  "Manual break-policy changes clear intersecting stale projections."
  (ekp-buffer-test--manual-policy-command-clears-projection
   #'ekp-enable-hyphenation-region 'ekp-break-policy 'hyphenate))

(ert-deftest ekp-buffer-test-g003-manual-no-break-invalidates-projection ()
  "Manual no-break changes clear intersecting stale projections."
  (ekp-buffer-test--manual-policy-command-clears-projection
   #'ekp-no-break-region 'ekp-no-break t))

(ert-deftest ekp-buffer-test-g003-measure-and-diagnose-contract ()
  "Measure modes use mocked windows and diagnose exposes effective policy."
  (with-temp-buffer
    (let ((widths '((w1 . 16) (w2 . 40))))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) '(w1 w2)))
                ((symbol-function 'ekp-buffer--window-pixel)
                 (lambda (&optional window)
                   (alist-get window widths))))
        (dolist (case '((narrowest-window . 16)
                        ((max . 12) . 12)
                        ((max . 24) . 16)
                        (30 . 30)))
          (let ((ekp-buffer-measure (car case)))
            (should (= (ekp-buffer--effective-width) (cdr case)))))
        (let* ((ekp-buffer-measure 30)
               (ekp-inline-code-policy 'no-hyphen)
               (ekp-hyphenation 'auto)
               (ekp-kinsoku-profile 'common)
               (ekp-overlong-token-policy 'emergency)
               diagnostic-message
               (report
                (cl-letf (((symbol-function 'message)
                           (lambda (format-string &rest args)
                             (setq diagnostic-message
                                   (apply #'format format-string args)))))
                  (ekp-diagnose))))
          (dolist (key '(:requested :narrowest :effective :policy
                         :conflicts :overflow-risk))
            (should (plist-member report key)))
          (should (equal (plist-get report :requested) 30))
          (should (= (plist-get report :narrowest) 16))
          (should (= (plist-get report :effective) 30))
          (should (plist-get report :overflow-risk))
          (should-not (plist-get report :conflicts))
          (let ((policy (plist-get report :policy)))
            (should (eq (plist-get policy :inline-code-policy)
                        'no-hyphen))
            (should (eq (plist-get policy :hyphenation) 'auto))
            (should (eq (plist-get policy :kinsoku-profile) 'common))
            (should (eq (plist-get policy :overlong-token-policy)
                        'emergency)))
          (dolist (fragment '("requested 30"
                              "narrowest 16"
                              "effective 30"
                              "overflow risk"
                              "0 conflicts"
                              "inline-code no-hyphen"
                              "hyphenation auto"
                              "kinsoku common"
                              "overlong emergency"))
            (should (string-match-p (regexp-quote fragment)
                                    diagnostic-message))))))))

(ert-deftest ekp-buffer-test-g003-policy-watchers-schedule-one-reflow ()
  "Global and local policy changes trigger one reflow; let bindings do not."
  (let ((original-default (default-value 'ekp-inline-code-policy))
        (original-profile (default-value 'ekp-buffer-mode-policy-alist)))
    (unwind-protect
        (progn
          (ekp-buffer-test--with-text "alpha beta gamma delta"
            (text-mode)
            (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                       (lambda (&optional _) 30)))
              (ekp-auto-justify-mode 1)
              (unwind-protect
                  (let ((calls 0)
                        contexts
                        (original-layout
                         (symbol-function 'ekp-buffer--layout-plan))
                        (original-reflow
                         (symbol-function 'ekp-buffer--reflow)))
                    (goto-char (+ (point-min) 5))
                    (set-marker (mark-marker) (+ (point-min) 11))
                    (setq mark-active nil)
                    (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                               (lambda (source width context)
                                 (push (copy-tree context) contexts)
                                 (funcall original-layout
                                          source width context)))
                              ((symbol-function 'ekp-buffer--reflow)
                               (lambda (&rest args)
                                 (setq calls (1+ calls))
                                 (apply original-reflow args))))
                      (let ((source-before
                             (substring-no-properties (buffer-string)))
                            (modified-before (buffer-modified-p))
                            (undo-before buffer-undo-list)
                            (tick-before
                             (buffer-chars-modified-tick))
                            (overlay-count-before
                             (length (overlays-in (point-min)
                                                  (point-max))))
                            (point-before (point))
                            (mark-before (marker-position (mark-marker)))
                            (mark-active-before mark-active)
                            (generation-before ekp-buffer--generation))
                        (setq-default ekp-inline-code-policy 'normal)
                        (should (= calls 0))
                        (ekp-buffer-test--drain-policy-reflow)
                        (should (= calls 1))
                        (should (> ekp-buffer--generation generation-before))
                        (should (equal (substring-no-properties
                                        (buffer-string))
                                       source-before))
                        (should (eq (buffer-modified-p) modified-before))
                        (should (eq buffer-undo-list undo-before))
                        (should (equal buffer-undo-list undo-before))
                        (should (= (buffer-chars-modified-tick)
                                   tick-before))
                        (should (= (length (overlays-in (point-min)
                                                        (point-max)))
                                   overlay-count-before))
                        (should (= (point) point-before))
                        (should (= (marker-position (mark-marker))
                                   mark-before))
                        (should (eq mark-active mark-active-before))
                        (should (eq (plist-get (car contexts)
                                               :inline-code-policy)
                                    'normal)))
                      (let ((same-generation ekp-buffer--generation))
                        (setq-default ekp-inline-code-policy 'normal)
                        (should-not (timerp ekp-buffer--policy-reflow-timer))
                        (should (= calls 1))
                        (should (= ekp-buffer--generation same-generation)))
                      (setq contexts nil)
                      (let ((local-generation ekp-buffer--generation))
                        (setq-local ekp-inline-code-policy 'no-hyphen)
                        (should (= calls 1))
                        (ekp-buffer-test--drain-policy-reflow)
                        (should (= calls 2))
                        (should (> ekp-buffer--generation local-generation))
                        (should (eq (plist-get (car contexts)
                                               :inline-code-policy)
                                    'no-hyphen)))
                      (let ((same-local-generation ekp-buffer--generation))
                        (setq-local ekp-inline-code-policy 'no-hyphen)
                        (when (timerp ekp-buffer--policy-reflow-timer)
                          (ekp-buffer-test--drain-policy-reflow))
                        (should-not (timerp ekp-buffer--policy-reflow-timer))
                        (should (= calls 2))
                        (should (= ekp-buffer--generation
                                   same-local-generation)))
                      (setq contexts nil)
                      (let ((local-shield-generation
                             ekp-buffer--generation)
                            (local-shield-cache
                             ekp-buffer--live-plan-cache)
                            (local-shield-context
                             (copy-tree (ekp-buffer--policy-context))))
                        (setq-default ekp-inline-code-policy 'no-break)
                        (when (timerp ekp-buffer--policy-reflow-timer)
                          (ekp-buffer-test--drain-policy-reflow))
                        (should-not (timerp ekp-buffer--policy-reflow-timer))
                        (should (= calls 2))
                        (should (= ekp-buffer--generation
                                   local-shield-generation))
                        (should (eq ekp-buffer--live-plan-cache
                                    local-shield-cache))
                        (should (equal (ekp-buffer--policy-context)
                                       local-shield-context))
                        (should (eq (plist-get local-shield-context
                                               :inline-code-policy)
                                    'no-hyphen)))
                      (let ((let-generation ekp-buffer--generation))
                        (let ((ekp-inline-code-policy 'normal))
                          (ignore ekp-inline-code-policy))
                        (should-not (timerp ekp-buffer--policy-reflow-timer))
                        (should (= calls 2))
                        (should (= ekp-buffer--generation let-generation)))))
                (ekp-auto-justify-mode -1)))))
          (setq-default ekp-inline-code-policy 'normal)
          (setq-default
           ekp-buffer-mode-policy-alist
           '((text-mode . ((ekp-inline-code-policy . no-hyphen)))))
          (ekp-buffer-test--with-text "profile alpha beta gamma"
            (text-mode)
            (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                       (lambda (&optional _) 30)))
              (ekp-auto-justify-mode 1)
              (unwind-protect
                  (let ((calls 0)
                        contexts
                        (original-layout
                         (symbol-function 'ekp-buffer--layout-plan))
                        (original-reflow
                         (symbol-function 'ekp-buffer--reflow)))
                    (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                               (lambda (source width context)
                                 (push (copy-tree context) contexts)
                                 (funcall original-layout
                                          source width context)))
                          ((symbol-function 'ekp-buffer--reflow)
                               (lambda (&rest args)
                                 (setq calls (1+ calls))
                                 (apply original-reflow args))))
                      (let ((generation-before ekp-buffer--generation)
                            (cache-before ekp-buffer--live-plan-cache)
                            (context-before
                             (copy-tree (ekp-buffer--policy-context))))
                        (setq-default ekp-inline-code-policy 'no-break)
                        (when (timerp ekp-buffer--policy-reflow-timer)
                          (ekp-buffer-test--drain-policy-reflow))
                        (should-not (timerp ekp-buffer--policy-reflow-timer))
                        (should (= calls 0))
                        (should (= ekp-buffer--generation
                                   generation-before))
                        (should (eq ekp-buffer--live-plan-cache
                                    cache-before))
                        (should (equal (ekp-buffer--policy-context)
                                       context-before))
                        (should (eq (plist-get context-before
                                               :inline-code-policy)
                                    'no-hyphen)))))
                (ekp-auto-justify-mode -1)))))
      (set-default-toplevel-value 'ekp-inline-code-policy original-default)
      (set-default-toplevel-value
       'ekp-buffer-mode-policy-alist original-profile)))

(ert-deftest ekp-buffer-test-g003-policy-watchers-local-profile-transitions ()
  "Local/profile effective policy transitions schedule post-set reflow."
  (let ((original-default (default-value 'ekp-inline-code-policy))
        (original-profile (default-value 'ekp-buffer-mode-policy-alist)))
    (unwind-protect
        (progn
          (setq-default ekp-inline-code-policy 'normal)
          (setq-default
           ekp-buffer-mode-policy-alist
           '((text-mode . ((ekp-inline-code-policy . no-hyphen)))))
          (ekp-buffer-test--with-text "profile transition alpha beta"
            (text-mode)
            (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
                       (lambda (&optional _) 30)))
              (ekp-auto-justify-mode 1)
              (unwind-protect
                  (let ((calls 0)
                        contexts
                        (original-layout
                         (symbol-function 'ekp-buffer--layout-plan))
                        (original-reflow
                         (symbol-function 'ekp-buffer--reflow)))
                    (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                               (lambda (source width context)
                                 (push (copy-tree context) contexts)
                                 (funcall original-layout
                                          source width context)))
                              ((symbol-function 'ekp-buffer--reflow)
                               (lambda (&rest args)
                                 (setq calls (1+ calls))
                                 (apply original-reflow args))))
                      (setq-local ekp-inline-code-policy 'normal)
                      (should (timerp ekp-buffer--policy-reflow-timer))
                      (ekp-buffer-test--drain-policy-reflow)
                      (should (= calls 1))
                      (should (eq (plist-get (car contexts)
                                             :inline-code-policy)
                                  'normal))
                      (setq contexts nil)
                      (kill-local-variable 'ekp-inline-code-policy)
                      (should (timerp ekp-buffer--policy-reflow-timer))
                      (ekp-buffer-test--drain-policy-reflow)
                      (should (= calls 2))
                      (should (eq (plist-get (car contexts)
                                             :inline-code-policy)
                                  'no-hyphen))))
                (ekp-auto-justify-mode -1)))))
      (set-default-toplevel-value 'ekp-inline-code-policy original-default)
      (set-default-toplevel-value
       'ekp-buffer-mode-policy-alist original-profile))))

(ert-deftest ekp-buffer-test-g003-natural-overlong-conflict-is-paragraph-local ()
  "Natural overlong paragraphs stay unprojected and clear after fitting."
  (let* ((ekp-overlong-token-policy 'natural)
         (long-token "supercalifragilisticexpialidocious")
         (text (concat long-token "\n汉字段落可以规划"))
         (original (symbol-function 'ekp--measured-width)))
    (cl-letf (((symbol-function 'ekp--measured-width)
               (lambda (string)
                 (let ((plain (substring-no-properties string)))
                   (if (equal plain long-token)
                       40
                     (funcall original string))))))
      (ekp-buffer-test--with-text text
        (ekp-justify-region (point-min) (point-max) 8)
        (should-not (get-text-property (point-min) 'ekp-justified))
        (should (seq-some
                 (lambda (conflict)
                   (string-match-p "overlong-token-natural" (caddr conflict)))
                 ekp-buffer--conflicts))
        (goto-char (point-max))
        (should (get-text-property (1- (point)) 'ekp-justified))
        (ekp-unjustify-region (point-min) (point-max))
        (setq ekp-buffer--conflicts nil)
        (ekp-justify-region (point-min) (point-max) 80)
        (should-not ekp-buffer--conflicts)
        (should (equal (substring-no-properties (buffer-string)) text))))))

(ert-deftest ekp-buffer-test-g003-live-identity-includes-policy-and-properties ()
  "Live history keys include policy, measure, profile, and source props."
  (dolist (symbol '(ekp-buffer-measure ekp-buffer-mode-policy-alist
                    ekp-buffer-inline-faces))
    (should (boundp symbol)))
  (ekp-buffer-test--with-text ""
    (text-mode)
    (let ((ekp-inline-code-policy 'normal)
          (ekp-buffer-measure 'narrowest-window)
          (ekp-buffer-mode-policy-alist nil))
      (setq ekp-buffer--live-plan-cache nil)
      (let ((first (cdr (ekp-buffer--live-plan-entry
                         "alpha beta gamma" 20))))
        (should (eq first
                    (cdr (ekp-buffer--live-plan-entry
                          "alpha beta gamma" 20))))
        (let ((ekp-inline-code-policy 'no-break))
          (should-not
           (eq first
               (cdr (ekp-buffer--live-plan-entry
                     "alpha beta gamma" 20)))))
        (let ((ekp-buffer-measure 24))
          (should-not
           (eq first
               (cdr (ekp-buffer--live-plan-entry
                     "alpha beta gamma" 20)))))
        (let ((ekp-buffer-mode-policy-alist
               '((text-mode . ((ekp-buffer-inline-faces . (font-lock-string-face)))))))
          (should-not
           (eq first
               (cdr (ekp-buffer--live-plan-entry
                     "alpha beta gamma" 20)))))
        (let ((annotated (copy-sequence "alpha beta gamma")))
          (put-text-property 6 10 'ekp-break-policy 'no-hyphen annotated)
          (should-not
           (eq first
               (cdr (ekp-buffer--live-plan-entry annotated 20)))))
        (dotimes (index 20)
          (ekp-buffer--live-plan-entry
           (format "history %02d alpha beta" index) 20))
        (should (= (length ekp-buffer--live-plan-cache) 16))))))

(ert-deftest ekp-buffer-test-live-plan-key-owns-policy-strings ()
  "Live plan keys must not retain mutable public policy strings."
  (ekp-buffer-test--with-text ""
    (text-mode)
    (let* ((ekp-use-c-module nil)
           (suffix (copy-sequence "uX"))
           (line-start-extra (copy-sequence "《"))
           (ekp-number-unit-suffixes (list suffix))
           (ekp-token-break-policies '((number-unit . no-break)))
           (ekp-kinsoku-profile 'custom)
           (ekp-cjk-no-line-start-extra line-start-extra)
           (text (copy-sequence "100uX alpha beta gamma delta"))
           (width 16)
           (calls 0)
           (original (symbol-function 'ekp-buffer--layout-plan))
           first key)
      (setq ekp-buffer--live-plan-cache nil)
      (cl-letf (((symbol-function 'ekp-buffer--layout-plan)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (apply original arguments))))
        (setq first (cdr (ekp-buffer--live-plan-entry text width)))
        (setq key (caar ekp-buffer--live-plan-cache))
        (should (seq-some (lambda (string) (equal string "uX"))
                          (ekp-buffer-test--strings-in-tree key)))
        (should (seq-some (lambda (string) (equal string "《"))
                          (ekp-buffer-test--strings-in-tree key)))
        (store-substring suffix 1 "Y")
        (store-substring line-start-extra 0 "》")
        (let ((changed (cdr (ekp-buffer--live-plan-entry text width))))
          (should-not (eq changed first))
          (should (= calls 2)))
        (should (seq-some (lambda (string) (equal string "uX"))
                          (ekp-buffer-test--strings-in-tree key)))
        (should (seq-some (lambda (string) (equal string "《"))
                          (ekp-buffer-test--strings-in-tree key)))))))

(provide 'ekp-buffer-tests)

;;; ekp-buffer-tests.el ends here
