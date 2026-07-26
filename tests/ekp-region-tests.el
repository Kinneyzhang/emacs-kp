;;; ekp-region-tests.el --- Tests for ekp-region.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe ERT tests for the buffer-level justification layer.
;; Widths are always passed explicitly, so no window is required.

;;; Code:

(require 'ert)
(require 'ekp-region)

(defconst ekp-region-test--samples
  (list "简单的中文段落测试内容,排版效果应当良好稳定。"
        "The quick brown fox jumps over the lazy dog several times today."
        "Mixed 中英文 paragraph with  double  spaces inside and a tail   "
        "para one\n\npara two 混排 content here\nthird para"
        "  leading indent 段落内容 preserved intact"
        "短\n\n\n多个空段落之间的内容")
  "Logical texts covering CJK, Latin, mixed, blanks, indent, tails.")

(defconst ekp-region-test--widths '(30 80 200 400)
  "Pixel widths from emergency-narrow to comfortable.")

(defmacro ekp-region-test--with-text (text &rest body)
  "Run BODY in a temp buffer containing TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     ,@body))

;;;; Roundtrip exactness

(ert-deftest ekp-region-test-roundtrip-exact ()
  "justify + unjustify restores text and properties exactly."
  (dolist (text ekp-region-test--samples)
    (dolist (w ekp-region-test--widths)
      (ekp-region-test--with-text text
        (ekp-justify-region (point-min) (point-max) w)
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-region-test-roundtrip-propertized ()
  "Roundtrip preserves user text properties."
  (let ((text (concat (propertize "加粗的中文开头内容" 'face 'bold)
                      " plain middle part "
                      (propertize "italic tail words" 'face 'italic))))
    (dolist (w '(60 250))
      (ekp-region-test--with-text text
        (ekp-justify-region (point-min) (point-max) w)
        (ekp-unjustify-region (point-min) (point-max))
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-region-test-hard-newlines-preserved ()
  "Hard newline count survives justification."
  (ekp-region-test--with-text "a 段落 one\n\nb 段落 two\nc 段落 three"
    (ekp-justify-region (point-min) (point-max) 100)
    (let ((hard 0))
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (unless (get-text-property (match-beginning 0) 'ekp-soft-break)
          (setq hard (1+ hard))))
      (should (= hard 3)))))

;;;; Justified-state invariants

(ert-deftest ekp-region-test-justified-marked ()
  "Justified region carries the ekp-justified width property."
  (ekp-region-test--with-text "中文内容需要标记属性验证正确性"
    (ekp-justify-region (point-min) (point-max) 120)
    (should (eq (get-text-property (point-min) 'ekp-justified) 120))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'ekp-justified 120))))

(ert-deftest ekp-region-test-rejustify-idempotent ()
  "Justifying at a new width equals a fresh justification at that width."
  (let ((text "The idempotence check 中英混排 must hold across widths."))
    (let (fresh)
      (ekp-region-test--with-text text
        (ekp-justify-region (point-min) (point-max) 150)
        (setq fresh (buffer-string)))
      (ekp-region-test--with-text text
        (ekp-justify-region (point-min) (point-max) 300)
        (ekp-justify-region (point-min) (point-max) 150)
        (should (equal-including-properties (buffer-string) fresh))))))

;;;; Edit robustness

(ert-deftest ekp-region-test-edit-then-unjustify ()
  "Text typed into a justified buffer survives unjustification."
  (ekp-region-test--with-text "abcdef ghijkl 中文内容 mnopqr stuvwx"
    (ekp-justify-region (point-min) (point-max) 80)
    ;; Insert inside the first word: physical == logical there.
    (goto-char (+ (point-min) 2))
    (insert "XY")
    (ekp-unjustify-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "abXYcdef ghijkl 中文内容 mnopqr stuvwx"))))

(ert-deftest ekp-region-test-point-stable ()
  "Point returns to its logical position after a roundtrip."
  (ekp-region-test--with-text "abcdef ghijkl mnopqr stuvwx yzabcd"
    (goto-char (+ (point-min) 9))       ; inside "ghijkl"
    (ekp-justify-region (point-min) (point-max) 60)
    (ekp-unjustify-region (point-min) (point-max))
    (should (= (point) (+ (point-min) 9)))))

;;;; Auto-justify mode

(defmacro ekp-region-test--with-mode (text width &rest body)
  "Enable `ekp-auto-justify-mode' on TEXT at WIDTH, run BODY, disable."
  (declare (indent 2))
  `(ekp-region-test--with-text ,text
     (cl-letf (((symbol-function 'ekp-region--window-pixel)
                (lambda (&optional _) ,width)))
       (ekp-auto-justify-mode 1)
       (unwind-protect
           (progn ,@body)
         (ekp-auto-justify-mode -1)))))

(ert-deftest ekp-region-test-mode-roundtrip ()
  "Enabling then disabling the mode restores the buffer exactly."
  (let ((text "first paragraph 内容 aaa bbb ccc\nsecond paragraph 内容 ddd"))
    (ekp-region-test--with-mode text 150
      (should ekp-region--auto-width)
      (should (get-text-property (point-min) 'ekp-justified)))
    ;; body ran; with-mode disabled the mode on exit — verify restore
    (ekp-region-test--with-text text
      (cl-letf (((symbol-function 'ekp-region--window-pixel)
                 (lambda (&optional _) 150)))
        (ekp-auto-justify-mode 1)
        (ekp-auto-justify-mode -1)
        (should (equal-including-properties (buffer-string) text))))))

(ert-deftest ekp-region-test-mode-incremental-edit ()
  "Edits re-justify only the touched paragraph, content stays correct."
  (let ((text "aaa bbb ccc ddd eee fff\nggg hhh iii jjj kkk lll")
        (calls nil))
    (ekp-region-test--with-mode text 100
      (let ((orig (symbol-function 'ekp-justify-region)))
        (cl-letf (((symbol-function 'ekp-justify-region)
                   (lambda (b e &optional px)
                     (push (cons (marker-position (copy-marker b))
                                 (marker-position (copy-marker e)))
                           calls)
                     (funcall orig b e px))))
          ;; Edit inside paragraph 1.
          (goto-char (+ (point-min) 4))
          (insert "zz")
          (should ekp-region--dirty)
          (ekp-region--flush-dirty (current-buffer))
          ;; Exactly one incremental call, confined before the hard \n.
          (should (= (length calls) 1))
          (let ((hard-nl (save-excursion
                           (goto-char (point-min))
                           (catch 'nl
                             (while (search-forward "\n" nil t)
                               (unless (get-text-property (match-beginning 0)
                                                          'ekp-soft-break)
                                 (throw 'nl (match-beginning 0))))))))
            (should (<= (cdar calls) hard-nl)))))
      ;; Logical text after disable = original with the edit applied.
      (ekp-auto-justify-mode -1)
      (should (equal (buffer-string)
                     "aaa zzbbb ccc ddd eee fff\nggg hhh iii jjj kkk lll"))
      ;; re-enable so with-mode's cleanup disable is a no-op state-wise
      (ekp-auto-justify-mode 1))))

(ert-deftest ekp-region-test-resize-hook-window-arg ()
  "The resize hook handles its WINDOW argument and foreign current buffer.
Regression: buffer-local `window-size-change-functions' members get
the displaying WINDOW, with an arbitrary buffer current."
  (let ((text "resize hook 检查 aaa bbb ccc ddd eee fff"))
    (ekp-region-test--with-mode text 200
      (let ((buf (current-buffer))
            (win (selected-window)))
        (set-window-buffer win buf)
        (cl-letf (((symbol-function 'ekp-region--window-pixel)
                   (lambda (&optional _) 120)))
          ;; simulate redisplay: window argument, unrelated buffer current
          (with-temp-buffer
            (ekp-region--on-resize win)))
        (with-current-buffer buf
          (should (timerp ekp-region--resize-timer))
          (cancel-timer ekp-region--resize-timer)
          ;; run what the timer would have run
          (ekp-region--reflow buf 120)
          (should (= ekp-region--auto-width 120)))))))

(ert-deftest ekp-region-test-mode-reflow-width ()
  "Reflow to a new width matches a fresh justification at that width."
  (let ((text "reflow 检查 aaa bbb ccc ddd eee fff ggg hhh")
        fresh)
    (ekp-region-test--with-text text
      (ekp-justify-region (point-min) (point-max) 90)
      (setq fresh (buffer-substring (point-min) (point-max))))
    (ekp-region-test--with-mode text 200
      (ekp-region--reflow (current-buffer) 90)
      (should (= ekp-region--auto-width 90))
      (let ((got (buffer-substring (point-min) (point-max))))
        ;; ekp-justified was written at two widths; ignore that prop
        (remove-text-properties 0 (length got) '(ekp-justified nil) got)
        (remove-text-properties 0 (length fresh) '(ekp-justified nil) fresh)
        (should (equal-including-properties got fresh))))))

(provide 'ekp-region-tests)

;;; ekp-region-tests.el ends here
