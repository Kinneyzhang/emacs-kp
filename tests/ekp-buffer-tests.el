;;; ekp-buffer-tests.el --- Public API acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Test supported package inputs, outputs, errors and lifecycle.
;; Internal implementation details are outside this contract.

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'ekp)

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

(provide 'ekp-buffer-tests)
;;; ekp-buffer-tests.el ends here
