;;; ekp-tests.el --- ERT tests for EKP -*- lexical-binding: t; -*-

;;; Commentary:

;; Automated test suite for emacs-kp.  All tests are batch-safe:
;;
;;   emacs -Q --batch -L . -l tests/ekp-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; In batch mode text is measured in character columns (1px per
;; column, 2px per CJK char), which exercises the full pipeline
;; deterministically without a window system.
;;
;; C module tests are skipped automatically when ekp_c/ekp.dylib (or
;; .so/.dll) has not been built.
;;
;; Interactive demos live in tests/ekp-demo.el; benchmarks in
;; tests/ekp-bench.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ekp)

;;;; Fixtures

(defconst ekp-tests--isolated-variables
  '(ekp-latin-lang
    ekp-use-c-module
    ekp-lws-ideal-pixel
    ekp-lws-stretch-pixel
    ekp-lws-shrink-pixel
    ekp-mws-ideal-pixel
    ekp-mws-stretch-pixel
    ekp-mws-shrink-pixel
    ekp-cws-ideal-pixel
    ekp-cws-stretch-pixel
    ekp-cws-shrink-pixel
    ekp-lws-max-pixel
    ekp-lws-min-pixel
    ekp-mws-max-pixel
    ekp-mws-min-pixel
    ekp-cws-max-pixel
    ekp-cws-min-pixel
    ekp-default-cws-stretch-pixel
    ekp-line-penalty
    ekp-hyphen-penalty
    ekp-adjacent-fitness-penalty
    ekp-consecutive-hyphen-penalty
    ekp-last-line-short-penalty
    ekp-last-line-min-ratio
    ekp-emergency-stretch-pixel
    ekp-alignment
    ekp-ragged-stretch-pixel
    ekp-protrusion
    ekp-protrusion-ratios
    ekp-parshape
    ekp-first-line-indent
    ekp-looseness
    ekp-para-cache-limit
    ekp-cjk-no-line-start-extra
    ekp-cjk-no-line-end-extra
    ekp-inline-code-policy
    ekp-hyphenation
    ekp-token-break-policies
    ekp-number-unit-suffixes
    ekp-kinsoku-profile
    ekp-overlong-token-policy
    ekp--params-explicit)
  "Dynamically scoped EKP state restored by the clean-state fixture.")

(defvar ekp-cjk-no-line-end-extra nil)
(defvar ekp-inline-code-policy nil)
(defvar ekp-hyphenation nil)
(defvar ekp-token-break-policies nil)
(defvar ekp-number-unit-suffixes nil)
(defvar ekp-kinsoku-profile nil)
(defvar ekp-overlong-token-policy nil)
(defvar ekp-emergency-stretch-pixel nil)

(defmacro ekp-tests--with-clean-state (&rest body)
  "Run BODY with fresh caches and restore all tunables afterwards."
  `(cl-progv ekp-tests--isolated-variables
       (mapcar #'symbol-value ekp-tests--isolated-variables)
     (unwind-protect
         (progn
           (ekp-clear-caches)
           (ekp-param-reset)
           ,@body)
       (ekp-clear-caches))))

(defun ekp-tests--line-widths (out)
  "Rendered pixel width of each line of OUT."
  (mapcar #'string-pixel-width (split-string out "\n")))

(defun ekp-tests--content (s)
  "S without whitespace, newlines, hyphens and zero-width spaces.
Used to verify no content is lost by justification."
  (replace-regexp-in-string "[ \t\n​-]+" "" (substring-no-properties s)))

(defvar ekp-tests--c-tried nil)
(defun ekp-tests--c-available ()
  "Load the C module once; return non-nil when usable."
  (unless ekp-tests--c-tried
    (setq ekp-tests--c-tried t)
    (ignore-errors (ekp-c-module-load)))
  (and (boundp 'ekp-c-module-loaded) ekp-c-module-loaded))

(defun ekp-tests--assert-dp-cache-parameter-isolated
    (variable before after width text)
  "Assert VARIABLE changing from BEFORE to AFTER invalidates cached DP data."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (baseline (progn (set variable before)
                           (ekp-dp-cache text width)))
          (cached (progn (set variable after)
                         (ekp-dp-cache text width)))
          (fresh (progn (ekp-clear-caches)
                        (ekp-dp-cache text width))))
     (should-not (equal (cons variable baseline)
                        (cons variable fresh)))
     (should (equal (cons variable cached)
                    (cons variable fresh))))))

(defun ekp-tests--file (name)
  (expand-file-name name (expand-file-name "tests" (ekp-root-dir))))

(defun ekp-tests--file-content (name)
  (with-temp-buffer
    (insert-file-contents (ekp-tests--file name))
    (buffer-string)))

(defun ekp-tests--rendered-lines (string width)
  "Return rendered STRING lines at WIDTH without text properties."
  (mapcar #'substring-no-properties
          (split-string (ekp-pixel-justify string width) "\n")))

(defun ekp-tests--layout-source-lines (string width)
  "Return source substrings selected by `ekp-layout-plan' at WIDTH."
  (let ((plan (ekp-layout-plan string width)))
    (mapcar (lambda (line)
              (substring-no-properties
               string
               (ekp-layout-line-source-start line)
               (ekp-layout-line-source-end line)))
            (append (ekp-layout-plan-lines plan) nil))))

(defun ekp-tests--assert-complete-plan (plan)
  "Assert that planned PLAN contains lines spanning its source."
  (let ((lines (append (ekp-layout-plan-lines plan) nil))
        (source (ekp-layout-plan-string plan)))
    (should (eq (ekp-layout-plan-state plan) 'planned))
    (should lines)
    (should (= (ekp-layout-line-source-start (car lines)) 0))
    (should (= (ekp-layout-line-source-end (car (last lines)))
               (length source)))))

(defun ekp-tests--layout-hyphen-lines (string width)
  "Return source lines whose rendered break uses a discretionary hyphen."
  (let ((plan (ekp-layout-plan string width)))
    (seq-filter #'ekp-layout-line-hyphen-p
                (append (ekp-layout-plan-lines plan) nil))))

(defun ekp-tests--line-start-chars (string width)
  "Return first chars of rendered non-empty STRING lines at WIDTH."
  (delq nil
        (mapcar (lambda (line)
                  (and (> (length line) 0) (aref line 0)))
                (ekp-tests--rendered-lines string width))))

(defun ekp-tests--plan-state (plan)
  "Return PLAN state, failing clearly until the public slot exists."
  (if (fboundp 'ekp-layout-plan-state)
      (ekp-layout-plan-state plan)
    (ert-fail "missing ekp-layout-plan-state accessor")))

(defun ekp-tests--plan-reason (plan)
  "Return PLAN reason, failing clearly until the public slot exists."
  (if (fboundp 'ekp-layout-plan-reason)
      (ekp-layout-plan-reason plan)
    (ert-fail "missing ekp-layout-plan-reason accessor")))

(defun ekp-tests--assert-no-private-policy-properties (string)
  "Assert STRING has no implementation-private policy properties."
  (let ((private '(ekp--break-policy ekp--hyphenation ekp--literal-spacing
                   ekp--policy-provenance ekp--automatic-no-break
                   ekp--resolved-policy ekp--no-hyphen
                   ekp--token-category ekp--downgraded-no-break
                   ekp--face-break-policy)))
    (dotimes (i (length string))
      (dolist (prop private)
        (should-not (get-text-property i prop string))))))

(defun ekp-tests--text-property-not-any (start end prop string)
  "Return non-nil when PROP is nil on every character in STRING interval."
  (eq (text-property-not-all start end prop nil string) nil))

(defun ekp-tests--policy-interval-signatures (intervals)
  "Return compact structural signatures for policy INTERVALS."
  (mapcar (lambda (interval)
            (list (ekp--policy-interval-start interval)
                  (ekp--policy-interval-end interval)
                  (ekp--policy-interval-break-policy interval)
                  (ekp--policy-interval-provenance interval)
                  (ekp--policy-interval-category interval)))
          intervals))

(defun ekp-tests--strings-in-tree (tree)
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

(defun ekp-tests--line-source-strings (plan)
  "Return source substrings selected by PLAN."
  (let ((source (ekp-layout-plan-string plan)))
    (mapcar
     (lambda (line)
       (substring-no-properties
        source
        (ekp-layout-line-source-start line)
        (ekp-layout-line-source-end line)))
     (append (ekp-layout-plan-lines plan) nil))))

(defun ekp-tests--single-cjk-source-line-p (line)
  "Return non-nil when LINE is exactly one CJK source character."
  (let ((trimmed (string-trim (substring-no-properties line))))
    (and (= (length trimmed) 1)
         (let ((char (aref trimmed 0)))
           (and (<= #x4E00 char) (<= char #x9FFF))))))

(defun ekp-tests--isolated-cjk-lines-in-source-lines (lines target)
  "Return single-CJK LINES whose char occurs in TARGET."
  (seq-filter
   (lambda (line)
     (let ((trimmed (string-trim (substring-no-properties line))))
       (and (ekp-tests--single-cjk-source-line-p line)
            (string-match-p (regexp-quote trimmed) target))))
   lines))

;;;; Hyphenation (Liang's algorithm)

(ert-deftest ekp-test-hyphen-en ()
  (let ((h (ekp-hyphen-create "en_US")))
    (should (equal (ekp-hyphen-boxes h "hyphenation")
                   '("hy" "phen" "ation")))
    ;; RIGHTHYPHENMIN 3 (declared by en_US): no "gen-cy" break,
    ;; "cy" would leave only 2 characters after the hyphen.
    (should (equal (ekp-hyphen-boxes h "emergency")
                   '("emer" "gency")))
    ;; Words with no break points come back whole
    (should (equal (ekp-hyphen-boxes h "cat") '("cat")))))

(ert-deftest ekp-test-hyphen-de-iso8859-dict ()
  "German dictionary is ISO-8859 encoded; umlauts must decode correctly."
  (let ((h (ekp-hyphen-create "de_DE")))
    (should (equal (ekp-hyphen-boxes h "ästhetisch")
                   '("äs" "the" "tisch")))
    (should (equal (ekp-hyphen-boxes h "Universität")
                   '("Uni" "ver" "si" "tät")))))

(ert-deftest ekp-test-hyphen-margins ()
  "Breaks respect the left/right margins (min 2 chars each side)."
  (let ((h (ekp-hyphen-create "en_US")))
    (dolist (pos (ekp-hyphen-positions h "hyphenation"))
      (should (>= pos 2))
      (should (<= pos (- (length "hyphenation") 2))))))

(ert-deftest ekp-test-hyphen-lang-fallback ()
  "Short language codes resolve to a dictionary."
  (should (ekp-hyphen-create "en"))
  (dolist (locale '("zz_XX" "zz-XX"))
    (should-error (ekp-hyphen-create locale)
                  :type 'ekp-hyphen-dictionary-not-found)))

(ert-deftest ekp-test-hyphen-normalized-locale-prefers-exact-dictionary ()
  "Equivalent locale spellings resolve before the short-code fallback."
  (dolist (locale '("de_CH" "de-CH" "de_ch" "DE-CH"))
    (should (equal (file-name-nondirectory
                    (ekp-hyphen--resolve-lang locale))
                   "hyph_de_CH.dic"))))

(ert-deftest ekp-test-hyphen-alternative-languages-fail-closed ()
  "Replacement-pattern dictionaries must not degrade to plain Liang breaks.
The golden forms are libhyphen outputs; EKP cannot safely emit them until its
DP represents break-specific replacement widths."
  (dolist (case '(("hu_HU" "asszony" "asz=szony")
                  ("ca" "paral·lel" "pa=ral=lel")
                  ("sq_AL" "adhem" "e")))
    (let ((lang (nth 0 case))
          (word (nth 1 case))
          (golden (nth 2 case)))
      (condition-case err
          (progn
            (ekp-hyphen-create lang)
            (ert-fail (format "%s incorrectly accepted; golden %s -> %s"
                              lang word golden)))
        (ekp-hyphen-unsupported-pattern
         (should (equal (cadr err) lang))
         (should (> (nth 3 err) 0))))))
  ;; Esperanto also contains slash-prefixed patterns whose libhyphen
  ;; meaning is not representable as an ordinary Liang pattern.
  (should-error (ekp-hyphen-create "eo")
                :type 'ekp-hyphen-unsupported-pattern)
  (should-error (ekp-hyphen-create "hu-HU")
                :type 'ekp-hyphen-unsupported-pattern))

(ert-deftest ekp-test-hyphen-alternative-error-reaches-public-dispatch ()
  "The public formatter must surface unsupported replacement dictionaries."
  (ekp-tests--with-clean-state
   (let ((ekp-latin-lang "hu_HU"))
     (should-error
      (ekp-pixel-justify "asszony asszony asszony" 12)
      :type 'ekp-hyphen-unsupported-pattern))))

(ert-deftest ekp-test-hyphen-nil-result-is-cached ()
  "A word with no break positions must compute only once."
  (let ((h (ekp-hyphen--create
            :patterns (make-hash-table :test 'equal)
            :cache (make-hash-table :test 'equal)
            :maxlen 0 :left 2 :right 2))
        (calls 0))
    (cl-letf (((symbol-function 'ekp-hyphen--compute)
               (lambda (_h _word)
                 (cl-incf calls)
                 nil)))
      (should-not (ekp-hyphen--positions h "qzxq"))
      (should-not (ekp-hyphen--positions h "QZXQ"))
      (should (= calls 1)))))

(ert-deftest ekp-test-hyphen-inserted-dense-breaks ()
  "Dense insertion must slice the original word without changing properties."
  (dolist (case '(("abcdefghij" (2 5 8) "--" "ab--cde--fgh--ij")
                  ("hyphenation" (2 6) "-" "hy-phen-ation")
                  ("abcdef" nil "*" "abcdef")))
    (pcase-let ((`(,word ,positions ,hyphen ,expected) case))
      (setq word (copy-sequence word))
      (put-text-property 1 (1- (length word)) 'face 'italic word)
      (cl-letf (((symbol-function 'ekp-hyphen-positions)
                 (lambda (_h _word) positions)))
        (let ((actual (ekp-hyphen-inserted nil word hyphen)))
          (should (equal (substring-no-properties actual) expected))
          (should (eq (get-text-property 1 'face actual) 'italic)))))))

(ert-deftest ekp-test-tokenizer-long-attached-run-preserves-properties ()
  "A long token and attached combining run must remain one exact box."
  (let* ((base (propertize (make-string 4096 ?a) 'face 'bold))
         (marks (make-string 2048 #x0301))
         (word (concat base marks))
         (boxes (ekp-split-to-boxes word)))
    (should (= (length boxes) 1))
    (should (equal-including-properties (aref boxes 0) word))))

;;;; Box splitting

(ert-deftest ekp-test-split-latin-words ()
  (should (equal (append (ekp-split-to-boxes "hello world") nil)
                 '("hello" "world"))))

(ert-deftest ekp-test-split-cjk-chars ()
  (should (equal (append (ekp-split-to-boxes "中文排版") nil)
                 '("中" "文" "排" "版"))))

(ert-deftest ekp-test-split-cjk-punct-own-boxes ()
  "CJK punctuation is its own box; kinsoku lives in break permissions."
  (should (equal (append (ekp-split-to-boxes "中文，排版。") nil)
                 '("中" "文" "，" "排" "版" "。")))
  (should (equal (append (ekp-split-to-boxes "看《中文》吧") nil)
                 '("看" "《" "中" "文" "》" "吧"))))

(ert-deftest ekp-test-breaks-allowed-kinsoku ()
  "Break permissions forbid line-initial closers and line-final openers."
  (let* ((para (ekp--get-para "看《中文》吧，好。」的"))
         (boxes (append (ekp-para-boxes para) nil))
         (ok (ekp-para-breaks-allowed para)))
    ;; boxes: 看 《 中 文 》 吧 , 好 。 」 的
    (should (equal boxes '("看" "《" "中" "文" "》" "吧" "，"
                           "好" "。" "」" "的")))
    ;; forbidden: after 《 (idx 2), before 》 (idx 4), before , (6),
    ;; before 。 (8), before 」 (9)
    (dolist (k '(2 4 6 8 9))
      (should-not (aref ok k)))
    ;; allowed elsewhere, e.g. 看|《, 》|吧, ,|好, 」|的
    (dolist (k '(1 5 7 10))
      (should (aref ok k)))))

(defun ekp-test--line-glue-widths (line)
  "Pixel widths of synthesized glue spaces in LINE, in order."
  (let (ws)
    (dotimes (i (length line))
      (let ((d (get-text-property i 'display line)))
        (when (and (consp d) (eq (car d) 'space)
                   (get-text-property i 'ekp-glue line))
          (push (car (plist-get (cdr d) :width)) ws))))
    (nreverse ws)))

(ert-deftest ekp-test-alignment-ragged-right ()
  "Ragged-right: lines fit, interior spacing stays at ideal."
  (let ((ekp-alignment 'ragged-right)
        (text "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll"))
    (let ((lines (split-string (ekp-pixel-justify text 12) "\n")))
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (string-pixel-width line) 12))
        ;; all interior glues at ideal (1px word space in batch);
        ;; only the trailing filler may be wider
        (let ((interior (butlast (ekp-test--line-glue-widths line))))
          (dolist (w interior) (should (<= w 1))))))))

(ert-deftest ekp-test-alignment-ragged-left ()
  "Ragged-left: every line is pushed flush to the right edge."
  (let ((ekp-alignment 'ragged-left)
        (text "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll"))
    (dolist (line (split-string (ekp-pixel-justify text 12) "\n"))
      (should (= (string-pixel-width line) 12)))))

(ert-deftest ekp-test-alignment-center ()
  "Center: leftover splits evenly between the two edges."
  (let ((ekp-alignment 'center)
        (text "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll"))
    (dolist (line (split-string (ekp-pixel-justify text 12) "\n"))
      (should (= (string-pixel-width line) 12))
      (let* ((len (length line))
             (lead (if (and (> len 0)
                            (get-text-property 0 'ekp-glue line))
                       (or (car (ekp-test--line-glue-widths line)) 0)
                     0))
             (trail (if (and (> len 0)
                             (get-text-property (1- len) 'ekp-glue line))
                        (or (car (last (ekp-test--line-glue-widths line))) 0)
                      0)))
        (should (<= (abs (- lead trail)) 1))))))

(ert-deftest ekp-test-alignment-c-parity ()
  "C and elisp engines agree under every alignment mode."
  (skip-unless (ekp-tests--c-available))
  (dolist (align '(justify ragged-right ragged-left center))
    (let ((ekp-alignment align)
          (text "对齐 parity 检查内容 mixed 中英文字 several words here too"))
      (let ((a (let ((ekp-use-c-module nil))
                 (ekp-clear-caches)
                 (ekp-pixel-justify text 60)))
            (b (let ((ekp-use-c-module t))
                 (ekp-clear-caches)
                 (ekp-pixel-justify text 60))))
        (should (string= a b))))))

(ert-deftest ekp-test-first-line-indent ()
  "First line carries an indent spacer; all lines fill the measure."
  (let ((ekp-first-line-indent 6)
        (text "缩进段落的内容足够长可以断成好几行来验证首行缩进的正确表现"))
    (let ((lines (split-string (ekp-pixel-justify text 30) "\n")))
      (should (> (length lines) 2))
      (should (get-text-property 0 'ekp-glue (car lines)))
      (should-not (get-text-property 0 'ekp-glue (cadr lines)))
      (dolist (l (butlast lines))
        (should (= (string-pixel-width l) 30))))))

(ert-deftest ekp-test-parshape ()
  "Per-line (INDENT . WIDTH) specs shape the paragraph."
  (let ((ekp-parshape '((0 . 20) (6 . 24) (0 . 30)))
        (text "参差形状段落内容也要足够长以便验证每一行宽度设置都生效呢"))
    (let ((lines (split-string (ekp-pixel-justify text 30) "\n")))
      (should (>= (length lines) 3))
      (should (= (string-pixel-width (nth 0 lines)) 20))
      (should (= (string-pixel-width (nth 1 lines)) 30))
      (dolist (l (butlast (cddr lines)))
        (should (= (string-pixel-width l) 30))))))

(ert-deftest ekp-test-parshape-bypasses-c ()
  "Per-line widths force the public dispatcher down the Elisp 2D path."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module t)
         (ekp-c-module-loaded t)
         (ekp-parshape '((0 . 20) (6 . 24)))
         c-called)
     (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                (lambda (&rest _)
                  (setq c-called t)
                  (error "C path must not run for parshape"))))
       (should (stringp
                (ekp-pixel-justify
                 "参差形状必须经过公开分派路径而不是只测内部谓词" 30)))
       (should-not c-called)))))

(ert-deftest ekp-test-clean-state-restores-config ()
  "The shared fixture must not leak configuration into later tests."
  (let ((ekp-use-c-module t)
        (ekp-alignment 'justify)
        (ekp-protrusion nil))
    (ekp-tests--with-clean-state
     (setq ekp-use-c-module nil
           ekp-alignment 'center
           ekp-protrusion t))
    (should ekp-use-c-module)
    (should (eq ekp-alignment 'justify))
    (should-not ekp-protrusion)))

(ert-deftest ekp-test-protrusion-hangs-line-end-punct ()
  "Protrusion lets line-final fullwidth punctuation hang past the edge."
  (let ((ekp-protrusion t)
        ;; periodic 5-char sentences: at 20px exactly two per line, so
        ;; every interior break lands right after 。
        (text (mapconcat #'identity (make-list 6 "四字一句。") "")))
    (let* ((lines (split-string (ekp-pixel-justify text 20) "\n")))
      (should (> (length lines) 1))
      ;; at least one non-last line hangs its punctuation
      (should (seq-some (lambda (l) (> (string-pixel-width l) 20))
                        (butlast lines)))
      ;; and never beyond the configured ratio (0.5 × 2px in batch)
      (dolist (l (butlast lines))
        (should (<= (string-pixel-width l) 21))))))

(ert-deftest ekp-test-protrusion-off-is-flush ()
  "With protrusion off (default), no line exceeds the target width."
  (let ((text "第一句话结束。第二句话继续写下去,内容足够长才会断行成很多行。"))
    (dolist (l (butlast (split-string (ekp-pixel-justify text 20) "\n")))
      (should (<= (string-pixel-width l) 20)))))

(ert-deftest ekp-test-protrusion-c-parity ()
  "C and elisp agree with protrusion enabled."
  (skip-unless (ekp-tests--c-available))
  (let ((ekp-protrusion t)
        (text "悬挂 parity 检查。标点很多,逗号,句号。分号;更多内容写在这里。"))
    (dolist (w '(24 40 60))
      (let ((a (let ((ekp-use-c-module nil))
                 (ekp-clear-caches)
                 (ekp-pixel-justify text w)))
            (b (let ((ekp-use-c-module t))
                 (ekp-clear-caches)
                 (ekp-pixel-justify text w))))
        (should (string= a b))))))

(ert-deftest ekp-test-no-break-span-atomic ()
  "An ekp-no-break span never splits, stretches, or hyphenates."
  (let* ((code (propertize "foo bar baz" 'ekp-no-break t))
         (text (concat "prefix words before " code " and after more words")))
    (dolist (w '(40 80 120 200))
      (let ((out (ekp-pixel-justify text w)))
        ;; contiguous, with literal single spaces — on one line
        (should (string-match-p "foo bar baz" out))))))

(ert-deftest ekp-test-no-break-overlong-atom ()
  "An atom wider than the line becomes a single emergency line."
  (let ((atom (propertize "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbb"
                          'ekp-no-break t)))
    (let ((out (ekp-pixel-justify (concat "x " atom " y") 30)))
      (should (string-match-p "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbb" out)))))

(ert-deftest ekp-test-no-break-overlong-atom-preserves-reachability ()
  "An overlong atom stays intact without making the plan unreachable."
  (dolist (case '(("行内原子演示:代码片段 " 40)
                  ("甲乙丙丁戊己庚辛 " 35)
                  ("短前缀 " 28)))
    (let* ((prefix (car case))
           (width (cadr case))
           (atom (propertize
                  (concat (make-string (+ width 8) ?a) " b")
                  'ekp-no-break t))
           (text (concat prefix atom " 后文继续。"))
           (atom-start (length prefix))
           (atom-end (+ atom-start (length atom))))
      (dolist (use-c (if (ekp-tests--c-available) '(nil t) '(nil)))
        (ekp-tests--with-clean-state
         (let* ((ekp-use-c-module use-c)
                (plan (ekp-layout-plan text width))
                (lines (ekp-layout-plan-lines plan))
                (atom-line
                 (seq-find
                  (lambda (line)
                    (and (<= (ekp-layout-line-source-start line) atom-start)
                         (>= (ekp-layout-line-source-end line) atom-end)))
                  (append lines nil)))
                (rendered (ekp-pixel-justify text width)))
           (ekp-tests--assert-complete-plan plan)
           (should atom-line)
           (should (string-match-p
                    (regexp-quote (substring-no-properties atom))
                    (substring-no-properties rendered)))
           (should (equal (ekp-tests--content rendered)
                          (ekp-tests--content text)))))))))

(ert-deftest ekp-test-no-break-overlong-atom-avoids-isolated-cjk-lines ()
  "An overlong no-break atom must not strand one CJK source char."
  (let* ((atom (propertize
                (concat "原子演示: 代码片段 " (make-string 20 ?a))
                'ekp-no-break t))
         (atom-text (substring-no-properties atom))
         (text (concat
                "行内" atom
                " 永不折散、空格保持字面宽度；不间断空格让 "
                "100_000 与 3.14_MB 这类数字单位锁在同一行。"))
         (width 41))
    (cl-labels
        ((run (use-c)
           (ekp-tests--with-clean-state
            (let* ((ekp-use-c-module use-c)
                   (plan (ekp-layout-plan text width))
                   (rendered (substring-no-properties
                              (ekp-render-layout-string plan)))
                   (lines (append (ekp-layout-plan-lines plan) nil))
                   (source-lines
                    (mapcar
                     (lambda (line)
                       (substring-no-properties
                        text
                        (ekp-layout-line-source-start line)
                        (ekp-layout-line-source-end line)))
                     lines))
                   (single-cjk-lines
                    (seq-filter
                     (lambda (line)
                       (and (= (length line) 1)
                            (> (aref line 0) 127)))
                     source-lines)))
              (ekp-tests--assert-complete-plan plan)
              (list :rendered rendered
                    :signatures (mapcar #'ekp-layout-line-signature lines)
                    :single-cjk-lines single-cjk-lines)))))
      (let* ((elisp (run nil))
             (c-available (ekp-tests--c-available))
             (c-result (and c-available (run t))))
        (dolist (result (delq nil (list elisp c-result)))
          (should (string-match-p (regexp-quote atom-text)
                                  (plist-get result :rendered)))
          (should-not (plist-get result :single-cjk-lines)))
        (when c-result
          (should (equal (plist-get c-result :signatures)
                         (plist-get elisp :signatures)))
          (should (equal (plist-get c-result :rendered)
                         (plist-get elisp :rendered))))))))

(ert-deftest ekp-test-core-1d-avoids-avoidable-single-cjk-lines ()
  "The 168px 1D DP keeps screenshot mixed source from orphaning CJK."
  (let* ((inline "(ekp-pixel-justify paragraph-text target-width 'justify nil)")
         (target "最常见的场景")
         (text (concat
                "中英混排是 Emacs 里" target
                ": The quick brown fox jumps over the lazy dog, 而 "
                "internationalization 这样的长词在窄栏会按 Liang 模式断词。"
                "自动行内代码 " inline
                " 可以在合法空白边界附近换行,但不会插入 "
                "discretionary hyphen。")))
    (cl-labels
        ((run (use-c)
           (ekp-tests--with-clean-state
            (let* ((ekp-use-c-module use-c)
                   (ekp-hyphenation 'on)
                   (plan (ekp-layout-plan text 168))
                   (source-lines (ekp-tests--line-source-strings plan))
                   (two-cjk-with-glue (string-pixel-width "场景")))
              (ekp-tests--assert-complete-plan plan)
              (should (<= two-cjk-with-glue 168))
              (list :rendered (substring-no-properties
                               (ekp-render-layout-string plan))
                    :signatures
                    (mapcar #'ekp-layout-line-signature
                            (append (ekp-layout-plan-lines plan) nil))
                    :single-cjk-lines
                    (ekp-tests--isolated-cjk-lines-in-source-lines
                     source-lines target))))))
      (let* ((elisp (run nil))
             (c-result (and (ekp-tests--c-available) (run t))))
        (when c-result
          (should (equal (plist-get c-result :signatures)
                         (plist-get elisp :signatures)))
          (should (equal (plist-get c-result :rendered)
                         (plist-get elisp :rendered))))
        (should-not (plist-get elisp :single-cjk-lines))
        (when c-result
          (should-not (plist-get c-result :single-cjk-lines)))))))

(ert-deftest ekp-test-core-parshape-loose-avoids-single-cjk-lines ()
  "Parshape plus looseness preserves the screenshot mixed source oracle."
  (let* ((inline "(ekp-pixel-justify paragraph-text target-width 'justify nil)")
         (target "最常见的场景")
         (text (concat
                "中英混排是 Emacs 里" target
                ": The quick brown fox jumps over the lazy dog, 而 "
                "internationalization 这样的长词在窄栏会按 Liang 模式断词。"
                "自动行内代码 " inline
                " 可以在合法空白边界附近换行,但不会插入 "
                "discretionary hyphen。"))
         (ekp-use-c-module nil)
         (ekp-hyphenation 'on)
         (ekp-looseness 1)
         (ekp-parshape '((0 . 280) (40 . 200) (80 . 120)
                         (40 . 200) (0 . 280))))
    (ekp-tests--with-clean-state
     (let* ((plan (ekp-layout-plan text 280))
            (source-lines (ekp-tests--line-source-strings plan))
            (two-cjk-with-glue (string-pixel-width "场景")))
       (ekp-tests--assert-complete-plan plan)
       (should (<= two-cjk-with-glue 120))
       (should-not
        (ekp-tests--isolated-cjk-lines-in-source-lines
         source-lines target))))))

(ert-deftest ekp-test-no-break-suppresses-hyphenation ()
  "No soft hyphen appears inside a no-break span."
  (let ((word "internationalization"))
    ;; sanity: unmarked long word does hyphenate at narrow width
    (should (string-match-p "-\n" (ekp-pixel-justify
                                   (concat "pad " word " pad") 15)))
    (should-not (string-match-p "-\n"
                                (ekp-pixel-justify
                                 (concat "pad "
                                         (propertize word 'ekp-no-break t)
                                         " pad")
                                 15)))))

(ert-deftest ekp-test-nbsp-and-word-joiner ()
  "NBSP and WORD JOINER keep their neighbors on the same line."
  (let ((nbsp (string #x00A0)) (wj (string #x2060)))
    (dolist (w '(20 30 44 60 90))
      (let ((out (ekp-pixel-justify
                  (concat "甲乙丙丁" nbsp "戊己庚辛写更多字") w)))
        (should (string-match-p (concat "丁" nbsp "戊") out)))
      (let ((out (ekp-pixel-justify
                  (concat "甲乙丙" wj "丁戊己庚辛写更多字") w)))
        (should (string-match-p (concat "丙" wj "丁") out))))))

(ert-deftest ekp-test-kinsoku-rendered-output ()
  "No rendered line starts with a closer or ends with an opener."
  (let ((text "他说:「今天天气很好。」然后就离开了这里,再也没有回来过。"))
    (dolist (w (number-sequence 30 200 7))
      (dolist (line (split-string (ekp-pixel-justify text w) "\n"))
        (should-not (string-match-p "\\`[。、,;:」』)》!?]" line))
        (should-not (string-match-p "[「『(《]\\'" line))))))

;;;; Configurable break policy contracts

(ert-deftest ekp-test-break-policy-public-defaults ()
  "The selected policy variables expose the approved public defaults."
  (should (eq ekp-inline-code-policy 'no-hyphen))
  (should (eq ekp-hyphenation 'auto))
  (should (eq ekp-kinsoku-profile 'common))
  (should (eq ekp-overlong-token-policy 'emergency))
  (should (equal ekp-token-break-policies
                 '((url . no-hyphen)
                   (path . no-hyphen)
                   (identifier . no-hyphen)
                   (number-unit . no-break))))
  (dolist (unit '("px" "MB" "°C" "μm"))
    (should (member unit ekp-number-unit-suffixes))))

(ert-deftest ekp-test-break-policy-region-hyphenation-public-path ()
  "Region `ekp-break-policy' controls hyphenation without becoming rigid."
  (ekp-tests--with-clean-state
   (let* ((word "internationalization")
          (width 12)
          (hyphenated (copy-sequence word))
          (normal (copy-sequence word))
          (blocked (copy-sequence word)))
     (put-text-property 0 (length word) 'ekp-break-policy 'hyphenate
                        hyphenated)
     (put-text-property 0 (length word) 'ekp-break-policy 'normal normal)
     (put-text-property 0 (length word) 'ekp-break-policy 'no-hyphen blocked)
     (let ((ekp-hyphenation 'off))
       (should (ekp-tests--layout-hyphen-lines hyphenated width))
       (should-not (ekp-tests--layout-hyphen-lines normal width)))
     (let ((ekp-hyphenation 'on))
       (should-not (ekp-tests--layout-hyphen-lines blocked width))
       (should (member word
                       (ekp-tests--layout-source-lines blocked width)))))))

(ert-deftest ekp-test-break-policy-normal-clears-automatic-token-restrictions ()
  "Region `normal' overrides automatic token no-hyphen/no-break restrictions."
  (ekp-tests--with-clean-state
   (dolist (case '(("processInternationalization42" identifier no-hyphen)
                   ("100000MB" number-unit no-break)))
     (pcase-let ((`(,token ,category ,policy) case))
       (let ((text (copy-sequence token))
             (ekp-token-break-policies (list (cons category policy))))
         (put-text-property 0 (length text) 'ekp-break-policy 'normal text)
         (let ((analysis (car (ekp--analyze-policies text))))
           (should (ekp-tests--text-property-not-any
                    0 (length text) 'ekp--no-hyphen analysis))
           (should (ekp-tests--text-property-not-any
                    0 (length text) 'ekp--automatic-no-break analysis))
           (should-not (get-text-property 0 'ekp--hyphenation analysis))
           (should (eq (get-text-property 0 'ekp--policy-provenance analysis)
                       'region))))))))

(ert-deftest ekp-test-break-policy-hyphenate-clears-automatic-token-restrictions ()
  "Region `hyphenate' enables hyphenation and clears automatic restrictions."
  (ekp-tests--with-clean-state
   (dolist (case '(("processInternationalization42" identifier no-hyphen)
                   ("100000MB" number-unit no-break)))
     (pcase-let ((`(,token ,category ,policy) case))
       (let ((text (copy-sequence token))
             (ekp-token-break-policies (list (cons category policy))))
         (put-text-property 0 (length text) 'ekp-break-policy 'hyphenate text)
         (let ((analysis (car (ekp--analyze-policies text))))
           (should (ekp-tests--text-property-not-any
                    0 (length text) 'ekp--no-hyphen analysis))
           (should (ekp-tests--text-property-not-any
                    0 (length text) 'ekp--automatic-no-break analysis))
           (should (eq (get-text-property 0 'ekp--hyphenation analysis) 'on))
           (should (eq (get-text-property 0 'ekp--policy-provenance analysis)
                       'region))))))))

(ert-deftest ekp-test-break-policy-no-hyphen-keeps-ordinary-word-atomic ()
  "Region `no-hyphen' suppresses hyphenation without inventing word breaks."
  (ekp-tests--with-clean-state
   (let* ((word (copy-sequence "internationalization"))
          (width 12))
     (put-text-property 0 (length word) 'ekp-break-policy 'no-hyphen word)
     (should-not (ekp-tests--layout-hyphen-lines word width))
     (should (member (substring-no-properties word)
                     (ekp-tests--layout-source-lines word width))))))

(ert-deftest ekp-test-break-policy-region-suppresses-token-only-in-subrange ()
  "A middle region override must leave outer automatic token policy intact."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "processInternationalization42"))
          (start 7)
          (end 27)
          (ekp-token-break-policies '((identifier . no-hyphen))))
     (put-text-property start end 'ekp-break-policy 'normal text)
     (pcase-let ((`(,analysis . ,intervals) (ekp--analyze-policies text)))
       (should (ekp-tests--text-property-not-any
                start end 'ekp--no-hyphen analysis))
       (should (eq (get-text-property (1- start) 'ekp--no-hyphen analysis)
                   t))
       (should (eq (get-text-property end 'ekp--no-hyphen analysis) t))
       (should (equal (ekp-tests--policy-interval-signatures intervals)
                      `((0 ,start no-hyphen token identifier)
                        (,end ,(length text) no-hyphen token identifier)
                        (,start ,end normal region nil))))))))

(ert-deftest ekp-test-break-policy-explicit-no-break-wins ()
  "Explicit `ekp-no-break' stays rigid under every region policy value."
  (ekp-tests--with-clean-state
   (dolist (policy '(normal hyphenate no-hyphen))
     (let* ((atom (copy-sequence "internationalization atom")))
       (put-text-property 0 (length atom) 'ekp-no-break t atom)
       (put-text-property 0 (length atom) 'ekp-break-policy policy atom)
       (let* ((text (concat "x " atom " y"))
              (start 2)
              (end (+ start (length atom))))
         (let ((source-lines (ekp-tests--layout-source-lines text 12)))
           (should (seq-some
                    (lambda (line)
                      (string-match-p
                       (regexp-quote (substring-no-properties atom)) line))
                    source-lines))
           (should-not (ekp-tests--layout-hyphen-lines text 12))
           (let ((line (seq-find
                        (lambda (candidate)
                          (string-match-p
                           (regexp-quote (substring-no-properties atom))
                           candidate))
                        source-lines)))
             (should line)))
         (should (eq (get-text-property start 'ekp-no-break text) t))
         (should (eq (get-text-property start 'ekp-break-policy text)
                     policy))
         (should (= end (+ start (length atom)))))))))

(ert-deftest ekp-test-token-break-policies-positive-negative-controls ()
  "Token policies classify only bounded URL/path/identifier/number-unit atoms."
  (ekp-tests--with-clean-state
   (let ((ekp-hyphenation 'on)
         (ekp-token-break-policies
          '((url . no-hyphen) (path . no-hyphen)
            (identifier . no-hyphen) (number-unit . no-break)))
         (cases '(("url" "https://example.test/internationalization"
                   "https prose internationalization")
                  ("path" "src/core/internationalization_file.el"
                   "/ internationalization")
                  ("identifier" "processInternationalization42"
                   "ordinary internationalization")
                  ("number-unit" "3.14MB" "2026August"))))
     (dolist (case cases)
       (pcase-let ((`(,_label ,positive ,negative) case))
         (should-not
          (ekp-tests--layout-hyphen-lines
           (concat "aa " positive " zz") 14))
         (when (string-match-p "internationalization" negative)
           (should
            (ekp-tests--layout-hyphen-lines
             (concat "aa " negative " zz") 14))))))
     (let ((normal (let ((ekp-token-break-policies
                          '((identifier . normal))))
                     (ekp-pixel-justify
                      "aa processInternationalization42 zz" 14)))
           (restricted (ekp-pixel-justify
                        "aa processInternationalization42 zz" 14)))
       (should-not (equal normal restricted)))))

(ert-deftest ekp-test-automatic-no-break-downgrades-when-overwide ()
  "Automatic no-break may downgrade; explicit no-break may not."
  (ekp-tests--with-clean-state
   (let* ((token "src/internationalization/configuration/file.el")
          (text (concat "前缀 " token " 后缀"))
          (explicit-token (propertize token 'ekp-no-break t))
          (explicit (concat "前缀 " explicit-token " 后缀"))
          (width 24)
          (ekp-token-break-policies '((path . no-break)))
          (automatic-lines (ekp-tests--layout-source-lines text width))
          (explicit-lines (ekp-tests--layout-source-lines explicit width)))
     (should (seq-some (lambda (line) (string-prefix-p "前缀" line))
                       automatic-lines))
     (should-not (member token automatic-lines))
     (should (seq-some (lambda (line) (string-match-p "src/" line))
                       automatic-lines))
     (should (seq-some
              (lambda (line) (string-match-p (regexp-quote token) line))
              explicit-lines)))))

(ert-deftest ekp-test-automatic-overwide-no-break-measures-token-once ()
  "Automatic overwide no-break analysis measures the matched token once."
  (ekp-tests--with-clean-state
   (let* ((token "supercalifragilisticexpialidocious")
          (text (concat "aa " token " zz"))
          (ekp-overlong-token-policy 'overflow)
          (ekp--policy-measure 8)
          (measure-count 0)
          (measure-fn (symbol-function 'ekp--measured-width)))
     (cl-letf (((symbol-function 'ekp--measured-width)
                (lambda (string)
                  (when (string= string token)
                    (setq measure-count (1+ measure-count)))
                  (funcall measure-fn string))))
       (ekp--analyze-policies text))
     (should (= measure-count 1)))))

(ert-deftest ekp-test-token-break-policies-negative-classification-controls ()
  "Approved token negatives stay unclassified; namespace identifiers classify."
  (ekp-tests--with-clean-state
   (let ((ekp-token-break-policies
          '((url . no-hyphen) (path . no-hyphen)
            (identifier . no-hyphen) (number-unit . no-break))))
     (let (classified)
       (dolist (token '("word." "/" "\\" "well-known" "3.14"
                        "纯中文" "name@example.test" "foo:bar"))
         (let ((analysis (ekp--analyze-policies token)))
           (unless (and (eq (car analysis) token)
                        (null (cdr analysis)))
             (push (cons token
                         (ekp-tests--policy-interval-signatures
                          (cdr analysis)))
                   classified))))
       (should (equal (nreverse classified) nil)))
     (pcase-let ((`(,analysis . ,intervals)
                  (ekp--analyze-policies "ns::value")))
       (should (eq (get-text-property 0 'ekp--no-hyphen analysis) t))
       (should (equal (ekp-tests--policy-interval-signatures intervals)
                      '((0 9 no-hyphen token identifier))))))))

(ert-deftest ekp-test-face-no-break-overrides-token-no-hyphen-by-span ()
  "Face no-break overrides token no-hyphen only on the annotated span."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "processInternationalization42"))
          (start 7)
          (end (length text))
          (ekp-token-break-policies '((identifier . no-hyphen))))
     (put-text-property start end 'ekp--face-break-policy 'no-break text)
     (pcase-let ((`(,analysis . ,intervals) (ekp--analyze-policies text)))
       (should (eq (get-text-property 0 'ekp--no-hyphen analysis) t))
       (should (eq (get-text-property start 'ekp--automatic-no-break
                                      analysis)
                   t))
       (should (equal (ekp-tests--policy-interval-signatures intervals)
                      `((0 ,start no-hyphen token identifier)
                        (,start ,end no-break face identifier))))))))

(ert-deftest ekp-test-token-no-break-outranks-face-no-hyphen ()
  "Token no-break remains stricter than face no-hyphen."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "100000MB"))
          (ekp-token-break-policies '((number-unit . no-break))))
     (put-text-property 0 (length text)
                        'ekp--face-break-policy 'no-hyphen text)
     (pcase-let ((`(,analysis . ,intervals) (ekp--analyze-policies text)))
       (should (eq (get-text-property 0 'ekp--automatic-no-break analysis)
                   t))
       (should (equal (ekp-tests--policy-interval-signatures intervals)
                      '((0 8 no-break token number-unit))))))))

(ert-deftest ekp-test-region-policy-clears-face-and-token-markers-on-overlap ()
  "Explicit region policy clears automatic face/token markers in its span."
  (ekp-tests--with-clean-state
   (dolist (policy '(normal hyphenate no-hyphen))
     (let* ((text (copy-sequence "processInternationalization42"))
            (start 7)
            (end 20)
            (ekp-token-break-policies '((identifier . no-hyphen))))
       (put-text-property 0 (length text)
                          'ekp--face-break-policy 'no-break text)
       (put-text-property start end 'ekp-break-policy policy text)
       (let ((analysis (car (ekp--analyze-policies text))))
         (should (eq (get-text-property start 'ekp--policy-provenance
                                        analysis)
                     'region))
         (should (ekp-tests--text-property-not-any
                  start end 'ekp--automatic-no-break analysis))
         (should (ekp-tests--text-property-not-any
                  start end 'ekp--literal-spacing analysis))
         (pcase policy
           ('hyphenate
            (should (eq (get-text-property start 'ekp--hyphenation
                                           analysis)
                        'on)))
           ('normal
            (should (ekp-tests--text-property-not-any
                     start end 'ekp--no-hyphen analysis))
            (should-not (get-text-property start 'ekp--hyphenation
                                           analysis)))
           ('no-hyphen
            (should (eq (get-text-property start 'ekp--no-hyphen
                                           analysis)
                        t)))))))))

(ert-deftest ekp-test-face-no-break-fits-as-rigid-span ()
  "Fitting face no-break forbids breaks inside the annotated span."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "aa bb cc"))
          (ekp--policy-measure 100))
     (put-text-property 0 5 'ekp--face-break-policy 'no-break text)
     (let* ((policy-analysis (ekp--analyze-policies text))
            (para (ekp--make-para text policy-analysis))
            (boxes (append (ekp-para-boxes para) nil))
            (breaks (ekp-para-breaks-allowed para)))
       (should (equal boxes '("aa" " " "bb" "cc")))
       (should-not (aref breaks 1))
       (should-not (aref breaks 2))
       (should (aref breaks 3))))))

(ert-deftest ekp-test-overwide-face-no-break-measures-once-and-downgrades ()
  "Overwide automatic face no-break measures once and downgrades to no-hyphen."
  (ekp-tests--with-clean-state
   (let* ((token "supercalifragilisticexpialidocious")
          (text (copy-sequence token))
          (ekp--policy-measure 8)
          (measure-count 0)
          (measure-fn (symbol-function 'ekp--measured-width)))
     (put-text-property 0 (length text)
                        'ekp--face-break-policy 'no-break text)
     (cl-letf (((symbol-function 'ekp--measured-width)
                (lambda (string)
                  (when (string= string token)
                    (setq measure-count (1+ measure-count)))
                  (funcall measure-fn string))))
       (pcase-let ((`(,analysis . ,intervals) (ekp--analyze-policies text)))
         (should (= measure-count 1))
         (should (eq (get-text-property 0 'ekp--no-hyphen analysis) t))
         (should-not (get-text-property 0 'ekp--automatic-no-break analysis))
         (should (equal (ekp-tests--policy-interval-signatures intervals)
                        `((0 ,(length text) no-hyphen face nil)))))))))

(ert-deftest ekp-test-face-no-break-measures-contiguous-span-across-public-props ()
  "Face no-break width analysis ignores unrelated public property splits."
  (ekp-tests--with-clean-state
   (let* ((span "abcdef")
          (text (copy-sequence (concat span " zz")))
          (ekp--policy-measure 40)
          measured
          measured-string)
     (put-text-property 0 (length span)
                        'ekp--face-break-policy 'no-break text)
     (put-text-property 1 3 'face 'ekp-test-inline text)
     (put-text-property 3 5 'ekp-custom-property 'kept text)
     (cl-letf (((symbol-function 'ekp--string-pixel-width)
                (lambda (string)
                  (push (substring-no-properties string) measured)
                  (setq measured-string string)
                  (* 10 (length string)))))
       (pcase-let ((`(,analysis . ,intervals) (ekp--analyze-policies text)))
         (maphash
          (lambda (key _)
            (dolist (cached-string (ekp-tests--strings-in-tree key))
              (ekp-tests--assert-no-private-policy-properties
               cached-string)))
          ekp--box-width-cache)
         (should (equal (nreverse measured) (list span)))
         (should (eq (get-text-property 1 'face measured-string)
                     'ekp-test-inline))
         (should (eq (get-text-property 3 'ekp-custom-property
                                        measured-string)
                     'kept))
         (should (seq-every-p
                  (lambda (interval)
                    (eq (ekp--policy-interval-break-policy interval)
                        'no-hyphen))
                  intervals))
         (should-not
          (seq-some
           (lambda (interval)
             (eq (ekp--policy-interval-break-policy interval) 'no-break))
           intervals))
         (should (equal (ekp-tests--policy-interval-signatures intervals)
                        `((0 ,(length span) no-hyphen face nil))))
         (should (eq (get-text-property 1 'face analysis)
                     'ekp-test-inline))
         (should (eq (get-text-property 3 'ekp-custom-property analysis)
                     'kept))
         (let* ((para (ekp--make-para text (cons analysis intervals)))
                (boxes (append (ekp-para-boxes para) nil)))
           (dolist (box boxes)
             (ekp-tests--assert-no-private-policy-properties box))))))))

(ert-deftest ekp-test-explicit-no-break-with-face-policy-never-downgrades ()
  "Explicit ekp-no-break stays rigid even with overwide face no-break."
  (ekp-tests--with-clean-state
   (let* ((atom (copy-sequence "foo bar baz"))
          (text (concat "x " atom " y"))
          (start 2)
          (end (+ start (length atom))))
     (put-text-property start end 'ekp-no-break t text)
     (put-text-property start end 'ekp--face-break-policy 'no-break text)
     (let ((lines (let ((ekp--policy-measure 4))
                    (ekp-tests--layout-source-lines text 4))))
       (should (seq-find
                (lambda (line)
                  (string-match-p (regexp-quote atom) line))
                lines))))))

(ert-deftest ekp-test-face-policy-makes-spacing-literal-only-for-face ()
  "Face policy uses literal spacing; token and region policy do not."
  (ekp-tests--with-clean-state
   (let ((face (copy-sequence "aa bb"))
         (region (copy-sequence "aa bb"))
         (token (copy-sequence "src/foo")))
     (put-text-property 0 (length face)
                        'ekp--face-break-policy 'no-hyphen face)
     (put-text-property 0 (length region)
                        'ekp-break-policy 'no-hyphen region)
     (let ((ekp-token-break-policies '((path . no-hyphen))))
       (should (eq (get-text-property 2 'ekp--literal-spacing
                                      (car (ekp--analyze-policies face)))
                   t))
       (should-not (get-text-property 2 'ekp--literal-spacing
                                      (car (ekp--analyze-policies region))))
       (should-not (get-text-property 3 'ekp--literal-spacing
                                      (car (ekp--analyze-policies token))))))))

(ert-deftest ekp-test-face-literal-spaces-stay-boxes-with-outer-breaks ()
  "Face literal spaces remain boxes while the following boundary can break."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "aa  bb cc"))
          (ekp--policy-measure 100))
     (put-text-property 0 6 'ekp--face-break-policy 'no-hyphen text)
     (let* ((policy-analysis (ekp--analyze-policies text))
            (para (ekp--make-para text policy-analysis))
            (boxes (append (ekp-para-boxes para) nil))
            (breaks (ekp-para-breaks-allowed para)))
       (should (equal boxes '("aa" "  " "bb" "cc")))
       (should (equal (append breaks nil) '(t nil t t t)))
       (should-not (aref breaks 1))
       (should (aref breaks 2))
       (should (aref breaks 3))))))

(ert-deftest ekp-test-face-literal-space-layout-breaks-after-space-boxes ()
  "Face literal-space breaks own source spaces, independent of style."
  (ekp-tests--with-clean-state
   (dolist (styled '(nil t))
     (let* ((text (copy-sequence "aa  bb  cc  dd"))
            (ekp--policy-measure 100))
       (put-text-property 0 (length text)
                          'ekp--face-break-policy 'no-hyphen text)
       (when styled
         (put-text-property 1 13 'face 'ekp-test-inline text))
       (cl-letf (((symbol-function 'ekp--string-pixel-width)
                  (lambda (string) (length string))))
         (let* ((policy-analysis (ekp--analyze-policies text))
                (para (ekp--make-para text policy-analysis))
                (breaks (append (ekp-para-breaks-allowed para) nil))
                (plan (ekp-layout-plan text 4))
                (lines (append (ekp-layout-plan-lines plan) nil))
                (source-lines
                 (mapcar
                  (lambda (line)
                    (substring-no-properties
                     text
                     (ekp-layout-line-source-start line)
                     (ekp-layout-line-source-end line)))
                  lines))
                (source-maps
                 (mapcar
                  (lambda (line)
                    (list (ekp-layout-line-source-start line)
                          (ekp-layout-line-source-end line)
                          (ekp-layout-line-break-source-start line)
                          (ekp-layout-line-break-source-end line)))
                  lines)))
           (should (equal breaks '(t nil t nil t nil t t)))
           (should (equal source-lines '("aa" "bb" "cc" "dd")))
           (should (equal source-maps
                          '((0 2 2 4) (4 6 6 8)
                            (8 10 10 12) (12 14 14 14))))
           (dolist (line source-lines)
             (should-not (string-prefix-p " " line))
             (should-not (string-suffix-p " " line)))
           (should (seq-every-p
                    (lambda (line)
                      (not (ekp-layout-line-hyphen-p line)))
                    lines))))))))

(ert-deftest ekp-test-private-face-policy-does-not-leak-from-plan ()
  "Private face policy is stripped from plan strings, boxes, and rendering."
  (ekp-tests--with-clean-state
   (let* ((source (propertize "aa bb cc"
                              'face 'ekp-test-inline
                              'ekp-custom-property 'kept))
          (width 100))
     (put-text-property 0 5 'ekp--face-break-policy 'no-hyphen source)
     (let* ((plan (ekp-layout-plan source width))
            (plan-string (ekp-layout-plan-string plan))
            (para (ekp-layout-plan-para plan))
            (boxes (append (ekp-para-boxes para) nil))
            (rendered (ekp-render-layout-string plan))
            (key (ekp--para-key source)))
       (ekp-tests--assert-no-private-policy-properties plan-string)
       (ekp-tests--assert-no-private-policy-properties (car key))
       (ekp-tests--assert-no-private-policy-properties (car ekp--last-para))
       (ekp-tests--assert-no-private-policy-properties
        (car (cadr ekp--last-para)))
       (dolist (box boxes)
         (ekp-tests--assert-no-private-policy-properties box))
       (ekp-tests--assert-no-private-policy-properties rendered)
       (should (equal-including-properties
                (substring plan-string 0 5)
                (propertize "aa bb"
                            'face 'ekp-test-inline
                            'ekp-custom-property 'kept)))
       (should (equal-including-properties
                (substring (car key) 0 5)
                (propertize "aa bb"
                            'face 'ekp-test-inline
                            'ekp-custom-property 'kept)))))))

(ert-deftest ekp-test-kinsoku-profiles-public-path ()
  "Kinsoku profiles are selectable and do not leak global punctuation state."
  (ekp-tests--with-clean-state
   (let ((width 2))
     (let ((ekp-kinsoku-profile 'common))
       (should (member ?ぁ (ekp-tests--line-start-chars "あぁい" width)))
       (should-not (member ?。 (ekp-tests--line-start-chars "あ。い" width))))
     (let ((ekp-kinsoku-profile 'zh))
       (should-not (member ?， (ekp-tests--line-start-chars "中，文" width)))
       (should-not (member ?、 (ekp-tests--line-start-chars "中、文" width))))
     (let ((ekp-kinsoku-profile 'ja))
       (dolist (char '(?ぁ ?ー ?々))
         (should-not
          (member char
                  (ekp-tests--line-start-chars
                   (concat "あ" (char-to-string char) "い") width)))))
     (let ((ekp-kinsoku-profile 'ja)
           (ekp-cjk-no-line-start-extra ""))
       (should-not (member ?ぁ (ekp-tests--line-start-chars "あぁい" width))))
     (let ((ekp-kinsoku-profile 'off))
       (should (member ?。 (ekp-tests--line-start-chars "あ。い" width))))
     (let ((ekp-kinsoku-profile 'custom)
           (ekp-cjk-no-line-start-extra "※")
           (ekp-cjk-no-line-end-extra "〒"))
       (should-not (member ?※ (ekp-tests--line-start-chars "あ※い" width)))
       (let ((lines (ekp-tests--rendered-lines "〒あい" width)))
         (should-not (seq-some
                      (lambda (line) (string-suffix-p "〒" line))
                      (butlast lines))))
       (should (member ?☆ (ekp-tests--line-start-chars "あ☆い" width))))
     (let ((ekp-kinsoku-profile 'custom)
           (ekp-cjk-no-line-start-extra "")
           (ekp-cjk-no-line-end-extra ""))
       (should (member ?※ (ekp-tests--line-start-chars "あ※い" width)))))))

(ert-deftest ekp-test-overlong-token-policies-are-distinguishable ()
  "Emergency, overflow, and natural overlong policies expose distinct states."
  (ekp-tests--with-clean-state
   (let* ((token "supercalifragilisticexpialidocious")
          (text (concat "aa " token " zz"))
          (width 8)
          (emergency (let ((ekp-overlong-token-policy 'emergency))
                       (ekp-layout-plan text width)))
          (overflow (let ((ekp-overlong-token-policy 'overflow))
                      (ekp-layout-plan text width)))
          (natural (let ((ekp-overlong-token-policy 'natural))
                     (ekp-layout-plan text width))))
     (should (eq (ekp-tests--plan-state emergency) 'planned))
     (should (eq (ekp-tests--plan-state overflow) 'planned))
     (should (eq (ekp-tests--plan-state natural) 'natural))
     (should (eq (ekp-tests--plan-reason natural) 'overlong-token))
     (should-not (equal (ekp-render-layout-string emergency)
                        (ekp-render-layout-string overflow)))
     (should (equal-including-properties
              (let ((ekp-overlong-token-policy 'natural))
                (ekp-pixel-justify text width))
              text))
     (ekp-tests--assert-no-private-policy-properties
      (ekp-render-layout-string natural)))))

(ert-deftest ekp-test-policy-properties-change-cache-identity ()
  "In-place `ekp-break-policy' changes invalidate paragraph and DP identity."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "internationalization policy cache"))
          (width 12)
          (plain (ekp-layout-plan text width)))
     (put-text-property 0 (length "internationalization")
                        'ekp-break-policy 'no-hyphen text)
     (let ((blocked (ekp-layout-plan text width)))
       (should-not (eq (ekp-layout-plan-para plain)
                       (ekp-layout-plan-para blocked)))
       (should-not (equal (mapcar #'ekp-layout-line-signature
                                  (append (ekp-layout-plan-lines plain) nil))
                          (mapcar #'ekp-layout-line-signature
                                  (append (ekp-layout-plan-lines blocked) nil))))
       (remove-text-properties 0 (length text) '(ekp-break-policy nil) text)
       (let ((roundtrip (ekp-layout-plan text width)))
         (should (equal (mapcar #'ekp-layout-line-signature
                                (append (ekp-layout-plan-lines plain) nil))
                        (mapcar #'ekp-layout-line-signature
                                (append (ekp-layout-plan-lines roundtrip)
                                        nil)))))))))

(ert-deftest ekp-test-policy-analysis-does-not-leak-private-properties ()
  "Policy analysis preserves public properties and strips internal markers."
  (ekp-tests--with-clean-state
   (let* ((source (propertize "processKeyword42"
                              'face 'ekp-test-inline
                              'ekp-custom-property 'kept))
          (rendered (let ((ekp-inline-code-policy 'no-hyphen)
                          (ekp-token-break-policies
                           '((identifier . no-hyphen))))
                      (ekp-pixel-justify source 8))))
     (should (equal (ekp-tests--content rendered)
                    (ekp-tests--content source)))
     (should (eq (get-text-property 0 'face rendered)
                    'ekp-test-inline))
     (should (eq (get-text-property 0 'ekp-custom-property rendered)
                 'kept))
     (ekp-tests--assert-no-private-policy-properties rendered))))

(ert-deftest ekp-test-policy-analysis-reuses-source-when-no-policy-matches ()
  "No-match policy analysis must return the original string object."
  (ekp-tests--with-clean-state
   (let ((source (propertize "ordinary words without policy"
                             'face 'ekp-test-face))
         (copy-count 0)
         (copy-sequence-fn (symbol-function 'copy-sequence)))
     (cl-letf (((symbol-function 'copy-sequence)
                (lambda (sequence)
                  (setq copy-count (1+ copy-count))
                  (funcall copy-sequence-fn sequence))))
       (let ((analysis (ekp--analyze-policies source)))
         (should (eq (car analysis) source))
         (should (null (cdr analysis)))))
     (should (= copy-count 0))
     (should (eq (get-text-property 0 'face source) 'ekp-test-face)))))

(ert-deftest ekp-test-c-policy-contract-remains-fifteen-fields-and-args ()
  "Policy compilation must not expand the C DP ABI."
  (ekp-tests--with-clean-state
   (let* ((para (ekp--get-para
                 (propertize "internationalization policy contract"
                             'ekp-break-policy 'no-hyphen)))
          (prepared (ekp--prepare-para-for-c para 12))
          captured-args)
     (should (= (length prepared) 15))
     (let ((ekp-use-c-module t)
           (ekp-c-module-loaded t))
       (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                  (lambda (&rest args)
                    (setq captured-args args)
                    (cons nil 0.0))))
         (ekp-dp-cache "internationalization policy contract" 12)))
     (should (= (length captured-args) 15)))))

(ert-deftest ekp-test-c-emergency-stretch-contract-remains-fifteen-fields-and-args ()
  "Configurable emergency stretch must not expand the C DP ABI."
  (ekp-tests--with-clean-state
   (let* ((ekp-emergency-stretch-pixel 4)
          (para (ekp--get-para "alpha beta emergency stretch contract"))
          (prepared (ekp--prepare-para-for-c para 12))
          captured-args)
     (should (= (length prepared) 15))
     (let ((ekp-use-c-module t)
           (ekp-c-module-loaded t))
       (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                  (lambda (&rest args)
                    (setq captured-args args)
                    (cons nil 0.0))))
         (ekp-dp-cache "alpha beta emergency stretch contract" 12)))
     (should (= (length captured-args) 15)))))

(ert-deftest ekp-test-c-emergency-stretch-syncs-fixed-budget-as-eighth-param ()
  "The C penalty sync receives the fixed emergency stretch as argument 8."
  (ekp-tests--with-clean-state
   (let ((ekp-emergency-stretch-pixel 7)
         captured-args)
     (cl-letf (((symbol-function 'ekp-c-set-penalties)
                (lambda (&rest args)
                  (setq captured-args args))))
       (ekp--c-sync-params))
     (should (= (length captured-args) 8))
     (should (= (nth 7 captured-args) 7)))))

(ert-deftest ekp-test-c-emergency-stretch-batch-payload-remains-fifteen-fields ()
  "Batch paragraphs keep the 15-field C ABI when emergency stretch is configured."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module t)
         (ekp-c-module-loaded t)
         (ekp-emergency-stretch-pixel 7)
         captured-batch)
     (cl-letf (((symbol-function 'ekp-c-set-penalties) #'ignore)
               ((symbol-function 'ekp-c-break-batch)
                (lambda (batch)
                  (setq captured-batch batch)
                  nil)))
       (ekp--dp-cache-batch
        '("alpha beta emergency batch"
          "gamma delta emergency batch")
        12))
     (should (vectorp captured-batch))
     (should (= (length captured-batch) 2))
     (dotimes (index (length captured-batch))
       (should (= (length (aref captured-batch index)) 15))))))

(ert-deftest ekp-test-c-policy-contract-explicit-no-break-reaches-c-dispatch ()
  "The public C dispatch path must handle explicit no-break paragraphs."
  (ekp-tests--with-clean-state
   (let* ((atom (propertize "foo bar baz" 'ekp-no-break t))
          (text (concat "aa " atom " zz"))
          (ekp-use-c-module t)
          (ekp-c-module-loaded t)
          c-called)
     (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                (lambda (&rest _args)
                  (setq c-called t)
                  (cons nil 0.0))))
       (ekp-dp-cache text 60))
     (should c-called))))

(ert-deftest ekp-test-c-policy-contract-explicit-no-break-stores-c-directly ()
  "Explicit no-break C results must be stored without Elisp comparison fallback."
  (skip-unless (ekp-tests--c-available))
  (ekp-tests--with-clean-state
   (let* ((atom (propertize "foo bar baz" 'ekp-no-break t))
          (text (concat "aa " atom " zz"))
          (ekp-use-c-module t))
     (cl-letf (((symbol-function 'ekp--dp-cache-elisp)
                (lambda (&rest _)
                  (error "explicit no-break C dispatch must not call Elisp"))))
       (let ((result (ekp-dp-cache text 60)))
         (should (plist-get result :breaks))
         (should (numberp (plist-get result :cost))))))))

(ert-deftest ekp-test-c-batch-policy-parity-explicit-no-break-orphan-fixture ()
  "Batch C DP must match Elisp for the strengthened explicit no-break fixture."
  (skip-unless (ekp-tests--c-available))
  (let* ((atom (propertize
                (concat "原子演示: 代码片段 " (make-string 20 ?a))
                'ekp-no-break t))
         (text (concat
                "行内" atom
                " 永不折散、空格保持字面宽度；不间断空格让 "
                "100_000 与 3.14_MB 这类数字单位锁在同一行。"))
         (width 41)
         elisp-result elisp-signatures
         batch-result batch-signatures)
    (ekp-tests--with-clean-state
     (let ((ekp-use-c-module nil))
       (setq elisp-result (ekp-dp-cache text width)
             elisp-signatures
             (mapcar #'ekp-layout-line-signature
                     (append (ekp-layout-plan-lines
                              (ekp-layout-plan text width))
                             nil)))))
    (ekp-tests--with-clean-state
     (let ((ekp-use-c-module t))
       (setq batch-result (car (ekp--dp-cache-batch (list text) width))
             batch-signatures
             (mapcar #'ekp-layout-line-signature
                     (append (ekp-layout-plan-lines
                              (ekp-layout-plan text width))
                             nil)))))
    (should (equal (plist-get batch-result :breaks)
                   (plist-get elisp-result :breaks)))
    (should (equal (plist-get batch-result :cost)
                   (plist-get elisp-result :cost)))
    (should (equal batch-signatures elisp-signatures))))

(ert-deftest ekp-test-c-policy-contract-forbidden-positions-are-break-indices ()
  "C forbidden positions carry only real break indices, never policy sentinels."
  (ekp-tests--with-clean-state
   (let* ((atom (propertize "foo bar"
                            'ekp-no-break t
                            'ekp-break-policy 'normal))
          (para (ekp--get-para atom))
          (n (length (ekp-para-boxes para)))
          (forbidden (ekp-para-forbidden-positions para)))
     (should-not (= (length forbidden) 0))
     (dotimes (i (length forbidden))
       (should (<= 1 (aref forbidden i)))
       (should (<= (aref forbidden i) n))))))

(ert-deftest ekp-test-explicit-hard-atom-keeps-preceding-boundary-legal ()
  "An explicit hard atom must not rewrite an adjacent ordinary break."
  (ekp-tests--with-clean-state
   (dolist (case '(("行" "内" "原子演示")
                   ("甲乙" "丙" "代码片段")))
     (pcase-let ((`(,prefix ,ordinary ,atom-text) case))
       (let* ((atom (propertize atom-text 'ekp-no-break t))
              (text (concat prefix ordinary atom " 后文"))
              (para (ekp--get-para text))
              (boxes (ekp-para-boxes para))
              (breaks (ekp-para-breaks-allowed para))
              (atom-index
               (cl-loop for i from 0 below (length boxes)
                        when (get-text-property 0 'ekp-no-break
                                                (aref boxes i))
                        return i))
              (ordinary-index (and atom-index (1- atom-index))))
         (should atom-index)
         (should (> ordinary-index 0))
         (should (equal (substring-no-properties (aref boxes ordinary-index))
                        ordinary))
         (should-not (get-text-property 0 'ekp-no-break
                                        (aref boxes ordinary-index)))
         (should-not (ekp--box-space-p (aref boxes ordinary-index)))
         (should (aref breaks ordinary-index)))))))

(ert-deftest ekp-test-c-policy-contract-single-dispatch-rejects-malformed-breaks ()
  "Malformed non-nil C single-dispatch breaks are backend contract errors."
  (ekp-tests--with-clean-state
   (cl-labels
       ((nboxes (args)
          (1- (length (car args))))
        (valid-breaks (args)
          (list (nboxes args)))
        (assert-backend-contract-error (_label err)
          (should
           (memq 'ekp-backend-contract-error
                 (get (car err) 'error-conditions)))))
     (let ((ekp-use-c-module t)
           (ekp-c-module-loaded t)
           (elisp-calls 0)
           (elisp (symbol-function 'ekp--dp-cache-elisp))
           (text "alpha beta gamma delta epsilon")
           (width 16))
       (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                  (lambda (&rest _)
                    (cons nil 0.0)))
                 ((symbol-function 'ekp--dp-cache-elisp)
                  (lambda (&rest args)
                    (cl-incf elisp-calls)
                    (apply elisp args))))
         (should (plist-get (ekp-dp-cache text width) :breaks))
         (should (= elisp-calls 1)))
       (dolist
           (case
            `(("out-of-range break"
               . ,(lambda (args) (cons (list (1+ (nboxes args))) 0.0)))
              ("partial final coverage"
               . ,(lambda (args) (cons (list (1- (nboxes args))) 0.0)))
              ("duplicate breaks"
               . ,(lambda (_args) (cons '(1 1) 0.0)))
              ("decreasing breaks"
               . ,(lambda (args) (cons (list 2 1 (nboxes args)) 0.0)))
              ("non-integer breaks"
               . ,(lambda (args) (cons (list 1 'bad (nboxes args)) 0.0)))
              ("non-list breaks"
               . ,(lambda (args) (cons (vector (nboxes args)) 0.0)))
              ("nonnumeric cost"
               . ,(lambda (args) (cons (valid-breaks args) 'bad-cost)))
              ("malformed non-cons result"
               . ,(lambda (_args) 'bad-result))))
        (ekp-clear-caches)
        (setq elisp-calls 0)
        (let ((label (car case))
              (make-result (cdr case)))
          (ert-info ((format "single malformed C case: %s" label))
            (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                       (lambda (&rest args)
                         (funcall make-result args)))
                      ((symbol-function 'ekp--dp-cache-elisp)
                       (lambda (&rest _)
                         (cl-incf elisp-calls)
                         (error "single malformed C result called Elisp fallback"))))
              (let ((err (should-error (ekp-dp-cache text width))))
                (should (= elisp-calls 0))
                (assert-backend-contract-error label err))))))))))

(ert-deftest ekp-test-c-policy-contract-batch-dispatch-rejects-malformed-breaks ()
  "Malformed non-nil C batch breaks are backend contract errors."
  (ekp-tests--with-clean-state
   (cl-labels
       ((nboxes (entry)
          (1- (length (aref entry 0))))
        (valid-breaks (entry)
          (list (nboxes entry)))
        (one-entry-vector (batch make-result)
          (let ((results (make-vector (length batch) nil)))
            (aset results 0 (funcall make-result (aref batch 0)))
            results))
        (assert-backend-contract-error (_label err)
          (should
           (memq 'ekp-backend-contract-error
                 (get (car err) 'error-conditions)))))
     (let ((ekp-use-c-module t)
           (ekp-c-module-loaded t)
           (elisp-calls 0)
           (elisp (symbol-function 'ekp--dp-cache-elisp))
           (text '("alpha beta gamma delta epsilon"))
           (width 16))
       (cl-letf (((symbol-function 'ekp-c-break-batch)
                  (lambda (_batch) nil))
                 ((symbol-function 'ekp--dp-cache-elisp)
                  (lambda (&rest args)
                    (cl-incf elisp-calls)
                    (apply elisp args))))
         (should (plist-get (car (ekp--dp-cache-batch text width))
                            :breaks))
         (should (= elisp-calls 1)))
       (ekp-clear-caches)
       (setq elisp-calls 0)
       (cl-letf (((symbol-function 'ekp-c-break-batch)
                  (lambda (batch)
                    (make-vector (length batch) nil)))
                 ((symbol-function 'ekp--dp-cache-elisp)
                  (lambda (&rest args)
                    (cl-incf elisp-calls)
                    (apply elisp args))))
         (should (plist-get (car (ekp--dp-cache-batch text width))
                            :breaks))
         (should (= elisp-calls 1)))
       (dolist
           (case
            `(("out-of-range break"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (list (1+ (nboxes entry))) 0.0)))))
              ("partial final coverage"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (list (1- (nboxes entry))) 0.0)))))
              ("duplicate breaks"
               . ,(lambda (batch)
                    (one-entry-vector batch
                                      (lambda (_entry) (cons '(1 1) 0.0)))))
              ("decreasing breaks"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (list 2 1 (nboxes entry)) 0.0)))))
              ("non-integer breaks"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (list 1 'bad (nboxes entry)) 0.0)))))
              ("non-list breaks"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (vector (nboxes entry)) 0.0)))))
              ("nonnumeric cost"
               . ,(lambda (batch)
                    (one-entry-vector
                     batch
                     (lambda (entry)
                       (cons (valid-breaks entry) 'bad-cost)))))
              ("malformed non-cons result"
               . ,(lambda (batch)
                    (one-entry-vector batch (lambda (_entry) 'bad-result))))
              ("wrong non-nil container"
               . ,(lambda (batch)
                    (append (one-entry-vector
                             batch
                             (lambda (entry)
                               (cons (valid-breaks entry) 0.0)))
                            nil)))
              ("wrong vector length"
               . ,(lambda (_batch) []))))
        (ekp-clear-caches)
        (setq elisp-calls 0)
        (let ((label (car case))
              (make-results (cdr case)))
          (ert-info ((format "batch malformed C case: %s" label))
            (cl-letf (((symbol-function 'ekp-c-break-batch)
                       (lambda (batch)
                         (funcall make-results batch)))
                      ((symbol-function 'ekp--dp-cache-elisp)
                       (lambda (&rest _)
                         (cl-incf elisp-calls)
                         (error "batch malformed C result called Elisp fallback"))))
              (let ((err (should-error (ekp--dp-cache-batch text width))))
                (should (= elisp-calls 0))
                (assert-backend-contract-error label err))))))))))

(ert-deftest ekp-test-split-fullwidth-alnum-not-punct ()
  "Fullwidth letters/digits are content, not punctuation."
  (should-not (ekp-cjk-fw-punct-p "Ａ"))
  (should-not (ekp-cjk-fw-punct-p "５"))
  (should-not (ekp-cjk-fw-punct-p "ｚ"))
  (should (ekp-cjk-fw-punct-p "！"))
  (should (ekp-cjk-fw-punct-p "。"))
  (should (equal (append (ekp-split-to-boxes "中ＡＢ文") nil)
                 '("中" "Ａ" "Ｂ" "文"))))

(ert-deftest ekp-test-split-combining-chars-attach ()
  "Combining marks must stay attached to their base character."
  (let ((nfd (string ?c ?a ?f ?e #x0301)))  ; "cafe" + combining acute
    (should (= 1 (length (ekp-split-to-boxes nfd))))))

(ert-deftest ekp-test-split-preserved-spaces ()
  "Leading spaces and CJK-adjacent spaces are preserved as boxes;
a single Latin-Latin space is dropped (glue handles it)."
  ;; leading spaces preserved
  (should (equal (aref (ekp-split-to-boxes "  indent text") 0) "  "))
  ;; CJK-latin space preserved
  (should (member " " (append (ekp-split-to-boxes "中文 Latin") nil)))
  ;; latin-latin single space dropped
  (should-not (member " " (append (ekp-split-to-boxes "two words") nil))))

(ert-deftest ekp-test-split-with-hyphen-punctuation ()
  "Punctuation around a word must not disable hyphenation."
  (ekp-tests--with-clean-state
   (let ((ekp-latin-lang "en_US"))
     (dolist (word '("hyphenation" "hyphenation!" "(hyphenation)"
                     "hyphenation;" "hyphenation," "\"hyphenation\""))
       (let ((res (ekp--split-with-hyphen word)))
         (should (> (length (cdr res)) 0)))))))

;;;; Justification core

(ert-deftest ekp-test-justify-line-width-invariant ()
  "Every justified line renders at exactly the requested pixel width."
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (dolist (case '(("中文 Latin 混排 test 保留 space 的情况 with spaces"
                    30 40 50)
                   ("The quick brown fox jumps over the lazy dog runs fast"
                    40 60)
                   ("中文排版是一门艺术需要考虑标点悬挂避头尾等规则" 20 30)))
     (let ((s (car case)))
       (dolist (w (cdr case))
         (ekp-clear-caches)
         (dolist (lw (ekp-tests--line-widths (ekp-pixel-justify s w)))
           (should (= lw w))))))))

(ert-deftest ekp-test-justify-no-content-loss ()
  "Justification must never lose characters, at any width."
  (ekp-tests--with-clean-state
   (ekp-param-set 3 1 1 2 1 1 0 2 0)
   (dolist (s '("中文排版测试"
                "The quick brown fox jumps over the lazy dog"
                "中文 mixed 混排 words 测试"
                "bcdfghjklmnpqrstvwxz supercalifragilistic"))
     (dolist (w '(1 3 10 50 200))
       (ekp-clear-caches)
       (should (equal (ekp-tests--content (ekp-pixel-justify s w))
                      (ekp-tests--content s)))))))

(ert-deftest ekp-test-emergency-stretch-underfull-renderer-distributes-rest-into-gaps ()
  "A normal emergency-stretch underfull line must not render rest as tail fill."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (ekp-emergency-stretch-pixel 1)
          (text "alpha beta gamma delta epsilon")
          (width 12)
          (para (ekp--get-para text))
          (strict (ekp--dp-run-1d para width nil))
          (plan (ekp-layout-plan text width))
          (rendered (ekp-render-layout-string plan))
          (line (aref (ekp-layout-plan-lines plan) 0))
          (gaps (append (ekp-layout-line-gaps line) nil)))
     (should-not strict)
     (should (= (car (ekp-tests--line-widths rendered)) width))
     (should gaps)
     (should (seq-some (lambda (gap)
                         (> (ekp-layout-gap-target-pixel gap)
                            (ekp-layout-gap-natural-pixel gap)))
                       gaps))
     (should (= (ekp-layout-line-trailing-pixel line) 0)))))

(ert-deftest ekp-test-justify-narrow-cjk-one-char-per-line ()
  "At a width narrower than one CJK char, output one char per line
instead of losing the paragraph (regression: used to return \"\")."
  (ekp-tests--with-clean-state
   (let ((out (ekp-pixel-justify "中文排版测试" 1)))
     (should (= 6 (length (split-string out "\n")))))))

(ert-deftest ekp-test-justify-edge-inputs ()
  (ekp-tests--with-clean-state
   (should (equal (ekp-pixel-justify "" 100) ""))
   (should (equal (ekp-pixel-justify "   " 100) ""))
   (should (stringp (ekp-pixel-justify "x" 100)))
   (should (stringp (ekp-pixel-justify "hello" 100)))))

(ert-deftest ekp-test-justify-invalid-args ()
  (ekp-tests--with-clean-state
   (should-error (ekp-pixel-justify "text" 0) :type 'user-error)
   (should-error (ekp-pixel-justify "text" -5) :type 'user-error)
   (should-error (ekp-pixel-justify "text" 2.5) :type 'user-error)
   (should-error (ekp-pixel-justify 42 100) :type 'wrong-type-argument)
   (should-error (ekp-pixel-range-justify "text" 100 50) :type 'user-error)))

(ert-deftest ekp-test-justify-multiline-blank-preserved ()
  "Blank input lines separate paragraphs and survive as empty lines."
  (ekp-tests--with-clean-state
   (let ((out (ekp-pixel-justify "para one text\n\npara two text" 200)))
     (should (= 3 (length (split-string out "\n"))))
     (should (equal "" (nth 1 (split-string out "\n")))))))

(ert-deftest ekp-test-justify-breaks-monotonic ()
  (ekp-tests--with-clean-state
   (let* ((s "one two three four five six seven eight")
          (breaks (ekp-line-breaks s 30))
          (n (length (ekp-para-boxes (ekp--get-para s)))))
     (should (equal breaks (sort (copy-sequence breaks) #'<)))
     (should (= (car (last breaks)) n)))))

;;;; Parameters

(ert-deftest ekp-test-params-persist ()
  "Explicit `ekp-param-set' persists across paragraphs (regression:
they used to be silently reset after the first justification)."
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (ekp-pixel-justify "first paragraph of text here" 60)
   (ekp-pixel-justify "second different paragraph text" 60)
   (should (= ekp-lws-ideal-pixel 5))
   (should (= ekp-mws-ideal-pixel 4))))

(ert-deftest ekp-test-params-reset-restores-auto ()
  (ekp-tests--with-clean-state
   (ekp-param-set 9 3 2 8 3 2 0 4 0)
   (ekp-param-reset)
   ;; After reset, defaults are derived per string again
   (ekp-pixel-justify "some text to justify here" 60)
   (should-not (= (or ekp-lws-ideal-pixel 0) 9))))

(ert-deftest ekp-test-params-affect-c-module ()
  "Penalty variables must reach the C module (regression: they were
never synced, so Elisp and C diverged)."
  (skip-unless (ekp-tests--c-available))
  (ekp-tests--with-clean-state
   (ekp-param-set 3 1 1 2 1 1 0 2 0)
   (let ((s "The quick brown fox jumps over the lazy dog and keeps running through the emergency broadcast system test of hyphenation quality"))
     (dolist (hp '(50 1000000))
       (setq ekp-hyphen-penalty hp)
       (setq ekp-use-c-module t)
       (ekp-clear-caches)
       (let ((c-out (ekp-pixel-justify s 40)))
         (setq ekp-use-c-module nil)
         (ekp-clear-caches)
         (should (equal (ekp-pixel-justify s 40) c-out)))))))

;;;; Looseness

(ert-deftest ekp-test-looseness ()
  "looseness=+1 adds a line when feasible; -1 removes one; the C
module is bypassed automatically (it has no looseness support)."
  (ekp-tests--with-clean-state
   (ekp-param-set 1 1 1 1 0 0 0 2 0)
   (let* ((s "one two three four five six seven eight nine ten eleven twelve")
          (lines (lambda ()
                   (ekp-clear-caches)
                   (length (split-string (ekp-pixel-justify s 30) "\n"))))
          (n0 (progn (setq ekp-looseness 0) (funcall lines)))
          (n+ (progn (setq ekp-looseness 1) (funcall lines)))
          (n- (progn (setq ekp-looseness -1) (funcall lines))))
     (should (= n+ (1+ n0)))
     (should (<= n- n0)))))

;;;; Caching

(ert-deftest ekp-test-para-cache-hit ()
  (ekp-tests--with-clean-state
   (let ((p1 (ekp--get-para "same string"))
         (p2 (ekp--get-para "same string")))
     (should (eq p1 p2)))))

(ert-deftest ekp-test-para-cache-distinguishes-properties ()
  "Strings differing only in text properties must not share a para."
  (ekp-tests--with-clean-state
   (let ((p1 (ekp--get-para "same string"))
         (p2 (ekp--get-para (propertize "same string" 'face 'bold))))
     (should-not (eq p1 p2)))))

(ert-deftest ekp-test-para-cache-detects-in-place-properties ()
  "Mutating properties on the same string object must miss every cache path."
  (ekp-tests--with-clean-state
   (dolist (text (list (copy-sequence "文中排版")
                       (copy-sequence "alpha beta gamma")))
     (let ((before (ekp--get-para text)))
       (put-text-property 0 (length text) 'ekp-no-break t text)
       (let ((changed (ekp--get-para text)))
         (should-not (eq before changed))
         (should-not (equal (ekp-para-breaks-allowed before)
                            (ekp-para-breaks-allowed changed)))
         (ekp-clear-caches)
         (let ((fresh (ekp--get-para text)))
           (should (equal (ekp-para-boxes changed)
                          (ekp-para-boxes fresh)))
           (should (equal (mapcar #'object-intervals
                                  (append (ekp-para-boxes changed) nil))
                          (mapcar #'object-intervals
                                  (append (ekp-para-boxes fresh) nil))))
           (should (equal (ekp-para-breaks-allowed changed)
                          (ekp-para-breaks-allowed fresh)))))))))

(ert-deftest ekp-test-para-cache-limit ()
  (ekp-tests--with-clean-state
   (let ((ekp-para-cache-limit 2))
     (ekp--get-para "one")
     (ekp--get-para "two")
     (ekp--get-para "three")  ; triggers flush
     (should (<= (hash-table-count ekp--para-cache) 2)))))

(ert-deftest ekp-test-para-cache-tracks-language ()
  "Switching `ekp-latin-lang' must not reuse stale hyphenation
\(regression: the cache key ignored the language)."
  (ekp-tests--with-clean-state
   (let ((s "Universität hyphenation emergency")
         (old ekp-latin-lang))
     (unwind-protect
         (progn
           (setq ekp-latin-lang "en_US")
           (let ((b-en (copy-sequence
                        (ekp-para-boxes (ekp--get-para s)))))
             (setq ekp-latin-lang "de_DE")
             ;; Same string object, no cache clear: must re-hyphenate.
             (let ((b-de (ekp-para-boxes (ekp--get-para s))))
               (should-not (equal b-en b-de))
               ;; And it must equal a fresh computation.
               (ekp-clear-caches)
               (should
                (equal b-de (ekp-para-boxes (ekp--get-para s)))))))
       (setq ekp-latin-lang old)))))

(ert-deftest ekp-test-dp-cache-reuse ()
  (ekp-tests--with-clean-state
   (let* ((s "cached paragraph text here")
          (r1 (ekp-dp-cache s 60))
          (r2 (ekp-dp-cache s 60)))
     (should (eq r1 r2)))))

(ert-deftest ekp-test-emergency-stretch-pixel-is-dp-cache-input ()
  "Changing explicit emergency stretch must change final-pass DP identity."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (text "alpha beta gamma delta epsilon")
          (width 12)
          (para (ekp--get-para text)))
     (should-not (ekp--dp-run-1d para width nil))
     (let* ((ekp-emergency-stretch-pixel 1)
            (small (ekp--dp-cache-para para width)))
       (should small)
       (let* ((ekp-emergency-stretch-pixel 20)
              (cached-large (ekp--dp-cache-para para width)))
         (should (= (hash-table-count (ekp-para-dp-cache para)) 2))
         (ekp-clear-caches)
         (let* ((fresh-para (ekp--get-para text))
                (ekp-emergency-stretch-pixel 20)
                (fresh-large (ekp--dp-cache-para fresh-para width)))
           (should (equal cached-large fresh-large))
           (should-not (equal small fresh-large))))))))

;;;; Line metrics invariants (brute force cross-check)

(ert-deftest ekp-test-line-ideal-brute-force ()
  "`ekp--line-ideal-pixel' must equal a naive recomputation."
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (let* ((s "中文 Latin 混排 test 的情况 with  spaces 结尾")
          (para (ekp--get-para s))
          (n (length (ekp-para-boxes para)))
          (widths (ekp-para-boxes-widths para))
          (types (ekp-para-boxes-types para))
          (glues (ekp-para-glues-types para))
          (hyphens (ekp-para-hyphen-positions para)))
     (dotimes (i n)
       (cl-loop for k from (1+ i) to n do
         (let* ((box-sum (cl-loop for j from i below k
                                  sum (aref widths j)))
                (glue-sum (cl-loop for j from (1+ i) below k
                                   sum (ekp--para-glue-ideal
                                        para (aref glues j))))
                ;; strip leading (i>0) and trailing space runs
                (lead (if (> i 0)
                          (let ((w 0) (j i))
                            (while (and (< j k)
                                        (eq (car (aref types j)) 'space))
                              (cl-incf w (aref widths j))
                              (cl-incf j))
                            w)
                        0))
                (trail (let ((w 0) (j (1- k)))
                         (while (and (>= j i)
                                     (eq (car (aref types j)) 'space))
                           (cl-incf w (aref widths j))
                           (cl-decf j))
                         w))
                (raw (+ box-sum glue-sum))
                (space-w (min raw (+ lead trail)))
                (expected (+ (- raw space-w)
                             (if (ekp--hyphenate-p hyphens (1- k))
                                 (ekp-para-hyphen-pixel para)
                               0))))
           (should (= (ekp--line-ideal-pixel para i k) expected))))))))

(ert-deftest ekp-test-line-edge-space-rule-brute-force ()
  "The shared edge-space rule must equal independent box scanning."
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (let* ((para (ekp--get-para "  中文  Latin  mixed  tail  "))
          (widths (ekp-para-boxes-widths para))
          (types (ekp-para-boxes-types para))
          (lead-spaces (ekp-para-lead-spaces para))
          (trail-spaces (ekp-para-trail-spaces para))
          (n (length widths)))
     (dotimes (i n)
       (cl-loop for k from (1+ i) to n do
         (let ((lead 0) (trail 0) (j i))
           (when (> i 0)
             (while (and (< j k) (eq (car (aref types j)) 'space))
               (cl-incf lead (aref widths j))
               (cl-incf j)))
           (setq j (1- k))
           (while (and (>= j i) (eq (car (aref types j)) 'space))
             (cl-incf trail (aref widths j))
             (cl-decf j))
           (let ((raw (cl-loop for x from i below k
                               sum (aref widths x))))
             (should
              (= (ekp--line-stripped-space-pixel
                  raw i k lead-spaces trail-spaces)
                 (min raw (+ lead trail)))))))))))

(ert-deftest ekp-test-layout-marker-protocol-owned ()
  "The render/inversion marker vocabulary must be complete and nonsticky."
  (should (equal ekp--layout-marker-properties
                 '(ekp-glue ekp-soft-break ekp-soft-hyphen ekp-hidden
                   ekp-justified)))
  (dolist (property ekp--layout-marker-properties)
    (should (eq (alist-get property text-property-default-nonsticky) t))))

(ert-deftest ekp-test-layout-plan-maps-source-gaps-and-breaks ()
  "The semantic plan must retain source offsets for every visual decision."
  (ekp-tests--with-clean-state
   (let* ((text "中文 Latin mixed paragraph with enough words to wrap")
          (plan (ekp-layout-plan text 24))
          (lines (ekp-layout-plan-lines plan))
          (last-end 0))
     (should (ekp-layout-plan-p plan))
     (should (equal (ekp-layout-plan-string plan) text))
     (should (= (ekp-layout-plan-line-pixel plan) 24))
     (should (> (length lines) 1))
     (dotimes (i (length lines))
       (let ((line (aref lines i)))
         (should (<= last-end (ekp-layout-line-source-start line)))
         (should (< (ekp-layout-line-source-start line)
                    (ekp-layout-line-source-end line)))
         (dolist (gap (append (ekp-layout-line-gaps line) nil))
           (should (memq (ekp-layout-gap-kind gap) '(lws mws cws nws)))
           (should (<= (ekp-layout-gap-source-start gap)
                       (ekp-layout-gap-source-end gap)))
           (should (>= (ekp-layout-gap-target-pixel gap) 0)))
         (when (< i (1- (length lines)))
           (should (memq (ekp-layout-line-break-kind line)
                         '(space cjk hyphen))))
         (setq last-end (ekp-layout-line-source-end line)))))))

(ert-deftest ekp-test-layout-plan-resolves-paragraph-once ()
  "One plan must not rebuild the same paragraph cache key downstream."
  (ekp-tests--with-clean-state
   (let ((calls 0)
         (get-para (symbol-function 'ekp--get-para)))
     (cl-letf (((symbol-function 'ekp--get-para)
                (lambda (string)
                  (setq calls (1+ calls))
                  (funcall get-para string))))
       (ekp-layout-plan
        "A mixed 中文 paragraph should resolve one cached paragraph object."
        24))
     (should (= calls 1)))))

(defun ekp-test--exact-append-plan (plan text width)
  "Return PLAN extended to TEXT after proving fresh parity at WIDTH."
  (let ((incremental (ekp-layout-plan-append plan text width)))
    (should incremental)
    (let ((actual (ekp-render-layout-string incremental)))
      (ekp-clear-caches)
      (should
       (equal-including-properties
        actual
        (ekp-render-layout-string
         (ekp-layout-plan text width)))))
    incremental))

(ert-deftest ekp-test-layout-plan-append-matches-fresh-plan ()
  "Incremental plain-text appends must equal a fresh global plan."
  (ekp-tests--with-clean-state
   (dolist (use-c (if (ekp-tests--c-available) '(nil t) '(nil)))
     (let* ((ekp-use-c-module use-c)
            (width 24)
            (text "alpha beta gamma changes responsive")
            (plan (ekp-layout-plan text width)))
       (dolist (suffix '("ly" " " "a" " 中文" " mixed" " continuation"))
         (setq text (concat text suffix)
               plan (ekp-test--exact-append-plan plan text width)))))))

(ert-deftest ekp-test-layout-plan-append-varied-chains-match-fresh ()
  "Diverse character-by-character append chains retain exact parity."
  (ekp-tests--with-clean-state
   (dolist (use-c (if (ekp-tests--c-available) '(nil t) '(nil)))
     (dolist (case '((justify nil 18 "ly  中文，punctuation.")
                     (justify 4 27 "extraordinary-hyphenation")
                     (ragged-right nil 21 " mixed 拉丁 alpha beta")
                     (center nil 31 "  repeated  spaces 文末")))
       (let* ((ekp-use-c-module use-c)
              (ekp-alignment (nth 0 case))
              (ekp-first-line-indent (nth 1 case))
              (width (nth 2 case))
              (text "alpha beta gamma")
              (plan (ekp-layout-plan text width)))
         (dolist (character (string-to-list (nth 3 case)))
           (setq text (concat text (char-to-string character))
                 plan (ekp-test--exact-append-plan
                       plan text width))))))))

(ert-deftest ekp-test-layout-plan-append-recomputes-dirty-boundary ()
  "The first retokenized boundary must not retain stale break metadata."
  (ekp-tests--with-clean-state
   (dolist (use-c (if (ekp-tests--c-available) '(nil t) '(nil)))
     (let* ((ekp-use-c-module use-c)
            (text "alpha beta  g-o。ycfrnqcc。( (")
            (width 12)
            (plan (ekp-layout-plan text width)))
       (ekp-test--exact-append-plan plan (concat text "l") width)))))

(ert-deftest ekp-test-layout-plan-append-rejects-unsafe-contexts ()
  "The append path rejects properties and non-1D layout contexts."
  (ekp-tests--with-clean-state
   (let* ((text "alpha beta gamma")
          (plan (ekp-layout-plan text 24))
          (plain (concat text " delta"))
          (styled (copy-sequence plain)))
     (add-text-properties 0 5 '(face bold) styled)
     (should-not (ekp-layout-plan-append plan styled 24))
     (should-not
      (ekp-layout-plan-append plan (concat text "\nnext") 24))
     (should-not
      (ekp-layout-plan-append plan (concat text "\ttail") 24))
     (should-not (ekp-layout-plan-append plan plain 25))
     (let ((ekp-looseness 1))
       (should-not (ekp-layout-plan-append plan plain 24)))
     (let ((ekp-parshape '((0 . 24))))
       (should-not (ekp-layout-plan-append plan plain 24)))
     (let ((ekp-first-line-indent 4))
       (should-not (ekp-layout-plan-append plan plain 24)))
     (let ((ekp-alignment 'center))
       (should-not (ekp-layout-plan-append plan plain 24)))
     (let ((ekp-latin-lang "de_DE"))
       (should-not (ekp-layout-plan-append plan plain 24))))))

(ert-deftest ekp-test-layout-plan-omits-zero-source-zero-width-gaps ()
  "The projection plan must omit gaps that cannot install a property."
  (ekp-tests--with-clean-state
   (let ((plan (ekp-layout-plan
                "中文 mixed paragraph keeps natural gaps off the hot path."
                480)))
     (cl-loop
      for line across (ekp-layout-plan-lines plan)
      do
      (cl-loop
       for gap across (ekp-layout-line-gaps line)
       do
       (should
        (or (< (ekp-layout-gap-source-start gap)
               (ekp-layout-gap-source-end gap))
            (> (ekp-layout-gap-target-pixel gap) 0))))))))

(ert-deftest ekp-test-layout-plan-records-discretionary-hyphen ()
  "A chosen Latin discretionary break must be explicit in the core plan."
  (ekp-tests--with-clean-state
   (let* ((plan (ekp-layout-plan
                 "extraordinary hyphenation demonstration paragraph" 16))
          (line (seq-find
                 (lambda (candidate)
                   (eq (ekp-layout-line-break-kind candidate) 'hyphen))
                 (append (ekp-layout-plan-lines plan) nil))))
     (should line)
     (should (ekp-layout-line-hyphen-p line))
     (should (= (ekp-layout-line-break-source-start line)
                (ekp-layout-line-break-source-end line))))))

(ert-deftest ekp-test-public-string-renderer-consumes-layout-plan ()
  "The public formatter must render the shared semantic plan."
  (ekp-tests--with-clean-state
   (let ((calls 0)
         (original (symbol-function 'ekp-layout-plan)))
     (cl-letf (((symbol-function 'ekp-layout-plan)
                (lambda (string width)
                  (cl-incf calls)
                  (funcall original string width))))
       (should (stringp
                (ekp-pixel-justify
                 "Shared plans keep the string and buffer renderers aligned"
                 24)))
       (should (> calls 0))))))

(ert-deftest ekp-test-gaps-between-brute-force ()
  "`ekp--gaps-between' must equal naive counting."
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (let* ((s "中文 Latin 混排 test words 测试")
          (para (ekp--get-para s))
          (n (length (ekp-para-boxes para)))
          (glues (ekp-para-glues-types para)))
     (dotimes (i n)
       (cl-loop for k from (1+ i) to n do
         (let ((expected
                (list (cl-loop for j from (1+ i) below k
                               count (eq (aref glues j) 'lws))
                      (cl-loop for j from (1+ i) below k
                               count (eq (aref glues j) 'mws))
                      (cl-loop for j from (1+ i) below k
                               count (eq (aref glues j) 'cws)))))
           (should (equal (ekp--gaps-between para i k) expected))))))))

;;;; Text properties

(ert-deftest ekp-test-properties-preserved ()
  (ekp-tests--with-clean-state
   (let* ((s (propertize "styled text keeps faces across justification"
                         'face '(:foreground "cyan")))
          (out (ekp-pixel-justify s 60))
          (pos (string-match "styled" out)))
     (should pos)
     (should (equal (get-text-property pos 'face out)
                    '(:foreground "cyan"))))))

(ert-deftest ekp-test-hyphen-inherits-properties ()
  "Inserted hyphens carry the face of the word they break."
  (ekp-tests--with-clean-state
   (ekp-param-set 3 1 1 2 1 1 0 2 0)
   (let* ((s (propertize "extraordinary hyphenation demonstration paragraph"
                         'face 'italic))
          (out (ekp-pixel-justify s 20)))
     (when-let* ((pos (cl-position ?- out)))
       (should (equal (get-text-property pos 'face out) 'italic))))))

;;;; Range justify

(ert-deftest ekp-test-range-justify ()
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (let* ((s "The quick brown fox jumps over the lazy dog and keeps running along")
          (res (ekp-pixel-range-justify s 50 80))
          (w (cdr res)))
     (should (<= 50 w 80))
     (ekp-clear-caches)
     (should (equal (car res) (ekp-pixel-justify s w))))))

;;;; C module parity

(ert-deftest ekp-test-c-parity-simple ()
  (skip-unless (ekp-tests--c-available))
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (dolist (s '("The quick brown fox jumps over the lazy dog and keeps running through the broadcast system"
                "中文排版是一门艺术,需要考虑标点悬挂、避头尾等规则,同时兼顾 Latin 混排的美观。"
                "中文 Latin 混排 test 保留 space 的情况 with spaces"))
     (dolist (w '(30 40 60 100 200))
       (setq ekp-use-c-module nil)
       (ekp-clear-caches)
       (let ((el (ekp-pixel-justify s w)))
         (setq ekp-use-c-module t)
         (ekp-clear-caches)
         (should (equal el (ekp-pixel-justify s w))))))))

(ert-deftest ekp-test-c-parity-files ()
  "Full parity on the bundled sample texts (exercises the batch API)."
  (skip-unless (ekp-tests--c-available))
  (ekp-tests--with-clean-state
   (ekp-param-set 5 2 1 4 2 1 0 3 0)
   (dolist (f '("text-zh.txt" "text-en_US.txt" "text-zh-en_US.txt"))
     (let ((s (ekp-tests--file-content f)))
       (dolist (w '(100 250))
         (setq ekp-use-c-module nil)
         (ekp-clear-caches)
         (let ((el (ekp-pixel-justify s w)))
           (setq ekp-use-c-module t)
           (ekp-clear-caches)
           (should (equal el (ekp-pixel-justify s w)))))))))

(ert-deftest ekp-test-c-fallback-when-disabled ()
  "`ekp-use-c-module' nil forces the Elisp engine even when loaded."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil))
     (should (stringp (ekp-pixel-justify "plain elisp path works" 60))))))

(ert-deftest ekp-test-c-module-errors-surface ()
  "An enabled C backend error must not be silently converted to Elisp."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module t)
         (ekp-c-module-loaded t))
     (cl-letf (((symbol-function 'ekp-c-break-with-arrays)
                (lambda (&rest _)
                  (error "forced C backend failure"))))
       (should-error (ekp-pixel-justify
                      "backend errors are observable at the public boundary"
                      60)
                     :type 'error)))))

;;;; Cache correctness (M3 wave)

(ert-deftest ekp-test-dp-cache-looseness-isolation ()
  "Results cached at one looseness must not serve another.
Regression: after justifying at looseness 0, changing `ekp-looseness'
returned the stale looseness-0 layout for the same (string, width)."
  (ekp-clear-caches)
  (let* ((s "aaa bbb ccc ddd eee fff ggg hhh iii jjj")
         (r0 (ekp-pixel-justify s 12))
         (r1 (let ((ekp-looseness 1)) (ekp-pixel-justify s 12))))
    (ekp-clear-caches)
    (let ((f1 (let ((ekp-looseness 1)) (ekp-pixel-justify s 12)))
          (f0 (ekp-pixel-justify s 12)))
      (should (equal-including-properties r1 f1))
      (should (equal-including-properties r0 f0))
      ;; and the two targets genuinely differ on this input
      (should-not (equal r0 r1)))))

(ert-deftest ekp-test-dp-cache-algorithm-parameter-isolation ()
  "Every algorithm parameter must participate in the DP cache key."
  (let* ((text-a (concat
                  "hyphenation representation configuration extraordinary "
                  "internationalization approximation characterization"))
         (text-b (concat
                  "The quick brown fox jumps over the lazy dog and keeps "
                  "running through the emergency broadcast system test of "
                  "hyphenation quality"))
         (cases `((ekp-line-penalty 0 1000 8 ,text-a)
                  (ekp-hyphen-penalty 0 1000000 8 ,text-a)
                  (ekp-adjacent-fitness-penalty 0 1000000 8 ,text-b)
                  (ekp-consecutive-hyphen-penalty 0 1000000 8 ,text-b)
                  (ekp-last-line-short-penalty 0 1000000 15 ,text-b)
                  (ekp-last-line-min-ratio 0.1 0.99 8 ,text-a))))
    (dolist (case cases)
      (apply #'ekp-tests--assert-dp-cache-parameter-isolated case))))

(ert-deftest ekp-test-final-pass-artificial-demerits-preserve-last-active-path ()
  "The final pass must not lose its last active path at an overfull atom."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (atom (propertize "internationalization atom" 'ekp-no-break t))
          (text (concat "x " atom " y"))
          (para (ekp--get-para text))
          (boxes (ekp-para-boxes para))
          (n (length boxes))
          (atom-end
           (cl-loop for i from 0 below n
                    when (get-text-property 0 'ekp-no-break (aref boxes i))
                    maximize (1+ i)))
          (strict (ekp--dp-run-1d para 12 nil))
          (final (ekp--dp-run-1d para 12 t)))
     (should-not strict)
     (should final)
     (should (equal (plist-get final :breaks) (list atom-end n)))
     (should (= (aref (aref (plist-get final :state) 1) atom-end) 0.0))
     (ekp-tests--assert-complete-plan (ekp-layout-plan text 12)))))

(ert-deftest ekp-test-emergency-stretch-pixel-controls-final-pass-fitness ()
  "Final-pass fitness must use the configured emergency stretch budget."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (ekp-emergency-stretch-pixel 1)
          (text "alpha beta gamma delta epsilon")
          (width 12)
          (para (ekp--get-para text))
          (strict (ekp--dp-run-1d para width nil))
          (dp (ekp-dp-cache text width))
          (first-break (car (plist-get dp :breaks)))
          (first-rest (car (plist-get dp :rests)))
          (first-gaps (car (plist-get dp :gaps)))
          (params (ekp-para-glue-params para))
          (stretch (+ (* (nth 0 first-gaps)
                         (plist-get params :lws-stretch))
                      (* (nth 1 first-gaps)
                         (plist-get params :mws-stretch))
                      (* (nth 2 first-gaps)
                         (plist-get params :cws-stretch))
                      (or (plist-get params :extra-stretch) 0)
                      ekp-emergency-stretch-pixel))
          (expected (ekp--compute-fitness-class first-rest stretch))
          (actual (aref (aref (plist-get dp :state) 5) first-break)))
     (should-not strict)
     (should (> first-rest 0))
     (should (= expected 3))
     (should (= actual expected)))))

(ert-deftest ekp-test-emergency-stretch-pixel-controls-parshape-cost ()
  "Parshape final-pass cost must use the configured emergency stretch budget."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (ekp-emergency-stretch-pixel 1)
          (ekp-parshape (list (cons 0 12) (cons 0 50)))
          (ekp-line-penalty 0)
          (ekp-adjacent-fitness-penalty 0)
          (ekp-hyphen-penalty 0)
          (ekp-consecutive-hyphen-penalty 0)
          (ekp-last-line-min-ratio 0)
          (text "alpha beta gamma delta epsilon")
          (para (ekp--get-para text))
          (strict (ekp--dp-run-loose para 12 nil))
          (dp (ekp-dp-cache text 12))
          (first-rest (car (plist-get dp :rests)))
          (first-gaps (car (plist-get dp :gaps)))
          (params (ekp-para-glue-params para))
          (stretch (+ (* (nth 0 first-gaps)
                         (plist-get params :lws-stretch))
                      (* (nth 1 first-gaps)
                         (plist-get params :mws-stretch))
                      (* (nth 2 first-gaps)
                         (plist-get params :cws-stretch))
                      (or (plist-get params :extra-stretch) 0)
                      ekp-emergency-stretch-pixel))
          (badness (ekp--compute-badness first-rest stretch))
          (expected-cost (expt badness 2)))
     (should-not strict)
     (should (equal (plist-get dp :breaks) '(3 7)))
     (should (> first-rest 0))
     (should (< (abs (- (plist-get dp :cost) expected-cost)) 0.0001)))))

(ert-deftest ekp-test-dp-cache-identical-signature-hits ()
  "Structurally equal DP signatures must reuse the same cached result."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (ekp-looseness 1)
          (text "aaa bbb ccc ddd eee fff ggg hhh iii jjj")
          (para (ekp--get-para text))
          (first (ekp-dp-cache text 12))
          (count (hash-table-count (ekp-para-dp-cache para)))
          (second (ekp-dp-cache text 12)))
     (should (eq first second))
     (should (= count (hash-table-count (ekp-para-dp-cache para)))))))

(ert-deftest ekp-test-para-key-ignores-fontified ()
  "Fontification bookkeeping must not split the paragraph cache."
  (let* ((plain "fontified 键检查内容")
         (marked (propertize plain 'fontified t))
         (faced (propertize plain 'face 'bold))
         (faced+marked (propertize plain 'face 'bold 'fontified t))
         (faced+split (propertize plain 'face 'bold))
         (faced+boundary (copy-sequence faced)))
    (put-text-property 3 7 'fontified t faced+split)
    (put-text-property 7 10 'jit-lock-defer-multiline t faced+split)
    (put-text-property 3 7 'face 'italic faced+boundary)
    (should (equal (ekp--para-key plain) (ekp--para-key marked)))
    (should (equal (ekp--para-key faced) (ekp--para-key faced+marked)))
    (should (equal (ekp--para-key faced) (ekp--para-key faced+split)))
    (should-not (equal (ekp--para-key faced) (ekp--para-key faced+boundary)))
    (should-not (equal (ekp--para-key plain) (ekp--para-key faced)))))

(ert-deftest ekp-test-auto-spacing-signature-keys-para-cache ()
  "Auto spacing inputs must participate in paragraph cache identity."
  (ekp-tests--with-clean-state
   (let* ((text "自动字距段落缓存签名")
          (first (let ((ekp-default-cws-stretch-pixel 2))
                   (ekp--get-para text))))
     (setq ekp--last-para nil)
     (let ((changed (let ((ekp-default-cws-stretch-pixel 9))
                      (ekp--get-para text))))
       (should (= 2 (plist-get (ekp-para-glue-params first) :cws-stretch)))
       (should (= 9 (plist-get (ekp-para-glue-params changed) :cws-stretch)))
       (should-not (eq first changed))))))

(ert-deftest ekp-test-auto-spacing-signature-keys-last-para ()
  "Auto spacing inputs must invalidate the same-string fast path."
  (ekp-tests--with-clean-state
   (let* ((text "自动字距最近段落快路径")
          (first (let ((ekp-default-cws-stretch-pixel 2))
                   (ekp--get-para text)))
          (changed (let ((ekp-default-cws-stretch-pixel 9))
                     (ekp--get-para text))))
     (should (= 9 (plist-get (ekp-para-glue-params changed) :cws-stretch)))
     (should-not (eq first changed)))))

(ert-deftest ekp-test-auto-spacing-identical-signature-hits ()
  "Identical auto spacing signatures must reuse paragraph data."
  (ekp-tests--with-clean-state
   (let* ((ekp-default-cws-stretch-pixel 3)
          (text "自动字距签名相同应命中缓存")
          (first (ekp--get-para text))
          (count (hash-table-count ekp--para-cache)))
     (setq ekp--last-para nil)
     (let ((table-hit (ekp--get-para text)))
       (should (eq first table-hit))
       (should (= count (hash-table-count ekp--para-cache)))
       (should (eq table-hit (ekp--get-para text)))))))

(ert-deftest ekp-test-para-cache-ignores-width-when-policy-semantics-match ()
  "Paragraph cache identity is semantic, not raw policy-measure keyed."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil)
         (text (copy-sequence "alpha  beta gamma delta"))
         (analysis-count 0)
         (make-count 0)
         (analyze-policies (symbol-function 'ekp--analyze-policies))
         (make-para (symbol-function 'ekp--make-para)))
     (put-text-property 0 (length text)
                        'ekp--face-break-policy 'no-hyphen text)
     (cl-letf (((symbol-function 'ekp--analyze-policies)
                (lambda (string)
                  (setq analysis-count (1+ analysis-count))
                  (funcall analyze-policies string)))
               ((symbol-function 'ekp--make-para)
                (lambda (string &optional policy-analysis)
                  (setq make-count (1+ make-count))
                  (funcall make-para string policy-analysis))))
       (let* ((narrow (ekp-layout-plan text 12))
              (narrow-intervals
               (ekp-para-resolved-policies (ekp-layout-plan-para narrow)))
              (wide (ekp-layout-plan text 30))
              (wide-intervals
               (ekp-para-resolved-policies (ekp-layout-plan-para wide))))
         (should (equal narrow-intervals wide-intervals))
         (should (= analysis-count 1))
         (should (eq (ekp-layout-plan-para narrow)
                     (ekp-layout-plan-para wide)))
         (should (= make-count 1)))))))

(ert-deftest ekp-test-para-cache-distinguishes-automatic-no-break-downgrade ()
  "A real automatic no-break threshold change keeps distinct paragraph data."
  (cl-labels
      ((break-policy-present-p (para policy)
         (seq-some
          (lambda (interval)
            (eq (ekp--policy-interval-break-policy interval) policy))
          (ekp-para-resolved-policies para)))
       (assert-order (first-width second-width rigid-width)
         (ekp-tests--with-clean-state
          (let ((ekp-use-c-module nil)
                (text (copy-sequence "alpha beta gamma")))
            (put-text-property 0 (length text)
                               'ekp--face-break-policy 'no-break text)
            (let* ((first (ekp-layout-plan text first-width))
                   (first-para (ekp-layout-plan-para first))
                   (second (ekp-layout-plan text second-width))
                   (second-para (ekp-layout-plan-para second))
                   (rigid-para (if (= first-width rigid-width)
                                   first-para
                                 second-para))
                   (downgraded-para (if (= first-width rigid-width)
                                        second-para
                                      first-para)))
              (should-not (eq first-para second-para))
              (should-not (equal (ekp-para-resolved-policies first-para)
                                 (ekp-para-resolved-policies second-para)))
              (should (break-policy-present-p rigid-para 'no-break))
              (should (break-policy-present-p downgraded-para 'no-hyphen)))))))
    (assert-order 100 8 100)
    (assert-order 8 100 100)))

(ert-deftest ekp-test-policy-cache-invalidates-on-input-and-config-change ()
  "Policy analysis cache keys must track raw inputs and public config."
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "processKeyword42 policy cache"))
          (width 100)
          (plain (ekp-layout-plan text width)))
     (put-text-property 0 (length "processKeyword42")
                        'ekp--face-break-policy 'no-hyphen text)
     (let ((face-policy (ekp-layout-plan text width)))
       (should-not (eq (ekp-layout-plan-para plain)
                       (ekp-layout-plan-para face-policy)))
       (should (seq-some
                (lambda (interval)
                  (eq (ekp--policy-interval-break-policy interval)
                      'no-hyphen))
                (ekp-para-resolved-policies
                 (ekp-layout-plan-para face-policy)))))))
  (ekp-tests--with-clean-state
   (let* ((text (copy-sequence "processKeyword42 policy cache"))
          (width 100)
          (normal (let ((ekp-token-break-policies nil))
                    (ekp-layout-plan text width)))
          (configured (let ((ekp-token-break-policies
                             '((identifier . no-break))))
                        (ekp-layout-plan text width))))
     (should-not (eq (ekp-layout-plan-para normal)
                     (ekp-layout-plan-para configured)))
     (should-not (equal (ekp-para-resolved-policies
                         (ekp-layout-plan-para normal))
                        (ekp-para-resolved-policies
                         (ekp-layout-plan-para configured)))))))

(ert-deftest ekp-test-layout-plan-cache-reuses-semantic-plan-for-same-key ()
  "Repeated layout for the same semantic paragraph and width reuses assembly."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil)
         (text (copy-sequence "alpha beta gamma delta epsilon"))
         (assembly-count 0)
         (assemble-plan (symbol-function 'ekp--layout-plan-from-para)))
     (cl-letf (((symbol-function 'ekp--layout-plan-from-para)
                (lambda (&rest args)
                  (setq assembly-count (1+ assembly-count))
                  (apply assemble-plan args))))
       (let ((first (ekp-layout-plan text 20)))
         (should (= assembly-count 1))
         (let ((second (ekp-layout-plan text 20)))
           (should (eq (ekp-layout-plan-para first)
                       (ekp-layout-plan-para second)))
           (should (equal (mapcar #'ekp-layout-line-signature
                                  (append (ekp-layout-plan-lines first) nil))
                          (mapcar #'ekp-layout-line-signature
                                  (append (ekp-layout-plan-lines second)
                                          nil))))
           (should (= assembly-count 1))))))))

(ert-deftest ekp-test-layout-plan-cache-keeps-distinct-semantic-keys ()
  "Plan reuse must not alias different widths, policy input, or config."
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil)
         (text (copy-sequence "alpha beta gamma delta epsilon"))
         (assembly-count 0)
         (assemble-plan (symbol-function 'ekp--layout-plan-from-para)))
     (cl-letf (((symbol-function 'ekp--layout-plan-from-para)
                (lambda (&rest args)
                  (setq assembly-count (1+ assembly-count))
                  (apply assemble-plan args))))
       (ekp-layout-plan text 20)
       (ekp-layout-plan text 30)
       (should (= assembly-count 2)))))
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil)
         (text (copy-sequence "alpha beta gamma delta epsilon"))
         (assembly-count 0)
         (assemble-plan (symbol-function 'ekp--layout-plan-from-para)))
     (cl-letf (((symbol-function 'ekp--layout-plan-from-para)
                (lambda (&rest args)
                  (setq assembly-count (1+ assembly-count))
                  (apply assemble-plan args))))
       (let ((plain (ekp-layout-plan text 20)))
         (put-text-property 0 (length text)
                            'ekp--face-break-policy 'no-hyphen text)
         (let ((policy (ekp-layout-plan text 20)))
           (should (= assembly-count 2))
           (should-not (eq (ekp-layout-plan-para plain)
                           (ekp-layout-plan-para policy))))))))
  (ekp-tests--with-clean-state
   (let ((ekp-use-c-module nil)
         (text (copy-sequence "processKeyword42 plan cache"))
         (assembly-count 0)
         (assemble-plan (symbol-function 'ekp--layout-plan-from-para)))
     (cl-letf (((symbol-function 'ekp--layout-plan-from-para)
                (lambda (&rest args)
                  (setq assembly-count (1+ assembly-count))
                  (apply assemble-plan args))))
       (let ((normal (let ((ekp-token-break-policies nil))
                       (ekp-layout-plan text 100)))
             (configured (let ((ekp-token-break-policies
                                '((identifier . no-break))))
                           (ekp-layout-plan text 100))))
         (should (= assembly-count 2))
         (should-not (eq (ekp-layout-plan-para normal)
                         (ekp-layout-plan-para configured))))))))

(ert-deftest ekp-test-layout-plan-cache-does-not-expose-stored-plan ()
  "Cached semantic plans must be returned as mutation-isolated values."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (text (copy-sequence "alpha beta gamma delta epsilon"))
          (width 20)
          (plan1 (ekp-layout-plan text width))
          (source (substring-no-properties text))
          (signatures (mapcar #'ekp-layout-line-signature
                              (append (ekp-layout-plan-lines plan1) nil))))
     (setf (ekp-layout-plan-string plan1) "mutated cached plan")
     (let ((plan2 (ekp-layout-plan text width)))
       (should-not (eq plan1 plan2))
       (should (equal (ekp-layout-plan-string plan2) source))
       (should (equal (mapcar #'ekp-layout-line-signature
                              (append (ekp-layout-plan-lines plan2) nil))
                      signatures))))))

(ert-deftest ekp-test-layout-plan-cache-deep-copies-nested-plan-objects ()
  "Cached semantic plans must own mutable consumer-facing plan slots."
  (ekp-tests--with-clean-state
   (cl-labels
       ((line-signatures (plan)
          (mapcar #'ekp-layout-line-signature
                  (append (ekp-layout-plan-lines plan) nil)))
        (gap-signatures (plan)
          (cl-loop
           for line across (ekp-layout-plan-lines plan)
           append (cl-loop
                   for gap across (ekp-layout-line-gaps line)
                   collect (list (ekp-layout-gap-kind gap)
                                 (ekp-layout-gap-left-box gap)
                                 (ekp-layout-gap-right-box gap)
                                 (ekp-layout-gap-source-start gap)
                                 (ekp-layout-gap-source-end gap)
                                 (ekp-layout-gap-natural-pixel gap)
                                 (ekp-layout-gap-target-pixel gap)))))
        (snapshot (plan)
          (list :string (substring-no-properties
                         (ekp-layout-plan-string plan))
                :context (copy-tree (ekp-layout-plan-context plan))
                :boxes (mapcar #'substring-no-properties
                               (append (ekp-layout-plan-boxes plan) nil))
                :offsets (mapcar (lambda (offset)
                                   (cons (car offset) (cdr offset)))
                                 (append (ekp-layout-plan-offsets plan) nil))
                :lines (line-signatures plan)
                :gaps (gap-signatures plan)))
        (assert-pristine (text width expected)
          (let ((again (ekp-layout-plan text width)))
            (should (equal (snapshot again) expected))))
        (first-line-with-gap (plan)
          (seq-find (lambda (candidate)
                      (and (> (length (ekp-layout-line-glues candidate)) 0)
                           (> (length (ekp-layout-line-gaps candidate)) 0)))
                    (append (ekp-layout-plan-lines plan) nil))))
     (let* ((ekp-use-c-module nil)
            (text (copy-sequence
                   "alpha中文beta混排gamma段落"))
            (width 20))
       (put-text-property 0 (length text)
                          'ekp--face-break-policy 'no-hyphen text)
       (let* ((baseline (ekp-layout-plan text width))
              (expected (snapshot baseline)))
         (should (> (length (ekp-layout-plan-lines baseline)) 1))
         (should (first-line-with-gap baseline))
         (let ((plan (ekp-layout-plan text width)))
           (aset (ekp-layout-plan-string plan) 0 ?X))
         (assert-pristine text width expected)
         (let ((plan (ekp-layout-plan text width)))
           (setcar (ekp-layout-plan-context plan) 'mutated-context))
         (assert-pristine text width expected)
         (let ((plan (ekp-layout-plan text width)))
           (aset (ekp-layout-plan-boxes plan) 0 "mutated-box"))
         (assert-pristine text width expected)
         (let* ((plan (ekp-layout-plan text width))
                (box (aref (ekp-layout-plan-boxes plan) 0)))
           (aset box 0 ?X))
         (assert-pristine text width expected)
         (let ((plan (ekp-layout-plan text width)))
           (setcar (aref (ekp-layout-plan-offsets plan) 0) 9999))
         (assert-pristine text width expected)
         (let ((plan (ekp-layout-plan text width)))
           (aset (ekp-layout-plan-lines plan) 0 nil))
         (assert-pristine text width expected)
         (let* ((plan (ekp-layout-plan text width))
                (line (aref (ekp-layout-plan-lines plan) 0)))
           (setf (ekp-layout-line-source-start line) 9999)
           (setf (ekp-layout-line-signature line) '(mutated line struct)))
         (assert-pristine text width expected)
         (let* ((plan (ekp-layout-plan text width))
                (line (first-line-with-gap plan)))
           (aset (ekp-layout-line-glues line) 0 9999)
           (aset (ekp-layout-line-gaps line) 0 nil))
         (assert-pristine text width expected)
         (let* ((plan (ekp-layout-plan text width))
                (line (first-line-with-gap plan))
                (gap (aref (ekp-layout-line-gaps line) 0)))
           (setf (ekp-layout-gap-target-pixel gap) 9999))
         (assert-pristine text width expected))))))

(ert-deftest ekp-test-layout-plan-cache-context-owns-policy-strings ()
  "Plan context copies must not alias mutable public policy inputs."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (suffix (copy-sequence "uX"))
          (ekp-number-unit-suffixes (list suffix))
          (ekp-token-break-policies '((number-unit . no-break)))
          (text (copy-sequence "100uX alpha beta gamma delta"))
          (width 16)
          (assembly-count 0)
          (assemble-plan (symbol-function 'ekp--layout-plan-from-para)))
     (cl-letf (((symbol-function 'ekp--layout-plan-from-para)
                (lambda (&rest args)
                  (setq assembly-count (1+ assembly-count))
                  (apply assemble-plan args))))
       (let* ((plan (ekp-layout-plan text width))
              (context-suffix
               (seq-find (lambda (candidate)
                           (equal candidate suffix))
                         (ekp-tests--strings-in-tree
                          (ekp-layout-plan-context plan)))))
         (should (= assembly-count 1))
         (should context-suffix)
         (aset context-suffix 0 ?v)
         (should (equal suffix "uX"))
         (let ((again (ekp-layout-plan text width)))
           (should (= assembly-count 1))
           (should
            (seq-some (lambda (candidate)
                        (equal candidate suffix))
                      (ekp-tests--strings-in-tree
                       (ekp-layout-plan-context again))))))))))

(ert-deftest ekp-test-policy-analysis-cache-key-owns-policy-strings ()
  "Policy-analysis cache keys must own mutable public policy strings."
  (ekp-tests--with-clean-state
   (let* ((ekp-use-c-module nil)
          (suffix (copy-sequence "uX"))
          (line-start-extra (copy-sequence "《"))
          (ekp-number-unit-suffixes (list suffix))
          (ekp-token-break-policies '((number-unit . no-break)))
          (ekp-kinsoku-profile 'custom)
          (ekp-cjk-no-line-start-extra line-start-extra)
          (text (copy-sequence "100uX alpha beta gamma delta"))
          (width 16)
          cache-key first-plan)
     (setq first-plan (ekp-layout-plan text width))
     (should
      (seq-some
       (lambda (interval)
         (and (eq (ekp--policy-interval-category interval) 'number-unit)
              (eq (ekp--policy-interval-break-policy interval) 'no-break)))
       (ekp-para-resolved-policies (ekp-layout-plan-para first-plan))))
     (maphash (lambda (key _value)
                (setq cache-key key))
              ekp--policy-analysis-cache)
     (should cache-key)
     (should (seq-some (lambda (string) (equal string "uX"))
                       (ekp-tests--strings-in-tree cache-key)))
     (should (seq-some (lambda (string) (equal string "《"))
                       (ekp-tests--strings-in-tree cache-key)))
     (store-substring suffix 1 "Y")
     (store-substring line-start-extra 0 "》")
     (should (seq-some (lambda (string) (equal string "uX"))
                       (ekp-tests--strings-in-tree cache-key)))
     (should (seq-some (lambda (string) (equal string "《"))
                       (ekp-tests--strings-in-tree cache-key)))
     (let ((changed-plan (ekp-layout-plan text width)))
       (should-not
        (seq-some
         (lambda (interval)
           (and (eq (ekp--policy-interval-category interval) 'number-unit)
                (eq (ekp--policy-interval-break-policy interval) 'no-break)))
         (ekp-para-resolved-policies
          (ekp-layout-plan-para changed-plan))))))))

(ert-deftest ekp-test-global-width-cache-consistent ()
  "The global width cache returns exactly `string-pixel-width'."
  (ekp-clear-caches)
  (dolist (s (list "中" "word" " " (propertize "中" 'face 'bold)))
    (should (= (ekp--measured-width s) (string-pixel-width s)))
    ;; second lookup: cached, same value
    (should (= (ekp--measured-width s) (string-pixel-width s)))))

(ert-deftest ekp-test-style-change-invalidates-fast-path ()
  "Changing a style variable must invalidate the same-string fast path.
Regression: with the same string object, (setq ekp-alignment ...)
kept returning the paragraph resolved under the previous style."
  (ekp-clear-caches)
  (let* ((s "style watcher 检查内容足够长断行几次的样子啊")
         (out-j (ekp-pixel-justify s 30))
         (out-c (let ((ekp-alignment 'center)) (ekp-pixel-justify s 30))))
    (ekp-clear-caches)
    (should (equal-including-properties
             out-c
             (let ((ekp-alignment 'center)) (ekp-pixel-justify s 30))))
    (ekp-clear-caches)
    (should (equal-including-properties out-j (ekp-pixel-justify s 30)))))

;;;; Typography quality (M3 wave)

(ert-deftest ekp-test-hyphenmin-honored ()
  "Dictionary LEFTHYPHENMIN/RIGHTHYPHENMIN are parsed and applied.
en_US declares 2/3; the old hardcoded 2/2 allowed \"quick-ly\"."
  (let ((h (ekp-hyphen-create "en_US")))
    (should (= (ekp-hyphen-left h) 2))
    (should (= (ekp-hyphen-right h) 3))
    (dolist (w '("quickly" "mainly" "activity" "hyphenation" "reader"))
      (dolist (p (ekp-hyphen-positions h w))
        (should (>= p 2))
        (should (<= p (- (length w) 3)))))
    ;; explicit overrides still work, partial override keeps the
    ;; dictionary's value for the other side
    (let ((h2 (ekp-hyphen-create "en_US" nil 1 1))
          (h3 (ekp-hyphen-create "en_US" nil 4 nil)))
      (should (= (ekp-hyphen-left h2) 1))
      (should (= (ekp-hyphen-right h2) 1))
      (should (= (ekp-hyphen-left h3) 4))
      (should (= (ekp-hyphen-right h3) 3)))))

(ert-deftest ekp-test-jis-kinsoku-line-start ()
  "Small kana and the prolonged sound mark never start a line.
JIS X 4051 line-start prohibition for っゃー々 etc."
  (ekp-tests--with-clean-state
   (let ((text "がっこうへいくよラーメンをたべたいなあそうかなぁいいなぁと")
         (forbidden (append ekp-cjk-no-line-start-extra nil)))
     (dolist (w '(20 28 40 60))
       (let ((out (ekp-pixel-justify text w)))
         (dolist (line (split-string out "\n"))
           (when (> (length line) 0)
             (should-not (memq (aref line 0) forbidden)))))))))

(ert-deftest ekp-test-first-line-indent-1d-matches-parshape ()
  "A plain first-line indent equals the equivalent parshape.
The indent runs on the 1D DP (and the C engine); parshape runs on
the (position × line-count) Elisp DP — they must agree."
  (ekp-tests--with-clean-state
   (let* ((s "首行缩进等价性检查内容足够长会断行几次的样子哦")
          (w 30)
          (via-indent (let ((ekp-first-line-indent 8))
                        (ekp-pixel-justify s w)))
          (via-parshape (progn
                          (ekp-clear-caches)
                          (let ((ekp-parshape (list (cons 8 (- w 8))
                                                    (cons 0 w))))
                            (ekp-pixel-justify s w)))))
     (should (equal-including-properties via-indent via-parshape)))))

(ert-deftest ekp-test-first-line-indent-c-parity ()
  "First-line indent: C and Elisp engines agree byte-for-byte."
  (skip-unless (ekp-tests--c-available))
  (ekp-tests--with-clean-state
   (dolist (indent '(t 8))
     (let ((ekp-first-line-indent indent))
       (dolist (s '("中文首行缩进检查内容足够长会断行几次的样子哦"
                    "Mixed 混排 first line indent parity with words"))
         (dolist (w '(30 60 90))
           (let* ((via-c (let ((ekp-use-c-module t))
                           (ekp-pixel-justify s w)))
                  (_ (ekp-clear-caches))
                  (via-el (let ((ekp-use-c-module nil))
                            (ekp-pixel-justify s w))))
             (ekp-clear-caches)
             (should (equal-including-properties via-c via-el)))))))))

;;;; Display-context measurement (M5 wave)

(ert-deftest ekp-test-width-context-keys-caches ()
  "Buffers with face remappings must never share cached paragraphs.
`text-scale-mode' and theme tweaks live in `face-remapping-alist';
glyphs render at different sizes there, so paragraph data measured
in one context is wrong in another."
  (ekp-clear-caches)
  (let ((s "上下文键控检查内容足够长断行"))
    (should-not (equal (ekp--para-key s)
                       (let ((face-remapping-alist
                              '((default :height 1.5))))
                         (ekp--para-key s))))
    ;; the same-string fast path must not leak across contexts either
    (ekp-clear-caches)
    (let* ((p1 (ekp--get-para s))
           (p2 (let ((face-remapping-alist '((default :height 1.5))))
                 (ekp--get-para s))))
      (should-not (eq p1 p2)))
    (ekp-clear-caches)))

(ert-deftest ekp-test-width-context-measurement-cached-separately ()
  "The width cache keeps remapped and plain measurements apart."
  (ekp-clear-caches)
  (let* ((s "宽")
         (plain (ekp--measured-width s))
         (remapped (let ((face-remapping-alist '((default :height 2.0))))
                     (ekp--measured-width s))))
    ;; In batch both degrade to columns (equal values); the point is
    ;; that neither call poisons the other's cache entry.
    (should (= plain (ekp--measured-width s)))
    (should (= remapped
               (let ((face-remapping-alist '((default :height 2.0))))
                 (ekp--measured-width s))))
    (ekp-clear-caches)))

(provide 'ekp-tests)

;;; ekp-tests.el ends here
