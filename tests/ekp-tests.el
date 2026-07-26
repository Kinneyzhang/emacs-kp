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

(defvar ekp-tests--defaults
  (list 10 50 100 100 50 0.5 0)
  "Default values of the tunable K-P variables (see fixture).")

(defmacro ekp-tests--with-clean-state (&rest body)
  "Run BODY with fresh caches and restore all tunables afterwards."
  `(unwind-protect
       (progn
         (ekp-clear-caches)
         (ekp-param-reset)
         ,@body)
     (cl-destructuring-bind (lp hp afp chp llsp llmr loose)
         ekp-tests--defaults
       (setq ekp-line-penalty lp
             ekp-hyphen-penalty hp
             ekp-adjacent-fitness-penalty afp
             ekp-consecutive-hyphen-penalty chp
             ekp-last-line-short-penalty llsp
             ekp-last-line-min-ratio llmr
             ekp-looseness loose))
     (ekp-param-reset)
     (ekp-clear-caches)))

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

(defun ekp-tests--file (name)
  (expand-file-name name (expand-file-name "tests" (ekp-root-dir))))

(defun ekp-tests--file-content (name)
  (with-temp-buffer
    (insert-file-contents (ekp-tests--file name))
    (buffer-string)))

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
  (should-error (ekp-hyphen-create "zz_XX")))

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
  "Per-line widths force the Elisp 2D path."
  (let ((ekp-first-line-indent 6))
    (should-not (ekp--c-available-p))))

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
          (n (length (ekp--boxes s))))
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
           (let ((b-en (copy-sequence (ekp--boxes s))))
             (setq ekp-latin-lang "de_DE")
             ;; Same string object, no cache clear: must re-hyphenate.
             (let ((b-de (ekp--boxes s)))
               (should-not (equal b-en b-de))
               ;; And it must equal a fresh computation.
               (ekp-clear-caches)
               (should (equal b-de (ekp--boxes s))))))
       (setq ekp-latin-lang old)))))

(ert-deftest ekp-test-dp-cache-reuse ()
  (ekp-tests--with-clean-state
   (let* ((s "cached paragraph text here")
          (r1 (ekp-dp-cache s 60))
          (r2 (ekp-dp-cache s 60)))
     (should (eq r1 r2)))))

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

(ert-deftest ekp-test-para-key-ignores-fontified ()
  "Fontification bookkeeping must not split the paragraph cache."
  (let* ((plain "fontified 键检查内容")
         (marked (propertize plain 'fontified t))
         (faced (propertize plain 'face 'bold))
         (faced+marked (propertize plain 'face 'bold 'fontified t)))
    (should (equal (ekp--para-key plain) (ekp--para-key marked)))
    (should (equal (ekp--para-key faced) (ekp--para-key faced+marked)))
    (should-not (equal (ekp--para-key plain) (ekp--para-key faced)))))

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
