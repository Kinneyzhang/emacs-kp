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
    (should (equal (ekp-hyphen-boxes h "emergency")
                   '("emer" "gen" "cy")))
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

(ert-deftest ekp-test-split-cjk-punct-attaches ()
  "Closing CJK punctuation attaches to the preceding char (kinsoku)."
  (let ((boxes (append (ekp-split-to-boxes "中文，排版。") nil)))
    (should (member "文，" boxes))
    (should (member "版。" boxes))))

(ert-deftest ekp-test-split-cjk-opening-punct-holds ()
  "Opening CJK punctuation attaches to the following char (kinsoku)."
  (let ((boxes (append (ekp-split-to-boxes "看《中文》吧") nil)))
    (should (member "《中" boxes))))

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

(provide 'ekp-tests)

;;; ekp-tests.el ends here
