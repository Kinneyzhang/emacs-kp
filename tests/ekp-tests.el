;;; ekp-tests.el --- Public API acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Test supported package inputs, outputs, errors and lifecycle.
;; Internal implementation details are outside this contract.

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'ekp)

(defvar ekp-inline-code-policy nil)

(defvar ekp-hyphenation nil)

(defvar ekp-token-break-policies nil)

(defvar ekp-number-unit-suffixes nil)

(defvar ekp-kinsoku-profile nil)

(defvar ekp-overlong-token-policy nil)

(ert-deftest ekp-test-hyphen-lang-fallback ()
  "Short language codes resolve to a dictionary."
  (should (ekp-hyphen-create "en"))
  (dolist (locale '("zz_XX" "zz-XX"))
    (should-error (ekp-hyphen-create locale)
                  :type 'ekp-hyphen-dictionary-not-found)))

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

(provide 'ekp-tests)
;;; ekp-tests.el ends here
