;;; ekp-fuzz.el --- property-based stress test for ekp -*- lexical-binding: t; -*-

(require 'ekp)
(require 'cl-lib)

(ekp-c-module-load)
(unless ekp-c-module-loaded (error "C module required for parity fuzz"))

(defvar fuzz--seed 42)
(defun fuzz--rand (n)  ; deterministic LCG so failures are reproducible
  (setq fuzz--seed (mod (+ (* fuzz--seed 1103515245) 12345) 2147483648))
  (mod fuzz--seed n))

(defconst fuzz--cjk "中文排版是门艺术需要考虑标点悬挂避头尾规则同时兼顾美观")
(defconst fuzz--words '("the" "quick" "hyphenation" "emergency" "extraordinary"
                        "a" "of" "supercalifragilisticexpialidocious"
                        "bcdfghjklmnpqrstvwxz" "word!" "(paren)" "don't"
                        "test," "end." "«quoted»" "naïve" "Ｆｕｌｌ" "１２３"))
(defconst fuzz--puncts '("，" "。" "、" "《" "》" "「" "」" "！" "？"))

(defun fuzz--gen-string ()
  "Random mixed paragraph of 5-60 tokens."
  (let ((n (+ 5 (fuzz--rand 56))) (parts nil))
    (dotimes (_ n)
      (pcase (fuzz--rand 10)
        ;; latin word
        ((or 0 1 2 3) (push (nth (fuzz--rand (length fuzz--words)) fuzz--words) parts)
                      (push " " parts))
        ;; CJK run
        ((or 4 5 6 7) (let ((len (1+ (fuzz--rand 6)))
                            (start (fuzz--rand (- (length fuzz--cjk) 7))))
                        (push (substring fuzz--cjk start (+ start len)) parts)))
        ;; CJK punct
        (8 (push (nth (fuzz--rand (length fuzz--puncts)) fuzz--puncts) parts))
        ;; spaces / zwsp
        (9 (push (if (= 0 (fuzz--rand 3)) "​" "  ") parts))))
    (string-trim (apply #'concat (nreverse parts)))))

(defun fuzz--content (s)
  (replace-regexp-in-string "[ \t\n​-]+" "" (substring-no-properties s)))

(ekp-param-set 5 2 1 4 2 1 0 3 0)
(let ((cases 300) (fails 0))
  (dotimes (i cases)
    (let* ((s (fuzz--gen-string))
           (w (+ 1 (fuzz--rand 300))))
      (unless (string-blank-p s)
        (condition-case err
            (let (el cr)
              (setq ekp-use-c-module nil)
              (ekp-clear-caches)
              (setq el (ekp-pixel-justify s w))
              (setq ekp-use-c-module t)
              (ekp-clear-caches)
              (setq cr (ekp-pixel-justify s w))
              ;; ① parity
              (unless (equal el cr)
                (cl-incf fails)
                (message "PARITY FAIL #%d w=%d s=%S" i w s))
              ;; ② content preservation
              (unless (equal (fuzz--content el) (fuzz--content s))
                (cl-incf fails)
                (message "CONTENT FAIL #%d w=%d s=%S" i w s))
              ;; ③ finite cost
              (unless (numberp (ekp-total-cost s w))
                (cl-incf fails)
                (message "COST FAIL #%d w=%d" i w)))
          (error (cl-incf fails)
                 (message "ERROR #%d w=%d s=%S err=%S" i w s err))))))
  (message "fuzz done: %d cases, %d failures" cases fails)
  (kill-emacs (if (> fails 0) 1 0)))
