;;; ekp-bench.el --- Benchmarks for EKP -*- lexical-binding: t; -*-

;;; Commentary:

;; Performance benchmarks over the bundled sample texts.  Run:
;;
;;   # Pure Elisp engine
;;   emacs -Q --batch -L . --eval '(setq ekp-use-c-module nil)' \
;;         -l tests/ekp-bench.el
;;
;;   # C module engine (build ekp_c first)
;;   emacs -Q --batch -L . \
;;         --eval '(progn (require (quote ekp)) (ekp-c-module-load))' \
;;         -l tests/ekp-bench.el
;;
;; In batch mode widths are measured in character columns, so the
;; numbers are engine-comparable but not identical to GUI timings.

;;; Code:

(require 'ekp)

(defun ekp-bench--read (name)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (expand-file-name "tests" (ekp-root-dir))))
    (buffer-string)))

(defun ekp-bench-run (label thunk &optional n)
  "Run THUNK N times (cold caches); report the fastest run."
  (let ((n (or n 3)) (times nil))
    (dotimes (_ n)
      (ekp-clear-caches)
      (garbage-collect)
      (let ((t0 (float-time)))
        (funcall thunk)
        (push (- (float-time) t0) times)))
    (message "%-42s %8.1f ms  (min of %d)"
             label (* 1000 (apply #'min times)) n)))

(let* ((zh (ekp-bench--read "text-zh.txt"))
       (en (ekp-bench--read "text-en_US.txt"))
       (mix (ekp-bench--read "text-zh-en_US.txt"))
       (zh3 (string-join (list zh zh zh) "\n")))
  (message "== engine: %s ==" (if (and (boundp 'ekp-c-module-loaded)
                                       ekp-c-module-loaded
                                       ekp-use-c-module)
                                  "C" "elisp"))
  (ekp-bench-run "justify zh    w=200" (lambda () (ekp-pixel-justify zh 200)))
  (ekp-bench-run "justify zh    w=400" (lambda () (ekp-pixel-justify zh 400)))
  (ekp-bench-run "justify en    w=200" (lambda () (ekp-pixel-justify en 200)))
  (ekp-bench-run "justify en    w=400" (lambda () (ekp-pixel-justify en 400)))
  (ekp-bench-run "justify mix   w=300" (lambda () (ekp-pixel-justify mix 300)))
  (ekp-bench-run "justify zh3   w=400" (lambda () (ekp-pixel-justify zh3 400)))
  (ekp-bench-run "range zh      340-380"
                 (lambda () (ekp-pixel-range-justify zh 340 380)))
  (ekp-bench-run "range mix     280-320"
                 (lambda () (ekp-pixel-range-justify mix 280 320)))
  (ekp-bench-run "para-create zh (all lines)"
                 (lambda () (dolist (s (split-string zh "\n"))
                              (unless (string-blank-p s) (ekp--get-para s)))))
  ;; DP only: paragraphs pre-tokenized, fresh DP each round
  (let ((paras (cl-remove-if #'string-blank-p (split-string zh "\n")))
        (times nil))
    (dolist (s paras) (ekp--get-para s))
    (dotimes (_ 3)
      (dolist (s paras)
        (clrhash (ekp-para-dp-cache (ekp--get-para s))))
      (garbage-collect)
      (let ((t0 (float-time)))
        (dolist (s paras) (ekp-dp-cache s 400))
        (push (- (float-time) t0) times)))
    (message "%-42s %8.1f ms  (min of 3)" "DP-only zh w=400 (paras cached)"
             (* 1000 (apply #'min times)))))

(message "bench done")

(provide 'ekp-bench)

;;; ekp-bench.el ends here
