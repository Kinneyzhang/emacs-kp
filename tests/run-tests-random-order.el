;;; run-tests-random-order.el --- Run EKP ERT tests in permuted order -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded after the EKP test files by tests/run-tests.sh --random-order.
;; EKP_TEST_SEED selects a reproducible permutation.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defun ekp-tests--permuted-names (names seed)
  "Return NAMES in a deterministic permutation selected by SEED."
  (let ((items (vconcat names))
        (state (logand seed #x7fffffff)))
    (dotimes (index (max 0 (1- (length items))))
      (setq state (mod (+ (* state 1103515245) 12345) #x80000000))
      (let ((other (+ index (mod state (- (length items) index)))))
        (cl-rotatef (aref items index) (aref items other))))
    (append items nil)))

;;;###autoload
(defun ekp-tests-run-random-order ()
  "Run every EKP ERT test in a reproducibly permuted order."
  (interactive)
  (let* ((seed (string-to-number (or (getenv "EKP_TEST_SEED") "20260728")))
         (tests
          (ert-select-tests "^ekp-\\(?:test\\|[[:alnum:]-]+-test\\)-" t))
         (names (mapcar #'ert-test-name tests))
         (selector
          (cons 'member (ekp-tests--permuted-names names seed))))
    (message "EKP ERT permutation seed: %d" seed)
    (ert-run-tests-batch-and-exit selector)))

(provide 'run-tests-random-order)
;;; run-tests-random-order.el ends here
