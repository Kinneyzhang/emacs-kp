;;; ekp-gui-tests.el --- ERT tests for EKP GUI verification -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe tests for the GUI verifier's reporting boundary.  Real pixel
;; measurements remain in the interactive matrix.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ekp-gui-verify)

(ert-deftest ekp-gui-test-noninteractive-failure-exits-one ()
  "A failed matrix result must terminate batch automation with status 1."
  (let ((noninteractive t)
        exit-status
        output)
    (cl-letf (((symbol-function 'princ)
               (lambda (text &optional _stream)
                 (setq output (concat output text))))
              ((symbol-function 'kill-emacs)
               (lambda (&optional status)
                 (setq exit-status status))))
      (ekp-gui-verify--report
       '((:name "forced failure" :body 100 :target 100 :widest 101
                :over 1 :lines 3 :pass nil)))
      (should (= exit-status 1))
      (should (string-match-p "forced failure.*FAIL" output)))))

(ert-deftest ekp-gui-test-noninteractive-success-stays-zero ()
  "A passing matrix result must not request batch termination."
  (let ((noninteractive t)
        exit-status)
    (cl-letf (((symbol-function 'princ) #'ignore)
              ((symbol-function 'kill-emacs)
               (lambda (&optional status)
                 (setq exit-status status))))
      (ekp-gui-verify--report
       '((:name "passing" :body 100 :target 100 :widest 100
                :over 0 :lines 3 :pass t)))
      (should-not exit-status))))

(provide 'ekp-gui-tests)
;;; ekp-gui-tests.el ends here
