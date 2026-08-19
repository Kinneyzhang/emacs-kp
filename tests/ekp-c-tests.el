;;; ekp-c-tests.el --- ERT tests for the EKP C boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Direct dynamic-module contract tests.  They skip when no compatible module
;; is available and otherwise exercise malformed data before the DP core.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ekp)

(declare-function ekp-c-break-batch "ext:ekp")
(declare-function ekp-c-break-with-arrays "ext:ekp")
(declare-function ekp-c-set-penalties "ext:ekp")

(defun ekp-c-tests--available ()
  "Return non-nil when a compatible C module can be loaded."
  (or ekp-c-module-loaded
      (progn
        (condition-case nil
            (ekp-c-module-load)
          (error nil))
        ekp-c-module-loaded)))

(defun ekp-c-tests--valid-args ()
  "Return one valid 15-field direct C API argument list."
  (list [0 10] [0 10] [0 10]
        [0] [0] [0] [] 0 10
        [0 0] [0 0] [] [0 0] 0 10))

(defun ekp-c-tests--position-args ()
  "Return valid direct C arguments with several position slots."
  (list [0 10 20 30] [0 10 20 30] [0 10 20 30]
        [0 0 0] [0 0 0] [0 0 0] [0 2] 0 15
        [0 0 0 0] [0 0 0 0] [1 3] [0 0 0 0] 0 15))

(ert-deftest ekp-c-test-rejects-non-vector-schema-field ()
  "Schema errors signal the module's explicit input condition."
  (skip-unless (ekp-c-tests--available))
  (let ((args (ekp-c-tests--valid-args)))
    (setcar args 'not-a-vector)
    (should-error (apply #'ekp-c-break-with-arrays args)
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-rejects-mismatched-vector-length ()
  "All n and n+1 vector lengths are checked before extraction."
  (skip-unless (ekp-c-tests--available))
  (let ((args (ekp-c-tests--valid-args)))
    (setf (nth 1 args) [0])
    (should-error (apply #'ekp-c-break-with-arrays args)
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-rejects-out-of-range-integer ()
  "Pixel integers outside signed 32-bit input range are rejected."
  (skip-unless (ekp-c-tests--available))
  (let ((args (ekp-c-tests--valid-args)))
    (setf (nth 8 args) 2147483648)
    (should-error (apply #'ekp-c-break-with-arrays args)
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-rejects-non-integer-vector-value ()
  "Vector values that are not integers use the explicit input condition."
  (skip-unless (ekp-c-tests--available))
  (let ((args (ekp-c-tests--valid-args)))
    (setf (nth 3 args) ["not-an-integer"])
    (should-error (apply #'ekp-c-break-with-arrays args)
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-rejects-invalid-break-position-vectors ()
  "Position vectors must be in range, strictly sorted, and unique."
  (skip-unless (ekp-c-tests--available))
  (dolist (case '((6 [-1]) (6 [3]) (6 [2 1]) (6 [1 1])
                  (11 [0]) (11 [4]) (11 [2 1]) (11 [2 2])))
    (let ((args (ekp-c-tests--position-args)))
      (setf (nth (car case) args) (cadr case))
      (should-error (apply #'ekp-c-break-with-arrays args)
                    :type 'ekp-c-invalid-input))))

(ert-deftest ekp-c-test-accepts-position-boundaries ()
  "Valid edge positions preserve the direct API result contract."
  (skip-unless (ekp-c-tests--available))
  (let* ((args (ekp-c-tests--position-args))
         (result (apply #'ekp-c-break-with-arrays args)))
    (should (consp result))
    (should (listp (car result)))
    (should (numberp (cdr result)))))

(ert-deftest ekp-c-test-batch-rejects-invalid-position-vectors ()
  "Batch preflight applies the same position contract to every paragraph."
  (skip-unless (ekp-c-tests--available))
  (let ((args (ekp-c-tests--position-args)))
    (setf (nth 11 args) [0])
    (should-error (ekp-c-break-batch (vector (vconcat args)))
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-batch-rejects-short-paragraph-vector ()
  "Batch preflight validates each paragraph before indexing 15 fields."
  (skip-unless (ekp-c-tests--available))
  (let ((paragraph (vconcat (butlast (ekp-c-tests--valid-args)))))
    (should-error (ekp-c-break-batch (vector paragraph))
                  :type 'ekp-c-invalid-input)))

(ert-deftest ekp-c-test-wide-intermediates-avoid-int32-overflow ()
  "Valid int32 inputs use wider intermediates in line arithmetic."
  (skip-unless (ekp-c-tests--available))
  (let* ((limit 2147483647)
         (args (list (vector 0 limit) (vector 0 limit) (vector 0 limit)
                     [0] [0] [0] [] 0 limit
                     [0 0] [0 0] [] (vector 0 limit) 0 limit))
         (result (apply #'ekp-c-break-with-arrays args)))
    (should (equal (car result) '(1)))
    (should (= (cdr result) 100200100.0))))

(ert-deftest ekp-c-test-penalties-use-explicit-input-condition ()
  "Penalty setters reject bad scalar types before extracting values."
  (skip-unless (ekp-c-tests--available))
  (should-error (ekp-c-set-penalties "10" 50 100 0.5)
                :type 'ekp-c-invalid-input))

(ert-deftest ekp-c-test-penalties-accept-emergency-stretch-eighth-param ()
  "The direct C setter accepts a non-negative emergency stretch parameter."
  (skip-unless (ekp-c-tests--available))
  (should (ekp-c-set-penalties 10 50 100 0.5 100 50 0 7)))

(ert-deftest ekp-c-test-penalties-reject-invalid-emergency-stretch ()
  "Invalid emergency stretch inputs use the module input condition."
  (skip-unless (ekp-c-tests--available))
  (should-error (ekp-c-set-penalties 10 50 100 0.5 100 50 0 -1)
                :type 'ekp-c-invalid-input)
  (should-error (ekp-c-set-penalties 10 50 100 0.5 100 50 0 "wide")
                :type 'ekp-c-invalid-input))

(ert-deftest ekp-c-test-penalties-keep-legacy-four-arg-call ()
  "The direct C setter remains compatible with the legacy 4-arg call."
  (skip-unless (ekp-c-tests--available))
  (should (ekp-c-set-penalties 10 50 100 0.5)))

(ert-deftest ekp-c-test-module-build-uses-argv-in-directory ()
  "Interactive builds must not interpolate a module path into a shell."
  (let (process-arguments process-directory)
    (cl-letf (((symbol-function 'ekp-c-module-dir)
               (lambda () "/tmp/EKP build with spaces"))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/make"))
              ((symbol-function 'make-process)
               (lambda (&rest arguments)
                 (setq process-arguments arguments
                       process-directory default-directory)
                 'ekp-test-process))
              ((symbol-function 'start-process)
               (lambda (&rest _) 'legacy-process))
              ((symbol-function 'set-process-sentinel) #'ignore))
      (should (eq (ekp-c-module-build 'portable) 'ekp-test-process))
      (should (equal (plist-get process-arguments :command)
                     '("/usr/bin/make" "PROFILE=portable")))
      (should (equal process-directory
                     "/tmp/EKP build with spaces/")))))

(ert-deftest ekp-c-test-module-build-rejects-unknown-profile ()
  "Unknown build profiles fail before starting a process."
  (should-error (ekp-c-module-build 'fast-maybe)
                :type 'user-error))

(provide 'ekp-c-tests)
;;; ekp-c-tests.el ends here
