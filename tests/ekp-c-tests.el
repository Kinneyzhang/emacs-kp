;;; ekp-c-tests.el --- Public API acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Test supported package inputs, outputs, errors and lifecycle.
;; Internal implementation details are outside this contract.

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'ekp)

(ert-deftest ekp-c-test-module-build-rejects-unknown-profile ()
  "Unknown build profiles fail before starting a process."
  (should-error (ekp-c-module-build 'fast-maybe)
                :type 'user-error))

(provide 'ekp-c-tests)
;;; ekp-c-tests.el ends here
