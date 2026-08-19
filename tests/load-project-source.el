;;; load-project-source.el --- Load EKP test subjects from source -*- lexical-binding: t; -*-

;;; Commentary:

;; Test bootstrap that prevents ignored or stale byte-code files from deciding
;; which implementation the suite exercises.

;;; Code:

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (file '("ekp-utils.el"
                  "ekp-hyphen.el"
                  "ekp.el"
                  "ekp-buffer.el"
                  "tests/ekp-showcase.el"
                  "tests/ekp-gui-verify.el"
                  "tests/ekp-tests.el"
                  "tests/ekp-buffer-tests.el"
                  "tests/ekp-gui-tests.el"
                  "tests/ekp-c-tests.el"
                  "tests/run-tests-random-order.el"))
    (load-file (expand-file-name file root))))

(provide 'load-project-source)
;;; load-project-source.el ends here
