#!/bin/sh
# Run every EKP ERT test in a fresh Emacs process.
# Usage: tests/run-tests-isolated.sh [path-to-emacs]

set -eu

EMACS="${1:-${EMACS:-emacs}}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

TESTS=$(
  "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/ekp-tests.el" \
    -l "$ROOT/tests/ekp-buffer-tests.el" \
    -l "$ROOT/tests/ekp-gui-tests.el" \
    -l "$ROOT/tests/ekp-c-tests.el" \
    --eval '(dolist (test (ert-select-tests "^ekp-\\(?:test\\|[[:alnum:]-]+-test\\)-" t))
              (princ (format "%s\n" (ert-test-name test))))'
)

for test_name in $TESTS; do
  echo "isolated ERT: $test_name"
  "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/ekp-tests.el" \
    -l "$ROOT/tests/ekp-buffer-tests.el" \
    -l "$ROOT/tests/ekp-gui-tests.el" \
    -l "$ROOT/tests/ekp-c-tests.el" \
    --eval "(ert-run-tests-batch-and-exit '$test_name)"
done
