#!/bin/sh
# Run every EKP ERT test in a fresh Emacs process.
# Usage: tests/run-tests-isolated.sh [path-to-emacs]

set -eu

EMACS="${1:-${EMACS:-emacs}}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

TESTS=$(
  "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/load-project-source.el" \
    --eval '(dolist (test (ert-select-tests t t))
              (princ (format "%s\n" (ert-test-name test))))'
)

for test_name in $TESTS; do
  echo "isolated ERT: $test_name"
  "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/load-project-source.el" \
    --eval "(ert-run-tests-batch-and-exit '$test_name)"
done
