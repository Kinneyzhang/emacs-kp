#!/bin/sh
# Run the EKP test suite in batch mode.
# Usage: tests/run-tests.sh [path-to-emacs]

EMACS="${1:-${EMACS:-emacs}}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

exec "$EMACS" -Q --batch -L "$ROOT" \
     -l "$ROOT/tests/ekp-tests.el" \
     -l "$ROOT/tests/ekp-region-tests.el" \
     -f ert-run-tests-batch-and-exit
