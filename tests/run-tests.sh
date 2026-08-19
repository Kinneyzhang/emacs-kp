#!/bin/sh
# Run the EKP test suite in batch mode.
# Usage: tests/run-tests.sh [path-to-emacs] [--random-order]

EMACS="${1:-${EMACS:-emacs}}"
ORDER="${2:-}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

case "$ORDER" in
  "")
    exec "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
         -l "$ROOT/tests/load-project-source.el" \
         -f ert-run-tests-batch-and-exit
    ;;
  --random-order)
    exec "$EMACS" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
         -l "$ROOT/tests/load-project-source.el" \
         -f ekp-tests-run-random-order
    ;;
  *)
    echo "usage: $0 [path-to-emacs] [--random-order]" >&2
    exit 2
    ;;
esac
