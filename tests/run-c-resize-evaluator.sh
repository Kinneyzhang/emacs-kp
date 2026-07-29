#!/bin/sh
# Evaluate task032 against the frozen portable baseline.

set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
GOAL="$ROOT/.omx/goals/performance/c-resize-latency"
BASELINE_ROOT="$GOAL/baseline/source"
RAW="$GOAL/raw"
ROUNDS=${EKP_RESIZE_ROUNDS:-4}

if test -n "${EMACS:-}"; then
  EMACS_BIN=$EMACS
elif command -v emacs >/dev/null 2>&1; then
  EMACS_BIN=$(command -v emacs)
elif test -x /Applications/Emacs.app/Contents/MacOS/Emacs-arm64-11; then
  EMACS_BIN=/Applications/Emacs.app/Contents/MacOS/Emacs-arm64-11
else
  printf '%s\n' "resize-evaluator: Emacs executable not found" >&2
  exit 2
fi

test -f "$BASELINE_ROOT/ekp.el"
test -f "$BASELINE_ROOT/ekp-buffer.el"
test -f "$BASELINE_ROOT/ekp_c/ekp.dylib"

make -C "$ROOT/ekp_c" clean all PROFILE=portable
mkdir -p "$RAW"
BASELINE_JSONL="$RAW/baseline.jsonl"
CANDIDATE_JSONL="$RAW/candidate.jsonl"
REPORT="$GOAL/latest-report.json"
: >"$BASELINE_JSONL"
: >"$CANDIDATE_JSONL"

run_round()
{
  code_root=$1
  label=$2
  round=$3
  output=$4
  EKP_RESIZE_LABEL=$label \
  EKP_RESIZE_ROUND=$round \
  EKP_RESIZE_OUTPUT=$output \
    "$EMACS_BIN" -Q --batch -L "$code_root" -L "$ROOT/tests" \
      -l "$ROOT/tests/ekp-c-resize-evaluator.el"
}

round=1
while test "$round" -le "$ROUNDS"; do
  if test $((round % 2)) -eq 1; then
    run_round "$BASELINE_ROOT" baseline "$round" "$BASELINE_JSONL"
    run_round "$ROOT" candidate "$round" "$CANDIDATE_JSONL"
  else
    run_round "$ROOT" candidate "$round" "$CANDIDATE_JSONL"
    run_round "$BASELINE_ROOT" baseline "$round" "$BASELINE_JSONL"
  fi
  round=$((round + 1))
done

EKP_RESIZE_MODE=compare \
EKP_RESIZE_BASELINE_JSONL="$BASELINE_JSONL" \
EKP_RESIZE_CANDIDATE_JSONL="$CANDIDATE_JSONL" \
EKP_RESIZE_REPORT="$REPORT" \
  "$EMACS_BIN" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/ekp-c-resize-evaluator.el"

"$ROOT/tests/run-tests.sh" "$EMACS_BIN"
"$EMACS_BIN" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
  -l "$ROOT/tests/ekp-fuzz.el"
"$ROOT/tests/check-release.sh"

printf '%s\n' "resize-evaluator: performance, parity, ERT, fuzz, and release gates pass"
