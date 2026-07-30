#!/bin/sh
# Evaluate task030 against the frozen portable baseline.

set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
GOAL="$ROOT/.omx/goals/performance/narrow-live-commit"
BASELINE_ROOT="$GOAL/baseline/source"
RAW="$GOAL/raw"
ROUNDS=${EKP_LIVE_COMMIT_ROUNDS:-4}

if test -n "${EMACS:-}"; then
  EMACS_BIN=$EMACS
elif command -v emacs >/dev/null 2>&1; then
  EMACS_BIN=$(command -v emacs)
elif test -x /Applications/Emacs.app/Contents/MacOS/Emacs-arm64-11; then
  EMACS_BIN=/Applications/Emacs.app/Contents/MacOS/Emacs-arm64-11
else
  printf '%s\n' "live-commit-evaluator: Emacs executable not found" >&2
  exit 2
fi

test -f "$BASELINE_ROOT/ekp.el"
test -f "$BASELINE_ROOT/ekp-buffer.el"
test -f "$BASELINE_ROOT/ekp_c/ekp.dylib"
test -f "$BASELINE_ROOT/dictionaries/hyph_en_US.dic"

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
  EKP_LIVE_COMMIT_LABEL=$label \
  EKP_LIVE_COMMIT_ROUND=$round \
  EKP_LIVE_COMMIT_OUTPUT=$output \
    "$EMACS_BIN" -Q --batch -L "$code_root" -L "$ROOT/tests" \
      -l "$ROOT/tests/ekp-live-commit-evaluator.el"
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

EKP_LIVE_COMMIT_MODE=compare \
EKP_LIVE_COMMIT_BASELINE_JSONL="$BASELINE_JSONL" \
EKP_LIVE_COMMIT_CANDIDATE_JSONL="$CANDIDATE_JSONL" \
EKP_LIVE_COMMIT_REPORT="$REPORT" \
  "$EMACS_BIN" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
    -l "$ROOT/tests/ekp-live-commit-evaluator.el"

if test "${EKP_LIVE_COMMIT_SKIP_AUDIT:-0}" = 1; then
  printf '%s\n' "live-commit-evaluator: performance and parity gates pass"
  exit 0
fi

"$ROOT/tests/run-tests.sh" "$EMACS_BIN"
"$EMACS_BIN" -Q --batch -L "$ROOT" -L "$ROOT/tests" \
  -l "$ROOT/tests/ekp-fuzz.el"
"$ROOT/tests/check-release.sh"

printf '%s\n' \
  "live-commit-evaluator: performance, parity, ERT, fuzz, and release gates pass"
