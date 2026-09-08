# issue029 — Emacs 31.1 strict compilation rejects obsolete let macros

Status: resolved

## Environment

- GNU Emacs 31.1 development build `fac653279dcb`
- macOS Apple Silicon
- EKP HEAD `97cb6a6e3506`

## Reproduction

Run the repository CI byte-compilation command with
`byte-compile-error-on-warn` set to `t` against the four production files.

## Expected vs actual

- Expected: production and maintained test tools compile with warnings treated
  as errors on the recorded Emacs executable.
- Actual: Emacs 31.1 rejects six single-binding `if-let`/`when-let` forms in
  production; strict changed-tool compilation also finds three such forms in
  `tests/ekp-live-commit-evaluator.el`.

## Investigation and fix

The macros became obsolete in Emacs 31.1. Every affected form has exactly one
binding, so the starred replacement has identical binding, truth, body, and
fallback behavior and needs no compatibility branch. Replace only those nine
macro names; do not alter layout or evaluator data flow.

## Verification

`task044` passed root integration compilation; WERROR production and changed
evaluator compilation; normal and seed-20260901 random ERT 296/296; warning-clean
C build; 300-case C/Elisp parity fuzz; release invariants; local 49-entry
dictionary manifest/hash; checkdoc; and diff-check. The network-backed
fixed-upstream dictionary fetch was stopped and excluded at the user's explicit
direction; no upstream-provenance claim is made.

Resolved At: 2026-09-01

Resolved By: C1a nine-repository verification

Commit: pending (protected pre-existing dirty worktree)
