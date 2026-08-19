# Plan: Post-Audit Hardening 2026-08-20

## Context

The repository audit after `183f256` found runtime and gate defects that are
independent of the already accepted text-property layout architecture. This
plan stays inside `phase-kp-overhaul-20260726`; it does not reopen closed
issues or weaken the source-clean display contract.

## Goals

1. Make public buffer width errors fail before projection mutation and keep
   the previous projection intact on every rejected request.
2. Define and test the backward-delete live projection invariant before
   changing the row-crossing owner.
3. Resolve locale spellings to the exact bundled dictionary before short-code
   fallback, and reject malformed C position vectors at the module boundary.
4. Restore the showcase's automatic inline-code contract and make every test
   runner use current source and the complete ERT set.
5. Remove manual buffer integrations when no projection or auto-mode owner
   remains.
6. Re-profile `issue018`/`task030` after the correctness fixes with a
   source-fresh evaluator and close it only if the locked source-instrumented
   16 ms contract is actually met.

## Scope and milestones

- M1 (`task037`, `task042`): buffer width validation, failure atomicity, and
  integration lifecycle.
- M2 (`task038`): backward-delete projection semantics and a regression that
  drives the public edit path.
- M3 (`task039`, `task040`): exact hyphenation locale lookup and complete C
  position-vector validation, with direct boundary tests.
- M4 (`task041`): showcase automatic inline face, source-first test loading,
  and complete random/isolated selection.
- M5 (`task030`, `issue028`): make the locked width/engine matrix source-fresh,
  then optimize only the remaining structural commit owner.

## Non-goals

- Do not change valid KP output, the 15-field C ABI, or the source-clean
  buffer representation.
- Do not hide latency with debounce, stale layout reuse, skipped publication,
  global GC changes, or test-only exceptions.
- Do not close `issue018` from byte-compiled production numbers alone.
- Do not rewrite historical postmortems; append current decisions and
  verification evidence.

## Acceptance gates

- Invalid or non-positive buffer widths signal before any owned property,
  span, filter, hook, point, mark, or modified-state change.
- Backward deletion either preserves the committed projection as specified or
  has an explicit revised contract documented and tested through the public
  command path.
- `de-CH`/`de_CH` and equivalent exact locale spellings select the same exact
  dictionary; unknown locales use the documented short-code fallback.
- Out-of-range, unsorted, or duplicate C hyphen/forbidden positions signal
  `ekp-c-invalid-input`; valid arrays retain exact Elisp/C parity.
- Clean source, stale bytecode, normal, random, and isolated test runners
  all exercise the same complete current ERT inventory and the showcase's
  automatic inline contract.
- The live evaluator explicitly loads source from both baseline and candidate
  roots; a bytecode-backed `pass:true` is not valid evidence.
- Full ERT, fuzz, C build/tests, byte compilation, checkdoc, package-lint,
  release, dictionary, and focused performance gates pass before closure.

## Risks and dependencies

- Backward-delete behavior is a product invariant, not a local condition;
  implementation must follow the existing spec and real public-path evidence.
- C position validation must match the DP's exclusive break-index domain and
  preserve sorted-vector assumptions used by binary search.
- The performance gate depends on trustworthy source loading; stale ignored
  `.elc` files must not influence any result.

## Stop condition

Stop only when all tasks in this plan have current issue/task evidence,
closed issues have user-visible or contract-level verification, `issue018`
is either genuinely closed by its locked gate or explicitly remains open with
fresh measurements, and the complete diff is committed and reviewed.
