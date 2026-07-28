# Plan: Final Cleanup and Review Gate 2026-07-28

## Goal

Remove residual low-signal code from the repository-remediation diff, prove
every documented acceptance criterion, and obtain independent code and
architecture approval.

## Behavior Lock

- Full ERT: 129/129.
- Seeded permuted ERT: 129/129.
- Every pre-owner ERT passed in a fresh Emacs process.
- C/Elisp fuzz: 300/300.
- Exact round-trip, public protection, C parity, save failure, and release
  boundary regressions are present.

## Cleanup Scope

All changed runtime/test/build files, with edits limited to a proven smell.
Documentation is reviewed for drift, not rewritten cosmetically.

## Smells and Order

1. **Fallback gate:** classify every fallback-like path before editing.
2. **Dead code:** remove the unused paragraph accessor stack and route its
   few test/render call sites directly to the owning `ekp-para`.
3. **Boundary scan:** replace direct use of dependency-private dispatch
   internals with the public dispatcher while preserving prior-filter and
   DELETE lifecycle semantics.
4. **Duplication/naming:** retain direct code where extraction would add
   glue; make no speculative refactor.
5. **Tests:** add nothing unless cleanup exposes an unprotected behavior.

## Fallback Inventory

- Short-language dictionary resolution: grounded compatibility behavior;
  deterministic and covered by `ekp-test-hyphen-lang-fallback`.
- Batch/tty font fallback: grounded external-display boundary; batch tests
  and GUI matrix cover fallback and primary environments.
- Interactive C build/load catch: grounded outer command boundary; retains
  build buffer and reports the error.
- Missing optional dictionary: grounded optional-resource fallback; only
  not-found is caught, unsupported syntax propagates.
- C nil/allocation/no-result and thread-pool sequential paths: grounded
  fail-safe/performance-boundary fallbacks; semantic errors propagate and
  C/Elisp parity/fuzz cover equivalence.
- Test/fuzz catches: assertion harness boundaries, not production defaults.

No masking fallback, broad compatibility shim, swallowed business-logic
error, or escalation candidate was found.

## Cleanup Result

- Removed nine unused one-line paragraph accessors and routed the remaining
  callers to the owning `ekp-para`.
- Removed the direct call to Emacs's private `buffer-substring--filter`.
  EKP now temporarily binds the prior filter and delegates through public
  `filter-buffer-substring`.
- The first boundary repair exposed a DELETE lifecycle ordering bug in the
  focused test: cleanup ran while the temporary prior-filter binding was
  active. The final implementation inhibits cleanup during dispatch, unwinds
  the binding, then performs ownership cleanup against the real slot.
- Direct private-API scan is clean. No new abstraction, file, dependency,
  compatibility shim, or silent fallback was introduced.

## Quality and Review Gate

Run full/default/permuted/isolated ERT, fuzz, portable/native/debug/sanitize
C builds, warning-as-error Elisp compilation, checkdoc, pinned
package-lint, shell/YAML/release/dictionary/static checks, GUI matrix, full
diff review, private-boundary scan, then independent `code-reviewer`
APPROVE and `architect` CLEAR.

Current post-fix evidence: default and seeded-permuted ERT 130/130; every
one of 130 ERT tests in a fresh Emacs process; C/Elisp fuzz 300/300;
warning-clean Elisp/C builds; checkdoc and pinned package-lint; release,
dictionary, shell/YAML/static checks; and live GUI matrix 7/7 with a clean
single-window screenshot. Independent code review covered 98 current paths
with zero findings and returned `APPROVE`; the independent architecture
invariant review returned `CLEAR`. `task015` is closed.

## Stop Condition

All issue/task records are closed, both independent review lanes are clean,
all gates pass from the final source state, and no remote publish/tag/push
action has been performed.
