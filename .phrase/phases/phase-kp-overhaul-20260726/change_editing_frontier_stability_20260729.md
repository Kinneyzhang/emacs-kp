# Change: Editing-Frontier Stability 2026-07-29

## 2026-07-29 — Implement and developer-verify task029

- **Modify** `ekp-buffer.el`.
  - Adds a source-relative `frontier` marker to live state.
  - Derives the active semantic line from the latest real edit, relocates
    it in `after-change`, and preserves it through width/font/context
    reflow.
  - Deletes point-driven live-boundary publication. Point motion inside
    the active hard line is display read-only; leaving the hard line keeps
    the existing static completion transition.
  - Keeps core DP semantics, `ekp.el`, C ABI, schema, plan contract, source
    characters, and no-overlay ownership unchanged.
- **Modify** `tests/ekp-buffer-tests.el`,
  `tests/ekp-gui-verify.el`, and
  `tests/ekp-buffer-live-bench.el`.
  - Locks exact property/state/source/undo identity and zero
    plan/cache/property writes across backward/forward point motion.
  - Proves reflow and deferred IME completion preserve the source-edit
    frontier even when point moves elsewhere.
  - Updates GUI and benchmark adapters to assert projection, plan,
    generation, cache, active-index, and frontier stability.
- **Modify** bilingual user/developer docs, spec, plan, technical
  reference, issues, changelog, and superseding postmortem to use the same
  source-edit-frontier vocabulary.
- **Verification:** focused regressions RED before implementation and
  GREEN afterward; independent focused ERT 10/10; buffer ERT 93/93;
  default, seed-`20260729`, and isolated ERT 182/182; C/Elisp fuzz 300/300;
  warning-as-error compilation, checkdoc, pinned package-lint, release,
  dictionary, pinned-source, no-overlay, no-stale-symbol, and diff gates
  pass. Point-motion p99 is 0.033 ms on C and 0.037 ms in the independent
  Elisp review, both with zero planner/cache calls.
- **GUI evidence:** reviewed run
  `/private/tmp/ekp-frontier-live-v3-66WYRW` contains 39 manifest lines and
  a 26.6-second recording. All checkpoints and assertions pass, no black
  segment is detected, and the final evidence verdict is PASS.
- **Review:** independent architecture review is CLEAR. Independent code
  review reports zero blockers; the one residual performance concern is
  separately owned by `issue018`/`task030`.
- **Status:** task029 developer gate complete. `issue016` and `issue017`
  remain open until the user personally confirms the visible editing
  experience.

## 2026-07-29 — Record the independent append-performance debt

- **Add** `issue018` and `task030` after the current checked-in 80-pixel
  benchmark failed to reproduce task028's 6.399 ms append p99.
- **Add** `postmortem/20260729-narrow-live-append-replanning.md` to keep
  unique-source-state planning cost separate from task029's point-motion
  correctness fix.
- **Evidence:** repeated GC-excluded C-backend runs record roughly
  33–85 ms append p99, 291 plan calls, and zero cache hits. The latest run
  recorded 84.526 ms. In the same run, point motion recorded 0.033 ms p99
  with zero plan/cache calls.
- **Behavior/Risk:** documentation only. No performance shortcut or core
  DP change is included in task029.

## 2026-07-29 — Plan task029

- **Add** `issue017`: point-only motion currently mutates an already
  published semantic live prefix.
- **Add** `task029`: move live-boundary ownership from transient point to
  the latest real source edit, while preserving whole-hard-line planning.
- **Modify** the current spec, plan, technical reference, and `issue016`
  expectation so point is not a plan or projection invalidation input.
- **Add** `postmortem/20260729-editing-frontier-not-point.md` as the
  superseding decision for the point-driven portion of `task028`.
- **Validation planned:** focused RED/GREEN public-hook ERT, complete
  buffer/default/random/isolated suites, fuzz/static gates, live
  benchmark, GUI dynamic verification, full diff review, and independent
  code/architecture review.
- **Status:** Planning complete; superseded by the implementation record
  above.
