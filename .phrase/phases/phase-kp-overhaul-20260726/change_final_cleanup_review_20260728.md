# Change Log: Final Cleanup and Independent Gate 2026-07-28

## task015

- **Delete/Modify** — changed runtime and test files
  - Deleted nine unused one-line paragraph accessors and routed remaining
    callers directly to the owning `ekp-para` fields.
  - Replaced the direct dependency on private
    `buffer-substring--filter` with public `filter-buffer-substring`
    dispatch under the prior filter binding.
  - Kept DELETE lifecycle cleanup outside the temporary dispatch binding so
    ownership restoration observes the real buffer-local slot.

- **Review** — fallback-like paths
  - Classified language, display, optional-resource, build-command, C
    allocation/no-result, sequential-pool, and test-harness paths.
  - Retained only documented external-boundary compatibility/fail-safe
    behavior with tests and visible failure evidence.
  - Found no masking fallback, swallowed business-logic error, broad shim,
    or escalation candidate.

- **Modify/Add** — phase, audit, postmortem, and final-gate records
  - Documented public filter dispatch ownership in
    `postmortem/20260728-public-filter-dispatch.md`.
  - Recorded the full post-clean verification and independent review
    evidence.

## Validation

- Default and seeded-permuted ERT: 130/130 each.
- Fresh-process isolation: every one of 130 ERT tests passed.
- C/Elisp fuzz: 300/300.
- Production byte compilation with warnings as errors and checkdoc: pass.
- Pinned package-lint commit
  `35996f478d81e51dae4fa30d051f741895d07399`: pass.
- Portable, native, debug, and sanitize C profiles: warning-free; portable
  artifact restored.
- Release gate, offline dictionary 49/49, pinned-upstream dictionary 49/49,
  shell/YAML, diff, private-API, dead-accessor, secret, and Markdown-link
  checks: pass.
- Live GUI matrix: 7/7 with a clean fullscreen one-window screenshot.
- Independent code review: 98 paths, zero findings, `APPROVE`.
- Independent architecture invariant review: `CLEAR`.

## Behavior and Risk

- Cleanup reduced indirection and removed a private dependency without
  changing public behavior, saved-file formats, C ABI, dependencies, or the
  Emacs 29.1 baseline.
- No remote push, tag, publication, release artifact, or production action
  was performed.
- Optional product expansions in audit section 4.4 remain demand-gated
  future directions, not incomplete repository defects.
