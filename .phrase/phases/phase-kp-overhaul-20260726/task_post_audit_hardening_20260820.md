# Tasks: Post-Audit Hardening 2026-08-20

- task037 [x] Validate public buffer widths before mutation and make rejected
  `ekp-justify-region` requests failure-atomic. Add red tests for zero,
  negative, non-integer, and projection-preservation cases.
  - Source: post-audit buffer probe and `plan_post_audit_hardening_20260820.md`.
  - Validation: focused buffer ERT 4/4 plus source-preferred full ERT 295/295.

- task038 [x] Resolve backward-delete live projection semantics. Add a public
  edit-path regression at the narrowest reproducible width and update the
  live-layout spec/postmortem with the accepted invariant.
  - Source: `postmortem/20260730-incremental-live-append-ownership.md` and
    `plan_post_audit_hardening_20260820.md`.
  - Validation: focused live-edit ERT 4/4, randomized ERT 295/295, and
    source-first isolated ERT 294/294 process runs plus focused isolated
    coverage of the later batch-position test.

- task039 [x] Make hyphenation locale resolution try the exact normalized
  locale registry before short-code fallback. Add equivalent BCP-47/underscore
  tests and preserve fail-closed unsupported-pattern behavior.
  - Source: `ekp-hyphen.el`, `plan_dictionary_governance_20260728.md`, and
    the post-audit locale probe.
  - Validation: focused dictionary ERT 11/11, manifest check, and full ERT
    295/295.

- task040 [x] Validate C hyphen and forbidden position vectors for the exact
  exclusive break-index domain, monotonic ordering, and duplicate policy
  before DP access. Add direct single/batch ABI regressions without changing
  the 15-field schema.
  - Source: `plan_c_api_contract_20260728.md` and the post-audit C probe.
  - Validation: C warning-clean build, direct/batch C ERT 15/15, fuzz
    300/300, and Elisp parity.

- task041 [x] Restore the showcase automatic inline-code face and make normal,
  random, and isolated runners source-fresh and complete over the ERT
  inventory. Add a regression proving stale ignored bytecode cannot mask the
  showcase contract.
  - Source: `888a401`, `plan_test_isolation_20260728.md`, and the post-audit
    clean-source failure.
  - Validation: source loader symbol checks, clean-source showcase 3/3,
    normal/random/isolated 295-test inventories, and GUI contract checks.

- task042 [x] Remove manual buffer integrations when no spans remain and
  auto-mode does not own the lifecycle. Preserve composition with foreign
  filters and add empty/foreign-only teardown regressions.
  - Source: `plan_integration_lifecycle_20260728.md` and the post-audit
    no-projection probe.
  - Validation: focused integration ERT 4/4 and full source-first buffer
    suite.

- task030 [ ] Re-profile and, if still necessary, optimize the remaining
  source-instrumented narrow live-append structural commit path after
  task037–task042. Keep exact parity, source-clean projection, and zero-work
  point motion as hard constraints; close only on the locked 16 ms gate.
  - `issue028` is part of this task: each baseline/candidate evaluator round
    must explicitly load source files from its own code root, never rely on
    local `.elc` precedence.
  - The evaluator now records `append_ms` and `append_dp_ms` and preserves
    nonempty raw JSONL per run. Current source-fresh p95 is 17.161/1.187 ms
    for C append/append-DP and 73.293/57.063 ms for Elisp; choose a
    production optimization only after this attribution remains stable.
  - It now also records append preparation/assembly separately: latest p95
    is 9.753/5.663 ms for both engines, while Elisp append-DP is 55.970 ms
    and C append-DP is 1.015 ms. The evaluator wrapper collision was fixed and
    the same source matrix passes the harness without argument errors.
  - User-selected architecture option 2 is implemented as
    `ekp-auto-justify-native-append`: loaded C DP is used only for prepared
    auto live append, while full/string `ekp-use-c-module=nil` remains pure
    Elisp and unavailable native modules fall back exactly. The source
    append-DP p95 drops from roughly 56 ms to roughly 2.1 ms in the bounded
    all-width/row run; preparation and assembly remain the active budget
    owners. Width-80 C candidate p95/p99 is 14.190/16.495 ms, so the locked
    16 ms gate remains open rather than being marked complete on a noisy
    single-round result.
