# Tasks: Post-Audit Hardening 2026-08-20

- task044 [x] Restore Emacs 31.1 WERROR compilation by replacing obsolete
  single-binding `if-let`/`when-let` forms with behavior-identical
  `if-let*`/`when-let*` forms in production and the live evaluator.
  - Source: `issue029` and the C1a nine-repository strict-compile gate.
  - Validation: root `make`; Emacs 31.1 WERROR production and changed-tool
    compilation; normal/random ERT 296/296; C build and 300-case fuzz;
    release, local 49-entry dictionary manifest/hash, checkdoc, and diff gates.
    The network-backed fixed-upstream dictionary fetch was explicitly excluded
    by the user and is not claimed as evidence.

- task043 [x] Restore the Emacs 31 fresh-source baseline without weakening
  cache-ownership or marker-noninheritance contracts. Keep policy fixtures
  mutable and multibyte, and install EKP marker properties in the global
  `text-property-default-nonsticky` default used by real buffers.
  - Source: Emacs 31 fresh-source failures in the M0a baseline.
  - Validation: focused ERT 3/3, normal and seeded-random ERT 296/296,
    source-load, release, and 49-entry dictionary gates.

- task037 [x] Validate public buffer widths before mutation and make rejected
  `ekp-justify-region` requests failure-atomic. Add red tests for zero,
  negative, non-integer, and projection-preservation cases.
  - Source: post-audit buffer probe and `plan_post_audit_hardening_20260820.md`.
  - Validation: focused buffer ERT 4/4 plus source-preferred full ERT 296/296.

- task038 [x] Resolve backward-delete live projection semantics. Add a public
  edit-path regression at the narrowest reproducible width and update the
  live-layout spec/postmortem with the accepted invariant.
  - Source: `postmortem/20260730-incremental-live-append-ownership.md` and
    `plan_post_audit_hardening_20260820.md`.
  - Validation: focused live-edit ERT 4/4, randomized ERT 296/296, and
    source-first isolated ERT 294/294 process runs plus focused isolated
    coverage of the later batch-position test.

- task039 [x] Make hyphenation locale resolution try the exact normalized
  locale registry before short-code fallback. Add equivalent BCP-47/underscore
  tests and preserve fail-closed unsupported-pattern behavior.
  - Source: `ekp-hyphen.el`, `plan_dictionary_governance_20260728.md`, and
    the post-audit locale probe.
  - Validation: focused dictionary ERT 11/11, manifest check, and full ERT
    296/296.

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
    normal/random/isolated 296-test inventories, and GUI contract checks.

- task042 [x] Remove manual buffer integrations when no spans remain and
  auto-mode does not own the lifecycle. Preserve composition with foreign
  filters and add empty/foreign-only teardown regressions.
  - Source: `plan_integration_lifecycle_20260728.md` and the post-audit
    no-projection probe.
  - Validation: focused integration ERT 4/4 and full source-first buffer
    suite.

- task030 [x] Re-profile and, if still necessary, optimize the remaining
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
    append-DP p95 drops from roughly 56 ms to roughly 2.2 ms in the complete
    all-width/row run; preparation and assembly remain exact. The formal
    four-round source-fresh evaluator passes with width-80 C p95/p99
    12.010/14.622 ms and Elisp-configured live p95/p99 10.980/11.194 ms,
    closing the locked 16 ms gate.
