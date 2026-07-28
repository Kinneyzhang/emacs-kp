# Tasks: Repository System Audit 2026-07-28

- task001 [x] Produce a repository-wide audit covering architecture, Elisp,
  C, buffer integration, tests, GUI, dictionaries, performance,
  compatibility, documentation, release health, and extension directions.
  - Source: user request; existing `phase-kp-overhaul-20260726`.
  - Output: `Docs/REPOSITORY_AUDIT_20260728.md`.
  - Validation: 94/94 ERT, 300/300 C/Elisp fuzz, clean-copy byte compilation,
    checkdoc, warning-clean C release build, clean-copy ERT/fuzz, GUI fit
    matrix 7/7, targeted negative probes, link/stale-fact/diff checks.
  - Impact: documentation and planning records only; no runtime behavior
    changed.

- task002 [x] Complete the DP cache signature so algorithm-parameter changes
  never reuse stale results.
  - Source: `issue001`, audit P1-01, and
    `plan_cache_signature_20260728.md`.
  - Red test: parameter-by-parameter cached-vs-fresh comparison through
    `ekp-dp-cache`, plus a same-signature cache-hit assertion.
  - Validation: focused red/green ERT, complete ERT, C/Elisp fuzz,
    warning-clean byte compilation, checkdoc, and full diff review.
  - Impact: `ekp.el`, `tests/ekp-tests.el`, user documentation, and phase
    records.
  - Completed: 2026-07-28; focused 0/2 → 2/2, full ERT 96/96, fuzz
    300/300, byte compilation/checkdoc clean, Elisp/C rendered-output
    probes matched fresh computation.

- task003 [x] Complete automatic spacing identity across both paragraph
  cache lookup paths.
  - Source: `issue001`, audit P2-01, and
    `plan_auto_spacing_signature_20260728.md`.
  - Red test: change `ekp-default-cws-stretch-pixel` without clearing
    caches and exercise the paragraph hash and `ekp--last-para` separately;
    retain an unchanged-signature hit control.
  - Validation: focused 1/3 → 3/3, complete ERT 99/99, C/Elisp fuzz
    300/300, warning-clean byte compilation, and checkdoc.
  - Impact: `ekp.el`, `tests/ekp-tests.el`, public/developer documentation,
    audit status, phase records, and a design postmortem.
  - Completed: 2026-07-28; `issue001` is resolved.

- task004 [x] Make justified-buffer saving non-mutating and failure-safe.
  - Source: `issue002`, audit P1-02, and
    `plan_save_transaction_20260728.md`.
  - Red test: real missing-directory failure and forced `quit` both left the
    old buffer unformatted.
  - Validation: focused 0/2 → 2/2, then success/failure/encoding/retry 4/4;
    complete ERT 102/102; C/Elisp fuzz 300/300; warning-clean byte
    compilation; checkdoc.
  - Impact: `ekp-region.el`, `tests/ekp-region-tests.el`, user/developer
    documentation, audit status, phase records, and a design postmortem.
  - Completed: 2026-07-28; `issue002` is resolved.

- task005 [x] Make buffer integrations composable and span/mode-owned.
  - Source: `issue003`, audit P1-03/P2-06, and
    `plan_integration_lifecycle_20260728.md`.
  - Red test: existing filter output was lost and final unjustify left the
    EKP filter/hooks installed (0/2).
  - Validation: focused 2/2 then five public composition/lifecycle cases;
    complete ERT 107/107; C/Elisp fuzz 300/300; warning-clean byte
    compilation; checkdoc.
  - Impact: `ekp-region.el`, `tests/ekp-region-tests.el`, public/developer
    documentation, audit status, phase records, and a design postmortem.
  - Completed: 2026-07-28; `issue003` is resolved.

- task006 [x] Eliminate ERT order-dependent false greens.
  - Source: `issue004`, audit P1-04, and
    `plan_test_isolation_20260728.md`.
  - Red test: loading C and running only
    `ekp-test-parshape-bypasses-c` failed 0/1.
  - Validation: focused public-dispatch/config-restoration/C-parameter
    matrix 3/3; reproducibly permuted ERT 108/108; every ERT independently
    passed in a fresh Emacs process, 108/108.
  - Impact: core fixture and dispatch regression, reusable test runners,
    CI, developer documentation, audit status, phase records, and a testing
    postmortem.
  - Completed: 2026-07-28; `issue004` is resolved.

- task007 [x] Make GUI verification fail closed in automation.
  - Source: `issue007`, audit P2-05, and
    `plan_gui_verification_20260728.md`.
  - Red test: the forced noninteractive failure control had no report
    assertion boundary, 0/1.
  - Validation: focused forced-failure/success ERT 2/2; default and
    permuted full ERT 110/110; real GUI matrix 7/7; inspected clean
    fullscreen single-window screenshot.
  - Impact: GUI verification result/report contract, batch-safe negative
    tests, test loaders, public/developer documentation, audit status,
    phase records, and a testing postmortem.
  - Completed: 2026-07-28; `issue007` is resolved.

- task008 [x] Close the C API input, error, and arithmetic contract.
  - Source: `issue005`, audit P2-02, and
    `plan_c_api_contract_20260728.md`.
  - Red tests: malformed/short/mismatched/out-of-range/penalty inputs and
    extreme valid int32 arithmetic failed 0/6; public dispatcher swallowed
    a forced module error, 0/1.
  - Validation: focused C boundary 6/6 and public error propagation 1/1;
    warning-clean release and sanitizer compilation; full ERT 116/116;
    C/Elisp fuzz 300/300.
  - Impact: C API validation, 64-bit DP intermediates, module version 1.6,
    Elisp backend error propagation, tests, public/developer documentation,
    audit status, phase records, and an architecture postmortem.
  - Completed: 2026-07-28; P2-02 is resolved. `issue005` remains open until
    the build invocation/profile slice is complete.

- task009 [x] Make C builds shell-free, portable by default, and profiled.
  - Source: `issue005`, audit P2-03, and
    `plan_c_build_boundary_20260728.md`.
  - Red tests: capture the interactive build process shape for a module path
    containing spaces and reject an unknown profile.
  - Validation: focused ERT, real portable/native/debug/sanitize builds,
    a copied build from a whitespace path, full ERT/fuzz, byte compilation,
    checkdoc, and diff review.
  - Impact: `ekp-utils.el`, `ekp_c/Makefile`, CI build arguments, tests,
    public/developer/C documentation, audit status, and phase records.
  - Completed: 2026-07-28; process/profile red 0/2 → 2/2; all four profiles
    compiled warning-free; portable succeeded through the real interactive
    command and from a whitespace path. `issue005` is resolved.

- task010 [x] Close repository-local release and CI governance.
  - Source: `issue009`, audit P2-07, and
    `plan_release_governance_20260728.md`.
  - Red test: a static release gate must reject floating action refs, absent
    Windows coverage, ignored `.phrase` truth, or inconsistent package/C
    version declarations.
  - Validation: red/green gate, workflow syntax/static inspection, default
    ERT, and `git diff --check`.
  - Impact: CI, `.gitignore`, release documentation, audit/phase records, and
    a release-governance postmortem.
  - Completed: 2026-07-28; invariant gate red → green, pinned package-lint
    passed, workflow YAML and shell syntax passed, and default ERT was
    119/119. `issue009` is resolved locally; no remote release action was
    performed.

- task011 [x] Make dictionary syntax support and provenance auditable.
  - Source: `issue006`, audit P2-04, and
    `plan_dictionary_governance_20260728.md`.
  - Red tests: affected-language golden behavior and a manifest/updater
    verifier must fail against the unpinned moving-HEAD bundle.
  - Validation: golden red/green, 49/49 checksum/source/license manifest,
    two identical pinned update outputs, full ERT, byte compilation,
    checkdoc, and diff review.
  - Impact: hyphen parser/tests, dictionary updater and metadata, public/
    developer/audit documentation, phase records, and an architecture
    postmortem.
  - Completed: 2026-07-28; affected-language red 0/2 → 2/2; offline and
    upstream 49/49; two exports identical; full ERT 121/121; compile,
    checkdoc, shell/YAML, and diff checks passed. `issue006` is resolved.

- task012 [x] Make measured tokenizer/insertion growth linear and cache nil.
  - Source: `issue008`, audit P3-01, and
    `plan_hot_loop_performance_20260728.md`.
  - Red tests: two no-break lookups must compute once; long/propertized
    tokenization and dense insertion must retain exact output.
  - Validation: before/after 1k–8k scaling, focused/full ERT, warning-as-error
    byte compilation, checkdoc, fuzz, and diff review.
  - Impact: tokenizer/hyphen cache/insertion, benchmark/tests, developer/
    audit documentation, phase records, and a performance postmortem.
  - Completed: 2026-07-28; nil-cache red 0/1 → 1/1; focused output
    regressions 3/3; 8,000-character tokenizer/insertion improved from
    3.133/0.945 s to 1.100/0.013 s; full ERT 124/124, fuzz 300/300,
    warning-as-error production compilation and checkdoc passed.

- task013 [x] Clarify and test interactive protection workflows.
  - Source: audit P3-03, ultragoal G009, and
    `plan_interactive_protection_20260728.md`.
  - Red tests: drive all four no-break/verbatim commands interactively,
    assert real formatter behavior and session-lifetime feedback, and
    require a standard mode menu exposing the workflows.
  - Validation: focused red/green ERT, default/permuted/isolated full ERT,
    warning-as-error byte compilation, checkdoc, and diff review.
  - Impact: public commands/mode map, region tests, bilingual user
    documentation, audit/phase records, and a workflow postmortem.
  - Completed: 2026-07-28; focused public-path red 0/3 → 3/3; default
    and seeded-permuted ERT 127/127; every ERT passed in a fresh Emacs
    process; warning-as-error production compilation, checkdoc, release,
    dictionary, and diff gates passed.

- task014 [x] Centralize the remaining duplicated core layout rules.
  - Source: audit P3-02 and `plan_core_rule_ownership_20260728.md`.
  - Red tests: require a directly testable edge-space exclusion rule and
    one complete, nonsticky render-marker vocabulary.
  - Validation: focused red/green ERT, full ERT, C/Elisp fuzz,
    warning-as-error byte compilation, checkdoc, and diff review.
  - Impact: `ekp.el`, `ekp-region.el`, core tests, developer/audit records,
    and an architecture postmortem; no public behavior or ABI change.
  - Completed: 2026-07-28; direct rules red 0/2 → 2/2; related focused
    invariants 4/4; full ERT 129/129; C/Elisp fuzz 300/300;
    warning-as-error compilation, checkdoc, release, dictionary, and diff
    gates passed.

- task015 [x] Run bounded anti-slop cleanup and the final independent gate.
  - Source: ultragoal G010, `ai-slop-cleaner`, `code-review`, and
    `plan_final_cleanup_review_20260728.md`.
  - Behavior lock: 129/129 ERT and 300/300 fuzz before cleanup.
  - Cleanup: delete only proven dead/pass-through accessors; classify every
    fallback-like path and retain grounded boundary behavior.
  - Validation: all repository quality gates plus independent
    `code-reviewer` APPROVE and `architect` CLEAR.
  - Impact: internal cleanup, final records, and review artifacts; no
    public behavior, baseline, ABI, or remote release action.
  - Completed: 2026-07-28; removed nine dead pass-through accessors and the
    private substring-filter dependency; classified all fallback-like paths
    with no masking fallback retained; default/permuted/isolated ERT
    130/130, fuzz 300/300, warning-clean Elisp/C builds, checkdoc,
    package-lint, release/dictionary/static gates, and GUI matrix 7/7
    passed. Independent review covered 98 paths with zero findings and
    returned `APPROVE`; architecture review returned `CLEAR`.

- task016 [x] Make the same-string paragraph fast path property-sensitive.
  - Source: `issue010`, independent final code review, and
    `plan_property_sensitive_fast_path_20260728.md`.
  - Red test: warm one string object, mutate its `ekp-no-break` property,
    and require the next lookup to match a fresh paragraph for both CJK and
    Latin-with-space inputs.
  - Validation: focused red/green, default/permuted/isolated full ERT, fuzz,
    static/build/GUI gates, and independent review.
  - Impact: paragraph cache identity, core regressions, developer/audit/
    phase records, and a cache-ownership postmortem; no public API or C ABI
    change.
  - Completed: 2026-07-28; same-object property mutation red 0/1 → 1/1;
    focused cache matrix 6/6; default and seeded-permuted ERT 130/130;
    every one of 130 ERT tests passed in a fresh Emacs process; C/Elisp fuzz
    300/300; warning-as-error compilation, checkdoc, package-lint, four C
    profiles, release/dictionary/static gates, and GUI matrix 7/7 passed.
    Independent review confirmed the implementation fix and architecture
    ownership; `issue010` is resolved.
