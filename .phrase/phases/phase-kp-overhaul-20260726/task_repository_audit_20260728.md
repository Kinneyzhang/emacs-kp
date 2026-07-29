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

- task017 [x] Rename the editor integration module around its buffer owner.
  - Source: user request and `plan_buffer_module_naming_20260728.md`.
  - Behavior lock: run the existing 130-test ERT suite before editing; the
    rename must preserve rendered output, logical serialization, and every
    public region/buffer workflow.
  - Change: rename the file/feature/customization/configuration/private/test
    namespaces from `ekp-region` to `ekp-buffer`, while keeping public
    operation names that correctly describe region or buffer operands.
  - Validation: focused/default/permuted/isolated ERT, C/Elisp fuzz,
    warning-as-error byte compilation, checkdoc, release/package/static
    gates, stale-name scan, and full diff review.
  - Impact: editor integration source/tests, automation, bilingual public
    and developer documentation, current audit records, phase records,
    changelog, and a naming decision postmortem.
  - Completed: 2026-07-28; pre-change ERT 130/130; focused buffer ERT 44/44;
    default and seeded-permuted ERT 130/130; every one of 130 ERT tests
    passed in a fresh Emacs process; C/Elisp fuzz 300/300; warning-as-error
    byte compilation, checkdoc, pinned package-lint, release/shell/diff
    gates, active stale-name scan, and mechanical equivalence check passed.

- task018 [x] Record the text-property-only layout and hyphen feasibility.
  - Source: `issue011`, the user's 2026-07-29 display-property constraints,
    and `tech-refer_text_property_layout_20260729.md`.
  - Scope: documentation and clean GUI probes only; no runtime source or
    test implementation changes.
  - Evidence: confirm absolute-pixel `min-width`, real-space
    shrink/stretch through `space-width`, `line-prefix` indentation, and a
    replacing display string that renders a source grapheme plus
    discretionary hyphen and visual newline without changing source
    characters.
  - Validation: official GNU Emacs documentation/source review; clean
    Emacs 30.2 GUI glue, hyphen/newline, vertical-motion, and line-prefix
    probes; document link/traceability review; `git diff --check`.
  - Impact: `issue011`, one new technical reference, current OMX plan/test
    supersession notices, phase/global change records; no runtime behavior.
  - Completed: 2026-07-29. Static LTR feasibility is recorded; `issue011`
    remains open for the full GUI/editor-semantic gate, live editing, and
    the one-buffer/multiple-window decision.

- task019 [x] Extract a semantic KP layout plan and preserve string output.
  - Source: `issue011`,
    `spec_text_property_layout_engine_20260729.md`, and
    `plan_text_property_layout_engine_20260729.md` M1.
  - Red tests: require line/source/gap/break/hyphen plan data and require the
    existing string renderer to consume that plan without changing its
    `equal-including-properties` output.
  - Validation: focused red/green ERT, full core ERT, C/Elisp parity, fuzz,
    warning-as-error byte compilation, and checkdoc.
  - Impact: `ekp.el`, core tests, developer reference, and phase records.
  - Completed: 2026-07-29; focused plan tests 0/3 → 3/3; the public string
    renderer consumes `ekp-layout-plan`; full ERT passed 133/133, C/Elisp
    fuzz passed 300/300, and warning-as-error compilation, checkdoc, and
    diff checks were clean.

- task020 [x] Replace physical buffer formatting with text-property projection.
  - Source: `issue011`,
    `spec_text_property_layout_engine_20260729.md`, and
    `plan_text_property_layout_engine_20260729.md` M2.
  - Red tests: unchanged source characters/positions/ticks/undo/modified
    state; exact cleanup and foreign-property restoration; no overlays or
    synthesized layout characters; copy/save/isearch see logical text.
  - Validation: focused ERT plus clean Emacs 30.2 GUI glue, CJK break,
    Latin discretionary hyphen, indentation, point, region, and mouse probes.
  - Impact: `ekp-buffer.el`, buffer/GUI tests, commands, integrations,
    bilingual docs, and phase records.
  - Completed: 2026-07-29. `ekp-buffer.el` now projects only onto existing
    source characters with owned, nonsticky text properties and creates no
    overlay. Buffer ERT passed 71/71. A clean Emacs 30.2 GUI probe rendered
    exact 1–64px ASCII and zero-source CJK glue, display-only Latin hyphens
    and breaks, `line-prefix`, point/vertical motion/region/mouse behavior,
    unchanged source text, and zero overlays. The 7-case scale/remap/fringe/
    width matrix passed.

- task021 [x] Implement active-line and bounded-suffix live layout.
  - Source: the user's seamless-editing requirement,
    `spec_text_property_layout_engine_20260729.md`, and
    `plan_text_property_layout_engine_20260729.md` M3.
  - Red tests: unfinished line stays natural; overflow pushes a minimal
    suffix; deletion pulls back; stable prefix does not move; stale
    generations cancel; composition defers; no delayed whole-paragraph snap.
  - Validation: deterministic ERT, performance budgets, and dynamic GUI
    recordings with temporal review.
  - Impact: `ekp-buffer.el`, live-edit state/tests/benchmarks, mode help,
    bilingual docs, and phase records.
  - Completed: 2026-07-29. Live edits publish synchronously from a stable
    active-flow anchor, preserve a natural unfinished line, push the
    minimal overflow suffix, pull text back on deletion, converge on an
    unchanged line signature, defer IME composition, reject stale
    generations, and bound both lookahead and automatic paragraph work.
    The type/delete round-trip regression failed with an extra break before
    the stable-anchor fix and now restores exact prior break positions.
    Buffer ERT passed 71/71. The 31.71s dynamic GUI artifact recorded
    before/immediate/settled overflow and deletion checkpoints; source and
    projection hashes were stable after redisplay, deletion restored the
    original hashes and two breaks, all assertions passed, zero overlays
    were present, and temporal review returned PASS.

- task022 [x] Close the text-property layout architecture and quality gates.
  - Source: `issue011`,
    `spec_text_property_layout_engine_20260729.md`, and
    `plan_text_property_layout_engine_20260729.md` M4.
  - Work: resolve documentation drift, record the one-width-per-buffer and
    display-conflict decisions, remove dead physical-layout adapters, run
    anti-slop cleanup, and obtain independent reviews.
  - Validation: default/permuted/isolated ERT, fuzz, warning-clean Elisp/C
    builds, checkdoc/static/release gates, GUI matrix, dynamic evidence,
    full diff review, code-reviewer APPROVE, and architect CLEAR.
  - Impact: runtime/tests/docs/postmortem and `issue011` closure.
  - Completed: 2026-07-29. Buffer ERT passed 71/71; full default and
    seeded-permuted ERT passed 162/162; every one of 162 tests passed in a
    fresh Emacs process; C/Elisp fuzz passed 300/300. Warning-as-error
    byte compilation, checkdoc, pinned package-lint, release, bundled and
    pinned-upstream dictionary, shell, diff, stale-name, no-overlay, and
    dead-private-function gates passed. Portable, native, debug, and
    sanitizer C profiles built cleanly. Static GUI verification passed
    exact 1–64px ASCII/CJK glue and the seven-case display matrix; dynamic
    verification passed the overflow/type/delete round trip with zero
    overlays and stable source/projection hashes. Anti-slop cleanup removed
    the dead `ekp-buffer--justified-spans` and `ekp--para-glue-shrink`
    functions. Independent code review returned `APPROVE` with no findings;
    independent architecture review returned `CLEAR`. `issue011` remains
    open only for the required user-visible behavior confirmation.
  - Completion-audit correction: the final objective named a phase spec,
    but the phase initially had none. Added
    `spec_text_property_layout_engine_20260729.md` and linked it from the
    plan, tasks, issue, and change record before claiming goal completion.
    The same audit corrected the dynamic duration to the manifest's
    31.707-second `run-end` value and strengthened the existing display
    tests to assert the exact nested `space-width`, `min-width`, and
    `line-prefix` values. The strengthened focused tests passed 3/3; the
    current full suite passed 162/162; warning-as-error test compilation,
    checkdoc, release, artifact-cleanliness, and diff gates remained clean.

- task023 [x] Preserve editor selection state and natural active-line edges.
  - Source: `issue012`, `issue013`, and
    `spec_text_property_layout_engine_20260729.md`.
  - Red tests: reprojection must preserve an inactive mark; one
    command-loop space/tab at either active-line edge and whitespace
    exposed by deletion must carry no EKP replacing display.
  - Work: restore the mark marker without changing `mark-active`; prevent
    completed-paragraph edge hiding from running on the active live line.
  - Validation: focused red/green ERT plus clean GUI width-key and
    single-space checkpoints.
  - Impact: `ekp-buffer.el`, buffer/GUI tests, user docs, issue/change
    records, and the live-layout decision record.
  - Completed: `ekp-justify-region` now restores the mark marker and
    `mark-active` independently. Live projection never applies static edge
    hiding to the active line and clears from the source span beginning
    when its first line is active. Focused red 0/3 → green 3/3; clean GUI
    width and single-space checkpoints pass.

- task024 [x] Audit the live interaction matrix for adjacent regressions.
  - Source: the user's request for detailed seamless-editing review and
    `issue_live_editing_interaction_20260729.md`.
  - Work: exercise insertion, consecutive whitespace, deletion/backspace,
    newline, yank, undo, point, inactive/active mark, visual-break
    boundaries, paragraph transitions, resize, and teardown through public
    command paths.
  - Validation: deterministic matrix ERT, full repository gates, and a
    temporally reviewed GUI recording with explicit invariants.
  - Impact: tests/verification artifacts and only root-cause runtime fixes
    for newly reproduced defects.
  - Completed: added public-command coverage for active/inactive regions,
    edge whitespace, consecutive spaces/backspace, yank, newline, undo,
    and deletion-exposed whitespace; existing tests cover the remaining
    flow, resize, composition, transition, and teardown boundaries. Buffer
    ERT passes 79/79; default/random/isolated full ERT passes 170/170;
    fuzz passes 300/300; compiler, lint, release, dictionary, static, diff,
    and clean dynamic GUI gates pass.

- task025 [x] Replace partial live KP with natural progressive editing.
  - Source: `issue014`, the user's approved three-state model, and
    `spec_text_property_layout_engine_20260729.md`.
  - Red tests: an underfull active tail owns no EKP layout property; natural
    wrap aligns only completed rows without a replacing break or live
    hyphen; editing/backspacing into a committed row restores native
    display; hard-paragraph completion permits one complete KP pass.
  - Work: delete the stable-anchor/lookahead/convergence/push-pull live
    state machine. Keep a natural active tail, project only gaps on rows
    already left by native screen wrapping, and run full KP only at hard
    paragraph completion or paragraph exit.
  - Validation: focused red/green ERT, full repository gates, and
    per-keystroke mixed Latin/CJK GUI recording across wrap and deletion.
  - Impact: `ekp-buffer.el`, core live-flow API if it becomes unused,
    buffer/GUI tests, bilingual docs, spec/plan, issue/change records, and
    the live-layout postmortem.
  - Completed: removed stable-anchor/lookahead/convergence/push-pull flow
    and its unused core signatures. The live path now clears the active
    hard paragraph to native display, commits only internal gap widths on
    native rows already left by point, keeps the active tail untouched,
    and runs complete KP only on hard-paragraph completion or exit. Live
    rows cannot publish prefixes, replacing breaks, or discretionary
    hyphens.

- task026 [x] Close natural-live-edit verification and documentation.
  - Source: `task025`, `issue014`, and the repository completion contract.
  - Work: remove obsolete tests/docs/private APIs, update the durable
    design record, inspect the full diff, and retain deterministic temporal
    evidence.
  - Validation: default/random/isolated ERT, fuzz, warning-as-error
    compilation, checkdoc, pinned package-lint, release/dictionary/static
    gates, clean fullscreen GUI screenshots, and reviewed recording.
  - Impact: tests, documentation, phase closure records, and no additional
    runtime abstraction.
  - Completed: buffer ERT 75/75; default, seeded-permuted, and isolated full
    ERT 164/164; fuzz 300/300; compiler, checkdoc, pinned package-lint,
    release, dictionary, pinned-source, ownership, no-overlay, stale-name,
    and diff gates pass. Seven static GUI variants pass. The retained
    14.82-second fullscreen recording
    `/private/tmp/ekp-native-live-2dYu2U/recording.mov` proves native wrap,
    deletion restoration, and hard-newline completion without flicker or
    non-Emacs frames. `issue014` remains open only for required
    user-visible confirmation.

- task027 [x] Make native soft wrapping an owned auto-mode precondition.
  - Source: `issue015`, the user's narrow split-window screenshot, and
    `spec_text_property_layout_engine_20260729.md`.
  - Red tests: enabling `ekp-auto-justify-mode` must locally disable both
    explicit line truncation and Emacs's default narrow partial-window
    truncation; normal disable, activation failure, and major-mode teardown
    must restore the exact prior values and local-binding ownership.
  - Work: give the minor-mode lifecycle temporary ownership of
    `truncate-lines` and `truncate-partial-width-windows`. Do not synthesize
    a break, alter source text, or permanently overwrite user settings.
  - Validation: focused red/green ERT, split-window GUI typing across the
    native wrap boundary, full repository gates, and temporal review.
  - Impact: `ekp-buffer.el`, buffer/GUI tests, bilingual docs, spec,
    issue/change records, and the native-live postmortem.
  - Completed: the mode owns native soft wrapping before its first reflow,
    shares one hook lifecycle for activation and cleanup, and uses
    `unwind-protect` so a failed activation restores state before
    propagating the original error. Focused lifecycle ERT passes 3/3;
    buffer ERT 77/77; default, seed-`20260729`, and isolated full ERT
    166/166; fuzz 300/300; compiler, checkdoc, pinned package-lint,
    release, dictionary, pinned-source, no-overlay, stale-name, conflict,
    artifact, and diff gates pass. Retained 44-column split-window
    evidence proves native wrap with `hscroll=0`, exact source text, zero
    overlays, and zero live replacing breaks. `issue015` remains open only
    for required user-visible confirmation.

- task028 [x] Replace native-row live commitment with semantic hard-line
  prefix projection.
  - Source: `issue016`, the user's multi-row live-editing screenshot, and
    `postmortem/20260729-whole-hard-line-live-prefix.md`.
  - Red tests: through public editing paths, require the complete current
    hard line to be planned by the existing `ekp-layout-plan`, require only
    plan lines before the point-containing line to be projected, require
    earlier breaks/glue to change together after later edits, and require
    plan cache hits/zero writes when the semantic plan is unchanged.
  - Work: delete the native-row freezing model from `ekp-buffer` live
    editing. Keep the core DP, C ABI, DP schema, and completed-paragraph
    plan semantics unchanged. Add buffer-local live plan history,
    line-signature diffing, point-movement boundary updates, fail-closed
    conflict handling, and exact cleanup without overlays or source layout
    characters.
  - Validation: focused red/green ERT for self-insert, yank, delete, real
    undo/redo, point movement, hard newline, resize, major-mode change, and
    projection failure; buffer/full/random/isolated ERT; C/Elisp fuzz;
    warning-as-error compilation; checkdoc; package/release/dictionary/
    no-overlay/static gates; performance benchmark with latency and cache
    counters; clean GUI dynamic recording proving aligned semantic prefix,
    natural point line/suffix, exact source text, zero overlays,
    `hscroll=0`, and no delayed snap.
  - Impact: `ekp-buffer.el`, live-edit tests/benchmarks, GUI verification,
    bilingual user/developer docs, spec/plan/tech reference, issue/change
    records, and the superseding live-prefix postmortem.
  - Completed: 2026-07-29. The implementation consumes one complete
    hard-line `ekp-layout-plan`, projects only semantic lines before the
    point-containing line, keeps the active line/suffix natural, assigns
    break whitespace to the preceding semantic owner, uses a bounded
    16-entry live plan LRU, and publishes only the changed signature
    suffix. No task028 change was made to core DP semantics, the C ABI,
    DP schema, or plan contract.
  - Verification: focused font-context invalidation RED 0/1 → GREEN 1/1;
    buffer ERT 92/92; default and seed-`20260729` full ERT 181/181;
    181/181 isolated-process ERT with the added regression independently
    green; C/Elisp fuzz 300/300; warning-as-error compilation, checkdoc,
    pinned package-lint, release, dictionary, pinned-source, static
    ownership, and diff gates pass. Reviewed temporal GUI evidence at
    `/private/tmp/ekp-semantic-live-v4-vFZTkr` and
    `/private/tmp/ekp-semantic-split-v3-uPwuOi` returns PASS. The
    GC-excluded C-backend append p99 is below one frame; default-GC raw p99
    retains a documented collection-pause risk. Independent code review is
    APPROVE and architecture review is CLEAR.
  - Closure: task028's implementation gate is complete. `issue016` remains
    open until the user personally confirms the visible editing
    experience.

- task029 [x] Make the semantic live prefix independent of point-only
  motion.
  - Source: `issue017`, the user's clarification that an already published
    layout is not cursor-owned, and
    `postmortem/20260729-editing-frontier-not-point.md`.
  - Red tests: after a real source edit publishes a multi-line prefix,
    moving point backward and forward inside the same hard line must
    preserve the exact owned-property projection, editing frontier,
    active index, signatures, plan/cache counters, modified tick, undo
    state, and source characters. A real edit in an earlier projected line
    must still naturalize that line before mutation and relocate the
    frontier afterward. Reflow after point motion must map the preserved
    frontier into the new plan.
  - Work: store the latest real source-edit position in the live state,
    derive the active semantic line from it, preserve it across
    width/font/layout reflow, and delete point-driven boundary publication
    from `post-command-hook`. Keep leaving-hard-line completion unchanged.
  - Constraints: do not modify core DP semantics, `ekp.el`, C ABI, DP
    schema, or the layout-plan contract; do not add overlays or source
    layout characters.
  - Validation: focused RED/GREEN ERT; complete buffer/default/random/
    isolated ERT; C/Elisp fuzz; compiler/checkdoc/package/release/
    dictionary/static gates; benchmark; clean dynamic GUI motion evidence;
    full diff review; independent code and architecture review.
  - Impact: `ekp-buffer.el`, buffer and GUI tests, benchmark assertions,
    bilingual user/developer docs, spec/plan/technical reference,
    issue/change records, and the superseding frontier postmortem.
  - Completed: 2026-07-29. Live state now owns a source-relative frontier
    marker at the latest real edit. Projection derives its active semantic
    line from that marker; `after-change` relocates it, reflow preserves
    it, and `post-command-hook` no longer republishes inside the active
    hard line. Leaving the hard line and hard-newline completion retain the
    existing static transition. Core DP semantics, `ekp.el`, C ABI, schema,
    and the layout-plan contract are unchanged.
  - Verification: the two focused regressions failed before the fix and
    passed afterward; independent review ran 10/10 focused tests and the
    buffer suite passes 93/93. Default, seed-`20260729`, and isolated full
    suites pass 182/182; C/Elisp fuzz passes 300/300. Warning-as-error
    compilation, checkdoc, pinned package-lint, release, dictionary,
    pinned-source, no-overlay, no-stale-symbol, and diff gates pass.
    Point-motion benchmark p99 is 0.033 ms on the C backend and 0.037 ms
    in the independent Elisp-backend review, with zero planner/cache calls
    in both. Reviewed 26.6-second GUI evidence at
    `/private/tmp/ekp-frontier-live-v3-66WYRW` contains 39 manifest lines,
    all staged assertions green, no black segment, and a PASS report.
    Architecture review is CLEAR; code review reports zero blockers and
    isolates the unrelated append latency as `issue018`/`task030`.
  - Closure: task029's developer gate is complete. `issue016` and
    `issue017` remain open until the user personally confirms the visible
    editing experience.

- task030 [ ] Diagnose and optimize unique-state live append planning at
  very narrow widths.
  - Source: `issue018` and
    `postmortem/20260729-narrow-live-append-replanning.md`.
  - Problem: after task031, the checked-in 80-pixel workload performs zero
    planning on ordinary same-row edits but still records 15 structural
    boundary plans across 291 appends. A fresh GC-excluded run measured
    C median 2.177 ms/p99 51.170 ms and Elisp median 2.176 ms/p99
    187.499 ms; the high percentiles now belong to permitted commit spikes,
    not per-key frontier replanning.
  - Work: profile a documented width/length and commit-event matrix,
    identify the surviving structural-planning owner of the cost, and
    implement the smallest exact optimization that reduces commit latency.
  - Constraints: preserve KP output semantics, exact cache identity,
    task031's stable transaction/structural-commit contract, source-clean
    text properties, zero-work point-only motion, and the existing core
    DP/C ABI/schema contracts unless a separately approved architecture
    decision changes them. Do not use debounce, stale reuse, skipped edits,
    timers, or global GC changes to hide mutator latency.
  - Validation: result-equivalence ERT/fuzz, profiler before/after evidence,
    repeatable Elisp/C default-GC and GC-excluded width-matrix benchmarks,
    complete repository gates, and dynamic GUI typing evidence.
  - Dependency: `task031` is complete. Optimize only the surviving
    structural-commit path; the obsolete per-keystroke whole-hard-line path
    no longer exists.

- task031 [x] Replace the overloaded live frontier with committed projection,
  a dirty edit transaction, and atomic structural commits.
  - Source: `issue019`, the user's stable-editing correction, and
    `postmortem/20260729-stable-live-transaction.md`.
  - Red tests: drive installed before/after-change and command hooks to prove
    that ordinary same-row edits perform no whole-hard-line planning, a
    middle-line edit preserves unaffected projection anchors, deleting and
    reinserting the same space restores the exact
    `equal-including-properties` projection, point-only motion across or
    outside the active hard line performs zero planning/property writes, and
    the next real edit elsewhere commits the prior dirty transaction.
  - Work: delete `frontier = natural suffix` ownership. Store the committed
    source, plan, line signatures, spans, and owned projection baseline in
    the edit transaction. Naturalize only the dirty edit island, preserve
    unaffected break anchors, let native soft wrapping absorb local
    push/pull, and replan atomically only when input crosses a visual-row
    boundary or another structural commit event occurs.
  - Commit events: native soft-wrap crossing, hard newline/paragraph end, the
    next real source edit outside the dirty island, explicit paragraph
    refill, and width/font/layout-context change. Cursor motion is never a
    commit event.
  - Constraints: keep core DP, `ekp.el`, C ABI, DP schema, and layout-plan
    semantics unchanged; never add overlays or source layout characters;
    preserve exact source/editor state and fail closed on conflicts.
  - Validation: focused RED/GREEN ERT, complete buffer/default/random/
    isolated ERT, C/Elisp fuzz, warning-as-error compile/checkdoc/package/
    release/static gates, a re-profiled live benchmark, and temporal GUI
    evidence for stable normal input, middle-line edits, reversible edits,
    local word migration, hard completion, and point-only motion.
  - Impact: `ekp-buffer.el`, buffer/live benchmark/GUI tests, bilingual
    user/developer docs, spec/plan/technical reference, issue/change records,
    changelog, and the superseding postmortem.
  - Implementation: `ekp-buffer--live-state` owns the committed
    source/key/plan/signatures/prefix/spans; `ekp-buffer--live-edit` snapshots
    that state plus marker offsets and one dirty island.
    `ekp-buffer--start-live-edit` naturalizes only the affected span range,
    `ekp-buffer--ordinary-live-edit-finished` restores an exact baseline or
    publishes only on a real native-row crossing, and hard/elsewhere/
    refill/context events commit through their existing boundaries. Lazy
    reflow excludes the active paragraph so static chunks cannot detach
    live-owned spans. No task031 change was made to `ekp.el` or the core DP.
  - Verification: focused RED tests first failed for same-row planning,
    middle-anchor invalidation, exact reversal, point-motion work, backward
    row crossing, and lazy-reflow ownership, then passed after the owning
    fixes. Final runs pass buffer 99/99 and default, seed-`20260729`, and
    isolated suites 188/188 each; C/Elisp fuzz passes 300/300. Production
    and task test files compile with warnings as errors; checkdoc is empty;
    pinned package-lint exits 0; release and 49-entry dictionary-manifest
    gates pass. The exact pinned-source gate passed earlier in this task
    with unchanged dictionary bytes; two redundant final downloads ended in
    GitHub transport `early EOF`, not a content mismatch.
  - Performance: at 80 px with GC excluded, same-row cache-revisit work
    performs zero plans (C p99 1.627 ms; Elisp p99 1.502 ms), point motion
    performs zero plan/cache calls (C p99 0.017 ms; Elisp p99 0.015 ms), and
    291 appends contain only 15 permitted structural plans. The remaining
    commit spikes are the re-profiled `task030` surface.
  - Dynamic evidence: the reviewed 48.95-second fullscreen run at
    `/private/tmp/ekp-stable-transaction-final5-2BFryc` contains 55 ordered
    checkpoints (11 actions × 5 phases), 17 green assertions at every
    checkpoint, one 1434×900 target window, a completed run-end, no black
    segment, and a PASS report. It covers middle-row locality, exact
    reversal, forward structural crossing, backward deletion stability,
    point motion, public yank/undo, resize/context commits, and hard
    paragraph completion.
  - Closure: developer verification is complete. `issue019` remains open
    only for the required user-visible confirmation; `task030` is unblocked.

- task032 [x] Reduce C-backed resize/reflow latency with exact layout parity.
  - Source: `issue020` and the user's observed 60–70 ms C resize latency.
  - Baseline first: freeze the current portable C module and build a
    same-machine evaluator that separates complete reflow, plan construction,
    Emacs/C marshalling, C DP, and projection publication. Exclude startup,
    resize debounce, and GC pauses from the mutator measurement.
  - Performance gate: an interleaved baseline/candidate width-and-length
    matrix must improve both p50 and p95 by at least 20%, keep candidate p95
    at or below 50 ms, and report raw samples rather than a best-of run.
  - Correctness gate: every candidate C result must match the frozen C
    baseline and Elisp layout exactly; complete ERT, C/Elisp fuzz,
    warning-as-error builds, static/release checks, and temporal GUI resize
    evidence remain mandatory.
  - Constraints: do not weaken cache identity, KP output semantics, the
    stable transaction model, or source-clean projection. Do not hide work
    with longer debounce, skipped widths, stale results, approximate reuse,
    background publication, or global GC changes.
  - Stop condition: optimize only the layer proven dominant by the baseline
    profile. If C DP is not the owner of most resize time, fix the actual
    Elisp/C or projection boundary instead of micro-optimizing the DP.
  - Impact: evaluator/benchmark, the proven runtime owner, C module tests,
    performance documentation, issue/change records, and a decision
    postmortem when the selected optimization is non-obvious.
  - Implementation: resolve one paragraph/DP payload per plan, cache natural
    gap geometry, omit true no-op gaps, consolidate owned property writes,
    and avoid static-plus-live double publication of the active paragraph.
  - Result: four interleaved rounds reduce core p50/p95 from
    22.949/42.006 ms to 15.318/27.687 ms and complete resize p50/p95 from
    28.149/46.611 ms to 15.900/27.487 ms. Exact frozen-C/Elisp parity holds.
  - Verification: normal and permuted 193-test suites, the per-test isolated
    runner, 300-case fuzzing, warning-as-error compilation, package-lint,
    checkdoc, release checks, and reviewed dynamic GUI evidence pass.
  - Closure: developer verification is complete. `issue020` remains open
    only for the required user-visible confirmation.
