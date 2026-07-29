# Change Log: Semantic Live Prefix 2026-07-29

## 2026-07-29 — Implement and verify task028

- **Modify** `ekp-buffer.el`.
  - Replaces native-row commitment with full-hard-line
    `ekp-layout-plan` consumption and point-line semantic prefix
    projection.
  - Keeps the point-containing line and suffix natural; previous semantic
    lines own their break whitespace and may reflow together.
  - Adds a 16-entry buffer-local plan LRU, same-line point-motion fast
    path, common-prefix differential publication, transactional rollback,
    theme-enable/theme-disable/frame-font context invalidation, and
    fail-closed oversized/conflict handling.
  - Leaves `ekp.el`, core DP semantics, C ABI, DP schema, and plan records
    unchanged for this task.
- **Modify** `tests/ekp-buffer-tests.el`,
  `tests/ekp-gui-verify.el`, and **Add**
  `tests/ekp-buffer-live-bench.el`.
  - Covers public editing commands, point boundaries, earlier-break
    revision, cache/history reuse, zero-write stable plans, whitespace and
    hyphen ownership, IME, failure rollback, resize, narrowing, lifecycle,
    exact source, and zero-overlay invariants.
  - Default and seed-`20260729` ERT pass 181/181; all 181 tests pass in
    independent Emacs processes; C/Elisp fuzz passes 300/300.
  - Main dynamic GUI evidence is retained at
    `/private/tmp/ekp-semantic-live-v4-vFZTkr`; split-window evidence is at
    `/private/tmp/ekp-semantic-split-v3-uPwuOi`. Both reviewed temporal
    reports return PASS with exact source, zero overlays, and `hscroll=0`.
  - C-backend append p99 is 6.399 ms with GC excluded. Default raw p99 is
    50.212 ms when samples include approximately 45 ms GC pauses; this
    residual risk is recorded instead of changing global GC behavior.
  - **Subsequent audit:** the 6.399 ms result is not reproducible with the
    current checked-in 80-pixel benchmark. Repeated GC-excluded task029
    runs measure roughly 33–85 ms p99 with 291 unique plan misses.
    `issue018`/`task030` now track that separate performance debt; this
    historical entry is retained to show what the task028 run reported.
- **Modify** bilingual user/developer docs, `CHANGELOG.md`, `issue016`,
  plan/technical records, and the superseding postmortem to match the
  implemented semantic-prefix model.
- **Behavior/Risk:** The active edit suffix is now native while all earlier
  semantic lines are one jointly revisable KP prefix. Automated and GUI
  developer gates pass. Independent code review returns APPROVE and
  independent architecture review returns CLEAR. `task028` is complete;
  `issue016` remains open until the user personally confirms the visible
  editing experience.

## 2026-07-29 — Plan task028

- **Add** `issue_semantic_live_prefix_20260729.md`.
  - Records `issue016`: live editing currently freezes completed native
    rows instead of repeatedly consuming the full hard-line KP plan and
    projecting only the semantic prefix before point.
- **Modify** `task_repository_audit_20260728.md`.
  - Adds `task028` as the next atomic implementation task with red tests,
    performance checks, GUI dynamic verification, and user-confirmation
    closure rules.
- **Modify** `spec_text_property_layout_engine_20260729.md`.
  - Replaces the native-row live contract with the semantic hard-line
    prefix contract: full hard-line plan input, point-line boundary,
    prefix-only projection, natural point line and suffix, and no core
    DP/C ABI changes.
- **Modify** `plan_text_property_layout_engine_20260729.md`.
  - Adds milestone M7 for replacing native-row commitment with semantic
    prefix projection while preserving the completed-paragraph DP owner.
- **Modify** `tech-refer_text_property_layout_20260729.md`.
  - Supersedes the native progressive row model for live editing and
    documents the buffer-local plan cache, line-signature diffing, and
    fail-closed ownership rules.
- **Add** `postmortem/20260729-whole-hard-line-live-prefix.md`.
  - Records why the native-row model was wrong for global KP alignment and
    why the correction belongs in `ekp-buffer`, not in core DP.
- **Behavior/Risk:** Documentation and planning only. Runtime behavior is
  unchanged. `issue015` remains open exactly as before pending user-visible
  confirmation; `issue016` remains open until implementation and user
  confirmation.
