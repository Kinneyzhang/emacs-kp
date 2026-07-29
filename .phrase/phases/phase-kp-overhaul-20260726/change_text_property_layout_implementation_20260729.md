# Change Log: Text-Property Layout Implementation 2026-07-29

## 2026-07-29 — Complete task027

- **Modify** `ekp-buffer.el`.
  - Makes native soft wrapping a mode-owned precondition by temporarily
    setting both `truncate-lines` and
    `truncate-partial-width-windows` buffer-locally to nil before the
    first reflow.
  - Restores exact prior values and local/global ownership on normal
    disable, major-mode teardown, and failed activation. An activation
    error rolls back the full lifecycle and then remains visible.
  - Centralizes installation and removal of the auto-mode hook set; no
    source character, overlay, synthetic live break, or wrapping algorithm
    was added.
- **Modify** buffer/GUI tests and bilingual user/developer documentation.
  - Adds public lifecycle regressions for normal disable, forced reflow
    failure, and major-mode teardown.
  - Adds a deterministic 44-column split-window scenario and records the
    native-wrap requirement in the spec, plan, issue, changelog, and
    decision history.
- **Validation:** focused lifecycle ERT 3/3; buffer ERT 77/77; default,
  seed-`20260729`, and isolated full ERT 166/166; C/Elisp fuzz 300/300;
  warning-as-error compilation; checkdoc; pinned package-lint; release,
  dictionary, pinned-source, no-overlay, stale-name, conflict, artifact,
  and diff gates.
- **Dynamic evidence:** `/private/tmp/ekp-soft-wrap-final-pass-PIUigY`
  retains a reviewed per-character Emacs 30.2 recording in an intentional
  two-window frame. The 44-column selected window advances from one to two
  visual rows with `hscroll=0`, exact source text, zero overlays, zero live
  replacing breaks, and no pending edit. Manifest/report/recording/contact
  SHA-256:
  `53cd23f8e1be1f29545edb6daa80184d438ca87d4d7068a182faf691a8437ad8`,
  `a97a74cc5a6255c2eb5992c266c727e3ed5d5556f37672ca319373a65a110e4d`,
  `2b3f7c155d1f0e2feeca2bb79a86aafe612cc1489c486121491ec6268bc1de0a`,
  `21a78ae849e3a1d5cda1d80ee375cd655e49b221b5192a219ac6ab11d9122d53`.
- **Behavior/Risk:** Narrow split windows now follow normal Emacs soft
  wrapping while auto mode is active. A buffer that deliberately requested
  truncation gets that exact setting back when the mode ends. `issue015`
  stays open pending user-visible confirmation.

## 2026-07-29 — Plan task027

- **Add** `issue_native_soft_wrap_20260729.md`.
  - Records `issue015`: Emacs's default narrow partial-window policy
    horizontally truncates the active paragraph, so the native progressive
    engine never observes a completed visual row.
- **Modify** phase spec, task list, and global issue/change indexes.
  - Makes native soft wrapping an explicit auto-mode lifecycle
    precondition with exact restoration on teardown.
- **Behavior/Risk:** Planning and red-test preparation only. Runtime
  behavior is unchanged at this entry.

## 2026-07-29 — Complete task025 and task026

- **Modify** `ekp-buffer.el`.
  - Deletes partial-KP live lookahead, stable anchors, line signatures,
    convergence, and push/pull repair.
  - Makes Emacs native redisplay own the active tail. EKP projects only
    internal gap widths on completed native visual rows and reserves full
    KP breaks, prefixes, and discretionary hyphens for completed hard
    paragraphs.
  - Invalidates a committed prefix before edits cross back into it, defers
    IME composition, rejects stale generations, respects narrowing, and
    keeps foreign display ownership isolated.
- **Modify** `ekp.el`, buffer tests, and GUI verification.
  - Removes the now-unused partial live-flow API and replaces its
    implementation-shaped tests with the approved three-state behavioral
    contract.
  - Adds dynamic actions for native wrap, deletion restoration, and hard
    paragraph completion.
- **Modify** bilingual user/developer docs, phase spec/plan/tasks/issue,
  changelog, and the live-layout decision history.
  - Records the native progressive model as the current workflow without
    rewriting the earlier partial-flow decision as if it never existed.
- **Validation:** focused live contract ERT 4/4; buffer ERT 75/75; default,
  seed-`20260729`, and isolated ERT 164/164; fuzz 300/300;
  warning-as-error compilation; checkdoc; pinned package-lint; release,
  dictionary, pinned-source, ownership, no-overlay, stale-name, and diff
  gates. Exact 1–64px ASCII/CJK probes and all seven GUI variants pass.
- **Dynamic evidence:** `/private/tmp/ekp-native-live-2dYu2U` retains a
  14.82-second fullscreen single-window Emacs 30.2 recording. It proves
  native mixed-text wrap with zero live owned breaks, exact deletion back
  to the original three native rows, and full KP only after hard newline.
  Every immediate/redisplay assertion passes; the recording contains no
  black segment or non-Emacs frame. Manifest/report/recording SHA-256:
  `cc3a626c7f8fb68f1ef1ea4ba0dd1e276281afee7a56fb88835f49bd86f239e1`,
  `0be9337a95469c6fe710518dbfa27432b4abcd511f623182021776f911887ab0`,
  `f87c3adbf16e5d17860471c1a85c89dc688b24c8e08ad82172291fc87483562e`.
- **Behavior/Risk:** Underfilled editing performs no layout planning and
  looks native. Completed native rows receive gap-only alignment. A hard
  paragraph receives full KP only after completion or exit. Native visual
  row discovery still depends on one authoritative graphical window;
  alternate-width simultaneous plans remain intentionally unsupported.
  `issue014` stays open pending user-visible confirmation.

## 2026-07-29 — Plan task025 and task026

- **Add** `issue_natural_live_editing_20260729.md`.
  - Records `issue014`: partial KP breaks and hyphens appear while typing
    near the right edge.
- **Modify** the phase spec, plan, task list, and global issue/change
  indexes.
  - Replaces the live product contract with the approved three-state model:
    native active tail, gap-only completed native rows, and full KP after
    hard-paragraph completion.
- **Behavior/Risk:** Planning and red-test preparation only. Runtime
  behavior is unchanged at this entry.

## 2026-07-29 — Complete task023 and task024

- **Modify** `ekp-buffer.el`.
  - Restores the mark marker without activating it, then restores
    `mark-active` as independent editor state.
  - Keeps static paragraph-edge whitespace hiding off the active live line
    and clears from the source span beginning when its first line becomes
    active.
- **Modify** buffer tests and GUI verification evidence.
  - Adds inactive/active mark, same-turn leading/trailing space and tab,
    CJK space, deletion-exposed whitespace, consecutive space/backspace,
    yank, newline, and undo regressions through public command paths.
  - Retains the existing overflow/pullback, paragraph transition, resize,
    IME, foreign-owner, teardown, and bounded-work matrix.
- **Modify** bilingual user/developer docs, phase spec/plan/tasks/issues,
  changelog, and `postmortem/20260729-active-line-edge-state.md`.
  - Records the editor-state and active-line ownership rules at their
    durable documentation boundaries.
- **Validation:** focused red 0/3 → green 3/3; buffer ERT 79/79; full
  default and seed-`20260729` ERT 170/170; isolated fresh-process ERT
  170/170; C/Elisp fuzz 300/300; warning-as-error compilation; checkdoc;
  pinned package-lint; release, dictionary, shell, artifact, stale-name,
  no-overlay, and diff gates.
- **Dynamic evidence:**
  `/private/tmp/ekp-interaction-clean-rdZhdD` records three actions:
  showcase width change, one-space insertion, and deletion exposing that
  space. Every immediate and redisplay assertion passes, with no region,
  pending transaction, replacing display, overlay, black segment, or
  non-Emacs window. Manifest/report/recording SHA-256:
  `0eaeebd1697d69304cfd70189d7381a026b761333ab46ab1e2c5923a6e2db0c9`,
  `940efdb923a831dcd4b098cafee33b9a67c472f227be1897bd54cab0693f8b68`,
  `519a4ba2757b453e54a2518c83784bfed26317eae4c3e246a984f38a2d30b2f5`.
- **Behavior/Risk:** The two regressions are fixed at shared projection
  owners rather than showcase keys or self-insert advice. Static committed
  lines retain edge cleanup. `issue012` and `issue013` remain open only for
  required user-visible acceptance.

## 2026-07-29 — Plan task023 and task024

- **Add** `issue_live_editing_interaction_20260729.md`.
  - Records `issue012` for inactive-mark activation during reprojection and
    `issue013` for hidden active-line edge whitespace.
- **Modify** the phase spec, plan, task list, and global issue/change
  indexes.
  - Makes same-turn whitespace visibility and independent `mark-active`
    preservation explicit product contracts.
  - Adds a bounded adjacent-interaction audit instead of treating the two
    reported symptoms as isolated key-handler patches.
- **Behavior/Risk:** Planning and red-test records only; runtime behavior is
  unchanged at this entry.

## 2026-07-29 — Complete task022

- **Add** `spec_text_property_layout_engine_20260729.md`.
  - Records the current user-visible contract, command and live-edit flows,
    display-property mapping, failure/rollback behavior, edge cases, and
    acceptance criteria.
  - Corrects a completion-audit gap: the objective required a phase spec,
    but the implementation phase had only plan/technical records.
- **Modify** implementation, tests, automation, bilingual documentation,
  phase records, changelog, and the live-layout postmortem.
  - Links the spec from the plan, tasks, issue, and change trace.
  - Closes the repository-wide text-property architecture gate after a full
    diff and ownership review.
  - Removes the dead `ekp-buffer--justified-spans` and
    `ekp--para-glue-shrink` functions; adds no wrapper, compatibility shim,
    dependency, overlay path, or source-character fallback.
- **Validation:** buffer ERT 71/71; full default and seeded-permuted ERT
  162/162; isolated fresh-process ERT 162/162; C/Elisp fuzz 300/300;
  warning-as-error byte compilation; checkdoc; pinned package-lint;
  release, dictionary, pinned-upstream dictionary, shell, diff, stale-name,
  no-overlay, and dead-private-function scans; portable/native/debug/
  sanitizer C builds; exact 1–64px GUI glue; seven-case GUI matrix; and
  dynamic overflow/type/delete temporal verification.
- **Retained GUI/dynamic evidence:**
  - `/tmp/ekp-live-final-029c-fOG8xp/manifest.jsonl`
    (`sha256:727305251edf07e4cee43bc6b8b25610a384e7d4f071656de388ec6e23913fd9`)
    records one live Emacs window, source/projection hashes, zero overlays,
    exact overflow/pullback assertions, immediate/redisplay equality, and a
    completed 31,707ms run.
  - The clean first and final screenshots are
    `frames/000001-setup-setup.png`
    (`sha256:5e324e25f278867b57c31b825c7d56c87d15b18f7fd1e99e276857127c0ddab8`)
    and `frames/000008-completion-final.png`
    (`sha256:f5e28d98190b8a494d7f010b423c217b104e76f8c79cc792a8359a328ae1797d`).
  - `report.md`
    (`sha256:fec75f10a851f220a937456fa17900c178f9466874340d1e178ca6e333693904`)
    returns `PASS` with no incomplete or black segments; `recording.mov`
    (`sha256:2e31a13b70c2387a0e9a08a1b87947180a46310e3f1becb8c445c768998ac347`)
    is the 4,875,735-byte temporal source.
- **Independent review:** code reviewer `APPROVE`, zero findings;
  architecture reviewer `CLEAR`, no unresolved architectural blocker.
- **Evidence correction:** replaces the stale duration in phase records
  with 31.71 seconds, matching the retained dynamic
  manifest's 31,707ms `run-end`.
- **Strengthen** `tests/ekp-buffer-tests.el`.
  - Replaces presence-only display checks with exact structural assertions
    for ASCII `((space-width FACTOR) (min-width ((PIXELS))))`, zero-source
    CJK `(min-width ((PIXELS)))`, and the `line-prefix` pixel-space value.
  - Focused exact-property ERT passed 3/3; the current full suite passed
    162/162. Warning-as-error compilation of the strengthened test file,
    production checkdoc, release checks, artifact cleanliness, and
    `git diff --check` passed.
- **Behavior/Risk:** The runtime and repository gates are complete.
  `issue011` intentionally remains open until the user accepts the visible
  editing behavior, as required by the issue lifecycle.

## 2026-07-29 — Complete task021

- **Modify** `ekp-buffer.el`.
  - Adds synchronous bounded live flow with a natural unfinished line,
    minimal overflow push, deletion pullback, stable-line convergence,
    generation-checked IME/resize work, and automatic paragraph limits.
  - Keeps one source-marker anchor for a continuous editing flow so
    repeated deletion cannot move the recomputation boundary into the
    stable prefix.
  - Releases the active anchor on paragraph exit, explicit refill,
    hard-boundary reflow, conflict abandonment, and mode teardown.
- **Modify** `tests/ekp-buffer-tests.el`, `tests/ekp-gui-verify.el`.
  - Adds public-hook live-edit regressions, bounded-work tests, exact
    type/delete break restoration, anchor lifecycle checks, and
    before/immediate/settled source/projection hash assertions.
- **Modify** bilingual user/developer docs, current plan/test specification,
  changelog, and `postmortem/20260729-text-property-live-layout.md`.
- **Behavior/Risk:** Ordinary edits never run a delayed whole-paragraph
  formatter. The unfinished point line remains natural; overflow and
  pullback publish synchronously. Buffer ERT passed 71/71. The final
  31.71s dynamic artifact passed every checkpoint with zero overlays,
  no immediate/settled hash drift, and exact deletion restoration.

## 2026-07-29 — Complete task020

- **Delete** `ekp-region.el`, `tests/ekp-region-tests.el`.
- **Add** `ekp-buffer.el`, `tests/ekp-buffer-tests.el`.
  - Replaces delete/insert physical layout with owned text properties on
    existing source characters only.
  - Maps ASCII glue to combined `space-width`/absolute-pixel `min-width`,
    zero-source glue to `min-width`, indentation to `line-prefix`, and
    breaks/hyphens to replacing display strings.
  - Preserves foreign property owners, direct logical text APIs, copy/save/
    search semantics, point, undo, modified state, and external hooks.
- **Modify** GUI verification and bilingual user/developer documentation.
- **Behavior/Risk:** Buffer ERT passed 71/71. The clean Emacs 30.2 static
  probe passed exact 1–64px ASCII/CJK targets, display-only break/hyphen,
  point/vertical motion/region/mouse, unchanged source, and zero overlays;
  the 7-case scale/remap/fringe/width matrix passed.

## 2026-07-29 — Complete task019

- **Modify** `ekp.el`.
  - Adds semantic `ekp-layout-plan`, `ekp-layout-line`, and
    `ekp-layout-gap` records with source offsets, exact glue targets,
    break kinds, and discretionary-hyphen decisions.
  - Makes the existing reversible string renderer consume the shared plan.
- **Modify** `tests/ekp-tests.el`.
  - Adds source mapping, hyphen decision, and public-dispatch regressions.
- **Modify** `DEVELOPER.md`, `DEVELOPER_ZH.md`.
  - Documents the layout/rendering representation boundary.
- **Behavior/Risk:** Public string formatting remains compatible.  The new
  plan is the single source for the upcoming buffer display projection.
  Validation: focused red 0/3 → green 3/3; full ERT 133/133; fuzz 300/300;
  warning-as-error compilation, checkdoc, and diff checks clean.

## 2026-07-29 — Plan task019 through task022

- **Add**
  `plan_text_property_layout_engine_20260729.md`.
  - Locks the shared core-plan architecture, text-property-only buffer
    mapping, live active-line state machine, narrowest-window policy,
    display ownership, verification gates, and rollback boundary.
- **Modify**
  `task_repository_audit_20260728.md`.
  - Adds atomic `task019` through `task022` for core extraction, static
    projection, live editing, and final closure.
- **Behavior/Risk:** Planning records only.  Runtime behavior is unchanged.
  The main risk is Emacs display-property interaction; each milestone has a
  stop gate before downstream implementation.
