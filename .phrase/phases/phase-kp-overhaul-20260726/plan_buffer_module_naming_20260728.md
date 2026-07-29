# Plan: Buffer Module Naming 2026-07-28

## Goal

Rename the editor-facing buffer integration module from `ekp-region` to
`ekp-buffer` so its file, feature, customization group, configuration
variables, private namespace, tests, and documentation match the
responsibility it actually owns.

## Intended User Path

- Load buffer integration with `(require 'ekp-buffer)`.
- Keep public operation names whose suffix describes their real operand:
  `ekp-justify-region`, `ekp-unjustify-region`, `ekp-justify-buffer`,
  `ekp-unjustify-buffer`, and the region protection commands.
- Keep `ekp-auto-justify-mode`, `ekp-org-setup`, `ekp-markdown-setup`,
  `ekp-refill-paragraph`, and `ekp-diagnose`; they already follow the
  package-level `ekp-` namespace and describe behavior rather than the old
  module filename.
- Rename the module-owned public configuration surface and all private
  implementation/test symbols from `ekp-region*` to `ekp-buffer*`.

## Scope and Order

1. Rename `ekp-region.el` to `ekp-buffer.el` and
   `tests/ekp-region-tests.el` to `tests/ekp-buffer-tests.el`.
2. Rename the provided feature, customization group, module-owned settings,
   internal functions/state, and test namespace.
3. Update test runners, GUI/showcase helpers, CI, contributor commands, and
   release checks that name the files or symbols.
4. Update current public/developer/audit documentation and add an
   Unreleased breaking migration note.
5. Preserve historical phase/postmortem wording; add a new decision record
   instead of rewriting history.

## Compatibility Decision

This is an explicit breaking rename requested for the current Unreleased
line. Do not retain `ekp-region.el`, provide `ekp-region`, define obsolete
aliases, or add variable aliases. The migration is one direct replacement:
`ekp-region` becomes `ekp-buffer`. Public region commands retain their
semantic names.

## Behavior Lock

- Existing full ERT suite: 130 tests before the rename.
- Existing buffer tests cover exact round-trip, save failure, isearch,
  kill/yank, undo, mode lifecycle, resize/lazy reflow, protection commands,
  and whole-buffer/DWIM paths.
- The rename changes lookup names only; rendered output and persisted bytes
  must remain identical.

## Cleanup and Fallback Inventory

- Smell: the `ekp-region` namespace names only one operation scope while
  the module owns the entire buffer/editor lifecycle.
- No dead-code, duplication, error-handling, dependency, or UI pass is in
  scope.
- The selected-window fallback in width calculation is a grounded
  undisplayed-buffer boundary and remains unchanged.
- No masking fallback, swallowed error, broad compatibility shim, or
  escalation candidate was found.

## Validation

- Focused load and buffer ERT through `ekp-buffer`.
- Default, seeded-permuted, and fresh-process-isolated full ERT.
- C/Elisp fuzz to prove rendered behavior remains unchanged.
- Warning-as-error byte compilation, checkdoc, package-lint/release gates,
  shell syntax, and `git diff --check`.
- Scan active runtime, tests, CI, and current documentation for stale
  `ekp-region` / `ekp-region-tests` references; historical records are the
  only allowed occurrences.
- Read the complete diff before closing the task.

## Stop Condition

The new feature loads from `ekp-buffer.el`, all renamed tests and automation
pass, current documentation presents only the new API, historical references
are explicitly contextual, and no behavior or unrelated source changed.

## Result

Completed as `task017`. The approved direct rename is mechanically
equivalent to the prior runtime/test implementation apart from the
customization parent and explicitly private protrusion helper. All validation
listed above passed; no compatibility shim or unrelated cleanup was added.
