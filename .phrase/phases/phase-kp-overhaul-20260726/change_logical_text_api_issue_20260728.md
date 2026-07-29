# Change Log: Logical Text API Issue 2026-07-28

## issue011

- **Add** —
  `.phrase/phases/phase-kp-overhaul-20260726/issue_logical_text_api_20260728.md`
  - Recorded that direct Elisp buffer APIs observe EKP's physical layout
    representation.
  - Captured the verified physical-vs-logical extraction evidence, root
    cause, required outcome, and unresolved architecture questions.
  - Explicitly deferred implementation and avoided selecting a design or
    creating an execution task.

- **Modify** — `.phrase/docs/ISSUES.md`, `.phrase/docs/CHANGE.md`
  - Added the open issue and linked this documentation-only change.

## Behavior and Risk

- Runtime code and behavior are unchanged.
- No implementation plan, compatibility promise, or solution architecture
  was approved.
- The issue remains open until the user chooses to resume design work.

## Validation

- Confirmed `issue011` is the next unused global issue ID.
- Confirmed the issue index link resolves to the phase detail.
- `git diff --check` passes.
