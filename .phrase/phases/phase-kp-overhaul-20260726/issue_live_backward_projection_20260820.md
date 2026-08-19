# Issue: Backward Live Projection Stability 2026-08-20

## issue023 [ ] Backward deletion can republish a stable projection

- **Status:** Runtime fix verified by `task038`; user-visible confirmation
  remains pending.
- **Summary:** Live row crossing treats forward typing and backward deletion
  identically. After a forward crossing, deleting a suffix into the previous
  native row can republish earlier rows and visibly move already typed text.
- **Expected vs Actual:** The current live-edit contract preserves unaffected
  committed projection anchors during local tail shrink. Actual
  `ekp-buffer--live-row-crossed-p` triggers a full prefix publication in both
  directions.
- **Related:** `postmortem/20260730-incremental-live-append-ownership.md`,
  `ekp-buffer.el`, `task038`.
- **Fix:** A live row crossing now commits only when the native row moves
  forward; backward tail shrink keeps the transaction local.
- **Verification:** Public edit-path regression and source-first full/random/
  isolated ERT all pass.
- **Resolved At:** Pending user-visible confirmation.
- **Resolved By:** Developer implementation and verification.
- **Commit:** `6a8c7e0`.
