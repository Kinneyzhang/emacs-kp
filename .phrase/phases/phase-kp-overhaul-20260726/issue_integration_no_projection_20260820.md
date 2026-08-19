# Issue: No-Projection Integration Lifecycle 2026-08-20

## issue027 [ ] Manual integrations remain installed with no projection owner

- **Status:** Closed by lifecycle verification on 2026-08-20.
- **Summary:** A manual justify request that produces no spans, such as an
  empty or foreign-display-only buffer, still installs EKP change hooks and
  the substring filter even though auto-mode is disabled.
- **Expected vs Actual:** Integrations should have one active owner: spans or
  auto-mode. Actual `spans=nil` still leaves the filter and hooks installed
  until a later change happens to trigger cleanup.
- **Related:** `ekp-buffer.el`, `plan_integration_lifecycle_20260728.md`,
  `task042`.
- **Fix:** Manual justification installs integrations only when a span or
  auto-mode owner exists.
- **Verification:** Empty/foreign-only lifecycle ERT 2/2 and full buffer
  suite pass.
- **Resolved At:** 2026-08-20.
- **Resolved By:** Developer implementation and lifecycle verification.
- **Commit:** `6a8c7e0`.
