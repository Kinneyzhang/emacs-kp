# Issue: Buffer Width Validation and Failure Atomicity 2026-08-20

## issue022 [ ] Invalid buffer widths mutate or create invalid projection state

- **Status:** Runtime fix verified by `task037`; user-visible confirmation
  remains pending.
- **Summary:** `ekp-justify-region` accepts zero, negative, and non-integer
  widths without the string API's positive-integer validation. It clears the
  existing projection before a later type error, so a rejected request can
  destroy valid visible state.
- **Repro:** In a justified buffer, call `ekp-justify-region` with `0`, `-1`,
  `1.5`, or `"80"`.
- **Expected vs Actual:** Expected a preflight signal with the old projection
  unchanged. Actual zero creates a span with width zero; a type error after a
  prior projection leaves no spans.
- **Related:** `ekp-buffer.el`, `ekp.el`, `task037`.
- **Fix:** `ekp-justify-region` now validates the width before font-lock or
  projection clearing.
- **Verification:** Red/green focused ERT 1/1 and source-first full ERT
  294/294.
- **Resolved At:** Pending user-visible confirmation.
- **Resolved By:** Developer implementation and verification.
- **Commit:** `6a8c7e0`.
