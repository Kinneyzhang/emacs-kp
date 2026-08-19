# Issue: C Position Vector Validation 2026-08-20

## issue025 [ ] C direct API accepts invalid break positions

- **Status:** Closed by direct contract verification on 2026-08-20.
- **Summary:** The C boundary validates vector shape and signed integer type,
  but not the exclusive break-index range, monotonic ordering, or duplicate
  policy of hyphen/forbidden positions.
- **Expected vs Actual:** Malformed direct arguments should signal
  `ekp-c-invalid-input` per the C API contract. Current `[-1]` and `[99]`
  vectors return a layout result instead.
- **Related:** `ekp_c/ekp.c`, `ekp_c/ekp_kp.c`, `task040`.
- **Fix:** Shared preflight now rejects out-of-range, duplicate, and
  non-increasing vectors for both direct and batch calls.
- **Verification:** Direct/batch C ERT 15/15, portable warning-clean build,
  300-case fuzz, and Elisp parity pass.
- **Resolved At:** 2026-08-20.
- **Resolved By:** Developer implementation and contract verification.
- **Commit:** `6a8c7e0`.
