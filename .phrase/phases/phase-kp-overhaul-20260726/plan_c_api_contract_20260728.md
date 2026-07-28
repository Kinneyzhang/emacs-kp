# Plan: C API and Arithmetic Contract

## Scope

Resolve the P2-02 half of `issue005`: validate the complete public schema
before extraction, make caller errors explicit, and eliminate signed
overflow for valid inputs.

## Resolution Path

1. Lock malformed single/batch vectors, bad penalty scalars, out-of-range
   integers, and a valid extreme-width calculation with direct C red tests.
2. Validate vector types, n/n+1 lengths, scalar types, positivity, and
   signed 32-bit range before allocating or extracting.
3. Register one `ekp-c-invalid-input` condition for caller errors; retain
   nil only for allocation/no-result.
4. Widen DP line-metric, adjustment, flexibility, and rest intermediates to
   64-bit without changing the 15-field ABI.
5. Propagate module signals through the enabled Elisp dispatcher.
6. Build release/sanitizer profiles and run focused/full ERT plus fuzz.

## Non-goals

- No new algorithm or C tokenization.
- No change to valid 15-field layout or result shape.
- Build command/profile portability is the next atomic task.

## Rollback

Restore permissive extraction, int32 intermediates, C 1.5 requirement, and
the error-swallowing dispatcher. Valid persisted data is unaffected.
