# Validate the Whole C Boundary Before Entering the Algorithm

## Context

The C entrypoint indexed and extracted fields piecemeal. A short vector
could leave a pending Emacs exception while later API calls continued, and
large integers were silently clamped. Even valid int32 values could overflow
when line width, protrusion, and prefix differences were combined in int32.
The Elisp adapter then swallowed every module signal and recomputed, hiding
backend contract bugs.

## Decision

Treat the 15-field paragraph as one schema. Before allocation or extraction,
validate every vector field, all n/n+1 lengths, scalar type/range, and
positive width. Batch input preflights every paragraph before processing
any of them. Caller violations signal `ekp-c-invalid-input`; nil is reserved
for resource/no-result failure.

Keep public values int32 for the stable ABI, but use int64 for all line
metric, adjustment, flexibility, and remaining-space intermediates. The
Elisp adapter falls back only for nil and lets module signals propagate.

## Alternatives Rejected

### Clamp arbitrary Lisp integers

Clamping silently changes caller data and still leaves arithmetic overflow
after multiple valid int32 operands are combined.

### Check pending exit after extraction

It prevents some crashes but still calls module functions while an error is
pending and produces inconsistent error types.

### Catch every module error and recompute in Elisp

That makes user output look robust while hiding a broken enabled backend
from tests and maintainers.

### Change every public array to int64

The inputs themselves fit pixel-scale int32; only expressions combining
them need wider representation. An ABI expansion adds no current value.

## Consequences

- Direct caller mistakes have one explicit condition.
- Batch validation is atomic.
- Valid extreme int32 inputs no longer overflow.
- Allocation/no-result remains recoverable.
- C 1.6 must be rebuilt before Elisp enables the accelerator.

## Verification

Six direct boundary controls failed 0/6 before the change and pass 6/6
after it. A forced public dispatcher signal failed 0/1 then passes 1/1.
Release and sanitizer builds compile warning-free, full ERT passes 116/116,
and C/Elisp fuzz passes 300/300. macOS platform policy prevents loading the
ASan runtime into the signed Emacs process; Linux sanitizer CI remains the
runtime gate.

## Rollback

Restore C 1.5 extraction, int32 intermediates, and broad Elisp catch. No
saved data or user configuration needs migration.
