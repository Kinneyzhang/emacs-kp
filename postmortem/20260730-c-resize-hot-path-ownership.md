# C Resize Hot-Path Ownership

## Context

The reported 60–70 ms resize number was initially described as C dynamic
module performance. The measured operation actually crossed core planning,
the Emacs module boundary, projection construction, text-property
publication, and live-prefix replacement.

## What the profile showed

The portable C call averaged roughly 1.8 ms in the frozen four-round
baseline. Repeated paragraph lookup and plan construction dominated the core
path, while redundant gap measurement and property publication dominated
the buffer projection path. Resize also published the active paragraph
statically before replacing it with the live prefix.

An apparent remaining 60–70 ms GUI spike was a separate phenomenon:
`set-window-margins` allocated in the Emacs window system and a later EKP
callback paid the resulting GC pause. The EKP mutator stayed below the
50 ms contract when total, GC, and mutator time were recorded separately.

## Decision

Keep the C ABI and Knuth-Plass algorithm unchanged. Move prepared paragraph,
DP, and natural gap geometry through the existing core-to-buffer path,
remove only true no-op projection records, consolidate owned property
publication, and make resize publish the active paragraph only through its
live owner.

## Rejected alternatives

- Micro-optimizing C DP: it was not the dominant measured layer.
- Increasing resize debounce or skipping widths: this hides work and makes
  projection stale.
- Reusing approximate plans across widths: this breaks exact layout parity.
- Raising the global GC threshold: this changes process-wide behavior and
  misattributes window-system allocation to EKP.

## Consequences

The four-round gate reduces core p95 from 42.006 ms to 27.687 ms and complete
resize p95 from 46.611 ms to 27.487 ms, with 34.09% and 41.03% improvements.
Frozen C, candidate C, and Elisp layout hashes remain identical. External GC
can still increase wall-clock totals, but it is explicitly observable and
is not concealed by runtime policy changes.

## Verification and rollback

The evaluator, normal/permuted/isolated ERT suites, fuzzing, warning-as-error
builds, static and release gates, and reviewed temporal GUI evidence pass.
Rollback is a direct revert of the prepared-data and publication changes;
no compatibility layer or data migration is required.
