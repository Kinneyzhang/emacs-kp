# Keep Narrow Append Optimization Separate from Frontier Correctness

## Context

Task029 removes point from live-layout ownership. Its point-motion workload
is now a zero-work path, but the same benchmark exposed a different
problem: at a synthetic width of 80 pixels, every appended character
creates a new whole-hard-line cache key and therefore invokes the complete
planner again.

The earlier task028 record reported a 6.399 ms GC-excluded append p99. That
number is not reproducible with the current checked-in benchmark. Repeated
audits instead measured roughly 33–85 ms, while point motion remained near
0.03 ms with zero planner and cache calls.

## Decision

Close task029 on its correctness and navigation-performance invariants, and
track unique-state narrow append latency independently as issue018/task030.

This is not permission to weaken live layout. Task030 must retain:

- exact whole-hard-line KP semantics;
- exact cache identity;
- immediate source-edit publication;
- source-clean text-property display;
- zero-work point-only motion;
- unchanged core DP/C ABI/schema contracts unless a later architecture
  decision explicitly proves a core change is necessary.

## Why

The two paths have different owners and different evidence:

- point motion previously performed unnecessary display work and is fixed
  by source-edit-owned frontier state;
- unique append legitimately changes the source and currently pays for a
  new whole-hard-line plan.

Combining them would either delay a verified correctness fix or encourage a
wrong-layer shortcut such as stale reuse, debounce, skipped publication, or
point-dependent planning.

## Consequences

- Task029 may complete when its automated, GUI, and independent-review
  gates pass.
- Current documentation must report the narrow-width miss rather than the
  stale 6.399 ms claim.
- Issue018 remains open until task030 produces equivalent layout results
  within an explicitly documented width/latency envelope.

## Rollback

There is no runtime change in this decision record. If later optimization
cannot preserve exact layout and editing semantics, abandon that
optimization and leave issue018 open rather than weakening the contract.
