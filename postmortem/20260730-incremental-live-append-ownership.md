# Incremental Live-Append Ownership

## Context

After stable live transactions removed per-key whole-paragraph planning,
only native visual-row crossings still paid the complete paragraph
preparation, dynamic program, plan construction, and projection cost.
At 80 pixels the old source benchmark attributed 51–187 ms p99 to these
permitted commits.

The C module itself was not the primary owner. In the frozen baseline its
80-pixel module call measured only 2.615/2.655 ms p95/p99, while paragraph
preparation and the surrounding plan dominated. A Rust module would still
enter through `emacs_env`, consume the same prepared vectors, and return the
same DP result.

## Decision

Keep the C module and make the existing ownership model incremental:

- a layout plan retains its prepared paragraph and exact layout context;
- a property-free plain-text append retokenizes from the last complete-word
  boundary instead of from paragraph start;
- derived prefix, break, protrusion, and gap data are copied only through the
  proven-stable box boundary and recomputed from that boundary onward;
- pure Elisp DP retains its state and resumes from the earliest old state
  that can still reach the first new break;
- C runs its full native DP over incrementally prepared vectors because its
  sub-millisecond candidate call is already cheaper than adding another ABI;
- the buffer replaces only the dirty source island and reuses common layout
  lines before publishing the changed suffix.

Any unsupported property, font, layout-context, tab/newline, parshape,
looseness, or token-boundary case returns nil and takes the unchanged full
planner. The fast path never approximates cache identity or layout output.

## Rejected alternatives

- **Rewrite the module in Rust:** it does not remove the Emacs C ABI or the
  Elisp-owned work, while adding Cargo, Rust toolchain, target, packaging,
  and Windows support obligations.
- **Move the whole planner behind the module boundary:** font measurement,
  text properties, and editor transaction ownership belong to Emacs; moving
  them would create a larger, less stable ABI.
- **Reuse stale prefix decisions:** a suffix can change the globally optimal
  KP path, so only prepared data and reachable DP states may be reused.
- **Debounce, skip, or delay commits:** this hides latency by making display
  state stale and violates the synchronous live-edit contract.

## Correctness trap

The first boundary box is dirty even when every earlier box is unchanged.
Its break permission depends on both neighboring boxes, so copying through
that position preserved a stale forbidden break. The final implementation
copies strictly before the stable boundary and recomputes break data from
the boundary. A regression fixes this ownership rule.

## Results

The four-round frozen source/instrumentation matrix preserves exact
baseline/C/Elisp source and projection hashes, zero conflicts, valid
GC-excluded samples, zero-work ordinary keys, and non-regression at
64/80/96/128/160 pixels. At 80 pixels:

- C improves 77.78% p95 and 78.37% p99 to 25.490/25.785 ms;
- Elisp improves 92.54% p95 and 92.03% p99 to 43.860/47.578 ms;
- the C module itself falls to 0.697/0.701 ms p95/p99.

Those source-instrumented values still miss the locked 16 ms absolute gate.
The production-shaped byte-compiled public path is materially different:
three repeated runs measure append p99 at 1.158–1.326 ms for C and
1.429–1.438 ms for Elisp, with no GC during the samples. Both measurements
remain documented; the stricter goal is not silently weakened.

## Known design debt

At a 558-pixel GUI width, deleting a suffix after a forward row crossing can
republish a backward layout and violate the existing “preserve typed
projection” visual invariant. The frozen baseline reproduces the same
behavior, and current ERT explicitly treats a backward wrap crossing as a
commit. This is not caused by incremental append work and needs a separate
semantic decision before a narrow tail-shrink predicate is changed.

## Verification and rollback

Exact append-chain equivalence is covered across C and Elisp, varied widths,
unsafe fallback contexts, and the stable-boundary regression. Normal and
permuted 199-test suites, 300 fuzz cases, warning-as-error compilation,
portable C warnings/tests, release checks, and reviewed temporal GUI
evidence pass.

Rollback is a direct revert of retained plan/paragraph state, incremental
append preparation/DP, dirty-island reconstruction, and one-pass C integer
validation. No data migration, compatibility shim, or cache conversion is
required.
