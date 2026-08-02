# Policy Compilation Boundary

Superseded note: the historical hard-atom adjacency prohibition below was
removed by `task036`. Explicit atoms forbid only interior breaks; final-pass
reachability is owned by core K-P as recorded in
`20260802-final-pass-active-path-preservation.md`.

## Context

Configurable break policy could have been implemented by teaching every
planning caller about inline code, tokens, mode profiles, and local
overrides. That would have spread policy precedence across core DP,
buffer projection, C marshalling, showcase code, and diagnostics.

The accepted task034 contract required inline code to default to legal
wrapping without dictionary hyphenation, explicit `ekp-no-break` to remain
the only manual hard atom, token categories to have independent defaults,
and the existing 15-argument / 15-field C boundary to stay stable unless
architecture review was reopened.

## Decision

Compile policy before dynamic programming. The buffer layer resolves public
configuration and annotates exact structural intervals; the core turns those
intervals into the existing hyphenation, no-hyphen, and forbidden-break
vectors before DP. The semantic plan still preserves the original source
string and source properties for renderers and diagnostics.

This keeps policy as input classification, not a second DP algorithm or a
post-projection correction.

## Rejected alternatives

- Make inline code a hard atom by default. This caused the visible orphan
  glyph failure and made ordinary code-like prose too rigid.
- Add a broader C schema for policy objects. The current vectors already
  express the required break permissions, and expanding the ABI would add
  release risk without a new semantic capability.
- Handle inline faces in the buffer projector after planning. Projection
  would disagree with the semantic plan and C parity evidence.
- Copy mode profiles into buffer locals automatically. That makes consulted
  defaults look like user overrides and hides precedence bugs.

## Consequences

Policy precedence is explicit: region properties, then explicit local values,
then mode profiles, then global defaults. Block faces stay paragraph-level
verbatim; inline faces affect only their exact spans. Manual `ekp-no-break`
remains session-local and strictly stronger than automatic policies.

The C boundary remains stable, but every new policy that affects break
legality must be reflected in the compiled paragraph inputs before DP. GUI
verification must keep separate examples for automatic inline wrapping,
manual hard atoms, and verbatim blocks so later regressions cannot pass by
collapsing all three concepts into one rendering path.

## Performance root causes

The first performance boundary was paragraph identity. The semantic signature
included policy measure unconditionally, so width-only policy context rebuilt
paragraphs even when no break rule actually depended on measure. The fix
removed measure from paragraph semantic identity and confined measure-sensitive
work to the policy analysis tier.

The second boundary was property-run identity. Clearing volatile properties
could split otherwise equal filtered property runs, making equivalent source
look different to the cache. Canonicalizing adjacent equal filtered intervals
restores the intended stable identity.

The third boundary was policy analysis order. Full policy analysis ran before
paragraph-cache hits, so repeated layout requests paid the analysis cost even
when the paragraph itself was reusable. The implementation now uses a bounded
two-tier full-analysis cache keyed by clean source, canonical layout and raw
face policy intervals, policy signature, and width context. The measure tier
is used only for actual width-sensitive no-break or overflow candidates.

The fourth boundary was semantic plan assembly. Repeated same-paragraph and
same-width requests duplicated plan construction. A bounded per-paragraph plan
cache stores the reusable plan and returns consumer-owned copies of every
plan-owned mutable payload so later renderer or buffer mutations cannot poison
the cached plan. The paragraph pointer remains intentionally shared because
the older paragraph cache owns that lifetime and append planning uses stable
paragraph identity.

G006 cleanup exposed two ownership boundaries during independent review.
First, shallow semantic-plan copies were not enough: nested line/gap/glue
payloads could still alias the cached plan. Second, malformed non-nil C
results were too soft if they reached Elisp fallback. G007 resolved those
first blockers by deep-copying plan-owned mutable payloads at the consumer
boundary and by making `ekp-backend-contract-error` the contract for malformed
non-nil single or batch backend results. Nil whole results and nil per-item
break results remain the only C soft-failure path that may fall back to Elisp.

G007 final review then found one remaining string-leaf alias: `copy-tree`
does not copy strings inside the layout context, so a returned context could
mutate a dynamic policy suffix and poison the cached context. G008 resolves
that final blocker with the same recursive context copier at both context
snapshot/cache-key creation and returned-plan copying. The copier owns conses,
vectors, and strings; `para` remains the only deliberate shared exception.

The explicit hard-atom adjacency rule is also documented here because it is
part of policy compilation, not the emergency fallback. Before DP runs, the
ordinary boundary immediately before a following explicit `ekp-no-break` atom
is forbidden. The later G009 line-breaking correction is narrower and belongs
to core K-P: ordinary underfull final-pass candidates receive finite
emergency stretch and remain normal badness/demerits candidates, while fixed
emergency transitions are reserved for truly overfull hard/atomic runs.

## Verification and remaining gate

Recorded evidence so far: G002 focused core policy and C parity gates pass;
G003 buffer policy ownership and public controls pass; G004 GUI verifier ERT
7/7 and clean single-window evidence at `/tmp/ekp-g004-evidence.Tp77dW`
passes 12/12 checkpoints with automatic inline wrapping, source-space
internal breaks, explicit hard atom, block verbatim, exact source, zero
overlays, C active, and no stale policy projection. G005 records layout
parity true in the formal four-interleaved evaluator; core baseline p50/p95
38.6679/51.6782ms versus candidate 21.7102/33.0040ms for
43.8549%/36.1354% gains; resize baseline p50/p95 43.3831/55.6250ms versus
candidate 22.0919/32.8202ms for 49.0773%/40.9973% gains; both candidate p95
values under 50ms and both gain sets at least 20%.

The live evaluator records the source-instrumented locked goal as the known
`validation_failed` debt, with parity, zero-work, GC, conflict, and
all-width-nonregression checks true. Current C p95/p99 is 26.449/26.740ms,
a 76.65%/77.23% improvement. Current Elisp p95/p99 is 49.940/52.017ms, a
91.64%/91.35% improvement. This is consistent with historical open
`issue018` and is not a regression. The byte-compiled production public path
passed three runs with zero GC: C append p99 1.361-1.368ms, C hard p99
1.876-1.891ms, Elisp append p99 1.692-1.775ms, and Elisp hard p99
2.100-2.230ms.

G005 repository gates are complete: default ERT 262/262; seeded permuted ERT
seed 20260728 255/255; isolated per-test process suite exit 0; targeted
alias guard passed; property fuzz 300/300; warning-as-error byte
compilation; pinned package-lint at
`35996f478d81e51dae4fa30d051f741895d07399` exit 0 with only an external
obsolete warning from the local names dependency; empty checkdoc; release,
49-entry dictionary manifest, pinned dictionary update, shell syntax, CI
YAML, and diff-check passed; portable/native/debug/sanitize C builds were
warning-clean; module 1.6/4-thread smoke passed; and focused C passed 19/19
including the 15-argument/15-field boundary.

G006 cleanup edited only `ekp.el` and `ekp-buffer.el`, removing redundant
policy/cache code without changing the locked behavior. Its first independent
review found the shallow-copy and C-boundary blockers recorded above, so G006
is review-blocked historically rather than final-clean.

G007 resolved the first review blockers, then G008 resolved the final
string-leaf context blocker. Fresh G008 gates: formal resize parity true;
core baseline p50/p95 36.595/48.357ms versus candidate 23.266/36.363ms for
36.42%/24.80% gains; resize baseline p50/p95 41.443/53.541ms versus
candidate 23.761/36.889ms for 42.67%/31.10% gains; ERT 268/268; property
fuzz 300/300; release gate pass; and the byte-compiled current public path
reports zero GC with C append/hard p99 1.440-1.464/2.022-2.050ms and Elisp
append/hard p99 1.648-1.687/2.136-2.335ms, all below 16ms. After the final
warning-clean test-fixture and C README remediation, independent code review
returned APPROVE and the subsequent architecture review returned CLEAR.

This record explains the implementation boundary. `task034` is closed for
developer work and independent review. `issue021` closed after the user's
2026-08-02 visual confirmation.
