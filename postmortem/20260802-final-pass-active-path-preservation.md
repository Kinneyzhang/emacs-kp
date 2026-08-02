# Final-Pass Active-Path Preservation

## Context

The narrow showcase could still isolate a CJK glyph after ordinary underfull
final-pass lines were moved onto fixed emergency stretch and normal K-P
badness. The strict pass, renderer, policy compiler, units, and source
projection were not choosing those lines. The failure remained in the core
final-pass transition model shared by the Elisp and C implementations.

Two compensations had accumulated around the symptom: a high fixed-cost
transition described as hard/atomic-specific, and a rule forbidding the
otherwise legal boundary immediately before a manual hard atom. Both encoded
content and adjacency into a reachability problem.

## Root Cause

Fixed emergency stretch solves one problem: it gives ordinary underfull
candidates finite flexibility so adjustment ratio, badness, fitness, and
demerits can compare them globally. It does not solve the separate case where
an overfull candidate would remove the last active final-pass path.

The implementation treated that second case as a special hard-atom line with
a large invented cost. That is not the role of TeX's
`artificial_demerits`. In the final pass TeX preserves reachability when the
active list would otherwise lose the remaining path; the artificial break
adds no demerits. It is a control-flow safeguard, not another line-quality
score and not a token-class rule.

The upstream reference is the pinned TeX Live `tex.web` at commit
`1a25c04b49317750330b4cf95994ea0d08f9d5ec`:
https://raw.githubusercontent.com/TeX-Live/texlive-source/1a25c04b49317750330b4cf95994ea0d08f9d5ec/texk/web2c/tex.web

## Decision

Keep the strict pass unchanged.

In the final pass:

1. Ordinary underfull candidates receive the resolved fixed
   `ekp-emergency-stretch-pixel` and use the normal adjustment-ratio,
   badness, fitness, and demerits pipeline.
2. Remember the best provisional overfull candidate for each breakpoint.
3. If no normal state exists at that breakpoint and no non-overfull active
   candidate survives to it, install the provisional path with tight fitness
   and zero incremental demerits.

The artificial path therefore cannot beat a surviving normal path. It exists
only at the path-extinction boundary. The Elisp 1D, Elisp
looseness/parshape, and C paths implement the same state transition.

Delete the atom-adjacency prohibition. An explicit `ekp-no-break` interval
forbids only its interior breaks; otherwise legal boundaries immediately
before and after it remain legal. An overwide atom stays intact but may share
an overflow line with preceding ordinary content.

## Test-Oracle Failure

The isolated-CJK oracle originally checked only the lines returned by the
planner. An empty plan contained no isolated line, so the assertion could
pass while the core had failed to cover any source. The oracle now first
requires a nonempty, contiguous plan whose ranges cover the entire source,
then checks for isolated CJK lines. A direct regression also proves that an
artificial line adds zero demerits to the preceding path.

## Rejected Alternatives

- A CJK orphan penalty would encode a visible sample instead of the K-P
  invariant.
- Unit suffix or hard-atom scoring would make content classification affect a
  content-independent final-pass safeguard.
- Forbidding the boundary before an atom would delete a legitimate K-P choice
  and compensate in policy compilation for a core DP bug.
- Renderer reshaping would make display diverge from the semantic plan.
- A large artificial cost would turn reachability into a competing heuristic
  and differ from TeX's zero-increment behavior.

## Consequences

The public configuration remains the fixed emergency-stretch dimension; no
new orphan, unit, or atom-adjacency option exists. The public C contract stays
at 15 paragraph fields and 15 entry arguments. Renderer glue distribution
uses the selected line's actual rest and actual glue-set proportions.

Focused regressions pass 8/8, emergency selection passes 10/10, the
core/buffer/GUI oracle passes 8/8, full ERT passes 288/288, seeded and isolated
suites pass 279/279, fuzz passes 300/300, and all Elisp/C build, static,
release, and performance gates pass. Reviewed 42.78-second dynamic GUI
evidence at `/tmp/ekp-g009-evidence-retry.UOpPNp` covers
480→168→280→168 plus a no-hyphen→normal policy transition and returns
`VERDICT=PASS` with one window, exact source, zero overlays/stale spans,
active C, and no isolated CJK source line.
