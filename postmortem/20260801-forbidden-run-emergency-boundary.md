# Forbidden-Run Emergency Boundary

Superseded note: this record explains the earlier rigid-atom repair. Neither
the fullest-prefix fallback nor the later atom-adjacency prohibition is
current. The accepted final-pass model is recorded in
`postmortem/20260802-final-pass-active-path-preservation.md`.

## Context

At 280px, the showcase paragraph before an overwide `ekp-no-break` atom
collapsed into one-glyph CJK lines. The buffer projector reproduced the
semantic plan exactly, so the visible failure was owned by core line breaking,
not redisplay, overlays, resize timing, or projection.

## What the trace showed

The strict pass correctly found no valid candidate where widths jumped from
an underfull CJK prefix to an overfull rigid atom. The emergency pass retained
only the first permitted candidate from each state. That rule guarantees
reachability for arbitrary hard input, but here it made the atom boundary
reachable only through repeated one-glyph emergency transitions.

Replacing that rule globally with the latest underfull candidate was also
wrong. It fixed the screenshot but changed emergency layouts for ordinary
Latin text: discretionary hyphens disappeared and existing DP-parameter
sensitivity changed. Three established regressions failed independently,
while the unmodified baseline passed them.

## Decision

Keep the original first-candidate emergency transition. In the emergency pass
only, remember the latest permitted underfull candidate. Use that additional
candidate only when one or more forbidden break positions are crossed and the
next permitted candidate is already overfull.

This assigns the exception to the event that creates it: a rigid or otherwise
forbidden run skipping the valid width range. The Elisp 1D path, looseness/
parshape path, and C engine implement the same state transition.

## Rejected alternatives

- Compensate in `ekp-buffer.el`: projection would diverge from the semantic
  plan and the public string renderer would remain wrong.
- Break the protected atom: this violates the `ekp-no-break` contract.
- Replace every first-candidate emergency with the fullest underfull one: this
  changes unrelated emergency layout, hyphenation, and cost sensitivity.
- Add resize debounce or cache invalidation: the failure is deterministic in
  a fresh batch plan and is unrelated to time or stale state.

## Consequences

Rigid atoms remain intact, their ordinary prefix uses the fullest permitted
line, and source-clean projection is unchanged. Ordinary emergency behavior,
strict Knuth-Plass results, discretionary hyphenation, and DP parameter
sensitivity retain their previous semantics. The only additional DP state is
three fallback values plus a flag local to one candidate scan.

Automatic inline no-hyphen and explicit hard atoms are different contracts.
The later policy work keeps automatic inline code breakable at source spaces.
For explicit `ekp-no-break`, the ordinary boundary immediately before the
following hard atom is forbidden before DP; that adjacency rule prevents a
manual hard atom from stranding the preceding ordinary box. It is separate
from this postmortem's emergency forbidden-run fallback, which applies only
after a forbidden run skips the valid width range.

## Verification and rollback

The focused regression failed before the fix and passes in Elisp, C, and the
public buffer path. Normal, seeded-random, and per-test isolated 201-test
suites pass, as do 300 C/Elisp fuzz cases, warning-as-error Elisp compilation,
pinned package-lint, checkdoc, release/dictionary gates, and warning-clean
debug, ASan/UBSan, and portable C builds.

Reviewed 62.7-second evidence at
`/private/tmp/ekp-atom-gui-final-Beg8hb` covers nine checkpoints across
480→280→340→280 and returns PASS: the prefix stays on one screen line, the
atom remains intact, source is exact, overlays remain zero, the C engine is
active, and no black or transient vertical frame appears. Rollback is a
direct revert of the forbidden-run fallback and its tests; no API, schema,
cache, or stored-data migration is involved.
