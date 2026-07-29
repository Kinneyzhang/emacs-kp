# Let Native Redisplay Own Unfinished Text

## Context

The first text-property live engine correctly kept synthesized layout out
of the source character stream, but it still treated an unfinished hard
paragraph as a partial KP document. Stable anchors, lookahead, push/pull,
and convergence limited the amount of recomputation; they did not change
the user-visible ownership error.

Near the right edge, that engine could choose several visual breaks and a
discretionary hyphen while the user was still typing. Calling one row
“natural” only suppressed its glue adjustment. The surrounding partial KP
plan still controlled wrapping, so editing did not behave like ordinary
Emacs.

The product requirement is stricter: before the user naturally leaves a
visual row, EKP must have no opinion about its layout.

## Decision

Use three states with distinct owners:

1. The point-containing, underfilled source tail belongs entirely to native
   Emacs redisplay. EKP performs no KP planning and publishes no layout
   property there.
2. Once native redisplay has moved point to a later visual row, EKP may
   align internal gaps on rows already left behind. This projection uses
   only `space-width` and `min-width`; it cannot add `line-prefix`, a
   replacing break, or a discretionary hyphen.
3. Once a hard paragraph ends or point leaves it, EKP runs one complete KP
   pass. Only this state may publish planned visual breaks, indentation,
   and discretionary hyphens.

Before a source edit, the active hard paragraph is restored to native
display. If editing crosses into a previously committed row, that prefix is
therefore invalidated before Emacs changes the source. After the edit and
redisplay, completed native row boundaries are derived with
`vertical-motion`; gap targets are then computed for those fixed source
ranges. The row boundary itself remains Emacs-owned.

IME composition defers projection. Resize work is generation-checked.
Narrowing may release an inaccessible previous live paragraph but must not
modify it outside the accessible restriction. Foreign replacing display
ownership causes the affected row to remain natural rather than inviting a
second owner.

## Why This Is Simpler

The live engine no longer predicts how an incomplete paragraph will flow.
It has no lookahead limit, stable-line signature, anchor-creep rule,
pullback algorithm, or convergence state. Each transition follows a
visible editor event:

- still on the same native row: do nothing;
- native wrap completed a row: align that row's gaps;
- hard paragraph completed or was left: run full KP.

The completed-paragraph planner and the live editor now share only
semantic width/glue computations. They do not pretend to share the same
control flow.

## Alternatives Rejected

### Keep partial KP and tune the active-line exception

This retains the wrong owner. Suppressing glue on one chosen row cannot
prevent adjacent planned breaks or hyphens from appearing while the
paragraph is unfinished.

### Run full KP synchronously after every edit

Removing an idle timer does not make completed-paragraph optimization an
editing algorithm. It would still move unfinished text and would make
typing latency proportional to paragraph planning.

### Insert soft newlines or hyphen characters

That would restore the original source-pollution defect. Direct Elisp
character APIs, save, search, and syntax must continue to observe only
logical text.

### Use overlays for transient rows

The project has one text-property projection ownership model, and the user
explicitly excluded overlays. A second transient representation would
complicate invalidation without correcting the row-ownership rule.

## Consequences

- Typing before native wrap looks and behaves like auto layout is disabled.
- A committed live row can improve spacing without changing its native
  wrap boundary.
- Live editing deliberately has no discretionary hyphenation. Hyphens
  appear only in the completed paragraph's display projection and never in
  source text.
- Deleting back across a native wrap removes the committed row projection
  before the source edit, so Emacs immediately restores its natural flow.
- One authoritative graphical window determines native row boundaries.
  Text properties cannot represent simultaneous plans for different window
  widths.
- Automatic work remains bounded by the existing hard-paragraph limit;
  explicit refill is the opt-in path for an oversized paragraph.

## Verification

Behavioral ERT proves the three state transitions through the installed
before/after-change path, including underfilled tails, native wrap,
backward invalidation, hard newline, paragraph exit, IME, narrowing,
resize, foreign ownership, and teardown.

Static GUI probes prove exact `space-width`/`min-width` projection from
1–64 pixels, display-only completed-paragraph breaks/hyphens/indentation,
unchanged source, and zero overlays across scale, remap, fringe, and width
variants.

The retained dynamic artifact
`/private/tmp/ekp-native-live-2dYu2U` records native mixed Latin/CJK wrap,
deletion back across the boundary, and hard-paragraph completion. Every
immediate and post-redisplay checkpoint passes, and temporal review finds
no flicker, black segment, or non-Emacs frame.

## Rollback

Revert the native progressive live transition as one unit. Do not restore
the partial-KP state machine piecemeal or add a compatibility shim. The
semantic completed-paragraph planner and text-property projection layer
remain independently valid.
