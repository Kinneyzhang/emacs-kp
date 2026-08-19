# Issue: Natural Live Editing 2026-07-29

## issue014 [x] Near-edge editing publishes premature KP layout

- **Status:** Closed by user re-audit on 2026-08-13. Implementation and
  independent verification were complete.
- **Summary:** With `ekp-auto-justify-mode` enabled, typing near the right
  edge can publish several visual KP breaks and a discretionary hyphen
  while the user is still composing the current hard paragraph. The result
  does not feel like ordinary Emacs editing.
- **Environment:** Text-property live-flow renderer after `task024`;
  graphical Emacs 30.2; mixed Latin/CJK prose.
- **Repro:**
  1. Enable `ekp-auto-justify-mode` in a graphical window.
  2. Type mixed Latin/CJK prose continuously toward the right edge.
  3. Observe the transition one character at a time as Emacs naturally
     wraps.
- **Expected vs Actual:**
  - Expected: the source tail containing point is displayed exactly as
    ordinary Emacs would display it. Before natural wrap, EKP publishes no
    layout property there. After natural wrap, EKP may align only the row
    the user has left. A complete KP pass runs only after the hard
    paragraph ends or point leaves it.
  - Actual: bounded KP flow selects and publishes display breaks during
    typing. `natural-p` suppresses glue projection only; it still permits
    prefix and break/hyphen projection.
- **Root Cause:** The live model treats an unfinished paragraph as a
  continuously maintained partial KP plan. “Natural active line” is only
  a glue exception inside that plan, not native Emacs display ownership.
- **Required Outcome:** Replace partial live KP with three explicit states:
  a natural active tail, locally aligned rows already left by natural
  wrapping, and a complete KP paragraph after completion. No live row may
  add a discretionary hyphen or replacing visual break.
- **Fix:** `task025` deletes the partial-KP
  anchor/lookahead/convergence/push-pull state machine. The active hard
  paragraph is restored to native display before each source edit. After
  the edit, EKP derives completed native visual rows from Emacs redisplay
  and projects only their internal gap widths. The point-containing tail
  stays native, and only hard-paragraph completion or paragraph exit runs
  the complete KP planner. Live projection cannot install replacing
  breaks, prefixes, or discretionary hyphens.
- **Verification:** Focused live contract ERT passes 4/4; buffer ERT passes
  75/75; default, seeded-permuted, and isolated full ERT pass 164/164; fuzz
  passes 300/300. Warning-as-error compilation, checkdoc, pinned
  package-lint, release, dictionary, pinned-source, static ownership,
  no-overlay, and diff gates pass. The retained clean Emacs 30.2 dynamic
  artifact at `/private/tmp/ekp-native-live-2dYu2U` proves native
  mixed-text wrap, exact deletion restoration, and the sole full-KP
  transition after hard newline; every immediate and redisplay checkpoint
  passes, with zero live owned breaks and no black recording segment.
- **User Confirmation:** Provided by user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; implementation complete.
- **Commit:** — (documentation-only closure).
