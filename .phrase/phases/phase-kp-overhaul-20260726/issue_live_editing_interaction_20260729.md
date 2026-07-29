# Issue: Live Editing Interaction Regressions 2026-07-29

## issue012 [ ] Reprojection activates an inactive mark

- **Status:** Runtime fix and repository verification are complete. The
  issue remains open until the user confirms the visible behavior.
- **Summary:** Changing width in `ekp-showcase` can highlight text even
  though the user did not activate a region.
- **Environment:** Text-property renderer after `task022`; Emacs 30.2.
- **Repro:**
  1. Open `ekp-showcase`.
  2. Move point so the buffer has an existing but inactive mark.
  3. Press `-`, `+`, Left, or Right to change the projection width.
  4. Observe a highlighted region between point and the old mark.
- **Expected vs Actual:**
  - Expected: width changes preserve point, mark position, and whether the
    region is active.
  - Actual: the old mark becomes active during reprojection.
- **Investigation:** `ekp-justify-region` restores every existing mark with
  `set-mark`. That command activates the mark even when it was inactive
  before layout. The showcase exposes the central command defect because
  every width key calls the same reprojection path.
- **Root Cause:** The projection command restores the mark's position but
  does not preserve the independent `mark-active` editor state.
- **Required Outcome:** Restore the mark marker without activating it, and
  prove both the central command and showcase width flow leave an inactive
  mark inactive.
- **Fix:** `task023` now restores the existing mark marker directly and
  restores `mark-active` independently. It never calls `set-mark` during
  reprojection.
- **Verification:** The focused regression failed before the fix because
  `mark-active` became non-nil, then passed after the owner-layer change.
  A clean full-screen GUI width-key run preserved point 120, mark 20,
  `mark_active=false`, `region_active=false`, and zero overlays before,
  immediately after, and after redisplay.
- **User Confirmation:** Pending after the visible fix is delivered.
- **Resolved At:** Unresolved.
- **Resolved By:** Pending.
- **Commit:** Pending.

## issue013 [ ] Active-line edge whitespace is hidden

- **Status:** Runtime fix and repository verification are complete. The
  issue remains open until the user confirms the visible behavior.
- **Summary:** With `ekp-auto-justify-mode` enabled, a newly typed trailing
  space or tab can appear to do nothing. It becomes visible only after a
  following non-whitespace glyph is typed. The same defect affects newly
  typed leading whitespace and whitespace exposed by deletion.
- **Environment:** Text-property renderer after `task022`; Emacs 30.2.
- **Repro:**
  1. Enable `ekp-auto-justify-mode` in a non-empty paragraph.
  2. Move to the paragraph end and type one space.
  3. Inspect the inserted source character or type one ordinary glyph.
- **Expected vs Actual:**
  - Expected: every command-loop insertion is visible immediately on the
    natural active line.
  - Actual: the source space exists immediately, but EKP owns
    `display ""` on it until another glyph makes it an interior character.
- **Investigation:** The live renderer correctly asks `ekp--project-line`
  to leave the active line's glue natural, then separately applies static
  paragraph-edge cleanup. Because the KP plan strips leading and trailing
  whitespace from its content bounds, that cleanup hides active-line edge
  whitespace after every edit.
- **Root Cause:** Static completed-paragraph edge policy leaks into the
  unfinished active-line projection. The active line is only partly
  natural.
- **Required Outcome:** Keep all source characters on the point-containing
  active line natural, including leading/trailing spaces and tabs. Preserve
  static edge cleanup on committed lines.
- **Fix:** `task023` prevents static paragraph-edge hiding from running on
  the active line. When the first planned line is active, live cleanup also
  starts at the source span beginning so an old hidden leading edge cannot
  survive.
- **Verification:** Focused mark/edge regressions failed 0/3 before the
  fix and passed 3/3 after it. The adjacent public-command matrix covers
  consecutive whitespace/backspace, yank, newline, undo, active region,
  overflow/pullback, paragraph exit, resize, IME, and teardown. In the
  clean GUI run, one space advanced point 67→68 and cursor x 462→469 in
  the same action; the source space had neither EKP-owned nor public
  replacing display immediately or after redisplay. Typing and deleting a
  following glyph left that state unchanged.
- **User Confirmation:** Pending after the visible fix is delivered.
- **Resolved At:** Unresolved.
- **Resolved By:** Pending.
- **Commit:** Pending.

## Related Audit Scope

`task024` checks the same ownership boundary across character insertion,
consecutive whitespace, deletion/backspace, newline, yank, undo, point,
mark/region, visual-break boundaries, paragraph entry/exit, resize, and
mode teardown. New concrete defects receive new issue IDs rather than being
silently folded into these two symptoms.

## Verification Evidence

- Buffer ERT: 79/79.
- Full ERT: 170/170 in default order and 170/170 with seed `20260729`;
  every test also passed in a fresh Emacs process.
- C/Elisp property fuzz: 300/300.
- Warning-as-error compilation: production files plus
  `tests/ekp-buffer-tests.el`.
- checkdoc, pinned package-lint, release, dictionary, shell syntax,
  artifact, stale-name, no-overlay, and diff checks pass.
- Dynamic artifact:
  `/private/tmp/ekp-interaction-clean-rdZhdD`.
  `report.md` returns `PASS`, `manifest.jsonl` records all assertions true,
  and the temporal contact sheet contains one clean full-screen Emacs
  window with no black segment.
- Artifact hashes:
  - manifest:
    `0eaeebd1697d69304cfd70189d7381a026b761333ab46ab1e2c5923a6e2db0c9`
  - report:
    `940efdb923a831dcd4b098cafee33b9a67c472f227be1897bd54cab0693f8b68`
  - recording:
    `519a4ba2757b453e54a2518c83784bfed26317eae4c3e246a984f38a2d30b2f5`
  - first/final screenshots:
    `8cd697130fd54b65bde6d1c06410f88c205779abce563b1e83661849dfc163e2`,
    `11996136c3924aea9d539f77bd9db13b55310c089967c43091d2f61133a0d4db`
