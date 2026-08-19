# Issue: Stable Live Edit Transaction 2026-07-29

## issue019 [x] Per-edit frontier replanning destabilizes live projection and cannot restore reversible edits exactly

- **Status:** Closed by user re-audit on 2026-08-13. Implementation was
  developer-verified by `task031`.
- **Summary:** The current live path makes one `frontier` simultaneously own
  the latest edit, the natural suffix, and projection reuse. Before every
  edit it clears the touched projected line and every following line; after
  every edit it replans the complete hard line. This makes unrelated rows
  move during continuous input and turns narrow append latency into a
  symptom of the same ownership error.
- **Environment:** Graphical Emacs 30.2; `ekp-auto-justify-mode`; long mixed
  Latin/CJK hard lines with an installed live prefix.
- **Repro:**
  1. Type a hard line until multiple visual rows exist.
  2. Continue typing within the unfinished row, or edit a previously
     projected middle row.
  3. Observe earlier/succeeding rows and the owned text properties after each
     source change.
  4. Delete a projected space and insert the identical space again.
- **Expected vs Actual:**
  - Expected: ordinary same-row input leaves the committed projection
    untouched. A middle-row edit opens one local dirty island while
    unaffected break anchors remain installed. Small changes are absorbed by
    existing glue/native wrapping; local words move only when the dirty row
    no longer fits. Restoring the exact source restores the exact projection
    immediately.
  - Actual: `before-change` invalidates the touched line and its complete
    projected suffix. `after-change` moves `frontier`, runs or reuses a
    whole-hard-line plan, and republishes immediately. The transaction stores
    only an old paragraph marker and deleted fragment, so it cannot restore a
    committed baseline as a first-class state transition.
- **Root Cause:** `frontier` conflates three independent owners: source-edit
  location, unprojected edit boundary, and plan/projection reuse boundary.
  Signature diffing reduces writes after a plan is chosen, but cannot prevent
  global optimal breaks from changing when a new plan is computed after
  every key.
- **Required Outcome:**
  - committed projection owns the last published source/plan/signatures/spans;
  - a dirty edit transaction snapshots that baseline and owns only the
    affected local island;
  - ordinary same-row edits do no whole-hard-line DP and do not rewrite
    unaffected projection;
  - crossing a native visual-row boundary atomically replans the completed
    hard-line prefix, leaving the new current row natural;
  - the next real edit elsewhere, hard newline/paragraph completion,
    explicit refill, or width/font/layout-context change is a commit event;
  - point-only motion performs zero planning, cache, property, or layout work;
  - a reversible edit restores source and the complete owned projection
    `equal-including-properties` without depending on a best-effort replan;
  - completed paragraphs still use the existing globally optimal core DP;
  - no overlays or source layout characters are introduced.
- **Fix:** `task031` deletes frontier-owned immediate publication. Committed
  live state now owns source/key/plan/signatures/prefix/spans; one edit
  transaction snapshots that state and marker offsets, naturalizes only its
  dirty island, restores exact baselines directly, and permits replanning
  only at native-row or structural commit events. Static lazy-reflow chunks
  exclude the active paragraph. The core DP and `ekp.el` are unchanged.
- **Verification:** Focused public-path regressions were observed red before
  implementation and green afterward. Final buffer/default/random/isolated
  ERT runs pass 99/99, 188/188, 188/188, and 188/188; C/Elisp fuzz passes
  300/300; compile, checkdoc, package-lint, release, and local dictionary
  gates pass. The reviewed dynamic run
  `/private/tmp/ekp-stable-transaction-final5-2BFryc` is PASS with 55
  checkpoints, no failed assertion, a completed run-end, and no black
  segment or visual noise. Same-row and point-motion benchmark scenarios
  perform zero plans; 291 appends now contain only 15 structural plans.
- **User Confirmation:** Provided by user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; `task031` developer verification.
- **Commit:** — (documentation-only closure).
