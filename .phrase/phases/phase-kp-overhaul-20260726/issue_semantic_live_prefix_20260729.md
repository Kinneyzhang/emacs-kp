# Issue: Semantic Live Prefix 2026-07-29

## issue016 [x] Live editing freezes native rows instead of projecting a semantic hard-line prefix

- **Status:** Closed by user re-audit on 2026-08-13. The whole-hard-line
  commit result is retained; per-edit replanning was superseded by
  `issue019`/`task031`.
- **Summary:** Before `task028`, live editing committed completed native
  visual rows independently. User testing showed that earlier soft-wrapped
  rows in the same hard line did not become globally KP-aligned while the
  user continued typing. The behavior was row-local, not a dynamic
  projection of one semantic hard-line plan.
- **Environment:** Graphical Emacs 30.2; `ekp-auto-justify-mode`; mixed
  Latin/CJK hard lines that wrap across multiple visual rows.
- **Repro:**
  1. Enable `ekp-auto-justify-mode`.
  2. Type one long hard line that naturally soft-wraps into multiple rows.
  3. Continue editing on the current unfinished row without inserting a
     hard newline.
  4. Observe the rows before point.
- **Expected vs Actual:**
  - Expected: EKP sends the complete current hard-line source text to the
    existing `ekp-layout-plan` and projects only the complete semantic
    lines before the line containing the latest real source edit. Those
    preceding lines may change breaks and glue together whenever text,
    width, font, or layout options change. The editing-frontier line and
    all following source stay naturally editable. Point-only navigation
    does not change the projection.
  - Actual before `task028`: the live model derived native visual rows and
    committed them one at a time. Once a row was treated as completed,
    later edits could not let the whole hard-line KP plan revise earlier
    breaks and glue as a single semantic decision.
- **Root Cause:** `ekp-buffer` owned live editing but used native visual-row
  boundaries as the persistent layout unit. That was the wrong owner
  boundary. Core DP already computed the correct complete-text plan; the
  buffer layer needed to consume that plan and decide which prefix was safe
  to project, not invent a row-freezing live algorithm.
- **Required Outcome:** Replace native-row commitment with semantic prefix
  projection:
  - keep the core DP, C ABI, DP schema, and `ekp-layout-plan` contract
    unchanged;
  - pass the full current hard line to `ekp-layout-plan`;
  - find the plan line containing the latest real source edit;
  - project only the complete plan lines before that line;
  - leave the edit-frontier plan line and all following source natural;
  - compare old/new line signatures so unchanged prefixes are not
    reinstalled;
  - cache recent full hard-line plans in `ekp-buffer` only;
  - fail closed to native display for conflicts, oversized hard lines, or
    stale generations;
  - never use overlays or insert source layout characters.
- **Fix:** `task028` replaces the native-row commit state with one current
  whole-hard-line semantic plan. `task029` makes the latest real source
  edit, rather than transient point, own the natural-suffix frontier.
  `ekp-buffer` projects only prior lines, owns break whitespace on the
  preceding semantic line, keeps the editable suffix property-free,
  reuses a 16-entry buffer-local plan LRU, and updates changed projection
  suffixes from common-prefix line signatures. The core DP, C ABI, schema,
  and `ekp-layout-plan` contract are unchanged.
- **Refinement:** `issue017`/`task029` supersede the point-driven boundary
  portion of `task028`. The persistent natural-suffix boundary belongs to
  the latest source edit; transient point motion is display read-only.
- **Later Refinement:** `issue019`/`task031` supersede immediate
  whole-hard-line replanning after every edit. The plan remains authoritative
  at structural commits; stable same-row editing is owned by a saved
  committed projection and local dirty transaction.
- **Verification:**
  - Public-path and edge-case ERT pass in the 181-test default suite, the
    seed-`20260729` suite, and 181 isolated Emacs processes. C/Elisp fuzz
    passes 300/300.
  - Warning-as-error compilation, checkdoc, the CI-pinned package-lint,
    release, dictionary, pinned-source, no-overlay, and source-clean gates
    pass.
  - The main dynamic GUI run
    `/private/tmp/ekp-semantic-live-v4-vFZTkr` exercises typing, deletion,
    backward/forward point motion, yank, real undo, narrow/restore resize,
    and hard newline. Its 38 checkpoints have exact source text, zero
    overlays, `hscroll=0`, no pending transaction, no failed assertion,
    and a reviewed temporal verdict of PASS.
  - The 44-column split-window run
    `/private/tmp/ekp-semantic-split-v3-uPwuOi` proves native incremental
    soft wrapping, one semantic projected prefix line, a natural active
    line, exact source text, zero overlays, `hscroll=0`, clean disable, and
    a reviewed temporal verdict of PASS.
  - A subsequent task029 audit could not reproduce the original 6.399 ms
    append p99 with the current checked-in benchmark. At its synthetic
    80-pixel width, repeated GC-excluded runs instead record one plan per
    unique append and roughly 33–85 ms p99. `issue018`/`task030` track this
    separate performance debt. Point motion remains a zero-work path at
    about 0.03 ms p99.
  - Final independent code review returns APPROVE; independent architecture
    review returns CLEAR. Both reviewed the latest font-context
    invalidation delta.
- **User Confirmation:** Provided by user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; runtime work previously implemented.
- **Commit:** — (documentation-only closure).
