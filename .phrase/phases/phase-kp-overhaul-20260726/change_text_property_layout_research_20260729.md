# Change Log: Text-Property Layout Research 2026-07-29

## task018 / issue011

- **Add** —
  `.phrase/phases/phase-kp-overhaul-20260726/tech-refer_text_property_layout_20260729.md`
  - Recorded the user-mandated text-property-only direction.
  - Mapped KP boxes, Latin/CJK glue, indentation, chosen breaks, and
    discretionary hyphens to Emacs display properties.
  - Recorded official documentation/source evidence, exact boundaries, and
    a minimum GUI verification gate.
  - Proposed a non-polluting Latin hyphen representation in which the
    preceding source grapheme displays as
    `GRAPHEME + HYPHEN + NEWLINE`.

- **Modify** —
  `.phrase/phases/phase-kp-overhaul-20260726/issue_logical_text_api_20260728.md`
  - Marked design research as resumed while keeping the issue open.
  - Added the confirmed primitives, Emacs 30.2 GUI probe results, proposed
    hyphen owner, and unresolved architecture constraints.

- **Modify** —
  `.phrase/phases/phase-kp-overhaul-20260726/task_repository_audit_20260728.md`
  - Added and completed documentation-only `task018`.

- **Modify** — `.omx/plans/prd-ekp-seamless-live-layout.md`,
  `.omx/plans/test-spec-ekp-seamless-live-layout.md`
  - Added a supersession notice so the prior overlay-oriented prototypes
    are not executed after the user's text-property-only decision.

- **Modify** — `.phrase/docs/CHANGE.md`
  - Indexed this phase change.

## Behavior and Risk

- No runtime source, test implementation, package API, or persisted file
  format changed.
- The recorded GUI probes used temporary buffers in clean Emacs 30.2
  daemons and left no repository or user-buffer state.
- Static LTR feasibility is evidence, not proof of live editing,
  multi-window independence, bidi correctness, or foreign display-property
  composition.
- `issue011` remains open.

## Validation

- Official GNU Emacs display/text-property documentation was cross-checked
  against the Emacs 30.2 display source.
- GUI glue probe: a natural 7px source space rendered at exact 3px and 20px
  targets through combined `space-width` and `min-width`; source characters
  were unchanged.
- GUI hyphen probe: `abcdefgh` remained eight source characters while a
  property on `d` displayed `d-\n`, produced two screen lines, moved one
  visual line to source position 5, and honored a 20px `line-prefix`.
- Documentation links, task/change traceability, Markdown structure, and
  `git diff --check` were inspected.
