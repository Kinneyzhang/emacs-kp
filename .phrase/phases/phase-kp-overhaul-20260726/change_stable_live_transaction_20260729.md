# Change: Stable Live Edit Transaction 2026-07-29

## 2026-07-29 — Implement and verify task031

- **Modify** `ekp-buffer.el`: replace frontier-owned immediate publication
  with committed live state plus one baseline-snapshot edit transaction;
  naturalize only the owning dirty span range; restore reversible edits
  exactly; and publish only at native-row or structural commit events.
- **Modify** `ekp-buffer.el`: keep active live spans out of static lazy-reflow
  chunks and recognize both forward and backward native-row crossings.
- **Modify** `tests/ekp-buffer-tests.el` and
  `tests/ekp-gui-verify.el`: add RED/GREEN public-path coverage for zero-plan
  same-row edits, local middle-row anchors, object-identical reversal,
  forward/backward row crossing, zero-work point motion, lazy ownership,
  stable yank/undo, resize, and hard completion.
- **Modify** bilingual user/developer documentation, changelog, spec, plan,
  technical reference, task/issue records, and the decision postmortem to
  make transaction triggers and signature-diff ownership explicit.
- **Verification:** buffer 99/99; default, seed-`20260729`, and isolated ERT
  188/188 each; C/Elisp fuzz 300/300; warnings-as-errors production/test
  compile; empty checkdoc; pinned package-lint exit 0; release and 49-entry
  dictionary manifest pass. The earlier exact pinned-source run remains
  applicable because dictionary bytes did not change; two redundant final
  fetches failed with GitHub `early EOF`, not a byte mismatch.
- **Benchmark:** with GC excluded, cache-revisit and point-motion scenarios
  perform zero plans; 291 appends contain 15 structural plans. Remaining
  C/Elisp structural p99 spikes move to `issue018`/`task030`.
- **GUI evidence:** reviewed PASS at
  `/private/tmp/ekp-stable-transaction-final5-2BFryc`: 48.95 seconds,
  55 checkpoints, 17 assertions per checkpoint, completed run-end, one
  fullscreen target window, no black segment, split, stale buffer, client
  instruction, or transient blank frame.
- **Behavior/Risk:** ordinary editing is intentionally stability-first;
  the current dirty row may be temporarily non-optimal until a structural
  commit. Completed paragraphs still use the unchanged global KP core.

## 2026-07-29 — Plan issue019 and task031

- **Add** `issue019`: per-edit whole-hard-line replanning and suffix
  invalidation share one overloaded frontier and cannot express stable local
  editing or exact reversible restoration.
- **Add** `task031`: replace that model with committed projection, a dirty
  edit transaction, preserved unaffected anchors, and atomic structural
  commits.
- **Modify** `task030` to depend on `task031`; performance work must measure
  the surviving path instead of optimizing the rejected control flow.
- **Modify** the current spec, plan, technical reference, issue index, and
  decision history so trigger ownership is explicit: transactions decide
  when layout may change, signature diffing decides what properties change,
  and the unchanged core DP decides final layout.
- **Validation planned:** focused public-path RED/GREEN ERT; complete
  automated/static gates; re-profiled live benchmark; temporal GUI evidence
  for stable input, local middle-line editing, exact reversible restoration,
  structural commits, and zero-work point motion.
- **Behavior/Risk:** this planning entry is superseded by the completed
  implementation entry above.
