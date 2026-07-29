# Change: C Resize Latency 2026-07-29

## 2026-07-30 — Complete task032 core and resize optimization

- **Modify** `ekp.el`: resolve a paragraph once per layout plan, reuse its
  prepared DP data, memoize width-independent natural gap geometry, and omit
  true zero-source/zero-target projection gaps.
- **Modify** `ekp-buffer.el`: publish owned properties in one mutation,
  consume prepared gap geometry without projection-time measurement, and
  keep the active paragraph out of the static resize pass before installing
  its live prefix.
- **Add** focused RED/GREEN regressions in `tests/ekp-tests.el` and
  `tests/ekp-buffer-tests.el`, plus the frozen dual-path evaluator in
  `tests/ekp-c-resize-evaluator.el` and
  `tests/run-c-resize-evaluator.sh`.
- **Performance:** four interleaved rounds reduce core p95 from 42.006 ms to
  27.687 ms (34.09%) and complete resize p95 from 46.611 ms to 27.487 ms
  (41.03%); both p50 gains also exceed 20%.
- **Correctness:** frozen-C, candidate-C, and Elisp projection hashes match.
  Normal, random-order, and isolated ERT gates pass; 300 fuzz cases, static
  checks, release checks, and reviewed temporal GUI evidence pass.
- **Risk:** external window-system allocation can trigger an Emacs GC pause
  at the resize callback boundary. Total, GC, and EKP mutator time remain
  separately observable; no debounce or global GC behavior changed.

## 2026-07-29 — Plan issue020 and task032

- **Add** `issue020` and `task032` for the reported 60–70 ms C-backed
  resize/reflow latency.
- **Add** the `c-resize-latency` performance-goal contract before
  optimization: paired p50/p95 improvement of at least 20%, candidate p95
  at or below 50 ms, exact frozen-C and Elisp parity, and complete
  regression gates.
- **Decision:** measure complete reflow, plan construction, Emacs/C
  marshalling, C DP, and projection publication separately before choosing
  the implementation layer.
- **Risk:** a whole-reflow stopwatch can misattribute Elisp measurement or
  property publication to the C algorithm. No runtime change is included in
  this planning entry.
