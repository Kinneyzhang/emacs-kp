# Issues: Repository System Audit 2026-07-28

Detailed evidence and prioritization live in
`Docs/REPOSITORY_AUDIT_20260728.md`.

## issue001 [x] Incomplete cache signatures reuse stale layout results

- **Status (2026-07-28):** Resolved by `task002` and `task003`.
- **Summary:** DP and automatic paragraph caches omitted configuration
  values that determine the result.
- **Environment:** `main@29cef97`, Emacs 30.2, C module 1.5.
- **Repro:** Compute a paragraph, change `ekp-hyphen-penalty` or
  `ekp-default-cws-stretch-pixel`, and compute again without clearing caches.
- **Expected vs Actual:** The second result must match a fresh computation;
  it instead reuses the old cached object/result.
- **Investigation:** DP reuse and paragraph preprocessing have separate
  owners, so each needs a complete structural signature.
- **Fix:** `task002` keys DP by width, looseness, and six runtime cost
  parameters. `task003` shares one spacing signature between the paragraph
  hash and same-string fast path, without adding watchers.
- **Verification:** Focused red/green cache tests; full ERT 99/99; C/Elisp
  fuzz 300/300; warning-clean byte compilation; checkdoc clean.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P1-01, P2-01; `task002`, `task003`.

## issue002 [x] Failed saves leave a justified buffer unformatted

- **Status (2026-07-28):** Resolved by `task004`.
- **Summary:** The pre-save hook reverses layout, but a failed write does not
  run an unconditional restoration path.
- **Environment:** `main@29cef97`, Emacs 30.2.
- **Repro:** Justify a visited buffer, make its target directory unavailable,
  then call `save-buffer`.
- **Expected vs Actual:** The save should fail while preserving the visible
  justified state; the buffer remains unformatted and retains stale save
  markers.
- **Investigation:** Restoration existed only in `after-save-hook`; no hook
  could provide finally semantics across every write failure.
- **Fix:** Whole-buffer writes switch to a logical copy from
  `write-region-annotate-functions`; the display buffer is never
  unformatted.
- **Verification:** Successful write, missing-directory failure, forced
  encoding error plus retry, and `quit`; full ERT 102/102; fuzz 300/300;
  warning-clean byte compilation; checkdoc clean.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P1-02; `task004`.

## issue003 [x] Buffer integration is not composable or lifecycle-owned

- **Status (2026-07-28):** Resolved by `task005`.
- **Summary:** EKP overwrites an existing copy filter and leaves hooks/filter
  installed after manual unjustify removes the final layout span.
- **Environment:** `main@29cef97`, Emacs 30.2.
- **Repro:** Install a buffer-local `filter-buffer-substring-function`,
  justify, copy, then unjustify the whole buffer.
- **Expected vs Actual:** Both filters should retain their semantics and
  unused integrations should be removed; the prior filter is bypassed and
  EKP integrations remain.
- **Investigation:** Installation used direct `setq-local`; public and
  internal unjustify shared one body with no lifecycle boundary.
- **Fix:** Record local/inherited filter ownership, run the prior filter
  before EKP logical inversion, split internal structural unjustify from the
  public cleanup wrapper, and observe external deletion with a guarded
  after-change hook.
- **Verification:** Public copy and DELETE filter paths; local/global
  restoration; final unjustify; mode disable; full ERT 107/107; fuzz
  300/300; warning-clean byte compilation; checkdoc clean.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P1-03, P2-06; `task005`.

## issue004 [x] Test order dependency produces a false green suite

- **Status (2026-07-28):** Resolved by `task006`.
- **Summary:** The named parshape C-bypass test exercises first-line indent
  and passes only after another test leaks `ekp-use-c-module=nil`.
- **Environment:** `main@29cef97`, Emacs 30.2, C module 1.5.
- **Repro:** Load the C module and run only
  `ekp-test-parshape-bypasses-c`.
- **Expected vs Actual:** The test should independently prove the real
  parshape dispatch path; it fails in isolation while the full suite passes.
- **Investigation:** The test binds the wrong variable and fixtures restore
  only a subset of mutable global state.
- **Fix:** Bind the real parshape and drive `ekp-pixel-justify`; dynamically
  restore every isolated EKP tunable; provide permuted-order and
  fresh-process runners, with the former in CI.
- **Verification:** Focused 0/1 red then 3/3 green; reproducibly permuted
  ERT 108/108; all 108 ERT tests passed independently in fresh Emacs
  processes.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P1-04; `task006`.

## issue005 [x] C API and build boundary need explicit validation

- **Status (2026-07-28):** Resolved by `task008` and `task009`.
- **Summary:** Malformed direct calls can signal despite the former nil-only
  contract, extreme int32 arithmetic is not closed, and the interactive
  build command uses an unquoted shell path.
- **Environment:** `main@29cef97`; macOS arm64; C11 toolchain.
- **Repro:** Pass a malformed field vector directly; inspect int32
  accumulation and `ekp-c-module-build` path construction.
- **Expected vs Actual:** Inputs, error semantics, ranges, and build profiles
  should be explicit; the current boundary mixes signaling, clamping, and
  shell assumptions.
- **Investigation:** Fifteen-field schema checks happen piecemeal; arithmetic
  continues in int32; default flags include native/LTO optimization.
- **Fix:** C 1.6 preflights the complete schema and uses
  `ekp-c-invalid-input`; DP intermediates are int64 and backend signals
  surface. Builds use direct argv and explicit portable/native/debug/
  sanitize profiles, with portable default.
- **Verification:** Direct boundary 0/6 → 6/6; dispatcher 0/1 → 1/1;
  build process/profile 0/2 → 2/2; full ERT 116/116; fuzz 300/300; four
  warning-clean profiles; real interactive and whitespace-path portable
  builds. ASan runtime loading is blocked by macOS policy and remains a
  Linux CI gate.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P2-02, P2-03; `task008`, `task009`.

## issue006 [x] Dictionary claims exceed parser and provenance coverage

- **Status (2026-07-28):** Resolved by `task011`.
- **Summary:** Alternative pattern syntax is skipped, several license/source
  companions are absent, and the update script is unpinned.
- **Environment:** 50 bundled `hyph_*.dic` files at `main@29cef97`.
- **Repro:** Count non-comment patterns containing `/` and compare dictionary
  files with same-name README files.
- **Expected vs Actual:** Supported syntax and provenance should be auditable;
  2399 alternative patterns across three languages are ignored and 20
  dictionaries lack same-name README files.
- **Investigation:** The parser intentionally skipped `/` patterns; deeper
  review found 2399 libhyphen replacement rules across Hungarian, Catalan,
  and Albanian plus four Esperanto slash-prefixed rules. The updater pulled
  moving HEAD without a manifest/checksums, and Sanskrit had no exact license
  evidence at the pinned snapshot.
- **Fix:** Reject those four dictionaries with a typed condition instead of
  corrupt partial support; pin 49 files and their checksum/source/license
  evidence; remove unverifiable Sanskrit; add portable check/export tooling.
- **Verification:** Golden red 0/2 → 2/2; offline/upstream 49/49; two
  identical exports; full ERT 121/121; compile/checkdoc/shell/YAML/diff pass.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P2-04; `task011`.

## issue007 [x] GUI verification reports failure without failing automation

- **Status (2026-07-28):** Resolved by `task007`.
- **Summary:** The full fit command lives only under `tests/`, while its
  noninteractive matrix prints failures but exits successfully.
- **Environment:** `main@29cef97`, GUI Emacs 30.2.
- **Repro:** Load `tests/ekp-gui-verify.el`; force a failing row in
  noninteractive matrix mode.
- **Expected vs Actual:** Installation/loading instructions should be clear
  and any failed row should return nonzero; failure is currently textual
  only.
- **Investigation:** The diagnostic and matrix have no shared packaged
  entrypoint or assertion boundary.
- **Fix:** Cases return structured results to one report boundary; batch
  failure exits 1 after printing the table. Documentation explicitly loads
  the developer tool and states it is not part of `ekp-region`.
- **Verification:** Forced-failure/success controls 2/2; default and
  permuted full ERT 110/110; real GUI matrix 7/7; clean fullscreen
  single-window screenshot inspected at
  `/tmp/emacs-kp-gui-20260728-clean.png`.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P2-05; `task007`.

## issue008 [x] Hot-loop allocation and nil caching need benchmark gates

- **Status (2026-07-28):** Resolved by `task012`.
- **Summary:** Tokenization and hyphen insertion repeatedly rebuild strings,
  and nil hyphen results are recomputed.
- **Environment:** `main@29cef97`, Emacs 30.2.
- **Repro:** Profile long tokens and query an unhyphenated word twice while
  counting `ekp-hyphen--compute`.
- **Expected vs Actual:** Repeated nil results should hit the cache and
  hot-loop growth should remain linear; nil results compute twice.
- **Investigation:** Cache lookup uses `or`, which cannot distinguish an
  absent key from a cached nil.
- **Fix:** The word-position cache distinguishes absent entries with a
  sentinel; tokenizer boxes accumulate reversed fragments and join once;
  inserted words concatenate original slices once.
- **Verification:** Nil call-count red 0/1 → 1/1; exact long/propertized and
  dense output regressions 3/3; 1k–8k benchmark improved the 8k cases from
  3.133/0.945 s to 1.100/0.013 s; full ERT 124/124; fuzz 300/300;
  warning-as-error production compilation and checkdoc passed.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P3-01; `task012`.

## issue009 [x] Release and documentation governance are not closed

- **Status (2026-07-28):** Resolved locally by `task010`.
- **Summary:** Local version/tag state is ahead of the published remote,
  CI dependencies float, Windows is absent, and ignored `.phrase` records are
  called the source of truth.
- **Environment:** local `main@29cef97`, `origin/main@11437cb`.
- **Repro:** Compare local/remote commits and tags, CI pins, `.gitignore`, and
  the repository workflow contract.
- **Expected vs Actual:** Release metadata and versioned decisions should be
  mutually consistent; local `v1.0.0` is not present remotely and the process
  source of truth is ignored.
- **Investigation:** No single release gate currently checks commit, tag,
  changelog, package/C versions, CI, and artifacts together.
- **Fix:** `.phrase` is tracked truth; actions and package-lint are immutable
  inputs; Windows compiles/runs the pure-Elisp baseline; an executable local
  invariant gate and separate remote/artifact checklist own the release path.
- **Verification:** Gate red → green; pinned package-lint pass; workflow YAML
  and shell syntax pass; default ERT 119/119; diff check pass.
- **User Confirmation:** User requested completion of all audit findings on
  2026-07-28; the automated local acceptance criteria are satisfied.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P2-07; `task010`.

## issue010 [x] Same-string property mutation bypasses paragraph identity

- **Status (2026-07-28):** Resolved by `task016`.
- **Summary:** `ekp--para-key` includes layout-relevant text properties, but
  the most-recent paragraph fast path compares only string identity plus a
  partial configuration tuple.
- **Environment:** current remediation working tree; Emacs 30.2.
- **Repro:** Resolve a string, add `ekp-no-break` to the same string object,
  then resolve it again without clearing caches.
- **Expected vs Actual:** The second lookup must equal a fresh property-aware
  paragraph; it returns the pre-mutation paragraph object.
- **Investigation:** The fast path duplicates only part of the hash-cache
  identity contract, so in-place property changes cannot invalidate it.
- **Fix:** Store `(string, complete ekp--para-key, paragraph)` in the
  most-recent slot and compare the same structural key used by the hash
  cache. Remove the six style-variable watchers that compensated for the
  former partial identity.
- **Verification:** Same-object mutation red 0/1 → 1/1; focused cache matrix
  6/6; default and seeded-permuted ERT 130/130; every one of 130 ERT tests
  passed in a fresh Emacs process; fuzz 300/300; warning-clean Elisp/C
  compilation, checkdoc, package-lint, release/dictionary/static gates, and
  GUI matrix 7/7 passed. Independent architecture review returned `CLEAR`
  for the shared-key ownership.
- **User Confirmation:** User requested completion of every repository
  finding on 2026-07-28.
- **Resolved At:** 2026-07-28.
- **Resolved By:** Codex.
- **Commit:** Pending.
- **Related:** audit P1-05; `task016`.
