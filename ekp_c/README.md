# EKP C Dynamic Module

C implementation of the Knuth-Plass DP for emacs-kp (module version 1.6).

The division of labor: **Elisp owns all font-dependent data**
(tokenization, pixel measurement, glue values, prefix sums); the C
module runs only the O(n²) dynamic program.  This keeps the two engines
byte-identical in output while making the hot loop native.

The module remains C deliberately. A Rust implementation would still expose
the Emacs C module ABI and consume the same Elisp-prepared vectors. Current
live-commit profiles put the candidate module call below one millisecond, so
a Rust rewrite would add Cargo, target, and packaging obligations without
removing the measured end-to-end owners.

## Architecture

```
ekp_c/
├── ekp_module.h      # Core data structures and API declarations
├── ekp.c             # Emacs module entry point (emacs_module_init)
├── ekp_kp.c          # Knuth-Plass DP + two-pass emergency strategy
├── ekp_thread_pool.c # Thread pool (parallelism across paragraphs)
└── Makefile
```

Parallelism model: the DP for one paragraph is sequential (each
position depends on all earlier ones), so the thread pool parallelizes
across **paragraphs** via `ekp-c-break-batch` — the correct granularity,
with zero synchronization in the inner loop.  The pool is created
lazily on the first multi-paragraph batch, sized to the machine's
core count; a full queue blocks the submitter instead of dropping
tasks.

## Building

```bash
cd ekp_c
make PROFILE=portable # default → ekp.dylib/.so/.dll
```

Requirements: C11 compiler, Emacs module headers, pthreads.
Windows builds need MinGW-w64 (for pthreads) and
`make EMACS_ROOT=<path to your Emacs installation>`.

```bash
make PROFILE=native   # local CPU + LTO; benchmark-only
make PROFILE=debug    # -O0 with debug symbols
make PROFILE=sanitize # ASan/UBSan with frame pointers
make clean
make info              # includes the selected profile and final flags
```

`portable` is the release and CI default and contains no
`-march=native`/LTO flags. An unknown profile is a make error. From Emacs,
`M-x ekp-c-module-build` prompts for the same profile names and starts make
with a direct argv plus `default-directory`; whitespace and shell
metacharacters in the checkout path are not interpreted.

## API (as used by ekp.el)

```elisp
(ekp-c-init)             ; init global state
(ekp-c-version)          ; => "1.6" — checked by ekp-c-module-load
(ekp-c-thread-count)     ; worker count (created lazily on first batch)
(ekp-c-cleanup)

;; Synced automatically by ekp.el before every call:
(ekp-c-set-penalties LINE HYPHEN FITNESS LAST-RATIO
                     &optional CONSEC-HYPHEN LAST-SHORT EXTRA-STRETCH
                     EMERGENCY-STRETCH)

;; Single paragraph (15 args):
(ekp-c-break-with-arrays IDEAL-PREFIX MIN-PREFIX MAX-PREFIX
                         GLUE-IDEALS GLUE-SHRINKS GLUE-STRETCHES
                         HYPHEN-POS HYPHEN-WIDTH LINE-WIDTH
                         LEAD-SPACES TRAIL-SPACES FORBIDDEN-POS
                         TAIL-PROTRUDES HYPHEN-PROTRUDE
                         FIRST-LINE-WIDTH)
;; => (BREAKS . TOTAL-COST)

;; Many paragraphs in parallel: vector of 15-element vectors
(ekp-c-break-batch PARAGRAPHS)   ; => vector of (BREAKS . COST)
```

`LEAD-SPACES` / `TRAIL-SPACES` are the space-box run widths that the
Elisp renderer strips from line edges; the DP excludes them from line
metrics so both layers agree exactly (since 1.1).  `FORBIDDEN-POS`
carries the kinsoku / no-break gap indices (since 1.2),
`TAIL-PROTRUDES` / `HYPHEN-PROTRUDE` the right-edge protrusion
allowances (since 1.4), and `FIRST-LINE-WIDTH` the width of line 0
for first-line indentation (since 1.5; pass the line width or ≤0
when no indent is active).

The DP uses the same two-pass strategy as the Elisp engine: a strict
Knuth-Plass pass, then — only when the paragraph end is unreachable —
a final pass that adds finite background emergency stretch to ordinary
underfull candidates and still scores them through the same
badness/fitness/demerits path. Separately, if an overfull candidate would
extinguish the final active path to a breakpoint and no non-overfull
candidate survives there, TeX-style artificial demerits install the best
provisional path with tight fitness and zero incremental cost. This is a
content-independent reachability safeguard, not a hard-atom scoring rule.
Thus an overlong unbreakable token cannot make the result empty, while
ordinary underfull choices remain global K-P decisions. Badness saturates at
10000 exactly like the Elisp side.

Failure behavior: the full schema is checked before extraction. Malformed
direct API arguments signal `ekp-c-invalid-input`; allocation failure or an
unavailable DP result returns nil. `ekp.el` falls back only for nil. A module
signal propagates because it means the enabled backend contract is broken;
the dispatcher never hides it or silently produces a different layout.

Every public pixel/position integer must fit signed 32-bit range. The DP
uses 64-bit intermediates for sums and differences, so valid extreme inputs
cannot overflow when line width and protrusion are combined.
Validation uses one `extract_integer` call per integer value and clears the
temporary non-local exit only when the value is not an integer; it does not
round-trip through Lisp predicates and comparisons.

## Performance

Measured with `tests/ekp-bench.el` (batch Emacs 30.2, Apple Silicon,
byte-compiled Elisp around the C calls, min of 3 cold-cache runs):

| Case                        | Elisp engine (compiled) | C engine |
|:----------------------------|------------------------:|---------:|
| justify text-zh.txt w=200   |                  150 ms |    41 ms |
| justify mixed text w=300    |                   82 ms |    31 ms |
| range-justify zh 340–380    |                  529 ms |   106 ms |
| range-justify mix 280–320   |                  762 ms |    52 ms |
| DP only, text-zh w=400      |                   30 ms |   2.5 ms |

The pure-DP speedup is ~12×; end-to-end gains are smaller because
tokenization, measurement and rendering stay in Elisp.  The C engine
matters most for `range-justify` (many widths per text) and
multi-paragraph batches.  Absolute numbers vary with the machine and
power state; regenerate them with the two commands in DEVELOPER.md §9.

For task030's frozen 80-pixel structural-commit matrix, the C module layer
improved from 2.615/2.655 ms to 0.697/0.701 ms p95/p99 after one-pass integer
validation. With the production Elisp files byte-compiled, the complete
public append path measures 1.158–1.326 ms p99 for C and 1.429–1.438 ms for
pure Elisp on the same machine. These figures are separate from the
deliberately source-loaded, fully instrumented evaluator.
