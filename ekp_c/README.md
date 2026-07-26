# EKP C Dynamic Module

C implementation of the Knuth-Plass DP for emacs-kp (module version 1.5).

The division of labor: **Elisp owns all font-dependent data**
(tokenization, pixel measurement, glue values, prefix sums); the C
module runs only the O(n²) dynamic program.  This keeps the two engines
byte-identical in output while making the hot loop native.

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
make            # → ekp.dylib (macOS) / ekp.so (Linux) / ekp.dll (Windows)
```

Requirements: C11 compiler, Emacs module headers, pthreads.
Windows builds need MinGW-w64 (for pthreads) and
`make EMACS_ROOT=<path to your Emacs installation>`.

```bash
make DEBUG=1    # Debug build with ASan/UBSan
make clean
make info
```

## API (as used by ekp.el)

```elisp
(ekp-c-init)             ; init global state
(ekp-c-version)          ; => "1.5" — checked by ekp-c-module-load
(ekp-c-thread-count)     ; worker count (created lazily on first batch)
(ekp-c-cleanup)

;; Synced automatically by ekp.el before every call:
(ekp-c-set-penalties LINE HYPHEN FITNESS LAST-RATIO
                     &optional CONSEC-HYPHEN LAST-SHORT EXTRA-STRETCH)

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
a second pass permitting emergency single-box breaks, so overlong
unbreakable tokens can never make the result empty.  Badness saturates
at 10000 exactly like the Elisp side.

Failure behavior: any allocation failure or bad argument makes the
call return nil, and ekp.el falls back to the Elisp engine — the C
module never silently degrades to a subtly different layout.

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
