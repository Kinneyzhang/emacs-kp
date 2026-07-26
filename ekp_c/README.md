# EKP C Dynamic Module

C implementation of the Knuth-Plass DP for emacs-kp (module version 1.4).

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
├── ekp_hyphen.c      # Liang hyphenation (experimental path only)
├── ekp_paragraph.c   # C-side tokenization (experimental path only)
└── Makefile
```

Parallelism model: the DP for one paragraph is sequential (each
position depends on all earlier ones), so the thread pool parallelizes
across **paragraphs** via `ekp-c-break-batch` — the correct granularity,
with zero synchronization in the inner loop.

## Building

```bash
cd ekp_c
make            # → ekp.dylib (macOS) / ekp.so (Linux) / ekp.dll (Windows)
```

Requirements: C11 compiler, Emacs 27.1+ headers, pthreads.

```bash
make DEBUG=1    # Debug build with sanitizers
make clean
make info
```

## API (as used by ekp.el)

```elisp
(ekp-c-init)             ; init global state + thread pool
(ekp-c-version)          ; => "1.4" — checked by ekp-c-module-load
(ekp-c-thread-count)     ; => 8
(ekp-c-cleanup)

;; Synced automatically by ekp.el before every call:
(ekp-c-set-penalties LINE HYPHEN FITNESS LAST-RATIO
                     &optional CONSEC-HYPHEN LAST-SHORT)

;; Single paragraph (11 args):
(ekp-c-break-with-arrays IDEAL-PREFIX MIN-PREFIX MAX-PREFIX
                         GLUE-IDEALS GLUE-SHRINKS GLUE-STRETCHES
                         HYPHEN-POS HYPHEN-WIDTH LINE-WIDTH
                         LEAD-SPACES TRAIL-SPACES)
;; => (BREAKS . TOTAL-COST)

;; Many paragraphs in parallel: vector of 11-element vectors
(ekp-c-break-batch PARAGRAPHS)   ; => vector of (BREAKS . COST)
```

`LEAD-SPACES` / `TRAIL-SPACES` are the space-box run widths that the
Elisp renderer strips from line edges; the DP excludes them from line
metrics so both layers agree exactly (since 1.1).

The DP uses the same two-pass strategy as the Elisp engine: a strict
Knuth-Plass pass, then — only when the paragraph end is unreachable —
a second pass permitting emergency single-box breaks, so overlong
unbreakable tokens can never make the result empty.  Badness saturates
at 10000 exactly like the Elisp side.

### Experimental: self-contained C path

`ekp-c-break-lines` tokenizes and hyphenates in C
(`ekp_paragraph.c`, `ekp_hyphen.c`) with a measurement callback into
Emacs.  ekp.el does **not** use this path; its tokenizer is a
simplified approximation of `ekp-split-to-boxes`.  Kept for
experimentation.

```elisp
(ekp-c-load-hyphenator "/path/to/hyph_en_US.dic")   ; => index
(ekp-c-hyphenate 0 "hyphenation")                   ; => (2 5)
(ekp-c-break-lines "text..." 0 600 #'string-pixel-width)
```

## Performance

Measured with `tests/ekp-bench.el` (batch Emacs 30.2, Apple Silicon,
byte-compiled Elisp around the C calls, min of 3 cold-cache runs):

| Case                        | Elisp engine (compiled) | C engine |
|:----------------------------|------------------------:|---------:|
| justify text-zh.txt w=200   |                   96 ms |    57 ms |
| justify mixed text w=300    |                   53 ms |    23 ms |
| range-justify zh 340–380    |                  294 ms |    75 ms |
| range-justify mix 280–320   |                  480 ms |    34 ms |
| DP only, text-zh w=400      |                   15 ms |   1.3 ms |

The pure-DP speedup is ~12× (1.3 ms vs 15 ms); end-to-end gains are
smaller because tokenization, measurement and rendering stay in Elisp.
The C engine matters most for `range-justify` (many widths per text)
and multi-paragraph batches.
