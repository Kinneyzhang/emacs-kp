# Developer Documentation for Emacs-KP

This document details the internal architecture, API, and algorithms of `emacs-kp`. It is intended for contributors and advanced users who want to understand how the package works or extend it.

## 1. Architecture Overview

`emacs-kp` follows a layered architecture to separate text processing, layout computation, and rendering.

```
┌─────────────────────────────────────────────────────────────────┐
│                      User API Layer (ekp.el)                    │
│  ekp-pixel-justify  ekp-pixel-range-justify  ekp-clear-caches   │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Caching Layer (ekp-utils.el)                 │
│  ekp--get-para (paragraph cache)  ekp-dp-cache (DP result cache)│
└─────────────────────────────────────────────────────────────────┘
                               │
               ┌───────────────┴───────────────┐
               ▼                               ▼
┌─────────────────────────┐     ┌─────────────────────────┐
│   Pure Elisp Path       │     │   C Module Path         │
│   ekp--dp-cache-elisp   │     │   ekp--dp-cache-via-c   │
│   (O(n²) DP in Elisp)   │     │   (calls C for DP)      │
└─────────────────────────┘     └─────────────────────────┘
                                               │
                                               ▼
                                 ┌─────────────────────────┐
                                 │   C Dynamic Module      │
                                 │   ekp_break_with_prefixes│
                                 │   (8-thread parallel)   │
                                 └─────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Rendering Layer                               │
│  ekp--render-justified (apply breaks, insert glue pixels)       │
└─────────────────────────────────────────────────────────────────┘
```

## 2. Elisp Core (ekp.el)

### Data Structures

#### `ekp-para` Struct

The central data structure is `ekp-para`, which represents a preprocessed paragraph. It is cached to avoid re-tokenizing and re-measuring text.

```elisp
(cl-defstruct ekp-para
  string           ; Original text with properties
  latin-font       ; Detected Latin font
  cjk-font         ; Detected CJK font
  boxes            ; Vector of box strings
  boxes-widths     ; Vector of box pixel widths
  boxes-types      ; Vector of (start-type . end-type)
  glues-types      ; Vector of glue type symbols (lws, mws, cws, nws)
  hyphen-pixel     ; Width of hyphen character
  hyphen-positions ; Vector of hyphenable box indices
  ideal-prefixs    ; Prefix sum: ideal widths (for O(1) width calc)
  min-prefixs      ; Prefix sum: minimum widths
  max-prefixs      ; Prefix sum: maximum widths
  dp-cache)        ; Hash table: line-pixel → DP result
```

#### Glue Types
- `lws`: Latin Word Space (between Latin words)
- `mws`: Mixed Word Space (between Latin and CJK)
- `cws`: CJK Word Space (between CJK chars)
- `nws`: No Word Space (fixed)

### Core Functions

#### `(ekp-pixel-justify STRING LINE-PIXEL)`
Justifies `STRING` to `LINE-PIXEL` width.
1. Checks cache for existing `ekp-para`.
2. If miss, creates `ekp-para` (tokenize, measure, hyphenate).
3. Calls DP engine (Elisp or C) to get breaks.
4. Renders result using display properties (specifically `space` display property for glues).

#### `(ekp-pixel-range-justify STRING MIN-PIXEL MAX-PIXEL)`
Finds the "best" width within a range. Uses ternary search (O(log n)) to minimize demerits. Useful for finding the optimal width for a specific paragraph.

#### `(ekp-param-set ...)`
Sets the 9 spacing parameters (Ideal/Stretch/Shrink for LWS/MWS/CWS).

## 3. C Dynamic Module (ekp_c)

For large texts, the C module provides ~20x speedup by parallelizing the O(n²) Dynamic Programming phase.

### Source Structure
- `ekp_c/ekp.c`: Emacs module entry point.
- `ekp_c/ekp_kp.c`: The Knuth-Plass algorithm implementation.
- `ekp_c/ekp_thread_pool.c`: Worker thread pool.
- `ekp_c/ekp_hyphen.c`: Liang's hyphenation algorithm.

### C API (exposed to Elisp)

#### `(ekp-c-init)`
Initializes the module and thread pool.

#### `(ekp-c-break-with-prefixes ...)`
The low-level DP function. It takes flat arrays (pointers) from Elisp:
- Prefix sums (ideal, min, max)
- Glue parameters per box
- Hyphen positions
- Target line width

It returns a list of break indices and total cost.

### Memory Model
- **Zero Copy**: Elisp passes pointers to vector data directly to C.
- **Flat Arrays**: Data is structured as parallel arrays for cache efficiency.
- **Thread Safety**: The module uses a fixed thread pool. The DP algorithm uses a wavefront pattern for parallelizing the inner loop.

## 4. Algorithm Details

### The Knuth-Plass Algorithm
Based on the 1981 paper "Breaking Paragraphs into Lines".

**Cost Function (Demerits):**
`D = (LinePenalty + Badness)² + Penalty²`

**Badness:**
`100 * |Adjustment / Flexibility|³`

### CJK Extensions
- **Boxes**: Each CJK character is a separate box.
- **Glues**: Specific glue types for CJK-CJK and CJK-Latin transitions allow fine-tuning spacing (e.g., adding slight breathing room between English and Chinese).

### Hyphenation
Uses Frank Liang's algorithm (standard in TeX).
- Patterns are loaded from `dictionaries/*.dic`.
- `ekp-hyphen.el` handles this in pure Elisp.
- C module has its own implementation (`ekp_hyphen.c`) for speed if needed, though currently Elisp handles tokenization.
