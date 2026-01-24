[中文文档](./readme_zh.md)

# Emacs-KP: Knuth-Plass Line Breaking for Emacs

Emacs-kp implements the Knuth-Plass optimal line breaking algorithm with full support for CJK (Chinese, Japanese, Korean) and Latin mixed text typesetting.

## Demo

![ekp-demo](./images/ekp-demo-with-cache.gif)

## Algorithm Overview

### The Knuth-Plass Algorithm

The algorithm is based on the seminal 1981 paper ["Breaking Paragraphs into Lines"](http://www.eprg.org/G53DOC/pdfs/knuth-plass-breaking.pdf) by Donald Knuth and Michael Plass. Unlike greedy line-breaking (used by most text editors), K-P considers **all possible breakpoints** simultaneously to find the globally optimal solution.

#### Core Concepts

**1. Boxes, Glue, and Penalties**

Text is modeled as a sequence of three elements:
- **Box**: Indivisible content (characters, words) with fixed width
- **Glue**: Flexible space with ideal width, stretchability, and shrinkability
- **Penalty**: Cost for breaking at specific points (e.g., hyphenation)

```
┌─────┐      ┌─────┐      ┌─────┐
│ Box │─Glue─│ Box │─Glue─│ Box │
└─────┘      └─────┘      └─────┘
  word      (flexible)      word
```

**2. Badness: Measuring Line Quality**

Each line's quality is measured by how much glue must stretch/shrink:

```
            ⎧ 0                        if adjustment = 0
badness =   ⎨ ∞                        if impossible to fit
            ⎩ 100 × |adjustment/flexibility|³
```

- `adjustment` = target_width - natural_width
- `flexibility` = total stretchability (if stretching) or shrinkability (if shrinking)

**3. Demerits: Ranking Break Sequences**

Demerits combine badness with penalties to rank entire paragraph layouts:

```
demerits = (line_penalty + badness)² + penalty² + fitness_penalty
```

Where:
- `line_penalty`: Base cost per line (default: 10)
- `penalty`: Break-specific cost (hyphenation: 50)
- `fitness_penalty`: Extra cost when adjacent lines differ significantly in tightness

**4. Fitness Classes**

Lines are classified by tightness to ensure visual consistency:
- Class 0: Tight (significantly shrunk)
- Class 1: Decent (close to ideal)
- Class 2: Loose (stretched)
- Class 3: Very loose (significantly stretched)

Adjacent lines differing by more than one class incur additional penalty.

**5. Dynamic Programming**

The algorithm uses DP to find the minimum-demerits path through all valid breakpoints:

```
dp[k] = min over all valid i < k {
    dp[i] + demerits(line from i to k)
}
```

Time complexity: O(n²) where n = number of potential breakpoints.

### CJK Extensions

Emacs-kp extends the original algorithm for CJK text:

1. **Character-level breaking**: CJK text can break between any characters
2. **Mixed spacing**: Three glue types for Latin-Latin, Latin-CJK, and CJK-CJK gaps
3. **Punctuation handling**: CJK punctuation attaches to adjacent characters

### Hyphenation

Latin word hyphenation uses Frank Liang's algorithm (TeX's hyphenation):
- Pattern-based approach with priority values
- Language-specific dictionaries (en_US, de_DE, fr, etc.)
- Configurable minimum characters before/after breaks

## Limitations

Currently supports CJK mixed with **one** Latin language only. Multi-Latin-language mixing is not supported because the system cannot reliably determine which language a word belongs to for hyphenation.

## Usage

### Configuration

**`ekp-latin-lang`**: Primary Latin language for hyphenation (default: `"en_US"`).
See `dictionaries/` for supported languages.

**`ekp-param-set`**: Configure spacing parameters (in pixels):

| Parameter             | Description                                    |
|:----------------------|:-----------------------------------------------|
| `ekp-lws-ideal-pixel`   | Ideal space between Latin words               |
| `ekp-lws-stretch-pixel` | Maximum stretch between Latin words           |
| `ekp-lws-shrink-pixel`  | Maximum shrink between Latin words            |
| `ekp-mws-ideal-pixel`   | Ideal space between Latin and CJK             |
| `ekp-mws-stretch-pixel` | Maximum stretch between Latin and CJK         |
| `ekp-mws-shrink-pixel`  | Maximum shrink between Latin and CJK          |
| `ekp-cws-ideal-pixel`   | Ideal space between CJK characters            |
| `ekp-cws-stretch-pixel` | Maximum stretch between CJK characters        |
| `ekp-cws-shrink-pixel`  | Maximum shrink between CJK characters         |

Example: `(ekp-param-set 7 3 2 5 2 1 0 2 0)`

**Do not set these variables directly—always use `ekp-param-set`.**

Default values follow K-P recommendations:
- Ideal = space character width
- Stretch = ideal × 0.5
- Shrink = ideal × 0.33

### K-P Algorithm Parameters

| Parameter                     | Default | Description                              |
|:------------------------------|:--------|:-----------------------------------------|
| `ekp-line-penalty`            | 10      | Base penalty per line break              |
| `ekp-hyphen-penalty`          | 50      | Penalty for hyphenated breaks            |
| `ekp-adjacent-fitness-penalty`| 100     | Penalty for inconsistent line tightness  |
| `ekp-last-line-min-ratio`     | 0.5     | Minimum fill ratio for last line         |
| `ekp-looseness`               | 0       | Target line count offset (±n lines)      |

### Core Functions

```elisp
(ekp-pixel-justify string line-pixel)
```
Justify STRING to LINE-PIXEL width per line. Returns formatted text.

```elisp
(ekp-pixel-range-justify string min-pixel max-pixel)
```
Find optimal width in [MIN-PIXEL, MAX-PIXEL] range using ternary search.
Returns `(formatted-text . optimal-pixel)`.

Note: Uses O(log n) ternary search with aggressive caching.

```elisp
(ekp-clear-caches)
```
Clear all paragraph caches.

## Roadmap

- [x] Preserve original text properties after formatting
- [x] Full Knuth-Plass demerits model with fitness classes
- [x] Hyphenation with consecutive-hyphen penalty
- [ ] Rust dynamic module for parallel computation
- [ ] Auto-correction for mixed punctuation

## Credits

- Core algorithm: ["Breaking Paragraphs into Lines"](http://www.eprg.org/G53DOC/pdfs/knuth-plass-breaking.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- Hyphenation: Adapted from [Pyphen](https://github.com/Kozea/Pyphen), using Liang's algorithm
- Dictionaries: [Hunspell hyphenation patterns](https://github.com/Kozea/Pyphen)
