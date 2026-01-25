# Emacs-KP: Knuth-Plass Line Breaking for Emacs

[中文文档](./readme_zh.md) | [Developer Guide](./DEVELOPER.md)

Emacs-kp implements the Knuth-Plass optimal line breaking algorithm with full support for CJK (Chinese, Japanese, Korean) and Latin mixed text typesetting.

## Demo


## Features

- **Optimal Line Breaking**: Uses Knuth-Plass algorithm for globally optimal paragraph layout.
- **CJK Support**: Full support for Chinese, Japanese, Korean with mixed Latin text.
- **Hyphenation**: Frank Liang's algorithm with language-specific dictionaries.
- **Text Properties Preserved**: Font faces, colors, and other Emacs text properties are maintained.
- **C Module Acceleration**: Optional multi-threaded C module for 16-29x speedup.
- **Automatic Font Handling**: Spacing parameters computed from actual font metrics.

---

## User Guide

### Quick Start

1. **Install Dependencies**:
   Ensure you have a C compiler if you plan to use the C module (recommended for performance).

2. **Configuration**:

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
(require 'ekp)

;; Basic usage: justify text to 600 pixels width
(ekp-pixel-justify "Your paragraph text here..." 600)

;; Find optimal width in a range (returns (text . optimal-width))
(ekp-pixel-range-justify "Your text" 400 800)
```

### Configuration

#### Language Settings

**`ekp-latin-lang`** (default: `"en_US"`)

Primary Latin language for hyphenation. Supported languages are in `dictionaries/` directory:
- `en_US`, `en_GB` - English
- `de_DE` - German
- `fr` - French
- `es` - Spanish
- And many more...

```elisp
(setq ekp-latin-lang "de_DE")
```

#### Spacing Parameters

Use `ekp-param-set` to configure spacing (in pixels). If not set, defaults are computed automatically from font metrics.

```elisp
(ekp-param-set lws-ideal lws-stretch lws-shrink
               mws-ideal mws-stretch mws-shrink
               cws-ideal cws-stretch cws-shrink)
```

| Parameter Group | Description |
|:----------------|:------------|
| `lws-*` | Latin Word Space: between Latin words |
| `mws-*` | Mixed Word Space: between Latin and CJK |
| `cws-*` | CJK Word Space: between CJK characters |

#### K-P Algorithm Parameters

| Variable | Default | Description |
|:---------|:--------|:------------|
| `ekp-line-penalty` | 10 | Base cost per line break |
| `ekp-hyphen-penalty` | 50 | Extra cost for hyphenated breaks |
| `ekp-adjacent-fitness-penalty` | 100 | Cost for inconsistent line tightness |
| `ekp-last-line-min-ratio` | 0.5 | Minimum fill ratio for last line |
| `ekp-looseness` | 0 | Target line count offset (±n lines) |

### C Dynamic Module (Recommended)

For large texts, the optional C module provides significant performance improvement through multi-threaded parallel computation.

#### Building

```bash
cd ekp_c
make
```
*Requirements: C11 compiler, Emacs 27.1+*

#### Loading

```elisp
(require 'ekp-utils)

;; Load and initialize C module
(ekp-c-module-load)

;; Optional: Load hyphenation dictionary for C module
(ekp-c-load-dictionary "en_US")
```

Once loaded, `ekp-use-c-module` defaults to `t`, and all justification functions will automatically use the C module.

---

## Algorithm & Architecture

For a detailed explanation of the internal architecture, algorithms, and API reference, please refer to the **[Developer Guide](./DEVELOPER.md)**.

## Credits

- **Core Algorithm**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **Hyphenation**: Adapted from [Pyphen](https://github.com/Kozea/Pyphen), using Liang's algorithm
- **Dictionaries**: [Hunspell hyphenation patterns](https://github.com/Kozea/Pyphen)
