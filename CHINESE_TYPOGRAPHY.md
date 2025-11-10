# Chinese Typography Parameters

## Overview

This document describes the Chinese typography-specific parameters and functions available in emacs-kp. These features follow traditional Chinese typesetting conventions to produce professional-quality text layout.

## Parameters

### Line Height Control

#### `ekp-line-height-ratio`
- **Type**: Number (float or integer)
- **Default**: `1.0`
- **Range**: >= 1.0, typically 1.0 to 2.0
- **Description**: Controls the vertical spacing between lines. A value of 1.0 means default line height, 1.5 means 150% of default, etc.
- **Usage**: Higher values create more "airy" text layout, which can improve readability for long passages.
- **Function**: `(ekp-set-line-height 1.5)`

### Paragraph Indentation

#### `ekp-paragraph-indent-chars`
- **Type**: Integer
- **Default**: `2`
- **Range**: >= 0
- **Description**: Number of CJK character widths to indent at the start of paragraphs.
- **Common values**: 2 (standard Chinese), 0 (no indentation)
- **Function**: `(ekp-set-paragraph-indent 2)`

### Punctuation Compression

#### `ekp-punct-compress-ratio`
- **Type**: Number (float)
- **Default**: `0.5`
- **Range**: 0.0 to 1.0
- **Description**: Compression ratio for CJK full-width punctuation marks. 
  - `0.0` = no compression (full width)
  - `0.5` = 50% compression (half of the excess space removed)
  - `1.0` = full compression (minimal spacing)
- **Purpose**: CJK punctuation often takes full character width, but can be compressed for tighter layout
- **Function**: `(ekp-set-punct-compression 0.5)`

### Punctuation Positioning Rules

#### `ekp-prohibit-line-end-open-punct`
- **Type**: Boolean
- **Default**: `t` (enabled)
- **Description**: When enabled, prevents opening punctuation (like 「、（、《) from appearing at the end of a line.
- **Rationale**: Following Chinese typography convention, opening punctuation should not be isolated at line end
- **Function**: `(ekp-toggle-line-end-punct-prohibition)` to toggle

#### `ekp-prohibit-line-start-close-punct`
- **Type**: Boolean
- **Default**: `t` (enabled)
- **Description**: When enabled, prevents closing punctuation (like 」、）、》、。、，) from appearing at the start of a line.
- **Rationale**: Following Chinese typography convention, closing punctuation should stay with preceding text
- **Function**: `(ekp-toggle-line-start-punct-prohibition)` to toggle

### Mixed Content Spacing

#### `ekp-cjk-latin-spacing-auto`
- **Type**: Boolean
- **Default**: `t` (enabled)
- **Description**: Automatically adds appropriate spacing between CJK characters and Latin text.
- **Purpose**: Improves readability in mixed Chinese-English content
- **Function**: `(ekp-toggle-cjk-latin-spacing)` to toggle

## Functions

### Interactive Configuration Functions

All these functions can be called interactively (via `M-x`) or programmatically:

```elisp
;; Set line height
(ekp-set-line-height 1.5)

;; Set paragraph indentation
(ekp-set-paragraph-indent 2)

;; Set punctuation compression
(ekp-set-punct-compression 0.5)

;; Toggle punctuation rules
(ekp-toggle-line-end-punct-prohibition)
(ekp-toggle-line-start-punct-prohibition)

;; Toggle CJK-Latin spacing
(ekp-toggle-cjk-latin-spacing)
```

### Helper Functions

#### `ekp-is-opening-punct`
Checks if a character is an opening punctuation mark.

#### `ekp-is-closing-punct`
Checks if a character is a closing punctuation mark.

#### `ekp-check-cache-size`
Monitors and manages cache size to prevent excessive memory usage.

## Cache Management

### `ekp-cache-max-size`
- **Type**: Integer
- **Default**: `1000`
- **Description**: Maximum number of entries in the typography cache. When exceeded, cache is automatically cleared.
- **Purpose**: Prevents unbounded memory growth during long editing sessions

## Example Configuration

Here's a complete example configuration for traditional Chinese typography:

```elisp
;; Load emacs-kp
(require 'ekp)

;; Set language for hyphenation (if using mixed content)
(setq ekp-latin-lang "en_US")

;; Configure Chinese typography
(ekp-set-line-height 1.6)           ; Comfortable line spacing
(ekp-set-paragraph-indent 2)        ; Standard 2-character indent
(ekp-set-punct-compression 0.5)     ; Moderate punctuation compression

;; Enable punctuation rules (default is already t)
;; These ensure proper punctuation positioning
(setq ekp-prohibit-line-end-open-punct t)
(setq ekp-prohibit-line-start-close-punct t)

;; Enable automatic CJK-Latin spacing
(setq ekp-cjk-latin-spacing-auto t)

;; Set cache size limit
(setq ekp-cache-max-size 1000)

;; Configure basic spacing parameters
(ekp-param-set 7 3 2   ; Latin word spacing (ideal, stretch, shrink)
               5 2 1   ; Mixed spacing (ideal, stretch, shrink)
               0 2 0)  ; CJK spacing (ideal, stretch, shrink)
```

## Typography Best Practices

### For Pure Chinese Text
- Use line height ratio of 1.5-1.8 for better readability
- Standard paragraph indent is 2 characters
- Enable both punctuation prohibition rules

### For Mixed Chinese-English Text
- Keep line height ratio around 1.4-1.6
- Enable automatic CJK-Latin spacing
- Use moderate punctuation compression (0.4-0.6)
- Ensure Latin word spacing is properly configured

### For Dense Layout
- Lower line height ratio (1.0-1.3)
- Higher punctuation compression (0.6-0.8)
- Reduce paragraph indentation (0-1 characters)

### For Loose, Comfortable Layout
- Higher line height ratio (1.7-2.0)
- Lower punctuation compression (0.3-0.5)
- Standard paragraph indentation (2 characters)

## Technical Notes

### Parameter Validation

All parameter-setting functions include validation:
- Line height ratio must be >= 1.0
- Paragraph indentation must be >= 0
- Punctuation compression must be between 0.0 and 1.0
- Spacing parameters must be non-negative

Invalid parameters will raise descriptive error messages.

### Cache Management

The cache system automatically monitors its size and clears when the limit is exceeded. This ensures stable memory usage even with extensive typography operations.

### Compatibility

These parameters are designed to work seamlessly with the existing Knuth-Plass algorithm implementation. They extend the algorithm to better handle Chinese typography conventions without modifying core layout logic.
