[中文文档](./readme_zh.md)

## Introduction
Emacs-kp implements the knuth-plass typesetting algorithm, but its capabilities extend beyond English typesetting. Through further optimization of the algorithm, it achieves hybrid typesetting for both CJK and Latin-based languages.

## Demo
First, let's look at a demo of the typesetting effect:

![ekp-demo](./images/ekp-demo-with-cache.gif)

## Limitations
Currently, it only supports hybrid typesetting between CJK and one Latin-based language. Mixed typesetting with multiple Latin-based languages is not supported. This limitation arises because the system cannot precisely determine which language a word belongs to in order to perform hyphenation.

## Usage

### Configuration

`ekp-latin-lang` is used to set the primary Latin-based language in the text. The default setting is "en_US". All supported languages can be found in the "dictionaries" directory. The language name must match the name following "hyph_" in the dictionary files. Test cases include examples of German and French typesetting. Other languages have not been tested extensively but should theoretically work; however, finer customization may be required.

`ekp-param-set` is a function used to configure fundamental typesetting parameters. These parameters include:

| Parameter             | Meaning                                                        |
|:----------------------|:---------------------------------------------------------------|
| ekp-lws-ideal-pixel   | Ideal pixel width between Latin words                          |
| ekp-lws-stretch-pixel | Stretchable pixel width between Latin words                    |
| ekp-lws-shrink-pixel  | Shrinkable pixel width between Latin words                     |
| ekp-mws-ideal-pixel   | Ideal pixel width between Latin words and CJK characters       |
| ekp-mws-stretch-pixel | Stretchable pixel width between Latin words and CJK characters |
| ekp-mws-shrink-pixel  | Shrinkable pixel width between Latin words and CJK characters  |
| ekp-cws-ideal-pixel   | Ideal pixel width between CJK characters                       |
| ekp-cws-stretch-pixel | Stretchable pixel width between CJK characters                 |
| ekp-cws-shrink-pixel  | Shrinkable pixel width between CJK characters                  |

For example: `(ekp-param-set 7 3 2 5 2 1 0 2 0)` sets the above parameters accordingly. **​​Do not modify these variables directly – always use this function for configuration.​​**

If not manually configured, the default values follow KP algorithm recommendations for spaces between latin words:

- The ideal width is set to the pixel width of a space character.
- The stretchable width defaults to 1/2 of the ideal width.
- The shrinkable width defaults to 1/3 of the ideal width.

For spaces between latin word and CJK character: `ekp-mws-ideal-pixel = ekp-lws-ideal-pixel - 2` while maintaining the same stretch/shrink proportions.

For spaces between CJK characters: Ideal width between CJK characters defaults to 0. Stretchable width between CJK characters defaults to 2 pixels. Shrinkable width between CJK characters defaults to 0 (non-compressible).

### Core Functions

Two functions are provided:

```(ekp-pixel-justify string line-pixel)```

Formats the text STRING to fit a pixel width of LINE-PIXEL per line and returns the justified text.

```(ekp-pixel-range-justify string min-pixel max-pixel)```

Searches for optimal typesetting within the range of MIN-PIXEL to MAX-PIXEL. Returns a cons-cell where the car is the formatted text and the cdr is the pixel value achieving the best typesetting result.
