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

### Core Functions

Two functions are provided:

```(ekp-pixel-justify string line-pixel)```

Formats the text STRING to fit a pixel width of LINE-PIXEL per line and returns the justified text.

```(ekp-pixel-range-justify string min-pixel max-pixel)```

Searches for optimal typesetting within the range of MIN-PIXEL to MAX-PIXEL. Returns a cons-cell where the car is the formatted text and the cdr is the pixel value achieving the best typesetting result.