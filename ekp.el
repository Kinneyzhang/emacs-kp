;;; ekp.el --- Knuth-Plass line breaking with CJK support -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; Maintainer: Kinney Zhang <kinneyzhang666@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/Kinneyzhang/emacs-kp
;; Keywords: wp, text, typesetting, CJK
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Public API
;;
;; Load this package with (require 'ekp) after adding the repository
;; root to load-path.
;; Only the interfaces listed below are supported for use by other packages.
;; All unlisted symbols and implementation features are internal, including
;; names without a double hyphen. Do not require files below lisp/ from consumers.
;; This Commentary is the authoritative export list; implementations and precise
;; docstrings remain beside their definitions. The manual contains workflows.
;;
;; Variable: ekp-adjacent-fitness-penalty
;; Penalty when adjacent lines differ in tightness by >1 class.
;;
;; Variable: ekp-alignment
;; Paragraph alignment mode.
;; `justify'      — flush both edges (default)
;; `ragged-right' — natural spacing, lines end ragged on the right
;; `ragged-left'  — natural spacing, lines start ragged on the left
;; `center'       — natural spacing, both edges share the leftover
;; Non-justify modes keep inter-word glue rigid; the K-P optimizer still
;; picks breaks that minimize raggedness within
;; `ekp-ragged-stretch-pixel' per line.
;;
;; Function: ekp-allow-break-region (beg end &optional announce)
;;;###autoload (autoload 'ekp-allow-break-region "ekp" nil t)
;; Clear session-local `ekp-no-break' from BEG through END.
;; ANNOUNCE requests interactive feedback.
;;
;; Variable: ekp-auto-justify-lazy-threshold
;; Buffer size beyond which whole-buffer reflows run visible-first.
;;
;; Function: ekp-auto-justify-mode (&optional arg)
;;;###autoload (autoload 'ekp-auto-justify-mode "ekp" nil t)
;; Maintain a non-mutating KP display projection.
;;
;; Completed paragraphs use the effective `ekp-buffer-measure': narrowest
;; window by default, a fixed width, or a `(max . N)' cap.
;; Manual no-break and verbatim properties last only for the current buffer
;; session.
;;
;; This is a minor mode.  If called interactively, toggle the
;; `EKP-Auto-Justify mode' mode.  If the prefix argument is positive,
;; enable the mode, and if it is zero or negative, disable the mode.
;;
;; If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
;; mode if ARG is nil, omitted, or is a positive number.  Disable the mode
;; if ARG is a negative number.
;;
;; To check whether the minor mode is enabled in the current buffer,
;; evaluate the variable `ekp-auto-justify-mode'.
;;
;; The mode's hook is called both when the mode is enabled and when it is
;; disabled.
;;
;; \{ekp-auto-justify-mode-map}
;;
;; Variable: ekp-auto-justify-native-append
;; Use the loaded native module for automatic live append DP.
;; This affects only the already prepared live append path.  Explicit string
;; layout and full buffer layout still obey `ekp-use-c-module' directly; when
;; the native module is unavailable, live append falls back to Elisp.
;;
;; Variable: ekp-auto-justify-paragraph-limit
;; Maximum hard-paragraph characters planned automatically.
;; Longer paragraphs stay natural so enabling the mode, pasting, and
;; ordinary editing cannot enter an unbounded paragraph-wide planning
;; operation.  `ekp-refill-paragraph' remains the explicit full-quality
;; command and is not limited by this value.
;;
;; Variable: ekp-auto-justify-resize-delay
;; Seconds to debounce window resize reflows.
;;
;; Variable: ekp-auto-justify-tick-budget
;; Seconds of work allowed in one lazy reflow tick.
;;
;; Error: ekp-backend-contract-error
;; Malformed C backend result
;; Catch this condition with condition-case; failed calls do not return a value.
;;
;; Variable: ekp-break-policy
;; Text property controlling regional break policy.
;; `normal' clears automatic token or face restrictions, `hyphenate'
;; enables discretionary hyphenation, and `no-hyphen' suppresses it.
;; This property is not a hard no-break switch; rigid atoms remain
;; owned by the separate `ekp-no-break' text property.
;;
;; Variable: ekp-buffer-inline-faces
;; Faces whose exact spans use `ekp-inline-code-policy'.
;;
;; Variable: ekp-buffer-margin-pixel
;; Pixels reserved inside the window body width.
;;
;; Variable: ekp-buffer-measure
;; Measure used for automatic and default manual buffer layout.
;; `narrowest-window' uses the narrowest live window displaying the
;; buffer.  A positive integer is a fixed pixel width.  `(max . N)'
;; caps the narrowest live window at N pixels.
;;
;; Variable: ekp-buffer-mode-policy-alist
;; Mode profiles consulted by automatic and manual buffer layout.
;;
;; Variable: ekp-buffer-skip-faces
;; Faces whose paragraphs stay verbatim.
;;
;; Variable: ekp-buffer-skip-predicate
;; Function called with a paragraph string that should stay verbatim.
;;
;; Function: ekp-c-module-build (&optional profile)
;;;###autoload (autoload 'ekp-c-module-build "ekp" nil t)
;; Build the C module with make using PROFILE.
;; PROFILE is one of `portable', `native', `debug', or `sanitize';
;; the default is `portable'.
;;
;; Function: ekp-c-module-load ()
;;;###autoload (autoload 'ekp-c-module-load "ekp" nil t)
;; Load EKP C module if available.
;; Refuses to enable a module older than
;; `ekp-c-module-required-version' (rebuild with make).
;;
;; Variable: ekp-cjk-no-line-end-extra
;; Custom CJK characters that must not end a line.
;; Used only by the `custom' kinsoku profile.
;;
;; Variable: ekp-cjk-no-line-start-extra
;; Custom/Japanese CJK letters that must not start a line.
;; Used only by the `custom' kinsoku profile.  The `ja' profile uses
;; an immutable built-in Japanese addition set.
;;
;; Function: ekp-clear-caches ()
;;;###autoload (autoload 'ekp-clear-caches "ekp" nil t)
;; Clear all paragraph and measurement caches.
;; Run after font or theme changes that affect glyph widths.
;;
;; Function: ekp-clear-verbatim-region (beg end &optional announce)
;;;###autoload (autoload 'ekp-clear-verbatim-region "ekp" nil t)
;; Clear session-local `ekp-verbatim' from BEG through END.
;; ANNOUNCE requests interactive feedback.
;;
;; Variable: ekp-consecutive-hyphen-penalty
;; Base penalty multiplier for consecutive hyphenated lines.
;; Actual penalty = this × count², encouraging spread of hyphens.
;;
;; Variable: ekp-default-cws-stretch-pixel
;; Max stretched pixel of whitespace between CJK chars.
;;
;; Function: ekp-diagnose ()
;;;###autoload (autoload 'ekp-diagnose "ekp" nil t)
;; Report the authoritative width and any skipped projection conflicts.
;;
;; Variable: ekp-first-line-indent
;; First-line indentation: pixels, or t for 2 em of the paragraph font.
;; Sugar for the common CJK paragraph convention; ignored when
;; `ekp-parshape' is set.
;;
;; Function: ekp-hyphen-create (&optional lang file left right)
;;;###autoload (autoload 'ekp-hyphen-create "ekp" nil nil)
;; Create hyphenator for LANG or dictionary FILE.
;; LEFT/RIGHT override the minimum characters kept before/after breaks;
;; by default the dictionary's own LEFTHYPHENMIN/RIGHTHYPHENMIN apply
;; (2/2 when it declares none).
;;
;; Error: ekp-hyphen-dictionary-not-found
;; Hyphenation dictionary not found
;; Catch this condition with condition-case; failed calls do not return a value.
;;
;; Error: ekp-hyphen-error
;; Hyphenation error
;; Catch this condition with condition-case; failed calls do not return a value.
;;
;; Variable: ekp-hyphen-penalty
;; Penalty for hyphenated breaks.  Higher = avoid hyphenation.  Default 50.
;; Note: added to demerits as penalty², following the K-P formula.
;;
;; Error: ekp-hyphen-unsupported-pattern
;; Dictionary uses unsupported replacement patterns
;; Catch this condition with condition-case; failed calls do not return a value.
;;
;; Variable: ekp-hyphenation
;; Global discretionary hyphenation policy.
;; `auto' and `on' use the configured dictionary when available;
;; missing dictionaries disable hyphenation without signaling.
;; `off' disables discretionary hyphenation.
;;
;; Variable: ekp-inline-code-policy
;; Default break policy for automatic inline code spans.
;; `normal' uses ordinary breaks and hyphenation.  `no-hyphen' keeps
;; ordinary legal breaks but suppresses discretionary dictionary
;; hyphens.  `no-break' makes fitting automatic spans rigid; overwide
;; automatic spans downgrade to `no-hyphen'.  This option does not
;; affect explicit `ekp-no-break' regions.
;;
;; Function: ekp-justify-buffer (&optional pixel)
;;;###autoload (autoload 'ekp-justify-buffer "ekp" nil t)
;; Project the accessible buffer as KP layout at PIXEL.
;;
;; Function: ekp-justify-region (beg end &optional pixel)
;;;###autoload (autoload 'ekp-justify-region "ekp" nil t)
;; Project BEG through END as KP layout at PIXEL without changing text.
;;
;; Variable: ekp-kinsoku-profile
;; Kinsoku profile used when compiling paragraph break permissions.
;;
;; Variable: ekp-last-line-min-ratio
;; Minimum fill ratio for last line (0.0-1.0).
;;
;; Variable: ekp-last-line-short-penalty
;; Penalty multiplier for underfilled last lines.
;; Applied as: this × (1 - fill-ratio) when fill < `ekp-last-line-min-ratio'.
;;
;; Variable: ekp-latin-lang
;; Language code for hyphenation (e.g., "en_US", "de_DE").
;;
;; Function: ekp-layout-plan (string line-pixel)
;;;###autoload (autoload 'ekp-layout-plan "ekp" nil nil)
;; Return a semantic KP layout plan for STRING at LINE-PIXEL.
;; The plan records source offsets, glue targets, breaks, indentation,
;; and discretionary hyphens without choosing a display representation.
;;
;; Variable: ekp-line-penalty
;; Penalty for each line break.  Higher = fewer lines.  Default 10.
;;
;; Variable: ekp-looseness
;; Target line count offset: 0=optimal, +1=looser (more lines), -1=tighter.
;; When non-zero, a full (position × line-count) dynamic program is run
;; and the path whose line count is closest to (optimal + looseness) with
;; the lowest demerits is selected.  Only supported by the Elisp engine;
;; when non-zero the C module is bypassed automatically.
;;
;; Function: ekp-markdown-setup ()
;;;###autoload (autoload 'ekp-markdown-setup "ekp" nil nil)
;; Protect common Markdown code faces in the current buffer.
;;
;; Function: ekp-no-break-region (beg end &optional announce)
;;;###autoload (autoload 'ekp-no-break-region "ekp" nil t)
;; Mark BEG through END as an unbreakable session-local atom.
;; ANNOUNCE requests interactive feedback.
;;
;; Variable: ekp-number-unit-suffixes
;; Exact suffixes recognized by the compact number-unit classifier.
;;
;; Function: ekp-org-setup ()
;;;###autoload (autoload 'ekp-org-setup "ekp" nil nil)
;; Protect common Org structural faces in the current buffer.
;;
;; Variable: ekp-overlong-token-policy
;; Policy for ordinary non-CJK tokens wider than the measure.
;;
;; Variable: ekp-para-cache-limit
;; Maximum number of cached paragraphs.
;; When exceeded, the whole paragraph cache is flushed (cheap to rebuild).
;;
;; Function: ekp-param-reset ()
;;;###autoload (autoload 'ekp-param-reset "ekp" nil t)
;; Clear explicit spacing parameters; defaults are derived per string again.
;;
;; Function: ekp-param-set (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
;;;###autoload (autoload 'ekp-param-set "ekp" nil nil)
;; Set all spacing parameters explicitly; persist until `ekp-param-reset'.
;; The nine pixel values, in order, are LWS-I LWS-+ LWS-- MWS-I MWS-+
;; MWS-- CWS-I CWS-+ CWS--: ideal, stretch (+) and shrink (-) for the
;; Latin (LWS), mixed (MWS) and CJK (CWS) word spaces.
;;
;; Variable: ekp-parshape
;; Per-line layout, as a sequence of (INDENT . WIDTH) cons cells.
;; Line i (0-based) uses element i; lines beyond the last element reuse
;; it (like TeX \parshape).  INDENT is the left offset in pixels,
;; WIDTH the text width — the rendered line occupies INDENT + WIDTH.
;; Line-number-dependent widths require the (position × line-count) DP,
;; so this is Elisp-only: the C module is bypassed while set.
;;
;; Function: ekp-pixel-justify (string line-pixel)
;;;###autoload (autoload 'ekp-pixel-justify "ekp" nil nil)
;; Justify multiline STRING to LINE-PIXEL pixels.
;; Each newline-separated segment is treated as one paragraph.
;; When the C module is available, paragraphs are computed in parallel.
;;
;; Function: ekp-pixel-range-justify (string min-pixel max-pixel)
;;;###autoload (autoload 'ekp-pixel-range-justify "ekp" nil nil)
;; Find optimal width for STRING between MIN-PIXEL and MAX-PIXEL.
;; Returns (justified-text . optimal-pixel).
;;
;; Variable: ekp-protrusion
;; Non-nil enables right-edge character protrusion (hanging punctuation).
;; A line ending in punctuation lets part of that glyph hang past the
;; flush edge, per `ekp-protrusion-ratios' — CLREQ line-end punctuation
;; squeeze and microtype-style hanging periods/hyphens in one mechanism.
;; Left-edge protrusion is not implemented: Emacs cannot render text
;; before the line origin.  Buffer integration reserves the protrusion
;; width in its layout when enabled.
;;
;; Variable: ekp-protrusion-ratios
;; Alist CLASS → RATIO of the glyph width allowed to protrude.
;; `cjk-close': fullwidth closers (。、」); 0.5 hangs exactly the
;; whitespace half of the glyph — visually equivalent to CLREQ line-end
;; compression.  `latin-close': chars from `ekp--no-line-start-chars'
;; ending a word (period, comma, quotes).  `hyphen': the soft hyphen
;; inserted at a break.
;;
;; Variable: ekp-ragged-stretch-pixel
;; Per-line end-of-line flexibility (pixels) for non-justify alignment.
;; This is what a ragged line may fall short of the target width without
;; badness reaching infinity (like \raggedright with a finite \rightskip
;; stretch).  nil derives 8× the Latin word-space ideal (≈2 em).
;;
;; Function: ekp-refill-paragraph ()
;;;###autoload (autoload 'ekp-refill-paragraph "ekp" nil t)
;; Apply a complete KP layout to the hard paragraph at point.
;;
;; Variable: ekp-token-break-policies
;; Break policies for bounded automatic token classifiers.
;;
;; Function: ekp-unjustify-buffer ()
;;;###autoload (autoload 'ekp-unjustify-buffer "ekp" nil t)
;; Remove EKP projection from the accessible buffer.
;;
;; Function: ekp-unjustify-region (beg end)
;;;###autoload (autoload 'ekp-unjustify-region "ekp" nil t)
;; Remove EKP display projection intersecting BEG through END.
;;
;; Variable: ekp-use-c-module
;; When non-nil, use C dynamic module for DP computation if available.
;; The C module provides significant performance improvement for large texts.
;; Set to nil to force pure Elisp implementation.
;;
;; Function: ekp-verbatim-region (beg end &optional announce)
;;;###autoload (autoload 'ekp-verbatim-region "ekp" nil t)
;; Keep paragraphs intersecting BEG through END verbatim this session.
;; ANNOUNCE requests interactive feedback.
;;

;;; Code:

;;;###autoload
(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "lisp"
                                 (file-name-directory
                                  (or load-file-name
                                      (bound-and-true-p byte-compile-current-file)
                                      buffer-file-name)))))

(require 'ekp-layout)
(require 'ekp-buffer)

(provide 'ekp)
;;; ekp.el ends here
