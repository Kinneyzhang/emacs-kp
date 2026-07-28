# Plan: Interactive Protection Workflow 2026-07-28

## Goal

Make the existing no-break and verbatim workflows discoverable and
unambiguous without inventing persistence or another command layer.

## Resolution Path

1. Exercise all four protection commands through `call-interactively`.
2. Prove no-break affects the real formatter and verbatim affects the real
   region formatter.
3. Give every property change explicit feedback that states its
   current-buffer-session lifetime.
4. Expose the existing commands in the minor mode's standard menu and mode
   help; keep the existing `fill-paragraph` remap.
5. Document removal commands and session-local lifetime in both READMEs.

## Default Behavior

Protection text properties remain local to the live buffer.  Saving writes
logical text only; reopening does not restore manually applied protection.
Mode-native faces/predicates remain the persistent-source integration path.

## Non-goals

- Do not serialize text properties or change the file format.
- Do not add a transient UI, dependency, or global key binding.
- Do not infer language/mode syntax beyond the existing Org/Markdown
  presets.

## Validation

Focused red/green public-command ERT, full ERT in default/permuted/isolated
orders, warning-as-error byte compilation, checkdoc, and documentation/diff
checks.
