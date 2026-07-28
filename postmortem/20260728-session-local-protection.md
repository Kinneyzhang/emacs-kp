# Manual Protection Is Session State, Not Document Syntax

## Context

`ekp-no-break-region` and `ekp-verbatim-region` correctly changed layout but
were hard to discover and gave no feedback.  Because they attach ordinary
text properties, a user could reasonably assume saving preserves the
protection even though plain-text serialization intentionally removes all
layout metadata.

## Decision

Keep manual protection session-local and say so at every user boundary:
interactive messages, command and mode help, the EKP menu, and bilingual
README sections.  Existing removal commands appear beside mark commands.
Persistent document semantics remain owned by major-mode faces or the
buffer-local skip predicate.

## Alternatives

- Serializing properties was rejected because it changes file formats and
  requires mode-specific round-trip semantics.
- Adding a transient UI or global bindings was rejected because a standard
  minor-mode menu and `C-h m` already provide discovery without another
  interaction model.
- Silently leaving behavior unchanged was rejected because the lifetime
  ambiguity is user-visible even when layout is technically correct.

## Consequences

Interactive commands now provide immediate, consistent confirmation.
Users can discover mark and clear operations from one EKP menu.  Plain files
remain unchanged, and Org/Markdown/source-aware modes retain the existing
face/predicate integration route.

## Rollback

Remove the menu and feedback together with their documentation.  Do not
claim persistence unless a future design specifies storage, migration,
failure behavior, and mode ownership first.
