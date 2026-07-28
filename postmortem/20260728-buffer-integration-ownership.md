# Let Layout Spans and Auto Mode Own Buffer Integrations

> Updated by `20260728-public-filter-dispatch.md`: the same composition
> decision now delegates through Emacs's public dispatcher instead of its
> private helper, with DELETE cleanup ordered after dynamic-binding unwind.

## Context

`filter-buffer-substring-function` is a single-slot protocol. EKP replaced
that slot and later killed its local binding, losing any prior buffer-local
owner. Separately, public unjustify removed layout markers but left save,
search, change, and copy integrations installed.

## Decision

On first installation, record both the previous filter value and whether it
was buffer-local. The composed function invokes that filter first, preserving
its buffer context and DELETE semantics, then structurally inverts EKP
markers in the returned string.

Separate the structural unjustify core from the public lifecycle wrapper.
Internal reflow, isearch, serialization, and teardown call the core. Public
unjustify and external edits remove integrations when neither auto mode nor
any justified span still needs them.

## Alternatives Rejected

### Replace the prior filter

This silently breaks major-mode or user copy semantics and cannot be called
composition.

### Run the prior filter on a temporary logical buffer

Arbitrary filters may depend on the original buffer's local variables,
positions, fields, and deletion behavior. A temporary buffer cannot preserve
that contract.

### Leave hooks installed because most become no-ops

The copy slot remains occupied, hidden state survives after the visible
feature is gone, and later packages cannot recover ownership predictably.

## Consequences

- Prior local and inherited filters are restored exactly.
- Copy and kill preserve both prior-filter and EKP logical-text semantics.
- Internal temporary unjustify no longer changes integration lifecycle.
- A prior filter that intentionally removes text properties also removes
  EKP marker evidence; that filter owns its returned representation.
- No new abstraction layer or external dependency was introduced.

## Verification

The old model failed both the composition and final-span cleanup controls.
The new model passes five public-path cases plus the existing isearch/save/
mode suite. Full ERT passes 107/107, fuzz passes 300/300, and
warnings-as-errors byte compilation plus checkdoc are clean.

## Rollback

Restore direct filter assignment and the monolithic public unjustify
function. No persisted data needs migration.
