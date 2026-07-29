# Name Editor Integration After Its Buffer Owner

## Context

`ekp-region.el` began as an in-place region formatter, but its stable
responsibility grew to the whole buffer lifecycle: whole-buffer commands,
window-width tracking, incremental reflow, save serialization, isearch,
kill/yank filtering, modified-state preservation, mode presets, and teardown.
The old module name described one public operation scope rather than the
state and external protocols the module owns.

That mismatch also spread into the feature name, customization group,
configuration variables, private implementation prefix, test file, CI, and
documentation. A reader looking for buffer integration had to know the
historical `region` name first.

## Decision

Rename the module directly to `ekp-buffer.el` and provide `ekp-buffer`.
Rename the customization group, module-owned configuration surface, private
implementation namespace, test file, and test namespace to `ekp-buffer*`.
Attach the subgroup to the top-level `ekp` customization group.

Keep public operation names whose final component describes the real
operand or workflow:

- `ekp-justify-region` and `ekp-unjustify-region`
- `ekp-justify-buffer` and `ekp-unjustify-buffer`
- the no-break/verbatim region commands
- `ekp-auto-justify-mode`, setup commands, refill, and diagnostics

The unexported protrusion-reserve calculation now uses the conventional
double-hyphen private name `ekp-buffer--protrusion-reserve`.

## Compatibility Decision

This is an explicit breaking change in the Unreleased line. Remove the old
file, feature, variables, and internal names instead of keeping aliases or a
loader shim. The migration is recorded in `CHANGELOG.md`:

```elisp
(require 'ekp-buffer)
```

Configurations that set the module-owned `ekp-region-*` variables must use
their corresponding `ekp-buffer-*` names.

## Alternatives Rejected

### Keep `ekp-region`

This preserves source compatibility but keeps the wrong owner in every file
and symbol lookup. The mismatch is concrete, not hypothetical: most of the
module handles buffer-wide state or editor protocols.

### Rename to `ekp-mode`

The minor mode is only one entry point. Manual region and whole-buffer
commands install the same integration lifecycle without enabling the mode.

### Split Region Commands from Buffer Integration

The commands, reversible markers, serialization, search, copy filtering, and
mode state share one lifecycle. Splitting them would add declarations and
cross-file glue without creating independent owners.

### Keep a Compatibility Loader or Aliases

Two feature names and parallel variable surfaces would preserve the naming
debt and create an indefinite compatibility boundary. The requested
breaking migration has one direct replacement and no persisted-data format
change.

## Consequences

- File and feature lookup now identify the actual buffer integration owner.
- Public commands remain semantically precise instead of receiving a
  redundant `buffer` prefix.
- Customization appears under the main EKP group.
- Test names, CI commands, and documentation use the same vocabulary.
- Existing configurations must update their `require` and any
  module-owned settings; layout behavior and saved bytes do not change.

## Verification

The pre-change behavior lock passed 130/130 ERT. After the rename, focused
buffer ERT passed 44/44; default and seeded-permuted full ERT passed 130/130;
all 130 selected ERT tests passed one per fresh Emacs process; and C/Elisp
fuzz passed 300/300.

Warnings-as-errors byte compilation, checkdoc, pinned package-lint, release
invariants, shell syntax, diff checks, and active-surface stale-name scans
passed. A mechanical equivalence check compared the new runtime and test
files with the old files transformed by the approved rename table; the only
additional runtime differences were the EKP customization parent and the
private protrusion helper name.

## Rollback

Reverse the direct file/feature/symbol mapping and the documentation/CI
references together. No data migration or C module rebuild is required.
