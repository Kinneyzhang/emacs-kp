# Repository development contract

This file is the shared development policy for this workspace and its packages.
The workspace copy is authoritative; `python3 scripts/workspace.py sync` distributes
it and the shared checks. Do not customize a package copy. Package-specific
contracts belong in the package manual, architecture document and executable tests.

## Structure

- Keep runtime Emacs Lisp files at the package root, with the package prefix.
- Use `tests/` for automated tests and `tests/fixtures/` for test data.
- Use `examples/` for runnable examples, `benchmarks/` for performance workloads,
  `scripts/` for development/release tools, and `native/` for native implementations.
- Create directories only when they contain maintained files. Use lowercase kebab-case
  directory names. Preserve upstream resource names and license notices.
- Follow `.editorconfig` for whitespace and encoding; preserve upstream resources.
- Keep generated files, caches, local reports and recordings out of version control.
- Do not add compatibility paths, obsolete implementation plans, audit reports,
  milestone inventories, historical placeholder documents or per-task notes.
  Git history holds removed material. Delete obsolete code and its callers together.

## Documentation

- `README.md`: purpose, installation, minimal example, documentation links and checks.
- `CHANGELOG.md`: notable user-visible changes and migration instructions, grouped by
  version; follow Keep a Changelog. Do not fabricate releases or copy commit logs.
- `docs/manual.md`: usage and public interface contracts, including integration APIs.
  Separate task-oriented usage and interface reference into sections in this file.
- `docs/architecture.md`: current ownership, dependency direction and invariants.
  Describe implemented behavior, not unaccepted proposals.
- English uses the default name; Chinese translations use `.zh-CN.md` consistently.
  Keep existing translations aligned when their contract changes. Do not create empty
  translations or documents merely to fill a template. Legal notices are exempt.
- Document each fact once. README links to details. Consumers link to the provider's
  public contract instead of copying it. Function docstrings own precise signatures;
  the manual owns workflows, lifetimes, errors, rollback and cross-module contracts.
- Every package with integration APIs documents supported callers, inputs/outputs,
  ownership, lifecycle, failure semantics and a runnable example in its manual.
  Keep private implementation symbols out of consumer code.

## Development and validation

- Read README and relevant manual/architecture sections before changing a boundary.
- A move or deletion must update all code, Makefiles, tests and documentation references
  in the same change. Use stable behavior-based names, not development-phase labels.
- Put behavioral regressions in tests. Do not require documents to enumerate every
  source file or retired command. Test public examples and integration contracts.
- Test observable public inputs, outputs, errors, rollback and lifecycle. Internal
  helper names, data layouts and call order are not contracts. Remove obsolete
  version/phase tests; consolidate duplicate scenarios. Keep focused internal tests
  only when they materially protect a difficult algorithm or diagnosed defect.
- `tests/acceptance.json` names the maintained public scenarios and their purposes.
  `make check` runs structure validation, compilation and these acceptance cases.
  `make test` runs broader regressions when affected behavior warrants them; GUI
  appearance and performance require their separate acceptance targets.
  Never select acceptance cases merely because they currently pass.
- Run `make structure-check` and the affected tests while developing; run `make check`
  before declaring a package change complete. Workspace changes also run
  `python3 scripts/workspace.py check` and affected consumer checks.
- Each package exposes `compile`, `test`, `check`, `clean`, `structure-check`, and
  `setup-hooks`. `check` includes the structural gate. Serialize commands that clean
  and rebuild shared dependencies; parallel builds must not race over sibling outputs.
- Run `make setup-hooks` after cloning. The pre-commit gate checks staged contents,
  not merely the working tree. CI runs the same structural checker.
- Never weaken a check, add an exclusion or skip a test just to make a failure green.
  Update obsolete checks to the current contract and retain behavioral coverage.
- If a new document or directory does not fit this policy, first simplify the design.
  Necessary policy changes must update the workspace source, shared checker and its
  regression tests together, then synchronize every package.
