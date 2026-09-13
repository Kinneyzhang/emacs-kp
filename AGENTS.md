# Repository development contract

This file is the shared development policy for this workspace and its packages.
The workspace copy is authoritative; `python3 scripts/workspace.py sync` distributes
it and the shared checks. Do not customize a package copy. Package-specific
contracts belong in the package manual, architecture document and executable tests.

## Structure

- Keep multi-file package runtime Lisp in `lisp/`, with the package prefix.
  A single-file package may keep its one runtime file at the root. Do not split
  runtime files between both locations. Keep repository roots focused on navigation,
  configuration and build entry points. Update load paths and release recipes together.
- Organize by responsibility, not file extension. Use `tests/` for correctness
  scenarios and their helpers, `tests/fixtures/` for data, and `tests/tools/`
  for development-tool tests. Keep the acceptance inventory at `tests/acceptance.json`.
- Use `benchmarks/` for performance workloads, dedicated measurement harnesses and
  input data; use `scripts/` for build, release, environment and general runners.
  Tests and benchmarks may use Lisp, Python, Shell or appropriate data formats.
- Use `examples/` for runnable usage examples and `native/` for native implementations.
  Native subprojects follow their language's own layout. Keep domain resources in
  descriptive directories such as `dictionaries/`; do not create generic dumping grounds.
- Share directory meanings and command interfaces across repositories. Create only
  the directories a repository needs; do not impose identical empty skeletons.
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

## Git history and delivery

- Commit promptly after a coherent change has been validated, before starting an
  unrelated change and before ending a completed task. A commit is one explainable,
  independently reviewable and revertible change, not one function or a fixed count
  of files/lines. Keep its code, regression coverage and documentation together.
  Split unrelated fixes, formatting, moves and features; keep necessary path/caller
  updates together so each commit remains usable for `git bisect`.
- Inspect `git status`, the diff and staged diff before committing. Stage only the
  current change; never include another person's unfinished work or generated files.
  Run affected checks and `make check` before committing a completed package change.
  Report failures honestly; do not create a passing-looking checkpoint by hiding them.
- Use Conventional Commits: `type(scope): concrete imperative English summary`.
  Scope is optional; types are feat, fix, docs, refactor, perf, test, build, ci,
  chore and revert. The project limits titles to 72 characters. Avoid generic
  titles such as "update files". After a blank line, explain the problem and resulting
  behavior or reason; include `Validation:` with actual commands/results (or why a
  check was not run). Breaking changes require both `!` and a `BREAKING CHANGE:`
  footer explaining impact and migration. Edit generated merge/revert messages to
  follow this contract. The commit-msg hook checks format, not factual accuracy.
- Commit provider changes before consumers in cross-repository work. Mention relevant
  provider commit hashes in consumer messages when an interface dependency changes.
  At an integrated checkpoint, update workspace.json to the verified child revisions
  and record the checks; never describe uncommitted trees as a reproducible checkpoint.
- Preserve useful checkpoints. Do not amend published commits, force-push, rewrite
  shared history or squash distinct changes without explicit authorization. Prefer a
  focused fix or revert. Use log, blame, diff and bisect when locating regressions;
  mark pre-existing untestable revisions as skipped rather than bad in a bisect.
- The user authorizes automatic upstream pushes: after completing a coherent group
  of validated commits, fetch and inspect divergence, then push the current branch
  to its configured upstream without asking again. Keep local commits timely while
  work is in progress; push at validated delivery checkpoints, not on every save.
  Required acceptance gates must pass; disclose known pre-existing broader failures.
- Do not infer a destination when no upstream exists, or push other remotes, branches
  or tags implicitly. Resolve divergence without force or rewriting shared history.
  If authentication, connectivity or missing upstream prevents delivery, retain local
  commits and report the specific blocker. Never substitute a different remote.
  Report local commit and push status separately; never claim an unpushed commit is
  backed up remotely. Do not bypass the shared hooks to finish a task.
