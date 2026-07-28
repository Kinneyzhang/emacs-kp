# Plan: Repository System Audit 2026-07-28

## Context

This audit is a follow-up inside the existing `phase-kp-overhaul-20260726`.
It does not open a new phase because the requested repository-wide health
review is part of the same overhaul purpose.

## Milestones

1. Inventory the repository, architecture, public contracts, current phase,
   and release state.
2. Audit Elisp core, C module, buffer/region integration, tests, GUI,
   dictionaries, documentation, and release maintenance.
3. Reproduce high-risk findings and run the existing verification surfaces
   from a clean source snapshot.
4. Publish a prioritized system audit with evidence, inference boundaries,
   improvement directions, extension options, and explicit non-goals.
5. Record open issues and correct factual drift in public documentation.

## Scope

- Runtime source is read-only for this task.
- Documentation changes may add the audit and correct facts already proven
  by the repository.
- No release, push, dependency addition, or architectural implementation.

## Evidence

- Emacs 30.2 ERT suite and C/Elisp fuzz suite.
- Clean-copy byte compilation, checkdoc, C release build, ERT, and fuzz.
- GUI fit matrix across scaling, remapping, fringe, and narrow-window cases.
- Targeted negative probes for caches, save failure, copy-filter composition,
  and test isolation.
- Local/remote Git and tag inspection.

## Risks and Dependencies

- Windows and remote CI were not available locally.
- macOS sanitizer runtime policy prevented loading the debug C module.
- The GUI matrix produced numerical evidence, but the clean screenshot path
  was obstructed by a macOS permission prompt.

## Stop Condition

The audit is complete when findings are prioritized and documented, factual
README drift is corrected, phase records are updated, the complete diff is
reviewed, and no runtime source file has changed.
