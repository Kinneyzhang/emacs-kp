# Plan: Composable Buffer Integration Lifecycle

## Scope

Resolve `issue003`: compose and restore an existing substring filter, and
remove save/search/copy integrations when no justified span or auto mode
needs them.

## Resolution Path

1. Drive the public `filter-buffer-substring` path with a prior local filter
   and prove EKP currently overwrites it.
2. Record the prior filter and whether it was local; define prior-filter
   then logical-inversion order so DELETE remains owned by the prior path.
3. Split internal structural unjustify from the public lifecycle wrapper.
4. Derive integration removal from actual spans plus auto-mode state,
   including external deletion and mode shutdown.
5. Verify copy, delete, local/global restoration, manual unjustify,
   isearch, mode disable, full ERT, fuzz, compilation, and checkdoc.

## Non-goals

- No new command/menu surface.
- No general multi-filter framework.
- No save-serialization redesign beyond preserving `task004`.

## Rollback

Restore direct filter assignment and the prior public unjustify body, then
remove the composition/lifecycle tests. No persisted state changes.
