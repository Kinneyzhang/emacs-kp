# Measure Before Replacing Text Builders

## Context

The tokenizer appended each character to an accumulator string, dense
hyphen insertion rebuilt the whole prefix at every break, and the cache used
nil for both “absent” and “computed with no breaks.”  These are familiar
smells, but ordinary paragraph performance did not prove they mattered.

An adversarial 1,000–8,000-character benchmark established the boundary:
8× input grew tokenizer time 10.3× and dense insertion 11.5×.  Repeated
no-break lookups also called the dictionary computation twice.

## Decision

Keep ownership in the existing tokenizer and hyphenator.  Accumulate
fragments locally, join once at the point each complete value is emitted,
and use one private sentinel to distinguish a missing cache entry from a
cached nil.  No general builder layer is introduced.

## Alternatives

- Leaving the loops unchanged was rejected after scaling and absolute time
  both showed material cost.
- A reusable rope/builder abstraction was rejected because the two loops
  have different owners and need only local lists.
- Moving either operation to C was rejected because text properties,
  Unicode segmentation, and dictionary semantics remain Elisp-owned.

## Consequences

At 8,000 characters, source-mode tokenizer time fell from 3.133 s to
1.100 s and dense insertion from 0.945 s to 0.013 s.  Growth across the
range is 6.3× and 7.7× respectively for 8× input.  Negative hyphenation
results now reuse the cache.  Exact output and text-property regressions
protect the representation change.

## Rollback

Revert the fragment builders together with their benchmark claims.  Keep
the nil-cache sentinel and all output regressions unless evidence shows the
cache invalidation boundary itself is wrong.
