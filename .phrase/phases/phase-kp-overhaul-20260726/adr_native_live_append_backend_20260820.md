# ADR: Native Backend for Automatic Live Append 2026-08-20

## Context

Source-loaded automatic live append exceeded the interaction budget because
the strict Elisp append DP interpreted every structural transition. The
existing C backend already accepts the prepared paragraph arrays and produces
exactly the same break result, but `ekp-use-c-module=nil` previously disabled
it even inside the live append path.

## Decision

Add `ekp-auto-justify-native-append`, defaulting to non-nil. When auto mode is
publishing an already prepared, context-safe 1D live append and the compatible
C module is loaded, `ekp--dp-cache-append` may use the native DP regardless of
the ordinary full-layout `ekp-use-c-module` setting. The string API and full
paragraph layout continue to obey `ekp-use-c-module` directly. Setting the new
option to nil restores pure Elisp live append; an unavailable module always
falls back to Elisp.

The native call receives the same prepared 15-field arrays and is validated by
the existing C/Elisp parity contract. No C ABI field or source projection
representation changes.

## Alternatives

- Duplicate the strict Elisp DP for append: rejected after the parity
  experiment required a second 160-line transition kernel and returned nil.
- Reuse final-pass transient state: rejected because artificial candidates and
  surviving-path arrays are not part of the persisted state.
- Keep the source stress debt open: insufficient after the user selected the
  native live-append architecture.

## Consequences

- A loaded C module accelerates live append even when full layout is explicitly
  configured for Elisp; this is documented and user-controllable.
- The remaining source latency belongs to Elisp-owned append preparation and
  semantic plan assembly, which remain exact and testable.
- Native-unavailable environments retain the previous pure-Elisp behavior.

## Verification

- A public buffer regression proves native calls occur only when the option is
  enabled and are absent when it is disabled.
- Existing append-chain, C/Elisp parity, fuzz, and source-clean tests remain
  required; source-fresh evaluator reports native-live usage explicitly.

## Rollback

Set `ekp-auto-justify-native-append` to nil or revert the dispatch change;
the full layout API and valid C contract remain independently usable.
