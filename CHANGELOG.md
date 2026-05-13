# Changelog

## 0.7.0

### Breaking

- Minimum supported Erlang/OTP version is now **25**. OTP 21–24 are no longer tested and the compatibility code paths for those releases have been removed.

### Changed

- CI matrix now covers OTP 25, 26, 27, and 28; `actions/checkout` upgraded to v4.
- `granderl` dependency moved from a git ref to the hex 0.1.5 release.
- `telemetry` dependency bumped from 1.2.1 to 1.4.2.
- README documents the full set of telemetry events shackle emits and their measurement shapes.

### Removed

- `ATOMICS`, `DECENTRALIZED_COUNTERS`, `ETS_TAKE`, and `SSL_HANDSHAKE` platform_define macros.
- `OTP_RELEASE` ifdef block in `shackle_internal.hrl` and the associated pre-OTP-21 logger / stacktrace fallbacks.
- `shackle_utils:info_msg/3` and `shackle_utils:warning_msg/3`.
- ETS-based fallback path in `shackle_status` (the persistent_term + counters path is now unconditional).

## 0.6.20

See git history for details.
