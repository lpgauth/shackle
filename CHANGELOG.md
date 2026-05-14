# Changelog

## 0.7.2

### Added

- `shackle:receive_response/2` — like `receive_response/1` but with
  a millisecond timeout. Returns `{error, timeout}` if no reply
  arrives within the window. The arity-1 variant still blocks
  indefinitely (unchanged).

- `cast_error/0` is now an exported type — the sum
  `no_server | pool_not_started | shackle_not_started` of atoms
  that `cast/2..4` can return.

### Changed

- `cast/2..4` and `call/2,3` error specs tightened from
  `{error, atom()}` / `{error, term()}` to `{error, cast_error()}`.
  No behavioural change; dialyzer now flags `{error, typo}` at
  call sites that don't match the sum. Existing
  `no_server | pool_not_started | shackle_not_started` were the
  only atoms ever returned.

- Latency measurement in `shackle_server` switched from
  `os:timestamp/0` + `timer:now_diff/2` to
  `erlang:monotonic_time(microsecond)` + subtraction. The
  monotonic clock isn't NTP-jumpable and skips the kernel call,
  so latency telemetry now stays accurate across clock skew
  events and shaves ~50ns off per request.

  The `#cast.timestamp` field type changed from
  `erlang:timestamp/0` (the 3-tuple) to `integer()` (microsecond
  monotonic). The field is internal — clients that don't
  destructure the cast record see no impact. Custom
  shackle_client behaviours that read `Cast#cast.timestamp`
  receive an integer now.

- `vsn` in `shackle.app.src` is now an explicit string
  (`"0.7.2"`) — was `git`, which only resolved correctly when
  building from a checkout.

## 0.7.1

### Changed

- Bumped `foil` dependency from `0.1.3` to `0.1.4` (CI/infrastructure
  refresh; `error/0` type tightened; macro-based DRY refactor with
  no perf change).
- Bumped `metal` dependency from `0.1.1` to `0.1.2` (CI/infrastructure
  refresh; one `system_continue/3` spec fix from `ok` to
  `no_return()`).

No source changes in shackle itself. Behaviour, API, and benchmarks
are identical to 0.7.0.

## 0.7.0

### Breaking

- Minimum supported Erlang/OTP version is now **25**. OTP 21–24 are no longer tested and the compatibility code paths for those releases have been removed.

### Changed

- Replaced the `granderl` dependency with **`knot`** (in-house C NIF
  using wyrand). `shackle_utils:random/1` now calls `knot:uniform/1`
  — same signature, same range guarantees, no behavioural difference
  at call sites. `granderl 0.1.5` on hex.pm fails to build on OTP 27+
  (its `preamble.sh` invokes `erl -noshell -s init stop -eval ...`
  with options in an order that causes the node to terminate before
  `-eval` runs, leaving `ERTS_INCLUDE_DIR` empty); the fix exists in
  granderl's master but its hex package is not under our ownership,
  so we couldn't release a fix. `knot` is a tiny C NIF (~80 LOC)
  that builds correctly on every supported OTP and matches
  hand-written C dispatch overhead. Benchmarks (Apple Silicon, OTP
  29, 10M iterations of `uniform(254)`, median of 5 runs):

  | conc | rand:uniform/1 | granderl:uniform/1 | knot:uniform/1 |
  |---|---|---|---|
  | 1   | 34 ns | 13 ns  | **12 ns**  |
  | 8   | 9 ns  | 7 ns   | **3.3 ns** |
  | 32  | 6 ns  | 8 ns   | **3.2 ns** |
  | 128 | 6 ns  | 8.5 ns | **3.1 ns** |

- CI matrix now covers OTP 25, 26, 27, and 28; `actions/checkout` upgraded to v5.
- `telemetry` dependency bumped from 1.2.1 to 1.4.2.
- README documents the full set of telemetry events shackle emits and their measurement shapes.

### Removed

- `ATOMICS`, `DECENTRALIZED_COUNTERS`, `ETS_TAKE`, and `SSL_HANDSHAKE` platform_define macros.
- `OTP_RELEASE` ifdef block in `shackle_internal.hrl` and the associated pre-OTP-21 logger / stacktrace fallbacks.
- `shackle_utils:info_msg/3` and `shackle_utils:warning_msg/3`.
- ETS-based fallback path in `shackle_status` (the persistent_term + counters path is now unconditional).

## 0.6.20

See git history for details.
