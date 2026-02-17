# `kda-tool` Changelog

## 1.2.0 (2026-02-17)

### Improvements

* Capture cumulative `cw-version` branch updates for forked network compatibility and naming alignment.
* Extend CLI network parameter set/help text to support active targets (`mono`, `triad`, `icosa`, plus current dev naming model).
* Change command defaults from legacy mainnet naming to `mono` in key query paths (including `cut`/`mempool` behavior).
* Update template fixtures/golden inputs to the unified target naming model and keep environment/network argument parsing aligned with current runtime profiles.
* Retain host/port parsing and environment handling improvements needed for stable local operator workflows.
* Refresh README wording and template-repo references for the forked repository context.

## 1.1

### Improvements

*   Update to new signing API
*   Allow user to specify the node scheme (i.e. http/https)
*   Add logging verbosity controls, increased logging
*   Add ability to sign and verify arbitrary signatures
*   Allow transactions to be tested before signing with --no-verify-sigs
*   Add -s option to local that gives shortened status output
*   Return an error code if any local transaction doesn't have status "success".

### Bug Fixes

*   Fix handling of holes with a single element array
*   Fix bug in template array handling

## 1.0 (2022-11-09)

Initial release
