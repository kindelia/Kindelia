# TODO

## 1

- separate KHVM language module
- organize `kindelia_code::config`

- add total work field to heartbeat event (Rhediner)

- split HTTP client and server crates (Rheidner)

- fix problem of 1 tick rollback request causing 256 ticks rollback (Kelvin)
  - [ ] reimplement rollback algorithm on separate lib
  - [ ] integrate on KHVM

- sugar big numbers to hex format
- show space and mana usage on fun definition

- `kindelia subject`
- command to publish to multiple nodes

- KHVM refactors
  - `Io` enum
  - `split_names` without using strings
  - `get_tag`, `get_ext` etc on `RawCell`
  - `get_name_from_ext` on `RawCell`
  - `Cell` enum

- fix `Drop` of too nested terms

- contract UX improvements
  - failure opcode
  - nonce contract
  - node transaction heuristics
  - node transaction sequencing

## 2

- way to check if transaction(s) fit on a block
- command to list names defined inside of `.kdl` file
- `kindelia get name`
- commands to interact with txs on mempool
- rename "slots" or other references to space to "cells"?

## 3

- flag to enable logging statements results (disabled by default)
- unify `crypto::Hash` with `api::Hash`
- avoid conversions between `crypto::Hash` and `U256`
