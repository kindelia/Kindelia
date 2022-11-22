# TODO

## 1

- dirty constructor system

- fix problem of 1 tick rollback request causing 256 ticks rollback (Kelvin)
  - [ ] reimplement rollback algorithm on separate lib
  - [ ] integrate on KHVM

- sugar big numbers to hex format
- show space and mana usage on fun definition

- `kindelia subject`

- KHVM refactors
  - decouple heap storage path from `Runtime` (move logic into `SimpleFileStorage`)
  - `Io` enum
  - `split_names` without using strings
  - `Cell` enum
  - U72 type? ()
    - would be mainly used on `get_ext` (maybe inside `Name`)
    - call it Label?
  - `Absorb` trait

- contract UX improvements
  - [x] nonce contract
  - [ ] transaction sequencing
    - [ ] transactions pool inspection/heuristics system

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
