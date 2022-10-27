# TODO

## 1

- fix 1 tick rollback request causing 256 ticks rollback
- don't write to disk while loading blocks
- show space (and mana?) usage on fun definition
- `kindelia subject`
- command to publish to multiple nodes

## 2

- write state files to disk on separate thread
- way to check if transaction(s) fit on a block
- `kindelia get name`
- command to list names defined inside of `.kdl` file
- commands to interact with txs on mempool
- rename "slots" or other references to space to "cells"?

## 3

- flag to enable logging statements results (disabled by default)
- unify `crypto::Hash` with `api::Hash`
- avoid conversions between `crypto::Hash` and `U256`
