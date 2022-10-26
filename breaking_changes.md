# Breaking changes for next deploy

## Serialization

- [X] reversion of TX_COUNT bit order
- [X] `fun` initial state is optional

## Chain state

- [X] genesis code on genesis block
- [X] prev of genesis block is zero (0x00)
- [X] genesis block with computed hash, instead of arbitrary number (as it now
      has contents)
