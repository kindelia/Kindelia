# Breaking changes for next deploy

## Serialization

- [ ] inversion of TX_COUNT bit order
- [X] `fun` initial state is optional

## Chain state

- [ ] genesis code on genesis block
- [ ] prev of genesis block is zero (0x00)
- [ ] genesis block with computed hash, instead of arbitrary number (as it now
      has contents)
