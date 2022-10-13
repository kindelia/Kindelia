# Breaking changes for next deploy

## Networking

- [ ] inversion of TX_COUNT bit order
- [ ] 2 bits for statement tag serialization

## Chain state

- [ ] genesis code on genesis block
- [ ] prev of genesis block is zero (0x00)
- [ ] genesis block with computed hash, instead of arbitrary number (as it now
      has contents)
