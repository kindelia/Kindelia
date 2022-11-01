# Breaking changes

## Next

- [ ] `Kdl.` namespace

## v0.1.5 2022-11-01

- `network_id`: `0xCAFE0004`
- genesis hash: `0x39a01224353cc2536b29b80199c03d359230db3794962e25abb30b2a58e9af4d`

### Serialization

- reversion of TX_COUNT bit order
- `fun` initial state is optional

### Chain state

- genesis code on genesis block
- prev of genesis block is zero (0x00)
- genesis block with computed hash, instead of arbitrary number (as it now has
  contents)
