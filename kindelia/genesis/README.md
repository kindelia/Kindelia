# About this directory

The `genesis/networks` directory holds genesis block files, one per network.

Important! All files inside the `networks` sub-directory get compiled into the 
`kindelia` executable.  So please do not put any other type of file inside and
observe the naming convention.

The files are named to match hexadecimal network identifiers as specified
in the config file with extension `.kdl`.  For example, `network/0xCAFE0006.kdl` corresponds to network `0xCAFE0006`.

Typically a new network is created by bumping this value, eg creating the
file `networks/0xCAFE0007.kdl`. Also `../default.toml` should be updated to match.