#!/bin/sh

echo_run() {
  echo "> " "$@"
  "$@"
  echo
}

set -e
set -x

cargo install --path=.

echo "0000000000000000000000000000000000000000000000000000000000000001" >/tmp/secret

kindelia serialize ./example/block_1.kdl >/tmp/block_1.hex.txt

cat /tmp/block_1.hex.txt

kindelia deserialize /tmp/block_1.hex.txt

kindelia test ./example/block_1.kdl

kindelia serialize example/block_3.unsig.kdl | kindelia sign -eE -s /tmp/secret - >/tmp/block_3.sig.hex.txt

cat /tmp/block_3.sig.hex.txt

kindelia deserialize /tmp/block_3.sig.hex.txt

kindelia deserialize /tmp/block_3.sig.hex.txt > /tmp/block_3.sig.kdl

kindelia test /tmp/block_3.sig.kdl

# ---------

kindelia get fun Count state

kindelia get fun Count code

kindelia get stats

# kindelia run-remote ./example/block_2.kdl || true

# kindelia publish ./example/block_2.kdl

# sleep 5
