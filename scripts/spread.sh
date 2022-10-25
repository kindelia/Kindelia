#!/bin/bash
set -e

peers="$(kindelia get peers)"

for p in $peers; do
  p=$(echo -n "$p" | grep -P -o "(.+)(?=:)")
  echo "Publishing to $p..."
  KINDELIA_API_URL="http://$p"  kindelia publish "$@"
done
