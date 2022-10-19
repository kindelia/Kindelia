#!/bin/sh
jq -r '
  to_entries[]
  | .key as $ref
  | (
      .value 
      | to_entries[]
      | .key as $bench
      | .value
      | [$bench, $ref, .mean, .deviation]
    )
  | @tsv
  '
