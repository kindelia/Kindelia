#!/bin/sh

count_cmd="scc --no-cocomo"

(
  set -e
  cd ./src/
  outfile="../scc.txt"

  true >"$outfile"

  {
    echo "CORE"
    echo "===="
    $count_cmd ./*.rs
    echo
  } >>"$outfile"

  {
    echo "FRONTEND"
    echo "========"
    $count_cmd ./frontend/*.rs
    echo
  } >>"$outfile"

)
