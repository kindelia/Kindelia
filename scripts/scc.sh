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

  # {
  #   echo "FOLDER"
  #   echo "======"
  #   $count_cmd ./folder/*.rs
  #   echo
  # } >>"$outfile"

)
