# uses hyperfine to compare two branches
# you can use this as `./scripts/bench.sh refactor-name 'cargo run --profile=dev_fast -- run example/block_1.kdl'`
hyperfine \
  --warmup 1 \
  -n "master" \
  --prepare "git checkout master" \
  "$2" \
  -n $1 \
  --prepare "git checkout $1" \
  "$2" \
