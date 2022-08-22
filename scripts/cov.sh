# to execute this script install llvm-cov with
# cargo install cargo-llvm-cov
# you can use it as `./script/cov` or `./script/cov test::hvm` (runs only hvm tests)
cargo llvm-cov --open --profile=dev_fast -- $1
