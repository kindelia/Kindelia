FROM rust:1.60-buster as build

# Cache crates.io index
RUN cargo search --limit=0

# Cache dependencies
COPY ./Cargo.toml ./Cargo.lock ./
RUN mkdir src && touch ./src/lib.rs
RUN sed -i '/^default-run = /d' Cargo.toml
RUN cargo build --lib --release

# Build binary
COPY ./Cargo.toml ./Cargo.lock ./
COPY ./src/ ./src/
RUN cargo build --bin=kindelia-node-headless --release


# from alpine:3.15
from debian:11-slim

WORKDIR /app/
COPY --from=build ./target/release/kindelia-node-headless /app/
CMD ["/app/kindelia-node-headless"]
CMD ["/bin/bash"]
