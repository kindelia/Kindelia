FROM rust:1.60-buster as build

# Cache crates.io index
RUN cargo search --limit=0

# Cache dependencies
COPY ./Cargo.toml ./Cargo.lock ./
RUN mkdir src && touch ./src/lib.rs
# RUN sed -i '/^default-run = /d' Cargo.toml
RUN cargo build --lib --release

# Build binary
COPY ./Cargo.toml ./Cargo.lock ./
COPY ./src/ ./src/
RUN cargo build --bin=kindelia-node --release


# from alpine:3.15
from debian:11-slim

WORKDIR /app/

COPY --from=build ./target/release/kindelia-node /app/

CMD ["/app/kindelia-node --no-ui"]

EXPOSE 42000/udp
