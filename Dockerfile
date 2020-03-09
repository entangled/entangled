# Build with:
# ```shell
# docker build -t nlesc/entangled .
# ```
# Run with:
# ```shell
# docker run --rm -u $(id -u):$(id -g) -v $PWD:/data nlesc/entangled README.md
# ```

FROM haskell:8 AS builder

WORKDIR /opt/entangled

RUN cabal update

# COPY entangled.cabal /opt/entangled/

# RUN cabal install --only-dependencies

COPY . /opt/entangled

RUN cabal install

FROM debian:stretch

RUN apt update && apt install -y libgmp10 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.cabal/bin/entangled /usr/bin/entangled

WORKDIR /data

ENTRYPOINT ["entangled"]
