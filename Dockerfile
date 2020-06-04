# Build with:
# ```shell
# docker build -t nlesc/entangled .
# ```
FROM haskell:8 AS builder

WORKDIR /opt/entangled

RUN cabal update

COPY . /opt/entangled

RUN cabal install -j6 --installdir=/tmp/dist --install-method=copy && \
    strip /tmp/dist/entangled

FROM debian:buster

RUN apt update -q && apt install -y libgmp10 libncurses5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /tmp/dist/entangled /usr/bin/entangled
COPY --from=builder /opt/entangled/data /usr/lib/entangled/data

ENV entangled_datadir=/usr/lib/entangled

WORKDIR /data

ENTRYPOINT ["entangled"]
