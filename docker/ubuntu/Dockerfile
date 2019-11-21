FROM ubuntu:18.4

RUN apt-get update && apt-get install -y \
    cabal-debian \
    cabal-install \
    devscripts \
    ghc

RUN apt-get install --no-install-recommends -y \
    build-essential \
    cdbs \
    fakeroot \
    haskell-devscripts

RUN apt-get install --no-install-recommends -y \
    libghc-either-dev \
    libghc-extra-dev \
    libghc-fsnotify-dev \
    libghc-fsnotify-dev  \
    libghc-hspec-dev  \
    libghc-microlens-platform-dev \
    libghc-optparse-applicative-dev \
    libghc-prettyprinter-dev \
    libghc-prettyprinter-ansi-terminal-dev \
    libghc-random-dev \
    libghc-regex-tdfa-dev \
    libghc-terminal-size-dev \
    libghc-uuid-dev

COPY entangled-0.2.0.0.tar.gz /tmp

WORKDIR /tmp
RUN tar xvf entangled-0.2.0.0.tar.gz \
    && cd entangled-0.2.0.0 \
    && cabal-debian -m "Johan Hidding <j.hidding@esciencecenter.nl>" \
        --native -s entangled -e entangled --disable-profiling \
        --disable-haddock --disable-tests \
        --depends "entangled:libatomic1 (>= 8) libc6 (>= 2)" \
    && debuild -us -uc
