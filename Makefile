.PHONY: test dist

test:
	cabal build
	cabal test
	cd bash/test && ./run-tests.sh

dist:
	./bash/make-tar

packaging/alpine-ghcup.sif: packaging/alpine-ghcup.def
	singularity build -f $@ $<

static-tar: packaging/alpine-build.sif packaging/alpine-ghcup.sif
	singularity build -f --sandbox /tmp/alpine-entangled $<
	singularity run -f --no-home --writable /tmp/alpine-entangled

