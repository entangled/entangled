.PHONY: test dist static-tar

test:
	cabal build
	cabal test
	cd bash/test && ./run-tests.sh

dist:
	./bash/make-tar

packaging/alpine-ghcup.sif: packaging/alpine-ghcup.def
	singularity build -f $@ $<

static-tar: packaging/alpine-build.def packaging/alpine-ghcup.sif
	singularity build --update -f --sandbox /tmp/alpine-entangled $<
	singularity run -f --no-home --bind .:/mnt --writable /tmp/alpine-entangled

