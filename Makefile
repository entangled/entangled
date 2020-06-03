.PHONY: test dist

test:
	cabal build
	cabal test
	cd bash/test && ./run-tests.sh

dist:
	./bash/make-tar

