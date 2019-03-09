.PHONY: build test-deb test

build: entangled_0.2.0.0_amd64.deb

test: test-deb

entangled_0.2.0.0_amd64.deb: debian/Dockerfile
	stack dist --tar-dir debian
	docker build -t entangled-debian debian
	docker run -v $$(readlink -f .):/mnt entangled-debian cp entangled_0.2.0.0_amd64.deb /mnt

test-deb: entangled_0.2.0.0_amd64.deb
	cp entangled_0.2.0.0_amd64.deb debian-test
	docker build -t entangled-debian-test debian-test
	docker run -v $$(readlink -f ./mnt):/mnt -w /mnt -t entangled-debian-test entangled test.md
