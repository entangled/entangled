.PHONY: docs all

all: docs

docs: docs/99-bottles.html docs/elm-slasher.html docs/index.html docs/slasher.html docs/slasher.min.js docs/slasher.css docs/.nojekyll docs/screenshot.png docs/hello-world.html

docs/.nojekyll:
	@mkdir -p docs
	touch $@

docs/screenshot.png: examples/elm-slasher/screenshot.png
	@mkdir -p docs
	cp $^ $@

docs/slasher.html docs/slasher.min.js docs/slasher.css: examples/elm-slasher/elm-slasher.md
	@mkdir -p docs
	cp -r examples/elm-slasher build-slasher ; cd build-slasher ;\
	../scripts/tangle ../$^ ;\
	make ;\
	cp slasher.html slasher.min.js slasher.css ../docs ;\
	cd ..; rm -rf build-slasher

docs/99-bottles.html: examples/99-bottles/99-bottles.md scripts/header.html
	@mkdir -p docs
	./scripts/weave $^ --output=$@	

docs/elm-slasher.html: examples/elm-slasher/elm-slasher.md scripts/header.html
	@mkdir -p docs
	./scripts/weave $^ --output=$@

docs/index.html: README.md scripts/header.html
	@mkdir -p docs
	./scripts/weave $^ --output=$@

docs/hello-world.html: examples/hello-world/hello-world.md
	@mkdir -p docs
	cd $(<D) ;\
	../../scripts/weave $(<F) --output=../../$@
