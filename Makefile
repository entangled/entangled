.PHONY: docs all debian build install clean

all: docs

docs: docs/99-bottles.html docs/elm-slasher.html docs/index.html docs/slasher.html docs/slasher.min.js docs/slasher.css docs/.nojekyll docs/screenshot.png docs/hello-world.html docs/entangled.html

docs/.nojekyll:
	touch $@

docs/screenshot.png: examples/elm-slasher/screenshot.png
	cp $^ $@

docs/slasher.html docs/slasher.min.js docs/slasher.css: examples/elm-slasher/elm-slasher.md
	cp -r examples/elm-slasher build-slasher ; cd build-slasher ;\
	../scripts/tangle ../$^ ;\
	make ;\
	cp slasher.html slasher.min.js slasher.css ../docs ;\
	cd ..; rm -rf build-slasher

docs/99-bottles.html: examples/99-bottles/99-bottles.md scripts/header.html
	./scripts/weave $^ --output=$@	

docs/elm-slasher.html: examples/elm-slasher/elm-slasher.md scripts/header.html
	./scripts/weave $^ --output=$@

docs/index.html: docs/site.md scripts/header.html
	./scripts/weave $^ --output=$@

docs/hello-world.html: examples/hello-world/hello-world.md
	cd $(<D) ;\
	../../scripts/weave $(<F) --output=../../$@

litcode := entangled main document_model database daemon configuration logging megaparsec stitch tangle
litcode_source := $(litcode:%=lit/%.md)
format := "markdown+fenced_code_attributes+citations+all_symbols_escapable+fenced_divs+multiline_tables+fenced_divs+bracketed_spans"

pandoc_args := -s -t html5 --toc
pandoc_args += --highlight-style style/syntax.theme
pandoc_args += --filter pandoc-citeproc --mathjax
pandoc_args += --filter pandoc-test
pandoc_args += --css nlesc.css
pandoc_args += --template=style/template.html

docs/entangled.html: Makefile $(litcode_source)
	pandoc -f $(format) $(pandoc_args) $(litcode_source) -o $@

build: 
	stack build

install:
	stack install

debian: package.yaml
	cabal-debian -m "Johan Hidding <j.hidding@esciencecenter.nl>" --native -s entangled -e entangled --depends "entangled:libatomic1 (>= 8)"
	debuild -us -uc

clean:
	rm -rf debian
