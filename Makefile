input_files = README.md
html_dir = ./public
build_dir = ./build

# === Stuff ==============================================================
SHELL := /bin/bash

format = markdown+fenced_code_attributes+citations+all_symbols_escapable+fenced_divs+multiline_tables
pandoc_filters = pandoc-eqnos pandoc-fignos pandoc-citeproc
report_args = --toc $(pandoc_filters:%=--filter %) --lua-filter "scripts/annotate-code-blocks.lua" --template scripts/eisvogel.tex --listings 
html_args = -s --toc --toc-depth=3 $(pandoc_filters:%=--filter %) --lua-filter "scripts/annotate-code-blocks.lua" --mathjax --css "style.css" --base-header-level=2

pd_call = pandoc -f $(format) --lua-filter "scripts/$(1).lua" -t plain
pd_list = $(call pd_call,list)
pd_tangle = $(call pd_call,tangle)

sources = $(shell $(pd_list) $(input_files))
targets = $(shell $(pd_list) README.md)

# === Build docs =========================================================
tangle: $(input_files)
	mkdir -p $(build_dir)
	$(pd_tangle) $^ > $(build_dir)/tangle.sh
	source $(build_dir)/tangle.sh

report: $(pdf_files)

html: $(html_files)
	cp -r figures $(html_dir)
	cp scripts/style.css $(html_dir)

$(html_dir)/%.html: $(input_files)
	mkdir -p $(html_dir)
	pandoc $^ -f $(format) $(html_args) -t html5 -o $@

$(build_dir)/%.pdf : $(input_files)
	pandoc $^ -f $(format) $(report_args) -t latex -o $@ --pdf-engine=xelatex

$(sources): tangle

# === Tests ==============================================================

test_runner.py = python3
test_runner.sh = bash

run_tests = $(foreach x,$(targets),\
	echo "Running $(x) ...";\
	$(test_runner$(suffix $(x))) $(x);)

all: $(targets)

$(targets): $(input_files)
	$(pd_tangle) $< | bash

.PHONY: clean test tangle
.SILENT: clean test

clean:
	rm -vf $$($(pd_list) README.md)

test: all
	$(run_tests)