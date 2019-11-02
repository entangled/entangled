---
title: enTangleD
---

> **Literate programming** [/ˈlɪtəɹət ˈpɹəʊɡɹæmɪŋ/]{.phonetic} (computing) Literate programming is a programming paradigm introduced by Donald Knuth in which a program is given as an explanation of the program logic in a natural language, such as English, interspersed with snippets of macros and traditional source code, from which a compilable source code can be generated. [(Wikipedia)](https://en.wikipedia.org/wiki/Literate_programming)


`enTangleD` makes writing literate programs easier by keeping code blocks in markdown up-to-date with generated source files. By monitoring the tangled source files, any change in the master document or source files is reflected in the other. In practice this means:

*    Write well documented code using Markdown.
*    Use any programming language you like (or are forced to use).
*    Keep debugging and using other IDE features without change.
*    Generate a report in PDF or HTML from the same source (see examples on the right).

# Status

`enTangleD` is working, but still in a alpha stage. It has been tested Linux, and Windows and should work on MacOS equally well. If you edit anything serious with the enTangle Daemon running, I strongly recommend using version control and commit often. If you encounter unexpected behaviour, please post an issue and describe the steps to reproduce.

Features:

- live bi-directional updates
- monitor multiple markdown files
- (reasonably) robust against wrongly edited source files
- create PDF or HTML pages from literate source

Todo:

- configurability using Yaml file
- integration with git: commit every change, squash when done
- add workflow to create figures for HTML/PDF reports

# Building

`enTangleD` is written in [Haskell](https://www.haskell.org/), and uses the `cabal` build system. You can build an executable by running

    cabal build

Install the executable in your `~/.local/bin`

    cabal install

Run unit tests

    cabal test

# Using

There are several tools included in the enTangleD distribution:

* `entangled`: the main binary
* `scripts/weave`: a bash script for creating HTML or PDF output using Pandoc.
* `scripts/tangle`: a bash script for tangling sources other than through using `entangled`, this too uses Pandoc.

## Syntax (markdown side)

The markdown syntax `enTangleD` uses is compatible with `Pandoc`'s. This relies on the use of *fenced code attributes*. To tangle a code block to a file:

~~~markdown
``` {.bash file=src/count.sh}
   ...
```
~~~

Composing a file using multiple code blocks is done through *noweb* syntax. You can reference a named code block in another code block by putting something like `<<named-code-block>>` on a single line. This reference may be indented. Such an indentation is then prefixed to each line in the final result.

A named code block should have an identifier given:

~~~markdown
``` {.python #named-code-block}
   ...
```
~~~

If a name appears multiple times in the source, the code blocks are concatenated during tangling. When weaving, the first code block with a certain name will appear as `<<name>>=`, while consecutive code blocks with the same name will appear as `<<name>>+=`.

Please see the [Hello World](hello-world.html) and other examples!

## Syntax (source side)

In the source code we know exactly where the code came from, so there would be no strict need for extra syntax there. However, once we start to edit the source file it may not be clear where the extra code needs to end up. To make our life a little easier, named code blocks that were tangled into the file are marked with a comment at begin and end.

```cpp
// ----- begin <<main-body>>[0]
std::cout << "Hello, World!" << std::endl;
// ----- end
```

These comments should not be tampered with!

## Running `entangled`

Assuming you have created a Markdown file, say `program.md`, you can start `entangled` by running

```
entangled ./program.md
```

in the shell. You may run `entangled --help` to get help on options, but currently there are none.

## Running `tangle`

The `tangle` script depends on a recent version of Pandoc being installed, as well as `rsync`. 

## Running `weave`

Next to Pandoc, `weave` requires the `pandoc-fignos`, `pandoc-eqnos` and `pandoc-citeproc` filters to be installed. `pandoc-citeproc` is distributed with Pandoc, however in some GNU/Linux it resides in a separate package. For `pandoc-fignos` and `pandoc-eqnos` make sure you have Python and `pip` installed, and run

```
pip install --user pandoc-fignos pandoc-eqnos
```

If you want to create PDF output, you'll also need a LaTeX distribution installed.

## Distribution

If you've written a literate code using enTangleD and would like to distribute it, one way is to include the tangled source code in the tar ball. The other way is to include the `scripts` folder in your repository. You may want to check out the `tangle` and `weave` scripts, include a different LaTeX theme or play with `style.css`.

# Development

## Generating manpage

```
pandoc lit/manpage.md -s -t man | /usr/bin/man -l -
```
