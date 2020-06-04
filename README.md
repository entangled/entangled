---
title: Entangled
author: Johan Hidding
---

> **Literate programming** [/ˈlɪtəɹət ˈpɹəʊɡɹæmɪŋ/]{.phonetic} (computing) Literate programming is a programming paradigm introduced by Donald Knuth in which a program is given as an explanation of the program logic in a natural language, such as English, interspersed with snippets of macros and traditional source code, from which a compilable source code can be generated. [(Wikipedia)](https://en.wikipedia.org/wiki/Literate_programming)

In short: you write Markdown containing code fragments. These code fragments are combined into working code in a process called **tangling**.

`Entangled` makes writing literate programs easier by keeping code blocks in markdown up-to-date with generated source files. By monitoring the tangled source files, any change in the master document or source files is reflected in the other. In practice this means:

*    Write well documented code using Markdown.
*    Use any programming language you like (or are forced to use).
*    Keep debugging and using other IDE features without change.
*    Generate a report in PDF or HTML from the same source (see examples at [Entangled homepage](https://entangled.github.io/)).

# Status

`Entangled` is approaching 1.0 release! It has been tested Linux, Windows and MacOS. Still, it is highly recommended to use version control and *commit often*. If you encounter unexpected behaviour, please post an issue and describe the steps to reproduce.

Features:

- live bi-directional updates
- (reasonably) robust against wrongly edited source files
- configurable with [Dhall](https://dhall-lang.org/)
- hackable through SQLite
- create PDF or HTML pages from literate source
- line directives to point compilers to markdown source

# Building

`Entangled` is written in [Haskell](https://www.haskell.org/), and uses the `cabal` build system. You can build an executable by running

    cabal build

Install the executable in your `~/.local/bin`

    cabal install

Run unit tests

    cabal test

# Using

`Entangled` should be run from the command-line. The idea is that you run it from the root folder of the project that you're working on. This folder should contain a `entangled.dhall` file that contains the configuration. You can get an example config file by running

    entangled config

This config asumes you have the markdown files in a folder named `./lit`, and stores information in a SQLite3 database located at `./.entangled/db`. To run the daemon,

    entangled daemon [files ...]

where the `[files ...]` bits is sequence of additional files that you want monitored.

# Syntax (markdown side)

The markdown syntax `Entangled` uses is compatible with `Pandoc`'s. This relies on the use of *fenced code attributes*. To tangle a code block to a file:

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

Please see the [Hello World](https://entangled.github.io/examples/hello-world.html) and [other examples](https://entangled.github.io/examples)!

## Syntax (source side)

In the source code we know exactly where the code came from, so there would be no strict need for extra syntax there. However, once we start to edit the source file it may not be clear where the extra code needs to end up. To make our life a little easier, named code blocks that were tangled into the file are marked with a comment at begin and end.

```cpp
// ~|~ begin <<lit/story.md|main-body>>[0]
std::cout << "Hello, World!" << std::endl;
// ~|~ end
```

These comments should not be tampered with!

## Running `entangled`

Assuming you have created a Markdown file, say `program.md`, you can start `entangled` by running

```
entangled daemon ./program.md
```

in the shell. You may run `entangled --help` to get help on options, or check out [the user manual](https://entangled.github.io/manual.html).

## Distribution

If you've written a literate code using Entangled and would like to distribute it, one way is to include the tangled source code in the tar ball. You may also wish to use the pandoc filters included in [`entangled/filters`](https://github.com/entangled/filters).

# Development

## Credits

The following persons have made contributions to Entangled:

- Michał J. Gajda (gh:mgajda), first implemented the line-directive feature
- Danny Wilson (gh:vizanto), first implemented the project annotation

## Generating manpage

```
pandoc lit/a2-manpage.md -s -t man | /usr/bin/man -l -
```

## License

Entangled is distributed under the Apache v2 license.

