---
title: enTangleD
---

::: {#logobar}
[![Octocat](images/github.png) Find us on Github](https://github.com/jhidding/entangled)&nbsp;
[![Download](images/download.svg) Download enTangled](https://github.com/jhidding/enTangleD/releases)
:::

> **Literate programming** [/ˈlɪtəɹət ˈpɹəʊɡɹæmɪŋ/]{.phonetic} (computing) Literate programming is a programming paradigm introduced by Donald Knuth in which a program is given as an explanation of the program logic in a natural language, such as English, interspersed with snippets of macros and traditional source code, from which a compilable source code can be generated. [(Wikipedia)](https://en.wikipedia.org/wiki/Literate_programming)

Literate programming is awesome! Write your documentation and code in one markdown document, tangle the source code from that document, compile and run your code. This is all fine if your code does not contain any bugs. But what if it does? Your compiler and debugger may point to locations in the source where things go wrong. But you cannot edit those sources directly, since you generated them from the mother document!

Enter enTangleD! This monitors the tangled source files and reflects any change in master document or source files to the other.

The markdown file is still the master document.

# enTangleD in 4 screencasts

::: {#slide-show}
:::: {.slide .fade}
![Write your documentation, and fire up enTangleD](screencasts/01-getting-started.png){ still="screencasts/01-getting-started.png" width="768" .anim }
::::

:::: {.slide .fade}
![Add annotated code blocks](screencasts/02-adding-code.png){ still="screencasts/02-adding-code.png" width="768" .anim }
::::

:::: {.slide .fade}
![Use references to grow your program](screencasts/03-edit-code.png){ still="screencasts/03-edit-code.png" width="768" .anim }
::::

:::: {.slide .fade}
![Fix bugs like you'd normally do](screencasts/04-fixing-bugs.png){ still="screencasts/04-fixing-bugs.png" width="768" .anim }
::::

<div style="text-align:center">
<a class="prev" onclick="plusSlides(-1)">&#10094;</a>
<a class="next" onclick="plusSlides(1)">&#10095;</a>
</div>
:::

<div style="text-align: center;">
  <span class="dot" onclick="currentSlide(1)"></span>
  <span class="dot" onclick="currentSlide(2)"></span>
  <span class="dot" onclick="currentSlide(3)"></span>
  <span class="dot" onclick="currentSlide(4)"></span>
</div>

<script src="jquery-3.3.1.slim.min.js"></script>
<script src="anim.js"></script>

# Status

`enTangleD` is working, but still in a alpha stage. It has been tested Linux, and Windows and should work on MacOS equally well. If you edit anything serious with the enTangle Daemon running, I strongly recommend using version control and commit often. If you encounter unexpected behaviour, please post an issue and describe the steps to reproduce.

Features:

- live bi-directional updates
- monitor multiple markdown files
- PanDoc filter and `Makefile` to generate report
- create HTML pages from literate source
  
Todo:

- configurability using Yaml file
- robustness against wrongly edited output files
- integration with git: commit every change, squash when done
- add workflow to create figures for HTML/PDF reports

::: {#examples-div}
# Literate Examples

:::: {.example}
[![Apollo Earthrise](images/hello-world-thumb.jpg) Hello World in C++](hello-world.html): Teaches the basics of literate programming using Markdown and fenced code blocks. Also shows how to use a BibTeX file for references.
::::

:::: {.example}
[![99 bottles thumbnail](images/99-bottles-thumb.jpg) 99-bottles in C++](99-bottles.html): Over-engineered song-text generator. Teaching how to setup a basic C++ program with enTangleD, use of ArgAgg to parse command-line arguments, use of FmtLib to do string formatting and setting up a slightly non-basic Makefile.
::::

:::: {.example}
[![Slasher thumbnail](images/slasher-thumb.jpg)](slasher.html)
[Slasher](elm-slasher.html): a browser game written in Elm. A dashing hero is zipping across the screen, only deflected by slashes and backslashes. The game works, but the source may need some more literacy in some places.
::::

:::: {.example}
[![Adhesion code thumbnail](images/adhesion-code-thumb.jpg) Adhesion code](https://jhidding.github.io/adhesion-code): presenting the cosmological adhesion model and its implementation in C++ and CGAL.
::::
:::

# Building

`enTangleD` is written in [Haskell](https://www.haskell.org/), and uses the `stack` build system. You can build an executable by running

    stack build

If this is the first time you run `stack`, this may take a while. Install the executable in your `~/.local/bin`

    stack install

Run unit tests

    stack test

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
