---
title: 99 Bottles of Beer
author: Johan Hidding
---

``` {.cpp #version}
#define VERSION "1.0"
```

This example shows how to create a basic C++ executable including some basic argument parsing. You may learn from this:

* Create a basic C++ program using enTangleD.
* Use [ArgAgg](https://github.com/vietjtnguyen/argagg) to parse command line arguments.
* Use [fmtlib](http://fmtlib.net/latest/index.html) to do easy (C#, Python, etc. -like) string formating in C++.
* Setting up a basic `Makefile`.
* An annoying drinking song.

This program will be so tiny, that it will fit into a single source file:

``` {.cpp file=src/99-bottles.cc}
<<includes>>
<<version>>

<<bottles-function>>
<<print-song-function>>
<<main-function>>
```

This source file includes some headers, a version number and three functions. The `bottles` function helps us create gramatically correct sentences with an arbitrary (though $\le 0$) number of bottles. `print_song` is the core function generating the 99-bottles song and `main` will handle command-line arguments.

First, we'll include some headers that we'll need:

``` {.cpp #includes}
#include <cstdlib>
#include <iostream>
#include <argagg/argagg.hpp>
#include <fmt/format.h>
```

The `cstdlib` file includes basic functions and constants from the C `stdlib.h` header. `iostream` contains the C++ library for IO. Next to that we include the *ArgAgg* and *fmtlib* headers.

# Argument parsing

An executable in C/C++ always has a main function of the signature

``` {.cpp #main-function}
int main(int argc, char **argv)
{
    <<main-body>>

    return EXIT_SUCCESS;
}
```

The arguments `argc` and `argv` give the number of arguments given on the command line and an array of null-terminated C-strings respectively. If we were writing C, we would use the `getopt` library to parse the given arguments. This C interface is tedious. It's 2019 and we're programming C++, so we use ArgAgg (say this out loud and twist your face in a painful contortion just to annoy who ever is sitting next to you)!

ArgAgg is a header-only library that simplifies argument parsing greatly. We may declare the possible command-line arguments and flags as follows:

``` {.cpp #declare-arguments}
argagg::parser argparser {{
    { "n", {"-n"},
        "(99) number of bottles to start with", 1 },
    { "help", {"-h", "--help"},
        "shows this help message", 0 },
    { "version", {"-v", "--version"},
        "shows the software version", 0 }
}};
```

It is considered good behaviour to always include the `"--help"` and `"--version"` arguments (see for instance: [tldp.org](https://www.tldp.org/LDP/abs/html/standard-options.html) and [the GNU Coding Standards](https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html)).
For good measure we also include an argument that gives the number of bottles that we'll start the song with.

Using the `argagg::parser` instance we can parse the command-line arguments stored in `argc` and `argv`. Since this operation may fail, we perform it inside a `try`, `catch` block:

``` {.cpp #parse-arguments}
argagg::parser_results args;
try {
    args = argparser.parse(argc, argv);
} catch (std::exception const &e) {
    std::cerr << e.what() << std::endl;
    return EXIT_FAILURE;
}
```

Now that we have parsed the arguments, we can implement the different ways the program will execute. If either the `"--help"` or `"--version"` flags was given, we print the requested information and exit the program.

``` {.cpp #print-help-or-version}
if (args["help"]) {
    std::cerr << "99 bottles -- " << VERSION << std::endl;
    std::cerr << "Usage: " << args.program << " [options]" << std::endl;
    std::cerr << argparser;
    return EXIT_SUCCESS;
}

if (args["version"]) {
    std::cerr << "99 bottles -- " << VERSION << std::endl;
    return EXIT_SUCCESS;
}
```

If none of those flags was given we continue with the main program. We extract the number of bottles that we want to start with and print the song:

``` {.cpp #sing-a-song}
unsigned n = args["n"].as<unsigned>(99);
print_song(n);
```

In one line we extract the argument `n`, parse it to `unsigned` and give a default value, should the argument not been given.

Now we can compose the body of the main function:

``` {.cpp #main-body}
<<declare-arguments>>
<<parse-arguments>>
<<print-help-or-version>>
<<sing-a-song>>
```

# Formatting a drinking song

Now, to the problem of *99 bottles*! One part of the problem is that towards the end of the song, the grammar of the text is changing slightly due to the singularity of the number *one*. We'll create a little helper function to figure out how to communicate the number of bottles we have left.

``` {.cpp #bottles-function}
std::string bottles(unsigned i)
{
    if (i > 1) {
        return fmt::format("{} bottles", i);
    }
    if (i == 1) {
        return "1 bottle";
    }
    return "no more bottles";
}
```

The `fmt::format` function formats a string using `"{}"` as a placeholder for whatever arguments follow. This function is much easier to use than the native C++ method of opening a `std::ostringstream`, using `iomanip` and so on. There is an overview of the format string syntax on [the fmtlib homepage](http://fmtlib.net/latest/syntax.html).

Now, we may write out the song:

``` {.cpp #print-song-function}
void print_song(unsigned n)
{
    unsigned i = n;
    while (i > 0)
    {
        fmt::print(
            "{0} of beer on the wall, "
            "{0} of beer.\n",
            bottles(i));

        fmt::print(
            "Take one down and pass it around, "
            "{} of beer on the wall.\n", 
            bottles(--i));
    }

    fmt::print(
        "No more bottles of beer on the wall, "
        "no more bottles of beer.\n"
        "Go to the store and buy some more, "
        "{} of beer on the wall.\n",
        bottles(n));
}
```

# Song synthesis

To compile the `99-bottles` program, you can use GCC directly.

```
mkdir build
g++ src/99-bottles.cc -lfmt -o build/99-bottles
```

But it is much better to provide a `Makefile`. We will create a `Makefile` that is generic enough to compile your program without too much modification, even as your source code expands.

We make sure all our build files end up in a single folder, preferably called `build` (hint: this folder can be added to your `.gitignore`).

``` {.makefile #set-build-dir}
build_dir = ./build
```

We search for all files with the `.cc` extension (warning: for bigger projects this will slow down compilation considerably). `make` does not easily support scanning for source files, but we may use a shell command.

``` {.makefile #search-cc-files}
cc_files = $(shell find ./src -name *.cc)
```

We can use string manipulation present in `make` to create the target object files and dependency files. The latter store the dependencies of the source files on header files, so that if a header file changes, the source file is recompiled.

``` {.makefile #set-obj-files}
obj_files = $(cc_files:%.cc=$(build_dir)/%.o)
dep_files = $(obj_file:%.o=%.d)
```

The `fmtlib` library requires linking using `-lfmt`, we next define the compiler and linker commands and their arguments.

``` {.makefile #compile-flags}
compile = g++
link = g++

fmtlib_lflags = -lfmt

compile_flags = -std=c++14 -Wall -O3
link_flags = $(fmtlib_lflags)
```

Now that we have defined all these variables we can setup the make targets. The first target given is also the default one, in this case `build`. Since both `build` and `clean` don't create any files themselves, these targets have to be declared as *phony*. This means `make` won't search for a file called `build` (which is our build directory!) to see if the `build` rule should be executed.

In this case `build` depends only on the target executable `99-bottles`, making sure that this target is getting built.

``` {.makefile #declare-target-rules}
.PHONY: clean build

build: $(build_dir)/99-bottles

clean:
	rm -rf $(build_dir)
```

Last we define the *pattern* rules. These tell `make` how to build a generic target adhering to some pattern from sources of a similar pattern. The first rule we give compiles a source file to an object file, the second rule links all objects files into an executable. We used several handy `make` smart variables here:

* `$(@D)` is the directory that contains the target.
* `$<` is the name of the first prerequisite, in this case the source file.
* `$^` is the list of all prerequisites, in this case the object files.
* `$@` is the name of the target file.

There are more of these listed in the [`make` documentation](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html).

Note also, that the `-MMD` flag to the compiler is actually telling GCC to create the aforementioned header dependency files. These files follow a `make` syntax generating a set of rules that is included using the `-include` statement.

``` {.makefile #declare-pattern-rules}
-include $(dep_files)

$(build_dir)/%.o: %.cc
	@mkdir -p $(@D)
	$(compile) $(compile_flags) -MMD -c $< -o $@

$(build_dir)/99-bottles: $(obj_files)
	@mkdir -p $(@D)
	$(link) $^ $(link_flags) -o $@
```

The complete `Makefile`:

``` {.makefile file=Makefile}
.RECIPEPREFIX +=

<<set-build-dir>>
<<search-cc-files>>
<<set-obj-files>>
<<compile-flags>>

<<declare-target-rules>>
<<declare-pattern-rules>>
```

Now, compiling is as simple as typing `make`. If compilation is successful we can run the program.

```
$ ./build/99-bottles -n 3
3 bottles of beer on the wall, 3 bottles of beer.
Take one down and pass it around, 2 bottles of beer on the wall.
2 bottles of beer on the wall, 2 bottles of beer.
Take one down and pass it around, 1 bottle of beer on the wall.
1 bottle of beer on the wall, 1 bottle of beer.
Take one down and pass it around, no more bottles of beer on the wall.
No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 3 bottles of beer on the wall.
```
