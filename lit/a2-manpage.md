% ENTANGLED(1)
% Johan Hidding (j.hidding@esciencecenter.nl)
% 2019

# NAME

entangled - literate programming with markdown

# SYNOPSIS

`entangled` [*input files*] [*`--help`*] [*`--defaults`*] --- **Watch input files for changes**  
`entangled` *`insert`* [*input files*]                        --- **Insert input files into database**  

# DESCRIPTION

**entangled** is a tool to aid literate programming with Markdown. In literate programming, you do all programming in code blocks inside a natural language literary work. These code blocks are extracted to generate the source files that are suitable for the compiler, a process known as **tangling**. Using **entangled**, you can also do the inverse, patching changes to the generated files back to the markdown documents, also known as **stitching**. **entangled** runs as a daemon in the background, and is triggered on write events to any of the files involved.

Configuration of **entangled** is managed from a *`entangled.toml`* file, which is searched for in all parent folders and *`$HOME/.config/entangled`*.

# GENERAL OPTIONS

*`-h`*, *`--help`*
: Display a friendly help message.

*`--defaults`*
: Print an `entangled.toml` file with default settings.
