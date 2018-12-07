# enTangleD: a bi-directional literate programming tool

Literate programming is awesome! Write your documentation and code in one markdown document, tangle the source code from that document, compile and run your code. But ow what happens? Compiler error? Bug? Where? Your debugger is no longer pointing to your real source file! No worries, just edit the source file first, fix the bug and then copy the edits to your master document. Meh.

Enter enTangleD! This monitors the tangled source files and reflects any change in master document or source files in one live source database. The markdown file is still the master document.

## Syntax (markdown side)

The markdown syntax `enTangleD` uses is compatible with `Pandoc`'s.
This relies on the use of *fenced code attributes*. To tangle a code block to a file:

~~~markdown
``` {.bash file=src/count.sh}
   ...
```
~~~

Composing a file using multiple code blocks is done through *noweb* syntax. You can reference a named code block in another code block by putting something like `<<named-code-block>>` on a single line. This reference may be indented. Such an indentation is then prefixed to each line in the final result.

A named code block is should have an identifier given:

~~~markdown
``` {.python #named-code-block}
   ...
```
~~~

## Syntax (source side)

In the source code we know exactly where the code came from, so there would be no strict need for extra syntax there. However, once we start to edit the source file it may not be clear where the extra code needs to end up. To make our life a little easier, named code blocks that were tangled into the file are marked with a comment at begin and end.

```cpp
// _____ begin <<main-body>>[0]
std::cout << "Hello, World!" << std::endl;
// _____ end
```

These comments should not be tampered with!

## Configuration

> Not yet implemented: The project should contain a `tangle.yaml` file.
