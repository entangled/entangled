# Tangling

The task of tangling means:

* Parse the markdown to a `Document`
* Generate annotated source files.

Parsing the markdown is done on a line per line basis. We don't try to parse the markdown itself, rather we try to detect lines that start and end code-blocks.

Remember the golden rule:

> Untangling from a generated source returns the same markdown **to the byte**.

Scanning a document, we trigger on each line matching:

``` {.haskell #code-header-regex}
"^``` *{(.*)} *$"
```

A code block is ended with a line:

``` {.haskell #code-footer-regex}
"^```` *$"
```


