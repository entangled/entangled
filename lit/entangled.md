---
title: Entangled, literate programming Swiss army knife
author: Johan Hidding
---

Entangled makes writing literate programs easier by keeping code blocks in markdown up-to-date with generated source files. By monitoring the tangled source files, any change in the master document or source files is reflected in the other. In practice this means:

* Write well documented code using Markdown.
* Use any programming language you like (or are forced to use).
* Keep debugging and using other IDE features without change.
* Generate a report in PDF or HTML from the same source (see examples on the right).

# Preliminaries

## Modules

Several modules have standard stature but have to be imported qualified due to clashes in the namespace.

### Map

We use strict maps only.

``` {.haskell #import-map}
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
```

### Text

We will be using the `Text` module everywhere.

``` {.haskell #import-text}
import qualified Data.Text as T
import Data.Text (Text)
```

