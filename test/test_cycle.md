---
title: Cyclic reference
---

Tangling this file should fail with a `CyclicReference` error.

``` {.cpp #test}
std::cout << "When time becomes a loop ...\n";
<<test>>
```

``` {.cpp file=loop.cc}
#include <iostream>
#include <cstdlib>

int main() {
    <<test>>
    return EXIT_SUCCESS;
}
```
