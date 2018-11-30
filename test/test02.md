---
title: test2
---

We present our first program:

``` {.cpp file=hello.cc}
#include <iostream>
#include <cstdlib>

int main() {
    <<main-body>>
}
```

Will print `Hello, World!` on the console.

``` {.cpp #main-body}
std::cout << \"Hello, World!\" << std::endl;
return EXIT_SUCCESS;
```
