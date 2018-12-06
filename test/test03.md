---
title: test3
---

We present our first program:

``` {.cpp #imports}
#include <iostream>
#include <cstdlib>
```

``` {.cpp file=hello.cc}
<<imports>>

int main() {
    <<main-body>>
}
```

Will print `Hello, World!` on the console.

``` {.cpp #main-body}
std::cout << "Hello, World!" << std::endl;
```

And make sure we exit properly:

``` {.cpp #main-body}
return EXIT_SUCCESS;
```
