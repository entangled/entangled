# Simple test with block comments

This tests basic one-shot behaviour of entangled.

``` {.c file=hello.c}
#include <stdio.h>
#include <stdlib.h>

int main() {
    <<hello-world>>
    return EXIT_SUCCESS;
}
```

``` {.c #hello-world}
printf("Hello, World!\n");
```

## Factorials

``` {.c file=factorial.c}
#include <stdio.h>
#include <stdlib.h>

<<factorial>>

int main() {
    printf("%u! = %u\n", 10, factorial(10));
    return EXIT_SUCCESS;
}
```

``` {.c #factorial}
unsigned factorial(unsigned n) {
    unsigned m = 1, i;
    for (i = 1; i <= n; ++i) {
        m *= i;
    }
    return m;
}
```

