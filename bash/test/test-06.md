# This should give line directives

``` {.c++ file=test.cc}
<<includes>>
<<main>>
```

``` {.c++ #includes}
#include <iostream>
```

``` {.c++ #main}
int main() {
    <<hello>>
    return 0;
}
```

``` {.c++ #hello}
std::cout << "Hello, World!" << std::endl;
```

