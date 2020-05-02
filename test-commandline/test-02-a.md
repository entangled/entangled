# Testing multiple outputs

``` {.scheme #a}
(display "a")
```

``` {.scheme #b}
(display "b")
<<a>>
```

## Case 1

``` {.scheme file=case1.scm}
<<a>>
<<b>>
```

## Case 2

``` {.scheme file=case2.scm}
<<a>>
<<a>>
```

