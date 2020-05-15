# Editing one of multiple instances

``` {.scheme #a}
(display "a")
```

``` {.scheme file=a1.scm}
<<a>>
<<a>>
(display "1")
```

``` {.scheme file=a2.scm}
<<a>>
(display "2")
```

``` {.scheme file=b.scm}
(display "b")
```

