# Simple test

This tests basic one-shot behaviour of entangled.

``` {.scheme file=hello.scm}
<<hello-world>>
```

``` {.scheme #hello-world}
(display "Hello, World!") (newline)
```

## Factorials

``` {.scheme file=factorial.scm}
<<factorial>>

(display (factorial 10)) (newline)
```

``` {.scheme #factorial}
(define (factorial n)
  (let loop ((x 1)
             (n n))
    (if (zero? n)
      x
      (loop (* x n) (- n 1)))))
```

