# Simple test

This tests basic one-shot behaviour of entangled.

``` {.scheme file=hello.scm}
<<hello-world>>
```

``` {.scheme #hello-world}
(display "Hello, Universe!") (newline)
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

## Fibonacci

``` {.scheme file=fib.scm}
(define (fib x n)
  (if (zero? n)
    (reverse x)
    (fib (cons (+ (car x) (cadr x)) x) (- n 1))))
```

``` {.scheme #fib.scm}
(display (fib '(1 1) 10)) (newline)
```

