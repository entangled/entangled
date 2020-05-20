Test if files and directories get removed when they are orphaned.

``` {.scheme file=sits/in/nested/directories.scm}
(let* ((yin
         ((lambda (cc) (display #\@) cc) (call/cc (lambda (c) c))))
       (yang
         ((lambda (cc) (display #\*) cc) (call/cc (lambda (c) c)))))
    (yin yang))
```

``` {.scheme file=sits/unchanged.scm}
(define Y
  (lambda (h)
    ((lambda (x) (h (lambda (a) ((x x) a))))
     (lambda (x) (h (lambda (a) ((x x) a)))))))
```

