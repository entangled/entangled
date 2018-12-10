# Solving project Euler 66

In this document we will be solving [problem 66 on Project Euler](https://projecteuler.net/problem=66), in the Scheme programming language. The problem is stated as follows:

> Consider quadratic Diophantine equations of the form:
>
> $$x^2 - Dy^2 = 1.$$
>
> For example, when $D=13$, the minimal solution in $x$ is $6492 – 13 \times 1802 = 1$.
>
> It can be assumed that there are no solutions in positive integers when $D$ is square.
> By finding minimal solutions in $x$ for $D = {2, 3, 5, 6, 7}$, we obtain the following:
>
> $$\begin{aligned}
> 3^2 – 2 \times 2^2 &= 1\\
> 2^2 – 3 \times 1^2 &= 1\\
> 9^2 – 5 \times 4^2 &= 1\\
> 5^2 – 6 \times 2^2 &= 1\\
> 8^2 – 7 \times 3^2 &= 1
> \end{aligned}$$
>
> Hence, by considering minimal solutions in $x$ for $D \le 7$, the largest x is obtained when $D=5$.
>
> Find the value of $D \le 1000$ in minimal solutions of $x$ for which the largest value of $x$ is obtained.

Not knowing anything about solving the Diophantine equation, Wikipedia is the first goto. There we may read that this specific instance of the Diophantine equation is also known as *Pell's equation*, studied by both Bramagupta in the 7th century and by Fermat in the 17th century. It says:

> Let $h_{i}/k_{i}$ denote the sequence of convergents to the regular continued fraction for $\sqrt{n}$. This sequence is unique. Then the pair $(x_1, y_1)$ solving Pell's equation and minimizing $x$ satisfies $x_1 = h_i$ and $y_1 = k_i$ for some $i$. This pair is called the fundamental solution. Thus, the fundamental solution may be found by performing the continued fraction expansion and testing each successive convergent until a solution to Pell's equation is found.

The problem is therefore (in part) reduced to generating continued fractions of the square roots.

## Computing continued fractions

``` {.scm #continued-fraction-sqrt}
(define (continued-fraction-sqrt n)
  (let ((a0 (integer-sqrt n)))
    (let loop ((m 0)
               (d 1)
               (a (integer-sqrt n)))
      (let* ((ms (- (* d a) m))
             (ds (/ (- n (* ms ms)) d))
             (as (quotient (+ a0 ms) ds)))
        (stream a (loop ms ds as))))))
```

``` {.scm #convergents}
(define (convergents s)
  (let ((a0 (head s))
        (a1 (head (tail s))))
    (let loop ((s  (tail (tail s)))
               (h0 a0)
               (h1 (+ 1 (* a0 a1)))
               (k0 1)
               (k1 a1))
      (let* ((an (head s))
             (hn (+ (* an h1) h0))
             (kn (+ (* an k1) k0)))
        (stream (/ h0 k0)
          (loop (tail s) h1 hn k1 kn))))))
```

We need to test the resulting numbers for Pell's equation:

``` {.scm #pell-equation}
(define (pell-equation D)
  (lambda (a)
    (let ((x (numerator a))
          (y (denominator a)))
      (= 1 (- (* x x) (* D y y))))))
```

Then we should be able to find the smallest solution for any $D$.

## Integer square root

To get the square root of $s$, we use Newton's method. We first implement a function to find integer fixed points. The algorithm may end up oscillating between two values. In that case the *fixed-point* function returns the smaller of the two.

``` {.scm #integer-sqrt}
(define (fixed-point f start)
  (let loop ((a0 (f start))
             (as (list start start)))
    (pmatch as
      ((,a1 ,a2 . ,rest)
       (cond
         ((eq? a0 a1) a0)
         ((eq? a0 a2) (min a0 a1))
         (else (loop (f a0) (cons a0 as))))))))

(define (integer-sqrt x)
  (if (<= x 1)
    x
    (fixed-point 
      (lambda (n)
        (div (+ n (div x n)) 2))
      (div x 2))))
```

## Synthesis

``` {.scm file=euler66.scm}
(import (rnrs (6))
        (streams)
        (pmatch))

<<integer-sqrt>>
<<continued-fraction-sqrt>>
<<convergents>>
<<pell-equation>>

(define (perfect-square n)
  (let ((m (integer-sqrt n)))
    (= (* m m) n)))

(define (range a b)
  (do ((n (- b 1) (- n 1))
       (r '() (cons n r)))
      ((= n a) (cons n r))))

(define (list-max < xs)
  (fold-left (lambda (a b) (if (< a b) b a))
             (car xs) (cdr xs)))

(define (list-head x n)
  (let loop ((r '())
             (n n)
             (x x))
    (if (zero? n)
      (reverse r)
      (loop (cons (car x) r) (- n 1) (cdr x)))))

(let* ((non-squares (filter (lambda (x)
                              (not (perfect-square x)))
                            (range 2 1001)))
       (pell-pairs  (map (lambda (x)
                           (cons x (stream-find
                                     (pell-equation x)
                                     (convergents (continued-fraction-sqrt x)))))
                         non-squares)))
  (display "first five solutions:\n")
  (for-each (lambda (p) (display p) (newline))
            (list-head pell-pairs 5))
  (newline)
  (display "maximum numerator at:\n")
  (display (list-max (lambda (a b) (< (numerator (cdr a)) (numerator (cdr b))))
                     pell-pairs))
  (newline))
```

## Appendix A: `streams`

``` {.scm file=streams.scm}
(library (streams)
  (export stream head tail take stream-find)
  (import (rnrs (6))
          (rnrs r5rs))

  (define-syntax stream
    (syntax-rules ()
      ((stream <a> <b>)
       (cons <a> (delay <b>)))))

  (define head car)
  (define (tail s) (force (cdr s)))

  (define (take s n)
    (let loop ((result '())
               (s s)
               (n n))
      (if (zero? n)
        (reverse result)
        (loop (cons (head s) result) (tail s) (- n 1)))))

  (define (stream-find pred s)
    (if (pred (head s))
      (head s)
      (stream-find pred (tail s))))
)
```

## Appendix B: `pmatch`

> This is a new version of `pmatch` (August 8, 2012).
> It has two important new features:
>
> 1. It allows for a name to be given to the pmatch if an error ensues.
> 2. A line from the specification has been removed. (see below).  Without that line removed, it was impossible for a pattern to be `(quote ,x)`, which might be worth having especially when we write an interpreter for Scheme, which includes quote as a language form.
>
> Code written by Oleg Kiselyov (http://pobox.com/~oleg/ftp/)
>
> Taken from leanTAP.scm
>
> http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log
>
> A simple linear pattern matcher. It is efficient (generates code at macro-expansion time) and simple: it should work on any R5RS (and R6RS) Scheme system.
>
>     (pmatch exp <clause> ...[<else-clause>])
>     <clause> ::= (<pattern> <guard> exp ...)
>     <else-clause> ::= (else exp ...)
>     <guard> ::= boolean exp | ()
>     <pattern> :: =
>            ,var  -- matches always and binds the var
>                     pattern must be linear! No check is done
>             _    -- matches always
>            exp   -- comparison with exp (using equal?)
>            (<pattern1> <pattern2> ...) -- matches the list of patterns
>            (<pattern1> . <pattern2>)  -- ditto
>            ()    -- matches the empty list


``` {.scm file=pmatch.scm}
(library (pmatch)
  (export pmatch)
  (import (rnrs (6)))

  (define-syntax pmatch
    (syntax-rules (else guard)
      ((_ v (e ...) ...)
       (pmatch-aux #f v (e ...) ...))
      ((_ v name (e ...) ...)
       (pmatch-aux name v (e ...) ...))))

  (define-syntax pmatch-aux
    (syntax-rules (else guard)
      ((_ name (rator rand ...) cs ...)
       (let ((v (rator rand ...)))
         (pmatch-aux name v cs ...)))
      ((_ name v)
       (error 'pmatch "match failed"))
      ((_ name v (else e0 e ...)) (begin e0 e ...))
      ((_ name v (pat (guard g ...) e0 e ...) cs ...)
       (let ((fk (lambda () (pmatch-aux name v cs ...))))
         (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
      ((_ name v (pat e0 e ...) cs ...)
       (let ((fk (lambda () (pmatch-aux name v cs ...))))
         (ppat v pat (begin e0 e ...) (fk))))))

  (define-syntax ppat
    (syntax-rules (? comma unquote)
      ((_ v ? kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v (unquote var) kt kf) (let ((var v)) kt))
      ((_ v (x . y) kt kf)
       (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
             (ppat vx x (ppat vy y kt kf) kf))
           kf))
      ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
)
```
