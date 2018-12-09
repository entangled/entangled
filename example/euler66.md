---
title: Solving project Euler 66
---

In this document we will be solving [problem 66 on Project Euler](https://projecteuler.net/problem=66). The problem is statet as follows:

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
>  32 – 2 \times 22 &= 1\\
>  22 – 3 \times 12 &= 1\\
>  92 – 5 \times 42 &= 1\\
>  52 – 6 \times 22 &= 1\\
>  82 – 7 \times 32 &= 1
> \end{aligned}$$
>
> Hence, by considering minimal solutions in $x$ for $D \le 7$, the largest x is obtained when $D=5$.
>
> Find the value of $D \le 1000$ in minimal solutions of $x$ for which the largest value of $x$ is obtained.

Not knowing anything about solving the Diophantine equation, Wikipedia is the first goto. There we may read that this specific instance of the Diophantine equation is also known as *Pell's equation*, studied by both Bramagupta in the 7th century and by Fermat in the 17th century. It says:

> Let $h_{i}/k_{i}$ denote the sequence of convergents to the regular continued fraction for $\sqrt{n}$. This sequence is unique. Then the pair $(x_1, y_1)$ solving Pell's equation and minimizing $x$ satisfies $x_1 = h_i$ and $y_1 = k_i$ for some $i$. This pair is called the fundamental solution. Thus, the fundamental solution may be found by performing the continued fraction expansion and testing each successive convergent until a solution to Pell's equation is found. 
