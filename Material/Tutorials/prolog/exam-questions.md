# prolog

## exam questions

---

### question 1

<!--vert-->

#### question 1.1

write rules for the predicate `interleave/2` such that `interleave(L, R)` is true iff `L` is a list of lists and `R` is the lists in `L` interleaved.

* you may assume that `L` is a concrete list of lists
* example: `interleave([[p,l],[r,o],[o,g]], [p,r,o,l,o,g])`

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
interleave([], []).
interleave([[]|X], R) :- interleave(X, R).
interleave([[X|Xs]|Ys], [X|R]) :-
    append(Ys, [Xs], Z),
    interleave(Z, R).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### question 1.2

the predicates `cond1/1` and `cond2/1` take a concrete integer. implement `filter/2` such that `filter(Xs, Ys)` is true iff `Xs` is a list of integers and `Ys` is a list of all integers from `Xs` for which `cond1` is true and `cond2` is false.

* you may assume `Xs` is a concrete list of integers

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
filter([], []).
filter([X|Xs], [X|R]) :-
    cond1(X), \+ cond2(X), filter(Xs, R), !.
filter([X|Xs], R) :- filter(Xs, R).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### question 2

<!--vert-->

#### question 2.1

implement `dec_bin/2` such that `dec_bin(D, B)` is true iff `B` is the binary representation of `D`. (`D` is concrete)

example:

```prolog
?- dec_bin(12, K).
K = '1100'.
```

(you may use `atom_concat/3` - `atom_concat(A, B, R)` holds iff `A`, `B` and `R` are atoms and `R = concat(A, B)`)

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
dec_bin(0, '0') :- !.
dec_bin(1, '1') :- !.
dec_bin(N, B) :-
    X is N mod 2, Y is N // 2,
    dec_bin(Y, H),
    atom_concat(H, X, B)
.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### question 2.2

implement `bin_dec/2`.

example:

```prolog
?- bin_dec('1100', K).
K = 12.
```

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
bin_dec('0', 0) :- !.
bin_dec('1', 1) :- !.
bin_dec(B, N) :- atom_concat(A, '1', B), !, bin_dec(A, N1), N is 2 * N1 + 1.
bin_dec(B, N) :- atom_concat(A, '0', B), !, bin_dec(A, N1), N is 2 * N1.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### question 3

implement `mergeSort/2`.

```prolog
mergeSort([56, 29, 35, 42, 15, 41, 75, 21], R).
R = [15, 21, 29, 35, 41, 42, 56, 75].
```

<!--vert-->

let's implement `merge/2` first:

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
merge(L, [], L).
merge([], L, L).
merge([X|Xs], [Y|Ys], [X|R]) :- X =< Y, merge(Xs, [Y|Ys], R), !.
merge(Xs, Ys, R) :- merge(Ys, Xs, R).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
mergeSort([], []) :- !.
mergeSort([A], [A]) :- !.
mergeSort(L, R) :-
    length(L, N),
    NL is N // 2, NR is N - NL,
    length(LL, NL), length(LR, NR),
    append(LL, LR, L),
    mergeSort(LL, LLS),
    mergeSort(LR, LRS),
    merge(LLS, LRS, R).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### question 4

the predicate `edge/3` defines a weighted graph.

`edge(X,Y,N)` is true iff there's an edge from `X` to `Y` with weight `N`.

define `path/4`. `path(X,Y,P,N)` is true iff `P` is a path from `X` to `Y` with weight `N`.

```prolog
edge(a, b, 2)
?- path(a, a, [a], 0).
true.
?- path(a, b, P, N).
P = [a, b], N = 2.
```

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
path(X, X, [X], 0).
path(X, Y, P, N) :-
    X \= Y,
    path(X, Z, PX, NX),
    path(Z, Y, PY, NY),
    append(PX, PY, P),
    N is NX + NY.
.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

define `cycle/1`. `cycle(X)` is true iff there's a cycle that goes through `X`.

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
cycle(X) :- path(X, X, P, _), length(P, N), N > 1.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### question 5

implement `is_prime/1`.

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
is_prime(2).
is_prime(P) :-
    P > 2, \+ divisible(P,2).  

divisible(N,L) :- N mod L =:= 0.
divisible(N,L) :-
    L * L < N, L2 is L + 1, divisible(N,L2).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.

<!--vert-->

implement `goldbach/3`.

* `goldbach(A,B,S)` is true iff `A+B=S`, `A` and `B` are prime numbers and `S` is an even number.

* either `S` is concrete or `A` and `B` are concrete

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
:- use_module(library(clpfd)).
goldbach(A,B,S) :-
    A #>= 2, A #< S, A + B #= S , A #=< B,
    label([A, B, S]),
    is_prime(A), is_prime(B).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

---

### question 6

how can a knight jump on an NxN chessboard in such a way that it visits every square exactly once?

<!--vert-->

```prolog
jump_dist(1,2).
jump_dist(2,1).
jump_dist(2,-1).
jump_dist(1,-2).
jump_dist(-1,-2).
jump_dist(-2,-1).
jump_dist(-2,1).
jump_dist(-1,2).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

define `jump(N,A/B,C/D)` such that there's a jump from `A/B` to `C/D` on an NxN board

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
jump(N,A/B,C/D) :- 
   jump_dist(X,Y), 
   C is A+X, C > 0, C =< N,
   D is B+Y, D > 0, D =< N.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

define `knights(N,Ks)` such that `Ks` is a valid knight's tour on an NxN chessboard

```prolog
?- knights(5, Ks).
Ks = [1/5, 3/4, 5/5, 4/3, 5/1, 3/2, 1/3, 2/5, 4/4, 5/2, 3/1, 1/2,
2/4, 4/5, 5/3, 4/1, 2/2, 1/4, 3/3, 2/1, 4/2, 5/4, 3/5, 2/3, 1/1].

?- knights(4, Ks).
false.
```

<!--vert-->

```prolog
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
knights(N,Knights) :-
    M is N*N-1, knights(N,M,[1/1],Knights).
knights(_,0,Knights,Knights).
knights(N,M,Visited,Knights) :-
   Visited = [X/Y|_],
   jump(N,X/Y,U/V),
   \+ member(U/V,Visited),
   M1 is M-1,
   knights(N,M1,[U/V|Visited],Knights).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->
