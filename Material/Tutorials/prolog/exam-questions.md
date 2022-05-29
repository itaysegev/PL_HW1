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
