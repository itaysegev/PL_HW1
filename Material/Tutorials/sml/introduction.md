# Standard ML

## introduction

---

### What's SML?

born 1985. named Standard ML = Meta Language...

* the 1970s, Robin Milner and group working at Edinburgh University on "LCF" (a  theorem proover)
* ML invented as an embedded scripting language of LCF
* many ad-hoc independent implementations, many new ideas
* 1997: first real standard

<!--vert-->

### Why SML?

* exemplar of functional programming
  * functional = forget about variables
  * functional = functions are values
  * functional = higher lever functions
  * ...
* exemplar of **type safe** functional programming
  * strongly typed: no type error can go undetected
  * statically typed: all type errors are detected at compile time
* influenced: Haskell, OCaml, Scala, F#, ...

<!--vert-->

### More on SML

main applications:

* research
* teaching
* few industrial applications

<!--vert-->

### Language Spirit

Robin Milner on ML:

> "ML is a general purpose programming language. First, it has **an escape and escape trapping mechanism**, well-adapted to programming strategies which may be inapplicable to certain goals. Second, it has a **polymorphic type discipline** which combines the flexibility of programming in a typeless language with the security of compile-time type checking."

<!--vert-->

two main features emphasized by the language creator

* exception mechanism for disciplined management of errors
* type system
  * flexibility of typeless
  * safety of typed

---

### Running

mode of operation

* REPL (sort of; it is not interpreted)
* first prompt (-) and secondary prompt (=)
* semicolon terminated

```text
Standard ML of New Jersey v110.79 [built: Sat Oct 26 12:27:04 2019]
- 5 + 5;
val it = 10 : int
```

<!--vert-->

* ML is usually used in a REPL
* expressions followed by a semicolon yield a response

```sml
2 + 2;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### Naming Values

<!--vert-->

naming values

```sml
val seconds = 60;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

using named values in expressions

```sml
val minutes = 60;

val hours = 24;

seconds * minutes * hours;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

the identifier `it`

```sml
seconds * minutes * hours;

it div 24;

val secs_in_hour = it;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### Identifiers in SML

two kinds of identifiers:

* alphabetical identifiers, as found in most languages
* special identifiers, mainly for defining new operators

<!--vert-->

use of identifiers:

* names of types
* names of values
  * ordinary values
  * functions

<!--vert-->

identifier congestion?

* scoping
* hiding
* no overloading!

<!--vert-->

### Alphabetical Identifiers

* begin with a letter
* followed by a sequence of letters, digits, `_`, or `'`
* case sensitive
* some alphabetical identifiers are reserved words
  * names of operators: `and`, `if`, `then`, `else`, `orelse`, ...
  * punctuations: `fun`, `let`, `local`, `of`, ... 
  * many more

```sml
x
x'
uB40
hamlet_prince_of_denmark
h''3_H
```

<!--vert-->

### Special Identifiers

are called "symbolic" identifiers in the SML lingo

* a sequence of one or more of the following ``! % & $ # + - * / : < = > ? @ \ ~ \ ^ | ` ``
* should not be one of: `: | = => -> #`
* allowed wherever an alphabetic name is

    ```sml
    val +-+-+ = 1415;
    val <=> = "asdf";
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### ML keywords

`abstype` | `and` | `andalso` | `as` | `case` | `datatype` | `do` | `else` | `end` | `eqtype` | `exception` | `fn` | `fun` | `functor` | `handle` | `if` | `in` | `include` | `infix` | `infixr` | `let` | `local` | `nonfix` | `of` | `op` | `open` | `orelse` | `raise` | `rec` | `sharing` | `sig` | `signature` | `struct` | `structure` | `then` | `type` | `val` | `while` | `with` | `withtype`

keywords are words that look like identifiers, but cannot be used as such; they are reserved for other purposes.

---

### The Basic Types of ML

six "basic" types: `int`, `real`, `string`, `char`, `bool`, `unit`

* they are basic, since they are *atomic*
* they are basic, since they are *builtin*
* there are other, user defined types:
  * user defined types can be atomic
  * user defined types can be compound
  * a programming language may furnish the user with some builtin, non-atomic types (not in SML)

<!--vert-->

`int`, `real`, `string`, `char`, `bool`, `unit`

* are valid identifiers
* are not reserved identifiers
* are not keywords
* are predefined identifiers, naming the builtin types
* the identifiers may be used for other purposes; the underlying type may remain nameless

```sml
val int = 6;
val real = 7;
real * int - real;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### int

#### literals

* sequence of digits
  * 0
  * 01234
* `~` for a unary minus sign
  * `~23`
  * `~85601435654638`

<!--vert-->

infix operations: `+` `-` `*` `div` `mod`

conventional precedence (parenthesis can be dropped without change of meaning)

```sml
(((m * n) * l) - (m div j)) + j
```

---

### real

#### literal

* decimal point
  * `0.01`
  * `2.718281828`
* e notation
  * `7E~5`
  * `~1.2E12`
  * `~123.4E~2` is the same as `~1.234`

<!--vert-->

infix operators: `+` `-` `*` `/`

NOTE: note that `+`, `-`, `*` are overloaded

<!--vert-->

#### functions

* `floor` converts `real` to `int`
* `real` converts `int` to `real`
* `sqrt`, `sin`, `cos`, `tan`, `exp`, `ln` all of type `real->real`
* all need the `Math.` prefix: `Math.sqrt`

---

### strings

<!--vert-->

string literals are written in double quotes

```sml
"ML is the best";
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

escape sequences: `\n`, `\t`, `\"`, `\\`

<!--vert-->

concatenation operator

```sml
"Standard" ^ " ML";
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

comparison of strings

```sml
"abc" < "cba";

"zzz" > "aaa";
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

`size` returns the number of characters

```sml
size "SML";
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

notice the syntax of function application - no parentheses!

---

### Characters

<!--vert-->

characters (values of type `char`) are distinguished from strings of length 1 by the character `#`

```sml
"0";

#"0";
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

conversion between strings and chars

```sml
str #"0";

String.sub("hello", 0);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

conversion between chars and ASCII

```sml
ord #"0";

chr it;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### boolean

two named values: `true` and `false`

```sml
true;

false;

2 + 2 = 4;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

he names `true` and `false` are not keywords; they are predefined identifiers 

---

### tuples - cartesian product type

* the n-tuple whose components are `x1`, ..., `xn`:

    ```sml
    (x1, x2, ..., xn)
    ```

* the components can be of any type, including tuples

```sml
val a = (1.5, 6.8);

(1, 1.5);

("str",1,true,(#"0",0.1));
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### records

* records have components (fields) identified by name

    ```sml
    val me = { name="Ofir", age=30 };
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* type lists each field as `label : type`
* enclosed in braces `{...}`

<!--vert-->

* selecting a field

    ```sml
    #name me;
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* tuples **can be seen as** records with numbers as implicit field labels

    ```sml
    #2 ("one", "two", "three");
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* note that the numbering starts with 1

---

### lists

a list is a finite sequence of elements

```sml
[3, 5, 9];
["a", "list"];
[];
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

elements may appear more than once

```sml
[3,4,3];
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

elements may have any type but all elements must have the same type

```sml
[(1, "one"), (2, "two")] : (int*string) list;
[[3.1], [], [5.7, ~0.6]] : (real list) list;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### mapping - functions

```sml
fun sq(x: int) = x*x;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* keyword `fun` starts the function declaration
* `sq` is the function name
* `x:int` is the formal parameter with type constraint
* `x*x` is the body and it is an **expression**

<!--vert-->

* the result of the function is the result of evaluating the **expression** of the function body with the actual parameter
* `int->int` is the standard mathematical notation for a function type that takes an integer and returns an integer

---

### applying a function

* simple function call

    ```sml
    (sq 3);
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* when a function is called the argument is evaluated and then passed to the function

    ```sml
    sq (sq 3);
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* the parentheses are optional

    ```sml
    sq 3;
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

* parentheses are also optional in function definitions

    ```sml
    fun sq x:int = x*x;
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### arguments and results

* every function has one argument and one result
* any type can be passed/returned!!!
* tuples are used to pass/return several values

```sml
val a = (3.0, 4.0);

fun lengthvec (x:real, y:real) = Math.sqrt(x*x + y*y);

lengthvec a;

lengthvec (5.0, 12.0);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### functions as values

anonymous functions with `fn` notation

```sml
fn x:int => x*x;

it 3;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

the following declarations are identical

```sml
fun sq x:int = x*x;
val sq = fn x:int => x*x;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### returning functions

* functions can also be __returned__ from other functions

    ```sml
    fun inttwice(f: (int->int)) =
        fn x => f (f x);
    ```
    <!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* the arrow is right associative so the type of `inttwice` is equivalent to:

    ```sml
    val inttwice = fn : (int -> int) -> (int -> int)
    ```

<!--vert-->

example

```sml
inttwice (fn x => x*x);

it 3;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### type inference

ML deduces the types in expressions

```sml
fun facti (n, p) =
    if n=0 then p else facti (n-1, n*p);
(*val facti = fn : int * int -> int*)
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

* literals `0` and `1` have type `int`.</span>
* therefore `n=0` and `n-1` involve integers so `n` has type `int`</span>
* `n*p` must be integer multiplication, so `p` has type `int`</span>
* `facti` returns type `int`</span>

---

### type constraints

* certain functions are overloaded, e.g. `abs`, `+`, `-`, `~`, `*`, `<`
* the type of an overloaded function is determined from context, or is set to `int` by default
* types can be stated explicitly

---

what will be printed for the following definitions of `min`?

<!--vert-->

```sml
fun min(x, y) = if x < y then x else y;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

```sml
fun min(x:real,y) = if x < y then x else y;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

```sml
fun min(x:string,y) = if x < y then x else y;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

```sml
fun min(x,y):real = if x < y then x else y;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

```sml
fun min(x,y) = if x < y then x:real else y;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

write a function `foo` such that its type is:

```sml
val foo = fn : int * real -> real
```

but you can't use type annotations

```sml
...
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### polymorphic type checking

|                           |        | flexibility | security |
|:-------------------------:|:------:|:-----------:|:--------:|
|        weakly typed       |  lisp  |      ✔      |          |
|       strongly typed*     | Pascal |             |     ✔    |
| polymorphic type checking |   ML   |      ✔      |     ✔    |

and in ML most types are inferred automatically 😎

NOTE: Pascal has type punning so its not as strongly typed as ML

---

### polymorphic function definitions

* if type inference leaves some types completely unconstrained then the definition is polymorphic
* a polymorphic type contains a type variable, e.g. 'a, 'b

```sml
fun pairself x = (x, x);

pairself 4.0;

fun pair (x,y) = (y, x);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### functions as values - the polymorphic case

<!--vert-->

what will be printed?

```sml
fun twice f = fn x => f (f x);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

what will be printed?

```sml
fun ident x = x;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

what will be printed?

```sml
fun twice f = fn x => f (f x);
twice (fn x => x * x);
it(2);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

<!--vert-->

sometimes ML gives us a hard time

```sml
fun twice f = fn x => f (f x);
fun ident x = x;
twice ident;
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

you usually may ignore it. or use a workaround:

```sml
fn x => (twice ident) (x);
```
<!-- .element: data-thebe-executable-sml data-language="text/x-ocaml" -->

---

### functional vs. imperative

* imperative - using commands to change the state
* functional - stateless. using expressions recursively to calculate the result
* example: Euclid's algorithm for the Greatest Common Divisor (GCD) of two natural numbers:

$$gcd(m,n) = \begin{cases}n,m = 0&\\gcd(n mod m,m), m>0\end{cases}$$

<!--vert-->

### GCD - Python vs. ML

an imperative Python program:

```python
def gcd(m: int, n: int) -> int:
    while m != 0:
        n, m = m, n % m
    return n
```

a functional program in Standard ML:

```sml
fun gcd(m,n) = if m=0 then n else gcd(n mod m, m);
```

which one is more efficient? 🧐
