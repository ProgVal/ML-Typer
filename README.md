### Simple ML typer

This is a typer of a very simple purely functional ML-like language.
The language supports :
* Basic integer arithmetics
* Tuples
* Sum types
* Conditional expressions
* Unary functions
* let ... in form
* Pattern matching

The goal is to build an interface showing how a ML-like typer works,
starting with a very simple subset and adding more and more features,
more and more complex.

### Usage

The typer is written in OCaml, with OCamlLex and OcamlYacc for the
frontend. Just compile with make and run it:

```
make
./typer
```

You can enter any expression directly on stdin, and terminate with
double semi-colon or EOF (^D). The typer works on a single expression.

### Syntax

##### Integer arithmetics

```ocaml
a + b
a - b
a * b
a / b
```

##### Tuples

```ocaml
(a, b, c, d)
```

##### Sum types

```ocaml
Foo a
Bar (a, b, c)
```

##### Conditional expressions

```ocaml
ifz expr then expr_true else expr_false
```

##### Unary functions

```ocaml
fun pattern -> expression
```

##### let ... in

```ocaml
let pattern = expr in body
```

##### pattern matching

```ocaml
match expr with
| pattern1 -> expr1
| pattern2 -> expr2
```
