# Functions

Functions are first-class values in Lits. You can define them, pass them around, and compose them freely.

## Arrow Functions

Define functions with the arrow (`->`) syntax. For a single parameter, parentheses are optional:

```
let double = x -> x * 2;
double(21)
```

## Multiple Parameters

Wrap multiple parameters in parentheses:

```
let add = (a, b) -> a + b;
add(3, 4)
```

## No Parameters

Use empty parentheses for functions that take no arguments:

```
let greet = () -> "Hello!";
greet()
```

## Default Parameters

Parameters can have default values:

```
let greet = (name = "World") -> "Hello, " ++ name;
greet()
```

## Rest Parameters

Collect remaining arguments with the rest (`...`) syntax:

```
let sum-all = (...nums) -> reduce(nums, +, 0);
sum-all(1, 2, 3, 4, 5)
```

## Short-hand Lambdas

For quick one-liners, use `->` with `$` (or `$1`, `$2`, …) for positional arguments:

```
map([1, 2, 3], -> $ * $)
```

```
map([1, 2, 3, 4], -> $1 + 10)
```

## Recursion with self

A lambda can call itself using `self`:

```
let factorial = n ->
  if n <= 1 then
    1
  else
    n * self(n - 1)
  end;
factorial(6)
```

## Composition

`comp` composes functions right-to-left — the rightmost runs first:

```
(comp str inc)(41)
```

## Higher-order Functions

Functions can be passed as arguments. This is the heart of functional programming:

```
let double = x -> x * 2;
map([1, 2, 3, 4], double)
```

## Apply

Call a function with an array of arguments using `apply`:

```
apply(+, [1, 2, 3, 4, 5])
```
