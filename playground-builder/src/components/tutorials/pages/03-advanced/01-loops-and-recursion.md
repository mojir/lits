# Loops & Recursion

Lits provides `for` comprehensions for building arrays, `doseq` for side effects, and `loop`/`recur` for tail-recursive iteration.

## For Comprehensions

`for` iterates over a collection and returns a new array:

```
for (x in [1, 2, 3, 4]) -> x * 2
```

## Filtering with when

Use `when` to skip elements that don't match a condition:

```
for (x in range(10) when odd?(x)) -> x * x
```

## Early Exit with while

`while` stops the iteration entirely when the condition becomes false:

```
for (x in range(100) while x < 5) -> x * 10
```

## Local Bindings with let

Bind intermediate values inside the comprehension:

```
for (x in [1, 2, 3] let sq = x * x) -> sq + 1
```

## Multiple Iterators

Multiple bindings produce a cartesian product:

```
for (i in [1, 2], j in [10, 20]) -> i + j
```

## Complex Comprehension

Combine `let`, `when`, and `while` for powerful queries:

```
for (
  i in range(10)
  let sq = i ^ 2
  while sq < 50
  when sq % 3 == 0
) -> sq
```

## Loop / Recur

`loop` sets up initial bindings, and `recur` jumps back to the top with new values. This is tail-recursive and efficient:

```
loop (n = 5, acc = 1) ->
  if n <= 1 then
    acc
  else
    recur(n - 1, acc * n)
  end
```

## Self Recursion

Inside a lambda, `self` refers to the enclosing function:

```
let fib = n ->
  if n <= 1 then
    n
  else
    self(n - 1) + self(n - 2)
  end
fib(10)
```

## Doseq (Side Effects)

`doseq` iterates for side effects and returns `null`:

```
doseq (x in [1, 2, 3]) -> write!(x)
```
