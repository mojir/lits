# Intro

Lits is a lightweight, pure functional programming language designed to embed directly in JavaScript applications. It runs in a secure sandbox — no file system access, no network calls, no mutation of host state — making it safe for user-supplied logic.

## Why Lits?

Many applications need runtime programmability: custom formulas, dynamic business rules, scriptable workflows, or plugin systems. Letting users write raw JavaScript is a security risk. Lits solves this by providing a powerful yet sandboxed language that integrates seamlessly with any JavaScript runtime.

```
// Lits is expression-based — everything returns a value
if 10 > 5 then "big" else "small" end
```

## Key Features

**Safe by design** — Lits code runs in a sandbox with zero access to the host environment unless explicitly granted. Users can script without the power to break things.

**Pure functional** — All data is immutable and all functions are pure. No side effects, no surprises.

```
let original = [1, 2, 3];
let extended = push(original, 4);
original // => [1, 2, 3] — unchanged
```

**Expression-oriented** — There are no statements. Everything — from `if` to `let` to `loop` — is an expression that returns a value.

```
let label = if 42 >= 0 then "positive" else "negative" end;
label
```

**JavaScript interoperability** — JavaScript values and functions can be exposed to Lits, and Lits results are plain JavaScript values. Integration is as simple as calling `run()` with a string of code.

**First-class functions** — Functions are values. Pass them around, return them from other functions, and compose them freely.

```
let double = x -> x * 2;
map([1, 2, 3], double)
```

**Algebraic notation** — Operators can be used as functions, and two-argument functions can be used as operators. Write what reads best.

```
// Same result, different style
let double = x -> x * 2;
// As function
map([1, 2, 3], double);
// As operator
[1, 2, 3] map double;
```

**Rich standard library** — Built-in functions for math, strings, collections, regular expressions, and more. Optional modules add vector math, linear algebra, matrix operations, number theory and more.

## What Lits Looks Like

Here's a taste of what you can do:

```
let factorial = n ->
  if n <= 1
    then 1
    else n * factorial(n - 1)
  end;

factorial(10)
```

```
let people = [
  { name: "Alice", age: 30 },
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
];

people
  |> _ filter (-> $.age >= 30)
  |> _ map "name";

```

Ready to dive in? Continue to the next page get lits installed.
