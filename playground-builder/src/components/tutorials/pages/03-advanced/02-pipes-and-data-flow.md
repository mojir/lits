# Pipes & Data Flow

Lits has powerful features for building readable data transformation pipelines: the pipe operator, data-as-functions, and operator-style calling.

## The Pipe Operator

Use `|>` to pass a value through a chain of transformations. Use `_` to mark where the piped value goes:

```
[1, 2, 3, 4, 5, 6]
  |> filter(_, odd?)
  |> map(_, -> $ * $)
  |> reduce(_, +, 0)
```

## Pipe Without Placeholder

When piping to a single-argument function, no placeholder is needed:

```
"hello world"
  |> upper-case
  |> split(_, " ")
  |> reverse
  |> join(_, "-")
```

## Operator Style

Any two-argument function can be used as an infix operator. The left value becomes the first argument:

```
[1, 2, 3, 4] filter odd?
```

```
[1, 2, 3] map inc
```

## Chaining Operators

Chain multiple operator-style calls for a fluent reading:

```
[1, 2, 3, 4, 5, 6] filter even? map -> $ * $
```

## Arrays as Functions

An array can be called as a function with an index to get that element:

```
let arr = [10, 20, 30];
arr(1)
```

## Numbers as Functions

A number can be called with a collection to access that index:

```
let words = ["alpha", "beta", "gamma"];
1(words)
```

## Strings as Functions

A string can be called with an object to access that property. This is powerful with `map`:

```
let people = [{ name: "Alice" }, { name: "Bob" }];
people map "name"
```

## Objects as Functions

An object can be called with a key to get the value:

```
let config = { host: "localhost", port: 8080 };
config("port")
```

## Putting it Together

Combine pipes, data-as-functions, and operators for expressive data processing:

```
let data = [
  { name: "Alice", score: 95 },
  { name: "Bob", score: 60 },
  { name: "Carol", score: 82 }
];
data
  |> filter(_, -> $.score >= 80)
  |> map(_, "name")
```
