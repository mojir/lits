# Data Types

Lits has a small set of data types. Every value is immutable and fully serializable as JSON.

## Numbers

Numbers can be integers or floats. Lits also supports hexadecimal, binary, octal, and scientific notation:

```
42
```

```
3.14
```

```
0xFF
```

```
0b1010
```

```
-2.3e-2
```

## Strings

Strings are enclosed in double quotes and support escape sequences:

```
"Hello, World!"
```

```
"Line 1\nLine 2"
```

## Booleans and Null

The boolean values `true` and `false`, plus `null`:

```
true
```

```
null
```

## Arrays

Arrays hold ordered collections of any types:

```
[1, "two", true, null]
```

Use spread to merge arrays:

```
[1, 2, ...[3, 4], 5]
```

## Objects

Objects are key-value maps. Keys are strings:

```
{ name: "Alice", age: 30 }
```

Spread works in objects too:

```
let defaults = { theme: "dark", lang: "en" };
{ ...defaults, lang: "sv" }
```

## Regular Expressions

Regexp literals start with `#"`. No need to escape backslashes:

```
re-match("abc123", #"[a-z]+(\d+)")
```

## Type Predicates

Check the type of a value with predicate functions that end in `?`:

```
number?(42)
```

```
string?("hello")
```

```
array?([1, 2, 3])
```

```
object?({ a: 1 })
```

## Structural Equality

Values are compared by structure, not by reference. Two arrays with the same elements are equal:

```
[1, 2, 3] == [1, 2, 3]
```

```
{ a: 1 } == { a: 1 }
```

```
{ a: 1, b: 2 } == { b: 2, a: 1 }
```
