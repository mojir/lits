# Operators

Lits has a rich set of operators. A unique feature is that operators and functions are interchangeable — operators can be called as functions and two-argument functions can be used as infix operators.

## Arithmetic

The standard math operators, with whitespace required:

```
2 + 3 * 4
```

```
2 ^ 10
```

```
17 % 5
```

## Comparison

Comparison operators use structural equality (`==`), not reference equality:

```
3 > 2
```

```
[1, 2] == [1, 2]
```

```
1 != 2
```

## String Concatenation

Use `++` to concatenate strings and sequences:

```
"Hello" ++ ", " ++ "World!"
```

```
[1, 2] ++ [3, 4]
```

## Logical Operators

`&&` and `||` are short-circuit. `??` is the nullish coalescing operator:

```
true && "yes"
```

```
false || "fallback"
```

```
null ?? "default"
```

## Operators as Functions

Every operator can be called in function (prefix) form. Some are variadic:

```
+(1, 2, 3, 4, 5)
```

```
*(2, 3, 4)
```

```
<(1, 2, 3, 4)
```

## Functions as Operators

Any two-argument function can be used as an infix operator:

```
5 max 10
```

```
[1, 2, 3, 4] filter odd?
```

## Partial Application

Use `_` as a placeholder to create partially applied functions from operators:

```
let add5 = +(5, _);
add5(10)
```

```
let half = /(_, 2);
half(20)
```

## Ternary Operator

The ternary `? :` works as you would expect:

```
let age = 25;
age >= 18 ? "adult" : "minor"
```
