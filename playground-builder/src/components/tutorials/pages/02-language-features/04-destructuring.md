# Destructuring

Destructuring lets you extract values from arrays and objects directly into named bindings. It works in `let` bindings and function parameters.

## Object Destructuring

Pull out properties by name:

```
let { name, age } = { name: "Alice", age: 30 };
name
```

## Default Values

Provide defaults for missing properties:

```
let { name, role = "guest" } = { name: "Bob" };
role
```

## Renaming with as

Use `as` to bind a property to a different variable name:

```
let { name as userName } = { name: "Carol" };
userName
```

## Nested Object Destructuring

Reach into nested structures:

```
let { profile: { age } } = { profile: { age: 25 } };
age
```

## Array Destructuring

Extract elements by position. Use commas to skip elements:

```
let [a, b, c] = [10, 20, 30];
b
```

```
let [, , third] = [1, 2, 3];
third
```

## Array Defaults

Array elements can have defaults too:

```
let [x, y = 99] = [42];
y
```

## Rest Patterns

Collect remaining elements with `...`:

```
let [head, ...tail] = [1, 2, 3, 4, 5];
tail
```

```
let { user, ...others } = { user: "Eve", age: 28, city: "NYC" };
others
```

## Function Parameter Destructuring

Destructure directly in function parameters:

```
let greet = ({ name }) -> "Hello, " ++ name;
greet({ name: "Pat" })
```

```
let sum-pair = ([a, b]) -> a + b;
sum-pair([3, 7])
```

## Combining Patterns

Mix array and object destructuring for complex data:

```
let [{ name }, { age }] = [{ name: "Tina" }, { age: 33 }];
name ++ " is " ++ str(age)
```
