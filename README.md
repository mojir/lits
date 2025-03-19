# Lits

Lits is a lexically scoped pure functional language with algebraic notation. It combines the power of functional programming with an intuitive, readable syntax.

## Features

- **Pure functional language** - Variables cannot be changed, ensuring predictable behavior and easier reasoning about code
- **First-class functions** - Functions are treated as values that can be passed to other functions
- **Algebraic notation** - All operators can be used as functions, and functions that take two parameters can be used as operators
- **Clojure-inspired functions** - Most core functions are inspired by Clojure, with a key difference in parameter order
- **Comprehensive standard library** - Rich set of functions for collections, math, strings, and more
- **Structural equality** - Objects are compared by value, not by reference
- **Destructuring** - Extract values from complex data structures with ease

## Installation

*[Add installation instructions here]*

## Quick Start

Here's a simple example to get you started:

```
// Defining a function
function square(x)
  x * x
end

// Using the function
let result := square(5); // 25

// Using function as an operator
let squares := [1, 2, 3, 4, 5] map square; // [1, 4, 9, 16, 25]

// Using operator as a function
let sum := +([1, 2, 3, 4, 5]); // 15
```

## Syntax

### Basic Data Types

```
// Numbers
42       // integer
3.14     // float
0xFFFF   // hexadecimal
0b1100   // binary
0o77     // octal
-2.3e-2  // scientific notation

// Strings
"Hello, world!"

// Booleans
true
false

// Null
null

// Arrays
[1, 2, 3, 4]

// Objects
{ name := "John", age := 30 }
```

### Variable Binding

```
// Let expression
let x := 10;

// Variables are immutable
let x := 20; // Error: x is already defined

// But can be shadowed in inner scopes
let y := do
  let x := 20;
  x
end; // y is 20, outer x is still 10
```

### Functions

```
// Standard function definition
function add(a, b)
  a + b
end

// Lambda functions
let add := (a, b) -> a + b;

// Short form with positional arguments
let add := -> $1 + $2;

// Single argument short form
let cube := x -> x ** 3;
let fourth := -> $ ** 4;
```

### Control Flow

```
// If expression
if x > 10 then
  "large"
else
  "small"
end

// Unless expression (reversed if)
unless x > 10 then
  "small"
else
  "large"
end

// Switch expression
switch x
  case 0 then "zero"
  case 1 then "one"
  case 2 then "two"
end

// Cond expression
cond
  case val < 5 then "S"
  case val < 10 then "M"
  case val < 15 then "L"
end ?? "No match"

// Try/catch
try
  riskyOperation()
catch (error)
  "Error: " ++ error.message
end
```

### List Comprehension

```
// Simple for comprehension
for
  each x of [0, 1, 2, 3, 4, 5], let y := x * 3, while even?(y)
do
  y
end

// Multiple generators
for (
  each x of [1, 2, 3]
  each y of [1, 2, 3], when x <= y
  z of [1, 2, 3]
)
  [x, y, z]
end
```

### Destructuring

```
// Object destructuring
let { name, age } := { name := "John", age := 30 };

// Array destructuring
let [first, second] := [1, 2, 3, 4];

// Destructuring in function parameters
function displayPerson({name, age})
  name ++ " is " ++ str(age) ++ " years old"
end
```

## Operators and Functions

### Function Usage vs Operator Usage

All functions that take two parameters can be used as operators:

```
// As a function
max(5, 10)  // 10

// As an operator
5 max 10    // 10
```

All operators can be used as functions:

```
// As an operator
5 + 3       // 8

// As a function
+(5, 3)     // 8
```

### Parameter Order

Unlike Clojure, Lits favors subject-first parameter order:

```
// Lits
filter([1, 2, 3, 4], odd?)  // [1, 3]

// Equivalent Clojure
// (filter odd? [1 2 3 4])
```

This makes operator usage more readable:

```
[1, 2, 3, 4] filter odd?  // [1, 3]
```

## Built-in Functions

Lits comes with a comprehensive standard library of functions organized into categories:

### Collection Functions
`count`, `get`, `get-in`, `contains?`, `assoc`, `assoc-in`, `++`, `not-empty`, `every?`, `not-every?`, `any?`, `not-any?`, `update`, `update-in`

### Array Functions
`array`, `range`, `repeat`, `flatten`, `mapcat`

### Sequence Functions
`nth`, `push`, `pop`, `unshift`, `shift`, `slice`, `splice`, `reductions`, `reduce`, `reduce-right`, `map`, `filter`, `position`, `index-of`, `last-index-of`, `some`, `reverse`, `first`, `second`, `last`, `rest`, `next`, `take`, `take-last`, `take-while`, `drop`, `drop-last`, `drop-while`, `sort`, `sort-by`, `distinct`, `remove`, `remove-at`, `split-at`, `split-with`, `frequencies`, `group-by`, `partition`, `partition-all`, `partition-by`, `starts-with?`, `ends-with?`, `interleave`, `interpose`

### Math Functions
`+`, `-`, `*`, `/`, `mod`, `%`, `quot`, `inc`, `dec`, `√`, `∛`, `**`, `round`, `trunc`, `floor`, `ceil`, `min`, `max`, `abs`, `sign`, `log`, `log2`, `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`

### Functional Programming
`apply`, `identity`, `partial`, `comp`, `constantly`, `juxt`, `complement`, `every-pred`, `some-pred`, `fnull`

### Object Functions
`dissoc`, `object`, `keys`, `vals`, `entries`, `find`, `merge`, `merge-with`, `zipmap`, `select-keys`

### String Functions
`string-repeat`, `str`, `number`, `lower-case`, `upper-case`, `trim`, `trim-left`, `trim-right`, `pad-left`, `pad-right`, `split`, `split-lines`, `template`, `to-char-code`, `from-char-code`, `encode-base64`, `decode-base64`, `encode-uri-component`, `decode-uri-component`, `join`, `capitalize`, `blank?`

### Predicates
`boolean?`, `null?`, `number?`, `string?`, `function?`, `integer?`, `array?`, `object?`, `coll?`, `seq?`, `regexp?`, `zero?`, `pos?`, `neg?`, `even?`, `odd?`, `finite?`, `nan?`, `negative-infinity?`, `positive-infinity?`, `false?`, `true?`, `empty?`, `not-empty?`

### Regular Expressions
`regexp`, `match`, `replace`, `replace-all`

### Bitwise Operations
`<<`, `>>`, `>>>`, `~`, `&`, `bit-and-not`, `|`, `^`, `bit-flip`, `bit-clear`, `bit-set`, `bit-test`

### Assertions
`assert`, `assert=`, `assert!=`, `assert-gt`, `assert-lt`, `assert-gte`, `assert-lte`, `assert-true`, `assert-false`, `assert-truthy`, `assert-falsy`, `assert-null`, `assert-throws`, `assert-throws-error`, `assert-not-throws`

## Modules and Exports

You can export definitions to make them available to other modules:

```
// Exporting variables
export let PI := 3.14159;

// Exporting functions
export function square(x)
  x * x
end
```

## API

Lits provides a JavaScript API for embedding:

```javascript
interface Lits {
  getRuntimeInfo: () => LitsRuntimeInfo
  run: (program: string, params?: ContextParams & FilePathParams) => unknown
  context: (program: string, params?: ContextParams & FilePathParams) => Context
  getUndefinedSymbols: (programOrAst: string | Ast, params: ContextParams) => Set<string>
  tokenize: (program: string, tokenizeParams: FilePathParams & MinifyParams) => TokenStream
  parse: (tokenStream: TokenStream) => Ast
  evaluate: (ast: Ast, params: ContextParams) => unknown
  transformSymbols: (tokenStream: TokenStream, transformer: (symbol: string) => string) => TokenStream
  untokenize: (tokenStream: TokenStream) => string
  apply: (fn: LitsFunction, fnParams: unknown[], params: ContextParams) => unknown
}
```

## Examples

### Factorial Function

```
function factorial(n)
  if n <= 1 then
    1
  else
    n * factorial(n - 1)
  end
end

factorial(5) // 120
```

### Fibonacci Sequence

```
function fib(n)
  if n < 2 then
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

// Generate the first 10 Fibonacci numbers
range(10) map fib // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

### Working with Collections

```
let people := [
  { name := "Alice", age := 25 },
  { name := "Bob", age := 30 },
  { name := "Charlie", age := 35 },
  { name := "Diana", age := 40 }
];

// Get all names
people map (p -> p.name) // ["Alice", "Bob", "Charlie", "Diana"]

// Get people older than 30
people filter (p -> p.age > 30) // [{ name: "Charlie", age: 35 }, { name: "Diana", age: 40 }]

// Calculate average age
people map (p -> p.age) reduce (+) / count(people) // 32.5
```

## Contributing

*[Add contribution guidelines here]*

## License

*[Add license information here]*