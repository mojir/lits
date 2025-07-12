# Lits

Lits is a lexically scoped pure functional language with algebraic notation. It combines the power of functional programming with an intuitive, readable syntax that makes complex operations simple and expressive.

Try it in the [Lits Playground](https://mojir.github.io/lits/).

## Features

- **Pure functional language** - Variables cannot be changed, ensuring predictable behavior and easier reasoning about code
- **JavaScript interoperability** - JavaScript values and functions can easily be exposed in Lits
- **First-class functions** - Functions are treated as values that can be passed to other functions
- **Algebraic notation** - All operators can be used as functions, and functions that take two parameters can be used as operators
- **Comprehensive standard library** - Rich set of functions for collections, math, strings, and more
- **Structural equality** - Objects are compared by value, not by reference
- **Destructuring** - Extract values from complex data structures with ease
- **Lexical scoping** - Variables are scoped to their defining context

## Installation

### As a Library

```bash
npm install @mojir/lits
```

### CLI Tool

Install globally to use the Lits command-line interface:

```bash
npm install --global @mojir/lits
```

#### CLI Usage

```bash
# Start an interactive REPL session
$ lits

# Evaluate Lits code directly
$ lits -e "5 + 3"
$ lits -e "[1, 2, 3, 4] filter odd? map inc"

# Run a Lits file
$ lits -f script.lits
$ lits -f examples/factorial.lits

# Get help
$ lits --help
```

The REPL provides an interactive environment where you can experiment with Lits code, test functions, and explore the language features in real-time.

## Quick Start

Here's a simple example to get you started:

```lits
// Defining a function
let square = x -> x * x;

// Using the function
let result = square(5);
// => 25

// Using function as an operator
let squares = [1, 2, 3, 4, 5] map square;
// => [1, 4, 9, 16, 25]

// Using operator as a function
let sum = +([1, 2, 3, 4, 5]);
// => 15
```

## Basic Syntax

### Data Types

```lits
// Numbers
42;          // integer
3.14;        // float
0xFFFF;      // hexadecimal
0b1100;      // binary
0o77;        // octal
-2.3e-2;     // scientific notation

// Strings
"Hello, world!";

// Booleans
true;
false;

// Null
null;

// Arrays
[1, 2, 3, 4];

// Objects
{ name: "John", age: 30 };

// Regular expressions
#"pattern";
```

### Mathematical Constants

Lits provides predefined mathematical constants:

```lits
PI;    // => 3.141592653589793
π;     // => 3.141592653589793 (Unicode alternative)
E;     // => 2.718281828459045 (Euler's number)
ε;     // => 2.718281828459045 (Unicode alternative)
PHI;   // => 1.618033988749895 (Golden ratio)
φ;     // => 1.618033988749895 (Unicode alternative)

// Infinity values
POSITIVE_INFINITY; // => Infinity
∞;                 // => Infinity (Unicode alternative)
NEGATIVE_INFINITY; // => -Infinity

// Integer and float limits
MAX_SAFE_INTEGER;  // => 9007199254740991
MIN_SAFE_INTEGER;  // => -9007199254740991
MAX_VALUE;         // => 1.7976931348623157e+308
MIN_VALUE;         // => 5e-324
```

## Special Expressions

### Variable Binding

#### Let

```lits
// Simple binding
let x = 10;
// => 10

// Variables are immutable
// let x = 20; // Error: x is already defined

// Shadowing in inner scopes
let y = {
  let x = 20;
  x
};
// => 20, outer x is still 10
```

#### Destructuring

```lits
// Object destructuring
let { name, age } = { name: "John", age: 30 };
// name => "John", age => 30
```

```lits
// Array destructuring
let [a, b] = [1, 2, 3, 4];
// a => 1, b => 2
```

```lits
// With default values
let { name = "Unknown", age = 0 } = { name: "John" };
// name => "John", age => 0
```

```lits
// Rest patterns
let [head, ...tail] = [1, 2, 3, 4];
// head => 1, tail => [2, 3, 4]
```

```lits
// Destructuring in function parameters
let displayPerson = ({name, age}) ->
  name ++ " is " ++ str(age) ++ " years old";

displayPerson({ name: "John", age: 30 });
// => "John is 30 years old"
```

### Functions

#### Lambda Functions

```lits
// Multi-parameter lambda
let add = (a, b) -> a + b;

// Single parameter (parentheses optional)
let square = x -> x * x;

// No parameters
let constant = () -> 42;

// Positional arguments
let add-v2 = -> $1 + $2;

// Single positional argument
let square-v2 = -> $ * $;

// Self-reference for recursion
let factorial = n ->
  if n <= 1 then
    1
  else
    n * self(n - 1)
  end;
```

### Control Flow

#### If/Unless

```lits
// If expression
let x = !:random-int(0, 20); // Random number between 0 and 19

if x > 10 then
  "large"
else
  "small"
end;
// => "large" (if x > 10) or "small" (if x <= 10)

// If without else returns null
if false then "never" end;
// => null

// Unless (inverted if)
unless x > 10 then
  "small"
else
  "large"
end;
// => "small" (if x <= 10) or "large" (if x > 10)
```

#### Cond

```lits
let x = !:random-int(0, 20); // Random number between 0 and 19

// Multi-branch conditional
cond
  case x < 5 then "small"
  case x < 10 then "medium"
  case x < 15 then "large"
end ?? "extra large"
// Tests conditions sequentially, returns first truthy match
```

#### Switch

```lits
let x = !:random-int(0, 3); // Random number between 0 and 2

// Switch on value
switch x
  case 0 then "zero"
  case 1 then "one"
  case 2 then "two"
end
// => "zero" (if x = 0), "one" (if x = 1), etc., or null if no match
```

### Loops and Iteration

#### For Comprehensions

```lits
// Simple iteration
for (x in [1, 2, 3, 4]) -> x * 2;
// => [2, 4, 6, 8]

// With filtering
for (x in [1, 2, 3, 4] when odd?(x)) -> x * 2;
// => [2, 6]

// With while condition
for (x in [1, 2, 3, 4] while x < 3) -> x * 2;
// => [2, 4]

// With let bindings
for (x in [1, 2, 3] let doubled = x * 2) -> doubled + 1;
// => [3, 5, 7]

// Multiple iterators
for (x in [1, 2], y in [10, 20]) -> x + y;
// => [11, 21, 12, 22]

// Object iteration
for (entry in { a: 1, b: 2 } let [key, value] = entry) -> key ++ ":" ++ str(value);
// => ["a:1", "b:2"]
```

#### Doseq (Side Effects)

```lits
// For side effects only (returns null)
doseq (x in [1, 2, 3]) -> write!(x)
// Prints: 1 2 3, returns null
```

#### Loop (Tail Recursion)

```lits
// Loop with recur for tail recursion
loop (n = 5, acc = 1) -> {
  if zero?(n) then
    acc
  else
    recur(n - 1, acc * n)
  end
}
// => 120 (factorial of 5)
```

### Error Handling

#### Try/Catch

```lits
// Basic try/catch
try
  riskyOperation()
catch
  "Something went wrong"
end;

// With error binding
try
  riskyOperation()
catch (error)
  "Error: " ++ error.message
end;
```

#### Throw

```lits
// Throwing errors
try
  throw("Custom error message");
catch
  "Caught an error"
end;

// In context
let divide = (a, b) ->
  if zero?(b) then
    throw("Division by zero")
  else
    a / b
  end;
```

### Logical Operators

#### And/Or

```lits
// Logical AND (short-circuit)
true && "second value";   // => "second value"
false && "never reached"; // => false

// Logical OR (short-circuit)
false || "default value"; // => "default value"
true || "never reached";  // => true

// Multiple arguments
&&(true, true, "all true"); // => "all true"
||(false, false, "found");  // => "found"
```

#### Null Coalescing

```lits
// Null coalescing operator
null ?? "default";     // => "default"
undefined ?? "default"; // => "default"
0 ?? "default";        // => 0 (only null/undefined are coalesced)
false ?? "default";    // => false
"" ?? "default";       // => ""
```

### Blocks

```lits
// Block expressions
{
  let a = 1 + 2 + 3;
  let b = x -> x * x;
  b(a)
}
// => 36 (returns value of last expression)
```

### Arrays and Objects

#### Array Construction

```lits
// Array literal
[1, 2, 3, 4];

// Array function
array(1, 2, 3, 4);

// With spread
let small-set = [3, 4, 5];
[1, 2, ...small-set, 6];
// => [1, 2, 3, 4, 5, 6];
```

#### Object Construction

```lits
// Object literal with static keys
{ name: "John", age: 30 };

// Object literal with dynamic keys using bracket notation
let keyName = "dynamic";
{ [keyName]: "value", ["computed" ++ "Key"]: 42 };
// => { dynamic: "value", computedKey: 42 }

// Object function
object("name", "John", "age", 30);

// With spread
let defaults = { type: "Person", active: true };
{
  ...defaults,
  name: "John",
  age: 30
};
// => { type: "Person", active: true, name: "John", age: 30 }

// Combining static and dynamic keys
let propName = "score";
{
  id: 123,
  [propName]: 95,
  ["level" ++ "Number"]: 5
};
// => { id: 123, score: 95, levelNumber: 5 }
```

### Recursion

#### Recur

```lits
// Tail recursion with recur
let countdown = n -> {
  write!(n);
  if pos?(n) then
    recur(n - 1)
  end
};

countdown(3)
// Prints: 3 2 1 0
```

## Operators and Functions

### Algebraic Notation

All functions that take two parameters can be used as operators:

```lits
// As a function
max(5, 10);    // => 10

// As an operator
5 max 10;      // => 10
```

All operators can be used as functions:

```lits
// As an operator
5 + 3;         // => 8

// As a function
+(5, 3);       // => 8

// Partial application with underscore placeholder
let add5 = +(5, _);
add5(3);       // => 8

// Multiple placeholders
let subtractTwoValues = -(100, _, _);
subtractTwoValues(4, 3);  // => 93

// Single placeholder in different positions
let subtract = -(_, 2);
subtract(10);  // => 8

let divide = /(10, _);
divide(2);     // => 5
```

### Data Types as Functions

Lits allows arrays, objects, numbers, and strings to be used as functions. This creates elegant, flexible code where data structures become accessors.

#### Arrays and Numbers as Index Accessors

Arrays can be called with an index to get an element, and numbers can be called with collections to access that index:

```lits
let arr = [10, 20, 30, 40];

// Array as function (accessing by index)
arr(0);          // => 10
arr(2);          // => 30

// Number as function (accessing array at that index)
2(arr);          // => 30 (same as arr(2))
0(arr);          // => 10 (same as arr(0))
```

#### Strings and Numbers for Character Access

Similar to arrays, strings support indexed access in both directions:

```lits
let name = "Albert";

// String as function (accessing character by index)
name(0);         // => "A"
name(2);         // => "b"

// Number as function (accessing string at that index)  
2(name);         // => "b" (same as name(2))
4(name);         // => "r" (same as name(4))
```

#### Objects and Strings as Property Accessors

Objects can be called with property names, and strings can be called with objects to access properties:

```lits
let person = { foo: 1, bar: 2, name: "John" };

// Object as function (accessing property by key)
person("foo");   // => 1
person("name");  // => "John"

// String as function (accessing object property)
"foo"(person);   // => 1 (same as person("foo"))
"bar"(person);   // => 2 (same as person("bar"))
```

#### Powerful Higher-Order Function Applications

This feature makes higher-order functions incredibly flexible. You can pass data directly as accessor functions:

```lits
let data = [
  { name: "Alice", score: 95 },
  { name: "Bob", score: 87 },
  { name: "Carol", score: 92 }
];

// Extract names using string as function
data map "name";
// => ["Alice", "Bob", "Carol"]

// Extract scores using string as function  
data map "score";
// => [95, 87, 92]

// Get second element of multiple arrays using number as function
let arrays = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
arrays map 1;
// => [2, 5, 8]

// Access nested data
let records = [
  { values: [10, 20, 30] },
  { values: [40, 50, 60] },
  { values: [70, 80, 90] }
];

// Get first value from each record's values array
records map "values" map 0;
// => [10, 40, 70]
```

#### Practical Examples

```lits
// Matrix column extraction
let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
matrix map 1;        // => [2, 5, 8] (second column)

// Object property extraction
let users = [
  { id: 1, active: true },
  { id: 2, active: false },
  { id: 3, active: true }
];
users map "active";  // => [true, false, true]

// String character extraction
let words = ["hello", "world", "test"];
words map 0;         // => ["h", "w", "t"] (first characters)

// Complex data navigation
let sales = [
  { quarter: "Q1", regions: { north: 100, south: 200 } },
  { quarter: "Q2", regions: { north: 150, south: 180 } }
];
sales map "regions" map "north";  // => [100, 150]
```

This feature eliminates the need for verbose accessor functions and makes data transformation pipelines more concise and readable.
### Parameter Order

Lits favors subject-first parameter order for better operator chaining:

```lits
// Function style
filter([1, 2, 3, 4], odd?);  // => [1, 3]

// Operator style (more readable)
[1, 2, 3, 4] filter odd?;    // => [1, 3]
```

### Pipe Operator

The pipe operator `|>` passes the result of the left expression as the first argument to the right function:

```lits
// Without pipe operator
reduce(map(filter([1, 2, 3, 4, 5, 6], odd?), -> $ + $), +, 0);

// With pipe operator (much more readable)
[1, 2, 3, 4, 5, 6]
  |> filter(_, odd?)
  |> map(_, -> $ * $)
  |> reduce(_, +, 0);
// => 35

// Simple transformations
"hello world"
  |> upper-case
  |> split(_, " ")
  |> reverse
  |> join(_, "-");
// => "WORLD-HELLO"

// Mathematical operations
10
  |> +(_, 5)
  |> *(_, 2)
  |> /(_, 3);
// => 10 (10 + 5 = 15, 15 * 2 = 30, 30 / 3 = 10)

// Data processing pipeline
{ numbers: [1, 2, 3, 4, 5], multiplier: 3 }
  |> get(_, "numbers")
  |> filter(_, even?)
  |> map(_, *(_, 3))
  |> reduce(_, +, 0);
// => 18 (even numbers [2, 4] -> [6, 12] -> sum = 18)
```

### Operator Precedence

Lits follows a specific operator precedence order that determines how expressions are evaluated. Operators with higher precedence are evaluated first. When operators have the same precedence, they are evaluated left-to-right.

Here's the complete precedence table, from highest to lowest:

| Precedence | Operator(s) | Description | Example |
|------------|-------------|-------------|---------|
| 12 | `^` | Exponentiation | `2 ^ 3 ^ 2` → `2 ^ (3 ^ 2)` → `512` |
| 11 | `*` `/` `%` | Multiplication, Division, Remainder | `6 + 4 * 2` → `6 + 8` → `14` |
| 10 | `+` `-` | Addition, Subtraction | `10 - 3 + 2` → `7 + 2` → `9` |
| 9 | `<<` `>>` `>>>` | Bit shift operations | `8 >> 1 + 1` → `8 >> 2` → `2` |
| 8 | `++` | String concatenation | `"a" ++ "b" ++ "c"` → `"abc"` |
| 7 | `<` `<=` `≤` `>` `>=` `≥` | Comparison operators | `3 + 2 > 4` → `5 > 4` → `true` |
| 6 | `==` `!=` `≠` | Equality operators | `2 * 3 == 6` → `6 == 6` → `true` |
| 5 | `&` `xor` `\|` | Bitwise operations | `4 \| 2 & 1` → `4 \| 0` → `4` |
| 4 | `&&` `\|\|` `??` | Logical operations | `true && false \|\| true` → `false \|\| true` → `true` |
| 3 | *function operators* | Binary functions used as operators | `5 max 3 + 2` → `5 max 5` → `5` |
| 2 | `\|>` | Pipe operator | `[1,2] \|> map(_, inc) \|> sum` |
| 1 | `?` `:` | Conditional (ternary) operator | `true ? 1 + 2 : 3` → `true ? 3 : 3` → `3` |

#### Examples of Precedence in Action

```lits
// Exponentiation has highest precedence
2 + 3 ^ 2;           // => 2 + 9 = 11 (not 5^2 = 25)

// Multiplication before addition
2 + 3 * 4;           // => 2 + 12 = 14 (not 5*4 = 20)

// String concatenation before comparison
"a" ++ "b" == "ab";  // => "ab" == "ab" = true

// Comparison before logical AND
3 > 2 && 1 < 2;      // => true && true = true

// Pipe has very low precedence
[1, 2, 3] |> map(_, inc) |> vec:sum;  // Evaluates left to right

// Conditional has lowest precedence
true ? 2 + 3 : 4 + 5;             // => true ? 5 : 9 = 5
```

#### Using Parentheses

When in doubt, or to make your intent clear, use parentheses to override precedence:

```lits
// Without parentheses (follows precedence)
2 + 3 * 4;          // => 14

// With parentheses (explicit grouping)
(2 + 3) * 4;        // => 20

// Complex expression with explicit grouping
let a = 2;
let b = 3;
let c = 4;
let d = true;
let e = false;
let f = 10;
let g = 5;
((a + b) * c) > (d && e ? f : g);  // => (5 * 4) > (false ? 10 : 5) = 20 > 5 = true
```

#### Associativity

Most operators are left-associative, meaning they evaluate from left to right when they have the same precedence:

```lits
10 - 5 - 2;         // => (10 - 5) - 2 = 3 (not 10 - (5 - 2) = 7)
"a" ++ "b" ++ "c";  // => ("a" ++ "b") ++ "c" = "abc"
```

**Exception**: Exponentiation (`^`) is right-associative:
```lits
2 ^ 3 ^ 2;          // => 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (not (2 ^ 3) ^ 2 = 64)
```

## Built-in Functions

Lits comes with a comprehensive standard library of functions for:

- **Arithmetic and Math**: Basic operations, trigonometry, logarithms, rounding
- **Collections**: Working with arrays and objects (get, assoc, merge, etc.)
- **Sequences**: Filtering, mapping, reducing, sorting, and transforming data
- **Strings**: Manipulation, formatting, encoding, and pattern matching
- **Predicates**: Type checking and condition testing functions
- **Functional Programming**: Function composition, partial application, and utilities
- **Regular Expressions**: Pattern matching and text processing
- **Bitwise Operations**: Low-level bit manipulation
- **Assertions**: Testing and validation utilities

For a complete reference of all available functions with examples, visit the [Lits Playground](https://mojir.github.io/lits/) where you can explore the interactive documentation and try functions in real-time.

## Modules and Exports

```lits
// Export variables and functions
export let pi = 3.14159;
export let square = x -> x * x;

// Exported values become available to other modules
```

## Examples

### Factorial

```lits
let factorial = n ->
  if n <= 1 then
    1
  else
    n * self(n - 1)
  end;

factorial(5)  // => 120
```

### Array Processing

```lits
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Get even numbers squared
let evenSquares = numbers
  |> filter(_, even?)
  |> map(_, -> $ * $);
// => [4, 16, 36, 64, 100]

// Sum of odd numbers
let oddSum = numbers
  |> filter(_, odd?)
  |> reduce(_, +, 0);
// => 25
```

### Object Manipulation

```lits
let person = { name: "John", age: 30, city: "New York" };

// Update age
let older = assoc(person, "age", 31);

// Add new field
let withJob = assoc(older, "job", "Developer");

// Using pipe operator for chaining
let updated = person
  |> assoc(_, "age", 31)
  |> assoc(_, "job", "Developer");
```

### Pattern Matching Examples

#### Switch (for matching against a single value)

```lits
let gradeToLetter = grade ->
  switch grade
    case 90 then "A"
    case 80 then "B" 
    case 70 then "C"
    case 60 then "D"
  end ?? "F";

gradeToLetter(80);  // => "B"
gradeToLetter(55);  // => "F"

// HTTP status code handling
let statusMessage = code ->
  switch code
    case 200 then "OK"
    case 404 then "Not Found"
    case 500 then "Internal Server Error"
  end ?? "Unknown Status";
```

#### Cond (for testing different conditions)

```lits
let describe = x ->
  cond
    case null?(x) then "nothing"
    case number?(x) then "a number: " ++ str(x)
    case string?(x) then "text: " ++ x
    case array?(x) then "list of " ++ str(count(x)) ++ " items"
  end ?? "unknown type";

describe(42);      // => "a number: 42"
describe([1,2,3]); // => "list of 3 items"

// Grade ranges
let gradeToLetter = score ->
  cond
    case score >= 90 then "A"
    case score >= 80 then "B"
    case score >= 70 then "C"
    case score >= 60 then "D"
  end ?? "F";

gradeToLetter(85);  // => "B"
gradeToLetter(55);  // => "F"
```

## JavaScript Interoperability

Lits provides a comprehensive JavaScript API for embedding and integration:

```javascript
import { Lits } from 'lits';

// Create a Lits instance
const lits = new Lits();

// Run Lits code
const result = lits.run('+(5, 3)');
console.log(result); // 8

// Pass JavaScript values to Lits
const result2 = lits.run('name ++ " is " ++ str(age)', {
  values: {
    name: "John",
    age: 30
  }
});
console.log(result2); // "John is 30"

// Expose JavaScript functions to Lits
const result3 = lits.run('myAlert("Hello from Lits!")', {
  jsFunctions: {
    myAlert: {
      fn: (message) => console.log(`Alert: ${message}`),
      arity: { min: 1, max: 1 }
    }
  }
});

// Parse and evaluate separately for better performance
const tokens = lits.tokenize('+(5, 3)');
const ast = lits.parse(tokens);
const result4 = lits.evaluate(ast, {});
```

### Lits Class Methods

```typescript
interface Lits {
  // Execute Lits code directly
  run(program: string, params?: ContextParams & FilePathParams): unknown
  
  // Get execution context after running code
  context(programOrAst: string | Ast, params?: ContextParams & FilePathParams): Context
  
  // Find undefined symbols in code
  getUndefinedSymbols(programOrAst: string | Ast, params?: ContextParams): Set<string>
  
  // Parse pipeline
  tokenize(program: string, params?: FilePathParams & MinifyParams): TokenStream
  parse(tokenStream: TokenStream): Ast
  evaluate(ast: Ast, params: ContextParams): unknown
  
  // Apply Lits function with JavaScript arguments
  apply(fn: LitsFunction, fnParams: unknown[], params?: ContextParams): unknown
  
  // Utility methods
  transformSymbols(tokenStream: TokenStream, transformer: (symbol: string) => string): TokenStream
  untokenize(tokenStream: TokenStream): string
  getRuntimeInfo(): LitsRuntimeInfo
}
```

### Context Parameters

```typescript
interface ContextParams {
  globalContext?: Context          // Global variable context
  contexts?: Context[]             // Additional context layers
  values?: Record<string, unknown> // JavaScript values to expose
  jsFunctions?: Record<string, JsFunction> // JavaScript functions to expose
  globalModuleScope?: boolean      // Module scoping behavior
}

interface JsFunction {
  fn: (...args: any[]) => unknown // The JavaScript function
  arity?: Arity                   // Function arity constraints
  docString?: string              // Documentation
}
```

## Learn More

- Try Lits in the [online playground](https://mojir.github.io/lits/)
- Explore the comprehensive function reference
- Check out more complex examples in the documentation

Lits combines the elegance of functional programming with practical syntax, making it perfect for data transformation, mathematical computation, and any scenario where immutability and expressiveness are valued.
