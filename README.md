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
let sum = +(1, 2, 3, 4, 5);
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

##### Basic Object Destructuring

```lits
// Object destructuring
let { name, age } = { name: "John", age: 30 };
// name => "John", age => 30
```

```lits
// With default values
let { name = "Unknown", age = 0 } = { name: "John" };
// name => "John", age => 0
```

```lits
// Renaming with 'as'
let { name as userName } = { name: "Dave" };
// userName => "Dave"
```

```lits
// Multiple renames
let { firstName as name, age as years } = { firstName: "Eve", age: 28 };
// name => "Eve", years => 28
```

##### Advanced Destructuring Patterns

```lits
// Complex nested destructuring with defaults and renaming
let { 
  name as userName = "Guest",
  profile: { 
    age = 0,
    contact: { email as userEmail = "none" }
  },
  settings = { theme: "light" },
  scores as userScores = [],
  ...others
} = { name: "Sam", profile: { contact: {} }};
// userName => "Sam", age => 0, userEmail => "none", etc.
```

```lits
// Combining array and object destructuring
let [{ name }, { age }] = [{ name: "Tina" }, { age: 33 }];
// name => "Tina", age => 33
```

```lits
// Object with array property destructuring
let { name, scores: [one, two] } = { name: "Uma", scores: [85, 92] };
// name => "Uma", one => 85, two => 92
```

##### Array Destructuring

```lits
// Array destructuring
let [, , a, b] = [1, 2, 3, 4];
// a => 3, b => 4

// Array destructuring with defaults
let [one, two = 2] = [1];
// one => 1, two => 2

// Skipping elements
let [x, , z] = [1, 2, 3];
// x => 1, z => 3
```

##### Rest Patterns

```lits
// Array rest pattern
let [head, ...tail] = [1, 2, 3, 4];
// head => 1, tail => [2, 3, 4]

// Object rest pattern  
let { name, ...otherProps } = { name: "John", age: 30, city: "NYC" };
// name => "John", otherProps => { age: 30, city: "NYC" }

// Empty rest patterns
let [only, ...empty] = [1];
// only => 1, empty => []
```

##### Function Parameter Destructuring

```lits
// Basic parameter destructuring
let greet = ({ name }) -> "Hello, " ++ name;
greet({ name: "Pat" });
// => "Hello, Pat"

// With defaults in parameters
let greet2 = ({ name = "friend" }) -> "Hello, " ++ name;
greet2({});
// => "Hello, friend"

// Nested parameter destructuring
let processUser = ({ profile: { name, age }}) -> 
  name ++ " is " ++ str(age);
processUser({ profile: { name: "Quinn", age: 29 }});
// => "Quinn is 29"

// Array parameter destructuring
let processCoords = ([x, y]) -> x + y;
processCoords([3, 4]);
// => 7
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
let y = !:random-int(0, 20);
unless y > 10 then
  "small"
else
  "large"
end;
// => "small" (if y <= 10) or "large" (if y > 10)
```

#### Cond

```lits
let x = !:random-int(0, 20); // Random number between 0 and 19

// Multi-branch conditional
cond
  case x < 5 then "small"
  case x < 10 then "medium"
  case x < 15 then "large"
end ?? "extra large";
// Tests conditions sequentially, returns first truthy match

// Cond with complex conditions
let urgent = !:random-int(0, 2) == 1;
let important = !:random-int(0, 2) == 1;
let priority = cond
  case urgent && important then "critical"
  case urgent then "high"
  case important then "medium"
end ?? "low";
```

#### Switch

```lits
let x = !:random-int(0, 3); // Random number between 0 and 2

// Switch on value
switch x
  case 0 then "zero"
  case 1 then "one"
  case 2 then "two"
end;
// => "zero" (if x = 0), "one" (if x = 1), etc., or null if no match

// Switch with multiple cases
let userInput = "help";
let exit = -> "exiting";
let showHelp = -> "showing help";
let saveData = -> "saving data";

switch userInput
  case "quit" then exit()
  case "help" then showHelp()
  case "save" then saveData()
end;
```

### Loops and Iteration

#### For Comprehensions

```lits
// Simple iteration
for (x in [1, 2, 3, 4]) -> x * 2;
// => [2, 4, 6, 8]

// With filtering (when clause)
for (x in [1, 2, 3, 4] when odd?(x)) -> x * 2;
// => [2, 6]

// With early termination (while clause)
for (x in [1, 2, 3, 4] while x < 3) -> x * 2;
// => [2, 4]

// With let bindings for intermediate calculations
for (x in [1, 2, 3] let doubled = x * 2) -> doubled + 1;
// => [3, 5, 7]

// Multiple iterators
for (x in [1, 2], y in [10, 20]) -> x + y;
// => [11, 21, 12, 22]

// Complex for comprehensions with multiple conditions
for (
  i in range(10) 
  let ii = i ^ 2 
  while ii < 40 
  when ii % 3 == 0,
  j in range(10) 
  when j % 2 == 1
) -> ii + j;

// Using previous bindings in subsequent iterations
for (x in [1, 2], y in [x, 2 * x]) -> x * y;
// => [1, 2, 4, 8]

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
};
// => 120 (factorial of 5)

// Complex loop with multiple variables
loop (items = [1, 2, 3, 4, 5], sum = 0, cnt = 0) -> {
  if empty?(items) then
    { sum: sum, average: sum / cnt }
  else
    recur(rest(items), sum + first(items), cnt + 1)
  end
};
```

### Recursion with Recur

#### Function Recursion

```lits
// Simple recursive function with recur
let factorial = (n) -> {
  if n <= 1 then
    1
  else
    n * recur(n - 1)
  end
};

// Tail-recursive function
let sumToN = (n, acc = 0) -> {
  if zero?(n) then
    acc
  else
    recur(n - 1, acc + n)
  end
};
```

### Error Handling

#### Try/Catch

```lits
// Basic try/catch
let riskyOperation = () -> throw("Something went wrong");
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

// Try-catch for graceful degradation
let parseData = () -> { value: 42 };
let process = (val) -> val * 2;
try
  let { value } = parseData();
  process(value);
catch
  "Using default value"
end;
```

#### Throw

```lits
// Throwing errors
try
  throw("Custom error message")
catch
  "Caught an error"
end;

// Custom error messages in functions
let divide = (a, b) ->
  if zero?(b) then
    throw("Cannot divide by zero")
  else
    a / b
  end;

// Conditional error throwing
let validateAge = (age) ->
  cond
    case age < 0 then throw("Age cannot be negative")
    case age > 150 then throw("Age seems unrealistic")
    case true then age
  end;
```

### Block Expressions

```lits
// Block for grouping expressions
let computeX = () -> 5;
let computeY = () -> 10;
let processResult = (z) -> z * 2;

let result = {
  let x = computeX();
  let y = computeY();
  let z = x * y;
  processResult(z)
};

// Block with side effects
let loadData = () -> [1, 2, 3];
let processData = (data) -> data map -> $ * 2;

{
  write!("Starting process...");
  let data = loadData();
  let processed = processData(data);
  write!("Process completed");
  processed
};
```

### Array and Object Spread

#### Array Spread

```lits
// Spread in array literals
let combined = [1, 2, ...[3, 4, 5], 6];
// => [1, 2, 3, 4, 5, 6]

// Multiple spreads
let start = [1, 2];
let middle = [3, 4];
let stop = [5, 6];
let result = [...start, ...middle, ...stop];
```

#### Object Spread

```lits
// Object spread for merging
let person = {
  name: "John",
  age: 30
};

let employee = {
  ...person,
  id: "E123",
  department: "Engineering"
};
// => { name: "John", age: 30, id: "E123", department: "Engineering" }

// Spread with override
let defaults = {
  name: "Default Name",
  theme: "light",
  active: true
};

let updated = {
  ...defaults,
  name: "Custom Name"  // Override defaults.name
};
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
null ?? "default";          // => "default"
undefined-var ?? "default"; // => "default"
0 ?? "default";             // => 0 (only null/undefined are coalesced)
false ?? "default";         // => false
"" ?? "default";            // => ""
```

### Ternary Operator

```lits
// Conditional expression
let age = !:random-int(10, 60);
let result = age >= 18 ? "adult" : "minor";

// Nested ternary
let score = !:random-int(0, 100);
let category = score >= 90 ? "A" : score >= 80 ? "B" : "C";

// With complex expressions
let isLoggedIn = () -> true;
let hasPermission = () -> true;
let status = isLoggedIn() && hasPermission() ? "authorized" : "unauthorized";
```

## Operators and Precedence

Lits follows a clear operator precedence hierarchy. Understanding precedence helps you write expressions that behave as expected:

### Precedence Table (Highest to Lowest)

1. **Function calls** - `fn(args)`
2. **Array/Object access** - `arr[index]`, `obj.property`
3. **Unary operators** - `not`, `!`, `-` (negation)
4. **Exponentiation** - `^` (right-associative)
5. **Multiplication, Division, Modulo** - `*`, `/`, `%`
6. **Addition, Subtraction** - `+`, `-`
7. **String concatenation** - `++`
8. **Comparison operators** - `<`, `>`, `<=`, `>=`
9. **Equality operators** - `==`, `!=`, `identical?`
10. **Logical AND** - `&&`
11. **Logical OR** - `||`
12. **Null coalescing** - `??`
13. **Ternary conditional** - `condition ? true-value : false-value`

### Examples

```lits
// Comparison and logical operators
5 > 3 && 2 < 4;          // => true
5 > 3 || 2 > 4;          // => true

// Ternary has low precedence
let x = !:random-int(0, 10);
x + 3 > 4 ? 1 : 0;       // => (x + 3) > 4 ? 1 : 0
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
let factorial = n -> n <= 1 ? 1 : n * self(n - 1);

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

### String Processing

```lits
let text = "Hello, World! How are you today?";

// Word count
let wordCount = text
  |> split(_, #"\s+")
  |> count(_);
// => 6

// Uppercase words longer than 4 characters
let longWords = text
  |> split(_, #"\s+")
  |> filter(_, -> count($) > 4)
  |> map(_, upper-case);
// => ["HELLO,", "WORLD!", "TODAY?"]
```

### Data Transformation

```lits
let users = [
  { name: "Alice", age: 30, department: "Engineering" },
  { name: "Bob", age: 25, department: "Marketing" },
  { name: "Charlie", age: 35, department: "Engineering" }
];

// Group by department and get average age
let grouped = users |> group-by(_, "department");
let departmentAges = grouped
  |> entries(_)
  |> map(_, ([dept, people]) -> {
       let ages = people |> map(_, "age");
       let total = ages |> reduce(_, +, 0);
       [dept, total / count(ages)]
     })
  |> (pairs -> zipmap(map(pairs, 0), map(pairs, 1)));
// => { "Engineering": 32.5, "Marketing": 25 }
```

## JavaScript Interoperability

### Using Lits in JavaScript

```javascript
import { Lits } from '@mojir/lits';

const lits = new Lits();

// Basic usage
const result1 = lits.run('+(1, 2, 3)');
console.log(result1); // 6

// Provide JavaScript values
const result2 = lits.run('name ++ " is " ++ str(age)', {
  values: { name: 'John', age: 30 }
});
console.log(result2); // "John is 30"

// Expose JavaScript functions
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
