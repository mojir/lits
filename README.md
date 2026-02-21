# Lits

A functional language with algebraic notation and JavaScript interoperability.

Try it in the [Lits Playground](https://mojir.github.io/lits/).

## Features

- **Pure functional language** - Variables cannot be changed, ensuring predictable behavior and easier reasoning about code
- **Expression-based syntax** - Everything in Lits is an expression that returns a value; there are no statements, making the language highly composable and consistent
- **Fully serializable** - Every value returned from Lits evaluation, including functions and regexps, is serializable as JSON
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
// Defining a function - note that everything returns a value
let square = x -> x * x;

// Using the function
let result = square(5);
// => 25

// Using function as an operator
let squares = [1, 2, 3, 4, 5] map square;
// => [1, 4, 9, 16, 25]

// Using operator as a function
+(1, 2, 3, 4, 5);
// => 15
```

## Expression-Based Language

In Lits, everything is an expression that evaluates to a value. This means:

```lits
// Conditional expressions always return a value
let a = 10;
let result = if a > 0 then "positive" else "non-positive" end;

// Function definitions are expressions that return the function
let add = (a, b) -> a + b;

// Even variable bindings return the bound value
let x = let y = 5;  // x becomes 5

// Blocks are expressions - they return the last expression's value
let value = do
  let temp = 42;
  temp * 2 + 1 // => 85
end;
```
This expression-based design makes Lits highly composable and eliminates the statement/expression distinction found in many other languages.

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
"String with \"escapes\"";

// Booleans
true;
false;

// Functions
(x -> x * 2);                  // Anonymous function
let add = (a, b) -> a + b;     // Named function

// Regular expressions
#"[a-z]+";
#"\d{3}-\d{3}-\d{4}";

// null
null;
```

#### Arrays (General Collections)

Arrays are the primary collection type in Lits, supporting mixed data types:

```lits
// Basic arrays
[1, 2, 3, 4, 5];
["apple", "banana", "orange"];
[true, 42, "mixed types"];

// Nested arrays
[[1, 2], [3, 4], [5, 6]];
[{name: "Alice"}, {name: "Bob"}];

// Array operations
let numbers = [1, 2, 3, 4, 5];
numbers[0];                    // => 1 (indexing)
count(numbers);                // => 5 (length)
first(numbers);                // => 1
last(numbers);                 // => 5
rest(numbers);                 // => [2, 3, 4, 5]

// Functional array operations
numbers map -> $ * 2;      // => [2, 4, 6, 8, 10]
numbers filter odd?;           // => [1, 3, 5]
```

#### Vectors (Number Arrays)

A vector is simply a non-empty array containing only numbers. The `vec` module provides mathematical operations specifically for these number arrays:

```lits
// Import vector and linear algebra modules
let vec = import("vector");
let lin = import("linear-algebra");

// Vectors are just number arrays
[1, 2, 3, 4, 5];              // This is a vector
[3.14, 2.71, 1.41];           // This is also a vector
[1, "hello", 3];               // This is NOT a vector (mixed types)
[];                            // This is NOT a vector (empty)

// Vector creation functions
vec.zeros(5);                  // => [0, 0, 0, 0, 0]
vec.ones(3);                   // => [1, 1, 1]
vec.linspace(0, 10, 5);        // => [0, 2.5, 5, 7.5, 10]
vec.fill(4, 3.14);             // => [3.14, 3.14, 3.14, 3.14]
vec.generate(5, -> $ * 2);     // => [0, 2, 4, 6, 8]

// Vector mathematical operations (use lin module for vector math)
lin.dot([1, 2, 3], [4, 5, 6]);      // => 32 (dot product)
lin.euclidean-norm([3, 4]);         // => 5.0 (Euclidean norm/magnitude)
lin.normalize-l2([3, 4]);           // => [0.6, 0.8] (unit vector)
lin.euclidean-distance([0, 0], [3, 4]); // => 5.0 (Euclidean distance)

// Vector statistical operations (sum, mean, median, prod are core built-ins)
sum([1, 2, 3, 4]);             // => 10
mean([1, 2, 3, 4]);            // => 2.5
median([1, 2, 3, 4, 5]);       // => 3
vec.stdev([1, 2, 3, 4]);       // => 1.29... (standard deviation)
vec.variance([1, 2, 3, 4]);    // => 1.67... (variance)

// Vector analysis (min and max are core built-ins)
min([3, 1, 4, 1, 5]); // => 1
max([3, 1, 4, 1, 5]); // => 5
vec.min-index([3, 1, 4]);      // => 1 (index of minimum)
vec.max-index([3, 1, 4]);      // => 2 (index of maximum)

// Cumulative operations
vec.cumsum([1, 2, 3, 4]);      // => [1, 3, 6, 10]
vec.cumprod([1, 2, 3, 4]);     // => [1, 2, 6, 24]

// Vector predicates
vec.increasing?([1, 1, 2, 3, 4]);          // => true
vec.strictly-increasing?([1, 1, 2, 3, 4]); // => false

// Structural equality works with all vectors
[1, 2, 3] == [1, 2, 3];       // => true
[1, 2] == [1, 2, 3];          // => false
```

#### Matrices (2D Vectors)

A matrix is a 2D array where each row is a vector (non-empty array of numbers) and all rows have the same length. The `mat` module provides linear algebra operations for these structures:

```lits
// Import matrix module
let mat = import("matrix");

// Matrices are 2D number arrays with consistent row lengths
[[1, 2], [3, 4]];              // This is a 2x2 matrix
[[1, 2, 3], [4, 5, 6]];        // This is a 2x3 matrix
[[1, 2], [3, 4, 5]];           // This is NOT a matrix (inconsistent row length)
[[1, "hello"], [3, 4]];        // This is NOT a matrix (contains non-numbers)
[[]];                          // This is NOT a matrix (contains empty row)

// Basic matrix operations
let matrixA = [[1, 2], [3, 4]];
let matrixB = [[5, 6], [7, 8]];

mat.mul(matrixA, matrixB);     // => [[19, 22], [43, 50]] (multiplication)
mat.det(matrixA);              // => -2 (determinant)
mat.inv(matrixA);              // => [[-2, 1], [1.5, -0.5]] (inverse)
mat.trace(matrixA);            // => 5 (trace - sum of diagonal)

// Matrix construction
mat.hilbert(3);                // => 3x3 Hilbert matrix
mat.vandermonde([1, 2, 3]);    // => Vandermonde matrix from vector
mat.band(4, 1, 1);             // => 4x4 band matrix

// Matrix properties and predicates
mat.symmetric?([[1, 2], [2, 1]]);    // => true
mat.invertible?([[1, 2], [3, 4]]);   // => true
mat.square?([[1, 2], [3, 4]]);       // => true
mat.diagonal?([[1, 0], [0, 2]]);     // => true
mat.identity?([[1, 0], [0, 1]]);     // => true

// Advanced matrix operations
mat.adj(matrixA);              // => [[4, -2], [-3, 1]] (adjugate)
mat.cofactor(matrixA);         // => cofactor matrix
mat.minor(matrixA, 0, 1);      // => minor by removing row 0, col 1
mat.frobenius-norm(matrixA);   // => Frobenius norm
mat.one-norm(matrixA);         // => 1-norm (max column sum)
mat.inf-norm(matrixA);         // => infinity norm (max row sum)
mat.max-norm(matrixA);         // => max norm (largest absolute element)

// Matrix analysis
mat.rank(matrixA);             // => matrix rank
```

#### Objects (Maps)

Objects store key-value pairs:

```lits
// Object creation
{ name: "John", age: 30 };
{ "key with spaces": "value", count: 42 };

// Nested objects
{
  person: { name: "Alice", age: 25 },
  scores: [95, 87, 92],
  active: true
};

// Object operations
let user = { name: "Bob", age: 30, city: "NYC" };
get(user, "name");             // => "Bob"
assoc(user, "age", 31);        // => new object with age updated
dissoc(user, "city");          // => new object without city
keys(user);                    // => ["name", "age", "city"]
vals(user);                  // => ["Bob", 30, "NYC"]
```

#### Type Predicates

Lits provides predicate functions to check data types at runtime:

```lits
// Basic type predicates
number?(42);                   // => true
string?("hello");              // => true
boolean?(true);                // => true
function?(x -> x * 2);         // => true
regexp?(#"[a-z]+");            // => true
array?([1, 2, 3]);             // => true
object?({name: "Alice"});      // => true
null?(null);                   // => true

// Specialized array predicates
vector?([1, 2, 3]);            // => true (non-empty number array)
vector?([1, "hello", 3]);      // => false (mixed types)
vector?([]);                   // => false (empty)

matrix?([[1, 2], [3, 4]]);     // => true (2D number array, consistent rows)
matrix?([[1, 2], [3]]);        // => false (inconsistent row lengths)
matrix?([[]]);                 // => false (contains empty row)

// Collection predicates
sequence?([1, 2, 3]);           // => true (sequences: strings and arrays)
sequence?("hello");             // => true
sequence?({a: 1});              // => false

collection?([1, 2, 3]);         // => true (collections: strings, arrays, objects)
collection?("hello");           // => true
collection?({a: 1});            // => true
collection?(42);                // => false
```

#### Type Hierarchy

The type predicates follow a logical hierarchy:

```lits
// If something is a matrix, it's also a vector and an array
let mat = [[1, 2], [3, 4]];
matrix?(mat);                  // => true
vector?(mat);                  // => true (matrix is a special vector)
array?(mat);                   // => true (vector is a special array)

// If something is a vector, it's also an array
let vec = [1, 2, 3];
vector?(vec);                  // => true
array?(vec);                   // => true

// But not all arrays are vectors
let arr = [1, "hello", 3];
array?(arr);                   // => true
vector?(arr);                  // => false (contains non-numbers)
```

Each data type is immutable by design - operations return new values rather than modifying existing ones, ensuring predictable behavior and easier reasoning about code.

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
let y = do
  let x = 20;
  x
end;
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
} = { name: "Sam", profile: { contact: {} } };
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
```

```lits
// Array destructuring with defaults
let [one, two = 2] = [1];
// one => 1, two => 2
```

```lits
// Skipping elements
let [x, , z] = [1, 2, 3];
// x => 1, z => 3
```

##### Rest Patterns

```lits
// Array rest pattern
let [head, ...tail] = [1, 2, 3, 4];
// head => 1, tail => [2, 3, 4]
```

```lits
// Object rest pattern  
let { name, ...otherProps } = { name: "John", age: 30, city: "NYC" };
// name => "John", otherProps => { age: 30, city: "NYC" }
```

```lits
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
```

```lits
// With defaults in parameters
let greet2 = ({ name = "friend" }) -> "Hello, " ++ name;
greet2({});
// => "Hello, friend"
```

```lits
// Nested parameter destructuring
let processUser = ({ profile: { name, age }}) -> 
  name ++ " is " ++ str(age);
processUser({ profile: { name: "Quinn", age: 29 }});
// => "Quinn is 29"
```

```lits
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
let x = 15; // Fixed value for compilation

if x > 10 then
  "large"
else
  "small"
end;
// => "large"

// If without else returns null
if false then "never" end;
// => null

// Unless (inverted if)
let y = 8;
unless y > 10 then
  "small"
else
  "large"
end;
// => "small"
```

#### Cond

```lits
let x = 12;

// Multi-branch conditional
cond
  case x < 5 then "small"
  case x < 10 then "medium"
  case x < 15 then "large"
  case true then "extra large" // default case
end;
// => "large"

// Cond with complex conditions
let urgent = true;
let important = false;
let priority = cond
  case urgent && important then "critical"
  case urgent then "high"
  case important then "medium"
  case true then "low"
end;
// => "high"
```

#### Switch

```lits
let x = 1;

// Switch on value
switch x
  case 0 then "zero"
  case 1 then "one"
  case 2 then "two"
end;
// => "one"

// Switch with multiple cases
let userInput = "help";
let exit = () -> "exiting";
let showHelp = () -> "showing help";
let saveData = () -> "saving data";

switch userInput
  case "quit" then exit()
  case "help" then showHelp()
  case "save" then saveData()
end;
// => "showing help"
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
loop (n = 5, acc = 1) -> do
  if zero?(n) then
    acc
  else
    recur(n - 1, acc * n)
  end
end;
// => 120 (factorial of 5)

// Complex loop with multiple variables
loop (items = [1, 2, 3, 4, 5], acc = 0, cnt = 0) -> do
  if empty?(items) then
    { sum: acc, average: acc / cnt }
  else
    recur(rest(items), acc + first(items), cnt + 1)
  end
end;
```

### Recursion with Recur

#### Function Recursion

```lits
// Simple recursive function with recur
let factorial = (n) -> do
  if n <= 1 then
    1
  else
    n * recur(n - 1)
  end
end;

// Tail-recursive function
let sumToN = (n, acc = 0) -> do
  if zero?(n) then
    acc
  else
    recur(n - 1, acc + n)
  end
end;
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
  process(value)
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

let result = do
  let x = computeX();
  let y = computeY();
  let z = x * y;
  processResult(z)
end;

// Block with side effects
let loadData = () -> [1, 2, 3];
let processData = (data) -> data map -> $ * 2;

do
  write!("Starting process...");
  let data = loadData();
  let processed = processData(data);
  write!("Process completed");
  processed
end
```

### Array and Object Construction

#### Array Construction

```lits
// Array literal
[1, 2, 3, 4];

// Array function
array(1, 2, 3, 4);

// With spread
let small-set = [3, 4, 5];
[1, 2, ...small-set, 6];
// => [1, 2, 3, 4, 5, 6]
```

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
null ?? "default";     // => "default"
0 ?? "default";        // => 0 (only null/undefined are coalesced)
false ?? "default";    // => false
"" ?? "default";       // => ""
```

### Ternary Operator

```lits
// Conditional expression
let age = 25;
let result = age >= 18 ? "adult" : "minor";

// Nested ternary
let score = 85;
let category = score >= 90 ? "A" : score >= 80 ? "B" : "C";

// With complex expressions
let isLoggedIn = () -> true;
let hasPermission = () -> true;
let status = isLoggedIn() && hasPermission() ? "authorized" : "unauthorized";
```

## Variable Names

Lits is generous with variable naming conventions, allowing a wide range of characters that would be invalid in many other programming languages.

### Basic Rules

Variable names in Lits can contain almost any character except for a small set of reserved symbols. The only restrictions are:

**Illegal characters anywhere in a variable name:**
- Parentheses: `(` `)`
- Brackets: `[` `]`
- Braces: `{` `}`
- Quotes: `'` `"` `` ` ``
- Punctuation: `,` `.` `;`
- Whitespace: spaces, newlines, tabs

**Additional restrictions for the first character:**
- Cannot start with digits `0-9`

### Unicode and Emoji Support

Beyond these minimal restrictions, Lits supports Unicode characters, including emojis, in variable names:

```lits
// Unicode characters are welcome
let résultat = 42;
let naïve = "simple approach";
let coöperation = "working together";

// Emojis work too!
let 😅 = "grinning face with sweat";
let 🚀 = "rocket ship";
let result = 😅 ++ " " ++ 🚀;
// => "grinning face with sweat rocket ship"
```

### Quoted Variable Names

For cases where you need to use the normally illegal characters in variable names, Lits supports quoted variable names using single quotes:

```lits
// Variables with spaces and special characters
let 'A strange variable' = 42;
let 'user.name' = "John Doe";
let 'data[0]' = "first element";
let 'function()' = "callable";

// Access them the same way
'A strange variable' + 8;
// => 50
```

### Practical Examples

Here are some examples showcasing the flexibility of Lits variable naming:

```lits
// Mathematical notation with Greek letters (avoiding reserved symbols)
let α = 0.5;
let β = 1.2;
let γ = 2.0;
let Δ = β - α;

// Descriptive names with special characters
let user-name = "alice";
let is-valid? = true;
let counter! = 0;

// Mixed styles
let dataSet₁ = [1, 2, 3];
let dataSet₂ = [4, 5, 6];
let 🔧config = { debug: true };
```

### Important: Operator Spacing

Due to Lits' flexible variable naming, **operators must be separated by whitespace**. This is crucial to understand:

```lits
// This is a variable name, NOT addition!
let x+1 = 42;
let result1 = x+1;  // => 42

// To perform addition, use spaces
let x = 5;
let result2 = x + 1;  // => 6

// More examples of what looks like operations but are actually variable names
let a-b = "subtraction variable";
let c*d = "multiplication variable"; 
let e/f = "division variable";
let g<h = "comparison variable";

// To use these as actual operations, add spaces
let a = 10;
let b = 3;
let a-sum = a + b;      // Addition
let a-diff = a - b;     // Subtraction
let a-prod = a * b;     // Multiplication
let a-quot = a / b;     // Division
let a-comp = a < b;     // Comparison
```

Without whitespace, Lits treats the entire sequence as a single variable identifier. This applies to all operators, including comparison operators, logical operators, and arithmetic operators.

This flexibility allows for expressive and readable code while maintaining the functional programming paradigm that Lits embodies.
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
sales map "regions" map "north"; // => [100, 150]
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
reduce(map(filter([1, 2, 3, 4, 5, 6], odd?), -> $ * $), +, 0);

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
| 7 | `<` `<=` `>` `>=` | Comparison operators | `3 + 2 > 4` → `5 > 4` → `true` |
| 6 | `==` `≠` | Equality operators | `2 * 3 == 6` → `6 == 6` → `true` |
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
let vec = import("vector");
[1, 2, 3] |> map(_, inc) |> vec.sum;  // Evaluates left to right

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
((a + b) * c) > (d && e ? f : g)  // => (5 * 4) > (false ? 10 : 5) = 20 > 5 = true;
```

#### Associativity

Most operators are left-associative, meaning they evaluate from left to right when they have the same precedence:

```lits
10 - 5 - 2;         // => (10 - 5) - 2 = 3 (not 10 - (5 - 2) = 7)
"a" ++ "b" ++ "c";  // => ("a" ++ "b") ++ "c" = "abc"
```

**Exception**: Exponentiation (`^`) is right-associative:
```lits
2 ^ 3 ^ 2           // => 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (not (2 ^ 3) ^ 2 = 64)
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

## Serialization

A unique feature of Lits is that every result from evaluation is fully serializable as JSON, including functions and regular expressions:

```lits
// Functions are serializable
let myFunction = x -> x * 2;

// Regular expressions are serializable  
let myRegex = #"[a-z]+";

// Complex data structures with functions are serializable
let config = {
  transform: x -> x * 3,
  pattern: #"\d+",
  data: [1, 2, 3]
};

// All of these can be serialized to JSON and later deserialized
// back into working Lits values, preserving their functionality
```

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

factorial(5);  // => 120
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
  |> count;
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
let su = import("sequence");
let users = [
  { name: "Alice", age: 30, department: "Engineering" },
  { name: "Bob", age: 25, department: "Marketing" },
  { name: "Charlie", age: 35, department: "Engineering" }
];

// Group by department and get average age
let grouped = users |> su.group-by(_, "department");
let departmentAges = grouped
  |> entries(_)
  |> map(_, ([dept, people]) -> do
       let ages = people |> map(_, "age");
       let total = ages |> reduce(_, +, 0);
       [dept, total / count(ages)]
     end)
  |> (pairs -> zipmap(map(pairs, 0), map(pairs, 1)));
// => { "Engineering": 32.5, "Marketing": 25 }
```

## JavaScript Interoperability

### Entry Points and Bundles

The package provides multiple entry points for different use cases:

```javascript
// Minimal entry — core Lits class, types, and type guards only.
// No modules or reference data. Smallest bundle size.
import { Lits } from '@mojir/lits';

// Full entry — everything from minimal plus all modules,
// reference data, and API helpers (e.g. apiReference, isApiName).
import { Lits, allBuiltinModules, apiReference } from '@mojir/lits/full';

// Individual module entries — import only the modules you need.
import { vectorModule } from '@mojir/lits/modules/vector';
import { matrixModule } from '@mojir/lits/modules/matrix';
import { linearAlgebraModule } from '@mojir/lits/modules/linearAlgebra';
import { gridModule } from '@mojir/lits/modules/grid';
import { randomModule } from '@mojir/lits/modules/random';
import { assertModule } from '@mojir/lits/modules/assert';
import { numberTheoryModule } from '@mojir/lits/modules/numberTheory';
```

To make module functions available in Lits code, pass them to the `Lits` constructor:

```javascript
import { Lits } from '@mojir/lits';
import { vectorModule } from '@mojir/lits/modules/vector';
import { matrixModule } from '@mojir/lits/modules/matrix';

const lits = new Lits({ modules: [vectorModule, matrixModule] });

// Now you can use import("vector") and import("matrix") in Lits code
lits.run('let v = import("vector"); v.dot([1, 2, 3], [4, 5, 6])'); // => 32
```

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
