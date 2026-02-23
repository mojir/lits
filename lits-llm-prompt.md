# Lits Language Reference

You are generating code in **Lits** — a pure functional programming language that runs in JavaScript environments. Every piece of syntax produces a value (there are no statements). All data is immutable. Functions are first-class values.

---

## Critical Rules (Read Before Writing Any Code)

1. **Operators REQUIRE whitespace.** `x+1` is a *variable name*, not an expression. Always write `x + 1`.
2. **Semicolons are separators, not terminators.** In a sequence of expressions, `;` goes *between* them. The last expression in a block is its return value — no trailing semicolon needed.
3. **`let` bindings are immutable.** You cannot reassign a variable. Build new values instead.
4. **Use `self` to recurse inside a lambda.** `self(args)` calls the immediately enclosing function.
5. **Comments:** `// single line` or `/* multi-line */`.

---

## Data Types

| Type | Literal examples |
|------|-----------------|
| number | `42`, `3.14`, `-2.3e-2`, `0xFF`, `0b1010`, `0o77` |
| string | `"hello"`, `"line\nbreak"` |
| boolean | `true`, `false` |
| null | `null` |
| array | `[1, "two", true]`, `[]` |
| object | `{ name: "Alice", age: 30 }`, `{}` |
| function | `(x, y) -> x + y` |
| regexp | `#"[a-z]+"` or `#"pattern"ig` |

**Spread in arrays:** `[1, 2, ...[3, 4], 5]` → `[1, 2, 3, 4, 5]`
**Spread in objects:** `{ ...defaults, name: "Lisa" }`

---

## Global Constants (Always Available)

| Constant | Aliases | Value |
|----------|---------|-------|
| `PI` | `π` | `Math.PI` |
| `E` | `ε` | `Math.E` |
| `PHI` | `φ` | Golden ratio |
| `POSITIVE_INFINITY` | `∞` | `Infinity` |
| `NEGATIVE_INFINITY` | `-∞` | `-Infinity` |
| `NaN` | | `NaN` |
| `MAX_SAFE_INTEGER` | | `9007199254740991` |
| `MIN_SAFE_INTEGER` | | `-9007199254740991` |
| `MAX_VALUE` | | `Number.MAX_VALUE` |
| `MIN_VALUE` | | `Number.MIN_VALUE` |

---

## Variables and Definitions

```lits
// Local binding (immutable, block-scoped)
let x = 42;
let greeting = "Hello, " ++ name;

// Destructuring — object
let { name, age } = person;
let { name as userName = "Guest" } = person;   // rename + default
let { profile: { age } } = person;             // nested

// Destructuring — array
let [head, ...tail] = [1, 2, 3, 4];            // head=1, tail=[2,3,4]
let [, , third] = [1, 2, 3, 4];                // skip elements
```

---

## Functions

```lits
// Full form
let add = (x, y) -> x + y;

// Single argument (parens optional)
let square = x -> x * x;

// Short lambda: $ or $1 = first arg, $2 = second, etc.
let double = -> $ * 2;
let hyp    = -> $1 ^ 2 + $2 ^ 2;

// Rest parameters
let sum-all = (...nums) -> reduce(nums, +, 0);

// Default parameter values
let greet = (name = "World") -> "Hello, " ++ name;

// Destructuring parameters
let greet = ({ name, age }) -> name ++ " is " ++ str(age);

// Recursion — use `self` inside a lambda
let factorial = n -> if n <= 1 then 1 else n * self(n - 1) end;

// Calling functions
add(3, 4)           // => 7
square(5)           // => 25

// apply: call with an array of arguments
apply(add, [3, 4])  // => 7
```

### Data as Functions

Arrays, objects, strings, and numbers can be used as accessor functions:

```lits
let arr = [10, 20, 30];
arr(1)          // => 20  (array as function, 0-based index)
1(arr)          // => 20  (number as function)

let obj = { name: "Alice" };
obj("name")     // => "Alice"  (object as function)
"name"(obj)     // => "Alice"  (string as function)

// Useful with map:
let people = [{ name: "Alice" }, { name: "Bob" }];
people map "name"    // => ["Alice", "Bob"]
```

---

## Operators

### Binary operators (all support both infix and prefix/function form)

| Operator | Description | Infix | Prefix form |
|----------|-------------|-------|-------------|
| `^` | Exponentiation (right-assoc) | `2 ^ 10` | `^(2, 10)` |
| `*` | Multiplication | `3 * 4` | `*(3, 4)` |
| `/` | Division | `10 / 2` | `/(10, 2)` |
| `%` | Remainder (sign of dividend) | `7 % 3` | `%(7, 3)` |
| `+` | Addition (variadic) | `1 + 2` | `+(1, 2, 3)` |
| `-` | Subtraction / negation | `5 - 3` | `-(5, 3)` |
| `<<` | Left bit shift | `1 << 4` | `<<(1, 4)` |
| `>>` | Signed right shift | `16 >> 2` | `>>(16, 2)` |
| `>>>` | Unsigned right shift | `16 >>> 2` | `>>>(16, 2)` |
| `++` | String / sequence concat | `"a" ++ "b"` | `++("a", "b")` |
| `<` | Less than | `a < b` | `<(a, b)` |
| `<=` / `≤` | Less than or equal | `a <= b` | |
| `>` | Greater than | `a > b` | `>(a, b)` |
| `>=` / `≥` | Greater than or equal | `a >= b` | |
| `==` | Structural equality | `a == b` | `==(a, b)` |
| `!=` / `!=` | Not equal | `a != b` | `!=(a, b)` |
| `&` | Bitwise AND (variadic) | `a & b` | `&(a, b)` |
| `xor` | Bitwise XOR (variadic) | `a xor b` | `xor(a, b)` |
| `\|` | Bitwise OR (variadic) | `a \| b` | `\|(a, b)` |
| `&&` | Logical AND (short-circuit) | `a && b` | `&&(a, b)` |
| `\|\|` | Logical OR (short-circuit) | `a \|\| b` | `\|\|(a, b)` |
| `??` | Nullish coalescing | `a ?? b` | `??(a, b)` |
| `\|>` | Pipe | `x \|> f` | |

### Operator precedence (high → low)

`^` → `* / %` → `+ -` → `<< >> >>>` → `++` → `< <= > >=` → `== !=` → `& xor |` → `&& || ??` → `|>` → `? :`

### Ternary operator

```lits
condition ? value-if-true : value-if-false
```

### Pipe operator with placeholder `_`

`_` marks where the left-hand value is inserted:

```lits
[1, 2, 3]
  |> map(_, -> $ ^ 2)
  |> filter(_, odd?)
  |> reduce(_, +, 0)
```

### Partial application with `_`

```lits
let add5 = +(5, _);   // partial: add5(3) => 8
let gt0  = <(0, _);   // partial: gt0(5) => true
```

---

## Special Expressions (Control Flow)

### `if` / `unless`

```lits
if condition then
  expr
else
  expr
end

if condition then expr end          // no else: returns null when false

unless condition then expr end      // negated if
```

### `cond` (multi-branch)

```lits
cond
  case condition1 then expr1
  case condition2 then expr2
end
// returns null if no branch matches
```

### `switch` (equality-based)

```lits
switch value
  case 1 then "one"
  case 2 then "two"
end
// returns null if no case matches
```

### `let` (local binding block)

```lits
let x = 10;
let y = x * 2;
x + y              // => 30  (last expression is the value)
```

### `do` (block)

Group multiple expressions; returns the last one:

```lits
do
  let temp = compute(x);
  transform(temp)
end
```

### `def` (top-level definition)

```lits
def answer = 42;
def double = x -> x * 2;
```

### `for` (comprehension — returns array)

```lits
for (x in [1, 2, 3]) -> x * 2
// => [2, 4, 6]

// With modifiers:
for (
  i in range(10)
  let sq = i ^ 2
  when sq % 3 == 0
  while sq < 50
) -> sq
```

Binding modifiers (all optional, in this order):
- `let name = expr` — local binding per iteration
- `when condition` — skip this iteration if false
- `while condition` — stop entirely if false

Multiple bindings produce a cartesian product:

```lits
for (i in [1, 2], j in [10, 20]) -> i + j
// => [11, 21, 12, 22]
```

### `doseq` (iteration for side effects — returns `null`)

```lits
doseq (x in [1, 2, 3]) -> write!(x)
```

### `loop` / `recur` (tail-recursive loop)

```lits
loop (n = 5, acc = 1) ->
  if n <= 1 then
    acc
  else
    recur(n - 1, acc * n)
  end
// => 120
```

### `try` / `catch` / `throw`

```lits
try
  riskyOperation()
catch(error)
  "Failed: " ++ error.message
end

try expr catch "fallback" end   // catch without binding the error

throw("Something went wrong")   // message must be a string
```

### `and` / `or` / `??`

```lits
a && b && c       // short-circuit AND; returns last truthy or first falsy
a || b || c       // short-circuit OR; returns first truthy or last falsy
a ?? b            // returns a if a != null, else b
```

### `defined?`

```lits
defined?(x)       // true if x is defined in scope, false otherwise
```

### `recur` (tail call in any enclosing function/loop)

```lits
let countdown = n -> do
  write!(n);
  if !(zero?(n)) then recur(n - 1) end
end;
countdown(3)
```

---

## Accessor Shorthands

```lits
obj.key               // property access (same as obj("key"))
arr[0]                // index access (same as arr(0))
data.users[0].name    // chaining
```

---

## Lambda Shorthand

`-> expression` with positional arguments:

```lits
-> $ * 2         // single arg: $ or $1
-> $1 + $2       // two args
-> $1 * $2 + $3  // three args

// Examples:
[1, 2, 3] map (-> $ ^ 2)     // => [4, 9, 16] — wait, map takes (coll, fn)
map([1, 2, 3], -> $ ^ 2)     // => [1, 4, 9]
```

---

## Regexp Shorthand

```lits
#"pattern"        // same as regexp("pattern")
#"pattern"ig      // with flags (i = case-insensitive, g = global)

// No need to escape backslashes:
#"\d+"            // matches one or more digits
```

---

## Built-in Functions

### Math

| Function | Description |
|----------|-------------|
| `inc(x)` | x + 1 (also works on vectors/matrices element-wise) |
| `dec(x)` | x - 1 |
| `+(a, b, ...)` | Addition (variadic) |
| `-(a, b)` / `-(a)` | Subtraction / negation |
| `*(a, b, ...)` | Multiplication |
| `/(a, b)` | Division |
| `%(a, b)` | Remainder (sign of dividend) |
| `mod(a, b)` | Modulo (sign of divisor) |
| `quot(a, b)` | Integer division truncated toward zero |
| `^(a, b)` | Exponentiation |
| `sqrt(x)` | Square root |
| `cbrt(x)` | Cube root |
| `abs(x)` | Absolute value |
| `sign(x)` | -1, 0, or 1 |
| `round(x, decimals?)` | Round to nearest (or to N decimals) |
| `floor(x)` | Largest integer ≤ x |
| `ceil(x)` | Smallest integer ≥ x |
| `trunc(x)` | Truncate toward zero |
| `min(a, b, ...)` / `min(vector)` | Minimum value |
| `max(a, b, ...)` / `max(vector)` | Maximum value |

### Comparison / Logic

| Function | Description |
|----------|-------------|
| `==(a, b, ...)` | Structural equality (deep) |
| `!=(a, b)` / `!=(a, b)` | Not equal |
| `<(a, b, ...)` | Strictly increasing |
| `<=(a, b, ...)` | Non-decreasing |
| `>(a, b, ...)` | Strictly decreasing |
| `>=(a, b, ...)` | Non-increasing |
| `not(x)` | Logical NOT |
| `boolean(x)` | Coerce to boolean |
| `compare(a, b)` | Returns -1, 0, or 1 |
| `identical?(a, b)` | Referential equality |

### String

| Function | Description |
|----------|-------------|
| `str(a, b, ...)` | Concatenate to string (null → "") |
| `number(s)` | Parse string to number |
| `lower-case(s)` | Lowercase |
| `upper-case(s)` | Uppercase |
| `trim(s)` | Remove leading/trailing whitespace |
| `split(s, delimiter, limit?)` | Split into array |
| `join(arr, delimiter)` | Join array into string |
| `blank?(s)` | True if null or whitespace-only |

### Sequence (arrays and strings)

| Function | Description |
|----------|-------------|
| `first(seq)` | First element (null if empty) |
| `second(seq)` | Second element |
| `last(seq)` | Last element |
| `rest(seq)` | All but first (empty array if ≤1) |
| `next(seq)` | All but first, or null if ≤1 |
| `nth(seq, n, not-found?)` | Element at index n |
| `slice(seq, start?, stop?)` | Subsequence (exclusive stop) |
| `push(seq, ...values)` | Add to end (returns new) |
| `pop(seq)` | Remove last (returns new) |
| `reverse(seq)` | Reversed sequence |
| `sort(seq, comparator?)` | Sorted (default: `compare`) |
| `index-of(seq, value)` | Index of value, or null |
| `some(seq, fn)` | First element passing fn, or null |

### Array-specific

| Function | Description |
|----------|-------------|
| `range(b)` / `range(a, b, step?)` | Array of numbers a..b (exclusive) |
| `repeat(value, n)` | Array of value repeated n times |
| `flatten(arr, depth?)` | Flatten nested arrays |
| `mapcat(arr, fn)` | Map then flatten one level |
| `moving-fn(arr, windowSize, fn)` | Apply fn to each sliding window |
| `running-fn(arr, fn)` | Apply fn to cumulative subarrays |

### Collection (arrays, objects, and strings)

| Function | Description |
|----------|-------------|
| `map(coll, fn)` / `map(coll1, coll2, fn)` | Transform elements |
| `filter(coll, fn)` | Keep elements passing fn |
| `reduce(coll, fn, initial)` | Fold to single value |
| `count(coll)` | Number of elements |
| `empty?(coll)` | True if empty or null |
| `not-empty?(coll)` | True if not empty and not null |
| `contains?(coll, key)` | True if key/index/substring exists |
| `get(coll, key, not-found?)` | Value at key (with optional default) |
| `assoc(coll, key, value, ...)` | Set key to value (returns new) |
| `++(a, b, ...)` | Concatenate collections |

### Object

| Function | Description |
|----------|-------------|
| `keys(obj)` | Array of keys |
| `vals(obj)` | Array of values |
| `entries(obj)` | Array of [key, value] pairs |
| `find(obj, key)` | [key, value] pair or null |
| `dissoc(obj, key)` | Copy without key |
| `merge(obj, ...)` | Merge objects (right wins) |
| `merge-with(obj, ..., fn)` | Merge with conflict resolver fn |
| `zipmap(keys, vals)` | Build object from two arrays |
| `select-keys(obj, keys)` | Keep only specified keys |

### Functional

| Function | Description |
|----------|-------------|
| `apply(fn, args-array)` | Call fn with array as args |
| `identity(x)` | Returns x |
| `comp(fn, ...)` | Compose functions (right-to-left) |
| `constantly(x)` | Returns function that always returns x |

### Predicates

| Function | Description |
|----------|-------------|
| `number?(x)` | Is x a number? |
| `integer?(x)` | Is x an integer? |
| `string?(x)` | Is x a string? |
| `boolean?(x)` | Is x a boolean? |
| `null?(x)` | Is x null? |
| `function?(x)` | Is x a function? |
| `array?(x)` | Is x an array? |
| `object?(x)` | Is x an object (map)? |
| `sequence?(x)` | Is x an array or string? |
| `collection?(x)` | Is x an array, object, or string? |
| `regexp?(x)` | Is x a regexp? |
| `vector?(x)` | Is x a vector (array of numbers)? |
| `matrix?(x)` | Is x a matrix (2D array of numbers)? |
| `grid?(x)` | Is x a grid (2D array, uniform row lengths)? |
| `empty?(x)` | Is x empty or null? |
| `not-empty?(x)` | Is x non-empty and non-null? |
| `zero?(x)` | Is x zero? |
| `pos?(x)` | Is x > 0? |
| `neg?(x)` | Is x < 0? |
| `even?(x)` | Is x even? |
| `odd?(x)` | Is x odd? |
| `finite?(x)` | Is x finite? |
| `true?(x)` | Is x exactly true? |
| `false?(x)` | Is x exactly false? |
| `positive-infinity?(x)` | Is x +Infinity? |
| `negative-infinity?(x)` | Is x -Infinity? |

### Regular Expressions

| Function | Description |
|----------|-------------|
| `regexp(pattern, flags?)` | Create regexp |
| `match(text, regexp)` | Returns match array or null |
| `replace(s, regexp, replacement)` | Replace first match |
| `replace-all(s, regexp, replacement)` | Replace all matches |

### Misc

| Function | Description |
|----------|-------------|
| `write!(values...)` | Log values, return last (side effect) |
| `json-parse(s)` | Parse JSON string |
| `json-stringify(x, indent?)` | Serialize to JSON string |
| `epoch->iso-date(ms)` | Milliseconds to ISO date string |
| `iso-date->epoch(s)` | ISO date string to milliseconds |
| `import(path)` | Import module or function (see Modules) |
| `doc(fn)` | Return documentation string |
| `arity(fn)` | Return `{min, max}` arity object |

### Bitwise (core)

| Function | Description |
|----------|-------------|
| `&(a, b, ...)` | Bitwise AND |
| `\|(a, b, ...)` | Bitwise OR |
| `xor(a, b, ...)` | Bitwise XOR |
| `<<(a, b)` | Left shift |
| `>>(a, b)` | Signed right shift |
| `>>>(a, b)` | Unsigned right shift |

---

## Modules

Modules must be explicitly imported before use:

```lits
// Import entire module as an object
let m = import("math");
m.sin(PI)

// Import with destructuring
let { sin, cos } = import("math");
sin(PI)

// Import a single function directly
let sin = import("math.sin");
sin(PI)
```

### Available Modules

**math**: `sin`, `asin`, `sinh`, `asinh`, `cos`, `acos`, `cosh`, `acosh`, `tan`, `atan`, `tanh`, `atanh`, `ln`, `log2`, `log10`, `to-rad`, `to-deg`

**vector**: `monotonic?`, `strictly-monotonic?`, `increasing?`, `decreasing?`, `strictly-increasing?`, `strictly-decreasing?`, `mode`, `min-index`, `max-index`, `sort-indices`, `count-values`, `linspace`, `ones`, `zeros`, `fill`, `generate`, `cumsum`, `cumprod`, `quartiles`, `percentile`, `quantile`, `histogram`, `ecdf`, `outliers?`, `outliers`, `bincount`, `winsorize`, `mse`, `rmse`, `mae`, `smape`

**sequence**: `position`, `last-index-of`, `shift`, `splice`, `sort-by`, `take`, `take-last`, `take-while`, `drop`, `drop-last`, `drop-while`, `unshift`, `distinct`, `remove`, `remove-at`, `split-at`, `split-with`, `frequencies`, `group-by`, `partition`, `partition-all`, `partition-by`, `ends-with?`, `starts-with?`, `interleave`, `interpose`

**collection**: `get-in`, `assoc-in`, `update`, `update-in`, `filteri`, `mapi`, `reducei`, `reduce-right`, `reducei-right`, `reductions`, `reductionsi`, `not-empty`, `every?`, `any?`, `not-any?`, `not-every?`

**functional**: `juxt`, `complement`, `every-pred`, `some-pred`, `fnull`

**string**: `string-repeat`, `from-char-code`, `to-char-code`, `trim-left`, `trim-right`, `split-lines`, `pad-left`, `pad-right`, `template`, `encode-base64`, `decode-base64`, `encode-uri-component`, `decode-uri-component`, `capitalize`

**bitwise**: `bit-not`, `bit-and-not`, `bit-flip`, `bit-set`, `bit-clear`, `bit-test`

**random**: `random!`, `random-int!`, `random-int-inclusive!`, `random-float!`, `random-boolean!`, `random-item!`, `random-sample!`, `random-sample-unique!`, `shuffle!`, `random-normal!`, `random-exponential!`, `random-binomial!`, `random-poisson!`, `random-gamma!`, `random-pareto!`, `uuid!`, `random-char!`, `random-string!`, `random-id!`, `random-color!`

**grid**: `every?`, `some?`, `every-row?`, `some-row?`, `every-col?`, `some-col?`, `row`, `col`, `shape`, `fill`, `generate`, `reshape`, `transpose`, `flip-h`, `flip-v`, `rotate`, `reverse-rows`, `reverse-cols`, `slice`, `slice-rows`, `slice-cols`, `splice-rows`, `splice-cols`, `concat-rows`, `concat-cols`, `map`, `mapi`, `reduce`, `reducei`, `push-rows`, `unshift-rows`, `pop-row`, `shift-row`, `push-cols`, `unshift-cols`, `pop-col`, `shift-col`, `from-array`

**matrix**: `mul`, `det`, `inv`, `adj`, `cofactor`, `minor`, `trace`, `symmetric?`, `triangular?`, `upper-triangular?`, `lower-triangular?`, `diagonal?`, `square?`, `orthogonal?`, `identity?`, `invertible?`, `hilbert`, `vandermonde`, `band`, `banded?`, `rank`, `frobenius-norm`, `one-norm`, `inf-norm`, `max-norm`

**linear-algebra**: `rotate2d`, `rotate3d`, `reflect`, `refract`, `lerp`, `dot`, `cross`, `normalize-minmax`, `normalize-robust`, `normalize-zscore`, `normalize-l1`, `normalize-l2`, `normalize-log`, `angle`, `projection`, `orthogonal?`, `parallel?`, `collinear?`, `cosine-similarity`, `euclidean-distance`, `euclidean-norm`, `manhattan-distance`, `manhattan-norm`, `hamming-distance`, `hamming-norm`, `chebyshev-distance`, `chebyshev-norm`, `minkowski-distance`, `minkowski-norm`, `cov`, `corr`, `spearman-corr`, `pearson-corr`, `kendall-tau`, `autocorrelation`, `cross-correlation`, `rref`, `solve`, `to-polar`, `from-polar`

**number-theory**: `coprime?`, `divisible-by?`, `gcd`, `lcm`, `multinomial`, `amicable?`, `euler-totient`, `mobius`, `mertens`, `sigma`, `carmichael-lambda`, `cartesian-product`, `perfect-power`, `mod-exp`, `mod-inv`, `extended-gcd`, `chinese-remainder`, `stirling-first`, `stirling-second`

**assert**: `assert`, `assert=`, `assert!=`, `assert-gt`, `assert-gte`, `assert-lt`, `assert-lte`, `assert-true`, `assert-false`, `assert-truthy`, `assert-falsy`, `assert-null`, `assert-throws`, `assert-throws-error`, `assert-not-throws`, `assert-array`, `assert-boolean`, `assert-collection`, `assert-function`, `assert-grid`, `assert-integer`, `assert-matrix`, `assert-number`, `assert-object`, `assert-regexp`, `assert-sequence`, `assert-string`, `assert-vector`

---

## Common Patterns

### Factorial (recursion with `self`)

```lits
let factorial = n ->
  if n <= 1 then 1 else n * self(n - 1) end;

factorial(5)   // => 120
```

### Tail-recursive loop with `loop` / `recur`

```lits
loop (n = 5, acc = 1) ->
  if n <= 1 then
    acc
  else
    recur(n - 1, acc * n)
  end
// => 120
```

### Pipeline with `|>` and `_`

```lits
range(10)
  |> map(_, -> $ ^ 2)
  |> filter(_, odd?)
  |> reduce(_, +, 0)
// => 1 + 9 + 25 + 49 + 81 = 165
```

### Immutable state update with `assoc`

```lits
let state = { score: 0, level: 1 };
let next-state = assoc(state, "score", state.score + 10);
// state is unchanged; next-state has score: 10
```

### Nested update

```lits
let user = { profile: { age: 30 } };
let older = assoc(user, "profile", assoc(user.profile, "age", 31));
```

### For comprehension

```lits
for (i in range(1, 4), j in range(1, 4) when i != j) ->
  [i, j]
// => [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
```

### Destructuring in a function

```lits
let distance = ({ x: x1, y: y1 }, { x: x2, y: y2 }) ->
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2);

distance({ x: 0, y: 0 }, { x: 3, y: 4 })  // => 5
```

### Error handling

```lits
let safe-divide = (a, b) ->
  try
    if b == 0 then throw("Division by zero") end;
    a / b
  catch(error)
    error.message
  end;

safe-divide(10, 2)   // => 5
safe-divide(10, 0)   // => "Division by zero"
```

### Using a module

```lits
let { sin, cos } = import("math");

let unit-circle-point = theta ->
  { x: cos(theta), y: sin(theta) };

unit-circle-point(PI / 4)
```

### `reduce` for aggregation

```lits
let words = ["the", "quick", "brown", "fox"];
let freq = reduce(
  words,
  (acc, word) -> assoc(acc, word, (acc[word] ?? 0) + 1),
  {}
);
```

### Higher-order composition

```lits
let pipeline = comp(
  -> $ * 2,
  -> $ + 1,
  -> $ ^ 2
);
// comp is right-to-left: square, then +1, then *2
pipeline(3)   // => (3^2 + 1) * 2 = 20
```
