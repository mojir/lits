Lits is a lexically scoped pure functional language algrebraic notation.

All operators can be used as functions and all functions that can take two parameters can be used as operator.
e.g.
```
1 = 2 // false
=(1, 2) // false
```

All built in functions that take two or more parameters can be used as an operator
e.g.
```
max(2, 4) // 4
2 max 4 // 4
```

Functions that take one parameter can NOT be used as an operator!

All operators must be surrounded with whitespaces.

There is one operator that takes three parameters. The conditional operator ?
E.g.
`x > 10 ? "Big" : "Small"`

The conditional operator has the lowest precedence.

Lits favor subject first in the built in functions
E.g.
* Lits: `filter([1, 2, 3], odd?)`
* Clojure: `(filter odd? [1, 2, 3])`

This is due to the heavy use of functions as operator in Lits.
`[1, 2, 3] filter odd?`
reads better than `odd? filter [1, 2, 3]`

## There are no statements, everything is an expression
expressions are separated with `;`

## Special expressions:
### Lambda functions
```
(x, y) -> x + y // standard form
-> $1 + $2 // short form
x -> x ** 3 // standard form (parentheses optional if one arg)
-> $ ** 4 // short form ($ is the same as $1, cannot mix $ with $2, then $1 must be used.)
```

### If
```
if (x > 10) {
  "large"
} // null is returned if x <= 10
```

#### If with else block
```
if (x > 10)
  "large"
else
  "small"
```

* Both the true-statement and the else-statement can be a block.
* Note. Anywhere an expression is expected, a block can always be used.
E.g.
```
if (x > 10) {
  let result = "large"
  result
} else {
  let result = "small"
  result
}
```

### unless
like if, but reversed logic

### let
```
let x := 10; // binds x to 10 in current scope.
```
#### Destructuring
```
let [a, b] = [1, 2]; // a = 1, b = 2
let { a: { b } } = { a: { b: 1 } }; // b = 2
```
#### Default values
```
let { a = 2} = {}; // a = 2
```

### function
```
function foo(a, b) {
  let a2 = a ** 2;
  let b2 = b ** 2;
  a2 + b2
} // binds foo to function in current scope. (Block scope)
```

### try
```
try
  xxx
catch (error)
  "oops: " ++ error.message
```

#### without binding error
```
try
  xxx
catch  // without binding error
  "oops"
```

### switch
```
switch x
  case 0 then "zero"
  case 1 then "one"
  case 2 then "two"
end // returns null if no match - there is no such thing as default
```

### cond
```
cond
  case val < 5 then "S"
  case val < 10 then "M"
  case val < 15 then "L"
end ?? "No match"
```

### for - list comprehension
```
for (x in [0, 1, 2, 3, 4, 5], let y = x * 3, while even?(y)) y
```
for (
  x of [1, 2, 3];
  y of [1, 2, 3], when x <= y;
  z of [1, 2, 3]
) {
  [x, y, z]
}

## Data types.

Number // `42.1` `0xffff` `0b1100`  `0o77` `-2.3e-2`
String // "Surrounded with double quotes
can be multiline"
Boolean // `true` `false`
Null // `null`
Object // `{ a: 1, "b": 2 } // keys are always strings, quotes are optional`
Array // `[1, 2, 3]`

## Other features
* Structural equality checks
* Operators must be separated with whitespace
* unquoted Symbol names can contain any character except:
  '(', ')', '[', ']', '{', '}', '\'', '"', '`', ',', '.', ';', ' ', '\n', '\r', '\t',
* First character of symbol cannot be a number
* Last character of symbol cannot be a `:`
* quoted symbols can contain any character. E.g.
`let '1 strange symbol name:' = 1;`
* semicolons are used as expression delimiter
* let and function expressions are expressions.
This is valid, though quite strange: `let a = let b = 2` // both a and be are now 2
* variables cannot be changed.
I.e.
```
let a = 10;
let a = 20; // error
```

* Variables can be shadowed in inner scope
let a = 10;
let b = {
  let a = 20;
  a
};
// b is now 20

Lits functions supports destructuring.
E.g.
function foo({items: [firstItem] = []} = { items := [ "Ball", "Shoe" ] }) {
  ...
}

* let and function can be preceeded with `export`.
```
export let foo := 1;
```
or
```
export function foo() { 10 };
```
This will store foo in the global context.

## Rest / Spread syntax
function foo(...x)
  +(...x)
end;

## Partial functions
Whenever calling a function with _ as an argument, a partial function is returned.
E.g.
```
let filterOdd := filter(_, odd?);
filterOdd([1, 2, 3]] // [1, 3]
```

## Pipe operator
example:
```
range(10)
  |> map(_, -> $ ^ 2)
  |> filter(_, odd?)
  |> reduce(_, +, 0)
  |> sqrt
  |> round(_, 2)
```

`|>` requires right operand to be function that accepts one parameter

Lits api:
```
export interface Lits {
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

All builtin functions:
    "count",
    "get",
    "get-in",
    "contains?",
    "assoc",
    "assoc-in",
    "++",
    "not-empty",
    "every?",
    "not-every?",
    "any?",
    "not-any?",
    "update",
    "update-in",
    "range",
    "repeat",
    "flatten",
    "mapcat",
    "nth",
    "push",
    "pop",
    "unshift",
    "shift",
    "slice",
    "splice",
    "reductions",
    "reduce",
    "reduce-right",
    "map",
    "filter",
    "position",
    "index-of",
    "last-index-of",
    "some",
    "reverse",
    "first",
    "second",
    "last",
    "rest",
    "next",
    "take",
    "take-last",
    "take-while",
    "drop",
    "drop-last",
    "drop-while",
    "sort",
    "sort-by",
    "distinct",
    "remove",
    "remove-at",
    "split-at",
    "split-with",
    "frequencies",
    "group-by",
    "partition",
    "partition-all",
    "partition-by",
    "starts-with?",
    "ends-with?",
    "interleave",
    "interpose",
    "+",
    "-",
    "*",
    "/",
    "~",
    "mod",
    "rem",
    "quot",
    "inc",
    "dec",
    "sqrt",
    "cbrt",
    "^",
    "round",
    "trunc",
    "floor",
    "ceil",
    "min",
    "max",
    "abs",
    "sign",
    "ln",
    "log2",
    "log10",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
    "asinh",
    "acosh",
    "atanh",
    "apply",
    "identity",
    "comp",
    "constantly",
    "juxt",
    "complement",
    "every-pred",
    "some-pred",
    "fnull",
    "≠",
    "=",
    "<",
    ">",
    "≤",
    "≥",
    "!",
    "write!",
    "iso-date->epoch",
    "epoch->iso-date",
    "boolean",
    "compare",
    "identical?",
    "json-parse",
    "json-stringify",
    "dissoc",
    "keys",
    "vals",
    "entries",
    "find",
    "merge",
    "merge-with",
    "zipmap",
    "select-keys",
    "boolean?",
    "null?",
    "number?",
    "string?",
    "function?",
    "integer?",
    "array?",
    "object?",
    "coll?",
    "seq?",
    "regexp?",
    "zero?",
    "pos?",
    "neg?",
    "even?",
    "odd?",
    "finite?",
    "nan?",
    "negative-infinity?",
    "positive-infinity?",
    "false?",
    "true?",
    "empty?",
    "not-empty?",
    "vector?",
    "matrix?",
    "grid?",
    "regexp",
    "match",
    "replace",
    "replace-all",
    "string-repeat",
    "str",
    "number",
    "lower-case",
    "upper-case",
    "trim",
    "trim-left",
    "trim-right",
    "pad-left",
    "pad-right",
    "split",
    "split-lines",
    "template",
    "to-char-code",
    "from-char-code",
    "encode-base64",
    "decode-base64",
    "encode-uri-component",
    "decode-uri-component",
    "join",
    "capitalize",
    "blank?",
    "<<",
    ">>",
    ">>>",
    "bit-not",
    "&",
    "bit-and-not",
    "|",
    "xor",
    "bit-flip",
    "bit-clear",
    "bit-set",
    "bit-test",
    "assert",
    "assert!=",
    "assert=",
    "assert-gt",
    "assert-lt",
    "assert-gte",
    "assert-lte",
    "assert-true",
    "assert-false",
    "assert-truthy",
    "assert-falsy",
    "assert-null",
    "assert-throws",
    "assert-throws-error",
    "assert-not-throws",
    "vec:mean",
    "vec:moving-mean",
    "vec:centered-moving-mean",
    "vec:running-mean",
    "vec:geometric-mean",
    "vec:moving-geometric-mean",
    "vec:centered-moving-geometric-mean",
    "vec:running-geometric-mean",
    "vec:harmonic-mean",
    "vec:moving-harmonic-mean",
    "vec:centered-moving-harmonic-mean",
    "vec:running-harmonic-mean",
    "vec:median",
    "vec:moving-median",
    "vec:centered-moving-median",
    "vec:running-median",
    "vec:variance",
    "vec:moving-variance",
    "vec:centered-moving-variance",
    "vec:running-variance",
    "vec:sample-variance",
    "vec:moving-sample-variance",
    "vec:centered-moving-sample-variance",
    "vec:running-sample-variance",
    "vec:stdev",
    "vec:moving-stdev",
    "vec:centered-moving-stdev",
    "vec:running-stdev",
    "vec:sample-stdev",
    "vec:moving-sample-stdev",
    "vec:centered-moving-sample-stdev",
    "vec:running-sample-stdev",
    "vec:iqr",
    "vec:moving-iqr",
    "vec:centered-moving-iqr",
    "vec:running-iqr",
    "vec:sum",
    "vec:moving-sum",
    "vec:centered-moving-sum",
    "vec:running-sum",
    "vec:prod",
    "vec:moving-prod",
    "vec:centered-moving-prod",
    "vec:running-prod",
    "vec:min",
    "vec:moving-min",
    "vec:centered-moving-min",
    "vec:running-min",
    "vec:max",
    "vec:moving-max",
    "vec:centered-moving-max",
    "vec:running-max",
    "vec:span",
    "vec:moving-span",
    "vec:centered-moving-span",
    "vec:running-span",
    "vec:skewness",
    "vec:moving-skewness",
    "vec:centered-moving-skewness",
    "vec:running-skewness",
    "vec:sample-skewness",
    "vec:moving-sample-skewness",
    "vec:centered-moving-sample-skewness",
    "vec:running-sample-skewness",
    "vec:excess-kurtosis",
    "vec:moving-excess-kurtosis",
    "vec:centered-moving-excess-kurtosis",
    "vec:running-excess-kurtosis",
    "vec:kurtosis",
    "vec:moving-kurtosis",
    "vec:centered-moving-kurtosis",
    "vec:running-kurtosis",
    "vec:sample-excess-kurtosis",
    "vec:moving-sample-excess-kurtosis",
    "vec:centered-moving-sample-excess-kurtosis",
    "vec:running-sample-excess-kurtosis",
    "vec:sample-kurtosis",
    "vec:moving-sample-kurtosis",
    "vec:centered-moving-sample-kurtosis",
    "vec:running-sample-kurtosis",
    "vec:rms",
    "vec:moving-rms",
    "vec:centered-moving-rms",
    "vec:running-rms",
    "vec:mad",
    "vec:moving-mad",
    "vec:centered-moving-mad",
    "vec:running-mad",
    "vec:medad",
    "vec:moving-medad",
    "vec:centered-moving-medad",
    "vec:running-medad",
    "vec:gini-coefficient",
    "vec:moving-gini-coefficient",
    "vec:centered-moving-gini-coefficient",
    "vec:running-gini-coefficient",
    "vec:entropy",
    "vec:moving-entropy",
    "vec:centered-moving-entropy",
    "vec:running-entropy",
    "vec:monotonic?",
    "vec:strictly-monotonic?",
    "vec:increasing?",
    "vec:decreasing?",
    "vec:strictly-increasing?",
    "vec:strictly-decreasing?",
    "vec:mode",
    "vec:min-index",
    "vec:max-index",
    "vec:sort-indices",
    "vec:count-values",
    "vec:linspace",
    "vec:ones",
    "vec:zeros",
    "vec:fill",
    "vec:generate",
    "vec:cumsum",
    "vec:cumprod",
    "vec:quartiles",
    "vec:percentile",
    "vec:quantile",
    "vec:histogram",
    "vec:ecdf",
    "vec:outliers?",
    "vec:outliers",
    "vec:bincount",
    "vec:winsorize",
    "vec:mse",
    "vec:rmse",
    "vec:mae",
    "vec:smape",
    "lin:dot",
    "lin:cross",
    "lin:normalize-minmax",
    "lin:normalize-zscore",
    "lin:normalize-robust",
    "lin:normalize-l1",
    "lin:normalize-l2",
    "lin:normalize-log",
    "lin:angle",
    "lin:projection",
    "lin:collinear?",
    "lin:parallel?",
    "lin:orthogonal?",
    "lin:cosine-similarity",
    "lin:euclidean-distance",
    "lin:euclidean-norm",
    "lin:manhattan-distance",
    "lin:manhattan-norm",
    "lin:hamming-distance",
    "lin:hamming-norm",
    "lin:chebyshev-distance",
    "lin:chebyshev-norm",
    "lin:minkowski-distance",
    "lin:minkowski-norm",
    "lin:cov",
    "lin:corr",
    "lin:spearman-corr",
    "lin:pearson-corr",
    "lin:kendall-tau",
    "lin:autocorrelation",
    "lin:cross-correlation",
    "lin:rref",
    "lin:solve",
    "mat:mul",
    "mat:det",
    "mat:inv",
    "mat:adj",
    "mat:cofactor",
    "mat:minor",
    "mat:trace",
    "mat:symmetric?",
    "mat:triangular?",
    "mat:upper-triangular?",
    "mat:lower-triangular?",
    "mat:diagonal?",
    "mat:square?",
    "mat:orthogonal?",
    "mat:identity?",
    "mat:invertible?",
    "mat:hilbert",
    "mat:vandermonde",
    "mat:band",
    "mat:banded?",
    "mat:rank",
    "mat:frobenius-norm",
    "mat:1-norm",
    "mat:inf-norm",
    "mat:max-norm",
    "nth:abundant-seq",
    "nth:abundant-take-while",
    "nth:abundant-nth",
    "nth:abundant?",
    "nth:arithmetic-seq",
    "nth:arithmetic-take-while",
    "nth:arithmetic-nth",
    "nth:arithmetic?",
    "nth:bell-seq",
    "nth:bell-take-while",
    "nth:bell-nth",
    "nth:bell?",
    "nth:bernoulli-seq",
    "nth:bernoulli-take-while",
    "nth:bernoulli-nth",
    "nth:catalan-seq",
    "nth:catalan-take-while",
    "nth:catalan-nth",
    "nth:catalan?",
    "nth:collatz-seq",
    "nth:composite-seq",
    "nth:composite-take-while",
    "nth:composite-nth",
    "nth:composite?",
    "nth:deficient-seq",
    "nth:deficient-take-while",
    "nth:deficient-nth",
    "nth:deficient?",
    "nth:factorial-seq",
    "nth:factorial-take-while",
    "nth:factorial-nth",
    "nth:factorial?",
    "nth:fibonacci-seq",
    "nth:fibonacci-take-while",
    "nth:fibonacci-nth",
    "nth:fibonacci?",
    "nth:geometric-seq",
    "nth:geometric-take-while",
    "nth:geometric-nth",
    "nth:geometric?",
    "nth:golomb-seq",
    "nth:golomb-take-while",
    "nth:golomb-nth",
    "nth:golomb?",
    "nth:happy-seq",
    "nth:happy-take-while",
    "nth:happy-nth",
    "nth:happy?",
    "nth:juggler-seq",
    "nth:look-and-say-seq",
    "nth:look-and-say-take-while",
    "nth:look-and-say-nth",
    "nth:look-and-say?",
    "nth:lucas-seq",
    "nth:lucas-take-while",
    "nth:lucas-nth",
    "nth:lucas?",
    "nth:lucky-seq",
    "nth:lucky-take-while",
    "nth:lucky-nth",
    "nth:lucky?",
    "nth:mersenne-seq",
    "nth:mersenne-take-while",
    "nth:mersenne-nth",
    "nth:mersenne?",
    "nth:padovan-seq",
    "nth:padovan-take-while",
    "nth:padovan-nth",
    "nth:padovan?",
    "nth:partition-seq",
    "nth:partition-take-while",
    "nth:partition-nth",
    "nth:partition?",
    "nth:pell-seq",
    "nth:pell-take-while",
    "nth:pell-nth",
    "nth:pell?",
    "nth:perfect-seq",
    "nth:perfect-take-while",
    "nth:perfect-nth",
    "nth:perfect?",
    "nth:perfect-square-seq",
    "nth:perfect-square-take-while",
    "nth:perfect-square-nth",
    "nth:perfect-square?",
    "nth:perfect-cube-seq",
    "nth:perfect-cube-take-while",
    "nth:perfect-cube-nth",
    "nth:perfect-cube?",
    "nth:perfect-power-seq",
    "nth:perfect-power-take-while",
    "nth:perfect-power-nth",
    "nth:perfect-power?",
    "nth:polygonal-seq",
    "nth:polygonal-take-while",
    "nth:polygonal-nth",
    "nth:polygonal?",
    "nth:prime-seq",
    "nth:prime-take-while",
    "nth:prime-nth",
    "nth:prime?",
    "nth:recaman-seq",
    "nth:recaman-take-while",
    "nth:recaman-nth",
    "nth:recaman?",
    "nth:sylvester-seq",
    "nth:sylvester-take-while",
    "nth:sylvester-nth",
    "nth:sylvester?",
    "nth:thue-morse-seq",
    "nth:thue-morse-take-while",
    "nth:thue-morse-nth",
    "nth:thue-morse?",
    "nth:tribonacci-seq",
    "nth:tribonacci-take-while",
    "nth:tribonacci-nth",
    "nth:tribonacci?",
    "nth:count-combinations",
    "nth:combinations",
    "nth:count-derangements",
    "nth:derangements",
    "nth:divisors",
    "nth:count-divisors",
    "nth:proper-divisors",
    "nth:count-proper-divisors",
    "nth:factorial",
    "nth:partitions",
    "nth:count-partitions",
    "nth:permutations",
    "nth:count-permutations",
    "nth:power-set",
    "nth:count-power-set",
    "nth:prime-factors",
    "nth:count-prime-factors",
    "nth:distinct-prime-factors",
    "nth:count-distinct-prime-factors",
    "nth:coprime?",
    "nth:divisible-by?",
    "nth:gcd",
    "nth:lcm",
    "nth:multinomial",
    "nth:amicable?",
    "nth:euler-totient",
    "nth:mobius",
    "nth:mertens",
    "nth:sigma",
    "nth:carmichael-lambda",
    "nth:cartesian-product",
    "nth:perfect-power",
    "nth:mod-exp",
    "nth:mod-inv",
    "nth:extended-gcd",
    "nth:chinese-remainder",
    "nth:stirling-first",
    "nth:stirling-second",
    "grid:every?",
    "grid:some?",
    "grid:every-row?",
    "grid:some-row?",
    "grid:every-col?",
    "grid:some-col?",
    "grid:row",
    "grid:col",
    "grid:shape",
    "grid:generate",
    "grid:reshape",
    "grid:transpose",
    "grid:flip-h",
    "grid:flip-v",
    "grid:rotate",
    "grid:reverse-rows",
    "grid:reverse-cols",
    "grid:slice",
    "grid:slice-rows",
    "grid:slice-cols",
    "grid:splice-rows",
    "grid:splice-cols",
    "grid:concat-rows",
    "grid:concat-cols",
    "grid:map",
    "grid:mapi",
    "grid:reduce",
    "grid:reducei",
    "grid:push-rows",
    "grid:unshift-rows",
    "grid:pop-row",
    "grid:shift-row",
    "grid:push-cols",
    "grid:unshift-cols",
    "grid:pop-col",
    "grid:shift-col",
    "grid:from-array",
    "!:random",
    "!:random-int",
    "!:random-int-inclusive",
    "!:random-float",
    "!:random-boolean",
    "!:random-item",
    "!:random-sample-unique",
    "!:random-sample",
    "!:shuffle",
    "!:random-normal",
    "!:random-exponential",
    "!:random-binomial",
    "!:random-poisson",
    "!:random-gamma",
    "!:random-pareto",
    "!:uuid",
    "!:random-char",
    "!:random-string",
    "!:random-id",
    "!:random-color",
    "array",
    "object",
    "&&",
    "||",
    "let",
    "function",
    "try",
    "throw",
    "if",
    "unless",
    "cond",
    "switch",
    "do",

/*******************************************
 *                 A game                  *
 *******************************************/

// Functional Text Adventure Game in Lits

// Define locations
let locations = {
  forest: {
    description: "You are in a dense forest. Light filters through the leaves above.",
    exits: { north: "cave", east: "river", south: "meadow" }
  },
  cave: {
    description: "You stand in a dark cave. Water drips from stalactites overhead.",
    exits: { south: "forest", east: "tunnel" },
    items: ["torch"]
  },
  river: {
    description: "A swift river flows from the mountains. The water is crystal clear.",
    exits: { west: "forest", north: "waterfall" },
    items: ["fishing rod"]
  },
  meadow: {
    description: "A peaceful meadow stretches before you, filled with wildflowers.",
    exits: { north: "forest", east: "cottage" },
    items: ["flowers"]
  },
  waterfall: {
    description: "A magnificent waterfall cascades down from high cliffs.",
    exits: { south: "river" },
    items: ["shiny stone"]
  },
  tunnel: {
    description: "A narrow tunnel leads deeper into the mountain.",
    exits: { west: "cave", east: "treasure room" }
  },
  "treasure room": {
    description: "A small chamber glittering with treasure!",
    exits: { west: "tunnel" },
    items: ["gold key", "ancient map", "jeweled crown"]
  },
  cottage: {
    description: "A cozy cottage with a smoking chimney stands here.",
    exits: { west: "meadow" },
    items: ["bread"]
  }
};

// Define game state
let initial-state = {
  current-location: "forest",
  inventory: [],
  visited: {},
  game-over: false,
  moves: 0,
  light-source: false
};

// Helper functions
function has-item?(state, item) {
  contains?(state.inventory, item);
};

function location-has-item?(location, item) {
  contains?(get(location, "items", []), item);
};

function describe-location(state) {
  let location = get(locations, state.current-location);
  let description = location.description;

  // Add visited status
  let visited-status = if (get(state.visited, state.current-location, 0) > 1) {
    "You've been here before."
  } else {
    "This is your first time here."
  };

  // Check if location has items
  let items-desc = if (!(empty?(get(location, "items", [])))) {
    "You see: " ++ join(location.items, ", ")
  } else {
    ""
  };

  // Describe exits
  let exits = keys(location.exits) join ", ";
  let exits-desc = "Exits: " ++ exits;

  // Join all descriptions
  filter([description, visited-status, items-desc, exits-desc], -> !(empty?($))) join "\n"
};

function get-location-items(state) {
  let location = get(locations, state.current-location);
  get(location, "items", [])
};

// Game actions
function move(state, direction) {
  let location = get(locations, state.current-location);
  let exits = get(location, "exits", {});

  // Check if direction is valid
  if (contains?(exits, direction)) {
    let new-location = get(exits, direction);
    let is-dark = new-location == "tunnel" || new-location == "treasure room";

    // Check if player has light source for dark areas
    if (is-dark && !(state.light-source)) {
      [state, "It's too dark to go that way without a light source."]
    } else {
      let new-visited = assoc(
        state.visited,
        new-location,
        inc(state.visited["new-location"] ?? 0)
      );
      let new-state = assoc(
        assoc(
          assoc(state, "current-location", new-location),
          "visited",
          new-visited
        ),
        "moves",
        state.moves + 1
      );

      [new-state, "You move " ++ direction ++ " to the " ++ new-location ++ "."]
    }
  } else {
    [state, "You can't go that way."]
  }
};

function take!(state, item) {
  let items = get-location-items(state);

  if (contains?(items, item)) {
    let location = get(locations, state.current-location);
    let new-location-items = filter(items, -> $ ≠ item);
    let new-inventory = push(state.inventory, item);

    // Update game state
    let new-locations = assoc(
      locations,
      state.current-location,
      assoc(location, "items", new-location-items)
    );

    // Special case for torch
    let has-light = item == "torch" || state.light-source;

    // Update locations and state
    let locations = new-locations;
    let new-state = assoc(
      assoc(
        assoc(state, "inventory", new-inventory),
        "light-source", has-light
      ),
      "moves",
      state.moves + 1
    );
    [new-state, "You take the " ++ item ++ "."]
  } else {
    [state, "There is no " ++ item ++ " here."]
  }
};

function drop!(state, item) {
  if (has-item?(state, item)) {
    let location = get(locations, state.current-location);
    let location-items = get(location, "items", []);
    let new-location-items = push(location-items, item);
    let new-inventory = filter(-> $ ≠ item, state.inventory);

    // Special case for torch
    let still-has-light = !(item == "torch") || contains?(new-inventory, "torch");

    // Update locations and state
    let new-location = assoc(location, "items", new-location-items);
    let locations = assoc(locations, state.current-location, new-location);

    let new-state = assoc(
      assoc(
        assoc(
          state, "inventory", new-inventory),
          "light-source",
          still-has-light
        ),
        "moves",
        state.moves + 1
      );
    [new-state, "You drop the " ++ item ++ "."]
  } else {
    [state, "You don't have a " ++ item ++ " in your inventory."]
  }
};

function inventory(state) {
  if (empty?(state.inventory)) {
    [state, "Your inventory is empty."]
  } else {
    [state, "Inventory: " ++ join(state.inventory, ", ")]
  }
};

function use(state, item) {
  switch item
    case "fishing rod" then
      if (state.current-location == "river") {
        [assoc(state, "moves", state.moves + 1), "You catch a small fish, but it slips away."]
      } else {
        [state, "There's no place to use a fishing rod here."]
      }
    case "torch" then
      if (has-item?(state, item)) {
        [
          assoc(assoc(state, "light-source", true), "moves", state.moves + 1),
          "The torch illuminates the area with a warm glow."
        ]
      } else {
        [state, "You don't have a torch."]
      }
    case "gold key" then
      if (has-item?(state, item) && state.current-location == "treasure room") {
        [
          assoc(
            assoc(state, "game-over", true),
            "moves",
            state.moves + 1
          ),
         "You use the gold key to unlock a secret compartment, revealing a fabulous diamond! You win!"
        ]
      } else {
        [state, "The key doesn't fit anything here."]
      }
    case "bread" then
      if (has-item?(state, item)) {
        let new-inventory = filter(state.inventory, -> $ ≠ item);
        [
          assoc(
            assoc(state, "inventory", new-inventory),
            "moves",
            state.moves + 1
          ),
          "You eat the bread. It's delicious and nourishing."
        ]
      } else {
        [state, "You don't have any bread."]
      }
    case "shiny stone" then
      if (has-item?(state, item)) {
        [
          assoc(state, "moves", state.moves + 1),
          "The stone glows with a faint blue light. It seems magical but you're not sure how to use it yet."
        ]
      } else {
        [state, "You don't have a shiny stone."]
      }
    case "flowers" then
      if (has-item?(state, item)) {
        [
          assoc(state, "moves", state.moves + 1),
          "You smell the flowers. They have a sweet, calming fragrance."
        ]
      } else {
        [state, "You don't have any flowers."]
      }
    case "ancient map" then
      if (has-item?(state, item)) {
        [
          assoc(state, "moves", state.moves + 1),
          "The map shows the layout of the area. All locations are now marked as visited."
        ]
      } else {
        [state, "You don't have a map."]
      }
    case "jeweled crown" then
      if (has-item?(state, item)) {
        [
          assoc(state, "moves", state.moves + 1),
          "You place the crown on your head. You feel very regal."
        ]
      } else {
        [state, "You don't have a crown."]
      }
  end ?? [state, "You can't use that."]
};

// Command parser
function parse-command(state, input) {
  let tokens = lower-case(input) split " ";
  let command = first(tokens);
  let args = rest(tokens) join " ";

  let result = switch command
    case "go" then
      move(state, args)
    case "north" then
      move(state, "north")
    case "south" then
      move(state, "south")
    case "east" then
      move(state, "east")
    case "west" then
      move(state, "west")
    case "take" then
      take!(state, args)
    case "drop" then
      drop!(state, args)
    case "inventory" then
      inventory(state)
    case "i" then
      inventory(state)
    case "look" then
      [assoc(state, "moves", state.moves + 1), describe-location(state)]
    case "use" then
      use(state, args)
    case "help" then
      [state, "Commands then go [direction], north, south, east, west, take [item], drop [item], inventory, look, use [item], help, quit"]
    case "quit" then
      [assoc(state, "game-over", true), "Thanks for playing!"]
  end ?? [state, "I don't understand that command. Type 'help' for a list of commands."];

  result
};

// Game loop
function game-loop(state) {
  alert!(describe-location(state) ++ "\nWhat do you do? ");

  let input = read-line!();
  let command_result = parse-command(state, input);
  let new-state = first(command_result);
  let message = second(command_result);

  alert!("\n" ++ message ++ "\n");

  if (new-state.game-over) {
    alert!("\nGame over! You made " ++ str(new-state.moves) ++ " moves.");
    new-state
  } else {
    game-loop(new-state)
  }
};

// Start game
function start-game() {
  alert!("=== Lits Adventure Game ===\n" ++ "Type 'help' for a list of commands.\n\n");
  game-loop(initial-state)
};

// Call the function to start the game
start-game()
```
