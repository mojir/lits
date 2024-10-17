

# Lits

Lits is a programming language and REPL (Read-Eval-Print Loop) that allows you to evaluate Lisp expressions. It provides a playground for experimenting with Lits and an API for integrating Lits into your own projects.

Lits is a Lisp dialect implemented in TypeScript, drawing heavy inspiration from Clojure. Most core functions have been ported to Lits, ensuring a robust and familiar experience for Clojure users.

* **Dependencies**: No third party dependencies.
* **Immutability**: All datatypes in Lits are immutable.
* **Pure Functions**: Functions are [pure](https://en.wikipedia.org/wiki/Pure_function) by default. Functions with side effects have names ending in an exclamation mark (!), such as `write!` or `rand!`
* **Type Mapping**: All datatypes in Lits map directly to JavaScript types.
* **Evaluation**: Lits does not support lazy evaluation.
* **Macros**:Macros are not supported in Lits.
* **Keyword Symbols**: There are no keyword symbols. The notation `:foo` is simply shorthand for the string `"foo"`
* **Scoping**: Lits uses [dynamic scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scope) not [lexical scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)

## Documentation

You can find the Lits playground [here](https://youcruit.github.io/lits/#index). The playground allows you to interactively write and evaluate Lits expressions.

## Installation

To install Lits globally, run the following command:

```
npm i -g @youcruit/lits
```
## Repl usage
Initiate the Lits REPL in a terminal by typing `lits`. If Lits hasn't been installed globally, you can use `npx lits` instead.

* Tab completion
* History stored on file

```
$ lits
Type "`help" for more information.
> (+ 7 4)
11
> (let ((day (* 24 60 60 1000))) (* 7 day)) ; Ever wondered how many milliseconds there are in a week?
604800000
```
```
$ lits --help
Usage: lits [options]

Options:
  -c, --context=...               Context as a JSON string
  -C, --context-file=...          Context file (.json file)
  -e, --eval=...                  Evaluate Lits expression
  -f, --file=...                  Evaluate .lits file
  -p, --test-pattern=...          Test name pattern, used together with --test
  -t, --test=...                  Test .test.lits file
  --help                          Show this help
  --version                       Print lits version
```
```
$ lits -e "(/ 81 9)"
9
```

# API
## Install api

```
npm i @youcruit/lits
```

## How to use?

```ts
import { Lits } from '@youcruit/lits'

const lits = new Lits()
lits.run("(+ 1 2 3 4)"); // returns 10
```

## Tokenization and Parsing
Lits provides two important functions for working with Lisp expressions: `lits.tokenize` and `lits.parse`.

### lits.tokenize
The `lits.tokenize` function takes a string as input and returns a `TokenStream`. Tokens are the individual components of a Lisp expression, such as parentheses, symbols, numbers, and strings. Here's an example usage:

```
const lits = new Lits()
const expression = "(+ 1 2)";
const tokens = lits.tokenize(expression);
console.log(tokens);
// Output: {
  "tokens": [
    {
      "t": 101,
      "v": "("
    },
    {
      "t": 103,
      "v": "+"
    },
    {
      "t": 102,
      "v": "1"
    },
    {
      "t": 102,
      "v": "2"
    },
    {
      "t": 101,
      "v": ")"
    }
  ]
}
```

### Lits.parse
The `lits.parse` function accepts a `TokenStream` as an argument and returns an Abstract Syntax Tree (AST). By compiling Lits expressions into an AST, you can significantly enhance the evaluation speed, achieving approximately a tenfold improvement. 

The AST is represented as a JSON document, which facilitates straightforward serialization.

To evaluate an AST, utilize the `lits.evaluate` function.

```
const lits = new Lits()
const expression = "(+ 1 2)";
const tokens = lits.tokenize(expression);
const ast = lits.parse(tokens)
console.log(tokens);
// Output: {
  "b": [
    {
      "t": 203,
      "n": "+",
      "p": [
        {
          "t": 201,
          "v": 1
        },
        {
          "t": 201,
          "v": 2
        }
      ]
    }
  ]
}
```