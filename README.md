A lisp implementation in Typescript. Could be used in browser or node.js.

# REPL
## Documentation
[mojir.github.io/lispish](https://mojir.github.io/lispish)
## Install
```
npm i -g lispish
```
## Repl usage
Start the lispish REPL in a terminal by enter `lispish`
* Tab completion
* History stored on file
```
$ lispish
Type "`help" for more information.
LISPISH> (+ 7 4)
11
LISPISH> (let ((day (* 24 60 60 1000))) (* 7 day)) ; Ever wondered how many milliseconds there are in a week?
604800000
```
```
$ lispish --help
Usage: lispish [options]

Options:
  -c ...         Global context as a JSON string
  -C ...         Global context file (.json file)
  -s ...         Repl scope as a JSON string
  -S ...         Repl scope file (.json file)
  -f ...         Lispish file
  -e ...         Lispish expression
  -h, --help     Show this help
```
```
$ lispish -e "(/ 81 9)"
9
```

# API
## Install api

```
npm i lispish
```

## How to use?

```ts
import { lispish, tokenize, parse, evaluate, Token } from 'lispish'

// tokenize, parse and evaluate. All at once!
lispish('(+ 1 2 3 4)'); // returns 10

// or do it step by step
const tokens: Token[] = tokenize('(+ a b c d')
const ast: Ast = parse(tokens)
const context = { a: 1, b: 2, c: 3, d: 4}
const result = evaluate(ast, context) // returns 10
```

# Missing features
* ***1**, ***1**, ***1**, ..., ***9** repl previous results
* ***e** cli most recent error caught by repl
* **any?**
* **assert**
* **assoc-in**
* **char** return charachter from code (charachter = string)
* **char-code** return charachter code from string 
* **comp** (composing functions)
* **compare**
* **complement**
* **conj** ? is this needed
* **distinct** (array) return array with duplicates removed (js === used to check for equallity)
* **drop-last**
* **drop-while**
* **drop**
* **eq?** (collection) recursevly compares two or more collections (deep-equal)
* **every-pred**
* **false?**
* **finite?**
* **find**
* **flatten**
* **fn shorthand** 
  1. #(* 10 %) <==> (fn [x] (* 10 x)
  2. #(* %1 %2) <==> (fn [x y] (* x y)
* **for**
* **get-in**
* **group-by**
* **identity**
* **if-let**
* **infinity?**
* **integer?**
* **juxt**
* **lispish-version**
* **mapcat**
* **memoize**
* **merge-with**
* **name**
* **nan?**
* **not-any?**
* **not-every?**
* **nthnext**
* **nthrest**
* **partial**
* **partition**
* **partition-by**
* **partition-all**
* **quot**
* **rand-nth**
* **rem**
* **remove**
* **select-keys**
* **shuffle** (array)
* **sort-by**
* **special-symbol?**
* **split** ? should work for both strings and arrays ?
* **split-at**
* **split-with**
* **symbol**
* **symbol?**
* **take** reorder arguments to follow clojure's signature
* **take-last** reorder arguments to follow clojure's signature
* **time!**
* **true?**
* **update**
* **update-in**
* **when-first**
* **when-let**
* **when-not**
* **zipmap**
   