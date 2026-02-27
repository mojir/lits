# Getting Started

## Installation

Install Lits from npm:

```sh
npm install @mojir/lits
```

## Using Lits as a Library

There are two main entry points: **minimal** and **full**.

### Minimal Bundle

The minimal bundle gives you the core `Lits` class, types, and type guards. No modules or reference data are included — this keeps your bundle size small.

```javascript
import { Lits } from '@mojir/lits'

const lits = new Lits()
lits.run('10 + 20') // => 30
```

This is the right choice when you want the core language and don't need optional modules like vector math or matrix operations.

### Full Bundle

The full bundle includes everything from the minimal bundle plus all built-in modules, reference data, and API helpers.

```javascript
import { Lits, allBuiltinModules } from '@mojir/lits/full'

const lits = new Lits({ modules: allBuiltinModules })
lits.run('let v = import(vector); v.dot([1, 2, 3], [4, 5, 6])') // => 32
```

### Individual Modules

You can also import only the modules you need and pass them to the `Lits` constructor. This gives you fine-grained control over bundle size:

```javascript
import { Lits } from '@mojir/lits'
import { vectorModule } from '@mojir/lits/modules/vector'
import { matrixModule } from '@mojir/lits/modules/matrix'

const lits = new Lits({ modules: [vectorModule, matrixModule] })
```

Available modules: `assertion`, `grid`, `random`, `vector`, `linear-algebra`, `matrix`, `number-theory`, `math`, `functional`, `string`, `collection`, `sequence`, and `bitwise`.

### Passing Values and Functions

You can expose JavaScript values and functions to Lits code via `bindings`:

```javascript
import { Lits } from '@mojir/lits'

const lits = new Lits()

// Expose JavaScript values
lits.run('name ++ " is " ++ str(age)', {
  bindings: { name: 'Alice', age: 30 }
}) // => "Alice is 30"

// Expose JavaScript functions
lits.run('greet("World")', {
  bindings: {
    greet: name => `Hello, ${name}!`,
  }
}) // => "Hello, World!"
```

## CLI Tool

Install the Lits CLI globally to use it from the command line:

```sh
npm install --global @mojir/lits
```

### Interactive REPL

Start an interactive session by running `lits` with no arguments:

```sh
$ lits
```

### Evaluate Expressions

```sh
$ lits eval "5 + 3"
8

$ lits eval "[1, 2, 3, 4] filter odd? map inc"
[2, 4]
```

### Run Files

```sh
$ lits run script.lits
```

### Other Commands

| Command | Description |
|---|---|
| `lits eval <expr>` | Evaluate a Lits expression |
| `lits run <file>` | Run a `.lits` source file |
| `lits bundle <entry>` | Bundle a multi-file project into a single JSON file |
| `lits run-bundle <file>` | Run a bundled `.json` file |
| `lits test <file>` | Run a `.test.lits` test file |
| `lits repl` | Start the interactive REPL (default) |

## Try It Here

You don't need to install anything to start learning. This playground runs Lits directly in your browser. Try it:

```
let greet = name -> str("Hello, ", name, "!");
greet("World")
```
