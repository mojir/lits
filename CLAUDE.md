# CLAUDE.md — Project Architecture Guide

## Overview

Lits is a pure functional programming language implemented in TypeScript. It features algebraic notation, JavaScript interoperability, and a comprehensive standard library.

## Key Commands

```bash
npm run check        # lint + typecheck + test + build (full pipeline)
npm run test         # vitest run --coverage
npm run typecheck    # tsc -p ./tsconfig.compile.json --noEmit
npm run lint         # eslint . --fix
npm run build        # build all bundles (lits + cli + playground)
```

## Project Structure

### Entry Points and Bundles

The package has multiple entry points configured in `package.json` `exports`:

- **`@mojir/lits`** → `src/index.ts` — Minimal entry: core `Lits` class, types, type guards. No modules or reference data.
- **`@mojir/lits/full`** → `src/full.ts` — Full entry: everything from minimal plus all modules, reference data, and API helpers.
- **`@mojir/lits/modules/<name>`** → `src/modules/<name>.ts` — Individual module entries (assert, grid, random, vector, linear-algebra, matrix, number-theory, math, functional, string, collection, sequence, bitwise).

Rollup configs: `rollup.config.js` (library bundles), `rollup.config.cli.js` (CLI), `rollup.config.playground-builder.js`, `rollup.config.playground-www.js`.

### Source Layout (`src/`)

- `Lits/Lits.ts` — Main `Lits` class. Modules are injected via constructor `config.modules`.
- `tokenizer/` — Lexer: source code → token stream.
- `parser/` — Parser: token stream → AST.
- `evaluator/` — Evaluator: AST → result value.
- `transformer/` — AST transformers (symbol renaming, etc.).
- `untokenizer/` — Token stream → source code (pretty-printing).
- `AutoCompleter/` — Autocomplete support.
- `getUndefinedSymbols/` — Static analysis for undefined symbols.
- `builtin/` — All built-in expressions and modules.

### Built-in Expressions (`src/builtin/`)

- `interface.ts` — Core types: `Category`, `FunctionDocs`, `CustomDocs`, `BuiltinNormalExpressions`, `BuiltinSpecialExpression`.
- `specialExpressionTypes.ts` — Maps special expression names to array indices.
- `index.ts` — Assembles `specialExpressions` array and `normalExpressions` record from all core categories.
- `normalExpressions/index.ts` — Aggregates all core normal expression maps.
- `core/` — 12 core categories, each file exports a `BuiltinNormalExpressions` record:
  - `array.ts`, `bitwise.ts`, `collection.ts`, `functional.ts`, `math.ts`, `meta.ts`, `misc.ts`, `object.ts`, `predicates.ts`, `regexp.ts`, `sequence.ts`, `string.ts`
- `specialExpressions/` — Individual special expression implementations (and, cond, def, defn, fn, for, if, let, loop, or, try, etc.).

### Modules (`src/builtin/modules/`)

Modules provide domain-specific function libraries. Each module is in its own directory and exports a `LitsModule` object:

- `assert/` (name: `"assert"`) — Assertion functions.
- `grid/` (name: `"grid"`) — 2D grid operations.
- `random/` (name: `"random"`) — Random number generation.
- `vector/` (name: `"vector"`) — Vector math.
- `linear-algebra/` (name: `"linear-algebra"`) — Linear algebra operations.
- `matrix/` (name: `"matrix"`) — Matrix operations.
- `number-theory/` (name: `"number-theory"`) — Number theory functions.
- `math/` (name: `"math"`) — Math utility functions.
- `functional/` (name: `"functional"`) — Functional programming utilities.
- `string/` (name: `"string"`) — String utility functions.
- `collection/` (name: `"collection"`) — Collection utility functions.
- `sequence/` (name: `"sequence"`) — Sequence utility functions.
- `bitwise/` (name: `"bitwise"`) — Bitwise utility functions.

**Registration**: Modules are injected via `new Lits({ modules: [...] })`. The global registry (`registry.ts`) is no longer used at import time; `allModules.ts` registers all built-in modules for the full bundle.

### Co-located Documentation

Every built-in function has a `docs` property directly on its expression object (in the same file as its implementation). The `docs` field follows the `FunctionDocs` interface:

```typescript
interface FunctionDocs {
  category: Category
  description: string
  returns: TypedValue
  args: Record<string, Argument>
  variants: Variant[]
  examples: string[]
  seeAlso?: string[]
  hideOperatorForm?: boolean
}
```

Special expressions may use `CustomDocs` instead (with `customVariants` instead of `args`/`variants`/`returns`).

### Reference Data (`reference/`)

- `index.ts` — Derives all reference data from co-located docs. Exports `normalExpressionReference`, `moduleReference`, `functionReference`, `apiReference`, `allReference`, and type guards (`isFunctionReference`, `isCustomReference`, `isShorthandReference`, `isDatatypeReference`).
- `api.ts` — Defines all API name types (`ApiName`, `CoreApiName`, `ModuleExpressionName`, etc.), `Category` type, validation functions (`isApiName`, `isDataType`).
- `datatype.ts` — Datatype reference entries.
- `examples.ts` — Example code for playground.
- `shorthand.ts` — Shorthand reference entries.

### Testing

- Test framework: Vitest 1.6.0.
- Tests live in `__tests__/` (integration), `src/**/*.test.ts` (unit), and co-located test files.
- `__tests__/docs-migration.test.ts` — Comprehensive validation that every function has docs, categories are correct, seeAlso references are valid, no orphaned references, type consistency, and snapshot stability.
- Run a single test file: `npx vitest run path/to/test.ts`

### Categories

22 valid categories defined in `src/builtin/interface.ts`:

**Core** (15): `special-expression`, `predicate`, `sequence`, `collection`, `array`, `object`, `string`, `math`, `functional`, `regular-expression`, `bitwise`, `misc`, `meta`, `shorthand`, `datatype`

**Module** (13): `math`, `functional`, `string`, `collection`, `sequence`, `bitwise`, `assert`, `vector`, `linear-algebra`, `matrix`, `grid`, `number-theory`, `random`

**Other** (2): `shorthand`, `datatype`

### Coding Conventions

- Imports must be sorted alphabetically (ESLint rule).
- `it()` descriptions must begin with lowercase.
- Target: ES5 with es2015 modules (no `--downlevelIteration`; use `Array.from()` instead of spread on iterables).
- No side-effect imports for module registration.
