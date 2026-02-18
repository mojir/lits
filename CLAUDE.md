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

- **`@mojir/lits`** → `src/index.ts` — Minimal entry: core `Lits` class, types, type guards. No namespaces or reference data.
- **`@mojir/lits/full`** → `src/full.ts` — Full entry: everything from minimal plus all namespaces, reference data, and API helpers.
- **`@mojir/lits/namespaces/<name>`** → `src/namespaces/<name>.ts` — Individual namespace entries (assert, grid, random, vector, linearAlgebra, matrix, numberTheory).

Rollup configs: `rollup.config.js` (library bundles), `rollup.config.cli.js` (CLI), `rollup.config.playground-builder.js`, `rollup.config.playground-www.js`.

### Source Layout (`src/`)

- `Lits/Lits.ts` — Main `Lits` class. Namespaces are injected via constructor `config.namespaces`.
- `tokenizer/` — Lexer: source code → token stream.
- `parser/` — Parser: token stream → AST.
- `evaluator/` — Evaluator: AST → result value.
- `transformer/` — AST transformers (symbol renaming, etc.).
- `untokenizer/` — Token stream → source code (pretty-printing).
- `AutoCompleter/` — Autocomplete support.
- `getUndefinedSymbols/` — Static analysis for undefined symbols.
- `builtin/` — All built-in expressions and namespaces.

### Built-in Expressions (`src/builtin/`)

- `interface.ts` — Core types: `Category`, `FunctionDocs`, `CustomDocs`, `BuiltinNormalExpressions`, `BuiltinSpecialExpression`.
- `specialExpressionTypes.ts` — Maps special expression names to array indices.
- `index.ts` — Assembles `specialExpressions` array and `normalExpressions` record from all core categories.
- `normalExpressions/index.ts` — Aggregates all core normal expression maps.
- `core/` — 12 core categories, each file exports a `BuiltinNormalExpressions` record:
  - `array.ts`, `bitwise.ts`, `collection.ts`, `functional.ts`, `math.ts`, `meta.ts`, `misc.ts`, `object.ts`, `predicates.ts`, `regexp.ts`, `sequence.ts`, `string.ts`
- `specialExpressions/` — Individual special expression implementations (and, cond, def, defn, fn, for, if, let, loop, or, try, etc.).

### Namespaces (`src/builtin/namespaces/`)

Namespaces provide domain-specific function libraries. Each namespace is in its own directory and exports a `LitsNamespace` object:

- `assert/` (name: `"Assert"`) — Assertion functions.
- `grid/` (name: `"Grid"`) — 2D grid operations.
- `random/` (name: `"Random"`) — Random number generation.
- `vector/` (name: `"Vector"`) — Vector math.
- `linearAlgebra/` (name: `"Linear-Algebra"`) — Linear algebra operations.
- `matrix/` (name: `"Matrix"`) — Matrix operations.
- `numberTheory/` (name: `"Number-Theory"`) — Number theory functions.

**Registration**: Namespaces are injected via `new Lits({ namespaces: [...] })`. The global registry (`registry.ts`) is no longer used at import time; `allNamespaces.ts` registers all built-in namespaces for the full bundle.

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

- `index.ts` — Derives all reference data from co-located docs. Exports `normalExpressionReference`, `namespaceReference`, `functionReference`, `apiReference`, `allReference`, and type guards (`isFunctionReference`, `isCustomReference`, `isShorthandReference`, `isDatatypeReference`).
- `api.ts` — Defines all API name types (`ApiName`, `CoreApiName`, `NamespaceExpressionName`, etc.), `Category` type, validation functions (`isApiName`, `isDataType`).
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

**Core** (12): `Special expression`, `Predicate`, `Sequence`, `Collection`, `Array`, `Object`, `String`, `Math`, `Functional`, `Regular expression`, `Bitwise`, `Misc`, `Meta`

**Namespace** (7): `Assert`, `Vector`, `Linear Algebra`, `Matrix`, `Grid`, `Number Theory`, `Random`

**Other** (2): `Shorthand`, `Datatype`

### Coding Conventions

- Imports must be sorted alphabetically (ESLint rule).
- `it()` descriptions must begin with lowercase.
- Target: ES5 with es2015 modules (no `--downlevelIteration`; use `Array.from()` instead of spread on iterables).
- No side-effect imports for namespace registration.
