# File Modules Design

## Problem

Lits currently has two separate mechanisms for referencing external code:

1. **`import(moduleName)`** — a language-level special expression that imports registered builtin modules (e.g., `import(vector)`, `import(math)`). These are TypeScript-implemented modules injected via `new Lits({ modules: [...] })`.

2. **`// @include file.lits`** — a comment-based directive used only by the test framework. It's invisible to the language, parsed by TypeScript host code, and has no general-purpose use.

This is not clean. The goal is to unify file imports under the existing `import` syntax and introduce a bundler for multi-file projects.

## Solution: Two-Phase Approach

### Phase 1 — Bundler (build time)

A standalone function (not a method on `Lits`) that:

1. Reads the entry file and scans for `import("./path/to/file.lits")` calls (string argument = file import).
2. Recursively resolves all file imports, following `import("...")` in each referenced file.
3. Resolves all relative paths to absolute paths, deduplicating files that are referenced from multiple locations.
4. Assigns each unique file a **canonical module name** (a valid Lits symbol).
5. Topologically sorts the file modules by dependency order.
6. Rewrites all `import("./path/to/file.lits")` calls to `import(canonicalName)` (string argument → bare symbol).
7. Outputs a `LitsBundle`.

### Phase 2 — Existing `Lits.run` (runtime)

`Lits.run` accepts `string | LitsBundle`. When it receives a bundle:

1. Iterates `fileModules` in order (dependency order).
2. For each `[name, source]`: parses and evaluates the source, registers the result as a **value module** keyed by `name`.
3. Parses and evaluates the main `program`, which can now `import(name)` and find the registered value modules.

No changes to the parser or tokenizer are needed — the bundler rewrites string imports to bare symbol imports before the parser sees them.

## Bundle Format

```typescript
interface LitsBundle {
  program: string
  fileModules: [string, string][]  // [canonicalName, source][], ordered by dependency
}
```

- `program`: The main program source, with file imports rewritten to bare symbols.
- `fileModules`: An ordered array of `[canonicalName, source]` pairs. Array (not object) because order matters — dependencies must be evaluated before dependents.
- All values are strings (unparsed Lits source code). Pre-parsing to ASTs is a potential future optimization, not included in the initial implementation.
- The bundle is pure JSON — fully serializable and portable (e.g., build on a server, run in a browser).

## Bundler API

The bundler is a standalone function, separate from the `Lits` class:

```typescript
// Separate entry point: '@mojir/lits/bundler'
import { bundle } from '@mojir/lits/bundler'

const b = bundle('./main.lits')
```

Import paths can be:
- **Relative**: `import("./lib/utils.lits")`, `import("../../shared/helpers.lits")`
- **Absolute**: `import("/opt/lits-libs/utils.lits")`

Paths outside the project root are allowed — no restrictions.

```typescript
// Runtime (anywhere, including browser)
import { Lits } from '@mojir/lits'
const lits = new Lits()
lits.run(b)
```

**Why standalone, not on the Lits class:**
- Bundling is a build-time concern; `Lits` is the runtime.
- Bundling requires file system access; the `Lits` class doesn't and shouldn't.
- Separate entry point (`@mojir/lits/bundler`) — browser consumers never pay for it.

## Canonical Module Names

The bundler resolves every `import("...")` to an absolute file path, then derives a canonical module name. Canonical name generation is an internal bundler concern — names only exist inside the bundle and are never referenced by the user directly.

The bundler should prefer readable names:

- **Files under the entry directory**: Path relative to the entry file's directory, with `.lits` stripped. E.g., `lib/utils` for `./lib/utils.lits`.
- **Files outside the entry directory**: The bundler derives a readable name from the file path (e.g., using the last N path segments). If there's a collision, the bundler disambiguates (e.g., by adding more path segments or a suffix).

Canonical names are always valid Lits symbols (no dots — `.lits` extension is stripped). They are naturally distinct from builtin module names (bare words like `math`, `vector`) because file module names contain path separators (`/`).

### Deduplication

Multiple files may reference the same file via different relative paths. The bundler resolves all paths to absolute, so the same file maps to the same canonical name. Each file appears exactly once in the bundle.

## Value Modules

Currently, `import(moduleName)` only works with **builtin modules** (`LitsModule`), which contain `BuiltinNormalExpression` objects with TypeScript `evaluate` functions. File modules are different — they evaluate to a plain Lits value.

The `import` evaluator handles two kinds of modules:

- **Builtin modules** (`LitsModule`): Existing behavior. Wraps functions as `ModuleFunction` descriptors.
- **Value modules** (new): The result of evaluating a file module source. `import(name)` returns the stored value directly — no wrapping.

A file module can evaluate to **any Lits value**: object, array, number, string, null, function, etc.

```lits
// math-helpers.lits → evaluates to an object
let add = (a, b) -> a + b;
{add: add}

// constants.lits → evaluates to a number
42

// names.lits → evaluates to an array
["alice", "bob"]
```

```lits
let { add } = import(math-helpers);   // object → destructure
let answer = import(constants);        // number → use directly
let names = import(names);             // array → use directly
```

The `ContextStack` needs a second map (or a unified map with a discriminated union) to hold value modules alongside builtin modules.

## Changes to `Lits.run`

`Lits.run` signature becomes:

```typescript
run(programOrBundle: string | LitsBundle, params?: ContextParams & FilePathParams): unknown
```

- `string`: Today's behavior — tokenize, parse, evaluate.
- `LitsBundle`: Evaluate file modules in order, register as value modules, then parse and evaluate the main program.

## Removing `evaluate` from the Public API

`Lits.evaluate(ast, params)` can be removed from the public API. With `Lits.run` accepting bundles, there's no external need for `evaluate`. It becomes a private implementation detail.

Current external usage is minimal:
- 2 calls in a performance test file (can use `run`)
- 1 documentation example in README.md (update)

## Replacing `@include`

The `// @include file.lits` comment directive in the test framework is replaced by standard `import("./file.lits")` syntax. The test framework would use the bundler to resolve imports before running tests, or construct a `LitsBundle` directly.

## Future Considerations (Not in Initial Implementation)

- **Bundler options**: `bundle(entry, options?)` — an optional second argument for configuration such as `noAccessOutsideProject`, `parse` (output ASTs instead of source strings), `optimize` (AST optimization when parsing), and parser options (e.g., `debug`). The current zero-config signature extends naturally without breaking changes.
- **Pre-parsed bundles**: A `{ parse: true }` option on the bundler to output ASTs instead of source strings. Would require `LitsBundle` to become a discriminated union or generic to represent both source and AST bundles. Would enable an eval-only runtime entry point (`@mojir/lits/eval`) that excludes the tokenizer and parser for smaller browser bundles.
- **Eval-only entry point**: `@mojir/lits/eval` — a minimal runtime that only accepts pre-parsed ASTs/bundles, excluding tokenizer and parser code.
