# Lits Refactoring Plan: Co-located Docs & Bundle-friendly Architecture

## Progress Tracker

**Current phase:** Phase 2 — Migrate all core categories
**Current step:** Step 11 — Repeat docs co-location for remaining core categories
**Last updated:** 2026-02-18
**Branch:** `new-namespace` (per-phase work on `phase-N` branches, squash-merged back)

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1: Dir rename, types, proof of concept | ✅ Done | bitwise pilot complete, all 2861 tests pass |
| Phase 2: Migrate all core categories | ⬜ Not started | |
| Phase 3: Migrate namespace categories | ⬜ Not started | |
| Phase 4: Explicit namespace registration | ⬜ Not started | |
| Phase 5: Multiple entry points | ⬜ Not started | |
| Phase 6: Cleanup and validation | ⬜ Not started | |

### Phase 1 completed work
- Renamed `src/builtin/normalExpressions/categories/` → `src/builtin/core/` (git mv, history preserved)
- Updated all imports (12 files in core/, 1 barrel file)
- Defined `FunctionDocs`, `TypedValue`, `Argument`, `Variant`, `Category`, `DataType` in `src/builtin/interface.ts`
- Added optional `docs?: FunctionDocs` to `BuiltinNormalExpression`
- Piloted bitwise: co-located all 12 function docs into `src/builtin/core/bitwise.ts`
- Updated `reference/index.ts` with `docsToReference()` helper — reads bitwise from implementation, others still from old reference files
- Added `__tests__/docs-migration.test.ts`: validates docs fields, compares against legacy reference, snapshot of `allReference`
- All 2861 tests pass

### Session start prompt

Paste this at the start of each LLM session:

```
Read /Users/albert.mojir/mojir/lits/REFACTORING-PLAN.md — this is the plan for a multi-session refactoring.
Check the "Progress Tracker" section to see where I left off.
Continue from the current phase/step. At the end of our session, update the Progress Tracker.
```

## Context

This document captures architectural decisions for refactoring the Lits codebase. The goals are:

1. **Bundle-friendly architecture** — enable multiple bundle variants (full, minimal, per-namespace)
2. **Co-located reference data** — move documentation into the same objects as the implementation
3. **Unified structure** — treat core functions and namespace functions uniformly

## Architectural Decisions

### 1. Co-locate docs with implementation

Add an optional `docs` field to `BuiltinNormalExpression`. Today, implementation lives in `src/builtin/` and reference data lives separately in `reference/`. After refactoring, each function object carries its own documentation.

**New `BuiltinNormalExpression` shape:**

```typescript
interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  arity: Arity
  docs?: FunctionDocs
}
```

**`FunctionDocs` type (replaces `FunctionReference`):**

```typescript
interface FunctionDocs {
  category: Category            // single canonical grouping, e.g. 'Math', 'Assert'
  tags?: string[]               // optional cross-cutting labels for search/discovery (populate later via LLM)
  description: string
  returns: TypedValue
  args: Record<string, Argument>
  variants: Variant[]
  examples: string[]
  seeAlso?: string[]            // optional array of function identifiers, e.g. ['+', 'Random.random']. Verified by test.
  hideOperatorForm?: true       // renamed from noOperatorDocumentation
}
```

**Key decisions:**
- `title` is REMOVED — compute it from the record key (or `${namespace}.${key}` for namespace functions)
- `category` is a **string union type** (`type Category = 'Math' | 'Collection' | 'Assert' | ...`), one single value per function
- `tags` is an **optional string array** for cross-cutting grouping — don't populate now, add later via LLM pass
- `noOperatorDocumentation` is renamed to `hideOperatorForm`
- `docs` is **optional** so minimal bundles can strip it via a build plugin

### 2. Category rules

- **Every function has a `category`, always.** Enforced by tests.
- **Core functions:** `category` = their logical group (`'Math'`, `'Collection'`, `'Sequence'`, etc.)
- **Namespace functions:** `category` = the namespace name (`'Assert'`, `'Vector'`, `'Grid'`, etc.)
- A test enforces that `category` matches the folder/namespace the function lives in.

### 3. Directory structure

Rename `normalExpressions/categories/` to `core/`. Keep namespaces as-is. Both use the same `BuiltinNormalExpression` interface.

```
src/builtin/
  core/                           ← was normalExpressions/categories/
    math/
      index.ts                    ← barrel exporting all math expressions
      abs.ts                      ← { evaluate, arity, docs: { category: 'Math', ... } }
      round.ts
      ...
    collection/
      index.ts
      filter.ts
      ...
    (etc. for all current core categories)

  namespaces/
    assert/                       ← small namespace, flat structure
      index.ts
      assert-equal.ts             ← { evaluate, arity, docs: { category: 'Assert', ... } }
      assert-truthy.ts
      ...
    vector/                       ← larger namespace, can have subfolders for file organization
      index.ts
      dot.ts                      ← { evaluate, arity, docs: { category: 'Vector', ... } }
      reductions/
        sum.ts                    ← category is still 'Vector', subfolder is just file organization
        mean.ts
      ...
```

**Rules:**
- Subfolders within a category/namespace are purely for developer convenience — they don't affect the `category` value
- Small namespaces stay flat, large ones grow subfolders organically
- No rigid "one file per function" rule

### 4. Remove global mutable namespace registry

Replace the side-effect-based `registerNamespace()` pattern in `src/builtin/namespaces/registry.ts` with explicit namespace injection via the `Lits` constructor:

```typescript
new Lits({ namespaces: [assertNamespace, gridNamespace, vectorNamespace] })
```

This is critical for tree-shaking and for enabling different bundle variants.

### 5. Multiple entry points for different bundles

Configure `package.json` `exports` for different consumers:

- `@mojir/lits` → minimal core (no namespaces, no docs)
- `@mojir/lits/full` → core + all namespaces + all docs
- `@mojir/lits/namespaces/assert` → just the assert namespace
- (etc.)

Use a build plugin to strip `docs` fields for the minimal bundle.

### 6. Special expressions

Special expressions (`if`, `for`, `let`, `def`, etc.) have a different interface (`BuiltinSpecialExpression`) from normal expressions. They should also get a `docs` field, but their refactoring follows the same pattern. Handle them alongside core normal expressions.

### 7. Eliminate `reference/api.ts`

The 736-line `reference/api.ts` exists to bridge the gap between the separate `reference/` and `src/builtin/` worlds. Once docs are co-located, this gap disappears. The file should be deleted and its useful parts (~30 lines) extracted:

- **`Category` type** — move to `src/builtin/interface.ts` alongside `FunctionDocs`. Keep as a string union type.
- **`DataType` type** — move to `src/builtin/interface.ts`.
- **`Argument` type** — move to `src/builtin/interface.ts`.
- **`getOperatorArgs` helper** — move to wherever it's used.
- **Function name union types** (`MathApiName`, `CoreNormalExpressionName`, etc.) — derive from `keyof typeof` the actual expression records instead of maintaining manual lists.
- **`isApiName` / `isCoreApiName`** — derive from `Object.keys()` of the expression records at runtime.
- **`categoryToNamespace`** — derive from namespace objects themselves, or eliminate if namespace names match category names.

This removes ~700 lines of manually maintained name lists that must stay in sync with the implementation.

### 8. Code to eliminate (bridging/glue inventory)

The following ~4,000+ lines across ~25 files exist primarily because docs and implementation are separate. Track their removal during the refactoring:

| File/Pattern | What it does | Lines | Action |
|---|---|---|---|
| `reference/categories/*.ts` (18 files) | Parallel doc records mirroring `src/builtin/` 1:1 | ~3000+ | **Delete** — docs move into expression objects |
| `reference/api.ts` | Manual name catalog + union types | ~736 | **Delete** — extract ~30 lines of types to `interface.ts`, derive the rest |
| `reference/index.ts` | Aggregator + cross-validator between reference and implementation | 165 | **Delete or reduce to thin utility** — aggregation becomes automatic |
| `setNormalExpressionReference` bridge | Empty mutable object in `normalExpressions/index.ts`, populated via setter from entry point | ~10 | **Delete** — no injection needed when docs are on the object |
| `src/index.ts` wiring (lines ~17-22) | Imports reference data and calls `setNormalExpressionReference()` | ~6 | **Delete** |
| `meta` category factory pattern | `getMetaNormalExpression(ref)` receives reference record via closure so `doc` builtin can look up docs | ~40 | **Simplify** — `doc` reads `expression.docs` directly, factory disappears |
| `reference/index.ts` operator patching | Loops over references + `normalExpressions` to compute `_isOperator` / `_prefereOperator` | ~20 | **Move** — compute where the expression is defined, or in a single post-processing pass |
| `__tests__/reference.test.ts` "everything documented" | Cross-checks every implementation key has a matching reference key | ~15 | **Delete** — structurally impossible to fail with co-located docs |
| `src/index.ts` reference re-exports | Re-exports types and data from `reference/` tree | ~4 | **Simplify** — exports come from within `src/builtin/` |
| `reference/datatype.ts` + `reference/shorthand.ts` | Doc-only records (no implementation counterpart) | ~100 | **Move** to a small utility file in `src/builtin/` or keep as-is (low priority) |

---

## Step-by-step Refactoring Plan

### Phase 1: Directory rename, new types, and proof of concept

1. **Rename `src/builtin/normalExpressions/categories/`** to **`src/builtin/core/`**. Use `git mv` to preserve history.
2. **Update all imports** referencing the old path. Run `tsc --noEmit` to verify.
3. **Update `src/builtin/normalExpressions/index.ts`** (or rename/restructure it) to import from `core/` instead.
4. **Define `FunctionDocs` type** in `src/builtin/interface.ts` (or a new `src/builtin/docs.ts`). Include `category`, `description`, `returns`, `args`, `variants`, `examples`, `seeAlso?`, `hideOperatorForm?`, `tags?`.
5. **Add optional `docs?: FunctionDocs` field** to the `BuiltinNormalExpression` interface.
6. **Pick one small core category** (e.g. `bitwise`) as the pilot.
7. **Copy the reference data** from `reference/categories/bitwise.ts` into each function object in `src/builtin/core/bitwise.ts` as the `docs` field. Remove `title` (it will be computed).
8. **Update `reference/index.ts`** to read docs from the implementation objects for the migrated category (instead of the separate reference file). Verify the playground/docs still work.
9. **Write a test** that asserts every function in the migrated category has a `docs` field with a valid `category`.
10. **Verify all existing tests pass.**

### Phase 2: Migrate all core categories

11. Repeat step 7-8 for each core category: `array`, `collection`, `sequence`, `math`, `misc`, `object`, `predicates`, `regexp`, `string`, `functional`, `meta`.
12. **Migrate special expressions** — add `docs` field to `BuiltinSpecialExpression` interface, co-locate docs from `reference/categories/specialExpressions.ts`.
13. **Migrate shorthand and datatype references** — these are documentation-only entries. Decide where they live (possibly a `src/builtin/docs/` directory or keep in a slimmed-down `reference/` file).
14. **Stop importing from old reference category files.** Don't delete them yet — keep as read-only validation. Add a test per category that compares the old reference data with the new `docs`-derived data to prove they're identical.
15. **Update `reference/index.ts`** to build `normalExpressionReference` and `specialExpressionsReference` by reading `docs` from the implementation objects.

### Phase 3: Migrate namespace categories

16. **Migrate each namespace's reference data** into the namespace implementation files: `assert`, `grid`, `random`, `vector`, `linearAlgebra`, `matrix`, `numberTheory`.
17. **Stop importing from old namespace reference files.** Keep as read-only validation with comparison tests, same as Phase 2.
18. **Update `reference/index.ts`** to build `namespaceReference` from namespace implementation objects.
19. At this point, all reference data lives in implementation files. `reference/index.ts` becomes a thin layer that collects `docs` from implementations.

### Phase 4: Explicit namespace registration

20. **Add a `namespaces` option** to the `Lits` constructor (or its options type).
21. **Change `Lits`** to register only the namespaces passed via the constructor (default: none, or all for backward compatibility during migration).
22. **Remove the side-effect registration** in `src/builtin/namespaces/index.ts` — no more top-level `registerNamespace()` calls.
23. **Replace the global mutable `Map`** in `registry.ts` with instance-level namespace storage on the `Lits` object (or passed through the context stack).
24. **Update all tests** that use namespaces to explicitly pass them.
25. **Decide on backward compatibility** — possibly keep a `Lits.full()` factory that includes all namespaces.

### Phase 5: Multiple entry points and bundle variants

26. **Create entry point files:**
   - `src/index.ts` → minimal (core only, no namespaces, docs optionally stripped)
   - `src/full.ts` → imports core + all namespaces + docs
   - `src/namespaces/assert.ts` → re-exports assert namespace
   - (etc. for each namespace)
27. **Configure build** (Rollup or Vite library mode) to produce separate bundles from each entry point.
28. **Configure `package.json` `exports`** map for each entry point.
29. **Add a build plugin** (Rollup/Vite) that strips `docs` fields for the minimal bundle.
30. **Test each bundle variant** — verify minimal bundle doesn't include namespace code or docs, full bundle has everything, individual namespace bundles work.

### Phase 6: Cleanup and validation

31. **Delete the old reference category files** (`reference/categories/`) now that comparison tests have proven the migrated data is identical.
32. **Slim down `reference/index.ts`** — it should now just collect `docs` from implementation objects, compute `title`, and derive `_isOperator` / `_prefereOperator`.
33. **Write comprehensive tests:**
    - Every function has `docs` with a valid `category`
    - `category` matches folder/namespace location
    - Core category names and namespace names are disjoint
    - `seeAlso` references resolve to valid function identifiers
    - No orphaned reference data
    - Each bundle variant exports the expected symbols
34. **Update `README.md`** with new import paths and bundle options.
35. **Update `CLAUDE.md`** with the new architecture description for future AI assistance.

---

## LLM-assisted Refactoring Strategy

The bulk of this refactoring is mechanical file/data movement. To minimize token cost, **ask the LLM to generate scripts** rather than performing edits one file at a time.

### Scripts to generate (keep in `scripts/refactor/`)

| Script | Purpose | Tool |
|--------|---------|------|
| `move-files.sh` | Move files from old paths to new paths based on a mapping | bash `mv` |
| `merge-docs.ts` | Read reference files + implementation files, merge reference data into `docs` field on each function object | `ts-morph` |
| `rewrite-imports.ts` | Update all import paths after files move | `ts-morph` |
| `generate-barrels.ts` | Regenerate `index.ts` barrel files from directory listings | `ts-morph` or simple fs |
| `validate.ts` | Post-step validation: all imports resolve, all functions have `docs`, categories match folders | TypeScript compiler API + custom checks |

### Why scripts over direct LLM edits

- **O(1) LLM calls** — generate the script once, run it on all files. Direct edits are O(n × file_size) in tokens.
- **Reproducible** — if the script isn't perfect, fix and re-run. Cheaper than re-prompting.
- **Auditable** — you can review the script before running it, and `git diff` the result.

### Prompt pattern for generating scripts

```
Write a Node.js script using ts-morph that:
1. [specific mechanical task]
2. [input: which files/directories to read]
3. [transform: what to change in each file]
4. [output: write modified files back / write new files]
5. Prints a summary of changes made

Here are the exact type shapes involved: [paste relevant types]
Here is an example of the input format: [paste one example file]
Here is the expected output format: [paste what the result should look like]
```

### What to handle directly with LLM (not scripts)

- Phase 1 proof-of-concept (one category, done manually to validate the approach)
- Edge cases in special expressions (varied shapes, not worth scripting)
- Type definition changes (`interface` updates in `src/builtin/interface.ts`)
- Constructor API changes for namespace injection
- Entry point files and build configuration

## Tips & Tricks

### Snapshot the reference output before starting

Generate the full reference output (the `allReference` object) as JSON and commit it as a snapshot test. After every phase, regenerate and diff. **If the output is byte-identical, the refactoring is correct** — regardless of how much you moved internally. This is your ultimate safety net.

```typescript
test('reference output is stable', () => {
  expect(JSON.stringify(allReference, null, 2)).toMatchSnapshot()
})
```

### Keep the old `reference/` directory as read-only validation during migration

Don't delete reference files immediately after migrating a category. Instead, add a test that compares the old reference data with the new `docs`-derived data. Delete the old file only when the test proves they're identical. This lets you migrate incrementally with confidence.

### Use `git mv` for file moves

`git mv` instead of plain `mv` preserves file history through renames. When reviewing later, `git log --follow` will track a function's history across the move.

### Use `tsc --noEmit` as a fast smoke test

After any file move or import change, run `tsc --noEmit` before running the full test suite. It catches broken imports in seconds, not minutes. Add it as a script: `"check": "tsc --noEmit"`.

### Track bundle size

Record the bundle size before starting. Check it after each phase. Co-locating docs will increase the full bundle — that's expected. But the minimal bundle (with docs stripped) should stay the same or shrink. Catch accidental bloat early.

```bash
# Quick bundle size check
ls -la dist/*.js | awk '{print $5, $9}'
```

### Use a compatibility shim during incremental migration

While migrating category by category, have `reference/index.ts` read from **both** sources — the old reference files for unmigrated categories and the new `docs` fields for migrated ones. This way the playground/docs work at every commit, not just after a completed phase.

```typescript
// Temporary during migration — remove when all categories are migrated
const mathRef = mathExpression.docs  // migrated: read from implementation
  ?? legacyMathReference             // not yet migrated: read from old reference file
```

### One git branch per phase, squash-merge back

The refactoring starts from the `new-namespace` branch. For each phase, create a sub-branch (e.g. `new-namespace/phase-1`, `new-namespace/phase-2`). If a phase goes sideways, discard the sub-branch — no damage to `new-namespace`. Squash-merge each phase back to `new-namespace` when done so the branch history stays clean. Merge `new-namespace` to `master` only when the full refactoring is complete and validated.

### Enable `noUnusedLocals` in tsconfig during migration

Temporarily enable `"noUnusedLocals": true` in tsconfig. After moving files and updating imports, any leftover dead imports from the old structure will surface as compiler errors. Disable it again after migration if it's too noisy for normal development.

### Dry-run scripts with `--dry-run` flag

Build all refactoring scripts with a `--dry-run` mode that prints what it *would* do without writing files. Review the plan, then run for real. Cheap insurance.

---

## Notes

- **Category name uniqueness:** Core category names and namespace names must be disjoint. If a core category and a namespace share the same name, `docs.category` becomes ambiguous — you can't tell if a function is core (always available) or namespace (requires `import`). Enforce this with a test that asserts the two sets have no overlap.
- **Tags:** The `tags` field is defined in the type but don't populate it during this refactoring. Do a separate LLM-assisted pass later to assign tags to all functions.
- **`_isOperator` / `_prefereOperator`:** These are computed values derived from arity and operator patterns. Keep computing them in the reference index layer, not in the `docs` field.
- **Namespace registry threading:** When moving from the global mutable registry to constructor injection (Phase 4), the evaluator needs access to the namespace map. Today it reads from the global `Map` in `registry.ts`. After refactoring, the namespace map must be threaded through — likely via the `Builtin` interface or `EvaluateHelpers`. This needs careful design since the context stack is created deep inside the evaluator.
- **Backward compatibility:** Consider shipping a major version bump since the import paths and constructor API will change.
- **Testing throughout:** Run the full test suite after every phase. Don't batch phases.
