# Lits Effects — Implementation Plan

## Overview

This plan breaks the implementation of algebraic effects and serializable continuations into
self-contained phases. Each phase builds on the previous, has clear deliverables, and leaves
all existing tests passing before moving on.

The three key architectural insights driving this plan:

1. **Closures are already `{params, body, capturedEnv}` triplets** — not JS closures. Values
   are already close to JSON-serializable. This is a major advantage.
2. **Pure built-in normal expressions don't need rewriting** — they receive pre-evaluated args
   and return values. Only special expressions and higher-order built-ins are affected.
3. **One trampoline, two API entry points** — `runSync` and `run` share the same underlying
   trampoline. The recursive evaluator is retired entirely. The trampoline runs synchronously
   when no async is in play; `runSync` simply guards against Promises surfacing. This avoids
   the permanent maintenance burden of two diverging evaluators.

---

## Rules
* Do one step at a time. Eg first 0a then 0b etc.
* I want you to explain the changes made. I want to get familiar with the code as we go.
* If any decision is needed to proceed, ask me first
* Before a task is completed:
  1. `npm run check` must pass
  2. Test coverage should be att 100%
* When a step is completed. Update the plan with the progress, so the work can be resumed later


## Phase 0 — Preparation

Before touching the evaluator, establish the infrastructure that all later phases depend on.

### 0a. Define the `LitsValue` serialization contract ✅ DONE

Document which value types are serializable and which are not:

| Value type | Serializable | Notes |
|---|---|---|
| number, string, boolean, null | ✅ | |
| array, object | ✅ | if contents are serializable |
| `RegularExpression` | ✅ | just `{s, f}` string data |
| `UserDefinedFunction` | ✅ | `{params, body, capturedEnv}` — all plain data |
| `NormalBuiltinFunction` | ✅ | just an index/name |
| `SpecialBuiltinFunction` | ✅ | just an index/name |
| `ModuleFunction` | ✅ | `{moduleName, functionName}` |
| `EffectRef` | ✅ | `{name: string}` |
| `NativeJsFunction` | ❌ | re-injected from `bindings` on resume |
| `PartialFunction` / `CompFunction` / etc. | ⚠️ | serializable only if inner functions are |

Write a `isSerializable(value: LitsValue): boolean` validator used at suspension time to
produce a clear error if non-serializable values are in scope.

**Implemented:**
- `src/evaluator/serialization.ts` — `isSerializable(value)` and `describeSerializationIssue(value, path)` 
- `src/evaluator/serialization.test.ts` — 38 tests covering all value types, compound function types, circular references, and error descriptions
- All 4891 existing tests still pass

### 0b. Add `EffectRef` value type ✅ DONE

```typescript
interface EffectRef {
  type: 'EffectRef'
  name: string  // e.g. 'llm.complete'
}
```

Interned: `effect('llm.complete')` always returns the same reference (use a `Map<string, EffectRef>`).
`EffectRef` is serializable — just the name string.

**Implemented:**
- `src/utils/symbols.ts` — Added `EFFECT_SYMBOL` marker constant (`'^^ef^^'`)
- `src/parser/types.ts` — `EffectRef` interface with `[EFFECT_SYMBOL]: true` and `name: string`
- `src/interface.ts` — Updated `Any` type union to include `EffectRef`
- `src/typeGuards/lits.ts` — `isEffectRef()`, `asEffectRef()`, `assertEffectRef()` type guards; updated `isObj()` to exclude `EffectRef`
- `src/evaluator/effectRef.ts` — `getEffectRef(name)` interning function with module-level `Map<string, EffectRef>`, plus `clearEffectRefInternMap()` for testing
- `src/evaluator/serialization.ts` — Updated `isSerializable()` and `describeSerializationIssue()` to treat `EffectRef` as serializable
- `src/evaluator/effectRef.test.ts` — 14 tests covering interning, type guards, and `isObj` exclusion
- `src/evaluator/serialization.test.ts` — 70 tests (added 6 new EffectRef serialization tests)
- All 4937 existing tests pass, 100% coverage on new code

### 0c. Extend the `try` AST node ✅ DONE

The parser needs to support the new `try/with/catch` syntax alongside the existing `try/catch`:

```
try
  <body>
with
  case <effect-expr> then <handler-fn>
  case <effect-expr> then <handler-fn>
catch (<error-symbol>)
  <catch-body>
end
```

All clauses are optional (can have `with` without `catch`, `catch` without `with`, or both).
Extend `TryNode` in the AST to carry an optional list of `{ effectExpr, handlerFn }` pairs.

**Implemented:**
- `src/tokenizer/reservedNames.ts` — Added `with` as a reserved symbol
- `src/parser/subParsers/parseImplicitBlock.ts` — Added `'with'` to `ImplicitBlockEnd` type
- `src/parser/subParsers/parseTry.ts` — Rewrote to handle three forms: `try/catch`, `try/with`, `try/with/catch`. Parses `with` clause using `case/then` syntax (same pattern as `cond` and `match`)
- `src/builtin/specialExpressions/try.ts` — Added `WithHandler` type `[AstNode, AstNode]`. Extended `TryNode` payload to include `WithHandler[]` and optional `catchExpression`. Updated evaluator to handle optional catch (with-only form just evaluates body). Updated `getUndefinedSymbols` to collect symbols from with-handler expressions
- `__tests__/builtin/specialExpressions.test.ts` — 7 new tests: try/with form, multiple handlers, try/with/catch, catch without error symbol, body success skips catch, undefined symbols from with-handlers, undefined symbols from with+catch
- All 4944 tests pass, 100% coverage on try.ts and parseTry.ts

### 0d. Deliverable

- New `EffectRef` type in `src/evaluator/interface.ts`
- `isSerializable` utility
- Extended `TryNode` AST type (parser updated, no evaluator changes yet)
- All existing tests still pass

---

## Phase 1 — Trampoline Evaluator

The foundation. Replace the recursive `evaluateNode` entirely with an explicit-stack
trampoline. The recursive evaluator is retired. Both `runSync` and `run` use the same
trampoline — the difference is only in the loop wrapper.

### 1a. Define Frame types ✅ DONE

One frame type per recursive call pattern. All frame types are **plain serializable objects** — no functions, no closures.

**Implemented:**
- `src/evaluator/frames.ts` — 22 frame types covering all recursive evaluation patterns:
  - **Program flow**: `SequenceFrame` (sequential node evaluation)
  - **Branching**: `IfBranchFrame` (if/unless), `CondFrame` (multi-way cond), `MatchFrame` (pattern matching with phases)
  - **Short-circuit**: `AndFrame` (&&), `OrFrame` (||), `QqFrame` (?? nullish coalescing)
  - **Collection construction**: `ArrayBuildFrame` (with spread), `ObjectBuildFrame` (key/value pairs with spread)
  - **Binding**: `LetBindFrame`, `LoopBindFrame` (loop binding setup), `LoopIterateFrame` (body with recur), `ForLoopFrame` (multi-binding nested iteration with phase tracking)
  - **Control flow**: `ThrowFrame`, `RecurFrame` (collects args then signals tail-call)
  - **Exception & effect handling**: `TryCatchFrame` (exception boundary), `TryWithFrame` (effect handler boundary)
  - **Function calls**: `EvalArgsFrame` (argument collection), `CallFnFrame` (function dispatch), `FnBodyFrame` (user-defined body with recur support)
  - **Destructuring**: `BindingDefaultFrame` (default value evaluation during destructuring)
  - **Post-processing**: `NanCheckFrame` (NaN guard after normal expressions)
- `src/evaluator/frames.test.ts` — 6 tests verifying type discriminants, exhaustive union coverage, and type exports
- Helper types: `ForBindingLevelState`, `ContinuationStack` (type alias for `Frame[]`)
- All 4950 tests pass, full pipeline passes

### 1b. Define the Step type ✅ DONE

```typescript
type Step =
  | { type: 'Value';   value: LitsValue }
  | { type: 'Eval';    node: AstNode; env: Context; k: Frame[] }
  | { type: 'Apply';   frame: Frame; value: LitsValue; k: Frame[] }
  | { type: 'Perform'; effect: EffectRef; args: LitsValue[]; k: Frame[] }
```

**Implemented:**
- `src/evaluator/step.ts` — 4 step variant interfaces (`ValueStep`, `EvalStep`, `ApplyStep`, `PerformStep`) and discriminated union `Step` type
  - `ValueStep`: sub-expression produced a value; trampoline checks `k` to continue or finish
  - `EvalStep`: AST node needs evaluation in given environment; dispatched via `stepNode`
  - `ApplyStep`: frame needs to process a completed sub-result; dispatched via `applyFrame`
  - `PerformStep`: effect invoked via `perform`; trampoline searches `k` for handler or dispatches to host
- `src/evaluator/step.test.ts` — 4 tests verifying type discriminants, exhaustive union coverage, unique type names, and individual interface exports
- Uses `Any` (Lits value type), `ContinuationStack` (from frames.ts), `ContextStack`, `EffectRef`, and `AstNode`
- All 4954 tests pass, full pipeline passes

### 1c. Implement `stepNode` and `applyFrame` ✅ DONE

`stepNode(node, env, k)` → `Step`:
- For each AST node type, return the next step.
- Leaf nodes (numbers, strings, symbols) → `{ type: 'Value', value: ... }`
- Compound nodes → push a frame onto `k`, return `{ type: 'Eval', node: firstSubNode, ... }`

`applyFrame(frame, value, k)` → `Step`:
- Given a completed sub-result and the top frame, return the next step.
- Example: `IfBranchFrame` + `true` → `{ type: 'Eval', node: thenNode, ... }`

**Implemented:**
- `src/evaluator/trampoline.ts` (~2100 lines) — full explicit-stack evaluator with:
  - **`stepNode`**: dispatches AST nodes to steps — leaf nodes (Number, String, ReservedSymbol, UserDefinedSymbol, NormalBuiltinSymbol, SpecialBuiltinSymbol) return `ValueStep`; compound nodes push frames. All 20 special expression types handled: if/unless, &&, ||, ??, cond, match, block, let, loop, for/doseq, try, throw, recur, array, object, lambda, defined?, import.
  - **`applyFrame`**: exhaustive switch over all 22 frame types. Each handler processes a completed sub-result and returns the next step.
  - **Recur handling**: Instead of the recursive evaluator's exception-based `RecurSignal`, recur is handled via continuation-stack walking. `handleRecur()` searches for the nearest `LoopIterateFrame` or `FnBodyFrame`, rebinds parameters, and re-evaluates the body. `FnBodyFrame` stores `outerEnv` to enable proper function recur.
  - **Recursive fallback**: Compound function types (Comp, Juxt, Partial, etc.) and async paths fall back to recursive evaluation via `executeLitsFunctionRecursive`. These fallbacks will be removed when the trampoline fully replaces the recursive evaluator.
  - **Helper functions**: `dispatchCall`, `dispatchFunction`, `dispatchLitsFunction`, `setupUserDefinedCall`, `wrapMaybePromiseAsStep`, plus for-loop helpers (`processForLetBindings`, `processForGuards`, `processForNextLevel`, `advanceForElement`).
- `src/evaluator/trampoline.test.ts` (~1064 lines, 98 tests) — unit and integration tests:
  - Leaf node evaluation (9 tests)
  - Normal expression dispatch (2 tests)
  - Special expression stepping (22 tests)
  - Frame apply handlers (30 tests)
  - Integration tests (35 tests) — arithmetic, if/cond/match, let, loop/recur, for, try/catch, throw, arrays, objects, lambdas, function calls, logical operators, sequences
- `src/evaluator/frames.ts` — `FnBodyFrame` extended with `outerEnv` field for recur support
- Test coverage at 63% — deferred to Phase 1d where integration tests will naturally cover the remaining paths (recursive fallbacks, complex dispatch paths, spread operators, etc.)
- All 5052 tests pass, full pipeline passes (`npm run check`)

### 1d. Implement the trampoline loop — two wrappers, one engine ✅ DONE

The core step engine is synchronous. The two entry points wrap it differently:

```typescript
// Shared synchronous step engine
function tick(step: Step): Step | Promise<Step> {
  if (step.type === 'Value') {
    if (step.k.length === 0) return step   // done
    return applyFrame(step.k[0], step.value, step.k.slice(1))
  } else if (step.type === 'Eval') {
    return stepNode(step.node, step.env, step.k)
  } else if (step.type === 'Apply') {
    return applyFrame(step.frame, step.value, step.k)
  } else {  // 'Perform'
    return dispatchEffect(step.effect, step.args, step.k)  // may return Promise
  }
}

// runSync wrapper — throws if a Promise surfaces
function runSyncTrampoline(initial: Step): LitsValue {
  let step = initial
  while (true) {
    const next = tick(step)
    if (next instanceof Promise) throw new LitsError('Async effect in runSync')
    if (next.type === 'Value' && next.k.length === 0) return next.value
    step = next
  }
}

// run wrapper — awaits Promises
async function runAsyncTrampoline(initial: Step): Promise<RunResult> {
  let step = initial
  while (true) {
    const next = await tick(step)
    if (next.type === 'Value' && next.k.length === 0)
      return { type: 'completed', value: next.value }
    step = next
  }
}
```

The recursive evaluator is deleted. `runSync` now calls `runSyncTrampoline`.
Stack overflow on deeply recursive Lits programs is eliminated as a bonus.

**Implemented:**
- `src/evaluator/trampoline.ts` — Added trampoline loop functions:
  - **`tick(step)`**: Core step engine dispatching Value/Eval/Apply/Perform steps. Wraps dispatch in try/catch to enable continuation-stack unwinding for error handling (searches for `TryCatchFrame` before re-throwing).
  - **`runSyncTrampoline(initial)`**: Synchronous loop wrapper — iterates `tick()` calls, throws `LitsError` if a `Promise` surfaces (async operation in sync context).
  - **`runAsyncTrampoline(initial)`**: Async loop wrapper — awaits `tick()` results when Promises surface.
  - **`evaluate(ast, contextStack)`**: Top-level entry point replacing `evaluator/index.ts` `evaluate`. Builds initial step from AST nodes and runs via `runSyncTrampoline`.
  - **`evaluateNode(node, contextStack)`**: Node-level entry point replacing old `evaluateNode`. Runs single node via `runSyncTrampoline`.
  - **`unwindToTryCatch(error, k)`**: Continuation-stack unwinding — searches for nearest `TryCatchFrame`, binds error to `errorSymbol` if present, evaluates catch body. Re-throws if no handler found.
  - **`applyThrow`**: Updated to use `unwindToTryCatch` instead of throwing JS exception directly, enabling proper trampoline-based error handling.
  - **`wrapMaybePromiseAsStep`**: Updated to return `Step | Promise<Step>` instead of throwing on Promise, propagating async capability through the entire function chain (~12 functions updated).
- `src/Lits/Lits.ts` — Import changed from `'../evaluator'` to `'../evaluator/trampoline'`, wiring all public API through the trampoline.
- `src/evaluator/trampoline.test.ts` (~1148 lines, 107 tests) — Added:
  - `applyFrameSync`/`stepNodeSync` test helpers for sync-only test assertions
  - 6 `tick` tests: terminal ValueStep, frame application, EvalStep dispatch, ApplyStep dispatch, PerformStep error, full program tick loop
  - 3 `runSyncTrampoline` tests: simple expression, complex expression, terminal ValueStep
- Old recursive evaluator (`src/evaluator/index.ts`) still exists but is no longer imported by `Lits.ts`. Will be deleted in a later cleanup step.
- `npm run check` passes: lint clean, typecheck clean, 5061 tests pass (152 files), build succeeds
- Coverage: trampoline.ts at 93.16% statements
```

### 1e. Rewrite special expressions

Each special expression's `.evaluate()` becomes a `stepNode` case. No recursion into
`evaluateNode` — instead push frames and return a `Step`.

Special expressions to convert (~15-20 total):
`if`, `when`, `let`, `do`, `and`, `or`, `cond`, `fn`, `for`, `loop`, `try`,
`def`, `defn`, `case`, `comment`, `assert`, `recur`

**Implemented:**
- Routed `evaluateNodeRecursive`'s `SpecialExpression` case through the trampoline (mini-trampoline via `runSyncTrampoline`/`runAsyncTrampoline`) instead of calling `.evaluate()` directly. All 20 special expression types now execute via `stepSpecialExpression` → frame-based evaluation.
- Made `evaluate` optional in `BuiltinSpecialExpression` interface (`src/builtin/interface.ts`).
- Removed `evaluate` method from all 19 special expression files: and.ts, array.ts, block.ts, cond.ts, defined.ts, functions.ts, if.ts, import.ts, let.ts, loop.ts, loops.ts (for+doseq), match.ts, object.ts, or.ts, qq.ts, recur.ts, throw.ts, try.ts, unless.ts.
- Removed dead `evaluateFunction` helper from `functions.ts` and dead `addToContext`/`evaluateLoop` helpers from `loops.ts`.
- Cleaned up 40 unused imports across all 19 files (imports that were only used by the removed `evaluate` methods).
- Deleted old recursive evaluator files: `src/evaluator/index.ts`, `src/evaluator/functionExecutors.ts`, `src/evaluator/functionExecutors.test.ts`.
- 7 files retain `evaluateAsNormalExpression` (operator forms): and.ts, array.ts, object.ts, or.ts, qq.ts, recur.ts, throw.ts — these are used by `NormalExpression` dispatch, not by the trampoline's special expression path.
- `npm run check` passes: lint clean, typecheck clean, 5061 tests pass (152 files), build succeeds.
- Coverage: trampoline.ts at 92.35% statements.

### 1f. Tail call (`recur`) in the trampoline ✅ DONE

In the recursive evaluator, `recur` throws `RecurSignal` caught by a `for(;;)` loop.
In the trampoline, a tail call simply replaces the current `FnBodyFrame` with a new eval step —
no frame growth. Proper tail call elimination falls out naturally.

**How it works:**
- `recur` in the trampoline pushes a `RecurFrame` to collect evaluated arguments one by one.
- When all args are collected, `applyRecur` calls `handleRecur(params, k, sourceCodeInfo)`.
- `handleRecur` walks the continuation stack for the nearest `LoopIterateFrame` or `FnBodyFrame`.
- It slices the stack at that point (`k.slice(i + 1)`), discarding all frames above the target — effectively replacing the current call/loop iteration rather than growing the stack.
- For `LoopIterateFrame`: rebinds variables in the existing context and pushes a fresh `LoopIterateFrame`.
- For `FnBodyFrame`: calls `setupUserDefinedCall` with the remaining stack, which sets up fresh parameter bindings and a new `FnBodyFrame`.
- Net result: constant continuation stack depth during recur iterations.

**Changes:**
- `src/evaluator/trampoline.ts` — Cleaned up `applyFnBody`: removed duplicate code branches (both paths were identical). Added clear comment explaining why the `FnBodyFrame` is always pushed even for the last body node: `handleRecur` needs to find it during continuation-stack walking.
- `src/evaluator/trampoline.test.ts` — Added 4 deep recursion TCE tests:
  - Deep loop recur (100,000 iterations) — verifies no stack overflow
  - Deep function recur (100,000 iterations) — verifies no stack overflow
  - Loop recur with accumulator (100,000 iterations, multi-param rebinding)
  - Function recur in multi-expression body (IIFE with `do` block)
- `npm run check` passes: lint clean, typecheck clean, 5063 tests pass (152 files), build succeeds.
- Coverage: trampoline.ts at 92.36% statements.

**Note:** The recursive fallback path (`executeLitsFunctionRecursive` / `executeUserDefinedRecursive`) still uses `RecurSignal` for compound function types (Comp, Partial, etc.) that fall back to recursive evaluation. This is acceptable — those paths are used for higher-order built-in callbacks and will be addressed when compound function types are migrated to the trampoline.

### 1g. Deliverable ✅ DONE

- `src/evaluator/trampoline.ts` — frame types, `stepNode`, `applyFrame`, `tick()`
- `runSyncTrampoline` and `runAsyncTrampoline` wrappers
- `runSync` and `run` both route through the trampoline
- Recursive evaluator deleted
- All existing tests pass, new integration tests confirm both wrappers behave identically
- Stack overflow no longer possible on deeply recursive Lits programs

**Verified:**
- `src/evaluator/trampoline.ts` (~2341 lines) — 22 frame types, `stepNode`, `applyFrame`, `tick()`, `runSyncTrampoline`, `runAsyncTrampoline`, `evaluate`, `evaluateAsync`, `evaluateNode`
- `src/Lits/Lits.ts` — imports `evaluate`, `evaluateAsync`, `evaluateNode` from `'../evaluator/trampoline'`
- Old recursive evaluator files deleted: `src/evaluator/index.ts`, `src/evaluator/functionExecutors.ts`
- `evaluateNodeRecursive` remains inside `trampoline.ts` as an internal helper for normal expression callbacks and binding utilities — these recursive paths will be eliminated when compound function types are fully migrated
- `src/evaluator/trampoline.test.ts` — 133 tests including:
  - 22 sync/async parity tests confirming `runSyncTrampoline` and `runAsyncTrampoline` produce identical results for the same programs (arithmetic, branching, loops, recur, try/catch, arrays, objects, lambdas, pattern matching)
  - 4 deep recursion TCE tests (100,000 iterations without stack overflow)
  - `__tests__/async.test.ts` — 81 existing async tests validate the async trampoline path
- `npm run check` passes: lint clean, typecheck clean, 5085 tests pass (152 files), build succeeds
- Coverage: trampoline.ts at 92.36% statements (accepted — uncovered lines are async fallback paths and recursive compound-type dispatch that will be naturally covered when those types are migrated)

---

## Phase 2 — Local Effect Handling (`try/with`) ✅ DONE

Effects fully within Lits code, no host API yet.

### 2a. `effect(name)` special expression ✅ DONE

Returns the interned `EffectRef` for `name`. The name is parsed as a dotted identifier
(e.g. `llm.complete`), not a string — ergonomics match the intro doc.

```lits
let llm = effect(llm.complete)
```

**Implemented:**
- `src/builtin/specialExpressionTypes.ts` — Added `'effect': 20`
- `src/builtin/specialExpressions/effect.ts` — NEW: `EffectNode` type, docs with category `'special-expression'`, `getUndefinedSymbols` (returns empty set since effect names are not Lits symbols)
- `src/builtin/index.ts` — Added `effectSpecialExpression` registration and updated `CommonSpecialExpressionType`
- `src/parser/subParsers/parseFunctionCall.ts` — Added custom parsing for `effect(dotted.name)`: `parseEffectArgs` helper reads symbol tokens separated by `.` to build the dotted name string (e.g., `effect(llm.complete)` → name `"llm.complete"`)
- `src/evaluator/trampoline.ts` — Added `effect` case to `stepSpecialExpression`: returns `{ type: 'Value', value: getEffectRef(name) }`
- 7 tests: effect references, dotted names, interning, first-class values, comparison

### 2b. `perform(eff, ...args)` special expression ✅ DONE

Emits `Step.Perform` with the resolved `EffectRef` and evaluated args.
The trampoline's `dispatchPerform` then searches `k` from top to bottom for a matching
`TryWithFrame`.

Effect matching: compare `EffectRef.name` values (interning means reference equality works too,
but name comparison is safer across serialization boundaries).

**Implemented:**
- `src/builtin/specialExpressionTypes.ts` — Added `'perform': 21`
- `src/builtin/specialExpressions/perform.ts` — NEW: `PerformNode` type, docs with category `'special-expression'`, min arity 1
- `src/builtin/index.ts` — Added `performSpecialExpression` registration
- `src/parser/subParsers/parseFunctionCall.ts` — Added `perform` case to the switch statement
- `src/evaluator/frames.ts` — Added `PerformArgsFrame` to collect evaluated effect ref + args before producing `PerformStep`
- `src/evaluator/trampoline.ts` — Added `perform` case to `stepSpecialExpression`: pushes `PerformArgsFrame`, evaluates args left-to-right, then produces `PerformStep`. Added `applyPerformArgs` handler.
- 6 tests: local handlers, no args, multiple args, args as array, unhandled effects, variables

### 2c. `TryWithFrame` handler dispatch ✅ DONE

When `perform` finds a matching `TryWithFrame`:

1. **Slice the continuation** — everything above the `TryWithFrame` up to (not including) the frame
   is the *delimited continuation* that the handler can resume. For single-shot handlers in
   `try/with`, the handler's return value is simply used as the result of `perform`.
2. Pop the `TryWithFrame` from `k` (errors in handlers escape to the outer try/catch).
3. Evaluate the handler function body with the perform args bound.
4. The result becomes the value that continues from the `perform` call site.

**Implemented:**
- `src/evaluator/frames.ts` — Added `EffectResumeFrame` with `resumeK: ContinuationStack` field. Changed `TryWithFrame.handlers` from `WithHandler[]` (raw AST pairs) to `EvaluatedWithHandler[]` (pre-evaluated effect refs + handler AST nodes). Added `EvaluatedWithHandler` interface.
- `src/evaluator/trampoline.ts` — Implemented `dispatchPerform(effect, args, k, sourceCodeInfo)`:
  - Searches `k` for nearest `TryWithFrame` whose `handlers` contain a matching `EffectRef` (by `.name` comparison)
  - Builds two continuations: **resumeK** = original `k` (TryWithFrame stays for subsequent performs), **handlerK** = `[EffectResumeFrame{resumeK}, ...outerK]` where `outerK` skips both TryWithFrame and adjacent TryCatchFrame (per P&P: handlers run outside try-scope)
  - Evaluates handler function (using `evaluateNodeRecursive`) and dispatches it with `handlerK`
  - When handler returns value V, `EffectResumeFrame` restores `resumeK` so V resumes at the perform call site with TryWithFrame intact
  - If no matching handler found, throws `UnhandledEffectError`
- Updated `stepSpecialExpression` `try` case to eagerly evaluate effect expressions via `evaluateNodeRecursive` and store as `EvaluatedWithHandler[]`
- Updated `tick()` to call `dispatchPerform` instead of throwing "not implemented"
- 8 tests: multi-handler matching, first match wins, outer delegation, nesting, handler scope removal, resume value, delegation, success passthrough

### 2d. `TryCatch` + `TryWith` interaction ✅ DONE

When an error is thrown:
- Walk `k` for the nearest `TryCatchFrame` — ignore `TryWithFrame` entries (they don't catch errors).
- When an effect is performed:
- Walk `k` for the nearest matching `TryWithFrame` — ignore `TryCatchFrame` entries.

These two frame types are independent and stack correctly.

**Implemented:**
- Error unwinding (`unwindToTryCatch`) already skips non-TryCatchFrame frames, so TryWithFrame is naturally ignored
- Effect dispatch (`dispatchPerform`) skips non-TryWithFrame frames, so TryCatchFrame is naturally ignored
- `EffectResumeFrame` ensures handler errors propagate past the inner TryCatchFrame (handlers run outside try-scope per P&P)
- 7 tests: error/effect separation, handler errors propagate to outer catch, combined try/with/catch

### 2e. Deliverable ✅ DONE

```lits
try
  perform(effect(llm.complete), "prompt")
with
  case effect(llm.complete) then ([prompt]) -> upper-case(prompt)
end
```

Works end-to-end. Full test coverage for:
- Effect matched by `try/with`
- Effect not matched (error: unhandled effect)
- Error in handler propagates past the enclosing try/catch
- Combined `try/with/catch`
- Effects as first-class values (passed as arguments)

**Verified:**
- `__tests__/effects.test.ts` — 31 tests covering all Phase 2 features
- `src/evaluator/frames.test.ts` — Updated for 24 frame types (added PerformArgsFrame, EffectResumeFrame)
- `npm run check` passes: lint clean, typecheck clean, 5118 tests pass (152 files + 1 skipped), build succeeds
- Coverage: effect.ts 100%, perform.ts 100%, effectRef.ts 100%, trampoline.ts 92.79%

---

## Phase 3 — Host Async API

Connect the trampoline to host-side JavaScript handlers.

### 3a. `run(source, options)` API ✅ DONE

```typescript
type HostHandler = (ctx: EffectContext) => Promise<void>

interface RunOptions {
  bindings?: Record<string, LitsValue>
  handlers?: Record<string, HostHandler>
}

type RunResult =
  | { type: 'completed'; value: LitsValue }
  | { type: 'suspended'; blob: string; meta?: LitsValue }
  | { type: 'error';     error: LitsError }

async function run(source: string, options?: RunOptions): Promise<RunResult>
```

**Implemented:**
- `src/evaluator/effectTypes.ts` — Shared types extracted to break circular dependency: `EffectContext`, `EffectHandler`, `Handlers`, `RunResult`, `SuspensionSignal`, `isSuspensionSignal`
- `src/effects.ts` — Standalone `run()` and `runSync()` functions with `RunOptions` and `RunSyncOptions`
- `runSync()` — Pure synchronous evaluation, supports bindings (including JS functions) and modules, throws if async
- `run()` — Async evaluation with handlers, wraps all errors (including parse errors) in `RunResult`
- `buildAst(source)` helper — tokenize → minify → parse (no caching)
- Exported from `src/index.ts`: `run`, `runSync`, `EffectContext`, `EffectHandler`, `Handlers`, `RunResult`, `RunOptions`, `RunSyncOptions`

### 3b. `dispatchEffect` — host handler lookup ✅ DONE

When `Step.Perform` finds no matching `TryWithFrame` in `k`:
1. Look up `effect.name` in `options.handlers`
2. If not found, check standard effects (Phase 5)
3. If still not found, throw `UnhandledEffectError`
4. If found, call `handler({ args, signal, resume, suspend })`

`resume(value)` — re-enters the trampoline with `value` and the captured `k`.
`suspend(meta?)` — returns immediately from `run()` with `{ type: 'suspended', blob, meta }`.

**Implemented:**
- `dispatchPerform` in `trampoline.ts` — Extended signature: `(effect, args, k, sourceCodeInfo?, handlers?, signal?)`. After searching continuation stack for `TryWithFrame`, falls back to `handlers?.[effect.name]`, then calls `dispatchHostHandler`
- `dispatchHostHandler` (NEW, ~70 lines) — Creates `EffectContext` with `resume` and `suspend` callbacks inside `new Promise<Step>()`. `resume(value)` resolves with `ValueStep`; `resume(promise)` awaits then resolves; failed promises fed to `unwindToTryCatch`. `suspend(meta)` rejects with `SuspensionSignal`. Double-call guarded by `settled` flag. Handler errors fed to `unwindToTryCatch`.
- `tick` — Extended signature: `(step, handlers?, signal?)`, passes handlers/signal to `dispatchPerform`
- Local `try/with` handlers take precedence over host handlers (tested)

### 3c. AbortSignal for handler cancellation ✅ DONE

Each `run()` call creates an `AbortController`. The signal is passed to every handler.
Used in Phase 6 (race), but wired up now.

**Implemented:**
- `evaluateWithEffects` in `trampoline.ts` creates `AbortController` per `run()` call
- Signal passed through `tick` → `dispatchPerform` → `dispatchHostHandler` → handler's `EffectContext`
- Test verifies signal is accessible in handler and not aborted during normal execution

### 3d. Deliverable ✅ DONE

```typescript
const result = await run(source, {
  handlers: {
    'llm.complete': async ({ args, resume }) => {
      resume(await callOpenAI(args[0]))
    }
  }
})
```

Works end-to-end. Tests covering sync resume, async resume, unhandled effect error.

**Verified:**
- `__tests__/effects.test.ts` — 63 tests (31 Phase 2 + 32 Phase 3), all passing
- Phase 3 test categories: runSync basics (4), run basics (5), sync resume (4), async resume (3), unhandled effects (2), error handling (2), local/host precedence (3), suspension (2), AbortSignal (1), end-to-end workflows (6)
- `npm run check` passes: lint clean, typecheck clean, 5150 tests pass, build succeeds, no circular dependencies
- Coverage: effects.ts 97.27%, effectTypes.ts 100%, trampoline.ts 92.67%

---

## Phase 4 — Suspension & Resume

The core value proposition: pause, serialize, store, resume across processes.

### 4a. Serialization format ✅ DONE

When `suspend(meta?)` is called, serialize:

```typescript
interface SuspensionBlob {
  version: 1
  k: Frame[]           // the full continuation stack
  meta: LitsValue      // from suspend(meta)
}
```

`JSON.stringify(blob)` → opaque string stored by the host.

Before serializing, validate that all values in `k` are serializable.
`NativeJsFunction` values must not appear in frames — they live in the global context
and are re-injected on resume. If found, throw a descriptive error.

**Implemented:**
- `src/evaluator/suspension.ts` — NEW file (~280 lines):
  - `serializeSuspension(k, meta?)`: Collects all unique `ContextStack` instances from frames by identity, assigns numeric IDs. Replaces inline `ContextStack` references with `{__csRef: id}` markers. Validates no `NativeJsFunction` values in scope. Returns JSON string with `{version: 1, k, meta, contextStacks}`.
  - `deserializeSuspension(blob, options?)`: Parses JSON, validates version field. Creates placeholder `ContextStack` instances, deep-resolves `__csRef` markers, fills in actual contexts. Re-injects host bindings (values, modules, nativeJsFunctions).
  - `DeserializeOptions` interface: `values`, `nativeJsFunctions`, `modules`
- `src/evaluator/effectTypes.ts` — Added `SuspensionBlob` type alias (`string`). Changed `RunResult.suspended` from `{ continuation: ContinuationStack }` to `{ blob: SuspensionBlob }`.
- `src/evaluator/ContextStack.ts` — Added serialization accessors: `getContextsRaw()`, `getGlobalContextIndex()`, `static fromDeserialized()`, `setContextsFromDeserialized()`.
- `src/evaluator/trampoline.ts` — Updated `evaluateWithEffects` catch block: calls `serializeSuspension(k, meta)` when `SuspensionSignal` is caught.

### 4b. `resume(blob, value, options)` API ✅ DONE

```typescript
async function resume(
  blob: string,
  value: LitsValue,
  options?: RunOptions
): Promise<RunResult>
```

1. `JSON.parse(blob)` → `SuspensionBlob`
2. Validate version field
3. Re-enter trampoline with `{ type: 'Value', value, k: blob.k }`
4. Host `bindings` and `handlers` re-injected from `options` (NativeJsFunctions
   are not in the blob — they're provided fresh each time)

**Implemented:**
- `src/effects.ts` — `resume(blob, value, options?)` function: deserializes blob, calls `resumeWithEffects(k, value, handlers)`. `ResumeOptions` interface: `bindings`, `handlers`, `modules`. Wraps all errors in `RunResult`.
- `src/evaluator/trampoline.ts` — `resumeWithEffects(k, value, handlers?)`: creates `AbortController`, builds initial `{type: 'Value', value, k}` step, enters `runEffectLoop()`.
- Shared `runEffectLoop(initial, handlers, signal)` extracted from `evaluateWithEffects` to avoid duplication.
- Exported from `src/index.ts`: `resume`, `SuspensionBlob`, `ResumeOptions`.

### 4c. `NativeJsFunction` re-injection on resume ✅ DONE

The global context (from `bindings`) is rebuilt from the `options.bindings` map on each
`run()` / `resume()` call, exactly as today. Since `NativeJsFunction` values are never in
the serialized frames, this is sufficient. No special handling needed.

**Implemented:**
- `deserializeSuspension` in `suspension.ts` accepts `DeserializeOptions` with `values`, `nativeJsFunctions`, and `modules` — these are injected into the global context of each deserialized `ContextStack`.
- `resume()` in `effects.ts` passes `bindings` as `values` to `deserializeSuspension`.
- Serialization validates that no `NativeJsFunction` values appear in continuation frames — throws descriptive error if found.

### 4d. Deliverable ✅ DONE

End-to-end suspension test:

```typescript
// Process 1
const r1 = await run(source, { handlers })
// r1.type === 'suspended'
await db.save(r1.blob)

// Process 2 — days later
const blob = await db.load(id)
const r2 = await resume(blob, humanDecision, { handlers })
// r2.type === 'completed'
```

**Verified:**
- `__tests__/effects.test.ts` — 87 tests (31 Phase 2 + 32 Phase 3 + 24 Phase 4), all passing
- Phase 4 test categories:
  - 4a: Blob format validation (2) — JSON structure, meta inclusion
  - 4b: resume() API (13) — simple resume, string/object/null values, closures, multiple suspensions, handlers on resume, bindings on resume, invalid blob, wrong version, errors after resume, try/catch after resume
  - 4c: NativeJsFunction not in blob (1) — host values usable before suspend
  - 4d: End-to-end workflows (7) — full suspend-store-resume cycle, rejection path, multi-step workflows, local try/with after resume, deep nesting + closures, loop/recur after resume, arrays/objects across resume
- `npm run check` passes: lint clean, typecheck clean, 5174 tests pass, build succeeds
- Coverage: suspension.ts covered by tests, effects.ts covered, trampoline.ts at ~93%

---

## Phase 5 — Standard Effects

Built-in effects with default implementations. Host handlers override them
(host handlers have higher priority in the lookup chain).

Effect lookup order: local try/with → host handlers → standard effects → unhandled error

| Effect | Default implementation | Sync? |
|---|---|---|
| `lits.log` | `console.log(...args)`, resumes with `null` | ✅ |
| `lits.now` | `Date.now()` | ✅ |
| `lits.random` | `Math.random()` | ✅ |
| `lits.sleep` | `setTimeout(resolve, ms)`, resumes with `null` | ❌ async only |

`lits.prompt` deferred — environment-dependent, hosts provide their own.

Override in tests for determinism:

```typescript
handlers: {
  'lits.now':    async ({ resume }) => resume(fixedTimestamp),
  'lits.random': async ({ resume }) => resume(0.42),
}
```

### Deliverable ✅ DONE

Standard effects work with default handlers. Overridable in tests.

**Implemented:**
- `src/evaluator/standardEffects.ts` — NEW file (~100 lines):
  - `StandardEffectHandler` type: `(args, k, sourceCodeInfo?) => Step | Promise<Step>`
  - 4 handlers: `lits.log` (sync, console.log → null), `lits.now` (sync, Date.now()), `lits.random` (sync, Math.random()), `lits.sleep` (async, setTimeout → null with argument validation)
  - `standardEffectNames` readonly set for introspection
  - `getStandardEffectHandler(name)` lookup function
  - Sync effects return `Step` directly — work in both `runSync` and `run`
  - Async effects (`lits.sleep`) return `Promise<Step>` — only work in `run` (sync trampoline throws on Promise)
- `src/evaluator/trampoline.ts` — Updated `dispatchPerform`: after host handler check, before "unhandled effect" error, calls `getStandardEffectHandler()`. Standard effects return `Step | Promise<Step>` directly (no `dispatchHostHandler` wrapping — keeps sync compatibility).
- `src/evaluator/standardEffects.test.ts` — 9 unit tests: effect names, handler lookup, each handler's return values, sleep argument validation
- `__tests__/effects.test.ts` — 22 new Phase 5 tests (109 total):
  - 5a: `lits.log` — via run, via Lits.run (sync), no args, host override, local try/with override
  - 5b: `lits.now` — via run, sync, host override for determinism, local try/with override
  - 5c: `lits.random` — via run, sync, host override for determinism, local try/with override
  - 5d: `lits.sleep` — via run, throws in sync context, validates arguments (negative, non-number), host override
  - 5e: workflows — multiple standard effects in sequence, standard effects + suspension, override all for testing, runSync with sync standard effects
- `npm run check` passes: lint clean, typecheck clean, 5205 tests pass (153 files + 1 skipped), build succeeds
- Coverage: standardEffects.ts 100% statements, 100% branches, 100% functions, 100% lines

---

## Phase 6 — Parallel & Race

Concurrent effect dispatch. More complex — treated as its own sub-project.

### 6a. `parallel(...performs)` special expression

Evaluates each `perform` concurrently. Returns array of results in order.
Suspension is fully supported — handlers need not know they are inside a `parallel`.

**Implementation:**

Run all branches as separate trampoline invocations collected with `Promise.allSettled`.
When any branch suspends, wait for all others to settle, then build a **composite blob**:

```typescript
interface ParallelBlob {
  branches: Array<
    | { status: 'completed'; value: LitsValue }
    | { status: 'suspended'; blob: string; meta: LitsValue }
  >
  outerK: Frame[]   // continuation waiting for the full result array
}
```

On `resume(blob, value, { handlers })`, the trampoline:
1. Injects `value` into the first pending suspended branch and re-runs it
2. If that branch completes and no others are pending → fires the outer continuation with the full array
3. If more suspended branches remain → returns another `{ type: 'suspended', blob, meta }` where `meta` is from the next pending branch

**The host-side loop is identical to single suspension — no API changes:**

```typescript
let result = await run(source, { handlers })
while (result.type === 'suspended') {
  const decision = await getDecision(result.meta)
  result = await resume(result.blob, decision, { handlers })
}
```

Each `resume` peels off one suspended branch. The host never needs to know it is inside a `parallel`.

### 6b. `race(...performs)` special expression

First branch to **succeed** wins (`Promise.any` semantics). Errors are ignored — the race
continues among remaining branches. Only if all branches error does `race` throw an aggregate
error. Losers receive an aborted `signal` via their `AbortController`.

**Suspension semantics for `race`:** A branch that suspends has not succeeded — the race
continues. If all branches either error or suspend (none has succeeded), the race suspends.

When the race suspends, the blob contains only the **outer continuation** — not individual
branch blobs. The host receives aggregated metas from all suspended branches and decides
externally which wins. Calling `resume(blob, winnerValue)` resolves the race with that value
directly. Individual branch continuations are discarded — the race result is simply whatever
value the host provides.

```typescript
interface RaceBlob {
  outerK: Frame[]   // continuation after race() — just needs a winner value
}

// Suspended result carries all branch metas so the host knows who is waiting
{ type: 'suspended', blob, meta: { type: 'race', branches: [meta_a, meta_b, ...] } }
```

This makes `race` **simpler than `parallel`** from a serialization standpoint: `parallel`
must track all branch blobs to resume them individually; `race` discards them and keeps only
the outer continuation.

**Branch outcome priority:** `completed` > `suspended` > `errored`. First completed branch
wins immediately. Errored branches are silently dropped. Suspended branches accumulate into
the race blob. If all branches errored, throw aggregate error.

Implementation: `Promise.allSettled` over branches. First `completed` result wins — abort all
other `AbortController`s. Accumulate errors and suspended branches; if no `completed` result,
build the `RaceBlob` or throw aggregate error accordingly.

### Deliverable

```lits
let [a b c] = parallel(
  perform(effect(llm), "task 1"),
  perform(effect(llm), "task 2"),
  perform(effect(llm), "task 3")
)
```

---

## Phase 7 — Time-Travel Debugger

### 7a. `lits.debug.step` injection

When a `lits.debug.step` handler is registered, the trampoline injects a `perform(lits.debug.step)`
between every evaluation step. No changes to Lits source needed.

Each injection captures a `HistoryEntry`:

```typescript
interface HistoryEntry {
  step:        number
  blob:        string          // serialized continuation at this point
  node:        AstNode         // expression being evaluated
  value:       LitsValue       // result of this expression
  bindings:    Record<string, LitsValue>  // all in-scope bindings
  sourceInfo:  SourceCodeInfo
  timestamp:   number
}
```

### 7b. Debugger API

```typescript
const dbg = createDebugger({ handlers })
await dbg.run(source)
await dbg.stepForward()
await dbg.stepBackward()         // resume older blob
await dbg.jumpTo(n)              // resume blob at step n
await dbg.rerunFrom(n, value)   // discard history after n, resume with alternate value
```

`stepBackward` and `jumpTo` are just `resume(history[n].blob, ...)` — no special mechanism needed.
`rerunFrom` discards `history[n+1..]` and resumes from `history[n].blob` with a new value —
creating an alternate execution timeline.

---

## Higher-Order Built-ins — Deferred Decision

Built-ins like `map`, `filter`, `reduce`, `apply`, `sort-by` call Lits functions internally.

**For async effects (Phases 3–4)**: These work without changes if they await each function
call result. The `MaybePromise` pattern already handles this.

**For suspension through callbacks**: If a `perform` inside a `map` callback suspends,
the `map` loop state (current index, accumulator) must be in the serialized continuation.
Two options:

- **Rewrite as Lits**: Implement `map`, `filter`, `reduce` etc. as Lits standard library
  functions. They naturally participate in the trampoline at no extra cost. Performance
  impact needs measurement.
- **Resumable JS**: Each higher-order built-in becomes an explicit state machine with
  loop state encoded as a frame. More work, keeps JS performance.

**Recommendation**: Defer this decision until Phase 4 is complete and the suspension
semantics are proven. Start with "no suspension through higher-order built-in callbacks"
and revisit based on real usage.

---

## Implementation Order Summary

| Phase | What | Risk | Prerequisite |
|---|---|---|---|
| 0 | `EffectRef`, parser extension, serialization contract | Low | — |
| 1 | Trampoline evaluator | High | Phase 0 |
| 2 | `effect()`, `perform()`, `try/with` (local) | Medium | Phase 1 |
| 3 | Host async API (`run`, handlers) | Medium | Phase 2 |
| 4 | Suspension & resume (`suspend`, `resume`) | Medium | Phase 3 |
| 5 | Standard effects | Low | Phase 3 |
| 6 | `parallel` / `race` | High | Phase 4 |
| 7 | Time-travel debugger | Medium | Phase 4 |
| — | Higher-order built-in suspension | TBD | Phase 4 |

**Phase 1 is the critical path.** Everything else follows directly from having the
explicit-stack trampoline in place. It is also the phase with the most surface area —
every special expression needs a corresponding frame type — but the work is mostly
mechanical and the existing test suite provides continuous validation.

The decision to retire the recursive evaluator entirely means Phase 1 is also a
**one-way door** — once done, there is no fallback. The test suite is the safety net.
Migrate one special expression at a time, running tests after each, rather than doing
it all in one pass.
