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

### 0c. Extend the `try` AST node

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

### 1a. Define Frame types

One frame type per recursive call pattern:

```typescript
type Frame =
  | { type: 'Sequence';     remaining: AstNode[]; env: Context }
  | { type: 'IfBranch';     thenNode: AstNode; elseNode: AstNode; env: Context }
  | { type: 'LetBinding';   name: string; remaining: Binding[]; body: AstNode[]; env: Context }
  | { type: 'FnArg';        fn: LitsValue; collectedArgs: LitsValue[]; remaining: AstNode[]; env: Context }
  | { type: 'FnBody';       /* return value passes through */ }
  | { type: 'TryCatch';     errorSymbol: string | null; catchNode: AstNode; env: Context }
  | { type: 'TryWith';      handlers: { effect: AstNode; handlerFn: AstNode }[]; env: Context }
  | { type: 'AndShort';     remaining: AstNode[]; env: Context }
  | { type: 'OrShort';      remaining: AstNode[]; env: Context }
  | { type: 'CondBranch';   branches: CondBranch[]; env: Context }
  | { type: 'WhenBody';     body: AstNode; env: Context }
  // ... one per special expression that recurses into evaluateNode
```

All frame types are **plain serializable objects** — no functions, no closures.

### 1b. Define the Step type

```typescript
type Step =
  | { type: 'Value';   value: LitsValue }
  | { type: 'Eval';    node: AstNode; env: Context; k: Frame[] }
  | { type: 'Apply';   frame: Frame; value: LitsValue; k: Frame[] }
  | { type: 'Perform'; effect: EffectRef; args: LitsValue[]; k: Frame[] }
```

### 1c. Implement `stepNode` and `applyFrame`

`stepNode(node, env, k)` → `Step`:
- For each AST node type, return the next step.
- Leaf nodes (numbers, strings, symbols) → `{ type: 'Value', value: ... }`
- Compound nodes → push a frame onto `k`, return `{ type: 'Eval', node: firstSubNode, ... }`

`applyFrame(frame, value, k)` → `Step`:
- Given a completed sub-result and the top frame, return the next step.
- Example: `IfBranchFrame` + `true` → `{ type: 'Eval', node: thenNode, ... }`

### 1d. Implement the trampoline loop — two wrappers, one engine

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
```

### 1e. Rewrite special expressions

Each special expression's `.evaluate()` becomes a `stepNode` case. No recursion into
`evaluateNode` — instead push frames and return a `Step`.

Special expressions to convert (~15-20 total):
`if`, `when`, `let`, `do`, `and`, `or`, `cond`, `fn`, `for`, `loop`, `try`,
`def`, `defn`, `case`, `comment`, `assert`, `recur`

### 1f. Tail call (`recur`) in the trampoline

In the recursive evaluator, `recur` throws `RecurSignal` caught by a `for(;;)` loop.
In the trampoline, a tail call simply replaces the current `FnBodyFrame` with a new eval step —
no frame growth. Proper tail call elimination falls out naturally.

### 1g. Deliverable

- `src/evaluator/trampoline.ts` — frame types, `stepNode`, `applyFrame`, `tick()`
- `runSyncTrampoline` and `runAsyncTrampoline` wrappers
- `runSync` and `run` both route through the trampoline
- Recursive evaluator deleted
- All existing tests pass, new integration tests confirm both wrappers behave identically
- Stack overflow no longer possible on deeply recursive Lits programs

---

## Phase 2 — Local Effect Handling (`try/with`)

Effects fully within Lits code, no host API yet.

### 2a. `effect(name)` special expression

Returns the interned `EffectRef` for `name`. The name is parsed as a dotted identifier
(e.g. `llm.complete`), not a string — ergonomics match the intro doc.

```lits
let llm = effect(llm.complete)
```

### 2b. `perform(eff, ...args)` special expression

Emits `Step.Perform` with the resolved `EffectRef` and evaluated args.
The trampoline's `dispatchEffect` then searches `k` from top to bottom for a matching
`TryWithFrame`.

Effect matching: compare `EffectRef.name` values (interning means reference equality works too,
but name comparison is safer across serialization boundaries).

### 2c. `TryWithFrame` handler dispatch

When `perform` finds a matching `TryWithFrame`:

1. **Slice the continuation** — everything above the `TryWithFrame` up to (not including) the frame
   is the *delimited continuation* that the handler can resume. For single-shot handlers in
   `try/with`, the handler's return value is simply used as the result of `perform`.
2. Pop the `TryWithFrame` from `k` (errors in handlers escape to the outer try/catch).
3. Evaluate the handler function body with the perform args bound.
4. The result becomes the value that continues from the `perform` call site.

### 2d. `TryCatch` + `TryWith` interaction

When an error is thrown:
- Walk `k` for the nearest `TryCatchFrame` — ignore `TryWithFrame` entries (they don't catch errors).
- When an effect is performed:
- Walk `k` for the nearest matching `TryWithFrame` — ignore `TryCatchFrame` entries.

These two frame types are independent and stack correctly.

### 2e. Deliverable

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

---

## Phase 3 — Host Async API

Connect the trampoline to host-side JavaScript handlers.

### 3a. `run(source, options)` API

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

### 3b. `dispatchEffect` — host handler lookup

When `Step.Perform` finds no matching `TryWithFrame` in `k`:
1. Look up `effect.name` in `options.handlers`
2. If not found, check standard effects (Phase 5)
3. If still not found, throw `UnhandledEffectError`
4. If found, call `handler({ args, signal, resume, suspend })`

`resume(value)` — re-enters the trampoline with `value` and the captured `k`.
`suspend(meta?)` — returns immediately from `run()` with `{ type: 'suspended', blob, meta }`.

### 3c. AbortSignal for handler cancellation

Each `run()` call creates an `AbortController`. The signal is passed to every handler.
Used in Phase 6 (race), but wired up now.

### 3d. Deliverable

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

---

## Phase 4 — Suspension & Resume

The core value proposition: pause, serialize, store, resume across processes.

### 4a. Serialization format

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

### 4b. `resume(blob, value, options)` API

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

### 4c. `NativeJsFunction` re-injection on resume

The global context (from `bindings`) is rebuilt from the `options.bindings` map on each
`run()` / `resume()` call, exactly as today. Since `NativeJsFunction` values are never in
the serialized frames, this is sufficient. No special handling needed.

### 4d. Deliverable

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

---

## Phase 5 — Standard Effects

Built-in effects with default implementations. Registered before host handlers so
hosts can override them.

| Effect | Default implementation |
|---|---|
| `lits.log` | `console.log(...args)` |
| `lits.now` | `Date.now()` |
| `lits.random` | `Math.random()` |
| `lits.sleep` | `setTimeout(resume, args[0])` |
| `lits.prompt` | `readline` / `window.prompt` |

Override in tests for determinism:

```typescript
handlers: {
  'lits.now':    async ({ resume }) => resume(fixedTimestamp),
  'lits.random': async ({ resume }) => resume(0.42),
}
```

### Deliverable

Standard effects work with default handlers. Overridable in tests.

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
