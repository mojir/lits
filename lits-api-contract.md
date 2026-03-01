# LITS API CONTRACT v7

## Effects, Suspensions & Serializable Continuations

---

## Package Structure

- `@mojir/lits` — core runtime, minimal, no debug overhead
- `@mojir/lits/debug` — time-travel debugger built on core primitives

---

# Relation to Plotkin & Pretnar (2009)

## "Handlers of Algebraic Effects"

Lits implements algebraic effects in the spirit of P&P but makes
deliberate deviations to support serializable continuations.

### What Lits preserves

- ✓ Effects as algebraic operations — `perform(eff, ...args)`
- ✓ Handlers as first-class effect interpreters — `try/with`
- ✓ Lexically scoped handlers — innermost `with` wins
- ✓ Deep handlers — effects inside handlers delegate to outer scope
- ✓ Handlers run outside try-scope — `catch` does not protect with-handlers

### Deliberate deviations

- ✗ **No return-clause**
  P&P handlers have a 'return' clause that transforms the final
  value of the body. Lits omits this — the same transformation
  can always be expressed by wrapping the `try/with` block.

- ✗ **No multi-shot continuations**
  In P&P, resume is first-class and can be called multiple times.
  Lits restricts to single-shot — resume is implicit via handler
  return value, and can only be called once.

  Reason: multi-shot continuations are fundamentally incompatible
  with serializable continuations.

  | | Serializable | Not Serializable |
  |---|---|---|
  | Multi-shot | impossible | academic languages (Koka, Effekt) |
  | Single-shot | **Lits** | — |
  |

- ✗ **No static effect types**
  Lits is dynamically typed. Effects are identified by interned
  values via `effect(name)` rather than static types.

---

# Lits Language — Effect System

## `effect(name)` — special expression

Returns the unique effect value for the given name. The runtime guarantees that calling `effect` with the same name always returns the exact same reference, so effects can be compared with `==`.
Dot notation is only valid inside `effect(...)` — never as property access.

```lits
// Lits code

effect(lits.log)                  // standard effect
effect(llm.complete)              // domain-specific
effect(com.myco.human.approve)    // namespaced

effect(llm.complete) == effect(llm.complete)  // always true
```

Effect values are serializable — stored as their name string.
When a continuation is restored, the name is used to look up
the unique effect reference in the new runtime, so `==`
comparisons and handler matching continue to work correctly.

**Automatic binding** — `effect(name)` binds to:

1. JS handler registered in `run()` — highest priority
2. Standard default implementation — if `lits.*` effect
3. "unhandled" — throws `LitsError` — if nothing found

**Standard effects** (pre-declared, always visible):

- `effect(lits.log)` — write to stdout/console
- `effect(lits.now)` — current timestamp in ms
- `effect(lits.random)` — random float 0–1
- `effect(lits.prompt)` — ask user for input
- `effect(lits.sleep)` — wait N milliseconds
- `effect(lits.debug.step)` — injected by runtime in debug mode

**Naming convention:**

- `lits.*` — standard library, default implementation provided
- `com.mydomain.*` — domain-specific, host must provide JS handler

## `perform` — special expression

```lits
// Lits code

perform(eff)
perform(eff, arg1)
perform(eff, arg1, arg2, ...)
```

`eff` is any expression that evaluates to an effect value.
Effect values are first-class — can be stored, passed, returned.

## `try` / `with` / `catch` / `end`

**Valid forms:**

- `try ... catch ... end` — exceptions only (as before)
- `try ... with ... end` — effects only
- `try ... with ... catch ... end` — effects and exceptions
- `try ... end` — invalid, use `do/end` instead

`with` uses `case/then` syntax — consistent with `cond` and `match`.

### Handler semantics

Handler return value IS the resume value — no explicit resume needed.
Handler is a plain Lits function: `(args) -> value`
`args` is always an array — destructure as needed.
`self()` available for recursive handlers.

### Matching semantics

Matching is by value/reference — not by name.
`effect(llm.complete)` always returns the same unique reference.
First matching case wins.
Dead handlers (same reference, later position) should warn in tooling.

```lits
// Lits code

let llm = effect(llm.complete)
let alias = effect(llm.complete)   // same reference as llm

try ... with
  case llm   then ...              // matches effect(llm.complete)
  case alias then ...              // never reached — same reference
end
```

### Scope semantics

- **try-body** sees overridden scope — with-handlers are active
- **with-handlers** see outer scope — original bindings, not overrides
- **catch** sees outer scope — same as with-handlers

### `catch` semantics

`catch` only protects the try-body — NOT with-handlers (per P&P).
Errors thrown inside a with-handler propagate to the nearest
enclosing `try/catch` outside the current `try/with/end` block.

### Effect lookup order

```
perform(eff, ...)
  → matching case in nearest enclosing try/with?   yes → use it
  → matching case in outer try/with?               yes → use it
  → JS handler registered in run()?                yes → use it
  → standard effect with default implementation?   yes → use it
  → LitsError: No handler for effect 'llm.complete'
```

## `parallel` / `race`

```lits
// Lits code

let [a, b] = parallel(
  perform(llm, "p1"),
  perform(llm, "p2")
)

let fastest = race(
  perform(effect(llm.gpt4),   prompt),
  perform(effect(llm.claude), prompt)
)
```

---

## Lits Syntax Examples

```lits
// Lits code

// Declare effect values
let llm     = effect(llm.complete)
let approve = effect(com.myco.human.approve)

// perform — eff is any expression evaluating to an effect value
let result = perform(llm, "Summarize this")

// Inline
perform(effect(lits.log), "hello")

// try/with — case/then syntax, handler return value is resume value
try
  perform(llm, "prompt")
with
  case llm then ([prompt]) -> upper-case(prompt)
end

// try/catch — exceptions only
try
  risky-operation()
catch (error)
  "failed: " ++ error.message
end

// try/with/catch — body errors caught, handler errors propagate upward
try
  perform(llm, "prompt")
with
  case llm then ([prompt]) ->
    if empty?(prompt) then
      throw("Empty prompt")        // propagates to OUTER try/catch
    else
      upper-case(prompt)
    end
catch (error)
  "Body failed: " ++ error.message // only sees errors from body, not handlers
end

// Destructuring args
try
  perform(effect(state.set), "key", 42)
with
  case effect(state.set) then ([key, value]) -> do
    perform(effect(lits.log), "Setting " ++ key)
    null
  end
end

// Delegating to outer handler — enriches the effect
try
  perform(llm, "prompt")
with
  case llm then ([prompt]) ->
    perform(llm, prompt ++ " — be concise")  // reaches next handler in chain
end

// Effects are first-class — pass as arguments
let with-retry = (eff, max-attempts, body) ->
  try
    body()
  with
    case eff then ([...args]) ->
      loop (attempt = 0) ->
        try
          perform(eff, ...args)
        catch
          if attempt < max-attempts then
            recur(attempt + 1)
          else
            throw("Max retries exceeded")
          end
        end
      end
  end

with-retry(llm, 3, () ->
  perform(llm, "critical task")
)

// Recursive handler via self
let llm = effect(llm.complete)
try
  perform(llm, "a very long prompt that needs shortening")
with
  case llm then ([prompt]) ->
    if count(prompt) > 100 then
      self([shorten(prompt)])   // recursive — calls this handler again
    else
      perform(llm, prompt)      // delegate to outer handler
    end
end

// Conditional suspension via effect
let charge   = effect(payment.charge)
let requires = effect(payment.approval-required)

let with-approval-policy = (threshold, body) ->
  try
    body()
  with
    case charge then ([amount, account]) ->
      if amount > threshold then
        perform(requires, { amount: amount, account: account })
      else
        perform(charge, amount, account)
      end
  end

with-approval-policy(10000, () ->
  perform(charge, 50000, "ACC-123")
)

// Package that declares and exports an effect
// llm-package.lits
let llm = effect(llm.complete)

let summarize = (doc) -> perform(llm, "Summarize: " ++ doc)
let critique  = (doc) -> perform(llm, "Critique: " ++ doc)

{ summarize: summarize, critique: critique, llm: llm }

// Consumer — override via try/with or JS handler in run()
let pkg = import(llm-package)

try
  pkg.summarize("document")
with
  case effect(llm.complete) then ([p]) -> "mocked: " ++ p
end

// parallel and race
let [summary, critique] = parallel(
  perform(llm, "Summarize: " ++ doc),
  perform(llm, "Critique: " ++ doc)
)

let fastest = race(
  perform(effect(llm.gpt4),   prompt),
  perform(effect(llm.claude), prompt)
)
```

---

# `@mojir/lits` — Core JS API

```typescript
type LitsValue = string | number | boolean | null | LitsValue[] | { [key: string]: LitsValue }

type RunResult =
  | { type: 'completed'; value: LitsValue }
  | { type: 'suspended'; blob: SuspensionBlob; meta?: LitsValue }
  | { type: 'error';     error: LitsError }

interface LitsError {
  message: string
  source?: string
  cause?:  unknown
}

type SuspensionBlob = string  // opaque — Lits internal format

interface EffectContext {
  args:    LitsValue[]

  // Aborted when: race() branch loses, or runtime is disposed.
  // Combine with timeout: AbortSignal.any([signal, AbortSignal.timeout(ms)])
  signal:  AbortSignal

  // Resume with value (sync) or promise (async) — Lits detects which
  resume:  (value: LitsValue | Promise<LitsValue>) => void

  // Suspend — meta passed through to RunResult
  suspend: (meta?: LitsValue) => void
}

type EffectHandler = (ctx: EffectContext) => Promise<void>
type Handlers = Record<string, EffectHandler>   // key is effect name e.g. 'llm.complete'
```

### `runSync` — Level 1: Pure computation

Sync JS functions allowed in bindings.
Throws `LitsSyncError` if async effect encountered.

```typescript
declare function runSync(
  source: string,
  options?: {
    bindings?: Record<string, LitsValue | ((...args: LitsValue[]) => LitsValue)>
  }
): LitsValue
```

### `run` — Level 2 & 3: Full execution with effect support

No JS functions in bindings — all async interaction via handlers.
Always resolves — never rejects. Errors in `RunResult`.

```typescript
declare function run(
  source: string,
  options?: {
    bindings?: Record<string, LitsValue>
    handlers?: Handlers
  }
): Promise<RunResult>
```

### `resume` — Resume a suspended continuation

`blob` comes from `RunResult` of type `'suspended'`.

```typescript
declare function resume(
  blob: SuspensionBlob,
  value: LitsValue,
  options?: {
    handlers?: Handlers
  }
): Promise<RunResult>
```

---

# `@mojir/lits/debug` — Debugger

```typescript
interface StepInfo {
  expression: string
  value:      LitsValue
  location:   { line: number; col: number }
  env:        Record<string, LitsValue>
}

interface HistoryEntry {
  blob:      SuspensionBlob  // for resume — opaque to host
  step:      StepInfo        // for UI — expression, value, location, env
  timestamp: number          // ms since epoch — enables performance profiling
}

interface LitsDebugger {
  run(source: string): Promise<RunResult>

  // Navigation — uses saved blobs, no external value needed
  stepForward():                                        Promise<RunResult>
  stepBackward():                                       Promise<RunResult>
  jumpTo(index: number):                                Promise<RunResult>

  // Rerun from a step with a different effect return value
  // Discards history after index — creates a new timeline
  rerunFrom(index: number, alternateValue: LitsValue):  Promise<RunResult>

  readonly history:     HistoryEntry[]
  readonly currentStep: number
  readonly current:     HistoryEntry
}

declare function createDebugger(options: {
  handlers?: Handlers
}): LitsDebugger
```

---

# JS Host Usage Examples

```typescript
import { run, resume, runSync } from '@mojir/lits'
import { createDebugger }       from '@mojir/lits/debug'
```

## Level 1: Pure computation with sync JS functions

```typescript
const value = runSync(`
  [1, 2, 3, 4, 5]
    |> filter(_, odd?)
    |> map(_, -> $ * $)
    |> reduce(_, +, 0)
`)
// => 35

const value2 = runSync(`
  // lits.now has a sync default — safe in runSync
  let formatted = formatDate(perform(effect(lits.now)))  
  "Today is: " ++ formatted
`, {
  bindings: {
    formatDate: (ts: number) => new Date(ts).toISOString().split('T')[0]
  }
})
```

## Level 2: Async effects — always runs to completion

```typescript
const result1 = await run(`
  let llm = effect(llm.complete)
  perform(effect(lits.log), "Starting...")
  let summary  = perform(llm, "Summarize: " ++ topic)
  let critique = perform(llm, "Critique: " ++ summary)
  { summary: summary, critique: critique }
`, {
  bindings: { topic: 'quantum computing' },
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(callLLM(args[0] as string, signal))
    }
  }
})

if (result1.type === 'completed') {
  console.log(result1.value)
}
```

## Level 2B: Standard effects overridden for deterministic testing

```typescript
const testResult = await run(`
  let t = perform(effect(lits.now))
  let x = perform(effect(lits.random))
  { time: t, random: x }
`, {
  handlers: {
    'lits.now':    async ({ resume }) => resume(new Date('2024-01-01').getTime()),
    'lits.random': async ({ resume }) => resume(0.42),
  }
})
```

## Level 2C: Effect with host-controlled timeout

```typescript
await run(`
  let llm = effect(llm.complete)
  perform(llm, prompt)
`, {
  values: { prompt: 'Explain recursion' },
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      const combined = AbortSignal.any([signal, AbortSignal.timeout(10_000)])
      resume(callLLM(args[0] as string, combined))
    }
  }
})
```

## Level 3A: Suspend — human in the loop

```typescript
const result3 = await run(`
  let llm     = effect(llm.complete)
  let approve = effect(com.myco.human.approve)

  let report   = perform(llm, "Generate Q4 report")
  let decision = perform(approve, report)

  if decision.approved then
    perform(llm, "Finalize: " ++ report)
  else
    "Rejected: " ++ decision.reason
  end
`, {
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(callLLM(args[0] as string, signal))
    },
    'com.myco.human.approve': async ({ args, suspend }) => {
      suspend({
        assignedTo: 'finance-team',
        payload:    args[0],
        deadline:   new Date(Date.now() + days(3)).toISOString(),
      })
    },
  }
})

if (result3.type === 'suspended') {
  await db.save({ blob: result3.blob, meta: result3.meta })
  sendSlackMessage(`Approval needed`, result3.meta)
}
```

Resume — days later, new process:

```typescript
async function handleApprovalWebhook(id: string, approved: boolean, reason?: string) {
  const { blob } = await db.load(id)
  const result = await resume(blob, { approved, reason: reason ?? null }, {
    handlers: {
      'llm.complete':           async ({ args, signal, resume: r }) => r(callLLM(args[0] as string, signal)),
      'com.myco.human.approve': async ({ args, suspend })           => suspend({ assignedTo: 'finance-team', payload: args[0] }),
    }
  })

  if (result.type === 'completed') {
    console.log('Workflow done:', result.value)
  } else if (result.type === 'suspended') {
    await db.save({ blob: result.blob, meta: result.meta })
  }
}
```

## Level 3B: Parallel effects

```typescript
await run(`
  let llm = effect(llm.complete)
  let [summary, critique, keywords] = parallel(
    perform(llm, "Summarize: " ++ doc),
    perform(llm, "Critique: " ++ doc),
    perform(llm, "Extract keywords: " ++ doc)
  )
  { summary: summary, critique: critique, keywords: keywords }
`, {
  values: { doc: 'Long document...' },
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(callLLM(args[0] as string, signal))
    }
  }
})
```

## Level 3C: Race — first wins, rest cancelled via signal

```typescript
await run(`
  let fastest = race(
    perform(effect(llm.gpt4),   prompt),
    perform(effect(llm.claude), prompt),
    perform(effect(llm.gemini), prompt)
  )
  fastest
`, {
  values: { prompt: 'Explain recursion' },
  handlers: {
    'llm.gpt4':   async ({ args, signal, resume }) => resume(callGPT4(args[0]   as string, signal)),
    'llm.claude': async ({ args, signal, resume }) => resume(callClaude(args[0] as string, signal)),
    'llm.gemini': async ({ args, signal, resume }) => resume(callGemini(args[0] as string, signal)),
  }
})
```

## Level 3D: Crash recovery via checkpointing

```typescript
function makeHandlers(workflowId: string): Handlers {
  return {
    'llm.complete': async ({ args, signal, resume, suspend }) => {
      const value = await callLLM(args[0] as string, signal)
      suspend({ checkpoint: true, workflowId })
      resume(value)
    },
    'com.myco.human.approve': async ({ args, suspend }) => {
      suspend({ workflowId, payload: args[0] })
    },
  }
}

async function runWithRecovery(source: string, workflowId: string) {
  const checkpoint = await db.loadCheckpoint(workflowId)
  if (checkpoint) {
    return resume(checkpoint.blob, checkpoint.lastValue, { handlers: makeHandlers(workflowId) })
  }
  return run(source, { handlers: makeHandlers(workflowId) })
}
```

## Level 4: Time-travel debugger

```typescript
const dbg = createDebugger({
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(callLLM(args[0] as string, signal))
    }
  }
})

await dbg.run(`
  let llm = effect(llm.complete)
  let x   = perform(effect(lits.random))
  let y   = perform(llm, "Compute something with " ++ str(x))
  { x: x, result: y }
`)

await dbg.stepForward()
await dbg.stepBackward()
await dbg.jumpTo(5)

// "What if lits.random had returned 0.3 instead?"
const randomStep = dbg.history.findIndex(e => e.step.expression.includes('lits.random'))
await dbg.rerunFrom(randomStep, 0.3)

// Performance profiling via timestamps
dbg.history.forEach((entry, i) => {
  const duration = i > 0 ? entry.timestamp - dbg.history[i - 1].timestamp : 0
  console.log(`Step ${i}: ${entry.step.expression} — ${duration}ms`)
})
```

---

## Stubs

```typescript
declare function callLLM(prompt: string, signal?: AbortSignal): Promise<string>
declare function callGPT4(prompt: string, signal?: AbortSignal): Promise<string>
declare function callClaude(prompt: string, signal?: AbortSignal): Promise<string>
declare function callGemini(prompt: string, signal?: AbortSignal): Promise<string>
declare function sendSlackMessage(msg: string, meta?: unknown): void
declare function days(n: number): number
declare const db: {
  save(record: object): Promise<void>
  load(id: string): Promise<{ blob: SuspensionBlob; meta?: LitsValue }>
  saveCheckpoint(id: string, data: object): Promise<void>
  loadCheckpoint(id: string): Promise<{ blob: SuspensionBlob; lastValue: LitsValue } | null>
}
```
