# Lits Effects
## Algebraic Effects & Serializable Continuations

---

## Executive Summary

Building AI agent systems is harder than it should be. The agent logic — what the agent does — is often straightforward. What's difficult is everything around it: keeping agents alive across failures, waiting days for human approvals, coordinating multiple agents in parallel, and reproducing what happened when something goes wrong.

Today, teams solve these problems with a stack of external tools:

| Problem | Typical solution |
|---|---|
| Long-running workflows | Temporal, AWS Step Functions |
| Human-in-the-loop | Custom webhooks + database state |
| Agent orchestration | LangChain, AutoGen, CrewAI |
| Crash recovery | Idempotent redesign + retry logic |
| Debugging agent runs | Logging + manual reconstruction |

Each tool solves one problem but introduces its own programming model, infrastructure, and failure modes. The agent logic ends up scattered across workflow definitions, database schemas, and infrastructure code.

**Lits offers a different foundation.** By extending the Lits language with algebraic effects and serializable continuations, the execution state of any program becomes a JSON blob that can be paused, stored, transferred, and resumed — across processes, machines, and time. This means:

- **Long-running workflows** are ordinary programs, not orchestration graphs
- **Human-in-the-loop** is a single `perform` call, not a webhook architecture
- **Crash recovery** is automatic — resume from the last saved state
- **Debugging** includes time-travel — step backward through any execution and explore alternate outcomes

The result is a dramatic reduction in infrastructure complexity. Agent logic stays in one place, expressed clearly, without leaking into database code or infrastructure configuration.

---

## Why Effects?

Modern applications — especially those built around AI agents, long-running workflows, and human-in-the-loop processes — share a common problem: **the gap between what the code describes and what the infrastructure has to do to make it work**.

Consider a simple agent workflow:

```
1. Call an LLM to draft a report
2. Wait for a human to approve it (could take days)
3. Send the approved report to the board
```

In a conventional system, step 2 forces you to stop the program, save state to a database, set up a webhook, restore state when the human responds, and carefully reconnect all the pieces. The workflow logic drowns in infrastructure.

Lits takes a different approach. The program above is just a program:

```lits
let llm     = effect(llm.complete)
let approve = effect(com.myco.human.approve)

let report   = perform(llm, "Generate Q4 report")
let decision = perform(approve, report)

if decision.approved then
  perform(llm, "Finalize and send: " ++ report)
else
  "Rejected: " ++ decision.reason
end
```

No database code. No webhooks. No state restoration. The program expresses pure intent — the host decides what each `perform` actually does, including whether to pause execution for days and resume it later.

This is made possible by two ideas working together: **algebraic effects** and **serializable continuations**.

---

## The Core Idea

### Effects are explicit interactions with the world

In Lits, any interaction with the outside world — calling an LLM, reading a clock, asking a human — is an *effect*. Effects are declared as first-class values and invoked via `perform`:

```lits
let llm = effect(llm.complete)   // declare an effect
perform(llm, "Summarize this")   // invoke it
```

The program doesn't know how `llm.complete` is implemented. It doesn't know if it calls GPT-4 or Claude, whether it times out after 10 seconds, or whether it's mocked in a test. That's the host's concern.

### Handlers interpret effects

The host registers handlers in JavaScript/TypeScript that give effects their meaning:

```typescript
await run(source, {
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(await callOpenAI(args[0], signal))
    }
  }
})
```

A handler receives the effect's arguments and chooses what to do:
- Call `resume(value)` to continue the program with a result
- Call `suspend(meta?)` to pause execution — saving the entire program state as a serializable blob

### Continuations are data

When a handler calls `suspend()`, Lits captures the complete execution state — every variable binding, every pending computation — as a JSON-serializable blob. This blob can be stored in a database, sent over a network, and resumed later in a completely new process:

```typescript
// Days later, in a new process
const { blob } = await db.load(suspensionId)
const result = await resume(blob, { approved: true }, { handlers })
```

The program continues exactly where it left off. It has no idea time passed.

---

## Three Levels of Use

### Level 1 — Pure computation

No effects, no async. Use `runSync` for deterministic, synchronous programs. Sync JS functions can be exposed via `bindings`.

```typescript
const result = runSync(`
  [1, 2, 3, 4, 5] |> filter(_, odd?) |> map(_, -> $ * $) |> reduce(_, +, 0)
`)
// => 35
```

### Level 2 — Async effects

Effects that complete asynchronously but never suspend. The program always runs to completion.

```typescript
const result = await run(source, {
  handlers: {
    'llm.complete': async ({ args, signal, resume }) => {
      resume(callLLM(args[0], signal))
    }
  }
})
```

### Level 3 — Suspendable workflows

Effects that can pause execution indefinitely. The program state is serialized and resumed when ready.

```typescript
// Run until suspension
const result = await run(source, { handlers })

if (result.type === 'suspended') {
  await db.save({ blob: result.blob, meta: result.meta })
}

// Resume later
const { blob } = await db.load(id)
const next = await resume(blob, humanDecision, { handlers })
```

---

## Effects in Lits Code

### Declaring effects

Effects are first-class values created with `effect(name)`. The same name always returns the same interned reference — safe to pass around, store, and serialize.

```lits
let llm     = effect(llm.complete)
let approve = effect(com.myco.human.approve)
let log     = effect(lits.log)              // standard effect — always available
```

### Invoking effects

```lits
perform(llm, "prompt")                      // one argument
perform(effect(state.set), "key", 42)       // multiple arguments
perform(effect(lits.now))                   // no arguments
```

### Handling effects locally — try/with

The `try/with` construct overrides effects within a lexical scope. The handler's return value becomes the result of `perform`. No explicit `resume` is needed.

```lits
try
  perform(llm, "prompt")
with
  case llm then ([prompt]) -> upper-case(prompt)   // mock the LLM
end
```

Matching is by value/reference — `effect(llm.complete)` always interns to the same reference, so any alias matches the same handler.

### Combining effect handling and error handling

```lits
try
  perform(llm, "prompt")
with
  case llm then ([prompt]) ->
    if empty?(prompt) then throw("Empty prompt")
    else upper-case(prompt)
    end
catch (error)
  "Failed: " ++ error.message  // only catches errors from the body, not handlers
end
```

Note: errors thrown inside `with` handlers propagate *upward* to the nearest enclosing `try/catch` — not the `catch` in the same block. This follows standard algebraic effects semantics (Plotkin & Pretnar, 2009).

### Effects as first-class values

Because effects are values, they can be passed as arguments, enabling composable abstractions:

```lits
let with-retry = (eff, max-attempts, body) ->
  try
    body()
  with
    case eff then ([...args]) ->
      loop (attempt = 0) ->
        try
          perform(eff, ...args)
        catch
          if attempt < max-attempts then recur(attempt + 1)
          else throw("Max retries exceeded")
          end
        end
      end
  end

// Works for any effect
with-retry(effect(llm.complete), 3) ->
  perform(effect(llm.complete), "critical task")
end
```

---

## The Host API

### Running a program

```typescript
import { runSync, run, resume } from '@mojir/lits'

// Synchronous — no effects
const value = runSync(source, { bindings: { myFn: (x) => x * 2 } })

// Async — with effect handlers
const result = await run(source, { bindings, handlers })

// Resume a suspended continuation
const next = await resume(blob, resumeValue, { handlers })
```

### The RunResult type

```typescript
type RunResult =
  | { type: 'completed'; value: LitsValue }
  | { type: 'suspended'; blob: SuspensionBlob; meta?: LitsValue }
  | { type: 'error';     error: LitsError }
```

`blob` is an opaque string — Lits internal format. Store it anywhere. `meta` is whatever the handler passed to `suspend(meta)` — use it to carry domain context like assignee, deadline, or priority.

### Writing a handler

```typescript
type EffectHandler = (ctx: EffectContext) => Promise<void>

interface EffectContext {
  args:    LitsValue[]          // arguments from perform()
  signal:  AbortSignal          // aborted on race() loss or timeout
  resume:  (value: LitsValue | Promise<LitsValue>) => void
  suspend: (meta?: LitsValue) => void
}
```

Three patterns:

```typescript
// Sync resume
'my.effect': async ({ args, resume }) => {
  resume(transform(args[0]))
}

// Async resume
'llm.complete': async ({ args, signal, resume }) => {
  resume(callLLM(args[0], signal))   // resume accepts a Promise
}

// Suspend
'com.myco.human.approve': async ({ args, suspend }) => {
  suspend({ assignedTo: 'finance-team', payload: args[0] })
  // execution stops here — blob is in RunResult
}
```

### Naming convention

Effect names use dot notation inside `effect(...)`. The JS handler key is the same string:

```
effect(lits.log)               →  handlers key: 'lits.log'
effect(llm.complete)           →  handlers key: 'llm.complete'
effect(com.myco.human.approve) →  handlers key: 'com.myco.human.approve'
```

Use `lits.*` for standard library effects (default implementations provided). Use reverse-domain notation for domain-specific effects.

---

## Parallel & Race

Lits supports concurrent effects natively. The handler contract is identical to single `perform` — the runtime handles coordination.

```lits
// All three run concurrently — wait for all
let [summary, critique, keywords] = parallel(
  perform(llm, "Summarize: " ++ doc),
  perform(llm, "Critique: " ++ doc),
  perform(llm, "Extract keywords: " ++ doc)
)

// First to complete wins — signal aborted on losers
let fastest = race(
  perform(effect(llm.gpt4),   prompt),
  perform(effect(llm.claude), prompt)
)
```

For `race`, use `signal` in your handler to support cancellation:

```typescript
'llm.gpt4': async ({ args, signal, resume }) => {
  resume(callGPT4(args[0], signal))  // aborted automatically if another branch wins
}
```

---

## Compared to Existing Approaches

### vs. async/await

`async/await` makes async code readable but provides no mechanism for pausing, serializing, or resuming execution. Every `await` must resolve in the same process.

Lits effects can pause across processes, machines, and time. The Lits program looks synchronous — the host decides what "pausing" means.

### vs. LangChain / AutoGen / CrewAI

These frameworks solve orchestration *on top of* languages that weren't designed for it. The result is significant boilerplate for state management, checkpointing, and human-in-the-loop.

Lits effects make orchestration a language primitive. The same `perform` that calls an LLM can also wait for human input — the program doesn't change, only the handler does.

### vs. Temporal / AWS Step Functions

These are powerful orchestration engines but require learning a new programming model and separating workflow definition from implementation.

With Lits, the workflow *is* the program. Serializable continuations give you Temporal-like durability without leaving the language.

### vs. try/catch for error handling

`catch` handles exceptional failure — it cannot resume the computation that threw. `with` handles effects — the computation is paused, the handler runs, and execution resumes with the handler's return value.

Effects generalize exceptions. In fact, exceptions *are* a special case of effects — one where the handler never resumes.

---

## Standard Effects

These effects are always available with default implementations. Override them in handlers for testing or custom behavior.

| Effect | Default | Use |
|---|---|---|
| `lits.log` | `console.log` | Logging |
| `lits.now` | `Date.now()` | Current timestamp |
| `lits.random` | `Math.random()` | Random float 0–1 |
| `lits.prompt` | stdin / browser dialog | User input |
| `lits.sleep` | `setTimeout` | Delay |
| `lits.debug.step` | — | Injected in debug mode |

Override in tests for determinism:

```typescript
handlers: {
  'lits.now':    async ({ resume }) => resume(new Date('2024-01-01').getTime()),
  'lits.random': async ({ resume }) => resume(0.42),
}
```

---

## Time-Travel Debugging

Registering a `lits.debug.step` handler activates debug mode. The runtime injects this effect between every operation — no changes to Lits source needed.

```typescript
import { createDebugger } from '@mojir/lits/debug'

const dbg = createDebugger({ handlers })
await dbg.run(source)

await dbg.stepForward()           // next operation
await dbg.stepBackward()          // previous operation
await dbg.jumpTo(5)               // any point in history
await dbg.rerunFrom(3, 0.42)      // alternate timeline from step 3
```

Each `HistoryEntry` contains the serialized continuation, the expression and its value, source location, all bindings in scope, and a timestamp for performance profiling.

`rerunFrom` discards history after the given step and resumes with an alternate value — creating a new execution timeline from that point. This is not a simulation — it is actual re-execution from the exact captured state.

---

## Key Design Decisions

**Effects are first-class values** — not strings, not symbols. `effect(llm.complete)` returns an interned reference that can be stored, passed, and serialized. Same name, always same reference.

**Single-shot continuations only** — handlers resume exactly once. This is a deliberate deviation from Plotkin & Pretnar's multi-shot model, motivated by serializability. A continuation that can be resumed multiple times cannot be meaningfully serialized.

**Handlers run outside try-scope** — errors in `with` handlers propagate upward, not to the `catch` in the same block. This prevents accidental error loops and follows standard algebraic effects semantics.

**Suspension is the host's responsibility** — Lits code expresses intent via `perform`. Only JS handlers can call `suspend`. This keeps infrastructure concerns out of business logic.

**`runSync` and `run` are separate** — `runSync` is for pure, synchronous computation. `run` is for async and suspendable workflows. This makes the boundary explicit and allows sync JS functions in `runSync` without risking serialization issues.
