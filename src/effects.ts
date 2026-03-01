/**
 * Algebraic effects — host-facing API.
 *
 * Top-level standalone functions for running Lits programs with effect handlers.
 *
 * Three levels of use:
 * 1. `runSync(source, options?)` — pure computation, sync JS functions in bindings.
 * 2. `run(source, options?)` — async effects with handlers, always completes or suspends.
 * 3. `resume(blob, value, options?)` — resume a suspended continuation (Phase 4).
 *
 * Effect handlers are JavaScript functions that receive an `EffectContext` and must
 * call exactly one of `resume(value)` or `suspend(meta?)`:
 *
 * ```typescript
 * handlers: {
 *   'llm.complete': async ({ args, signal, resume }) => {
 *     resume(await callLLM(args[0], signal))
 *   }
 * }
 * ```
 */

import type { Any } from './interface'
import { LitsError } from './errors'
import type { LitsModule } from './builtin/modules/interface'
import { createContextStack } from './evaluator/ContextStack'
import { evaluate, evaluateWithEffects, resumeWithEffects } from './evaluator/trampoline'
import { tokenize } from './tokenizer/tokenize'
import { minifyTokenStream } from './tokenizer/minifyTokenStream'
import { parse } from './parser'
import type { Ast } from './parser/types'
import { deserializeSuspension } from './evaluator/suspension'

import type { Handlers, RunResult } from './evaluator/effectTypes'

// Re-export all types from effectTypes so consumers import from one place
export type { EffectContext, EffectHandler, SuspensionBlob } from './evaluator/effectTypes'
export { SuspensionSignal, isSuspensionSignal } from './evaluator/effectTypes'
export type { Handlers, RunResult } from './evaluator/effectTypes'

// ---------------------------------------------------------------------------
// Options for standalone functions
// ---------------------------------------------------------------------------

/**
 * Options for `run()` — async execution with effect handlers.
 * `bindings` are plain values only (no JS functions).
 * All host interaction goes through `handlers`.
 */
export interface RunOptions {
  bindings?: Record<string, Any>
  handlers?: Handlers
  modules?: LitsModule[]
}

/**
 * Options for `runSync()` — synchronous pure computation.
 * `bindings` may include JS functions (they become NativeJsFunctions).
 */
export interface RunSyncOptions {
  bindings?: Record<string, unknown>
  modules?: LitsModule[]
}

/**
 * Options for `resume()` — resume a suspended continuation.
 * `bindings` are plain values only (no JS functions).
 * All host interaction goes through `handlers`.
 * `modules` must be provided again (they are not in the blob).
 */
export interface ResumeOptions {
  bindings?: Record<string, Any>
  handlers?: Handlers
  modules?: LitsModule[]
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Build an AST from source code (tokenize → minify → parse).
 * No caching — standalone functions are stateless.
 */
function buildAst(source: string): Ast {
  const tokenStream = tokenize(source, false, undefined)
  const minified = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
  return {
    body: parse(minified),
    hasDebugData: false,
  }
}

// ---------------------------------------------------------------------------
// Standalone functions
// ---------------------------------------------------------------------------

/**
 * Level 1: Pure synchronous computation.
 *
 * JS functions are allowed in `bindings` — they become NativeJsFunctions.
 * Throws if an async operation or effect is encountered.
 *
 * ```typescript
 * const value = runSync('[1, 2, 3] |> map(_, -> $ * $)')
 * ```
 */
export function runSync(source: string, options?: RunSyncOptions): Any {
  const modules = options?.modules
    ? new Map(options.modules.map(m => [m.name, m]))
    : undefined
  const contextStack = createContextStack(
    { bindings: options?.bindings },
    modules,
  )
  const ast = buildAst(source)
  const result = evaluate(ast, contextStack)
  if (result instanceof Promise) {
    throw new TypeError('Unexpected async operation in runSync(). Use run() for async operations.')
  }
  return result
}

/**
 * Level 2 & 3: Async execution with effect handler support.
 *
 * `bindings` are plain values only (no JS functions).
 * All host interaction goes through `handlers`.
 * Always resolves — never rejects. Errors are in `RunResult`.
 *
 * ```typescript
 * const result = await run(source, {
 *   handlers: {
 *     'llm.complete': async ({ args, resume }) => {
 *       resume(await callLLM(args[0]))
 *     }
 *   }
 * })
 * ```
 */
export async function run(source: string, options?: RunOptions): Promise<RunResult> {
  try {
    const modules = options?.modules
      ? new Map(options.modules.map(m => [m.name, m]))
      : undefined
    const contextStack = createContextStack(
      { bindings: options?.bindings },
      modules,
    )
    const ast = buildAst(source)
    return await evaluateWithEffects(ast, contextStack, options?.handlers)
  }
  catch (error) {
    // Catch parse errors and other errors that occur before the trampoline.
    if (error instanceof LitsError) {
      return { type: 'error', error }
    }
    return { type: 'error', error: new LitsError(`${error}`, undefined) }
  }
}

/**
 * Level 3: Resume a suspended continuation.
 *
 * Takes a blob from a previous `RunResult` of type `'suspended'`, a resume
 * value, and optional handlers/bindings. Re-enters the trampoline at the
 * point of suspension with the provided value.
 *
 * `bindings` are plain values only (no JS functions). They are re-injected
 * into the deserialized ContextStacks so that host-bound values remain
 * accessible after resume. `modules` must be provided again if the Lits
 * program uses `import`.
 *
 * Always resolves — never rejects. May return `completed`, `suspended`
 * (if another suspend is hit), or `error`.
 *
 * ```typescript
 * const blob = suspendedResult.blob
 * const next = await resume(blob, humanDecision, { handlers })
 * ```
 */
export async function resume(blob: string, value: Any, options?: ResumeOptions): Promise<RunResult> {
  try {
    const modules = options?.modules
      ? new Map(options.modules.map(m => [m.name, m]))
      : undefined

    // Deserialize the blob, re-injecting host bindings into ContextStacks.
    // NativeJsFunctions are not supported in resume — only plain values.
    const { k } = deserializeSuspension(blob, {
      values: options?.bindings as Record<string, unknown> | undefined,
      modules,
    })

    return await resumeWithEffects(k, value, options?.handlers)
  }
  catch (error) {
    if (error instanceof LitsError) {
      return { type: 'error', error }
    }
    return { type: 'error', error: new LitsError(`${error}`, undefined) }
  }
}
