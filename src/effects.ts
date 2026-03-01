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
import { evaluate, evaluateWithEffects } from './evaluator/trampoline'
import { tokenize } from './tokenizer/tokenize'
import { minifyTokenStream } from './tokenizer/minifyTokenStream'
import { parse } from './parser'
import type { Ast } from './parser/types'

import type { Handlers, RunResult } from './evaluator/effectTypes'

// Re-export all types from effectTypes so consumers import from one place
export type { EffectContext, EffectHandler } from './evaluator/effectTypes'
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
