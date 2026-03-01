/**
 * Shared types for the algebraic effects system.
 *
 * This module exists to break the circular dependency between `effects.ts`
 * (standalone functions) and `evaluator/trampoline.ts` (trampoline loop).
 * Both modules import types from here without creating a cycle.
 */

import type { Any } from '../interface'
import type { LitsError } from '../errors'
import type { ContinuationStack } from './frames'

// ---------------------------------------------------------------------------
// Suspension blob — opaque serialized continuation
// ---------------------------------------------------------------------------

/**
 * Opaque string containing the serialized continuation stack.
 * Created by `suspend()`, consumed by `resume()`.
 * Internally it's JSON, but hosts should treat it as opaque.
 */
export type SuspensionBlob = string

// ---------------------------------------------------------------------------
// Effect handler types
// ---------------------------------------------------------------------------

/**
 * Context passed to a host effect handler.
 *
 * The handler must call exactly one of `resume` or `suspend`, exactly once.
 * Calling both, or calling either more than once, is a programming error.
 */
export interface EffectContext {
  /** Arguments from the Lits `perform(eff, arg1, arg2, ...)` call. */
  args: Any[]

  /**
   * Aborted when: `race()` branch loses, runtime is disposed, or host cancels.
   * Combine with timeout: `AbortSignal.any([signal, AbortSignal.timeout(ms)])`
   */
  signal: AbortSignal

  /**
   * Resume the program with the given value (or a Promise that resolves to one).
   * The value becomes the result of the `perform(...)` expression in Lits.
   */
  resume: (value: Any | Promise<Any>) => void

  /**
   * Suspend the program. The entire execution state is captured and returned
   * in `RunResult` as `{ type: 'suspended', continuation, meta }`.
   * `meta` is passed through to `RunResult.meta` for domain context
   * (e.g., assignee, deadline, priority).
   */
  suspend: (meta?: Any) => void
}

/** An async function that handles an effect by calling `resume` or `suspend`. */
export type EffectHandler = (ctx: EffectContext) => Promise<void>

/** Map from effect name (e.g. `'llm.complete'`) to its handler. */
export type Handlers = Record<string, EffectHandler>

// ---------------------------------------------------------------------------
// Run result types
// ---------------------------------------------------------------------------

/**
 * The result of `run()` — always resolves, never rejects.
 * Errors are captured in the `error` variant.
 */
export type RunResult =
  | { type: 'completed', value: Any }
  | { type: 'suspended', blob: SuspensionBlob, meta?: Any }
  | { type: 'error', error: LitsError }

// ---------------------------------------------------------------------------
// Suspension signal — used internally by the trampoline
// ---------------------------------------------------------------------------

/**
 * Thrown (as a promise rejection) by `suspend()` inside a host handler.
 * Caught by the effect trampoline loop — NOT by Lits-level try/catch.
 */
export class SuspensionSignal {
  public readonly _brand = 'SuspensionSignal' as const
  constructor(
    /** The captured continuation stack at the point of suspension. */
    public readonly k: ContinuationStack,
    /** Optional domain metadata passed through to RunResult. */
    public readonly meta?: Any,
  ) {}
}

export function isSuspensionSignal(value: unknown): value is SuspensionSignal {
  return value instanceof SuspensionSignal
}
