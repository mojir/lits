/**
 * Continuation stack serialization and deserialization.
 *
 * When a host handler calls `suspend(meta?)`, the trampoline captures the
 * continuation stack (an array of Frame objects). This module converts that
 * stack to/from a JSON string (the "suspension blob") that can be stored,
 * transferred, and later resumed in a new process.
 *
 * The main challenge is that Frame objects contain `ContextStack` class
 * instances (which hold lexical scope chains). These are not plain data
 * and may form circular references (e.g., a global context containing a
 * UserDefinedFunction whose captured env references the same global context).
 *
 * The serialization approach:
 * 1. Walk the frame tree and collect all unique ContextStack instances (by identity)
 * 2. Assign each a numeric ID
 * 3. Serialize ContextStacks separately as plain objects
 * 4. In the frame tree, replace ContextStack references with `{ __csRef: id }`
 * 5. First occurrence of each ContextStack gets `{ __csDef: id, ... }` in the
 *    contextStacks array — circular refs just become `{ __csRef: id }`
 *
 * Deserialization reverses this:
 * 1. Parse the blob and create placeholder ContextStack instances
 * 2. Deep-resolve all `__csRef` markers back to real instances
 * 3. Fill in host bindings (values, nativeJsFunctions, modules) on each instance
 */

import { LitsError } from '../errors'
import type { Any } from '../interface'
import type { NativeJsFunction } from '../parser/types'
import { isLitsFunction } from '../typeGuards/litsFunction'
import type { LitsModule } from '../builtin/modules/interface'
import { ContextStackImpl } from './ContextStack'
import type { Context } from './interface'
import type { ContinuationStack } from './frames'
import { describeSerializationIssue } from './serialization'

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const SUSPENSION_VERSION = 1

// ---------------------------------------------------------------------------
// Internal blob structure (what gets JSON-stringified)
// ---------------------------------------------------------------------------

interface SerializedContextStack {
  id: number
  contexts: unknown[] // Context[] with nested ContextStacks replaced by refs
  globalContextIndex: number
  pure: boolean
}

interface SuspensionBlobData {
  version: number
  contextStacks: SerializedContextStack[]
  k: unknown // ContinuationStack with ContextStacks replaced by refs
  meta?: Any
}

// Marker objects embedded in the serialized data
interface CSRef { __csRef: number }

function isCSRef(value: unknown): value is CSRef {
  return value !== null
    && typeof value === 'object'
    && '__csRef' in value
    && typeof (value as CSRef).__csRef === 'number'
}

// ---------------------------------------------------------------------------
// Serialize
// ---------------------------------------------------------------------------

/**
 * Serialize a continuation stack and optional metadata into an opaque JSON blob.
 *
 * Validates that all values are serializable (no NativeJsFunctions in frames).
 * Throws a descriptive `LitsError` if non-serializable values are found.
 */
export function serializeSuspension(k: ContinuationStack, meta?: Any): string {
  // Phase 1: Collect all unique ContextStack instances
  const csMap = new Map<ContextStackImpl, number>()
  let nextId = 0

  function collectContextStacks(value: unknown): void {
    if (value instanceof ContextStackImpl) {
      if (csMap.has(value)) {
        return // Already visited — handles circular refs
      }
      csMap.set(value, nextId++)
      // Recurse into contexts to find nested ContextStacks (e.g., in UserDefinedFunction captured envs)
      for (const ctx of value.getContextsRaw()) {
        for (const entry of Object.values(ctx)) {
          collectContextStacks(entry.value)
        }
      }
      return
    }
    if (Array.isArray(value)) {
      for (const item of value) {
        collectContextStacks(item)
      }
      return
    }
    if (value !== null && typeof value === 'object') {
      for (const v of Object.values(value)) {
        collectContextStacks(v)
      }
    }
  }

  collectContextStacks(k)
  if (meta !== undefined) {
    collectContextStacks(meta)
  }

  // Phase 2: Serialize values, replacing ContextStacks with refs
  function serializeValue(value: unknown, path: string): unknown {
    if (value instanceof ContextStackImpl) {
      return { __csRef: csMap.get(value)! } satisfies CSRef
    }

    // Check for non-serializable function types
    if (isLitsFunction(value as Any) && (value as { functionType: string }).functionType === 'NativeJsFunction') {
      const issue = describeSerializationIssue(value as Any, path)
      throw new LitsError(
        `Cannot serialize continuation: ${issue ?? 'NativeJsFunction found in continuation stack'}`,
        undefined,
      )
    }

    if (Array.isArray(value)) {
      return value.map((item, i) => serializeValue(item, `${path}[${i}]`))
    }
    if (value !== null && typeof value === 'object') {
      const result: Record<string, unknown> = {}
      for (const [key, v] of Object.entries(value)) {
        result[key] = serializeValue(v, `${path}.${key}`)
      }
      return result
    }
    return value
  }

  // Serialize all collected ContextStacks
  const serializedContextStacks: SerializedContextStack[] = Array.from(csMap.entries()).map(
    ([cs, id]) => ({
      id,
      contexts: cs.getContextsRaw().map((ctx, ctxIdx) => {
        const serialized: Record<string, unknown> = {}
        for (const [name, entry] of Object.entries(ctx)) {
          serialized[name] = { value: serializeValue(entry.value, `cs[${id}].contexts[${ctxIdx}].${name}`) }
        }
        return serialized
      }),
      globalContextIndex: cs.getGlobalContextIndex(),
      pure: cs.pure,
    }),
  )

  // Serialize the continuation stack
  const serializedK = serializeValue(k, 'k')

  // Serialize meta
  const serializedMeta = meta !== undefined ? serializeValue(meta, 'meta') : undefined

  const blobData: SuspensionBlobData = {
    version: SUSPENSION_VERSION,
    contextStacks: serializedContextStacks,
    k: serializedK,
    ...(serializedMeta !== undefined ? { meta: serializedMeta as Any } : {}),
  }

  return JSON.stringify(blobData)
}

// ---------------------------------------------------------------------------
// Deserialize
// ---------------------------------------------------------------------------

/** Options for re-injecting host bindings on resume. */
export interface DeserializeOptions {
  values?: Record<string, unknown>
  nativeJsFunctions?: Record<string, NativeJsFunction>
  modules?: Map<string, LitsModule>
}

/**
 * Deserialize a suspension blob back into a continuation stack and metadata.
 *
 * Reconstructs `ContextStack` instances with fresh host bindings from `options`.
 * Handles circular references between ContextStacks and their contained values.
 */
export function deserializeSuspension(
  blob: string,
  options?: DeserializeOptions,
): { k: ContinuationStack, meta?: Any } {
  let blobData: SuspensionBlobData
  try {
    blobData = JSON.parse(blob) as SuspensionBlobData
  }
  catch {
    throw new LitsError('Invalid suspension blob: not valid JSON', undefined)
  }

  if (blobData.version !== SUSPENSION_VERSION) {
    throw new LitsError(
      `Unsupported suspension blob version: ${blobData.version} (expected ${SUSPENSION_VERSION})`,
      undefined,
    )
  }

  // Phase 1: Create placeholder ContextStack instances for each serialized one.
  // Contexts are empty initially — filled in Phase 2 after all instances exist.
  const csMap = new Map<number, ContextStackImpl>()

  for (const scs of blobData.contextStacks) {
    const placeholderContexts = scs.contexts.map(() => {
      const ctx: Context = {}
      return ctx
    })
    const cs = ContextStackImpl.fromDeserialized({
      contexts: placeholderContexts,
      globalContextIndex: scs.globalContextIndex,
      values: options?.values,
      nativeJsFunctions: options?.nativeJsFunctions,
      modules: options?.modules,
      pure: scs.pure,
    })
    csMap.set(scs.id, cs)
  }

  // Phase 2: Deep-resolve all values, replacing __csRef markers with real instances
  function resolveValue(value: unknown): unknown {
    if (isCSRef(value)) {
      const cs = csMap.get(value.__csRef)
      if (!cs) {
        throw new LitsError(`Invalid suspension blob: unknown context stack ref ${value.__csRef}`, undefined)
      }
      return cs
    }
    if (Array.isArray(value)) {
      return value.map(resolveValue)
    }
    if (value !== null && typeof value === 'object') {
      const result: Record<string, unknown> = {}
      for (const [key, v] of Object.entries(value)) {
        result[key] = resolveValue(v)
      }
      return result
    }
    return value
  }

  // Fill in contexts on each ContextStack
  for (const scs of blobData.contextStacks) {
    const cs = csMap.get(scs.id)!
    const resolvedContexts: Context[] = scs.contexts.map((serializedCtx) => {
      const ctx = serializedCtx as Record<string, { value: unknown }>
      const resolved: Context = {}
      for (const [name, entry] of Object.entries(ctx)) {
        resolved[name] = { value: resolveValue(entry.value) as Any }
      }
      return resolved
    })
    cs.setContextsFromDeserialized(resolvedContexts, scs.globalContextIndex)
  }

  // Resolve the continuation stack
  const resolvedK = resolveValue(blobData.k) as ContinuationStack

  // Resolve meta
  const resolvedMeta = blobData.meta !== undefined ? resolveValue(blobData.meta) as Any : undefined

  return { k: resolvedK, meta: resolvedMeta }
}
