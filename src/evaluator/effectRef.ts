/**
 * EffectRef Interning
 *
 * Maintains a module-level intern map so that `getEffectRef('llm.complete')`
 * always returns the exact same `EffectRef` object. This guarantees reference
 * equality (`===`) for effect values created with the same name, which is
 * important for handler matching in `try/with` blocks.
 *
 * EffectRef values are serializable — they are stored as just their name string.
 * When a continuation is restored, the name is used to look up (or recreate)
 * the unique EffectRef in the new runtime.
 */

import type { EffectRef } from '../parser/types'
import { EFFECT_SYMBOL } from '../utils/symbols'

const internMap = new Map<string, EffectRef>()

/**
 * Returns the unique EffectRef for the given name.
 * Calling with the same name always returns the same reference.
 *
 * @example
 * const a = getEffectRef('llm.complete')
 * const b = getEffectRef('llm.complete')
 * a === b // true
 */
export function getEffectRef(name: string): EffectRef {
  let ref = internMap.get(name)
  if (!ref) {
    ref = {
      [EFFECT_SYMBOL]: true,
      name,
    }
    internMap.set(name, ref)
  }
  return ref
}

/**
 * Clears the intern map. Primarily for testing.
 */
export function clearEffectRefInternMap(): void {
  internMap.clear()
}
