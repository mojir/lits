/**
 * LitsValue Serialization Contract
 *
 * Defines which runtime value types can be serialized to JSON and restored.
 * Used at suspension time to produce clear errors when non-serializable
 * values (e.g. NativeJsFunction) are found in the continuation stack.
 *
 * Serializable value types:
 *   - Primitives: number, string, boolean, null
 *   - Containers: array, object (if all contents are serializable)
 *   - RegularExpression: stored as {s, f} string data
 *   - UserDefinedFunction: {params, body, capturedEnv} — all plain data
 *   - NormalBuiltinFunction: identified by normalBuiltinSymbolType (number)
 *   - SpecialBuiltinFunction: identified by specialBuiltinSymbolType (number)
 *   - ModuleFunction: identified by {moduleName, functionName}
 *   - PartialFunction, CompFunction, ConstantlyFunction, JuxtFunction,
 *     ComplementFunction, EveryPredFunction, SomePredFunction, FNullFunction:
 *     serializable only if all inner values/functions are serializable
 *   - EffectRef: stored as just the name string
 *
 * Non-serializable value types:
 *   - NativeJsFunction: contains a live JS function reference.
 *     These live in the global context and are re-injected from `bindings`
 *     on resume — they must never appear in serialized continuation frames.
 */

import type { Any } from '../interface'
import { isLitsFunction } from '../typeGuards/litsFunction'
import { isEffectRef, isRegularExpression } from '../typeGuards/lits'
import type {
  LitsFunction,
} from '../parser/types'

/**
 * Checks whether a Lits runtime value is fully JSON-serializable.
 *
 * Returns `true` if the value can be serialized and later restored.
 * Returns `false` if any part of the value contains a NativeJsFunction
 * or other non-serializable reference.
 *
 * Uses a `Set` to track visited objects and avoid infinite loops from
 * circular references (which are themselves not serializable, but we
 * want to report NativeJsFunction as the problem, not stack overflow).
 */
export function isSerializable(value: Any, visited = new Set<object>()): boolean {
  // Primitives are always serializable
  if (value === null || typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
    return true
  }

  // Guard against circular references
  if (typeof value === 'object') {
    if (visited.has(value)) {
      return false
    }
    visited.add(value)
  }

  // RegularExpression — just string data {s, f}
  if (isRegularExpression(value)) {
    return true
  }

  // EffectRef — just a name string
  if (isEffectRef(value)) {
    return true
  }

  // LitsFunction — check by functionType
  if (isLitsFunction(value)) {
    return isLitsFunctionSerializable(value, visited)
  }

  // Array — serializable if all elements are
  if (Array.isArray(value)) {
    return value.every(item => isSerializable(item as Any, visited))
  }

  // Plain object — serializable if all values are
  if (typeof value === 'object') {
    return Object.values(value).every(v => isSerializable(v as Any, visited))
  }

  // Anything else (shouldn't happen in well-typed code) is not serializable
  return false
}

function isLitsFunctionSerializable(fn: LitsFunction, visited: Set<object>): boolean {
  switch (fn.functionType) {
    // Always serializable — contain only primitive/index data
    case 'UserDefined':
    case 'Builtin':
    case 'SpecialBuiltin':
    case 'Module':
      return true

    // NativeJsFunction — never serializable
    case 'NativeJsFunction':
      return false

    // Conditionally serializable — check inner values/functions
    case 'Partial': {
      const partial = fn
      return isSerializable(partial.function as Any, visited)
        && partial.params.every(p => isSerializable(p as Any, visited))
    }

    case 'Comp': {
      const comp = fn
      return comp.params.every(p => isSerializable(p as Any, visited))
    }

    case 'Constantly': {
      const constantly = fn
      return isSerializable(constantly.value, visited)
    }

    case 'Juxt': {
      const juxt = fn
      return juxt.params.every(p => isSerializable(p as Any, visited))
    }

    case 'Complement': {
      const complement = fn
      return isSerializable(complement.function as Any, visited)
    }

    case 'EveryPred': {
      const everyPred = fn
      return everyPred.params.every(p => isSerializable(p as Any, visited))
    }

    case 'SomePred': {
      const somePred = fn
      return somePred.params.every(p => isSerializable(p as Any, visited))
    }

    case 'Fnull': {
      const fnull = fn
      return isSerializable(fnull.function as Any, visited)
        && fnull.params.every(p => isSerializable(p as Any, visited))
    }

    /* v8 ignore next 2 */
    default:
      return false
  }
}

/**
 * Describes why a value is not serializable.
 * Returns `null` if the value is serializable.
 * Returns a human-readable string describing the first non-serializable
 * component found.
 */
export function describeSerializationIssue(value: Any, path: string = 'value'): string | null {
  if (value === null || typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
    return null
  }

  if (isRegularExpression(value)) {
    return null
  }

  if (isEffectRef(value)) {
    return null
  }

  if (isLitsFunction(value)) {
    if (value.functionType === 'NativeJsFunction') {
      return `${path} is a NativeJsFunction (${value.name ?? 'anonymous'}). NativeJsFunctions are not serializable — they are re-injected from bindings on resume.`
    }

    if (value.functionType === 'UserDefined' || value.functionType === 'Builtin' || value.functionType === 'SpecialBuiltin' || value.functionType === 'Module') {
      return null
    }

    // Check inner functions for compound function types
    if (value.functionType === 'Partial') {
      const partial = value
      const fnIssue = describeSerializationIssue(partial.function as Any, `${path}.function`)
      if (fnIssue)
        return fnIssue
      for (let i = 0; i < partial.params.length; i++) {
        const paramIssue = describeSerializationIssue(partial.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    if (value.functionType === 'Comp') {
      const comp = value
      for (let i = 0; i < comp.params.length; i++) {
        const paramIssue = describeSerializationIssue(comp.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    if (value.functionType === 'Complement') {
      const complement = value
      return describeSerializationIssue(complement.function as Any, `${path}.function`)
    }

    if (value.functionType === 'Constantly') {
      const constantly = value
      return describeSerializationIssue(constantly.value, `${path}.value`)
    }

    if (value.functionType === 'Juxt') {
      const juxt = value
      for (let i = 0; i < juxt.params.length; i++) {
        const paramIssue = describeSerializationIssue(juxt.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    if (value.functionType === 'EveryPred') {
      const everyPred = value
      for (let i = 0; i < everyPred.params.length; i++) {
        const paramIssue = describeSerializationIssue(everyPred.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    if (value.functionType === 'SomePred') {
      const somePred = value
      for (let i = 0; i < somePred.params.length; i++) {
        const paramIssue = describeSerializationIssue(somePred.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    if (value.functionType === 'Fnull') {
      const fnull = value
      const fnIssue = describeSerializationIssue(fnull.function as Any, `${path}.function`)
      if (fnIssue)
        return fnIssue
      for (let i = 0; i < fnull.params.length; i++) {
        const paramIssue = describeSerializationIssue(fnull.params[i] as Any, `${path}.params[${i}]`)
        if (paramIssue)
          return paramIssue
      }
      return null
    }

    return `${path} has unknown function type ${value}`
  }

  if (Array.isArray(value)) {
    for (let i = 0; i < value.length; i++) {
      const issue = describeSerializationIssue(value[i] as Any, `${path}[${i}]`)
      if (issue)
        return issue
    }
    return null
  }

  if (typeof value === 'object') {
    for (const [key, v] of Object.entries(value)) {
      const issue = describeSerializationIssue(v as Any, `${path}.${key}`)
      if (issue)
        return issue
    }
    return null
  }

  return `${path} has unexpected type ${typeof value}`
}
