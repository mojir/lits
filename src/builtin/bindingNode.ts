import { LitsError } from '../errors'
import type { Any } from '../interface'
import { type ArrayBindingTarget, type AstNode, type BindingTarget, type RestBindingTarget, type UserDefinedSymbolNode, bindingTargetTypes } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { assertUnknownRecord, isUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { asAny, assertAny } from '../typeGuards/lits'
import type { MaybePromise } from '../utils/maybePromise'
import { chain, forEachSequential } from '../utils/maybePromise'
import { deepEqual } from '../utils'

export function walkDefaults(
  bindingTarget: BindingTarget,
  onDefault: (Node: AstNode) => void,
): void {
  if (bindingTarget[0] === bindingTargetTypes.object) {
    Object.values(bindingTarget[1][0]).forEach((element) => {
      if (element[1][1]) {
        onDefault(element[1][1])
      }
      walkDefaults(element, onDefault)
    })
  }
  else if (bindingTarget[0] === bindingTargetTypes.array) {
    for (let index = 0; index < bindingTarget[1][0].length; index += 1) {
      const element = bindingTarget[1][0][index] ?? null
      if (element === null) {
        continue
      }
      if (element[1][1]) {
        onDefault(element[1][1])
      }
      walkDefaults(element, onDefault)
    }
  }
  // literal and wildcard have no defaults - nothing to walk
}

export function evaluateBindingNodeValues(
  target: BindingTarget,
  value: Any,
  evaluate: (Node: AstNode) => MaybePromise<Any>,
): MaybePromise<Record<string, Any>> {
  const sourceCodeInfo = target[2]
  const record: Record<string, Any> = {}
  return chain(createRecord(target, value, evaluate, sourceCodeInfo, record), () => record)
}

function createRecord(
  bindingTarget: BindingTarget,
  value: Any,
  evaluate: (Node: AstNode) => MaybePromise<Any>,
  sourceCodeInfo: SourceCodeInfo | undefined,
  record: Record<string, Any>,
): MaybePromise<void> {
  if (bindingTarget[0] === bindingTargetTypes.object) {
    assertUnknownRecord(value, sourceCodeInfo)
    const capturedKeys = new Set<string>()
    let restElement: RestBindingTarget | undefined
    const entries = Object.entries(bindingTarget[1][0])

    return chain(
      forEachSequential(entries, ([key, element]) => {
        if (element[0] === bindingTargetTypes.rest) {
          restElement = element
          return
        }
        capturedKeys.add(key)
        const existingVal = value[key]
        const maybeVal: MaybePromise<Any> = existingVal !== undefined
          ? existingVal as Any
          : element[1][1]
            ? evaluate(element[1][1])
            : null
        return chain(maybeVal, (resolvedVal) => {
          const val = resolvedVal ?? null
          assertAny(val, sourceCodeInfo)
          return createRecord(element, val, evaluate, sourceCodeInfo, record)
        })
      }),
      () => {
        if (restElement) {
          const restValues = Object.entries(value)
            .filter(([key]) => !capturedKeys.has(key))
            .reduce((acc: Record<string, Any>, [key, val]) => {
              acc[key] = asAny(val)
              return acc
            }, {})

          record[restElement[1][0]] = restValues
        }
      },
    )
  }
  else if (bindingTarget[0] === bindingTargetTypes.array) {
    let restIndex: number | null = null
    assertArray(value, sourceCodeInfo)

    const elements: Array<{ element: BindingTarget, index: number }> = []
    for (let index = 0; index < bindingTarget[1][0].length; index += 1) {
      const element = bindingTarget[1][0][index] ?? null
      if (element === null) {
        continue
      }
      if (element[0] === bindingTargetTypes.rest) {
        restIndex = index
        break
      }
      elements.push({ element, index })
    }

    return chain(
      forEachSequential(elements, ({ element, index }) => {
        const existingVal = value[index]
        const maybeVal: MaybePromise<Any> = existingVal !== undefined
          ? existingVal as Any
          : element[1][1]
            ? evaluate(element[1][1])
            : null
        return chain(maybeVal, (resolvedVal) => {
          const val = resolvedVal ?? null
          assertAny(val, sourceCodeInfo)
          return createRecord(element, val, evaluate, sourceCodeInfo, record)
        })
      }),
      () => {
        if (restIndex !== null) {
          const restValues = value.slice(restIndex)
          const restElement = bindingTarget[1][0][restIndex]! as RestBindingTarget
          record[restElement[1][0]] = restValues
        }
      },
    )
  }
  else if (bindingTarget[0] === bindingTargetTypes.rest) {
    record[bindingTarget[1][0]] = asAny(value)
  }
  else {
    record[(bindingTarget[1][0] as UserDefinedSymbolNode)[1]] = asAny(value)
  }
}

export function getAllBindingTargetNames(bindingTarget: BindingTarget): Record<string, true> {
  const names: Record<string, true> = {}
  getNamesFromBindingTarget(bindingTarget, names)
  return names
}

function getNamesFromBindingTarget(target: BindingTarget | null, names: Record<string, true>): void {
  if (target === null) {
    return
  }
  if (target[0] === bindingTargetTypes.array) {
    for (const element of target[1][0]) {
      getNamesFromBindingTarget(element, names)
    }
  }
  else if (target[0] === bindingTargetTypes.object) {
    for (const element of Object.values(target[1][0])) {
      getNamesFromBindingTarget(element, names)
    }
  }
  else if (target[0] === bindingTargetTypes.rest) {
    if (names[target[1][0]]) {
      throw new LitsError(`Duplicate binding name: ${target[1][0]}`, target[2])
    }
    names[target[1][0]] = true
  }
  else if (target[0] === bindingTargetTypes.symbol) {
    if (names[target[1][0][1]]) {
      throw new LitsError(`Duplicate binding name: ${target[1][0]}`, target[2])
    }
    names[target[1][0][1]] = true
  }
  // literal and wildcard bind no names - skip
}

/**
 * Non-throwing pattern matching. Returns bindings on match, null on mismatch.
 * Used by `switch` pattern matching.
 */
export function tryMatch(
  target: BindingTarget,
  value: Any,
  evaluate: (Node: AstNode) => MaybePromise<Any>,
): MaybePromise<Record<string, Any> | null> {
  const record: Record<string, Any> = {}
  return chain(tryMatchRecord(target, value, evaluate, record), (matched) => {
    if (!matched)
      return null
    return record
  })
}

function tryMatchRecord(
  bindingTarget: BindingTarget,
  value: Any,
  evaluate: (Node: AstNode) => MaybePromise<Any>,
  record: Record<string, Any>,
): MaybePromise<boolean> {
  // Wildcard: always matches, binds nothing
  if (bindingTarget[0] === bindingTargetTypes.wildcard) {
    return true
  }

  // Literal: compare with deepEqual
  else if (bindingTarget[0] === bindingTargetTypes.literal) {
    const literalNode = bindingTarget[1][0]
    return chain(evaluate(literalNode), (literalValue) => {
      return deepEqual(value, literalValue)
    })
  }

  // Symbol: always matches, binds value
  else if (bindingTarget[0] === bindingTargetTypes.symbol) {
    const symbolNode = bindingTarget[1][0] as UserDefinedSymbolNode
    const defaultNode = bindingTarget[1][1]
    if (value === undefined || value === null) {
      if (defaultNode) {
        return chain(evaluate(defaultNode), (defaultValue) => {
          record[symbolNode[1]] = asAny(defaultValue)
          return true
        })
      }
      record[symbolNode[1]] = value ?? null
    }
    else {
      record[symbolNode[1]] = asAny(value)
    }
    return true
  }

  // Object pattern
  else if (bindingTarget[0] === bindingTargetTypes.object) {
    if (!isUnknownRecord(value))
      return false

    const capturedKeys = new Set<string>()
    let restElement: RestBindingTarget | undefined
    const entries = Object.entries(bindingTarget[1][0])

    let result: MaybePromise<boolean> = true
    for (const [key, element] of entries) {
      result = chain(result, (matched) => {
        if (!matched)
          return false

        if (element[0] === bindingTargetTypes.rest) {
          restElement = element
          return true
        }

        capturedKeys.add(key)
        const existingVal = value[key]

        // For literal sub-patterns, missing key means no match
        if (existingVal === undefined && element[0] === bindingTargetTypes.literal) {
          return chain(evaluate(element[1][0]), (literalValue) => {
            return deepEqual(undefined, literalValue)
          })
        }

        const maybeVal: MaybePromise<Any> = existingVal !== undefined
          ? existingVal as Any
          : element[1] && element[1][1]
            ? evaluate(element[1][1])
            : null

        return chain(maybeVal, (resolvedVal) => {
          const val = resolvedVal ?? null
          return tryMatchRecord(element, val, evaluate, record)
        })
      })
    }

    return chain(result, (matched) => {
      if (!matched)
        return false
      if (restElement) {
        const restValues = Object.entries(value)
          .filter(([key]) => !capturedKeys.has(key))
          .reduce((acc: Record<string, Any>, [key, val]) => {
            acc[key] = asAny(val)
            return acc
          }, {})
        record[restElement[1][0]] = restValues
      }
      return true
    })
  }

  // Array pattern
  else {
    const arrayTarget = bindingTarget as ArrayBindingTarget
    if (!Array.isArray(value))
      return false

    const elements = arrayTarget[1][0]
    let restIndex: number | null = null

    // Find rest element index and check length constraints
    for (let i = 0; i < elements.length; i += 1) {
      const element = elements[i]!
      if (element !== null && element[0] === bindingTargetTypes.rest) {
        restIndex = i
        break
      }
    }

    // Without rest: array length must match exactly
    if (restIndex === null && value.length !== elements.length) {
      return false
    }
    // With rest: array must have at least the non-rest elements
    if (restIndex !== null && value.length < restIndex) {
      return false
    }

    let result: MaybePromise<boolean> = true
    for (let i = 0; i < elements.length; i += 1) {
      const element = elements[i]!
      if (element === null)
        continue // skipped position

      if (element[0] === bindingTargetTypes.rest) {
        // Rest collects remaining elements
        record[element[1][0]] = value.slice(i)
        break
      }

      const el = element
      result = chain(result, (matched) => {
        if (!matched)
          return false

        return tryMatchRecord(el, asAny(value[i]), evaluate, record)
      })
    }
    return result
  }
}
