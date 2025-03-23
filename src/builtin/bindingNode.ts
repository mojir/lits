import { LitsError } from '../errors'
import type { Any } from '../interface'
import { type BindingTarget, type Node, type RestBindingTarget, type UserDefinedSymbolNode, bindingTargetTypes } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { assertUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { asAny, assertAny } from '../typeGuards/lits'

export function walkDefaults(
  bindingTarget: BindingTarget,
  onDefault: (Node: Node) => void,
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
}

export function evalueateBindingNodeValues(
  target: BindingTarget,
  value: Any,
  evaluate: (Node: Node) => Any,
): Record<string, Any> {
  const sourceCodeInfo = target[2]
  const record: Record<string, Any> = {}
  createRecord(target, value, evaluate, sourceCodeInfo, record)
  return record
}

function createRecord(
  bindingTarget: BindingTarget,
  value: Any,
  evaluate: (Node: Node) => Any,
  sourceCodeInfo: SourceCodeInfo | undefined,
  record: Record<string, Any>,
): void {
  if (bindingTarget[0] === bindingTargetTypes.object) {
    assertUnknownRecord(value, sourceCodeInfo)
    const capturedKeys = new Set<string>()
    let restElement: RestBindingTarget | undefined
    Object.entries(bindingTarget[1][0]).forEach(([key, element]) => {
      if (element[0] === bindingTargetTypes.rest) {
        restElement = element
        return
      }
      capturedKeys.add(key)
      const val = (value[key] !== undefined ? value[key] : element[1][1] && evaluate(element[1][1])) ?? null
      assertAny(val, sourceCodeInfo)
      createRecord(element, val, evaluate, sourceCodeInfo, record)
    })
    if (restElement) {
      const restValues = Object.entries(value)
        .filter(([key]) => !capturedKeys.has(key))
        .reduce((acc: Record<string, Any>, [key, val]) => {
          acc[key] = asAny(val)
          return acc
        }, {})

      record[restElement[1][0]] = restValues
    }
  }
  else if (bindingTarget[0] === bindingTargetTypes.array) {
    let restIndex: number | null = null
    assertArray(value, sourceCodeInfo)
    for (let index = 0; index < bindingTarget[1][0].length; index += 1) {
      const element = bindingTarget[1][0][index] ?? null
      if (element === null) {
        continue
      }
      if (element[0] === bindingTargetTypes.rest) {
        restIndex = index
        break
      }
      const val = (value[index] !== undefined ? value[index] : element[1][1] && evaluate(element[1][1])) ?? null
      assertAny(val, sourceCodeInfo)
      createRecord(element, val, evaluate, sourceCodeInfo, record)
    }
    if (restIndex !== null) {
      const restValues = value.slice(restIndex)
      const restElement = bindingTarget[1][0][restIndex]! as RestBindingTarget
      record[restElement[1][0]] = restValues
    }
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
  else {
    if (names[target[1][0][1]]) {
      throw new LitsError(`Duplicate binding name: ${target[1][0]}`, target[2])
    }
    names[target[1][0][1]] = true
  }
}
