import { LitsError } from '../errors'
import type { Any } from '../interface'
import type { BindingTarget, Node, RestBindingTarget } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { assertUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { asAny, assertAny } from '../typeGuards/lits'

export function evalueateBindingNodeValues(
  target: BindingTarget,
  value: Any,
  evaluate: (Node: Node) => Any,
): Record<string, Any> {
  const sourceCodeInfo = target.sourceCodeInfo
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
  if (bindingTarget.type === 'object') {
    assertUnknownRecord(value, sourceCodeInfo)
    const capturedKeys = new Set<string>()
    let restElement: RestBindingTarget | undefined
    Object.entries(bindingTarget.elements).forEach(([key, element]) => {
      if (element.type === 'rest') {
        restElement = element
        return
      }
      capturedKeys.add(key)
      const val = (value[key] !== undefined ? value[key] : element.default && evaluate(element.default)) ?? null
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

      record[restElement.name] = restValues
    }
  }
  else if (bindingTarget.type === 'array') {
    let restIndex: number | null = null
    assertArray(value, sourceCodeInfo)
    for (let index = 0; index < bindingTarget.elements.length; index += 1) {
      const element = bindingTarget.elements[index] ?? null
      if (element === null) {
        continue
      }
      if (element.type === 'rest') {
        restIndex = index
        break
      }
      const val = (value[index] !== undefined ? value[index] : element.default && evaluate(element.default)) ?? null
      assertAny(val, sourceCodeInfo)
      createRecord(element, val, evaluate, sourceCodeInfo, record)
    }
    if (restIndex !== null) {
      const restValues = value.slice(restIndex)
      const restElement = bindingTarget.elements[restIndex]! as RestBindingTarget
      record[restElement.name] = restValues
    }
  }
  else {
    record[bindingTarget.name] = asAny(value)
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
  if (target.type === 'array') {
    for (const element of target.elements) {
      getNamesFromBindingTarget(element, names)
    }
  }
  else if (target.type === 'object') {
    for (const element of Object.values(target.elements)) {
      getNamesFromBindingTarget(element, names)
    }
  }
  else {
    if (names[target.name]) {
      throw new LitsError(`Duplicate binding name: ${target.name}`, target.sourceCodeInfo)
    }
    names[target.name] = true
  }
}
