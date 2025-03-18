import { LitsError } from '../errors'
import type { Any } from '../interface'
import type { AstNode, BindingNode, BindingTarget } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { assertUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { asAny, assertAny } from '../typeGuards/lits'
import type { FunctionArgument } from './utils'

export function evalueateBindingNodeValues(
  input: BindingNode | FunctionArgument,
  value: Any,
  evaluate: (astNode: AstNode) => Any,
): Record<string, Any> {
  const target = 'target' in input ? input.target : input
  const sourceCodeInfo = input.sourceCodeInfo
  const record: Record<string, Any> = {}
  createRecord(target, value, evaluate, sourceCodeInfo, record)
  return record
}

function createRecord(
  bindingTarget: BindingTarget,
  value: Any,
  evaluate: (astNode: AstNode) => Any,
  sourceCodeInfo: SourceCodeInfo | undefined,
  record: Record<string, Any>,
): void {
  if (bindingTarget.type === 'object') {
    Object.entries(bindingTarget.elements).forEach(([key, element]) => {
      assertUnknownRecord(value, sourceCodeInfo)
      const val = (value[key] !== undefined ? value[key] : element.default && evaluate(element.default)) ?? null
      assertAny(val, sourceCodeInfo)
      createRecord(element, val, evaluate, sourceCodeInfo, record)
    })
  }
  else if (bindingTarget.type === 'array') {
    bindingTarget.elements.forEach((element, index) => {
      if (element === null) {
        return
      }
      assertArray(value, sourceCodeInfo)
      const val = (value[index] !== undefined ? value[index] : element.default && evaluate(element.default)) ?? null
      assertAny(val, sourceCodeInfo)
      createRecord(element, val, evaluate, sourceCodeInfo, record)
    })
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
