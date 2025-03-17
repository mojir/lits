import type { Any } from '../interface'
import type { BindingNode, BindingTarget } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { assertUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { assertAny } from '../typeGuards/lits'

export function bindingNodeEntries(
  bindingNode: BindingNode,
  value: Any,
  onEntry: (name: string, value: Any) => void,
): void {
  const { target, sourceCodeInfo } = bindingNode
  bindingTargetEntries(target, value, onEntry, sourceCodeInfo)
}

export function bindingTargetEntries(
  bindingTarget: BindingTarget | null,
  value: Any,
  onEntry: (name: string, value: Any) => void,
  sourceCodeInfo: SourceCodeInfo | undefined,
): void {
  if (bindingTarget === null) {
    return
  }
  if (bindingTarget.type === 'object') {
    Object.entries(bindingTarget.elements).forEach(([key, element]) => {
      assertUnknownRecord(value, sourceCodeInfo)
      const val = value[key] ?? null
      assertAny(val, sourceCodeInfo)
      bindingTargetEntries(element, val, onEntry, sourceCodeInfo)
    })
  }
  else if (bindingTarget.type === 'array') {
    bindingTarget.elements.forEach((element, index) => {
      assertArray(value, sourceCodeInfo)
      const val = value[index] ?? null
      assertAny(val, sourceCodeInfo)
      bindingTargetEntries(element, val, onEntry, sourceCodeInfo)
    })
  }
  else {
    onEntry(bindingTarget.name, value)
  }
}

export function getAllBindingTargetNames(bindingTarget: BindingTarget): string[] {
  const names: string[] = []
  getNamesFromBindingTarget(bindingTarget, names)
  return names
}

function getNamesFromBindingTarget(target: BindingTarget | null, names: string[]): void {
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
    names.push(target.name)
  }
}
