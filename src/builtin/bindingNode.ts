import type { Any } from '../interface'
import type { BindingNode, BindingTarget } from '../parser/types'
import type { Token } from '../tokenizer/token'
import { assertUnknownRecord } from '../typeGuards'
import { assertArray } from '../typeGuards/array'
import { assertAny } from '../typeGuards/lits'

export function bindingNodeEntries(
  bindingNode: BindingNode,
  value: Any,
  onEntry: (name: string, value: Any) => void,
): void {
  const { target, token } = bindingNode
  bindingTargetEntries(target, value, onEntry, token)
}

export function bindingTargetEntries(
  bindingTarget: BindingTarget,
  value: Any,
  onEntry: (name: string, value: Any) => void,
  token: Token | undefined,
): void {
  if (bindingTarget.type === 'object') {
    Object.entries(bindingTarget.elements).forEach(([key, element]) => {
      assertUnknownRecord(value, token?.[2])
      const val = value[key] ?? null
      assertAny(val, token?.[2])
      bindingTargetEntries(element, val, onEntry, token)
    })
  }
  else if (bindingTarget.type === 'array') {
    bindingTarget.elements.forEach((element, index) => {
      assertArray(value, token?.[2])
      const val = value[index] ?? null
      assertAny(val, token?.[2])
      bindingTargetEntries(element, val, onEntry, token)
    })
  }
  else {
    onEntry(bindingTarget.alias ?? bindingTarget.name, value)
  }
}

export function getAllBindingTargetNames(bindingTarget: BindingTarget): string[] {
  const names: string[] = []
  getNamesFromBindingTarget(bindingTarget, names)
  return names
}

function getNamesFromBindingTarget(target: BindingTarget, names: string[]): void {
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
    names.push(target.alias ?? target.name)
  }
}
