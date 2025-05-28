import type { Arity } from '../builtin/interface'
import { LitsError } from '../errors'
import type { FunctionLike } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { isColl } from '../typeGuards/lits'
import { valueToString } from './debug/debugTools'

export function arityAccepts(arity: Arity, nbrOfParams: number): boolean {
  const { min, max } = arity
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  if (typeof max === 'number' && nbrOfParams > max) {
    return false
  }
  return true
}

export function arityAcceptsMin(arity: Arity, nbrOfParams: number): boolean {
  const { min } = arity
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  return true
}

export function getCommonArityFromFunctions(params: FunctionLike[]): Arity | null {
  return params.reduce((acc: Arity | null, param): Arity | null => {
    if (acc === null) {
      return null
    }
    const arity: Arity = (typeof param === 'number' || isColl(param)) ? toFixedArity(1) : param.arity
    const { min: aMin, max: aMax } = arity
    const { min: bMin, max: bMax } = acc
    const min = typeof aMin === 'number' && typeof bMin === 'number'
      ? Math.max(aMin, bMin)
      : typeof aMin === 'number' ? aMin : typeof bMin === 'number' ? bMin : undefined
    const max = typeof aMax === 'number' && typeof bMax === 'number'
      ? Math.min(aMax, bMax)
      : typeof aMax === 'number' ? aMax : typeof bMax === 'number' ? bMax : undefined

    if (typeof min === 'number' && typeof max === 'number' && min > max) {
      return null
    }

    return { min, max }
  }, {})
}

export function getArityFromFunction(param: FunctionLike): Arity {
  return (typeof param === 'number' || isColl(param)) ? toFixedArity(1) : param.arity
}

export function assertNumberOfParams(arity: Arity, length: number, sourceCodeInfo: SourceCodeInfo | undefined): void {
  const { min, max } = arity
  if (typeof min === 'number' && length < min) {
    throw new LitsError(
      `Wrong number of arguments, expected at least ${min}, got ${valueToString(length)}.`,
      sourceCodeInfo,
    )
  }

  if (typeof max === 'number' && length > max) {
    throw new LitsError(
      `Wrong number of arguments, expected at most ${max}, got ${valueToString(length)}.`,
      sourceCodeInfo,
    )
  }
}

export function canBeOperator(count: Arity): boolean {
  if (typeof count.max === 'number' && count.max < 2) {
    return false
  }

  if (typeof count.min === 'number' && count.min > 2) {
    return false
  }

  return true
}

export function toFixedArity(arity: number): Arity {
  return { min: arity, max: arity }
}
