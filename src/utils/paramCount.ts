import type { ParamCount } from '../builtin/interface'
import { getNodeTypeName } from '../constants/constants'
import { LitsError } from '../errors'
import type { FunctionLike, NormalExpressionNodeWithName } from '../parser/types'
import { isColl } from '../typeGuards/lits'
import { valueToString } from './debug/debugTools'

export function paramCountAccepts(paramsCount: ParamCount, nbrOfParams: number): boolean {
  if (typeof paramsCount === 'number') {
    return paramsCount === nbrOfParams
  }
  const { min, max, even, odd } = paramsCount
  if (even && nbrOfParams % 2 !== 0) {
    return false
  }
  if (odd && nbrOfParams % 2 !== 1) {
    return false
  }
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  if (typeof max === 'number' && nbrOfParams > max) {
    return false
  }
  return true
}

export function paramCountAcceptsMin(paramsCount: ParamCount, nbrOfParams: number): boolean {
  if (typeof paramsCount === 'number') {
    return nbrOfParams >= paramsCount
  }
  const { min } = paramsCount
  if (typeof min === 'number' && nbrOfParams < min) {
    return false
  }
  return true
}

export function getCommonParamCountFromFunctions(params: FunctionLike[]): ParamCount | null {
  return params.reduce((acc: ParamCount | null, param) => {
    if (acc === null) {
      return null
    }
    const paramCount = (typeof param === 'number' || isColl(param)) ? 1 : param.paramCount
    if (typeof acc === 'number' && typeof paramCount === 'number') {
      return acc === paramCount ? acc : null
    }
    if (typeof paramCount === 'number') {
      if (paramCountAccepts(acc, paramCount)) {
        return paramCount
      }
      return null
    }
    if (typeof acc === 'number') {
      if (paramCountAccepts(paramCount, acc)) {
        return acc
      }
      return null
    }
    const { min: aMin, max: aMax, even: aEven, odd: aOdd } = paramCount
    const { min: bMin, max: bMax, even: bEven, odd: bOdd } = acc
    let min = typeof aMin === 'number' && typeof bMin === 'number'
      ? Math.max(aMin, bMin)
      : typeof aMin === 'number' ? aMin : typeof bMin === 'number' ? bMin : undefined
    let max = typeof aMax === 'number' && typeof bMax === 'number'
      ? Math.min(aMax, bMax)
      : typeof aMax === 'number' ? aMax : typeof bMax === 'number' ? bMax : undefined
    const even = aEven ?? bEven
    const odd = aOdd ?? bOdd

    if (even && odd) {
      return null
    }

    if (even) {
      if (typeof min === 'number' && min % 2 !== 0) {
        min += 1
      }
      if (typeof max === 'number' && max % 2 !== 0) {
        max -= 1
      }
    }

    if (odd) {
      if (typeof min === 'number' && min % 2 === 0) {
        min += 1
      }
      if (typeof max === 'number' && max % 2 === 0) {
        max -= 1
      }
    }

    if (typeof min === 'number' && typeof max === 'number' && min > max) {
      return null
    }

    if (typeof min === 'number' && min === max) {
      return min
    }

    return { min, max, even, odd }
  }, {})
}

export function getParamCountFromFunction(param: FunctionLike): ParamCount {
  return (typeof param === 'number' || isColl(param)) ? 1 : param.paramCount
}

export function assertNumberOfParams(count: ParamCount, node: NormalExpressionNodeWithName): void {
  const length = node[1][1].length
  if (typeof count === 'number') {
    if (length !== count) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected ${count}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
  else {
    const { min, max, even, odd } = count
    if (even) {
      const name = getNodeTypeName(node[0])
      if (length % 2 !== 0) {
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an even number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (odd) {
      if (length % 2 !== 1) {
        const name = getNodeTypeName(node[0])
        throw new LitsError(
          `Wrong number of arguments to "${name}",, expected an odd number, got ${valueToString(length)}.`,
          node[2],
        )
      }
    }

    if (typeof min === 'number' && length < min) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at least ${min}, got ${valueToString(length)}.`,
        node[2],
      )
    }

    if (typeof max === 'number' && length > max) {
      const name = getNodeTypeName(node[0])
      throw new LitsError(
        `Wrong number of arguments to "${name}", expected at most ${max}, got ${valueToString(length)}.`,
        node[2],
      )
    }
  }
}
