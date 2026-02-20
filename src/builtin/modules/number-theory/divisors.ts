import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../../builtin/interface'

function calcUnsortedDivisors(number: number): number[] {
  const divisors: number[] = []
  for (let i = 1; i <= Math.sqrt(number); i++) {
    if (number % i === 0) {
      divisors.push(i)
      if (i !== number / i) {
        divisors.push(number / i)
      }
    }
  }
  return divisors
}

export function getDivisors(n: number): number[] {
  const unsortedDivisors = calcUnsortedDivisors(n)
  const sortedDivisors = unsortedDivisors.sort((a, b) => a - b)
  return sortedDivisors
}

export function getProperDivisors(n: number): number[] {
  return getDivisors(n).slice(0, -1) // Exclude the number itself
}

export const divisorsNormalExpressions: BuiltinNormalExpressions = {
  'divisors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return getDivisors(number)
    },
    arity: toFixedArity(1),
  },
  'count-divisors': {
    evaluate: ([number], sourceCodeInfo): number => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return calcUnsortedDivisors(number).length
    },
    arity: toFixedArity(1),
  },
  'proper-divisors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return getProperDivisors(number)
    },
    arity: toFixedArity(1),
  },
  'count-proper-divisors': {
    evaluate: ([number], sourceCodeInfo): number => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return calcUnsortedDivisors(number).length - 1 // Exclude the number itself
    },
    arity: toFixedArity(1),
  },
}
