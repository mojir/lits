import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'

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
  'n:divisors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return getDivisors(number)
    },
    paramCount: 1,
  },
  'n:count-divisors': {
    evaluate: ([number], sourceCodeInfo): number => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return calcUnsortedDivisors(number).length
    },
    paramCount: 1,
  },
  'n:proper-divisors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return getProperDivisors(number)
    },
    paramCount: 1,
  },
  'n:count-proper-divisors': {
    evaluate: ([number], sourceCodeInfo): number => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return calcUnsortedDivisors(number).length - 1 // Exclude the number itself
    },
    paramCount: 1,
  },
}
