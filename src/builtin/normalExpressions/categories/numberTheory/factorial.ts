import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { factorialNumbers } from './sequences/factorial'

export function factorialOf(n: number): number {
  if (n < 0)
    throw new Error('Factorial is not defined for negative numbers')

  if (n === 0 || n === 1)
    return 1

  if (n <= 18) {
    return factorialNumbers[n]!
  }
  let result = factorialNumbers[18]!
  for (let i = 19; i <= n; i++)
    result *= i

  return result
}

export const factorialNormalExpressions: BuiltinNormalExpressions = {
  'n:factorial': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true, lte: 170 })
      return factorialOf(n)
    },
    aliases: ['n:!'],
    paramCount: 1,
  },
}
