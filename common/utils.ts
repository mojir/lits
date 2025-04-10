import { prettyPi } from '@mojir/pretty-pi'
import type { UnknownRecord } from '../src/interface'
import { isRegularExpression } from '../src/typeGuards/lits'
import { isLitsFunction } from '../src/typeGuards/litsFunction'
import { isMatrix, isVector } from '../src/typeGuards/annotatedArrays'

export function stringifyValue(value: unknown, html: boolean): string {
  const gt = html ? '&gt;' : '>'
  const lt = html ? '&lt;' : '<'
  if (isLitsFunction(value)) {
    if (value.functionType === 'Builtin')
      return `${lt}builtin function ${value.normalBuitinSymbolType}${gt}`
    else
      return `${lt}function ${(value as unknown as UnknownRecord).n ?? '\u03BB'}${gt}`
  }
  if (value === null)
    return 'null'

  if (typeof value === 'object' && value instanceof Error)
    return value.toString()

  if (typeof value === 'object' && value instanceof RegExp)
    return `${value}`

  if (value === Number.POSITIVE_INFINITY)
    return `${Number.POSITIVE_INFINITY}`

  if (value === Number.NEGATIVE_INFINITY)
    return `${Number.NEGATIVE_INFINITY}`

  if (typeof value === 'number') {
    return prettyPi(value)
  }

  if (isRegularExpression(value))
    return `/${value.s}/${value.f}`

  if (typeof value === 'string')
    return `"${value}"`

  if (Array.isArray(value) && isMatrix(value))
    return stringifyMatrix(value)

  if (Array.isArray(value) && isVector(value)) {
    if (value.length === 0)
      return '[]'

    if (value.length > 8) {
      return `[\n  ${value.map((cell) => {
        return prettyPi(cell)
      }).join(',\n  ')}\n]`
    }
    else {
      return `[${value.map((cell) => {
        return prettyPi(cell)
      }).join(', ')}]`
    }
  }

  return JSON.stringify(value, null, 2)
}

function prettyIfNumber(value: unknown): string {
  if (typeof value === 'number') {
    return prettyPi(value)
  }
  return `${value}`
}

function stringifyMatrix(matrix: (null | number | string | boolean)[][]): string {
  const padding = matrix.flat().reduce((max: number, cell) => Math.max(max, prettyIfNumber(cell).length), 0) + 1
  const rows = matrix.map(row => `[${row.map(cell => prettyIfNumber(cell).padStart(padding)).join(' ')} ]`)
  return rows.join('\n')
}

// function isMatrix(value: unknown): value is (null | number | string | boolean)[][] {
//   if (!Array.isArray(value)) {
//     return false
//   }
//   if (!value.every(row => Array.isArray(row))) {
//     return false
//   }
//   let cols = -1
//   for (const row of value) {
//     if (cols === -1) {
//       cols = row.length
//       if (cols === 0) {
//         return false
//       }
//     }
//     else {
//       if (row.length !== cols) {
//         return false
//       }
//     }
//     for (const cell of row) {
//       if (typeof cell !== 'number' && typeof cell !== 'string' && typeof cell !== 'boolean' && cell !== null) {
//         return false
//       }
//     }
//   }
//   return true
// }

export function findAllOccurrences(input: string, pattern: RegExp): Set<string> {
  const matches = [...input.matchAll(pattern)]
  return new Set(matches.map(match => match[0]))
}
