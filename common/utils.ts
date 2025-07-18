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

  if (typeof value === 'number') {
    return `${value}`
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
        return cell
      }).join(',\n  ')}\n]`
    }
    else {
      return `[${value.map((cell) => {
        return cell
      }).join(', ')}]`
    }
  }

  return JSON.stringify(replaceInfinities(value), null, 2)
}

function replaceInfinities(value: unknown): unknown {
  if (value === Number.POSITIVE_INFINITY) {
    return '∞'
  }
  if (value === Number.NEGATIVE_INFINITY) {
    return '-∞'
  }
  if (Array.isArray(value)) {
    return value.map(replaceInfinities)
  }
  if (typeof value === 'object' && value !== null) {
    const result: Record<string, unknown> = {}
    for (const [key, val] of Object.entries(value)) {
      result[key] = replaceInfinities(val)
    }
    return result
  }
  return value
}

function stringifyMatrix(matrix: (null | number | string | boolean)[][]): string {
  const padding = matrix.flat().reduce((max: number, cell) => Math.max(max, `${cell}`.length), 0) + 1
  const rows = matrix.map(row => `[${row.map(cell => `${cell}`.padStart(padding)).join(' ')} ]`)
  return rows.join('\n')
}

export function findAllOccurrences(input: string, pattern: RegExp): Set<string> {
  const matches = [...input.matchAll(pattern)]
  return new Set(matches.map(match => match[0]))
}
