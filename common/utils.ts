import type { UnknownRecord } from '../src/interface'
import { isRegularExpression } from '../src/typeGuards/lits'
import { isLitsFunction } from '../src/typeGuards/litsFunction'

export function stringifyValue(value: unknown, html: boolean): string {
  const gt = html ? '&gt;' : '>'
  const lt = html ? '&lt;' : '<'
  if (isLitsFunction(value)) {
    if (value.functionType === 'Builtin')
      return `${lt}builtin function ${value.n}${gt}`
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

  if (typeof value === 'number' && Number.isNaN(value))
    return 'NaN'

  if (isRegularExpression(value))
    return `/${value.s}/${value.f}`

  if (typeof value === 'string')
    return `"${value}"`

  return JSON.stringify(value, null, 2)
}

export function findAllOccurrences(input: string, pattern: RegExp): Set<string> {
  const matches = [...input.matchAll(pattern)]
  return new Set(matches.map(match => match[0]))
}
