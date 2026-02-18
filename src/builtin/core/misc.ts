import { LitsError } from '../../errors'
import type { Any } from '../../interface'
import type { NamespaceFunction } from '../../parser/types'
import type { SourceCodeInfo } from '../../tokenizer/token'
import { asAny, assertAny } from '../../typeGuards/lits'
import { assertNumber } from '../../typeGuards/number'
import { asStringOrNumber, assertString, assertStringOrNumber } from '../../typeGuards/string'
import { compare, deepEqual } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { BuiltinNormalExpressions } from '../interface'

function isEqual([first, ...rest]: unknown[], sourceCodeInfo: SourceCodeInfo | undefined) {
  const firstAny = asAny(first, sourceCodeInfo)
  for (const param of rest) {
    if (!deepEqual(firstAny, asAny(param, sourceCodeInfo), sourceCodeInfo))
      return false
  }
  return true
}

function isIdentical([first, ...rest]: unknown[]) {
  for (const param of rest) {
    if (param !== first)
      return false
  }
  return true
}

export const miscNormalExpression: BuiltinNormalExpressions = {
  '==': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return isEqual(params, sourceCodeInfo)
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'any' },
        b: { type: 'any' },
        x: { type: 'any' },
        ys: { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if all `values` are structaul equal to each other, otherwise result is `false`.',
      examples: [
        '1 == 1',
        '[1, 2] == [1, 2]',
        `
{
 a: 1,
 b: 2,
} == {
 b: 2,
 a: 1,
}`,
        '==(1, 1)',
        '==(1.01, 1)',
        '==("1", 1)',
        '==("2", "2", "2", "2")',
        '==(2, 2, 1, 2)',
        '==([1, 2], [1, 2])',
        '==({ a: 1, b: 2 }, { b: 2, a: 1 })',
      ],
    },
  },
  '≠': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return !isEqual(params, sourceCodeInfo)
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'any' },
        b: { type: 'any' },
        x: { type: 'any' },
        ys: { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if all `values` are not equal to each other, otherwise result is `false`. `(≠ a b c)` is same as `(! (== a b c))`.',
      examples: [
        '1 ≠ 2',
        '3 ≠ 3',
        '≠(3)',
        '≠(3, 3, 2)',
        '≠("3", "2", "1", "0",)',
        '≠(0, -0)',
      ],
    },
  },
  'identical?': {
    evaluate: (params): boolean => {
      return isIdentical(params)
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: 'any' },
        b: { type: 'any' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns true if $a and $b are referential equal.',
      examples: [
        'identical?({ a: 10, b: 20 }, { b: 20, a: 10 })',
        'identical?([1, true, null], [1, true, null])',
        'identical?(0.3, 0.1 + 0.2)',
      ],
    },
  },
  '>': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) <= 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['number', 'string'] },
        b: { type: ['number', 'string'] },
        x: { type: ['number', 'string'] },
        ys: { type: ['number', 'string'], rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if $x and $ys are in decreasing order, `false` otherwise.',
      examples: [
        '>(1, 0)',
        '>(1.01, 1)',
        '>(1, 1)',
        '>(4, 3, 2, 1)',
        '>(3, 2, 2, 1)',
      ],
    },
  },

  '<': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) >= 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['number', 'string'] },
        b: { type: ['number', 'string'] },
        x: { type: ['number', 'string'] },
        ys: { type: ['number', 'string'], rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if $x and $ys are in increasing order, `false` otherwise.',
      examples: [
        '<(0, 1)',
        '<(1, 1.01)',
        '<(1, 1)',
        '<(1, 2, 2, 3)',
        '<("a", "b")',
      ],
    },
  },
  '>=': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) < 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['number', 'string'] },
        b: { type: ['number', 'string'] },
        x: { type: ['number', 'string'] },
        ys: { type: ['number', 'string'], rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if $x and $ys are in non increasing order, `false` otherwise.',
      examples: [
        '1 >= 1',
        '0 >= 1',
        '>=(1, 0)',
        '>=(1.01, 1)',
        '>=(1, 1)',
        '>=(4, 3, 2, 1)',
        '>=(3, 2, 2, 1)',
      ],
    },
  },
  '<=': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) > 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: {
        a: { type: ['number', 'string'] },
        b: { type: ['number', 'string'] },
        x: { type: ['number', 'string'] },
        ys: { type: ['number', 'string'], rest: true },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'ys'] },
      ],
      description: 'Returns `true` if $x and $ys are in non decreasing order, `false` otherwise.',
      examples: [
        '1 <= 1',
        '<=(0, 1)',
        '<=(1, 1.01)',
        '<=(1, 1)',
        '<=(1, 2, 3, 4)',
        '<=(1, 2, 2, 3)',
      ],
    },
  },
  '!': {
    evaluate: ([first]): boolean => !first,
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Computes logical negation. Note that any other $x than `false`, `0`, `null` and `\'\'` is truthy.',
      examples: [
        '!(3)',
        '!(true)',
        '!("A string")',
        '!(0)',
        '!(false)',
        '!(null)',
        '!("")',
      ],
    },
  },
  'epoch->iso-date': {
    evaluate: ([ms], sourceCodeInfo): string => {
      assertNumber(ms, sourceCodeInfo)
      return new Date(ms).toISOString()
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'string' },
      args: { ms: { type: 'number' } },
      variants: [{ argumentNames: ['ms'] }],
      description: 'Returns IOS date time string from `ms` (milliseconds elapsed since the UNIX epoch).',
      examples: [
        'epoch->iso-date(1649756230899)',
        'epoch->iso-date(0)',
      ],
    },
  },
  'iso-date->epoch': {
    evaluate: ([dateTime], sourceCodeInfo): number => {
      assertString(dateTime, sourceCodeInfo)
      const ms = new Date(dateTime).valueOf()
      assertNumber(ms, sourceCodeInfo, { finite: true })
      return ms
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'number' },
      args: { iso: { type: 'string' } },
      variants: [{ argumentNames: ['iso'] }],
      description: 'Returns milliseconds elapsed since the UNIX epoch to `iso`.',
      examples: [
        'iso-date->epoch("2022-04-12T09:37:10.899Z")',
        'iso-date->epoch("1980-01-01")',
      ],
    },
  },
  'write!': {
    evaluate: (params, sourceCodeInfo): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0)
        return asAny(params[params.length - 1], sourceCodeInfo)

      return null
    },
    arity: {},
    docs: {
      category: 'Misc',
      returns: { type: 'any' },
      args: { values: { type: 'any', rest: true } },
      variants: [{ argumentNames: ['values'] }],
      description: 'It logs the $values and then returns the last argument. If called with no arguments `null` is returned.',
      examples: [
        'write!("A string")',
        'write!(100, "items")',
        'write!(object("a", 10))',
        'write!(["a", "b", "c"])',
        'write!(#"^start")',
        'write!(null, true, false)',
      ],
      hideOperatorForm: true,
    },
  },
  'boolean': {
    evaluate: ([value]): boolean => {
      return !!value
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Coerces $x to boolean.',
      examples: [
        'boolean(0)',
        'boolean(1)',
        'boolean(null)',
        'boolean("Albert")',
      ],
    },
  },
  'compare': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertStringOrNumber(a, sourceCodeInfo)
      assertStringOrNumber(b, sourceCodeInfo)
      return compare(a, b, sourceCodeInfo)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Misc',
      returns: { type: 'number' },
      args: {
        a: { type: ['number', 'string'] },
        b: { type: ['number', 'string'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Compares two values. Returns `-1` if $a < $b, `1` if $a > $b and `0` if $a and $b have the same sort order.',
      examples: [
        'compare(0, 1)',
        'compare(0, 0)',
        'compare(1, 0)',
        'compare("Albert", "Mojir")',
      ],
    },
  },
  'json-parse': {
    evaluate: ([first], sourceCodeInfo): Any => {
      assertString(first, sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-return
      return JSON.parse(first)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'any' },
      args: { x: { type: 'string' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `JSON.parse(`$x`)`.',
      examples: [
        'json-parse("[1, 2, 3]")',
      ],
    },
  },
  'json-stringify': {
    evaluate: ([first, second], sourceCodeInfo): string => {
      assertAny(first, sourceCodeInfo)
      if (second === undefined)
        return JSON.stringify(first)

      assertNumber(second, sourceCodeInfo)
      return JSON.stringify(first, null, second)
    },
    arity: { min: 1, max: 2 },
    docs: {
      category: 'Misc',
      returns: { type: 'string' },
      args: {
        x: { type: 'any' },
        indent: { type: 'integer', description: 'Number of spaces to use for indentation.' },
      },
      variants: [
        { argumentNames: ['x'] },
        { argumentNames: ['x', 'indent'] },
      ],
      description: 'Returns `JSON.stringify(`$x`)`. If second argument is provided, returns `JSON.stringify(`$x`, null, `$indent`)`.',
      examples: [
        'json-stringify([1, 2, 3])',
        'json-stringify({ a: { b: 10 }}, 2)',
      ],
      hideOperatorForm: true,
    },
  },
  'import': {
    evaluate: ([importPath], sourceCodeInfo, contextStack): NamespaceFunction | Record<string, NamespaceFunction> => {
      assertString(importPath, sourceCodeInfo)

      // Check if importing a specific function (e.g., "Grid.row")
      const dotIndex = importPath.indexOf('.')
      if (dotIndex !== -1) {
        const namespaceName = importPath.substring(0, dotIndex)
        const functionName = importPath.substring(dotIndex + 1)

        const namespace = contextStack.getNamespace(namespaceName)
        if (!namespace) {
          throw new LitsError(`Unknown namespace: '${namespaceName}'`, sourceCodeInfo)
        }

        const expression = namespace.functions[functionName]

        if (!expression) {
          throw new LitsError(`Function '${functionName}' not found in namespace '${namespaceName}'`, sourceCodeInfo)
        }

        return {
          [FUNCTION_SYMBOL]: true,
          sourceCodeInfo,
          functionType: 'Namespace',
          namespaceName,
          functionName,
          arity: expression.arity,
        }
      }

      // Import entire namespace
      const namespaceName = importPath
      const namespace = contextStack.getNamespace(namespaceName)
      if (!namespace) {
        throw new LitsError(`Unknown namespace: '${namespaceName}'`, sourceCodeInfo)
      }

      // Create an object where each key is a function name and value is a NamespaceFunction
      const result: Record<string, NamespaceFunction> = {}
      for (const [functionName, expression] of Object.entries(namespace.functions)) {
        result[functionName] = {
          [FUNCTION_SYMBOL]: true,
          sourceCodeInfo,
          functionType: 'Namespace',
          namespaceName,
          functionName,
          arity: expression.arity,
        }
      }
      return result
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Misc',
      returns: { type: 'any' },
      args: {
        path: {
          type: 'string',
          description: 'The namespace path to import. Can be a namespace name (e.g., "Vector", "Grid") or a fully qualified function name (e.g., "Vector.mean", "Grid.row").',
        },
      },
      variants: [{ argumentNames: ['path'] }],
      description: 'Imports namespace functions. Use a namespace name (e.g., "Vector") to import all functions as an object, or a fully qualified name (e.g., "Vector.mean") to import a single function directly.',
      examples: [
        'let v = import("Vector"); v.mean([1, 2, 3, 4])',
        'let sum = import("Vector.sum"); sum([1, 2, 3])',
        'let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)',
      ],
    },
  },
}
