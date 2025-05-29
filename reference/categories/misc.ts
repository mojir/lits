import type { FunctionReference } from '..'
import { type MiscApiName, getOperatorArgs } from '../api'

export const miscReference: Record<MiscApiName, FunctionReference<'Misc'>> = {
  '!=': {
    title: '!=',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
      x: {
        type: 'any',
      },
      ys: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if all `values` are not equal to each other, otherwise result is `false`. `(!= a b c)` is same as `(! (== a b c))`.',
    examples: [
      '1 ≠ 2',
      '3 != 3',
      '≠(3)',
      '!=(3, 3, 2)',
      '≠("3", "2", "1", "0",)',
      '!=(0, -0)',
    ],
    aliases: ['≠'],
  },
  '==': {
    title: '==',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
      x: {
        type: 'any',
      },
      ys: {
        type: 'any',
        rest: true,
      },
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
  '<': {
    title: '<',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs(['number', 'string'], ['number', 'string']),
      x: {
        type: ['number', 'string'],
      },
      ys: {
        type: ['number', 'string'],
        rest: true,
      },
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
  '>': {
    title: '>',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs(['number', 'string'], ['number', 'string']),
      x: {
        type: ['number', 'string'],
      },
      ys: {
        type: ['number', 'string'],
        rest: true,
      },
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
  '<=': {
    title: '<=',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs(['number', 'string'], ['number', 'string']),
      x: {
        type: ['number', 'string'],
      },
      ys: {
        type: ['number', 'string'],
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if $x and $ys are in non decreasing order, `false` otherwise.',
    examples: [
      '1 ≤ 1',
      '<=(0, 1)',
      '≤(1, 1.01)',
      '<=(1, 1)',
      '≤(1, 2, 3, 4)',
      '<=(1, 2, 2, 3)',
    ],
    aliases: ['≤'],
  },
  '>=': {
    title: '>=',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs(['number', 'string'], ['number', 'string']),
      x: {
        type: ['number', 'string'],
      },
      ys: {
        type: ['number', 'string'],
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if $x and $ys are in non increasing order, `false` otherwise.',
    examples: [
      '1 ≥ 1',
      '0 ≥ 1',
      '>=(1, 0)',
      '≥(1.01, 1)',
      '>=(1, 1)',
      '≥(4, 3, 2, 1)',
      '>=(3, 2, 2, 1)',
    ],
    aliases: ['≥'],
  },
  '!': {
    title: '!',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
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
  'write!': {
    title: 'write!',
    category: 'Misc',
    returns: {
      type: 'any',
    },
    args: {
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['values'] },
    ],
    description: 'It logs the $values and then returns the last argument. If called with no arguments `null` is returned.',
    examples: [
      'write!("A string")',
      'write!(100, "items")',
      'write!(object("a", 10))',
      'write!(["a", "b", "c"])',
      'write!(#"^start")',
      'write!(null, true, false)',
    ],
    noOperatorDocumentation: true,
  },
  'iso-date->epoch': {
    title: 'iso-date->epoch',
    category: 'Misc',
    returns: {
      type: 'number',
    },
    args: {
      iso: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['iso'] },
    ],
    description: 'Returns milliseconds elapsed since the UNIX epoch to `iso`.',
    examples: [
      'iso-date->epoch("2022-04-12T09:37:10.899Z")',
      'iso-date->epoch("1980-01-01")',
    ],
  },
  'epoch->iso-date': {
    title: 'epoch->iso-date',
    category: 'Misc',
    returns: {
      type: 'string',
    },
    args: {
      ms: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['ms'] },
    ],
    description: 'Returns IOS date time string from `ms` (milliseconds elapsed since the UNIX epoch).',
    examples: [
      'epoch->iso-date(1649756230899)',
      'epoch->iso-date(0)',
    ],
  },
  'boolean': {
    title: 'boolean',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Coerces $x to boolean.',
    examples: [
      'boolean(0)',
      'boolean(1)',
      'boolean(null)',
      'boolean("Albert")',
    ],
  },
  'compare': {
    title: 'compare',
    category: 'Misc',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs(['number', 'string'], ['number', 'string']),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Compares two values. Returns `-1` if $a < $b, `1` if $a > $b and `0` if $a and $b have the same sort order.',
    examples: [
      'compare(0, 1)',
      'compare(0, 0)',
      'compare(1, 0)',
      'compare("Albert", "Mojir")',
    ],
  },
  'identical?': {
    title: 'identical?',
    category: 'Misc',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns true if $a and $b are referential equal.',
    examples: [
      'identical?({ a: 10, b: 20 }, { b: 20, a: 10 })',
      'identical?([1, true, null], [1, true, null])',
      'identical?(0.3, 0.1 + 0.2)',
    ],
  },
  'json-parse': {
    title: 'json-parse',
    category: 'Misc',
    returns: {
      type: 'any',
    },
    args: {
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `JSON.parse(`$x`)`.',
    examples: [
      'json-parse("[1, 2, 3]")',
    ],
  },
  'json-stringify': {
    title: 'json-stringify',
    category: 'Misc',
    returns: {
      type: 'string',
    },
    args: {
      x: {
        type: 'any',
      },
      indent: {
        type: 'integer',
        description: 'Number of spaces to use for indentation.',
      },
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
    noOperatorDocumentation: true,
  },
}
