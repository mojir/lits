import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { Argument, BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'

function getOperatorArgs(a: 'integer', b: 'integer'): Record<string, Argument> {
  return { a: { type: a }, b: { type: b } }
}

const bitwiseUtilsNormalExpression: BuiltinNormalExpressions = {
  'bit-not': {
    evaluate: ([num], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      return ~num
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Bitwise',
      returns: { type: 'integer' },
      args: { a: { type: 'integer' } },
      variants: [{ argumentNames: ['a'] }],
      description: 'Returns bitwise `not` of $a.',
      seeAlso: ['&', '|', 'xor', 'Bitwise.bit-and-not'],
      examples: [
        'let { bit-not } = import("Bitwise");\nbit-not(0)',
        'let { bit-not } = import("Bitwise");\nbit-not(255)',
      ],
    },
  },
  'bit-and-not': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & ~value
      }, first)
    },
    arity: { min: 2 },
    docs: {
      category: 'Bitwise',
      returns: { type: 'integer' },
      args: {
        ...getOperatorArgs('integer', 'integer'),
        c: { type: 'integer', rest: true },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'c'] },
      ],
      description: 'Returns bitwise `and` with complement.',
      seeAlso: ['&', '|', 'xor', 'Bitwise.bit-not'],
      examples: [
        'let { bit-and-not } = import("Bitwise");\n0b0011 bit-and-not 0b0110',
        'let { bit-and-not } = import("Bitwise");\nbit-and-not(0b0011, 0b0110)',
        'let { bit-and-not } = import("Bitwise");\nbit-and-not(0b0011, 0b0110, 0b1001)',
      ],
    },
  },
  'bit-flip': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num ^= mask)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Flips bit number $b.',
      seeAlso: ['Bitwise.bit-set', 'Bitwise.bit-clear', 'Bitwise.bit-test'],
      examples: [
        'let { bit-flip } = import("Bitwise");\n0b0011 bit-flip 1',
        'let { bit-flip } = import("Bitwise");\nbit-flip(0b0011, 1)',
        'let { bit-flip } = import("Bitwise");\nbit-flip(0b1100, 1)',
      ],
    },
  },
  'bit-set': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num |= mask)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Sets bit number $b.',
      seeAlso: ['Bitwise.bit-flip', 'Bitwise.bit-clear', 'Bitwise.bit-test'],
      examples: [
        'let { bit-set } = import("Bitwise");\n0b0010 bit-set 1',
        'let { bit-set } = import("Bitwise");\nbit-set(0b0011, 1)',
        'let { bit-set } = import("Bitwise");\nbit-set(0b1100, 1)',
      ],
    },
  },
  'bit-clear': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num &= ~mask)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Clears bit number $b.',
      seeAlso: ['Bitwise.bit-flip', 'Bitwise.bit-set', 'Bitwise.bit-test'],
      examples: [
        'let { bit-clear } = import("Bitwise");\n0b0011 bit-clear 1',
        'let { bit-clear } = import("Bitwise");\nbit-clear(0b0011, 1)',
        'let { bit-clear } = import("Bitwise");\nbit-clear(0b1100, 1)',
      ],
    },
  },
  'bit-test': {
    evaluate: ([num, index], sourceCodeInfo): boolean => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return !!(num & mask)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Bitwise',
      returns: { type: 'boolean' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Checks if bit number $b is set.',
      seeAlso: ['Bitwise.bit-flip', 'Bitwise.bit-set', 'Bitwise.bit-clear'],
      examples: [
        'let { bit-test } = import("Bitwise");\n0b0011 bit-test 1',
        'let { bit-test } = import("Bitwise");\nbit-test(0b0011, 1)',
        'let { bit-test } = import("Bitwise");\nbit-test(0b1100, 1)',
      ],
    },
  },
}

export const bitwiseUtilsModule: LitsModule = {
  name: 'Bitwise',
  functions: bitwiseUtilsNormalExpression,
}
