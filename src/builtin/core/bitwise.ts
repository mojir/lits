import { assertNumber } from '../../typeGuards/number'
import { toFixedArity } from '../../utils/arity'
import type { Argument, BuiltinNormalExpressions } from '../interface'

function getOperatorArgs(a: 'integer', b: 'integer'): Record<string, Argument> {
  return { a: { type: a }, b: { type: b } }
}

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  '<<': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num << count
    },
    arity: toFixedArity(2),
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Shifts $a arithmetically left by $b bit positions.',
      seeAlso: ['>>', '>>>'],
      examples: [
        '1 << 10',
        '<<(1, 10)',
        '<<(-4, 2)',
      ],
    },
  },
  '>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >> count
    },
    arity: toFixedArity(2),
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Shifts $a arithmetically right by $b bit positions.',
      seeAlso: ['<<', '>>>'],
      examples: [
        '2048 >> 10',
        '>>(2048, 10)',
        '>>>(-16, 2)',
        '>>(4, 10)',
      ],
    },
  },
  '>>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >>> count
    },
    arity: toFixedArity(2),
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: { ...getOperatorArgs('integer', 'integer') },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Shifts $a arithmetically right by $b bit positions without sign extension.',
      seeAlso: ['<<', '>>'],
      examples: [
        '-16 >>> 2',
        '>>>(2048, 10)',
        '>>>(-16, 2)',
        '>>>(4, 10)',
        '>>>(-1, 10)',
      ],
    },
  },
  '&': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & value
      }, first)
    },
    arity: { min: 2 },
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: {
        ...getOperatorArgs('integer', 'integer'),
        c: { type: 'integer', rest: true },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'c'] },
      ],
      description: 'Returns bitwise `and` of all arguments.',
      seeAlso: ['|', 'xor', 'bitwise.bit-not', 'bitwise.bit-and-not'],
      examples: [
        '0b0011 & 0b0110',
        '&(0b0011, 0b0110)',
        '&(0b0011, 0b0110, 0b1001)',
      ],
    },
  },
  '|': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result | value
      }, first)
    },
    arity: { min: 2 },
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: {
        ...getOperatorArgs('integer', 'integer'),
        c: { type: 'integer', rest: true },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'c'] },
      ],
      description: 'Returns bitwise `or` of all arguments.',
      seeAlso: ['&', 'xor', 'bitwise.bit-not', 'bitwise.bit-and-not'],
      examples: [
        '0b0011 | 0b0110',
        '|(0b0011, 0b0110)',
        '|(0b1000, 0b0100, 0b0010)',
      ],
    },
  },
  'xor': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result ^ value
      }, first)
    },
    arity: { min: 2 },
    docs: {
      category: 'bitwise',
      returns: { type: 'integer' },
      args: {
        ...getOperatorArgs('integer', 'integer'),
        c: { type: 'integer', rest: true },
      },
      variants: [
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'c'] },
      ],
      description: 'Returns bitwise `xor` of all arguments.',
      seeAlso: ['&', '|', 'bitwise.bit-not', 'bitwise.bit-and-not'],
      examples: [
        '0b0011 xor 0b0110',
        'xor(0b0011, 0b0110)',
        'xor(0b11110000, 0b00111100, 0b10101010)',
      ],
    },
  },
}
