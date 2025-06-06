import { type BitwiseApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const bitwiseReference: Record<BitwiseApiName, FunctionReference<'Bitwise'>> = {
  '<<': {
    title: '<<',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Shifts $a arithmetically left by $b bit positions.',
    examples: [
      '1 << 10',
      '<<(1, 10)',
      '<<(-4, 2)',
    ],
  },
  '>>': {
    title: '>>',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Shifts $a arithmetically right by $b bit positions.',
    examples: [
      '2048 >> 10',
      '>>(2048, 10)',
      '>>>(-16, 2)',
      '>>(4, 10)',
    ],
  },
  '>>>': {
    title: '>>>',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Shifts $a arithmetically right by $b bit positions without sign extension.',
    examples: [
      '-16 >>> 2',
      '>>>(2048, 10)',
      '>>>(-16, 2)',
      '>>>(4, 10)',
      '>>>(-1, 10)',
    ],
  },
  'bit-not': {
    title: 'bit-not',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['a'] },
    ],
    description: 'Returns bitwise `not` of $a.',
    examples: [
      'bit-not(0)',
      'bit-not(255)',
    ],
  },
  '&': {
    title: '&',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
      c: {
        type: 'integer',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: 'Returns bitwise `and` of all arguments.',
    examples: [
      '0b0011 & 0b0110',
      '&(0b0011, 0b0110)',
      '&(0b0011, 0b0110, 0b1001)',
    ],
  },
  'bit-and-not': {
    title: 'bit-and-not',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
      c: {
        type: 'integer',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: 'Returns bitwise `and` with complement.',
    examples: [
      '0b0011 bit-and-not 0b0110',
      'bit-and-not(0b0011, 0b0110)',
      'bit-and-not(0b0011, 0b0110, 0b1001)',
    ],
  },
  '|': {
    title: '|',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
      c: {
        type: 'integer',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: 'Returns bitwise `or` of all arguments.',
    examples: [
      '0b0011 | 0b0110',
      '|(0b0011, 0b0110)',
      '|(0b1000, 0b0100, 0b0010)',
    ],
  },
  'xor': {
    title: 'xor',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
      c: {
        type: 'integer',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: 'Returns bitwise `xor` of all arguments.',
    examples: [
      '0b0011 xor 0b0110',
      'xor(0b0011, 0b0110)',
      'xor(0b11110000, 0b00111100, 0b10101010)',
    ],
  },
  'bit-flip': {
    title: 'bit-flip',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Flips bit number $b.',
    examples: [
      '0b0011 bit-flip 1',
      'bit-flip(0b0011, 1)',
      'bit-flip(0b1100, 1)',
    ],
  },
  'bit-clear': {
    title: 'bit-clear',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Clears bit number $b.',
    examples: [
      '0b0011 bit-clear 1',
      'bit-clear(0b0011, 1)',
      'bit-clear(0b1100, 1)',
    ],
  },
  'bit-set': {
    title: 'bit-set',
    category: 'Bitwise',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Sets bit number $b.',
    examples: [
      '0b0010 bit-set 1',
      'bit-set(0b0011, 1)',
      'bit-set(0b1100, 1)',
    ],
  },
  'bit-test': {
    title: 'bit-test',
    category: 'Bitwise',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if bit number $b is set.',
    examples: [
      '0b0011 bit-test 1',
      'bit-test(0b0011, 1)',
      'bit-test(0b1100, 1)',
    ],
  },
}
