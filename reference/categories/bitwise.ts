import { type BitwiseApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const bitwiseReference: Record<BitwiseApiName, FunctionReference<'Bitwise'>> = { '<<': {
  title: '<<',
  category: 'Bitwise',
  linkName: '-lt-lt',
  clojureDocs: 'bit-shift-left',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Shifts $x arithmetically left by $n bit positions.',
  examples: [
    '1 << 10',
    '<<(1, 10)',
    '<<(-4, 2)',
  ],
  algebraic: true,
}, '>>': {
  title: '>>',
  category: 'Bitwise',
  linkName: '-gt-gt',
  clojureDocs: 'bit-shift-right',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Shifts $x arithmetically right by $n bit positions.',
  examples: [
    '2048 >> 10',
    '>>(2048, 10)',
    '>>>(-16, 2)',
    '>>(4, 10)',
  ],
  algebraic: true,
}, '>>>': {
  title: '>>>',
  category: 'Bitwise',
  linkName: '-gt-gt-gt',
  clojureDocs: 'unsigned-bit-shift-right',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Shifts $x arithmetically right by $n bit positions without sign extension.',
  examples: [
    '-16 >>> 2',
    '>>>(2048, 10)',
    '>>>(-16, 2)',
    '>>>(4, 10)',
    '>>>(-1, 10)',
  ],
  algebraic: true,
}, '~': {
  title: '~',
  category: 'Bitwise',
  linkName: '-tilde',
  clojureDocs: 'bit-not',
  returns: {
    type: 'integer',
  },
  args: {
    x: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x'] },
  ],
  description: 'Returns bitwise `not` of $x.',
  examples: [
    '~(0)',
    '~(255)',
  ],
  algebraic: true,
}, '&': {
  title: '&',
  category: 'Bitwise',
  linkName: '-and',
  clojureDocs: 'bit-and',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    y: {
      type: 'integer',
    },
    rest: {
      type: 'integer',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['x', 'y'] },
    { argumentNames: ['x', 'y', 'rest'] },
  ],
  description: 'Returns bitwise `and` of all arguments.',
  examples: [
    '0b0011 & 0b0110',
    '&(0b0011, 0b0110)',
    '&(0b0011, 0b0110, 0b1001)',
  ],
  algebraic: true,
}, 'bit-and-not': {
  title: 'bit-and-not',
  category: 'Bitwise',
  linkName: 'bit-and-not',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    y: {
      type: 'integer',
    },
    rest: {
      type: 'integer',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['x', 'y'] },
    { argumentNames: ['x', 'y', 'rest'] },
  ],
  description: 'Returns bitwise `and` with complement.',
  examples: [
    '0b0011 bit-and-not 0b0110',
    'bit-and-not(0b0011, 0b0110)',
    'bit-and-not(0b0011, 0b0110, 0b1001)',
  ],
  algebraic: true,
}, '|': {
  title: '|',
  category: 'Bitwise',
  linkName: '-or',
  clojureDocs: 'bit-or',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    y: {
      type: 'integer',
    },
    rest: {
      type: 'integer',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['x', 'y'] },
    { argumentNames: ['x', 'y', 'rest'] },
  ],
  description: 'Returns bitwise `or` of all arguments.',
  examples: [
    '0b0011 | 0b0110',
    '|(0b0011, 0b0110)',
    '|(0b1000, 0b0100, 0b0010)',
  ],
  algebraic: true,
}, '^': {
  title: '^',
  category: 'Bitwise',
  linkName: '-caret',
  clojureDocs: 'bit-xor',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    y: {
      type: 'integer',
    },
    rest: {
      type: 'integer',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['x', 'y'] },
    { argumentNames: ['x', 'y', 'rest'] },
  ],
  description: 'Returns bitwise `xor` of all arguments.',
  examples: [
    '0b0011 ^ 0b0110',
    '^(0b0011, 0b0110)',
    '^(0b11110000, 0b00111100, 0b10101010)',
  ],
  algebraic: true,
}, 'bit-flip': {
  title: 'bit-flip',
  category: 'Bitwise',
  linkName: 'bit-flip',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Flips bit number $n.',
  examples: [
    '0b0011 bit-flip 1',
    'bit-flip(0b0011, 1)',
    'bit-flip(0b1100, 1)',
  ],
  algebraic: true,
}, 'bit-clear': {
  title: 'bit-clear',
  category: 'Bitwise',
  linkName: 'bit-clear',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Clears bit number $n.',
  examples: [
    '0b0011 bit-clear 1',
    'bit-clear(0b0011, 1)',
    'bit-clear(0b1100, 1)',
  ],
  algebraic: true,
}, 'bit-set': {
  title: 'bit-set',
  category: 'Bitwise',
  linkName: 'bit-set',
  returns: {
    type: 'integer',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Sets bit number $n.',
  examples: [
    '0b0010 bit-set 1',
    'bit-set(0b0011, 1)',
    'bit-set(0b1100, 1)',
  ],
  algebraic: true,
}, 'bit-test': {
  title: 'bit-test',
  category: 'Bitwise',
  linkName: 'bit-test',
  returns: {
    type: 'boolean',
  },
  args: {
    ...getOperatorArgs('integer', 'integer'),
    x: {
      type: 'integer',
    },
    n: {
      type: 'integer',
    },
  },
  variants: [
    { argumentNames: ['x', 'n'] },
  ],
  description: 'Checks if bit number $n is set.',
  examples: [
    '0b0011 bit-test 1',
    'bit-test(0b0011, 1)',
    'bit-test(0b1100, 1)',
  ],
  algebraic: true,
} }
