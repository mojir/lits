import type { BitwiseApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

export const bitwiseReference: Record<BitwiseApiName, FunctionReference<'Bitwise'>> = { '<<': {
  title: '<<',
  category: 'Bitwise',
  linkName: '-lt-lt',
  clojureDocs: 'bit-shift-left',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(<< 1 10)', '(<< -4 2)'],
}, '>>': {
  title: '>>',
  category: 'Bitwise',
  linkName: '-gt-gt',
  clojureDocs: 'bit-shift-right',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(>> 2048 10)', '(>> 4 10)'],
}, '>>>': {
  title: '>>>',
  category: 'Bitwise',
  linkName: '-gt-gt-gt',
  clojureDocs: 'unsigned-bit-shift-right',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(>>> 2048 10)', '(>>> 4 10)', '(>>> -1 10)'],
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
  examples: ['(~ 0)', '(~ 255)'],
}, '&': {
  title: '&',
  category: 'Bitwise',
  linkName: '-and',
  clojureDocs: 'bit-and',
  returns: {
    type: 'integer',
  },
  args: {
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
    '(& 0b0011 0b0110)',
    '(& 0b0011 0b0110 0b1001)',
  ],
}, '&!': {
  title: '&!',
  category: 'Bitwise',
  linkName: '-and-exclamation',
  clojureDocs: 'bit-and-not',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(&! 0b0011 0b0110)', '(&! 0b0011 0b0110 0b1001)'],
}, '|': {
  title: '|',
  category: 'Bitwise',
  linkName: '-or',
  clojureDocs: 'bit-or',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(| 0b0011 0b0110)', '(| 0b1000 0b0100 0b0010)'],
}, '^': {
  title: '^',
  category: 'Bitwise',
  linkName: '-caret',
  clojureDocs: 'bit-xor',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(^ 0b0011 0b0110)', '(^ 0b11110000 0b00111100 0b10101010)'],
}, 'bit_flip': {
  title: 'bit_flip',
  category: 'Bitwise',
  linkName: 'bit_flip',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(bit_flip 0b0011 1)', '(bit_flip 0b1100 1)'],
}, 'bit_clear': {
  title: 'bit_clear',
  category: 'Bitwise',
  linkName: 'bit_clear',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(bit_clear 0b0011 1)', '(bit_clear 0b1100 1)'],
}, 'bit_set': {
  title: 'bit_set',
  category: 'Bitwise',
  linkName: 'bit_set',
  returns: {
    type: 'integer',
  },
  args: {
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
  examples: ['(bit_set 0b0011 1)', '(bit_set 0b1100 1)'],
}, 'bit_test': {
  title: 'bit_test',
  category: 'Bitwise',
  linkName: 'bit_test',
  returns: {
    type: 'boolean',
  },
  args: {
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
  examples: ['(bit_test 0b0011 1)', '(bit_test 0b1100 1)'],
} }
