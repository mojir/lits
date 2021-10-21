module.exports = {
  'bit-shift-left': {
    name: `bit-shift-left`,
    category: `Bitwise`,
    linkName: `bit-shift-left`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `count`,
        type: `non negative integer`,
      },
    ],
    description: `Shifts \`number\` arithmetically left by \`count\` bit positions if \`count\`.`,
    examples: [`(bit-shift-left 1 10)`, `(bit-shift-left -4 2)`],
    specialExpression: false,
  },
  'bit-shift-right': {
    name: `bit-shift-right`,
    category: `Bitwise`,
    linkName: `bit-shift-right`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `count`,
        type: `non negative integer`,
      },
    ],
    description: `Shifts \`number\` arithmetically right by \`count\` bit positions if \`count\`.`,
    examples: [`(bit-shift-right 2048 10)`, `(bit-shift-right 4 10)`],
    specialExpression: false,
  },
  'bit-not': {
    name: `bit-not`,
    category: `Bitwise`,
    linkName: `bit-not`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
    ],
    description: `Returns bitwise \`not\` of \`number\`.`,
    examples: [`(bit-not 0)`, `(bit-not 255)`],
    specialExpression: false,
  },
  'bit-and': {
    name: `bit-and`,
    category: `Bitwise`,
    linkName: `bit-and`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
        description: `two or more`,
      },
    ],
    description: `Returns bitwise \`and\` of all arguments.`,
    examples: [`(bit-and 0b0011 0b0110)`, `(bit-and 0b0011 0b0110 0b1001)`],
    specialExpression: false,
  },
  'bit-and-not': {
    name: `bit-and-not`,
    category: `Bitwise`,
    linkName: `bit-and-not`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
        description: `two or more`,
      },
    ],
    description: `Returns bitwise \`and\` with complement of all arguments. (bit-and-not x y) <==> (bit-and x (bit-not y)).`,
    examples: [`(bit-and-not 0b0011 0b0110)`, `(bit-and-not 0b0011 0b0110 0b1001)`],
    specialExpression: false,
  },
  'bit-or': {
    name: `bit-or`,
    category: `Bitwise`,
    linkName: `bit-or`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
        description: `two or more`,
      },
    ],
    description: `Returns bitwise \`or\` of all arguments. Return 0 if no arguments.`,
    examples: [`(bit-or 0b0011 0b0110)`, `(bit-or 0b1000 0b0100 0b0010)`],
    specialExpression: false,
  },
  'bit-xor': {
    name: `bit-xor`,
    category: `Bitwise`,
    linkName: `bit-xor`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
        description: `two or more`,
      },
    ],
    description: `Returns bitwise \`xor\` of all arguments.`,
    examples: [`(bit-xor 0b0011 0b0110)`, `(bit-xor 0b11110000 0b00111100 0b10101010)`],
    specialExpression: false,
  },
  'bit-flip': {
    name: `bit-flip`,
    category: `Bitwise`,
    linkName: `bit-flip`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `index`,
        type: `non negative integer`,
      },
    ],
    description: `Flips bit number \`index\`.`,
    examples: [`(bit-flip 0b0011 1)`, `(bit-flip 0b1100 1)`],
    specialExpression: false,
  },
  'bit-clear': {
    name: `bit-clear`,
    category: `Bitwise`,
    linkName: `bit-clear`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `index`,
        type: `non negative integer`,
      },
    ],
    description: `Clears bit number \`index\`.`,
    examples: [`(bit-clear 0b0011 1)`, `(bit-clear 0b1100 1)`],
    specialExpression: false,
  },
  'bit-set': {
    name: `bit-set`,
    category: `Bitwise`,
    linkName: `bit-set`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `index`,
        type: `non negative integer`,
      },
    ],
    description: `Sets bit number \`index\`.`,
    examples: [`(bit-set 0b0011 1)`, `(bit-set 0b1100 1)`],
    specialExpression: false,
  },
  'bit-test': {
    name: `bit-test`,
    category: `Bitwise`,
    linkName: `bit-test`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `integer`,
      },
      {
        name: `index`,
        type: `non negative integer`,
      },
    ],
    description: `Checks if bit number \`index\` is set.`,
    examples: [`(bit-test 0b0011 1)`, `(bit-test 0b1100 1)`],
    specialExpression: false,
  },
}
