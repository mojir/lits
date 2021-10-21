module.exports = {
  array: {
    name: `array`,
    category: `Array`,
    linkName: `array`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `values`,
        type: `array`,
      },
    ],
    description: `Makes new array from \`values\`.`,
    examples: [
      `(array 1 2 3)`,
      `(array (array null false true))`,
      `[]`,
      `[1 2 3]`,
      `[[null false true]]`,
      `[]`,
      `([1 2 3] 1)`,
      `([1 2 3 4 5 6 7 8 9] 3)`,
    ],
    specialExpression: false,
  },
  range: {
    name: `range`,
    category: `Array`,
    linkName: `range`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `a`,
        type: `number`,
      },
      {
        name: `length`,
        type: `number`,
        description: `optional`,
      },
      {
        name: `length`,
        type: `number`,
        description: `optional`,
      },
    ],
    description: `Create an array with a range of numbers. If only one argument: \`0...a\`, otherwise: \`a...b\`. \`step\` defaults to \`1\`.`,
    examples: [`(range 4)`, `(range 1 4)`, `(range 0.4 4.9)`, `(range 0.25 1 0.25)`],
    specialExpression: false,
  },
  repeat: {
    name: `repeat`,
    category: `Array`,
    linkName: `repeat`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `count`,
        type: `non negative integer`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns an array with \`value\` repeated \`count\` times.`,
    examples: [`(repeat 3 10)`, `(repeat 0 10)`, `(repeat 5 "Albert")`],
    specialExpression: false,
  },
  flatten: {
    name: `flatten`,
    category: `Array`,
    linkName: `flatten`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `input`,
        type: `Array`,
      },
    ],
    description: `Takes a nested array and return a flat array. If \`input\` isn't an array, an empty array is returned.`,
    examples: [`(flatten [1 2 [3 4] 5])`, `(flatten [1 2 [3 [4 [5]]] 6])`, `(flatten 12)`],
    specialExpression: false,
  },
  mapcat: {
    name: `mapcat`,
    category: `Array`,
    linkName: `mapcat`,
    returns: {
      type: `Array`,
    },
    arguments: [
      {
        name: `mapper`,
        type: `function`,
      },
      {
        name: `arrays`,
        type: `Array`,
        description: `one or many`,
      },
    ],
    description: `Returns the result of applying concat to the result of applying map to \`mapper\` and \`arrays\`.`,
    examples: [
      `(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])`,
      `(mapcat reverse [[3 2 1 0] [6 [5] 4] [9 8 7]])`,
      `(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat foo [1 2 3])`,
      `(mapcat #(remove even? %1) [[1 2] [2 2] [2 3]])`,
    ],
    specialExpression: false,
  },
}
