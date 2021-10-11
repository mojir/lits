module.exports = {
  'boolean?': {
    name: `boolean?`,
    category: `Predicate`,
    linkName: `boolean_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a \`boolean\`, otherwise \`false\`.`,
    examples: [`(boolean? true)`, `(boolean? false)`, `(boolean? [1 2 3])`, `(boolean? 0)`, `(boolean? "A string")`],
    specialExpression: false,
  },
  'null?': {
    name: `null?`,
    category: `Predicate`,
    linkName: `null_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is \`null\`, otherwise \`false\`.`,
    examples: [`(null? null)`, `(null? false)`, `(null? [1 2 3])`, `(null? 0)`, `(null? "A string")`],
    specialExpression: false,
  },
  'undefined?': {
    name: `undefined?`,
    category: `Predicate`,
    linkName: `undefined_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is \`undefined\`, otherwise \`false\`.`,
    examples: [
      `(undefined? undefined)`,
      `(undefined? false)`,
      `(undefined? null)`,
      `(undefined? [1 2 3])`,
      `(undefined? 0)`,
      `(undefined? "A string")`,
    ],
    specialExpression: false,
  },
  'number?': {
    name: `number?`,
    category: `Predicate`,
    linkName: `number_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a number, otherwise \`false\`.`,
    examples: [
      `(number? 0)`,
      `(number? 2)`,
      `(number? -0.12)`,
      `(number? false)`,
      `(number? [1 2 3])`,
      `(number? "A string")`,
    ],
    specialExpression: false,
  },
  'string?': {
    name: `string?`,
    category: `Predicate`,
    linkName: `string_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a string, otherwise \`false\`.`,
    examples: [
      `(string? "")`,
      `(string? "A string")`,
      `(string? (if true "A string" false))`,
      `(string? false)`,
      `(string? [1 2 3])`,
      `(string? 100)`,
    ],
    specialExpression: false,
  },
  'function?': {
    name: `function?`,
    category: `Predicate`,
    linkName: `function_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a function, otherwise \`false\`.`,
    examples: [
      `(function? +)`,
      `(function? /)`,
      `(function? (fn [x y] (+ x y)))`,
      `(function? false)`,
      `(function? "false")`,
      `(function? [1 2 3])`,
    ],
    specialExpression: false,
  },
  'integer?': {
    name: `integer?`,
    category: `Predicate`,
    linkName: `integer_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is an integer, otherwise \`false\`.`,
    examples: [
      `(integer? 0)`,
      `(integer? -12)`,
      `(integer? 42)`,
      `(integer? 10.1)`,
      `(integer? (fn [x y] (+ x y)))`,
      `(integer? false)`,
      `(integer? "false")`,
      `(integer? [1 2 3])`,
    ],
    specialExpression: false,
  },
  'array?': {
    name: `array?`,
    category: `Predicate`,
    linkName: `array_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is an array, otherwise \`false\`.`,
    examples: [
      `(array? [])`,
      `(array? [1 2 3])`,
      `(array? (object "a" 10))`,
      `(array? 42)`,
      `(array? 10.1)`,
      `(array? (fn [x y] (+ x y)))`,
    ],
    specialExpression: false,
  },
  'object?': {
    name: `object?`,
    category: `Predicate`,
    linkName: `object_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is an object, otherwise \`false\`.`,
    examples: [
      `(object? (object "a" 10))`,
      `(object? (object))`,
      `(object? 42)`,
      `(object? 10.1)`,
      `(object? (fn [x y] (+ x y)))`,
      `(object? (regexp "^start"))`,
      `(object? "false")`,
      `(object? [1 2 3])`,
    ],
    specialExpression: false,
  },
  'collection?': {
    name: `collection?`,
    category: `Predicate`,
    linkName: `collection_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a collection i.e. an array or an object, otherwise \`false\`.`,
    examples: [
      `(collection? [])`,
      `(collection? [1 2 3])`,
      `(collection? (object "a" 10))`,
      `(collection? 42)`,
      `(collection? 10.1)`,
      `(collection? (fn [x y] (+ x y)))`,
    ],
    specialExpression: false,
  },
  'regexp?': {
    name: `regexp?`,
    category: `Predicate`,
    linkName: `regexp_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a regexp, otherwise \`false\`.`,
    examples: [
      `(regexp? (regexp "^start"))`,
      `(regexp? -12)`,
      `(regexp? (object))`,
      `(regexp? 10.1)`,
      `(regexp? (fn [x y] (+ x y)))`,
      `(regexp? false)`,
      `(regexp? "false")`,
      `(regexp? [1 2 3])`,
    ],
    specialExpression: false,
  },
  'zero?': {
    name: `zero?`,
    category: `Predicate`,
    linkName: `zero_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is \`0\`, otherwise \`false\`.`,
    examples: [`(zero? 0)`, `(zero? -0.0)`, `(zero? 1)`, `(zero? 0.1)`],
    specialExpression: false,
  },
  'pos?': {
    name: `pos?`,
    category: `Predicate`,
    linkName: `pos_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is greater than \`0\`, otherwise \`false\`.`,
    examples: [`(pos? 0)`, `(pos? -0.0)`, `(pos? 1)`, `(pos? -0.1)`],
    specialExpression: false,
  },
  'neg?': {
    name: `neg?`,
    category: `Predicate`,
    linkName: `neg_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is less than \`0\`, otherwise \`false\`.`,
    examples: [`(neg? 0)`, `(neg? -0.0)`, `(neg? 1)`, `(neg? -0.1)`],
    specialExpression: false,
  },
  'even?': {
    name: `even?`,
    category: `Predicate`,
    linkName: `even_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is even, otherwise \`false\`.`,
    examples: [`(even? 0)`, `(even? -0.0)`, `(even? -1)`, `(even? 2.1)`],
    specialExpression: false,
  },
  'odd?': {
    name: `odd?`,
    category: `Predicate`,
    linkName: `odd_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is odd, otherwise \`false\`.`,
    examples: [`(odd? 1.0)`, `(odd? 1.001)`, `(odd? -1)`, `(odd? 2.1)`],
    specialExpression: false,
  },
  'finite?': {
    name: `finite?`,
    category: `Predicate`,
    linkName: `finite_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is finite, otherwise \`false\`.`,
    examples: [`(finite? 1.0)`, `(finite? (/ 1 0))`, `(finite? (/ -1 0))`, `(finite? (sqrt -1))`],
    specialExpression: false,
  },
  'nan?': {
    name: `nan?`,
    category: `Predicate`,
    linkName: `nan_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` is NaN (not a number), otherwise \`false\`.`,
    examples: [`(nan? 1.0)`, `(nan? (/ 1 0))`, `(nan? (/ -1 0))`, `(nan? (sqrt -1))`],
    specialExpression: false,
  },
  'negative-infinity?': {
    name: `negative-infinity?`,
    category: `Predicate`,
    linkName: `negative-infinity_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` equals negative infinity, otherwise \`false\`.`,
    examples: [
      `(negative-infinity? 1.0)`,
      `(negative-infinity? (/ 1 0))`,
      `(negative-infinity? (/ -1 0))`,
      `(negative-infinity? (sqrt -1))`,
    ],
    specialExpression: false,
  },
  'positive-infinity?': {
    name: `positive-infinity?`,
    category: `Predicate`,
    linkName: `positive-infinity_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`true\` if \`number\` equals positive infinity, otherwise \`false\`.`,
    examples: [
      `(positive-infinity? 1.0)`,
      `(positive-infinity? (/ 1 0))`,
      `(positive-infinity? (/ -1 0))`,
      `(positive-infinity? (sqrt -1))`,
    ],
    specialExpression: false,
  },
}
