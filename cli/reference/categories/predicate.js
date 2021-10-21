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
  'nil?': {
    name: `nil?`,
    category: `Predicate`,
    linkName: `nil_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is \`nil\`, otherwise \`false\`.`,
    examples: [`(nil? nil)`, `(nil? false)`, `(nil? [1 2 3])`, `(nil? 0)`, `(nil? "A string")`],
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
  'coll?': {
    name: `coll?`,
    category: `Predicate`,
    linkName: `coll_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a Coll i.e. an array, an object or a string, otherwise \`false\`.`,
    examples: [
      `(coll? [])`,
      `(coll? [1 2 3])`,
      `(coll? (object "a" 10))`,
      `(coll? "Albert")`,
      `(coll? 42)`,
      `(coll? 10.1)`,
      `(coll? (fn [x y] (+ x y)))`,
    ],
    specialExpression: false,
  },
  'seq?': {
    name: `seq?`,
    category: `Predicate`,
    linkName: `seq_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is a Seq i.e. an array or a string, otherwise \`false\`.`,
    examples: [
      `(seq? [])`,
      `(seq? [1 2 3])`,
      `(seq? (object "a" 10))`,
      `(seq? "Albert")`,
      `(seq? 42)`,
      `(seq? 10.1)`,
      `(seq? (fn [x y] (+ x y)))`,
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
  'false?': {
    name: `false?`,
    category: `Predicate`,
    linkName: `false_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is \`true\`, otherwise \`false\`.`,
    examples: [`(false? false)`, `(false? true)`, `(false? 1)`, `(false? 0)`],
    specialExpression: false,
  },
  'true?': {
    name: `true?`,
    category: `Predicate`,
    linkName: `true_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`true\` if \`value\` is \`true\`, otherwise \`false\`.`,
    examples: [`(true? false)`, `(true? true)`, `(true? 1)`, `(true? 0)`],
    specialExpression: false,
  },
}
