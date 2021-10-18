module.exports = {
  'not=': {
    name: `not=`,
    category: `Misc`,
    linkName: `not_equal`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `array`,
      },
    ],
    description: `Result is \`true\` if no two \`values\` are equal to each other, otherwise result is \`false\`. Note that only two argument version result is negation of \`=\` function, that is \`(not= a b)\` is same as \`(not (= a b))\`.`,
    examples: [`(not= 3)`, `(not= 3 2)`, `(not= "3" 3)`, `(not= 3 3 2)`, `(not= "3" "2" "1" "0")`, `(not= 0 -0)`],
    specialExpression: false,
  },
  '=': {
    name: `=`,
    category: `Misc`,
    linkName: `_equal`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `array`,
      },
    ],
    description: `Compares \`values\` according to "equal" predicate. Result is \`true\` if every specified value is equal to each other, otherwise result is \`false\`.`,
    examples: [`(= 1 1)`, `(= 1.01 1)`, `(= "1" 1)`, `(= "2" "2" "2" "2")`, `(= 2 2 1 2)`],
    specialExpression: false,
  },
  '<': {
    name: `<`,
    category: `Misc`,
    linkName: `_lt`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `any`,
        description: `one or many`,
      },
    ],
    description: `Compares \`values\` according to "less than" predicate. Each (overlapping) pair of the \`values\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(< 0 1)`, `(< 1 1.01)`, `(< 1 1)`, `(< 1 2 2 3)`, `(< "a" "b")`, `(< [9] [1 2])`],
    specialExpression: false,
  },
  '>': {
    name: `>`,
    category: `Misc`,
    linkName: `_gt`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `any`,
        description: `one or many`,
      },
    ],
    description: `Compares \`values\` according to "greater than" predicate. Each (overlapping) pair of the \`values\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(> 1 0)`, `(> 1.01 1)`, `(> 1 1)`, `(> 4 3 2 1)`, `(> 3 2 2 1)`],
    specialExpression: false,
  },
  '<=': {
    name: `<=`,
    category: `Misc`,
    linkName: `_lte`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `any`,
        description: `one or many`,
      },
    ],
    description: `Compares \`values\` according to "less than or equal" predicate. Each (overlapping) pair of the \`values\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(<= 0 1)`, `(<= 1 1.01)`, `(<= 1 1)`, `(<= 1 2 3 4)`, `(<= 1 2 2 3)`],
    specialExpression: false,
  },
  '>=': {
    name: `>=`,
    category: `Misc`,
    linkName: `_gte`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `values`,
        type: `any`,
        description: `one or many`,
      },
    ],
    description: `Compares \`values\` according to "greater than or equal" predicate. Each (overlapping) pair of the \`values\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(>= 1 0)`, `(>= 1.01 1)`, `(>= 1 1)`, `(>= 4 3 2 1)`, `(>= 3 2 2 1)`],
    specialExpression: false,
  },
  not: {
    name: `not`,
    category: `Misc`,
    linkName: `not`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Computes logical negation. Note that any other \`value\` than \`false\`, \`0\`, \`null\`, \`undefined\` and \`""\` is considered as \`true\`.`,
    examples: [
      `(not 3)`,
      `(not true)`,
      `(not "A string")`,
      `(not 0)`,
      `(not false)`,
      `(not null)`,
      `(not undefined)`,
      `(not "")`,
    ],
    specialExpression: false,
  },
  'write!': {
    name: `write!`,
    category: `Misc`,
    linkName: `write_exclamation`,
    returns: {
      type: `value`,
    },
    arguments: [
      {
        name: `values`,
        type: `array`,
      },
    ],
    description: `It console.log the \`values\` and then returns the last element of the \`values\` array.. If called with no arguments \`undefined\` is returned.`,
    examples: [
      `(write! "A string")`,
      `(write! 100 "items")`,
      `(write! (object "a" 10))`,
      `(write! ["a" "b" "c"])`,
      `(write! (regexp "^start"))`,
      `(write! null undefined true false)`,
    ],
    specialExpression: false,
  },
  'inst-ms': {
    name: `inst-ms`,
    category: `Misc`,
    linkName: `inst-ms`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns milliseconds elapsed since the UNIX epoch.`,
    examples: [`(inst-ms)`],
    specialExpression: false,
  },
  'get-path': {
    name: `get-path`,
    category: `Misc`,
    linkName: `get-path`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `object`,
        type: `object | array`,
      },
      {
        name: `path`,
        type: `string`,
      },
    ],
    description: `Is used to get the value at \`path\` of object or array.`,
    examples: [
      `(get-path (write! (object "a" (object "x" [1 2 3]))) "a.x[2]")`,
      `(get-path (write! (object "a" (object "x" [1 2 3]))) "b.z[10]")`,
      `(get-path (write! [(object "x" [1 2 3])]) "[1].x[2]")`,
    ],
    specialExpression: false,
  },
  'debug!': {
    name: `debug!`,
    category: `Misc`,
    linkName: `debug_exclamation`,
    returns: {
      type: `undefined`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `If no params, prints context stack, otherwise prints \`value\` details.`,
    examples: [`(debug!), (debug! #(> %1 2))`],
    specialExpression: false,
  },
  boolean: {
    name: `boolean`,
    category: `Misc`,
    linkName: `boolean`,
    returns: {
      type: `true | false`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Coerces \`value\` to boolean.`,
    examples: [`(boolean 0)`, `(boolean 1)`, `(boolean null)`, `(boolean "Albert")`],
    specialExpression: false,
  },
  compare: {
    name: `compare`,
    category: `Misc`,
    linkName: `compare`,
    returns: {
      type: `1 | -1 | 0`,
    },
    arguments: [
      {
        name: `a`,
        type: `any`,
      },
      {
        name: `b`,
        type: `any`,
      },
    ],
    description: `Compares two values. Returns -1 if a < b, 1 if a > b and 0 if a and b have the same sort order.`,
    examples: [
      `(compare 0 1)`,
      `(compare "Albert" "Mojir")`,
      `(compare 1 "1")`,
      `(compare [1 2 3] [2 3])`,
      `(compare [1 2 3] [2 3 4])`,
      `(compare {"a" 1 "b" 2} {"a" 1})`,
      `(compare {"a" 1} [2 3])`,
      `(compare + -)`,
    ],
    specialExpression: false,
  },
  assert: {
    name: `assert`,
    category: `Misc`,
    linkName: `assert`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
      {
        name: `message`,
        type: `string`,
        description: `optional`,
      },
    ],
    description: `If \`value\` is falsy it throws AssertionError with \`message\`. If no \`message\` is provided, message is set to \`value\`.`,
    examples: [`(assert 0 "Expected a positive value")`, `(assert false)`, `(assert 1)`],
    specialExpression: false,
  },
  'lispish-version': {
    name: `lispish-version`,
    category: `Misc`,
    linkName: `lispish-version`,
    returns: {
      type: `string`,
    },
    arguments: [],
    description: `Returns the lispish version.`,
    examples: [`(lispish-version)`],
    specialExpression: false,
  },
  'equal?': {
    name: `equal?`,
    category: `Misc`,
    linkName: `equal_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `a`,
        type: `any`,
      },
      {
        name: `b`,
        type: `any`,
      },
    ],
    description: `Returns true if \`a\` and \`b\` are structually equal.`,
    examples: [
      `(equal? {"a" 10 "b" 20} {"b" 20 "a" 10})`,
      `(equal? [1 true null undefined] [1 true null undefined])`,
      `(equal? {"a" 10 "b" [1 2 {"b" 20}]} {"b" [1 2 {"b" 20}] "a" 10})`,
      `(equal? {"a" 10 "b" [1 2 {"b" 20}]} {"b" [1 2 {"b" 21}] "a" 10})`,
    ],
    specialExpression: false,
  },
}
