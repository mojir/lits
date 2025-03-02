import type { AssertApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

export const assertReference: Record<AssertApiName, FunctionReference<'Assert'>> = { 'assert': {
  title: 'assert',
  category: 'Assert',
  linkName: 'assert',
  returns: {
    type: 'any',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is falsy it throws `AssertionError` with $message. If no $message is provided, message is set to $value.',
  examples: ['(assert 0 "Expected a positive value")'],
}, 'assert=': {
  title: 'assert=',
  category: 'Assert',
  linkName: 'assert-equal',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is not the same as $b it throws `AssertionError`.',
  examples: [
    '(assert= 0 1 "Expected same values")',
    '(assert= 0 1)',
    '(assert= 1 1)',
  ],
}, 'assert!=': {
  title: 'assert!=',
  category: 'Assert',
  linkName: 'assert-exclamation-equal',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is the same as $b it throws `AssertionError`.',
  examples: [
    '(assert!= 0 0 "Expected different values")',
    '(assert!= 0 0)',
    '(assert!= 0 1)',
  ],
}, 'assert_equal': {
  title: 'assert_equal',
  category: 'Assert',
  linkName: 'assert_equal',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is not deep equal to $b it throws `AssertionError`.',
  examples: [
    '(assert_equal { :a 1 } { :a 2 } "Expected equal values")',
    '(assert_equal { :a 1 } { :a 2 })',
    '(assert_equal { :a 1 } { :a 1 })',
  ],
}, 'assert_not_equal': {
  title: 'assert_not_equal',
  category: 'Assert',
  linkName: 'assert_not_equal',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is not deep equal to $b it throws `AssertionError`.',
  examples: [
    '(assert_not_equal { :a 2 } { :a 2 } "Expected different values")',
    '(assert_not_equal { :a 2 } { :a 2 })',
    '(assert_not_equal { :a 1 } { :a 2 })',
  ],
}, 'assert_gt': {
  title: 'assert_gt',
  category: 'Assert',
  linkName: 'assert_gt',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is not greater than $b it throws `AssertionError`.',
  examples: [
    '(assert_gt 0 1 "Expected greater value")',
    '(assert_gt 0 0)',
    '(assert_gt 1 0)',
  ],
}, 'assert_lt': {
  title: 'assert_lt',
  category: 'Assert',
  linkName: 'assert_lt',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is not less than $b it throws `AssertionError`.',
  examples: [
    '(assert_lte 1 0 "Expected smaller value value")',
    '(assert_lte 1 1)',
    '(assert_lte 0 1)',
  ],
}, 'assert_gte': {
  title: 'assert_gte',
  category: 'Assert',
  linkName: 'assert_gte',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is less than $b it throws `AssertionError`.',
  examples: [
    '(assert_gte 0 1 "Expected greater value")',
    '(assert_gte 0 1)',
    '(assert_gte 1 1)',
  ],
}, 'assert_lte': {
  title: 'assert_lte',
  category: 'Assert',
  linkName: 'assert_lte',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    a: {
      type: 'any',
    },
    b: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'message'] },
  ],
  description: 'If $a is grater than $b it throws `AssertionError`.',
  examples: [
    '(assert_lte 1 0 "Expected smaller value value")',
    '(assert_lte 1 0)',
    '(assert_lte 1 1)',
  ],
}, 'assert_true': {
  title: 'assert_true',
  category: 'Assert',
  linkName: 'assert_true',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is not `true` it throws `AssertionError`.',
  examples: [
    '(assert_true false "Expected true")',
    '(assert_true false)',
    '(assert_true true)',
  ],
}, 'assert_false': {
  title: 'assert_false',
  category: 'Assert',
  linkName: 'assert_false',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is not `false` it throws `AssertionError`.',
  examples: [
    '(assert_false true "Expected false")',
    '(assert_false true)',
    '(assert_false false)',
  ],
}, 'assert_truthy': {
  title: 'assert_truthy',
  category: 'Assert',
  linkName: 'assert_truthy',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is not `truthy` it throws `AssertionError`.',
  examples: [
    '(assert_truthy false "Expected truthy")',
    '(assert_truthy false)',
    '(assert_truthy 0)',
    '(assert_truthy nil)',
    '(assert_truthy "")',
    '(assert_truthy true)',
    '(assert_truthy 1)',
    '(assert_truthy :x)',
    '(assert_truthy [])',
    '(assert_truthy {})',
  ],
}, 'assert_falsy': {
  title: 'assert_falsy',
  category: 'Assert',
  linkName: 'assert_falsy',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is not `falsy` it throws `AssertionError`.',
  examples: [
    '(assert_falsy true "Expected falsy")',
    '(assert_falsy :x)',
    '(assert_falsy [])',
    '(assert_falsy {})',
    '(assert_falsy 1)',
    '(assert_falsy false)',
    '(assert_falsy 0)',
    '(assert_falsy nil)',
    '(assert_falsy "")',
  ],
}, 'assert_null': {
  title: 'assert_null',
  category: 'Assert',
  linkName: 'assert_null',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    value: {
      type: 'any',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value'] },
    { argumentNames: ['value', 'message'] },
  ],
  description: 'If $value is not `nil` it throws `AssertionError`.',
  examples: [
    '(assert_null nil)',
    '(assert_null true "Expected nil")',
    '(assert_null :x)',
    '(assert_null [])',
    '(assert_null {})',
    '(assert_null 1)',
    '(assert_null false)',
    '(assert_null 0)',
    '(assert_null "")',
  ],
}, 'assert_throws': {
  title: 'assert_throws',
  category: 'Assert',
  linkName: 'assert_throws',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    fn: {
      type: 'function',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['fn'] },
    { argumentNames: ['fn', 'message'] },
  ],
  description: 'If $fn does not throw, it throws `AssertionError`.',
  examples: ['(assert_throws #(throw "Error"))', '(assert_throws #(identity "Error"))'],
}, 'assert_throws_error': {
  title: 'assert_throws_error',
  category: 'Assert',
  linkName: 'assert_throws_error',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    'fn': {
      type: 'function',
    },
    'error-message': {
      type: 'string',
    },
    'message': {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['value', 'error-message'] },
    { argumentNames: ['value', 'error-message', 'message'] },
  ],
  description: 'If $fn does not throw $error-message, it throws `AssertionError`.',
  examples: [
    '(assert_throws_error #(throw :Error) :Error)',
    '(assert_throws_error #(throw "Something else") :Error "Hej alla barn")',
    '(assert_throws_error #(identity :Error) :Error)',
  ],
}, 'assert_not_throws': {
  title: 'assert_not_throws',
  category: 'Assert',
  linkName: 'assert_not_throws',
  clojureDocs: null,
  returns: {
    type: 'null',
  },
  args: {
    fn: {
      type: 'function',
    },
    message: {
      type: 'string',
    },
  },
  variants: [
    { argumentNames: ['fn'] },
    { argumentNames: ['fn', 'message'] },
  ],
  description: 'If $fn throws, it throws `AssertionError`.',
  examples: ['(assert_not_throws #(identity "Error"))', '(assert_not_throws #(throw "Error"))'],
} }
