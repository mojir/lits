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
  linkName: 'assert_equal',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
}, 'assert-not=': {
  title: 'assert-not=',
  category: 'Assert',
  linkName: 'assert-not_equal',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-not= 0 0 "Expected different values")',
    '(assert-not= 0 0)',
    '(assert-not= 0 1)',
  ],
}, 'assert-equal': {
  title: 'assert-equal',
  category: 'Assert',
  linkName: 'assert-equal',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-equal { :a 1 } { :a 2 } "Expected equal values")',
    '(assert-equal { :a 1 } { :a 2 })',
    '(assert-equal { :a 1 } { :a 1 })',
  ],
}, 'assert-not-equal': {
  title: 'assert-not-equal',
  category: 'Assert',
  linkName: 'assert-not-equal',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-not-equal { :a 2 } { :a 2 } "Expected different values")',
    '(assert-not-equal { :a 2 } { :a 2 })',
    '(assert-not-equal { :a 1 } { :a 2 })',
  ],
}, 'assert>': {
  title: 'assert>',
  category: 'Assert',
  linkName: 'assert_gt',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert> 0 1 "Expected greater value")',
    '(assert> 0 0)',
    '(assert> 1 0)',
  ],
}, 'assert<': {
  title: 'assert<',
  category: 'Assert',
  linkName: 'assert_lt',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert< 1 0 "Expected smaller value value")',
    '(assert< 1 1)',
    '(assert< 0 1)',
  ],
}, 'assert>=': {
  title: 'assert>=',
  category: 'Assert',
  linkName: 'assert_gte',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert>= 0 1 "Expected greater value")',
    '(assert>= 0 1)',
    '(assert>= 1 1)',
  ],
}, 'assert<=': {
  title: 'assert<=',
  category: 'Assert',
  linkName: 'assert_lte',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert<= 1 0 "Expected smaller value value")',
    '(assert<= 1 0)',
    '(assert<= 1 1)',
  ],
}, 'assert-true': {
  title: 'assert-true',
  category: 'Assert',
  linkName: 'assert-true',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-true false "Expected true")',
    '(assert-true false)',
    '(assert-true true)',
  ],
}, 'assert-false': {
  title: 'assert-false',
  category: 'Assert',
  linkName: 'assert-false',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-false true "Expected false")',
    '(assert-false true)',
    '(assert-false false)',
  ],
}, 'assert-truthy': {
  title: 'assert-truthy',
  category: 'Assert',
  linkName: 'assert-truthy',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-truthy false "Expected truthy")',
    '(assert-truthy false)',
    '(assert-truthy 0)',
    '(assert-truthy nil)',
    '(assert-truthy "")',
    '(assert-truthy true)',
    '(assert-truthy 1)',
    '(assert-truthy :x)',
    '(assert-truthy [])',
    '(assert-truthy {})',
  ],
}, 'assert-falsy': {
  title: 'assert-falsy',
  category: 'Assert',
  linkName: 'assert-falsy',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-falsy true "Expected falsy")',
    '(assert-falsy :x)',
    '(assert-falsy [])',
    '(assert-falsy {})',
    '(assert-falsy 1)',
    '(assert-falsy false)',
    '(assert-falsy 0)',
    '(assert-falsy nil)',
    '(assert-falsy "")',
  ],
}, 'assert-nil': {
  title: 'assert-nil',
  category: 'Assert',
  linkName: 'assert-nil',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-nil nil)',
    '(assert-nil true "Expected nil")',
    '(assert-nil :x)',
    '(assert-nil [])',
    '(assert-nil {})',
    '(assert-nil 1)',
    '(assert-nil false)',
    '(assert-nil 0)',
    '(assert-nil "")',
  ],
}, 'assert-throws': {
  title: 'assert-throws',
  category: 'Assert',
  linkName: 'assert-throws',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
  examples: ['(assert-throws #(throw "Error"))', '(assert-throws #(identity "Error"))'],
}, 'assert-throws-error': {
  title: 'assert-throws-error',
  category: 'Assert',
  linkName: 'assert-throws-error',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
    '(assert-throws-error #(throw :Error) :Error)',
    '(assert-throws-error #(throw "Something else") :Error "Hej alla barn")',
    '(assert-throws-error #(identity :Error) :Error)',
  ],
}, 'assert-not-throws': {
  title: 'assert-not-throws',
  category: 'Assert',
  linkName: 'assert-not-throws',
  clojureDocs: null,
  returns: {
    type: 'nil',
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
  examples: ['(assert-not-throws #(identity "Error"))', '(assert-not-throws #(throw "Error"))'],
} }
