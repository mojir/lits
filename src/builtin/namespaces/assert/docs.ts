import type { FunctionDocs } from '../../interface'

export const namespaceDocs: Record<string, FunctionDocs> = {
  'assert': {
    category: 'Assert',
    description: 'If $value is falsy it throws `AssertionError` with $message. If no $message is provided, message is set to $value.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert } = import("Assert");\ntry assert(0, "Expected a positive value") catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert!=': {
    category: 'Assert',
    description: 'If $a is the same as $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert!= } = import("Assert");\ntry assert!=(0, 0, "Expected different values") catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry assert!=(0, 0) catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry 0 assert!= 0 catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry assert!=(0, 1) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert=': {
    category: 'Assert',
    description: 'If $a is not structural equal to $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") catch (e) e.message end',
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 2 }) catch (e) e.message end',
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 1 }) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-gt': {
    category: 'Assert',
    description: 'If $a is not greater than $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-gt } = import("Assert");\ntry assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gt } = import("Assert");\ntry assert-gt(0, 0) catch (e) e.message end',
      'let { assert-gt } = import("Assert");\ntry assert-gt(1, 0) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-lt': {
    category: 'Assert',
    description: 'If $a is not less than $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-lt } = import("Assert");\ntry assert-lt(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lt } = import("Assert");\ntry assert-lt(1, 1) catch (e) e.message end',
      'let { assert-lt } = import("Assert");\ntry assert-lt(0, 1) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-gte': {
    category: 'Assert',
    description: 'If $a is less than $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-gte } = import("Assert");\ntry assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gte } = import("Assert");\ntry assert-gte(0, 1) catch (e) e.message end',
      'let { assert-gte } = import("Assert");\ntry assert-gte(1, 1) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-lte': {
    category: 'Assert',
    description: 'If $a is grater than $b it throws `AssertionError`.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
      {
        argumentNames: [
          'a',
          'b',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 0) catch (e) e.message end',
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 1) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-true': {
    category: 'Assert',
    description: 'If $value is not `true` it throws `AssertionError`.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-true } = import("Assert");\ntry assert-true(false, "Expected true") catch (e) e.message end',
      'let { assert-true } = import("Assert");\ntry assert-true(false) catch (e) e.message end',
      'let { assert-true } = import("Assert");\ntry assert-true(true) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-false': {
    category: 'Assert',
    description: 'If $value is not `false` it throws `AssertionError`.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-false } = import("Assert");\ntry assert-false(true, "Expected false") catch (e) e.message end',
      'let { assert-false } = import("Assert");\ntry assert-false(true) catch (e) e.message end',
      'let { assert-false } = import("Assert");\ntry assert-false(false) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-truthy': {
    category: 'Assert',
    description: 'If $value is not `truthy` it throws `AssertionError`.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(false, "Expected truthy") catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(false) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(0) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(null) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy("") catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(true) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(1) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy("x") catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy([]) catch (e) e.message end',
      'let { assert-truthy } = import("Assert");\ntry assert-truthy(nd) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-falsy': {
    category: 'Assert',
    description: 'If $value is not `falsy` it throws `AssertionError`.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(true, "Expected falsy") catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy("x") catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy([]) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(nd) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(1) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(false) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(0) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy(null) catch (e) e.message end',
      'let { assert-falsy } = import("Assert");\ntry assert-falsy("") catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-null': {
    category: 'Assert',
    description: 'If $value is not `null` it throws `AssertionError`.',
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
      {
        argumentNames: [
          'value',
        ],
      },
      {
        argumentNames: [
          'value',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-null } = import("Assert");\ntry assert-null(null) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null(true, "Expected null") catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null("x") catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null([]) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null(nd) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null(1) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null(false) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null(0) catch (e) e.message end',
      'let { assert-null } = import("Assert");\ntry assert-null("") catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-throws': {
    category: 'Assert',
    description: 'If $fun does not throw, it throws `AssertionError`.',
    returns: {
      type: 'null',
    },
    args: {
      fun: {
        type: 'function',
      },
      message: {
        type: 'string',
      },
    },
    variants: [
      {
        argumentNames: [
          'fun',
        ],
      },
      {
        argumentNames: [
          'fun',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-throws } = import("Assert");\nassert-throws(-> throw("Error"))',
      'let { assert-throws } = import("Assert");\ntry assert-throws(-> identity("Error")) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-throws-error': {
    category: 'Assert',
    description: 'If $fun does not throw $error-message, it throws `AssertionError`.',
    returns: {
      type: 'null',
    },
    args: {
      'fun': {
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
      {
        argumentNames: [
          'value',
          'error-message',
        ],
      },
      {
        argumentNames: [
          'value',
          'error-message',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-throws-error } = import("Assert");\ntry assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
      'let { assert-throws-error } = import("Assert");\ntry assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
  'assert-not-throws': {
    category: 'Assert',
    description: 'If $fun throws, it throws `AssertionError`.',
    returns: {
      type: 'null',
    },
    args: {
      fun: {
        type: 'function',
      },
      message: {
        type: 'string',
      },
    },
    variants: [
      {
        argumentNames: [
          'fun',
        ],
      },
      {
        argumentNames: [
          'fun',
          'message',
        ],
      },
    ],
    examples: [
      'let { assert-not-throws } = import("Assert");\ntry assert-not-throws(-> identity("Error")) catch (e) e.message end',
      'let { assert-not-throws } = import("Assert");\ntry assert-not-throws(-> throw("Error")) catch (e) e.message end',
    ],
    hideOperatorForm: true,
  },
}
