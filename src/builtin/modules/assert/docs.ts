import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
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
    seeAlso: ['Assert.assert-truthy', 'Assert.assert-true'],
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
    seeAlso: ['Assert.assert='],
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
    seeAlso: ['Assert.assert!='],
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
    seeAlso: ['Assert.assert-lt', 'Assert.assert-gte', 'Assert.assert-lte'],
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
    seeAlso: ['Assert.assert-gt', 'Assert.assert-lte', 'Assert.assert-gte'],
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
    seeAlso: ['Assert.assert-lte', 'Assert.assert-gt', 'Assert.assert-lt'],
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
    seeAlso: ['Assert.assert-gte', 'Assert.assert-lt', 'Assert.assert-gt'],
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
    seeAlso: ['Assert.assert-false', 'Assert.assert-truthy', 'Assert.assert-falsy', 'Assert.assert', 'Assert.assert-boolean'],
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
    seeAlso: ['Assert.assert-true', 'Assert.assert-falsy', 'Assert.assert-truthy', 'Assert.assert-boolean'],
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
    seeAlso: ['Assert.assert-falsy', 'Assert.assert-true', 'Assert.assert-false', 'Assert.assert', 'Assert.assert-null'],
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
    seeAlso: ['Assert.assert-truthy', 'Assert.assert-false', 'Assert.assert-true', 'Assert.assert-null'],
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
    seeAlso: ['Assert.assert-truthy', 'Assert.assert-falsy'],
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
    seeAlso: ['Assert.assert-throws-error', 'Assert.assert-not-throws'],
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
    seeAlso: ['Assert.assert-throws', 'Assert.assert-not-throws'],
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
    seeAlso: ['Assert.assert-throws', 'Assert.assert-throws-error'],
    hideOperatorForm: true,
  },
  'assert-array': {
    category: 'Assert',
    description: 'If $value is not an `array` it throws `AssertionError`.',
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
      'let { assert-array } = import("Assert");\ntry assert-array([1, 2, 3]) catch (e) e.message end',
      'let { assert-array } = import("Assert");\ntry assert-array("string") catch (e) e.message end',
      'let { assert-array } = import("Assert");\ntry assert-array(42, "Expected an array") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-object', 'Assert.assert-collection', 'Assert.assert-sequence'],
    hideOperatorForm: true,
  },
  'assert-boolean': {
    category: 'Assert',
    description: 'If $value is not a `boolean` it throws `AssertionError`.',
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
      'let { assert-boolean } = import("Assert");\ntry assert-boolean(true) catch (e) e.message end',
      'let { assert-boolean } = import("Assert");\ntry assert-boolean(false) catch (e) e.message end',
      'let { assert-boolean } = import("Assert");\ntry assert-boolean(1, "Expected a boolean") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-true', 'Assert.assert-false', 'Assert.assert-number', 'Assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-collection': {
    category: 'Assert',
    description: 'If $value is not a `collection` (array, object, or string) it throws `AssertionError`.',
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
      'let { assert-collection } = import("Assert");\ntry assert-collection([1, 2]) catch (e) e.message end',
      'let { assert-collection } = import("Assert");\ntry assert-collection({ a: 1 }) catch (e) e.message end',
      'let { assert-collection } = import("Assert");\ntry assert-collection("hello") catch (e) e.message end',
      'let { assert-collection } = import("Assert");\ntry assert-collection(42, "Expected a collection") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-sequence', 'Assert.assert-array', 'Assert.assert-object'],
    hideOperatorForm: true,
  },
  'assert-function': {
    category: 'Assert',
    description: 'If $value is not a `function` it throws `AssertionError`.',
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
      'let { assert-function } = import("Assert");\ntry assert-function(-> $ + 1) catch (e) e.message end',
      'let { assert-function } = import("Assert");\ntry assert-function(42, "Expected a function") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-number', 'Assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-grid': {
    category: 'Assert',
    description: 'If $value is not a `grid` it throws `AssertionError`.',
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
      'let { assert-grid } = import("Assert");\ntry assert-grid([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-grid } = import("Assert");\ntry assert-grid([1, 2], "Expected a grid") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-matrix', 'Assert.assert-vector'],
    hideOperatorForm: true,
  },
  'assert-integer': {
    category: 'Assert',
    description: 'If $value is not an `integer` it throws `AssertionError`.',
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
      'let { assert-integer } = import("Assert");\ntry assert-integer(42) catch (e) e.message end',
      'let { assert-integer } = import("Assert");\ntry assert-integer(3.14, "Expected an integer") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-number'],
    hideOperatorForm: true,
  },
  'assert-matrix': {
    category: 'Assert',
    description: 'If $value is not a `matrix` it throws `AssertionError`.',
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
      'let { assert-matrix } = import("Assert");\ntry assert-matrix([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-matrix } = import("Assert");\ntry assert-matrix([1, 2], "Expected a matrix") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-vector', 'Assert.assert-grid'],
    hideOperatorForm: true,
  },
  'assert-number': {
    category: 'Assert',
    description: 'If $value is not a `number` it throws `AssertionError`.',
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
      'let { assert-number } = import("Assert");\ntry assert-number(42) catch (e) e.message end',
      'let { assert-number } = import("Assert");\ntry assert-number("hello", "Expected a number") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-integer', 'Assert.assert-boolean', 'Assert.assert-string', 'Assert.assert-function'],
    hideOperatorForm: true,
  },
  'assert-object': {
    category: 'Assert',
    description: 'If $value is not an `object` it throws `AssertionError`.',
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
      'let { assert-object } = import("Assert");\ntry assert-object({ a: 1 }) catch (e) e.message end',
      'let { assert-object } = import("Assert");\ntry assert-object([1, 2], "Expected an object") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-array', 'Assert.assert-collection'],
    hideOperatorForm: true,
  },
  'assert-regexp': {
    category: 'Assert',
    description: 'If $value is not a `regexp` it throws `AssertionError`.',
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
      'let { assert-regexp } = import("Assert");\ntry assert-regexp(#"^start") catch (e) e.message end',
      'let { assert-regexp } = import("Assert");\ntry assert-regexp("hello", "Expected a regexp") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-sequence': {
    category: 'Assert',
    description: 'If $value is not a `sequence` (array or string) it throws `AssertionError`.',
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
      'let { assert-sequence } = import("Assert");\ntry assert-sequence([1, 2]) catch (e) e.message end',
      'let { assert-sequence } = import("Assert");\ntry assert-sequence("hello") catch (e) e.message end',
      'let { assert-sequence } = import("Assert");\ntry assert-sequence({ a: 1 }, "Expected a sequence") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-collection', 'Assert.assert-array'],
    hideOperatorForm: true,
  },
  'assert-string': {
    category: 'Assert',
    description: 'If $value is not a `string` it throws `AssertionError`.',
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
      'let { assert-string } = import("Assert");\ntry assert-string("hello") catch (e) e.message end',
      'let { assert-string } = import("Assert");\ntry assert-string(42, "Expected a string") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-number', 'Assert.assert-boolean', 'Assert.assert-regexp', 'Assert.assert-function'],
    hideOperatorForm: true,
  },
  'assert-vector': {
    category: 'Assert',
    description: 'If $value is not a `vector` it throws `AssertionError`.',
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
      'let { assert-vector } = import("Assert");\ntry assert-vector([1, 2, 3]) catch (e) e.message end',
      'let { assert-vector } = import("Assert");\ntry assert-vector(["a", "b"], "Expected a vector") catch (e) e.message end',
    ],
    seeAlso: ['Assert.assert-matrix', 'Assert.assert-grid'],
    hideOperatorForm: true,
  },
}
