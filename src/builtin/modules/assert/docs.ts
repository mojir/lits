import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'assert': {
    category: 'assert',
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
      'let { assert } = import("assert");\ntry assert(0, "Expected a positive value") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-truthy', 'assert.assert-true'],
    hideOperatorForm: true,
  },
  'assert!=': {
    category: 'assert',
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
      'let { assert!= } = import("assert");\ntry assert!=(0, 0, "Expected different values") catch (e) e.message end',
      'let { assert!= } = import("assert");\ntry assert!=(0, 0) catch (e) e.message end',
      'let { assert!= } = import("assert");\ntry 0 assert!= 0 catch (e) e.message end',
      'let { assert!= } = import("assert");\ntry assert!=(0, 1) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert='],
    hideOperatorForm: true,
  },
  'assert=': {
    category: 'assert',
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
      'let { assert= } = import("assert");\ntry assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") catch (e) e.message end',
      'let { assert= } = import("assert");\ntry assert=({ "a": 1 }, { "a": 2 }) catch (e) e.message end',
      'let { assert= } = import("assert");\ntry assert=({ "a": 1 }, { "a": 1 }) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert!='],
    hideOperatorForm: true,
  },
  'assert-gt': {
    category: 'assert',
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
      'let { assert-gt } = import("assert");\ntry assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gt } = import("assert");\ntry assert-gt(0, 0) catch (e) e.message end',
      'let { assert-gt } = import("assert");\ntry assert-gt(1, 0) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-lt', 'assert.assert-gte', 'assert.assert-lte'],
    hideOperatorForm: true,
  },
  'assert-lt': {
    category: 'assert',
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
      'let { assert-lt } = import("assert");\ntry assert-lt(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lt } = import("assert");\ntry assert-lt(1, 1) catch (e) e.message end',
      'let { assert-lt } = import("assert");\ntry assert-lt(0, 1) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-gt', 'assert.assert-lte', 'assert.assert-gte'],
    hideOperatorForm: true,
  },
  'assert-gte': {
    category: 'assert',
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
      'let { assert-gte } = import("assert");\ntry assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gte } = import("assert");\ntry assert-gte(0, 1) catch (e) e.message end',
      'let { assert-gte } = import("assert");\ntry assert-gte(1, 1) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-lte', 'assert.assert-gt', 'assert.assert-lt'],
    hideOperatorForm: true,
  },
  'assert-lte': {
    category: 'assert',
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
      'let { assert-lte } = import("assert");\ntry assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lte } = import("assert");\ntry assert-lte(1, 0) catch (e) e.message end',
      'let { assert-lte } = import("assert");\ntry assert-lte(1, 1) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-gte', 'assert.assert-lt', 'assert.assert-gt'],
    hideOperatorForm: true,
  },
  'assert-true': {
    category: 'assert',
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
      'let { assert-true } = import("assert");\ntry assert-true(false, "Expected true") catch (e) e.message end',
      'let { assert-true } = import("assert");\ntry assert-true(false) catch (e) e.message end',
      'let { assert-true } = import("assert");\ntry assert-true(true) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-false', 'assert.assert-truthy', 'assert.assert-falsy', 'assert.assert', 'assert.assert-boolean'],
    hideOperatorForm: true,
  },
  'assert-false': {
    category: 'assert',
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
      'let { assert-false } = import("assert");\ntry assert-false(true, "Expected false") catch (e) e.message end',
      'let { assert-false } = import("assert");\ntry assert-false(true) catch (e) e.message end',
      'let { assert-false } = import("assert");\ntry assert-false(false) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-true', 'assert.assert-falsy', 'assert.assert-truthy', 'assert.assert-boolean'],
    hideOperatorForm: true,
  },
  'assert-truthy': {
    category: 'assert',
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
      'let { assert-truthy } = import("assert");\ntry assert-truthy(false, "Expected truthy") catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(false) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(0) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(null) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy("") catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(true) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(1) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy("x") catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy([]) catch (e) e.message end',
      'let { assert-truthy } = import("assert");\ntry assert-truthy(nd) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-falsy', 'assert.assert-true', 'assert.assert-false', 'assert.assert', 'assert.assert-null'],
    hideOperatorForm: true,
  },
  'assert-falsy': {
    category: 'assert',
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
      'let { assert-falsy } = import("assert");\ntry assert-falsy(true, "Expected falsy") catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy("x") catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy([]) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy(nd) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy(1) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy(false) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy(0) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy(null) catch (e) e.message end',
      'let { assert-falsy } = import("assert");\ntry assert-falsy("") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-truthy', 'assert.assert-false', 'assert.assert-true', 'assert.assert-null'],
    hideOperatorForm: true,
  },
  'assert-null': {
    category: 'assert',
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
      'let { assert-null } = import("assert");\ntry assert-null(null) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null(true, "Expected null") catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null("x") catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null([]) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null(nd) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null(1) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null(false) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null(0) catch (e) e.message end',
      'let { assert-null } = import("assert");\ntry assert-null("") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-truthy', 'assert.assert-falsy'],
    hideOperatorForm: true,
  },
  'assert-throws': {
    category: 'assert',
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
      'let { assert-throws } = import("assert");\nassert-throws(-> throw("Error"))',
      'let { assert-throws } = import("assert");\ntry assert-throws(-> identity("Error")) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-throws-error', 'assert.assert-not-throws'],
    hideOperatorForm: true,
  },
  'assert-throws-error': {
    category: 'assert',
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
      'let { assert-throws-error } = import("assert");\ntry assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
      'let { assert-throws-error } = import("assert");\ntry assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-throws', 'assert.assert-not-throws'],
    hideOperatorForm: true,
  },
  'assert-not-throws': {
    category: 'assert',
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
      'let { assert-not-throws } = import("assert");\ntry assert-not-throws(-> identity("Error")) catch (e) e.message end',
      'let { assert-not-throws } = import("assert");\ntry assert-not-throws(-> throw("Error")) catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-throws', 'assert.assert-throws-error'],
    hideOperatorForm: true,
  },
  'assert-array': {
    category: 'assert',
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
      'let { assert-array } = import("assert");\ntry assert-array([1, 2, 3]) catch (e) e.message end',
      'let { assert-array } = import("assert");\ntry assert-array("string") catch (e) e.message end',
      'let { assert-array } = import("assert");\ntry assert-array(42, "Expected an array") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-object', 'assert.assert-collection', 'assert.assert-sequence'],
    hideOperatorForm: true,
  },
  'assert-boolean': {
    category: 'assert',
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
      'let { assert-boolean } = import("assert");\ntry assert-boolean(true) catch (e) e.message end',
      'let { assert-boolean } = import("assert");\ntry assert-boolean(false) catch (e) e.message end',
      'let { assert-boolean } = import("assert");\ntry assert-boolean(1, "Expected a boolean") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-true', 'assert.assert-false', 'assert.assert-number', 'assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-collection': {
    category: 'assert',
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
      'let { assert-collection } = import("assert");\ntry assert-collection([1, 2]) catch (e) e.message end',
      'let { assert-collection } = import("assert");\ntry assert-collection({ a: 1 }) catch (e) e.message end',
      'let { assert-collection } = import("assert");\ntry assert-collection("hello") catch (e) e.message end',
      'let { assert-collection } = import("assert");\ntry assert-collection(42, "Expected a collection") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-sequence', 'assert.assert-array', 'assert.assert-object'],
    hideOperatorForm: true,
  },
  'assert-function': {
    category: 'assert',
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
      'let { assert-function } = import("assert");\ntry assert-function(-> $ + 1) catch (e) e.message end',
      'let { assert-function } = import("assert");\ntry assert-function(42, "Expected a function") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-number', 'assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-grid': {
    category: 'assert',
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
      'let { assert-grid } = import("assert");\ntry assert-grid([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-grid } = import("assert");\ntry assert-grid([1, 2], "Expected a grid") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-matrix', 'assert.assert-vector'],
    hideOperatorForm: true,
  },
  'assert-integer': {
    category: 'assert',
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
      'let { assert-integer } = import("assert");\ntry assert-integer(42) catch (e) e.message end',
      'let { assert-integer } = import("assert");\ntry assert-integer(3.14, "Expected an integer") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-number'],
    hideOperatorForm: true,
  },
  'assert-matrix': {
    category: 'assert',
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
      'let { assert-matrix } = import("assert");\ntry assert-matrix([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-matrix } = import("assert");\ntry assert-matrix([1, 2], "Expected a matrix") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-vector', 'assert.assert-grid'],
    hideOperatorForm: true,
  },
  'assert-number': {
    category: 'assert',
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
      'let { assert-number } = import("assert");\ntry assert-number(42) catch (e) e.message end',
      'let { assert-number } = import("assert");\ntry assert-number("hello", "Expected a number") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-integer', 'assert.assert-boolean', 'assert.assert-string', 'assert.assert-function'],
    hideOperatorForm: true,
  },
  'assert-object': {
    category: 'assert',
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
      'let { assert-object } = import("assert");\ntry assert-object({ a: 1 }) catch (e) e.message end',
      'let { assert-object } = import("assert");\ntry assert-object([1, 2], "Expected an object") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-array', 'assert.assert-collection'],
    hideOperatorForm: true,
  },
  'assert-regexp': {
    category: 'assert',
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
      'let { assert-regexp } = import("assert");\ntry assert-regexp(#"^start") catch (e) e.message end',
      'let { assert-regexp } = import("assert");\ntry assert-regexp("hello", "Expected a regexp") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-string'],
    hideOperatorForm: true,
  },
  'assert-sequence': {
    category: 'assert',
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
      'let { assert-sequence } = import("assert");\ntry assert-sequence([1, 2]) catch (e) e.message end',
      'let { assert-sequence } = import("assert");\ntry assert-sequence("hello") catch (e) e.message end',
      'let { assert-sequence } = import("assert");\ntry assert-sequence({ a: 1 }, "Expected a sequence") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-collection', 'assert.assert-array'],
    hideOperatorForm: true,
  },
  'assert-string': {
    category: 'assert',
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
      'let { assert-string } = import("assert");\ntry assert-string("hello") catch (e) e.message end',
      'let { assert-string } = import("assert");\ntry assert-string(42, "Expected a string") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-number', 'assert.assert-boolean', 'assert.assert-regexp', 'assert.assert-function'],
    hideOperatorForm: true,
  },
  'assert-vector': {
    category: 'assert',
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
      'let { assert-vector } = import("assert");\ntry assert-vector([1, 2, 3]) catch (e) e.message end',
      'let { assert-vector } = import("assert");\ntry assert-vector(["a", "b"], "Expected a vector") catch (e) e.message end',
    ],
    seeAlso: ['assert.assert-matrix', 'assert.assert-grid'],
    hideOperatorForm: true,
  },
}
