import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'assert!=': {
    category: 'assertion',
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
      'let { assert!= } = import(assertion);\ntry assert!=(0, 0, "Expected different values") catch (e) e.message end',
      'let { assert!= } = import(assertion);\ntry assert!=(0, 0) catch (e) e.message end',
      'let { assert!= } = import(assertion);\ntry 0 assert!= 0 catch (e) e.message end',
      'let { assert!= } = import(assertion);\ntry assert!=(0, 1) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert='],
    hideOperatorForm: true,
  },
  'assert=': {
    category: 'assertion',
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
      'let { assert= } = import(assertion);\ntry assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") catch (e) e.message end',
      'let { assert= } = import(assertion);\ntry assert=({ "a": 1 }, { "a": 2 }) catch (e) e.message end',
      'let { assert= } = import(assertion);\ntry assert=({ "a": 1 }, { "a": 1 }) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert!='],
    hideOperatorForm: true,
  },
  'assert-gt': {
    category: 'assertion',
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
      'let { assert-gt } = import(assertion);\ntry assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gt } = import(assertion);\ntry assert-gt(0, 0) catch (e) e.message end',
      'let { assert-gt } = import(assertion);\ntry assert-gt(1, 0) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-lt', 'assertion.assert-gte', 'assertion.assert-lte'],
    hideOperatorForm: true,
  },
  'assert-lt': {
    category: 'assertion',
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
      'let { assert-lt } = import(assertion);\ntry assert-lt(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lt } = import(assertion);\ntry assert-lt(1, 1) catch (e) e.message end',
      'let { assert-lt } = import(assertion);\ntry assert-lt(0, 1) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-gt', 'assertion.assert-lte', 'assertion.assert-gte'],
    hideOperatorForm: true,
  },
  'assert-gte': {
    category: 'assertion',
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
      'let { assert-gte } = import(assertion);\ntry assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gte } = import(assertion);\ntry assert-gte(0, 1) catch (e) e.message end',
      'let { assert-gte } = import(assertion);\ntry assert-gte(1, 1) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-lte', 'assertion.assert-gt', 'assertion.assert-lt'],
    hideOperatorForm: true,
  },
  'assert-lte': {
    category: 'assertion',
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
      'let { assert-lte } = import(assertion);\ntry assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lte } = import(assertion);\ntry assert-lte(1, 0) catch (e) e.message end',
      'let { assert-lte } = import(assertion);\ntry assert-lte(1, 1) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-gte', 'assertion.assert-lt', 'assertion.assert-gt'],
    hideOperatorForm: true,
  },
  'assert-true': {
    category: 'assertion',
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
      'let { assert-true } = import(assertion);\ntry assert-true(false, "Expected true") catch (e) e.message end',
      'let { assert-true } = import(assertion);\ntry assert-true(false) catch (e) e.message end',
      'let { assert-true } = import(assertion);\ntry assert-true(true) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-false', 'assertion.assert-truthy', 'assertion.assert-falsy', 'assert', 'assertion.assert-boolean'],
    hideOperatorForm: true,
  },
  'assert-false': {
    category: 'assertion',
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
      'let { assert-false } = import(assertion);\ntry assert-false(true, "Expected false") catch (e) e.message end',
      'let { assert-false } = import(assertion);\ntry assert-false(true) catch (e) e.message end',
      'let { assert-false } = import(assertion);\ntry assert-false(false) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-true', 'assertion.assert-falsy', 'assertion.assert-truthy', 'assertion.assert-boolean'],
    hideOperatorForm: true,
  },
  'assert-truthy': {
    category: 'assertion',
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
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(false, "Expected truthy") catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(false) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(0) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(null) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy("") catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(true) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(1) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy("x") catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy([]) catch (e) e.message end',
      'let { assert-truthy } = import(assertion);\ntry assert-truthy(nd) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-falsy', 'assertion.assert-true', 'assertion.assert-false', 'assert', 'assertion.assert-null'],
    hideOperatorForm: true,
  },
  'assert-falsy': {
    category: 'assertion',
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
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(true, "Expected falsy") catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy("x") catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy([]) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(nd) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(1) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(false) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(0) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy(null) catch (e) e.message end',
      'let { assert-falsy } = import(assertion);\ntry assert-falsy("") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-truthy', 'assertion.assert-false', 'assertion.assert-true', 'assertion.assert-null'],
    hideOperatorForm: true,
  },
  'assert-null': {
    category: 'assertion',
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
      'let { assert-null } = import(assertion);\ntry assert-null(null) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null(true, "Expected null") catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null("x") catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null([]) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null(nd) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null(1) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null(false) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null(0) catch (e) e.message end',
      'let { assert-null } = import(assertion);\ntry assert-null("") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-truthy', 'assertion.assert-falsy'],
    hideOperatorForm: true,
  },
  'assert-throws': {
    category: 'assertion',
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
      'let { assert-throws } = import(assertion);\nassert-throws(-> throw("Error"))',
      'let { assert-throws } = import(assertion);\ntry assert-throws(-> identity("Error")) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-throws-error', 'assertion.assert-not-throws'],
    hideOperatorForm: true,
  },
  'assert-throws-error': {
    category: 'assertion',
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
      'let { assert-throws-error } = import(assertion);\ntry assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
      'let { assert-throws-error } = import(assertion);\ntry assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-throws', 'assertion.assert-not-throws'],
    hideOperatorForm: true,
  },
  'assert-not-throws': {
    category: 'assertion',
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
      'let { assert-not-throws } = import(assertion);\ntry assert-not-throws(-> identity("Error")) catch (e) e.message end',
      'let { assert-not-throws } = import(assertion);\ntry assert-not-throws(-> throw("Error")) catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-throws', 'assertion.assert-throws-error'],
    hideOperatorForm: true,
  },
  'assert-array': {
    category: 'assertion',
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
      'let { assert-array } = import(assertion);\ntry assert-array([1, 2, 3]) catch (e) e.message end',
      'let { assert-array } = import(assertion);\ntry assert-array("string") catch (e) e.message end',
      'let { assert-array } = import(assertion);\ntry assert-array(42, "Expected an array") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-object', 'assertion.assert-collection', 'assertion.assert-sequence'],
    hideOperatorForm: true,
  },
  'assert-boolean': {
    category: 'assertion',
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
      'let { assert-boolean } = import(assertion);\ntry assert-boolean(true) catch (e) e.message end',
      'let { assert-boolean } = import(assertion);\ntry assert-boolean(false) catch (e) e.message end',
      'let { assert-boolean } = import(assertion);\ntry assert-boolean(1, "Expected a boolean") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-true', 'assertion.assert-false', 'assertion.assert-number', 'assertion.assert-string'],
    hideOperatorForm: true,
  },
  'assert-collection': {
    category: 'assertion',
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
      'let { assert-collection } = import(assertion);\ntry assert-collection([1, 2]) catch (e) e.message end',
      'let { assert-collection } = import(assertion);\ntry assert-collection({ a: 1 }) catch (e) e.message end',
      'let { assert-collection } = import(assertion);\ntry assert-collection("hello") catch (e) e.message end',
      'let { assert-collection } = import(assertion);\ntry assert-collection(42, "Expected a collection") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-sequence', 'assertion.assert-array', 'assertion.assert-object'],
    hideOperatorForm: true,
  },
  'assert-function': {
    category: 'assertion',
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
      'let { assert-function } = import(assertion);\ntry assert-function(-> $ + 1) catch (e) e.message end',
      'let { assert-function } = import(assertion);\ntry assert-function(42, "Expected a function") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-number', 'assertion.assert-string'],
    hideOperatorForm: true,
  },
  'assert-grid': {
    category: 'assertion',
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
      'let { assert-grid } = import(assertion);\ntry assert-grid([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-grid } = import(assertion);\ntry assert-grid([1, 2], "Expected a grid") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-matrix', 'assertion.assert-vector'],
    hideOperatorForm: true,
  },
  'assert-integer': {
    category: 'assertion',
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
      'let { assert-integer } = import(assertion);\ntry assert-integer(42) catch (e) e.message end',
      'let { assert-integer } = import(assertion);\ntry assert-integer(3.14, "Expected an integer") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-number'],
    hideOperatorForm: true,
  },
  'assert-matrix': {
    category: 'assertion',
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
      'let { assert-matrix } = import(assertion);\ntry assert-matrix([[1, 2], [3, 4]]) catch (e) e.message end',
      'let { assert-matrix } = import(assertion);\ntry assert-matrix([1, 2], "Expected a matrix") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-vector', 'assertion.assert-grid'],
    hideOperatorForm: true,
  },
  'assert-number': {
    category: 'assertion',
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
      'let { assert-number } = import(assertion);\ntry assert-number(42) catch (e) e.message end',
      'let { assert-number } = import(assertion);\ntry assert-number("hello", "Expected a number") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-integer', 'assertion.assert-boolean', 'assertion.assert-string', 'assertion.assert-function'],
    hideOperatorForm: true,
  },
  'assert-object': {
    category: 'assertion',
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
      'let { assert-object } = import(assertion);\ntry assert-object({ a: 1 }) catch (e) e.message end',
      'let { assert-object } = import(assertion);\ntry assert-object([1, 2], "Expected an object") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-array', 'assertion.assert-collection'],
    hideOperatorForm: true,
  },
  'assert-regexp': {
    category: 'assertion',
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
      'let { assert-regexp } = import(assertion);\ntry assert-regexp(#"^start") catch (e) e.message end',
      'let { assert-regexp } = import(assertion);\ntry assert-regexp("hello", "Expected a regexp") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-string'],
    hideOperatorForm: true,
  },
  'assert-sequence': {
    category: 'assertion',
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
      'let { assert-sequence } = import(assertion);\ntry assert-sequence([1, 2]) catch (e) e.message end',
      'let { assert-sequence } = import(assertion);\ntry assert-sequence("hello") catch (e) e.message end',
      'let { assert-sequence } = import(assertion);\ntry assert-sequence({ a: 1 }, "Expected a sequence") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-collection', 'assertion.assert-array'],
    hideOperatorForm: true,
  },
  'assert-string': {
    category: 'assertion',
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
      'let { assert-string } = import(assertion);\ntry assert-string("hello") catch (e) e.message end',
      'let { assert-string } = import(assertion);\ntry assert-string(42, "Expected a string") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-number', 'assertion.assert-boolean', 'assertion.assert-regexp', 'assertion.assert-function'],
    hideOperatorForm: true,
  },
  'assert-vector': {
    category: 'assertion',
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
      'let { assert-vector } = import(assertion);\ntry assert-vector([1, 2, 3]) catch (e) e.message end',
      'let { assert-vector } = import(assertion);\ntry assert-vector(["a", "b"], "Expected a vector") catch (e) e.message end',
    ],
    seeAlso: ['assertion.assert-matrix', 'assertion.assert-grid'],
    hideOperatorForm: true,
  },
}
