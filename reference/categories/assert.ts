import { type AssertApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const assertReference: Record<AssertApiName, FunctionReference<'Assert'>> = {
  'assert': {
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
    examples: ['try assert(0, "Expected a positive value") catch (e) e.message end'],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert=': {
    title: 'assert=',
    category: 'Assert',
    linkName: 'assert-equal',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert=(0, 1, "Expected same values") catch (e) e.message end',
      'try assert=(0, 1) catch (e) e.message end',
      'try 0 assert= 1 catch (e) e.message end',
      'try assert=(1, 1) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert!=': {
    title: 'assert!=',
    category: 'Assert',
    linkName: 'assert-exclamation-equal',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert!=(0, 0, "Expected different values") catch (e) e.message end',
      'try assert!=(0, 0) catch (e) e.message end',
      'try 0 assert!= 0 catch (e) e.message end',
      'try assert!=(0, 1) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_equal': {
    title: 'assert_equal',
    category: 'Assert',
    linkName: 'assert_equal',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_equal({ "a" := 1 }, { "a" := 2 }, "Expected equal values") catch (e) e.message end',
      'try assert_equal({ "a" := 1 }, { "a" := 2 }) catch (e) e.message end',
      'try assert_equal({ "a" := 1 }, { "a" := 1 }) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_not_equal': {
    title: 'assert_not_equal',
    category: 'Assert',
    linkName: 'assert_not_equal',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_not_equal({ "a" := 2 }, { "a" := 2 }, "Expected different values") catch (e) e.message end',
      'try assert_not_equal({ "a" := 2 }, { "a" := 2 }) catch (e) e.message end',
      'try assert_not_equal({ "a" := 1 }, { "a" := 2 }) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_gt': {
    title: 'assert_gt',
    category: 'Assert',
    linkName: 'assert_gt',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_gt(0, 1, "Expected greater value") catch (e) e.message end',
      'try assert_gt(0, 0) catch (e) e.message end',
      'try assert_gt(1, 0) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_lt': {
    title: 'assert_lt',
    category: 'Assert',
    linkName: 'assert_lt',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'try assert_lte(1, 1) catch (e) e.message end',
      'try assert_lte(0, 1) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_gte': {
    title: 'assert_gte',
    category: 'Assert',
    linkName: 'assert_gte',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_gte(0, 1, "Expected greater value") catch (e) e.message end',
      'try assert_gte(0, 1) catch (e) e.message end',
      'try assert_gte(1, 1) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_lte': {
    title: 'assert_lte',
    category: 'Assert',
    linkName: 'assert_lte',
    clojureDocs: null,
    returns: {
      type: 'null',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
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
      'try assert_lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'try assert_lte(1, 0) catch (e) e.message end',
      'try assert_lte(1, 1) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_true': {
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
      'try assert_true(false, "Expected true") catch (e) e.message end',
      'try assert_true(false) catch (e) e.message end',
      'try assert_true(true) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_false': {
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
      'try assert_false(true, "Expected false") catch (e) e.message end',
      'try assert_false(true) catch (e) e.message end',
      'try assert_false(false) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_truthy': {
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
      'try assert_truthy(false, "Expected truthy") catch (e) e.message end',
      'try assert_truthy(false) catch (e) e.message end',
      'try assert_truthy(0) catch (e) e.message end',
      'try assert_truthy(null) catch (e) e.message end',
      'try assert_truthy("") catch (e) e.message end',
      'try assert_truthy(true) catch (e) e.message end',
      'try assert_truthy(1) catch (e) e.message end',
      'try assert_truthy("x") catch (e) e.message end',
      'try assert_truthy([]) catch (e) e.message end',
      'try assert_truthy({}) catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_falsy': {
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
      'try assert_falsy(true, "Expected falsy") catch (e) e.message end',
      'try assert_falsy("x") catch (e) e.message end',
      'try assert_falsy([]) catch (e) e.message end',
      'try assert_falsy({}) catch (e) e.message end',
      'try assert_falsy(1) catch (e) e.message end',
      'try assert_falsy(false) catch (e) e.message end',
      'try assert_falsy(0) catch (e) e.message end',
      'try assert_falsy(null) catch (e) e.message end',
      'try assert_falsy("") catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_null': {
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
    description: 'If $value is not `null` it throws `AssertionError`.',
    examples: [
      'try assert_null(null) catch (e) e.message end',
      'try assert_null(true, "Expected null") catch (e) e.message end',
      'try assert_null("x") catch (e) e.message end',
      'try assert_null([]) catch (e) e.message end',
      'try assert_null({}) catch (e) e.message end',
      'try assert_null(1) catch (e) e.message end',
      'try assert_null(false) catch (e) e.message end',
      'try assert_null(0) catch (e) e.message end',
      'try assert_null("") catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_throws': {
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
    examples: ['assert_throws(-> throw("Error"))', 'assert_throws(-> identity("Error"))'],
  },
  'assert_throws_error': {
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
      'try assert_throws_error(-> throw("Error"), "Error") catch (e) e.message end',
      'try assert_throws_error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    algebraic: true,
  },
  'assert_not_throws': {
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
    examples: [
      'try assert_not_throws(-> identity("Error")) catch (e) e.message end',
      'try assert_not_throws(-> throw("Error")) catch (e) e.message end',
    ],
  },
}
