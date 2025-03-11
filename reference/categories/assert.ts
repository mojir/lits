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
    description: 'If $a is not structural equal to $b it throws `AssertionError`.',
    examples: [
      'try assert=({ "a" := 1 }, { "a" := 2 }, "Expected equal values") catch (e) e.message end',
      'try assert=({ "a" := 1 }, { "a" := 2 }) catch (e) e.message end',
      'try assert=({ "a" := 1 }, { "a" := 1 }) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-gt': {
    title: 'assert-gt',
    category: 'Assert',
    linkName: 'assert-gt',
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
      'try assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
      'try assert-gt(0, 0) catch (e) e.message end',
      'try assert-gt(1, 0) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-lt': {
    title: 'assert-lt',
    category: 'Assert',
    linkName: 'assert-lt',
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
      'try assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'try assert-lte(1, 1) catch (e) e.message end',
      'try assert-lte(0, 1) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-gte': {
    title: 'assert-gte',
    category: 'Assert',
    linkName: 'assert-gte',
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
      'try assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
      'try assert-gte(0, 1) catch (e) e.message end',
      'try assert-gte(1, 1) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-lte': {
    title: 'assert-lte',
    category: 'Assert',
    linkName: 'assert-lte',
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
      'try assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'try assert-lte(1, 0) catch (e) e.message end',
      'try assert-lte(1, 1) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-true': {
    title: 'assert-true',
    category: 'Assert',
    linkName: 'assert-true',
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
      'try assert-true(false, "Expected true") catch (e) e.message end',
      'try assert-true(false) catch (e) e.message end',
      'try assert-true(true) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-false': {
    title: 'assert-false',
    category: 'Assert',
    linkName: 'assert-false',
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
      'try assert-false(true, "Expected false") catch (e) e.message end',
      'try assert-false(true) catch (e) e.message end',
      'try assert-false(false) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-truthy': {
    title: 'assert-truthy',
    category: 'Assert',
    linkName: 'assert-truthy',
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
      'try assert-truthy(false, "Expected truthy") catch (e) e.message end',
      'try assert-truthy(false) catch (e) e.message end',
      'try assert-truthy(0) catch (e) e.message end',
      'try assert-truthy(null) catch (e) e.message end',
      'try assert-truthy("") catch (e) e.message end',
      'try assert-truthy(true) catch (e) e.message end',
      'try assert-truthy(1) catch (e) e.message end',
      'try assert-truthy("x") catch (e) e.message end',
      'try assert-truthy([]) catch (e) e.message end',
      'try assert-truthy({}) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-falsy': {
    title: 'assert-falsy',
    category: 'Assert',
    linkName: 'assert-falsy',
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
      'try assert-falsy(true, "Expected falsy") catch (e) e.message end',
      'try assert-falsy("x") catch (e) e.message end',
      'try assert-falsy([]) catch (e) e.message end',
      'try assert-falsy({}) catch (e) e.message end',
      'try assert-falsy(1) catch (e) e.message end',
      'try assert-falsy(false) catch (e) e.message end',
      'try assert-falsy(0) catch (e) e.message end',
      'try assert-falsy(null) catch (e) e.message end',
      'try assert-falsy("") catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-null': {
    title: 'assert-null',
    category: 'Assert',
    linkName: 'assert-null',
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
      'try assert-null(null) catch (e) e.message end',
      'try assert-null(true, "Expected null") catch (e) e.message end',
      'try assert-null("x") catch (e) e.message end',
      'try assert-null([]) catch (e) e.message end',
      'try assert-null({}) catch (e) e.message end',
      'try assert-null(1) catch (e) e.message end',
      'try assert-null(false) catch (e) e.message end',
      'try assert-null(0) catch (e) e.message end',
      'try assert-null("") catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-throws': {
    title: 'assert-throws',
    category: 'Assert',
    linkName: 'assert-throws',
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
    examples: [
      'assert-throws(-> throw("Error"))',
      'try assert-throws(-> identity("Error")) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-throws-error': {
    title: 'assert-throws-error',
    category: 'Assert',
    linkName: 'assert-throws-error',
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
      'try assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
      'try assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'assert-not-throws': {
    title: 'assert-not-throws',
    category: 'Assert',
    linkName: 'assert-not-throws',
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
      'try assert-not-throws(-> identity("Error")) catch (e) e.message end',
      'try assert-not-throws(-> throw("Error")) catch (e) e.message end',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
}
