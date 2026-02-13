import { type AssertApiName, getOperatorArgs } from '../../api'
import type { FunctionReference } from '../..'

export const assertReference: Record<AssertApiName, FunctionReference<'Assert'>> = {
  'Assert.assert': {
    title: 'Assert.assert',
    category: 'Assert',
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
    examples: ['let { assert } = import("Assert");\ntry assert(0, "Expected a positive value") catch (e) e.message end'],
    noOperatorDocumentation: true,
  },
  'Assert.assert!=': {
    title: 'Assert.assert!=',
    category: 'Assert',
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
      'let { assert!= } = import("Assert");\ntry assert!=(0, 0, "Expected different values") catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry assert!=(0, 0) catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry 0 assert!= 0 catch (e) e.message end',
      'let { assert!= } = import("Assert");\ntry assert!=(0, 1) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert=': {
    title: 'Assert.assert=',
    category: 'Assert',
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
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") catch (e) e.message end',
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 2 }) catch (e) e.message end',
      'let { assert= } = import("Assert");\ntry assert=({ "a": 1 }, { "a": 1 }) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-gt': {
    title: 'Assert.assert-gt',
    category: 'Assert',
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
      'let { assert-gt } = import("Assert");\ntry assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gt } = import("Assert");\ntry assert-gt(0, 0) catch (e) e.message end',
      'let { assert-gt } = import("Assert");\ntry assert-gt(1, 0) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-lt': {
    title: 'Assert.assert-lt',
    category: 'Assert',
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
      'let { assert-lt } = import("Assert");\ntry assert-lt(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lt } = import("Assert");\ntry assert-lt(1, 1) catch (e) e.message end',
      'let { assert-lt } = import("Assert");\ntry assert-lt(0, 1) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-gte': {
    title: 'Assert.assert-gte',
    category: 'Assert',
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
      'let { assert-gte } = import("Assert");\ntry assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
      'let { assert-gte } = import("Assert");\ntry assert-gte(0, 1) catch (e) e.message end',
      'let { assert-gte } = import("Assert");\ntry assert-gte(1, 1) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-lte': {
    title: 'Assert.assert-lte',
    category: 'Assert',
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
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 0) catch (e) e.message end',
      'let { assert-lte } = import("Assert");\ntry assert-lte(1, 1) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-true': {
    title: 'Assert.assert-true',
    category: 'Assert',
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
      'let { assert-true } = import("Assert");\ntry assert-true(false, "Expected true") catch (e) e.message end',
      'let { assert-true } = import("Assert");\ntry assert-true(false) catch (e) e.message end',
      'let { assert-true } = import("Assert");\ntry assert-true(true) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-false': {
    title: 'Assert.assert-false',
    category: 'Assert',
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
      'let { assert-false } = import("Assert");\ntry assert-false(true, "Expected false") catch (e) e.message end',
      'let { assert-false } = import("Assert");\ntry assert-false(true) catch (e) e.message end',
      'let { assert-false } = import("Assert");\ntry assert-false(false) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-truthy': {
    title: 'Assert.assert-truthy',
    category: 'Assert',
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
    noOperatorDocumentation: true,
  },
  'Assert.assert-falsy': {
    title: 'Assert.assert-falsy',
    category: 'Assert',
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
    noOperatorDocumentation: true,
  },
  'Assert.assert-null': {
    title: 'Assert.assert-null',
    category: 'Assert',
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
    noOperatorDocumentation: true,
  },
  'Assert.assert-throws': {
    title: 'Assert.assert-throws',
    category: 'Assert',
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
      { argumentNames: ['fun'] },
      { argumentNames: ['fun', 'message'] },
    ],
    description: 'If $fun does not throw, it throws `AssertionError`.',
    examples: [
      'let { assert-throws } = import("Assert");\nassert-throws(-> throw("Error"))',
      'let { assert-throws } = import("Assert");\ntry assert-throws(-> identity("Error")) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-throws-error': {
    title: 'Assert.assert-throws-error',
    category: 'Assert',
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
      { argumentNames: ['value', 'error-message'] },
      { argumentNames: ['value', 'error-message', 'message'] },
    ],
    description: 'If $fun does not throw $error-message, it throws `AssertionError`.',
    examples: [
      'let { assert-throws-error } = import("Assert");\ntry assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
      'let { assert-throws-error } = import("Assert");\ntry assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
  'Assert.assert-not-throws': {
    title: 'Assert.assert-not-throws',
    category: 'Assert',
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
      { argumentNames: ['fun'] },
      { argumentNames: ['fun', 'message'] },
    ],
    description: 'If $fun throws, it throws `AssertionError`.',
    examples: [
      'let { assert-not-throws } = import("Assert");\ntry assert-not-throws(-> identity("Error")) catch (e) e.message end',
      'let { assert-not-throws } = import("Assert");\ntry assert-not-throws(-> throw("Error")) catch (e) e.message end',
    ],
    noOperatorDocumentation: true,
  },
}
