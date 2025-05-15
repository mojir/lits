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
    examples: ['try { assert(0, "Expected a positive value") } catch (e) { e.message }'],
    noOperatorDocumentation: true,
  },
  'assert!=': {
    title: 'assert!=',
    category: 'Assert',
    linkName: 'assert-exclamation-equal',
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
      'try { assert!=(0, 0, "Expected different values") } catch (e) { e.message }',
      'try { assert!=(0, 0) } catch (e) { e.message }',
      'try { 0 assert!= 0 } catch (e) { e.message }',
      'try { assert!=(0, 1) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert=': {
    title: 'assert=',
    category: 'Assert',
    linkName: 'assert-equal',
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
      'try { assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") } catch (e) { e.message }',
      'try { assert=({ "a": 1 }, { "a": 2 }) } catch (e) { e.message }',
      'try { assert=({ "a": 1 }, { "a": 1 }) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-gt': {
    title: 'assert-gt',
    category: 'Assert',
    linkName: 'assert-gt',
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
      'try { assert-gt(0, 1, "Expected greater value") } catch (e) { e.message }',
      'try { assert-gt(0, 0) } catch (e) { e.message }',
      'try { assert-gt(1, 0) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-lt': {
    title: 'assert-lt',
    category: 'Assert',
    linkName: 'assert-lt',
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
      'try { assert-lte(1, 0, "Expected smaller value value") } catch (e) { e.message }',
      'try { assert-lte(1, 1) } catch (e) { e.message }',
      'try { assert-lte(0, 1) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-gte': {
    title: 'assert-gte',
    category: 'Assert',
    linkName: 'assert-gte',
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
      'try { assert-gte(0, 1, "Expected greater value") } catch (e) { e.message }',
      'try { assert-gte(0, 1) } catch (e) { e.message }',
      'try { assert-gte(1, 1) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-lte': {
    title: 'assert-lte',
    category: 'Assert',
    linkName: 'assert-lte',
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
      'try { assert-lte(1, 0, "Expected smaller value value") } catch (e) { e.message }',
      'try { assert-lte(1, 0) } catch (e) { e.message }',
      'try { assert-lte(1, 1) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-true': {
    title: 'assert-true',
    category: 'Assert',
    linkName: 'assert-true',
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
      'try { assert-true(false, "Expected true") } catch (e) { e.message }',
      'try { assert-true(false) } catch (e) { e.message }',
      'try { assert-true(true) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-false': {
    title: 'assert-false',
    category: 'Assert',
    linkName: 'assert-false',
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
      'try { assert-false(true, "Expected false") } catch (e) { e.message }',
      'try { assert-false(true) } catch (e) { e.message }',
      'try { assert-false(false) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-truthy': {
    title: 'assert-truthy',
    category: 'Assert',
    linkName: 'assert-truthy',
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
      'try { assert-truthy(false, "Expected truthy") } catch (e) { e.message }',
      'try { assert-truthy(false) } catch (e) { e.message }',
      'try { assert-truthy(0) } catch (e) { e.message }',
      'try { assert-truthy(null) } catch (e) { e.message }',
      'try { assert-truthy("") } catch (e) { e.message }',
      'try { assert-truthy(true) } catch (e) { e.message }',
      'try { assert-truthy(1) } catch (e) { e.message }',
      'try { assert-truthy("x") } catch (e) { e.message }',
      'try { assert-truthy([]) } catch (e) { e.message }',
      'try { assert-truthy({}) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-falsy': {
    title: 'assert-falsy',
    category: 'Assert',
    linkName: 'assert-falsy',
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
      'try { assert-falsy(true, "Expected falsy") } catch (e) { e.message }',
      'try { assert-falsy("x") } catch (e) { e.message }',
      'try { assert-falsy([]) } catch (e) { e.message }',
      'try { assert-falsy({}) } catch (e) { e.message }',
      'try { assert-falsy(1) } catch (e) { e.message }',
      'try { assert-falsy(false) } catch (e) { e.message }',
      'try { assert-falsy(0) } catch (e) { e.message }',
      'try { assert-falsy(null) } catch (e) { e.message }',
      'try { assert-falsy("") } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-null': {
    title: 'assert-null',
    category: 'Assert',
    linkName: 'assert-null',
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
      'try { assert-null(null) } catch (e) { e.message }',
      'try { assert-null(true, "Expected null") } catch (e) { e.message }',
      'try { assert-null("x") } catch (e) { e.message }',
      'try { assert-null([]) } catch (e) { e.message }',
      'try { assert-null({}) } catch (e) { e.message }',
      'try { assert-null(1) } catch (e) { e.message }',
      'try { assert-null(false) } catch (e) { e.message }',
      'try { assert-null(0) } catch (e) { e.message }',
      'try { assert-null("") } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-throws': {
    title: 'assert-throws',
    category: 'Assert',
    linkName: 'assert-throws',
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
      'assert-throws(-> throw("Error"))',
      'try { assert-throws(-> identity("Error")) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-throws-error': {
    title: 'assert-throws-error',
    category: 'Assert',
    linkName: 'assert-throws-error',
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
      'try { assert-throws-error(-> throw("Error"), "Error") } catch (e) { e.message }',
      'try { assert-throws-error(-> identity("Error"), "Error") } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
  'assert-not-throws': {
    title: 'assert-not-throws',
    category: 'Assert',
    linkName: 'assert-not-throws',
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
      'try { assert-not-throws(-> identity("Error")) } catch (e) { e.message }',
      'try { assert-not-throws(-> throw("Error")) } catch (e) { e.message }',
    ],
    noOperatorDocumentation: true,
  },
}
