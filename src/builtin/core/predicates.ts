import { isLitsFunction } from '../../typeGuards/litsFunction'
import { assertColl, isColl, isObj, isRegularExpression, isSeq } from '../../typeGuards/lits'
import { assertNumber, isNumber } from '../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../interface'
import { isGrid, isMatrix, isVector } from '../../typeGuards/annotatedArrays'
import { EPSILON } from '../../utils'
import { toFixedArity } from '../../utils/arity'

export const predicatesNormalExpression: BuiltinNormalExpressions = {
  'function?': {
    evaluate: ([first]): boolean => isLitsFunction(first),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a function, otherwise `false`.',
      examples: [
        'function?(+)',
        'function?(/)',
        'function?((x, y) -> x + y)',
        'function?(false)',
        'function?("false")',
        'function?([1, 2, 3])',
      ],
    },
  },

  'string?': {
    evaluate: ([first]): boolean => typeof first === 'string',
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a string, otherwise `false`.',
      examples: [
        'string?("")',
        'string?("A string")',
        'string?(true ? "A string" : false)',
        'string?(false)',
        'string?([1, 2, 3])',
        'string?(100)',
      ],
    },
  },

  'number?': {
    evaluate: ([first]): boolean => typeof first === 'number',
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a number, otherwise `false`.',
      examples: [
        'number?(0)',
        'number?(2)',
        'number?(-0.12)',
        'number?(false)',
        'number?([1, 2, 3])',
        'number?("A string")',
      ],
    },
  },

  'integer?': {
    evaluate: ([first]): boolean => typeof first === 'number' && isNumber(first, { integer: true }),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is an integer, otherwise `false`.',
      examples: [
        'integer?(0)',
        'integer?(-12)',
        'integer?(42)',
        'integer?(10.1)',
        'integer?((x, y) -> x + y)',
        'integer?(false)',
        'integer?("false")',
        'integer?([1, 2, 3])',
      ],
    },
  },

  'boolean?': {
    evaluate: ([first]): boolean => typeof first === 'boolean',
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a `boolean`, otherwise `false`.',
      examples: [
        'boolean?(true)',
        'boolean?(false)',
        'boolean?([1, 2, 3])',
        'boolean?(0)',
        'boolean?("A string")',
      ],
    },
  },

  'null?': {
    evaluate: ([first]): boolean => first === null || first === undefined,
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is `null`, otherwise `false`.',
      examples: [
        'null?(null)',
        'null?(false)',
        'null?([1, 2, 3])',
        'null?(0)',
        'null?("A string")',
      ],
    },
  },

  'zero?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { finite: true })
      return Math.abs(value) < EPSILON
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is `0`, otherwise `false`.',
      examples: [
        'zero?(0)',
        'zero?(-0.0)',
        'zero?(1)',
        'zero?(0.1)',
      ],
    },
  },

  'pos?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first > 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is greater than `0`, otherwise `false`.',
      examples: [
        'pos?(0)',
        'pos?(-0.0)',
        'pos?(1)',
        'pos?(-0.1)',
      ],
    },
  },

  'neg?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first < 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is less than `0`, otherwise `false`.',
      examples: [
        'neg?(0)',
        'neg?(-0.0)',
        'neg?(1)',
        'neg?(-0.1)',
      ],
    },
  },

  'even?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return first % 2 === 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is even, otherwise `false`.',
      examples: [
        'even?(0)',
        'even?(-0.0)',
        'even?(-1)',
        'even?(2.1)',
      ],
    },
  },

  'odd?': {
    evaluate: ([first], sourceCodeInfo): boolean => {
      assertNumber(first, sourceCodeInfo, { finite: true })
      return isNumber(first, { integer: true }) && first % 2 !== 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is odd, otherwise `false`.',
      examples: [
        'odd?(1.0)',
        'odd?(1.001)',
        'odd?(-1)',
        'odd?(2.1)',
      ],
    },
  },

  'array?': {
    evaluate: ([first]): boolean => {
      return Array.isArray(first)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is an array, otherwise `false`.',
      examples: [
        'array?([])',
        'array?([1, 2, 3])',
        'array?(object("a", 10))',
        'array?(42)',
        'array?(10.1)',
        'array?((x, y) -> x + y)',
      ],
    },
  },

  'coll?': {
    evaluate: ([first]): boolean => {
      return isColl(first)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a Coll i.e. an array, an object or a string, otherwise `false`.',
      examples: [
        'coll?([])',
        'coll?([1, 2, 3])',
        'coll?(object("a", 10))',
        'coll?("Albert")',
        'coll?(42)',
        'coll?(10.1)',
        'coll?((x, y) -> x + y)',
      ],
    },
  },

  'seq?': {
    evaluate: ([first]): boolean => {
      return isSeq(first)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a Seq i.e. an array or a string, otherwise `false`.',
      examples: [
        'seq?([])',
        'seq?([1, 2, 3])',
        'seq?(object("a", 10))',
        'seq?("Albert")',
        'seq?(42)',
        'seq?(10.1)',
        'seq?((x, y) -> x + y)',
      ],
    },
  },

  'object?': {
    evaluate: ([first]): boolean => isObj(first),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is an object, otherwise `false`.',
      examples: [
        'object?(object("a", 10))',
        'object?(42)',
        'object?(10.1)',
        'object?((x, y) -> x + y)',
        'object?(#"^start")',
        'object?("false")',
        'object?([1, 2, 3])',
      ],
    },
  },

  'regexp?': {
    evaluate: ([value]): boolean => isRegularExpression(value),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is a regexp, otherwise `false`.',
      examples: [
        'regexp?(regexp("^start"))',
        'regexp?(#"^start")',
        'regexp?(-12)',
        'regexp?({})',
        'regexp?(10.1)',
        'regexp?((x, y) -> x + y)',
        'regexp?(false)',
        'regexp?("false")',
        'regexp?([1, 2, 3])',
      ],
    },
  },

  'finite?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return Number.isFinite(value)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is finite, otherwise `false`.',
      examples: [
        'finite?(1.0)',
        'finite?(1 / 0)',
        'finite?(-1 / 0)',
      ],
    },
  },

  'positive-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.POSITIVE_INFINITY
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x equals positive infinity, otherwise `false`.',
      examples: [
        'positive-infinity?(1.0)',
        'positive-infinity?(1 / 0)',
        'positive-infinity?(-1 / 0)',
      ],
    },
  },

  'negative-infinity?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      return value === Number.NEGATIVE_INFINITY
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'number' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x equals negative infinity, otherwise `false`.',
      examples: [
        'negative-infinity?(1.0)',
        'negative-infinity?(1 / 0)',
        'negative-infinity?(-1 / 0)',
      ],
    },
  },

  'true?': {
    evaluate: ([value]): boolean => {
      return value === true
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is `true`, otherwise `false`.',
      examples: [
        'true?(false)',
        'true?(true)',
        'true?(1)',
        'true?(0)',
      ],
    },
  },

  'false?': {
    evaluate: ([value]): boolean => {
      return value === false
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is `true`, otherwise `false`.',
      examples: [
        'false?(false)',
        'false?(true)',
        'false?(1)',
        'false?(0)',
      ],
    },
  },

  'empty?': {
    evaluate: ([coll], sourceCodeInfo): boolean => {
      if (coll === null)
        return true

      assertColl(coll, sourceCodeInfo)
      if (typeof coll === 'string')
        return coll.length === 0

      if (Array.isArray(coll))
        return coll.length === 0

      return Object.keys(coll).length === 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: ['collection', 'string', 'null'] } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `true` if $x is empty or `null`, otherwise `false`.',
      examples: [
        'empty?([])',
        'empty?([1, 2, 3])',
        'empty?({})',
        'empty?({ a: 2 })',
        'empty?("")',
        'empty?("Albert")',
        'empty?(null)',
      ],
    },
  },
  'not-empty?': {
    evaluate: ([coll], sourceCodeInfo): boolean => {
      if (coll === null)
        return false

      assertColl(coll, sourceCodeInfo)
      if (typeof coll === 'string')
        return coll.length > 0

      if (Array.isArray(coll))
        return coll.length > 0

      return Object.keys(coll).length > 0
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      returns: { type: 'boolean' },
      args: { x: { type: ['collection', 'string', 'null'] } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns `false` if $x is empty or `null`, otherwise `true`.',
      examples: [
        'not-empty?([])',
        'not-empty?([1, 2, 3])',
        'not-empty?({})',
        'not-empty?({ a: 2 })',
        'not-empty?("")',
        'not-empty?("Albert")',
        'not-empty?(null)',
      ],
    },
  },
  'vector?': {
    evaluate: ([vector]): boolean => isVector(vector),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      description: 'Checks if a value is a `vector`. A `vector` is an array of `numbers`.',
      returns: { type: 'boolean' },
      args: { value: { type: 'any', description: 'The value to check.' } },
      variants: [{ argumentNames: ['value'] }],
      examples: [
        'vector?(1)',
        'vector?([1, 2, 3])',
        'vector?([1, 2, "3"])',
      ],
    },
  },
  'matrix?': {
    evaluate: ([matrix]): boolean => isMatrix(matrix),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      description: 'Checks if a value is a `matrix`. A `matrix` is an array of arrays of `numbers`.',
      returns: { type: 'boolean' },
      args: { value: { type: 'any', description: 'The value to check.' } },
      variants: [{ argumentNames: ['value'] }],
      examples: [
        'matrix?(1)',
        'matrix?([1, 2, 3])',
        'matrix?([[1, 2], [3, 4]])',
        'matrix?([[1, 2], [3, "4"]])',
        'matrix?([[1, 2], [3]])',
      ],
    },
  },
  'grid?': {
    evaluate: ([table]): boolean => isGrid(table),
    arity: toFixedArity(1),
    docs: {
      category: 'Predicate',
      description: 'Checks if a `value` is a `grid`. A `grid` is an `array` of `arrays` where all inner `arrays` have the same length.',
      returns: { type: 'boolean' },
      args: { value: { type: 'any', description: 'The value to check.' } },
      variants: [{ argumentNames: ['value'] }],
      examples: [
        'grid?("1")',
        'grid?(["1", 2, 3])',
        'grid?([["1", 2], [3, 4]])',
        'grid?([["1", 2], [3, "4"]])',
        'grid?([["1", 2], [3]])',
      ],
    },
  },

}
