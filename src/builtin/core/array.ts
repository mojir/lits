import type { Arr } from '../../interface'
import { assertArray } from '../../typeGuards/array'
import { asNumber, assertNumber } from '../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../interface'
import { assertFunctionLike } from '../../typeGuards/lits'
import { toFixedArity } from '../../utils/arity'

export const arrayNormalExpression: BuiltinNormalExpressions = {
  'range': {
    evaluate: (params, sourceCodeInfo): Arr => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertNumber(first, sourceCodeInfo, { finite: true })

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      }
      else if (params.length === 2) {
        assertNumber(second, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = to >= from ? 1 : -1
      }
      else {
        assertNumber(second, sourceCodeInfo, { finite: true })
        assertNumber(third, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = third
        if (to > from)
          assertNumber(step, sourceCodeInfo, { positive: true })
        else if (to < from)
          assertNumber(step, sourceCodeInfo, { negative: true })
        else
          assertNumber(step, sourceCodeInfo, { nonZero: true })
      }

      const result: number[] = []

      for (let i = from; step < 0 ? i > to : i < to; i += step)
        result.push(i)

      return result
    },
    arity: { min: 1, max: 3 },
    docs: {
      category: 'array',
      returns: { type: 'number', array: true },
      args: {
        a: { type: 'number' },
        b: { type: 'number' },
        step: { type: 'number' },
      },
      variants: [
        { argumentNames: ['b'] },
        { argumentNames: ['a', 'b'] },
        { argumentNames: ['a', 'b', 'step'] },
      ],
      description: `$range creates an array with a range of numbers from $a to $b (exclusive), by $step.

$a defaults to 0.
$step defaults to 1.`,
      seeAlso: ['repeat', 'vector.linspace'],
      examples: [
        'range(4)',
        'range(1, 4)',
        '1 range 10',
        'range(0.4, 4.9)',
        `
range(
  0.25, // start value
  1,    // end value (exclusive)
  0.25, // step value
)`,
      ],
    },
  },

  'repeat': {
    evaluate: ([value, count], sourceCodeInfo): Arr => {
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })
      const result: Arr = []
      for (let i = 0; i < count; i += 1)
        result.push(value)

      return result
    },
    arity: toFixedArity(2),
    docs: {
      category: 'array',
      returns: { type: 'any', array: true },
      args: {
        a: { type: 'any' },
        b: { type: 'integer' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns an array with $a repeated $b times.',
      seeAlso: ['range', 'vector.ones', 'vector.zeros', 'vector.fill', 'vector.generate', 'string.string-repeat'],
      examples: [
        'repeat(10, 3)',
        'repeat(10, 0)',
        '"Albert" repeat 5',
      ],
    },
  },

  'flatten': {
    evaluate: ([seq, depth], sourceCodeInfo): Arr => {
      assertArray(seq, sourceCodeInfo)

      const actualDepth = depth === undefined || depth === Number.POSITIVE_INFINITY
        ? Number.POSITIVE_INFINITY
        : asNumber(depth, sourceCodeInfo, { integer: true, nonNegative: true })

      return seq.flat(actualDepth)
    },
    arity: { min: 1, max: 2 },
    docs: {
      category: 'array',
      returns: { type: 'any', array: true },
      args: {
        x: { type: ['array', 'any'], description: 'If $x is not an array, `[ ]` is returned.' },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'Takes a nested array $x and flattens it.',
      seeAlso: ['mapcat'],
      examples: [
        'flatten([1, 2, [3, 4], 5])',
        `
let foo = "bar";
flatten([
  1,
  " 2 A ",
  [foo, [4, ["ABC"]]],
  6,
])`,
      ],
      hideOperatorForm: true,
    },
  },
  'mapcat': {
    evaluate: ([arr, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr | string => {
      assertArray(arr, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      return arr.map(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)).flat(1)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'array',
      returns: { type: 'collection' },
      args: {
        a: { type: 'collection' },
        b: { type: 'function' },
        colls: { type: 'collection', array: true },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['colls', 'fun'] }],
      description: 'Returns the result of applying concat to the result of applying map to $fun and $colls.',
      seeAlso: ['flatten', 'map', '++'],
      examples: [
        '[[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]] mapcat reverse',
        'mapcat([[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]], reverse)',
        '[[3, 2, 1, 0,], [6, 5, 4,], [9, 8, 7]] mapcat reverse',
        `
let foo = (n) -> do
  [n - 1, n, n + 1]
end;
[1, 2, 3] mapcat foo`,
        `
mapcat(
  [[1, 2], [2, 2], [2, 3]],
  -> $ filter odd?
)`,
      ],
    },
  },
  'moving-fn': {
    evaluate: ([arr, windowSize, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr => {
      assertArray(arr, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, lte: arr.length })
      assertFunctionLike(fn, sourceCodeInfo)

      const result = []
      for (let i = 0; i <= arr.length - windowSize; i++) {
        const window = arr.slice(i, i + windowSize)
        const value = executeFunction(fn, [window], contextStack, sourceCodeInfo)
        result.push(value)
      }
      return result
    },
    arity: toFixedArity(3),
    docs: {
      category: 'array',
      returns: { type: 'array' },
      args: {
        arr: { type: 'array' },
        windowSize: { type: 'number', description: 'The size of the moving window.' },
        fn: { type: 'function' },
      },
      variants: [{ argumentNames: ['arr', 'windowSize', 'fn'] }],
      description: 'Returns the result of applying $fn to each moving window of size $windowSize in $arr.',
      seeAlso: ['running-fn', 'vector.moving-mean'],
      examples: [
        'let v = import("vector"); moving-fn([1, 2, 3], 2, v.sum)',
        'let v = import("vector"); moving-fn([1, 2, 3], 1, v.sum)',
        'let v = import("vector"); moving-fn([1, 2, 3], 3, v.sum)',
      ],
    },
  },
  'running-fn': {
    evaluate: ([arr, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr => {
      assertArray(arr, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      const result = []
      for (let i = 0; i < arr.length; i += 1) {
        const subArr = arr.slice(0, i + 1)
        result.push(executeFunction(fn, [subArr], contextStack, sourceCodeInfo))
      }
      return result
    },
    arity: toFixedArity(2),
    docs: {
      category: 'array',
      returns: { type: 'array' },
      args: {
        a: { type: 'array' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns the result of applying $b to each element of $a.',
      seeAlso: ['moving-fn', 'vector.running-mean'],
      examples: [
        'let v = import("vector"); running-fn([1, 2, 3], v.sum)',
        'let v = import("vector"); running-fn([1, 2, 3], v.max)',
        'let v = import("vector"); running-fn([1, 2, 3], v.min)',
      ],
    },
  },
}
