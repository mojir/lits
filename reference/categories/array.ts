import { type ArrayApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const arrayReference: Record<ArrayApiName, FunctionReference<'Array'>> = {
  'range': {
    title: 'range',
    category: 'Array',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      ...getOperatorArgs('number', 'number'),
      step: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['b'] },
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'step'] },
    ],
    description: `$range creates an array with a range of numbers from $a to $b (exclusive), by $step.

$a defaults to 0.  
$step defaults to 1.`,
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
  'repeat': {
    title: 'repeat',
    category: 'Array',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      ...getOperatorArgs('any', 'integer'),
    },
    variants: [{
      argumentNames: ['a', 'b'],
    }],
    description: 'Returns an array with $a repeated $b times.',
    examples: [
      'repeat(10, 3)',
      'repeat(10, 0)',
      '"Albert" repeat 5',
    ],
  },
  'flatten': {
    title: 'flatten',
    category: 'Array',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      x: {
        type: ['array', 'any'],
        description: 'If $x is not an array, `[ ]` is returned.',
      },
    },
    variants: [{
      argumentNames: ['x'],
    }],
    description: 'Takes a nested array $x and flattens it.',
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
    noOperatorDocumentation: true,
  },
  'mapcat': {
    title: 'mapcat',
    category: 'Array',
    returns: {
      type: 'collection',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
      colls: {
        type: 'collection',
        array: true,
      },
      fun: {
        type: 'function',
      },
    },
    variants: [{
      argumentNames: ['colls', 'fun'],
    }],
    description: 'Returns the result of applying concat to the result of applying map to $fun and $colls.',
    examples: [
      '[[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]] mapcat reverse',
      'mapcat([[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]], reverse)',
      '[[3, 2, 1, 0,], [6, 5, 4,], [9, 8, 7]] mapcat reverse',
      `
let foo = (n) -> {
  [n - 1, n, n + 1]
};
[1, 2, 3] mapcat foo`,
      `
mapcat(
  [[1, 2], [2, 2], [2, 3]],
  -> $ remove even?
)`,
    ],
  },
  'moving-fn': {
    title: 'moving-fn',
    category: 'Array',
    returns: {
      type: 'array',
    },
    args: {
      arr: {
        type: 'array',
      },
      windowSize: {
        type: 'number',
        description: 'The size of the moving window.',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [{
      argumentNames: ['arr', 'windowSize', 'fn'],
    }],
    description: 'Returns the result of applying $fn to each moving window of size $windowSize in $arr.',
    examples: [
      'let v = import("Vector"); moving-fn([1, 2, 3], 2, v.sum)',
      'let v = import("Vector"); moving-fn([1, 2, 3], 1, v.sum)',
      'let v = import("Vector"); moving-fn([1, 2, 3], 3, v.sum)',
    ],
  },
  'running-fn': {
    title: 'running-fn',
    category: 'Array',
    returns: {
      type: 'array',
    },
    args: {
      a: {
        type: 'array',
      },
      b: {
        type: 'function',
      },
    },
    variants: [{
      argumentNames: ['a', 'b'],
    }],
    description: 'Returns the result of applying $b to each element of $a.',
    examples: [
      'let v = import("Vector"); running-fn([1, 2, 3], v.sum)',
      'let v = import("Vector"); running-fn([1, 2, 3], v.max)',
      'let v = import("Vector"); running-fn([1, 2, 3], v.min)',
    ],
  },
}
