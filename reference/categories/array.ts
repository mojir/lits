import { type ArrayApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const arrayReference: Record<ArrayApiName, FunctionReference<'Array'>> = {
  array: {
    title: 'array',
    category: 'Array',
    linkName: 'array',
    clojureDocs: 'vector',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['values'] },
    ],
    description: 'Makes new array from $values.',
    examples: [
      'array(1, 2, 3)',
      'array(array(null, false, true))',
      '[]',
      '[1, 2, 3]',
      '[[null, false, true]]',
      '[1, 2, 3][1]',
    ],
    noOperatorDocumentation: true,
    algebraic: true,
  },
  range: {
    title: 'range',
    category: 'Array',
    linkName: 'range',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      start: {
        type: 'number',
      },
      stop: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['stop'] },
      { argumentNames: ['start', 'stop'] },
      { argumentNames: ['start', 'stop', 'step'] },
    ],
    description: `$range creates an array with a range of numbers from $start to $stop (exclusive), by $step.

$start defaults to 0.  
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
    algebraic: true,
  },
  repeat: {
    title: 'repeat',
    category: 'Array',
    linkName: 'repeat',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      x: {
        type: 'any',
      },
      n: {
        type: 'integer',
      },
      ...getOperatorArgs('any', 'integer'),
    },
    variants: [{
      argumentNames: ['x', 'n'],
    }],
    description: 'Returns an array with $x repeated $n times.',
    examples: [
      'repeat(10, 3)',
      'repeat(10, 0)',
      '"Albert" repeat 5',
    ],
    algebraic: true,
  },
  flatten: {
    title: 'flatten',
    category: 'Array',
    linkName: 'flatten',
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

      'flatten(12)',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  mapcat: {
    title: 'mapcat',
    category: 'Array',
    linkName: 'mapcat',
    returns: {
      type: 'collection',
    },
    args: {
      fn: {
        type: 'function',
      },
      colls: {
        type: 'collection',
        array: true,
      },
    },
    variants: [{
      argumentNames: ['colls', 'fn'],
    }],
    description: 'Returns the result of applying concat to the result of applying map to $fn and $colls.',
    examples: [
      'mapcat([[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]], reverse)',
      '[[3, 2, 1, 0,], [6, 5, 4,], [9, 8, 7]] mapcat reverse',
      `
function foo(n)
  [n - 1, n, n + 1]
end;
[1, 2, 3] mapcat foo`,
      `
mapcat(
  [[1, 2], [2, 2], [2, 3]],
  => $ remove even?
)`,
    ],
    algebraic: true,
  },
}
