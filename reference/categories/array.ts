import type { ArrayApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

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
      '(array 1 2 3)',
      '(array (array nil false true))',
      '[]',
      '[1 2 3]',
      '[[nil false true]]',
      '[]',
      '([1 2 3] 1)',
      '([1 2 3 4 5 6 7 8 9] 3)',
    ],
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
      end: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['end'] },
      { argumentNames: ['start', 'end'] },
      { argumentNames: ['start', 'end', 'step'] },
    ],
    description: `$range creates an array with a range of numbers from $start to $end (exclusive), by $step.

$start defaults to 0.  
$step defaults to 1.`,
    examples: [
      '(range 4)',
      '(range 1 4)',
      '(range 0.4 4.9)',
      `
(range
  0.25 ;; start value
  1    ;; end value (exclusive)
  0.25 ;; step value
)`,
    ],
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
    },
    variants: [{
      argumentNames: ['x', 'n'],
    }],
    description: 'Returns an array with $x repeated $n times.',
    examples: [
      '(repeat 10 3)',
      '(repeat 10 0)',
      '(repeat "Albert" 5)',
    ],
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
      '(flatten [1 2 [3 4] 5])',
      `
(let [foo :bar]
  (flatten
    [1
     " 2 A "
     [foo [4 [:ABC]]] 6]))`,

      '(flatten 12)',
    ],
  },
  mapcat: {
    title: 'mapcat',
    category: 'Array',
    linkName: 'mapcat',
    returns: {
      type: 'collection',
    },
    args: {
      f: {
        type: 'function',
      },
      colls: {
        type: 'collection',
        array: true,
      },
    },
    variants: [{
      argumentNames: ['f', 'colls'],
    }],
    description: 'Returns the result of applying concat to the result of applying map to $f and $colls.',
    examples: [
      '(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])',
      '(mapcat reverse [[3 2 1 0] [6 [5] 4] [9 8 7]])',
      '(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat foo [1 2 3])',
      '(mapcat #(remove even? %1) [[1 2] [2 2] [2 3]])',
    ],
  },
}
