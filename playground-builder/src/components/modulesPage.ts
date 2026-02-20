import { moduleCategories } from '../../../reference/api'
import { formatLitsExpression } from '../formatter/rules'
import { packageIcon } from '../icons'
import { styles } from '../styles'

interface ModuleInfo {
  name: string
  importName: string
  description: string
  examples: string[]
}

const moduleInfo: ModuleInfo[] = [
  {
    name: 'vector',
    importName: 'vector',
    description: 'Statistical and mathematical operations on vectors (arrays of numbers). Includes functions for sum, mean, median, standard deviation, and more.',
    examples: [
      'let vec = import("vector"); vec.sum([1, 2, 3, 4, 5])',
      'let vec = import("vector"); vec.mean([1, 2, 3, 4, 5])',
      'let vec = import("vector"); vec.stdev([1, 2, 3, 4, 5])',
    ],
  },
  {
    name: 'linear-algebra',
    importName: 'linear-algebra',
    description: 'Linear algebra operations including dot product, cross product, and vector norms.',
    examples: [
      'let lin = import("linear-algebra"); lin.dot([1, 2, 3], [4, 5, 6])',
      'let lin = import("linear-algebra"); lin.cross([1, 0, 0], [0, 1, 0])',
      'let lin = import("linear-algebra"); lin.norm([3, 4])',
    ],
  },
  {
    name: 'matrix',
    importName: 'matrix',
    description: 'Matrix operations including creation, manipulation, and mathematical operations like determinant, transpose, and multiplication.',
    examples: [
      'let mat = import("matrix"); mat.identity(3)',
      'let mat = import("matrix"); mat.transpose([[1, 2], [3, 4]])',
      'let mat = import("matrix"); mat.det([[1, 2], [3, 4]])',
    ],
  },
  {
    name: 'grid',
    importName: 'grid',
    description: 'Grid (2D array) operations for creating and manipulating two-dimensional data structures.',
    examples: [
      'let grid = import("grid"); grid.create(3, 3, 0)',
      'let grid = import("grid"); grid.get(grid.create(3, 3, :x), 1, 1)',
    ],
  },
  {
    name: 'number-theory',
    importName: 'number-theory',
    description: 'Number theory functions including prime numbers, factorials, fibonacci sequences, and more.',
    examples: [
      'let nt = import("number-theory"); nt.prime?(17)',
      'let nt = import("number-theory"); nt.fibonacci-seq(10)',
      'let nt = import("number-theory"); nt.factorial(5)',
    ],
  },
  {
    name: 'random',
    importName: 'random',
    description: 'Random number generation and related functions.',
    examples: [
      'let rand = import("random"); rand.random!()',
      'let rand = import("random"); rand.random-int!(1, 100)',
      'let rand = import("random"); rand.shuffle!([1, 2, 3, 4, 5])',
    ],
  },
  {
    name: 'assert',
    importName: 'assert',
    description: 'Assertion functions for validating conditions and throwing errors.',
    examples: [
      'let { assert } = import("assert");\ntry assert(0, "Expected a positive value") catch (e) e.message end',
    ],
  },
  {
    name: 'math',
    importName: 'math',
    description: 'Trigonometric, logarithmic, and angle conversion functions. Operates on numbers, vectors, and matrices element-wise.',
    examples: [
      'let { sin, cos } = import("math"); [sin(PI / 2), cos(0)]',
      'let { ln } = import("math"); ln(E)',
      'let { to-rad } = import("math"); to-rad(180)',
    ],
  },
  {
    name: 'functional',
    importName: 'functional',
    description: 'Higher-order functional utilities including juxt, complement, and predicate combinators.',
    examples: [
      'let { juxt } = import("functional"); juxt(inc, dec)(10)',
      'let { complement } = import("functional"); (complement(zero?))(1)',
    ],
  },
  {
    name: 'string',
    importName: 'string',
    description: 'Extended string operations: padding, encoding, char codes, templates, and more.',
    examples: [
      'let { pad-left } = import("string"); pad-left("42", 5, "0")',
      // eslint-disable-next-line no-template-curly-in-string
      'let { template } = import("string"); template("Hello, ${name}!", { name: "World" })',
    ],
  },
  {
    name: 'collection',
    importName: 'collection',
    description: 'Extended collection operations: indexed mapping, deep access, reduction variants, and predicate helpers.',
    examples: [
      'let { get-in } = import("collection"); get-in({ a: { b: 42 } }, ["a", "b"])',
      'let { every? } = import("collection"); every?(pos?, [1, 2, 3])',
    ],
  },
  {
    name: 'sequence',
    importName: 'sequence',
    description: 'Extended sequence operations: take, drop, partition, grouping, distinct, and more.',
    examples: [
      'let { take } = import("sequence"); take(3, [1, 2, 3, 4, 5])',
      'let { partition } = import("sequence"); partition(2, [1, 2, 3, 4, 5, 6])',
    ],
  },
  {
    name: 'bitwise',
    importName: 'bitwise',
    description: 'Additional bitwise operations: bit-not, bit-flip, bit-test, and more.',
    examples: [
      'let { bit-not } = import("bitwise"); bit-not(0)',
      'let { bit-test } = import("bitwise"); bit-test(6, 1)',
    ],
  },
]

export function getModulesPage(): string {
  return `
  <div id="modules-page" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl', 'mb-6')}>Modules</div>
    
    <div ${styles('mb-6', 'p-4', 'bg-gray-800', 'text-color-gray-300')}>
      <p ${styles('mb-4')}>
        Lits organizes additional functionality into <strong>modules</strong>. 
        Unlike core functions which are always available, module functions must be 
        imported before use.
      </p>
      <p ${styles('mb-4')}>
        To use a module, use the <code ${styles('bg-gray-700', 'px-1')}>import</code> special expression 
        to load the module, then call its functions using dot notation:
      </p>
      <pre ${styles('bg-gray-700', 'p-4', 'mb-4')} style="overflow-x: auto;">${formatLitsExpression(`; Import the Vector module
let vec = import("vector")
; Use vec.sum, vec.mean, etc.
vec.sum([1, 2, 3, 4, 5])`)}</pre>
      <p ${styles('text-color-gray-400')}>
        Available modules: ${moduleCategories.map((c) => {
          const ns = c
          return `<code ${styles('bg-gray-700', 'px-1')}>${ns}</code>`
        }).join(', ')}
      </p>
    </div>

    <div ${styles('flex', 'flex-col', 'gap-6', 'my-4')}>
    ${moduleInfo
      .map((ns) => {
        return `
          <div ${styles('flex', 'flex-col', 'bg-gray-700')}>
            <div ${styles('select-none', 'text-color-gray-400', 'flex', 'flex-row', 'items-center', 'pr-4')}>
              <div ${styles('font-size: 3.5rem;', 'flex', 'items-center', 'justify-center', 'width: 5rem;')}>${packageIcon}</div>
              <div ${styles('flex', 'flex-1', 'flex-col', 'gap-2', 'my-4')}>
                <div ${styles('text-xl', 'text-color-gray-200')}>
                  ${ns.name}
                  <code ${styles('ml-2', 'text-base', 'text-color-gray-400')}>(import "${ns.importName}")</code>
                </div>
                <div ${styles('text-base', 'text-color-gray-400')}>${ns.description}</div>
              </div>
            </div>
            <div ${styles('p-4', 'bg-gray-800', 'flex', 'flex-col', 'gap-2')}>
              ${ns.examples.map(example => `<pre style="overflow-x: auto;">${formatLitsExpression(example)}</pre>`).join('\n')}
            </div>
          </div>
        `
      })
      .join('\n')}
    </div>
  </div>
  `
}
