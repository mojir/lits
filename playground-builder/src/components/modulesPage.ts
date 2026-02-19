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
    name: 'Vector',
    importName: 'Vector',
    description: 'Statistical and mathematical operations on vectors (arrays of numbers). Includes functions for sum, mean, median, standard deviation, and more.',
    examples: [
      'let vec = import("Vector"); vec.sum([1, 2, 3, 4, 5])',
      'let vec = import("Vector"); vec.mean([1, 2, 3, 4, 5])',
      'let vec = import("Vector"); vec.stdev([1, 2, 3, 4, 5])',
    ],
  },
  {
    name: 'Linear Algebra',
    importName: 'Linear-Algebra',
    description: 'Linear algebra operations including dot product, cross product, and vector norms.',
    examples: [
      'let lin = import("Linear-Algebra"); lin.dot([1, 2, 3], [4, 5, 6])',
      'let lin = import("Linear-Algebra"); lin.cross([1, 0, 0], [0, 1, 0])',
      'let lin = import("Linear-Algebra"); lin.norm([3, 4])',
    ],
  },
  {
    name: 'Matrix',
    importName: 'Matrix',
    description: 'Matrix operations including creation, manipulation, and mathematical operations like determinant, transpose, and multiplication.',
    examples: [
      'let mat = import("Matrix"); mat.identity(3)',
      'let mat = import("Matrix"); mat.transpose([[1, 2], [3, 4]])',
      'let mat = import("Matrix"); mat.det([[1, 2], [3, 4]])',
    ],
  },
  {
    name: 'Grid',
    importName: 'Grid',
    description: 'Grid (2D array) operations for creating and manipulating two-dimensional data structures.',
    examples: [
      'let grid = import("Grid"); grid.create(3, 3, 0)',
      'let grid = import("Grid"); grid.get(grid.create(3, 3, :x), 1, 1)',
    ],
  },
  {
    name: 'Number-Theory',
    importName: 'Number-Theory',
    description: 'Number theory functions including prime numbers, factorials, fibonacci sequences, and more.',
    examples: [
      'let nt = import("Number-Theory"); nt.prime?(17)',
      'let nt = import("Number-Theory"); nt.fibonacci-seq(10)',
      'let nt = import("Number-Theory"); nt.factorial(5)',
    ],
  },
  {
    name: 'Random',
    importName: 'Random',
    description: 'Random number generation and related functions.',
    examples: [
      'let rand = import("Random"); rand.random!()',
      'let rand = import("Random"); rand.random-int!(1, 100)',
      'let rand = import("Random"); rand.shuffle!([1, 2, 3, 4, 5])',
    ],
  },
  {
    name: 'Asseert',
    importName: 'Assert',
    description: 'Assertion functions for validating conditions and throwing errors.',
    examples: [
      'let { assert } = import("Assert");\ntry assert(0, "Expected a positive value") catch (e) e.message end',
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
let vec = import("Vector")
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
