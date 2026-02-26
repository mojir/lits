import '../../../src/initReferenceData'
import { moduleCategories } from '../../../reference/api'
import { styles } from '../styles'
import { renderExample } from '../renderExample'

export const moduleExamples = [
  'let { dot } = import(linear-algebra);\ndot([1, 2, 3], [4, 5, 6])',
  'let lin = import(linear-algebra);\n{\n "Cross product": lin.cross([1, 0, 0], [0, 1, 0]),\n  "Distance": lin.euclidean-distance([1, 1], [4, 5])\n}',
] as const

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
      <p ${styles('mb-4')}>
        Import a single function from a module:
      </p>
      <div ${styles('flex', 'flex-col', 'gap-4', 'bg-gray-700', 'p-4')} style="overflow-x: auto;">
        ${renderExample(moduleExamples[0], 'module-example-0')}
      </div>
      <p ${styles('mb-4')}>
        Import the entire module and access multiple functions:
      </p>
      <div ${styles('flex', 'flex-col', 'gap-4', 'bg-gray-700', 'p-4')} style="overflow-x: auto;">
        ${renderExample(moduleExamples[1], 'module-example-1')}
      </div>
      <p ${styles('text-color-gray-400')}>
        Available modules: ${moduleCategories.map((c) => {
          const ns = c
          return `<code ${styles('bg-gray-700', 'px-1')}>${ns}</code>`
        }).join(', ')}
      </p>
    </div>
  </div>
  `
}
