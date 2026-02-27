import '../../../src/initReferenceData'
import { coreCategories } from '../../../reference/api'
import { styles } from '../styles'
import { renderExample } from '../renderExample'

export const corePageExamples = [
  '1 + 2 + 3',
  'upper-case("hello, world!")',
  'map([1, 2, 3], inc)',
  'if 3 > 2 then "yes" else "no" end',
]

export function getCorePage(): string {
  return `
  <div id="core-page" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl', 'mb-6')}>Core Functions</div>

    <div ${styles('mb-6', 'p-4', 'bg-gray-800', 'text-color-gray-300')}>
      <p ${styles('mb-4')}>
        Core functions are <strong>always available</strong> in Lits — no imports needed.
        They cover the fundamental building blocks of the language: control flow, data manipulation,
        math, string handling, predicates, and more.
      </p>
      <p ${styles('mb-4')}>
        Core functions are organized into the following categories:
      </p>
      <div ${styles('flex', 'flex-wrap', 'gap-2', 'mb-4')}>
        ${coreCategories.map(c => `<code ${styles('bg-gray-700', 'px-1')}>${c}</code>`).join(' ')}
      </div>
      <p ${styles('mb-4')}>
        Here are a few examples to get started:
      </p>
      ${corePageExamples.map((example, index) => renderExample(example, `core-example-${index}`)).join('\n')}
    </div>
  </div>
  `
}
