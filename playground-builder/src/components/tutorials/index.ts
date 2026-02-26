import { renderExample } from '../../renderExample'
import { styles } from '../../styles'
import { createFormatter } from '../../../../common/createFormatter'
import { mdRules } from '../../formatter/rules'
import { gettingStartedTutorial } from './gettingStarted'
import { functionsTutorial } from './functions'
import { dataTypesTutorial } from './dataTypes'
import { operatorsTutorial } from './operators'
import { collectionsTutorial } from './collections'
import { destructuringTutorial } from './destructuring'
import { controlFlowTutorial } from './controlFlow'
import { loopsRecursionTutorial } from './loopsRecursion'
import { pipesDataFlowTutorial } from './pipesDataFlow'
import { modulesTutorial } from './modules'

// --- Page element types ---

export interface HeaderElement {
  type: 'header'
  text: string
}

export interface ParagraphElement {
  type: 'paragraph'
  text: string
}

export interface ExampleElement {
  type: 'example'
  code: string[]
}

export type TutorialPageElement = HeaderElement | ParagraphElement | ExampleElement

export interface TutorialEntry {
  id: string
  title: string
  elements: TutorialPageElement[]
}

// --- Helpers ---

export function getExamples(tutorial: TutorialEntry): string[][] {
  return tutorial.elements
    .filter((el): el is ExampleElement => el.type === 'example')
    .map(el => el.code)
}

const formatParagraph = createFormatter(mdRules)

function renderTutorialPage(tutorial: TutorialEntry): string {
  let exampleIndex = 0
  const body = tutorial.elements.map((el) => {
    switch (el.type) {
      case 'header':
        return `<h3 ${styles('text-xl', 'mb-2', 'text-color-gray-200')}>${el.text}</h3>`
      case 'paragraph':
        return `<p ${styles('mb-4')}>${formatParagraph(el.text)}</p>`
      case 'example': {
        const name = `${tutorial.id}-${exampleIndex++}`
        return `<div ${styles('flex', 'flex-col', 'gap-4', 'bg-gray-700', 'p-4', 'mb-4')} style="overflow-x: auto;">${renderExample(el.code, name)}</div>`
      }
      default:
        throw new Error(`Unknown element type: ${(el)}`)
    }
  }).join('\n')

  return `
  <div id="${tutorial.id}" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl', 'mb-6')}>${tutorial.title}</div>
    <div ${styles('mb-6', 'p-4', 'bg-gray-800', 'text-color-gray-300')}>
      ${body}
    </div>
  </div>
  `
}

// --- Registry ---

export const tutorials: TutorialEntry[] = [
  gettingStartedTutorial,
  functionsTutorial,
  dataTypesTutorial,
  operatorsTutorial,
  collectionsTutorial,
  destructuringTutorial,
  controlFlowTutorial,
  loopsRecursionTutorial,
  pipesDataFlowTutorial,
  modulesTutorial,
]

export function getAllTutorialPages(): string {
  return tutorials.map(renderTutorialPage).join('\n')
}
