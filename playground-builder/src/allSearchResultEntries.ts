import { type Reference, apiReference, getLinkName, isFunctionReference } from '../../reference'
import { formatDescription } from './components/functionDocumentation/description'
import { getFunctionSignature } from './components/functionDocumentation/functionSignature'
import { styles } from './styles'

const shortDescriptionRegExp = /(.*?) {2}\n|\n\n|$/
export interface SearchResultEntry {
  title: string
  search: string
  html: string
}

const searchables: Reference[] = Object
  .values(apiReference)
  .sort((a, b) => a.title.localeCompare(b.title))

export const allSearchResultEntries: SearchResultEntry[] = searchables.map((reference) => {
  const match = shortDescriptionRegExp.exec(reference.description)
  const description = match?.[1] ?? reference.description

  const aliases = isFunctionReference(reference) ? reference.aliases?.join(' ') ?? '' : ''

  return {
    title: reference.title,
    search: `${reference.title.replace(/&quot;/g, '"')} ${reference.category} ${aliases}`,
    html: getHtml(description, reference),
  }
})

function getHtml(description: string, reference: Reference) {
  const title = escapeTitle(reference.title)

  return `
    <div onclick="Playground.showPage('${getLinkName(reference)}', 'smooth')" class="search-entry" ${styles('w-full', 'flex', 'flex-col', 'p-4', 'scroll-my-4', 'cursor-pointer', 'min-height: 12rem;')}>
      <div ${styles('mb-4', 'flex', 'justify-between', 'items-baseline')}>
        <div ${styles('text-2xl', 'font-bold', 'text-color-gray-300')}>${title}</div>
        <div ${styles('text-base', 'text-color-gray-400')}>${reference.category}</div>
      </div>
      ${isFunctionReference(reference)
        ? `
          <div ${styles('text-base', 'mb-4')}>
            ${getFunctionSignature(reference)}
                ${reference.aliases
                  ? `<div ${styles('text-base', 'font-sans', 'mt-3', 'mb-1', 'text-color-white')}>${reference.aliases.length === 1 ? 'Alias' : 'Aliases'}</div>
                    ${reference.aliases.map(alias =>
                      getFunctionSignature({ ...reference, title: alias }),
                    ).join('')}`
                  : ''}

          </div>`
        : ''}
      <div ${styles('text-base')}>
        ${formatDescription(description, reference)}
      </div>
    </div>
  `
}

function escapeTitle(title: string) {
  return title.replace(/"/g, '&quot;')
}
