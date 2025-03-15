import { apiReference, isFunctionReference } from '../../../../reference'
import type { FunctionReference, Reference } from '../../../../reference'
import { styles } from '../../styles'
import { externalLinkIcon } from '../../icons'
import { getClojureDocsLink } from '../../../../common/clojureDocs'
import { formatDescription } from './description'
import { getFunctionExamples } from './functionExamples'
import { getArgumentInfo } from './argumentInfo'
import { getSection } from './section'
import { getFunctionSignature } from './functionSignature'

export function getAllDocumentationItems() {
  return Object.values(apiReference)
    .map(obj => getDocumentation(obj))
    .join('\n')
}

function getDocumentation(reference: Reference) {
  const { linkName, category } = reference
  const aliases = isFunctionReference(reference) ? reference.aliases : undefined
  const title = `${escapeTitle(reference.title)}${aliases ? `, ${aliases.join(', ')}` : ''}`

  const clojureDocsLink = getClojureDocsLink(reference.title, reference.clojureDocs)
  const functionReferences = reference.seeAlso?.map(apiName => apiReference[apiName])

  return `
  <div id="${linkName}" class="content function">
    <div ${styles('flex', 'justify-between', 'text-2xl', 'items-center', 'bg-gray-700', 'p-2', 'px-4')}">
      <div ${styles('text-color-gray-200', 'font-mono')}><a onclick="Playground.showPage('${reference.linkName}', 'smooth')">${title}</a></div>
      <div ${styles('text-color-gray-400')}>${category}</div>
    </div>

    ${isFunctionReference(reference) ? getSignature(reference) : `<div ${styles('mb-4')}></div>`}

    ${getSection('Description', formatDescription(reference.description, reference), 'mb-3', 'text-base')}

    ${functionReferences
      ? getSection(
          'See also',
          getSeeAlsoLinks(functionReferences),
          'my-3',
          'text-base',
          'text-color-gray-400',
        )
      : ''}

    ${
      clojureDocsLink
        ? `<a target="_blank" ${styles('flex', 'gap-1', 'items-center', 'text-sm', 'underline', 'mt-3', 'mb-6')} href="${clojureDocsLink}">
            <span ${styles('pt-1')}>${externalLinkIcon}</span>
            <span>Clojure docs</span>
          </a>`
        : `<div ${styles('height: 0.75rem;')}></div>`
    }


    ${isFunctionReference(reference) ? getSection('Arguments', getArgumentInfo(reference)) : ''}

    ${getSection('Examples', getFunctionExamples(reference))}

  </div>`
}

function getSignature(reference: FunctionReference) {
  return `<div ${styles('mb-6', 'mt-4', 'font-mono', 'text-base')}>
    ${getFunctionSignature(reference)}
    ${reference.aliases
      ? `<div ${styles('text-base', 'font-sans', 'mt-3', 'mb-1')}>${reference.aliases.length === 1 ? 'Alias' : 'Aliases'}</div>
          ${reference.aliases.map(alias =>
            getFunctionSignature({ ...reference, title: alias }),
          ).join('')}`
      : ''}
  </div>`
}

function getSeeAlsoLinks(references: Reference[]) {
  return `<div ${styles('flex', 'flex-col')}>
    ${references.map((reference) => {
      return `<a onclick="Playground.showPage('${reference.linkName}', 'smooth')"><span>${escapeTitle(reference.title)}</span></a>`
    }).join('')}
  </div>`
}

function escapeTitle(title: string) {
  return title.replace(/"/g, '&quot;')
}
