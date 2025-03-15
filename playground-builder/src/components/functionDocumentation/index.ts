import { apiReference, isCustomReference, isFunctionReference } from '../../../../reference'
import type { CustomReference, FunctionReference, Reference } from '../../../../reference'
import { styles } from '../../styles'
import { formatLitsExpression } from '../../formatter/rules'
import { formatDescription } from './description'
import { getFunctionExamples } from './functionExamples'
import { getArgumentInfo } from './argumentInfo'
import { getSection } from './section'
import { getFunctionSignature } from './functionSignature'
import { getCustomSignature } from './customSignature'

export function getAllDocumentationItems() {
  return Object.values(apiReference)
    .map(obj => getDocumentation(obj))
    .join('\n')
}

function getDocumentation(reference: Reference) {
  const { linkName, category } = reference
  const aliases = isFunctionReference(reference) ? reference.aliases : undefined
  const title = `${escapeTitle(reference.title)}${aliases ? `, ${aliases.join(', ')}` : ''}`

  const functionReferences = reference.seeAlso?.map(apiName => apiReference[apiName])

  return `
  <div id="${linkName}" class="content function">
    <div ${styles('flex', 'justify-between', 'text-2xl', 'items-center', 'bg-gray-700', 'p-2', 'px-4')}">
      <div ${styles('text-color-gray-200', 'font-mono')}><a onclick="Playground.showPage('${reference.linkName}', 'smooth')">${title}</a></div>
      <div ${styles('text-color-gray-400')}>${category}</div>
    </div>

    ${isFunctionReference(reference)
      ? getSignature(reference)
      : isCustomReference(reference)
        ? getCustomSignatureSection(reference)
        : `<div ${styles('mb-4')}></div>`}

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

    ${isFunctionReference(reference) ? getSection('Arguments', getArgumentInfo(reference)) : ''}
    ${isCustomReference(reference) && reference.details ? getSection('Details', getDetailsTable(reference.details)) : ''}

    ${getSection('Examples', getFunctionExamples(reference))}

  </div>`
}

export function getDetailsTable(content: [string, string, string | undefined][]): string {
  return `<table>
    ${content.map(row => `
    <tr>
      <td ${styles('text-color-Rose')}>${row[0]}</td>
      <td ${styles('pl-8')}>${formatLitsExpression(row[1])}</td>
      ${row[2] ? `<td ${styles('pl-4', 'italic', 'text-base')}>${formatDescription(row[2])}</td>` : ''}
    </tr>`,
    ).join('')}
  </table>`
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

function getCustomSignatureSection(reference: CustomReference) {
  return `<div ${styles('mb-6', 'mt-4', 'font-mono', 'text-base')}>
    ${getCustomSignature(reference.customVariants)}
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
