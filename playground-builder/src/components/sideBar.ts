import type { Reference } from '../../../reference'
import { apiReference, getLinkName, isFunctionReference, namespaceReference } from '../../../reference'
import { categoryToNamespace, coreCategories, namespaceCategories } from '../../../reference/api'
import { chevronRightIcon, homeIcon, lampIcon, packageIcon, searchIcon } from '../icons'
import { styles } from '../styles'

export function getSideBar() {
  const categoryCollections = Object.values(apiReference).reduce((result: Record<string, Reference[]>, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category]!.push(obj)
    return result
  }, {})

  const namespaceCategoryCollections = Object.values(namespaceReference).reduce((result: Record<string, Reference[]>, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category]!.push(obj)
    return result
  }, {})

  const renderCategory = (categoryKey: string, collections: Record<string, Reference[]>) => {
    return `
      <div ${styles('flex', 'flex-col', 'gap-1')}>
        <div ${styles('text-color-gray-200')}>
          ${categoryKey}
        </div>
        <div ${styles('flex', 'flex-col', 'ml-2', 'text-color-gray-400', 'text-base')}>
          ${
            collections[categoryKey]
              ? collections[categoryKey]
                  .sort((a, b) => {
                    const aSpecial = a.title[0]!.match(/[^a-z]/i)
                    const bSpecial = b.title[0]!.match(/[^a-z]/i)
                    if (aSpecial && !bSpecial)
                      return -1
                    if (!aSpecial && bSpecial)
                      return 1
                    return (a.title < b.title ? -1 : a.title > b.title ? 1 : 0)
                  })
                  .map((obj) => {
                    const linkName = getLinkName(obj)
                    const aliases = isFunctionReference(obj) ? obj.aliases : undefined
                    const name = `${escape(obj.title)}${aliases ? `, ${aliases.join(', ')}` : ''}`
                    return `<a id="${linkName}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${linkName}', 'smooth')">${name}</a>`
                  })
                  .join('\n')
              : ''
          }
        </div>
      </div>`
  }

  const renderNamespaceCategory = (categoryKey: string) => {
    const nsName = categoryToNamespace[categoryKey as keyof typeof categoryToNamespace] || categoryKey.toLowerCase()
    return `
      <div ${styles('flex', 'flex-col', 'gap-1')}>
        <div 
          ${styles('text-color-gray-200', 'flex', 'items-center', 'gap-1', 'cursor-pointer')}
          onclick="Playground.toggleNamespaceCategory('${categoryKey}')"
        >
          <span id="ns-chevron-${categoryKey.replace(/\s+/g, '-')}" class="ns-chevron">${chevronRightIcon}</span>
          <span>${categoryKey}</span>
          <span ${styles('text-xs', 'text-color-gray-500')}>(${nsName})</span>
        </div>
        <div 
          id="ns-content-${categoryKey.replace(/\s+/g, '-')}" 
          ${styles('flex-col', 'ml-2', 'text-color-gray-400', 'text-base', 'display: none;')}
        >
          ${
            namespaceCategoryCollections[categoryKey]
              ? namespaceCategoryCollections[categoryKey]
                  .sort((a, b) => {
                    const aSpecial = a.title[0]!.match(/[^a-z]/i)
                    const bSpecial = b.title[0]!.match(/[^a-z]/i)
                    if (aSpecial && !bSpecial)
                      return -1
                    if (!aSpecial && bSpecial)
                      return 1
                    return (a.title < b.title ? -1 : a.title > b.title ? 1 : 0)
                  })
                  .map((obj) => {
                    const linkName = getLinkName(obj)
                    const aliases = isFunctionReference(obj) ? obj.aliases : undefined
                    // Strip namespace prefix (e.g., "Vector." from "Vector.sum")
                    const stripPrefix = (n: string) => n.includes('.') ? n.split('.').slice(1).join('.') : n
                    const displayName = stripPrefix(obj.title)
                    const name = `${escape(displayName)}${aliases ? `, ${aliases.map(stripPrefix).join(', ')}` : ''}`
                    return `<a id="${linkName}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${linkName}', 'smooth')">${name}</a>`
                  })
                  .join('\n')
              : ''
          }
        </div>
      </div>`
  }

  return `
  <nav id="sidebar" class="fancy-scroll-background">
    <div ${styles('py-1', 'px-2', 'text-color-gray-400', 'flex', 'items-center', 'justify-between', 'gap-2', 'mb-4', 'cursor-pointer', 'border-gray-300', 'border', 'border-solid')} onclick="Playground.Search.openSearch()">
      <span ${styles('flex', 'items-center', 'gap-1')}>
        ${searchIcon}
        <span>Search</span>
      </span>
      <span ${styles('text-sm')}>F3</span>
    </div>
    <div id='home-page_link' onclick="Playground.showPage('index', 'smooth')" ${styles('flex', 'mb-2', 'text-color-gray-400', 'text-base', 'cursor-pointer')}>
      <a ${styles('flex', 'items-center', 'gap-1')} class="link">
        ${homeIcon}
        <span>Home</span>
      </a>
    </div>
    <div id='example-page_link' onclick="Playground.showPage('example-page', 'smooth')" ${styles('flex', 'mb-2', 'text-color-gray-400', 'text-base', 'cursor-pointer')}>
      <a ${styles('flex', 'items-center', 'gap-1')} class="link">
        ${lampIcon}
        <span>Examples</span>
      </a>
    </div>
    <div id='namespaces-page_link' onclick="Playground.showPage('namespaces-page', 'smooth')" ${styles('flex', 'mb-2', 'text-color-gray-400', 'text-base', 'cursor-pointer')}>
      <a ${styles('flex', 'items-center', 'gap-1')} class="link">
        ${packageIcon}
        <span>Namespaces</span>
      </a>
    </div>

    <!-- Core Categories -->
    <div ${styles('flex', 'flex-col', 'gap-4', 'my-4')}>
      ${coreCategories.map(categoryKey => renderCategory(categoryKey, categoryCollections)).join('\n')}
    </div>

    <!-- Namespace Categories (Collapsible) -->
    <div ${styles('flex', 'flex-col', 'gap-2', 'my-4', 'border-t', 'border-gray-700', 'pt-4')}>
      <div ${styles('text-color-gray-300', 'text-sm', 'font-bold', 'mb-2')}>Namespaces</div>
      <div ${styles('flex', 'flex-col', 'gap-2')}>
        ${namespaceCategories.map(categoryKey => renderNamespaceCategory(categoryKey)).join('\n')}
      </div>
    </div>
  </nav>
  `
}

function escape(str: string) {
  str = str.replace(/>/g, '&gt;')
  str = str.replace(/</g, '&lt;')
  return str
}
