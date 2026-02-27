import type { Reference } from '../../../reference'
import { apiReference, getLinkName, moduleReference } from '../../../reference'
import { coreCategories, moduleCategories } from '../../../reference/api'
import { chevronRightIcon, homeIcon, labIcon, lampIcon, searchIcon } from '../icons'
import { styles } from '../styles'
import { isTutorialFolder, tutorialItems } from './tutorials'

function menuLink(icon: string, title: string, onclick: string) {
  return `
    <div onclick="${onclick}" ${styles('flex', 'mb-2', 'text-color-gray-400', 'text-base', 'cursor-pointer')}>
      <a ${styles('flex', 'items-center', 'gap-1')} class="link">
        <span ${styles('font-size: 1.2em;', 'flex', 'items-center')}>${icon}</span>
        <span>${title}</span>
      </a>
    </div>`
}

export function getSideBar() {
  const categoryCollections = Object.values(apiReference).reduce((result: Record<string, Reference[]>, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category]!.push(obj)
    return result
  }, {})

  const moduleCategoryCollections = Object.values(moduleReference).reduce((result: Record<string, Reference[]>, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category]!.push(obj)
    return result
  }, {})

  const renderCategory = (categoryKey: string, collections: Record<string, Reference[]>) => {
    return `
      <div ${styles('flex', 'flex-col', 'gap-1')}>
        <div 
          ${styles('text-color-gray-200', 'flex', 'items-center', 'gap-1', 'cursor-pointer')}
          onclick="Playground.toggleCoreCategory('${categoryKey}')"
        >
          <span id="core-chevron-${categoryKey}" class="core-chevron">${chevronRightIcon}</span>
          <span>${categoryKey}</span>
        </div>
        <div 
          id="core-content-${categoryKey.replace(/\s+/g, '-')}" 
          ${styles('flex-col', 'ml-2', 'text-color-gray-400', 'text-base', 'display: none;')}
        >
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
                    const name = `${escape(obj.title)}`
                    return `<a id="${linkName}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${linkName}', 'smooth')">${name}</a>`
                  })
                  .join('\n')
              : ''
          }
        </div>
      </div>`
  }

  const renderModuleCategory = (categoryKey: string) => {
    return `
      <div ${styles('flex', 'flex-col', 'gap-1')}>
        <div 
          ${styles('text-color-gray-200', 'flex', 'items-center', 'gap-1', 'cursor-pointer')}
          onclick="Playground.toggleModuleCategory('${categoryKey}')"
        >
          <span id="ns-chevron-${categoryKey}" class="ns-chevron">${chevronRightIcon}</span>
          <span>${categoryKey}</span>
        </div>
        <div 
          id="ns-content-${categoryKey.replace(/\s+/g, '-')}" 
          ${styles('flex-col', 'ml-2', 'text-color-gray-400', 'text-base', 'display: none;')}
        >
          ${
            moduleCategoryCollections[categoryKey]
              ? moduleCategoryCollections[categoryKey]
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
                    // Strip module prefix (e.g., "vector." from "vector.sum")
                    const stripPrefix = (n: string) => n.includes('.') ? n.split('.').slice(1).join('.') : n
                    const displayName = stripPrefix(obj.title)
                    const name = `${escape(displayName)}`
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
    ${menuLink(homeIcon, 'Home', 'Playground.showPage(\'index\', \'smooth\')')}
    ${menuLink(lampIcon, 'Examples', 'Playground.showPage(\'example-page\', \'smooth\')')}
    <!-- Tutorials (Collapsible) -->
    <div ${styles('flex', 'flex-col', 'gap-2')}>
      ${menuLink(labIcon, 'Tutorials', 'Playground.toggleTutorials()')}
      <div id="tutorial-content" ${styles('flex-col', 'ml-2', 'text-color-gray-400', 'text-base', 'display: none;')}>
        ${tutorialItems.map((item) => {
          if (isTutorialFolder(item)) {
            return `
              <div ${styles('flex', 'flex-col', 'gap-0')}>
                <div ${styles('scroll-my-2', 'pl-2', 'text-color-gray-300')}>${item.title}</div>
                <div ${styles('flex', 'flex-col', 'ml-4')}>
                  ${item.entries.map(e => `<a id="${e.id}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${e.id}', 'smooth')">${e.title}</a>`).join('\n')}
                </div>
              </div>
            `
          }
          return `<a id="${item.id}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${item.id}', 'smooth')">${item.title}</a>`
        }).join('\n')}
      </div>
    </div>
    <!-- Core Categories (Collapsible) -->
    <div ${styles('flex', 'flex-col', 'gap-2', 'my-4')}>
      <div id='core-page_link' ${styles('text-color-gray-300', 'text-base', 'font-bold', 'mb-1', 'cursor-pointer')} onclick="Playground.showPage('core-page', 'smooth')">Core reference</div>
      <div ${styles('flex', 'flex-col', 'gap-2')}>
        ${coreCategories.map(categoryKey => renderCategory(categoryKey, categoryCollections)).join('\n')}
      </div>
    </div>

    <!-- Module Categories (Collapsible) -->
    <div ${styles('flex', 'flex-col', 'gap-2', 'my-4', 'border-t', 'border-gray-700', 'pt-4')}>
      <div id='modules-page_link' ${styles('text-color-gray-300', 'text-base', 'font-bold', 'mb-1', 'cursor-pointer')} onclick="Playground.showPage('modules-page', 'smooth')">Module reference</div>
      <div ${styles('flex', 'flex-col', 'gap-2')}>
        ${moduleCategories.map(categoryKey => renderModuleCategory(categoryKey)).join('\n')}
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
