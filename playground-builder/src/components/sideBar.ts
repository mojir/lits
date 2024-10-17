import type { Reference } from '../../../reference'
import { apiReference } from '../../../reference'
import { categories } from '../../../reference/api'
import { homeIcon, lampIcon, searchIcon } from '../icons'
import { styles } from '../styles'

export function getSideBar() {
  const categoryCollections = Object.values(apiReference).reduce((result: Record<string, Reference[]>, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category]!.push(obj)
    return result
  }, {})

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
    <div ${styles('flex', 'flex-col', 'gap-4', 'my-4')}>
    ${categories
      .map((categoryKey) => {
        return `
          <div ${styles('flex', 'flex-col', 'gap-1')}>
            <div ${styles('text-color-gray-200')}>
              ${categoryKey}
            </div>
            <div ${styles('flex', 'flex-col', 'ml-2', 'text-color-gray-400', 'text-base')}>
              ${
                categoryCollections[categoryKey]
                  ? categoryCollections[categoryKey]!
                      .sort((a, b) => (a.title < b.title ? -1 : a.title > b.title ? 1 : 0))
                      .map((obj) => {
                        const linkName = obj.linkName
                        const name = escape(obj.title)
                        return `<a id="${linkName}_link" ${styles('scroll-my-2', 'pl-2')} onclick="Playground.showPage('${linkName}', 'smooth')">${name}</a>`
                      })
                      .join('\n')
                  : ''
              }
            </div>
          </div>`
      })
      .join('\n')}
      </div>
  </nav>
  `
}

function escape(str: string) {
  str = str.replace(/>/g, '&gt;')
  str = str.replace(/</g, '&lt;')
  return str
}
