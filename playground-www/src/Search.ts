import { asNotNull } from './utils'

type OnCloseCallback = () => void

let ctrlKeyTimer: number = 0
let ctrlKeyStarted: number | null = null
let selectedIndex: number | null = null
let onCloseCallback: OnCloseCallback | null = null

const searchInput = asNotNull(document.getElementById('search-input')) as HTMLInputElement
const searchResult = asNotNull(document.getElementById('search-result')) as HTMLDivElement
const noSearchResult = asNotNull(document.getElementById('no-search-result')) as HTMLDivElement
const searchOverlay = asNotNull(document.getElementById('search-dialog-overlay')) as HTMLDivElement
const searchDialog = asNotNull(document.getElementById('search-dialog')) as HTMLDivElement

searchInput.addEventListener('input', onSearch)
searchOverlay.addEventListener('click', closeSearch)
searchDialog.addEventListener('click', (event) => {
  searchInput.focus()
  event.stopPropagation()
})

function onClose(callback: OnCloseCallback) {
  onCloseCallback = callback
}

function handleKeyDown(event: KeyboardEvent): 'stop' | void {
  if (event.key === 'Control')
    handleCtrlKey()

  if (isOpen()) {
    switch (event.key) {
      case 'Escape':
        event.preventDefault()
        if (selectedIndex !== null && selectedIndex > 0)
          resetSelection()
        else if ((event.target as HTMLElement)?.closest('#search-input') && searchInput.value)
          clearSearch()
        else
          closeSearch()
        break
      case 'ArrowDown':
        event.preventDefault()
        selectNext()
        break
      case 'ArrowUp':
        event.preventDefault()
        selectPrevious()
        break
      case 'PageDown':
        event.preventDefault()
        selectPageDown()
        break
      case 'PageUp':
        event.preventDefault()
        selectPageUp()
        break
      case 'Home':
        event.preventDefault()
        selectFirst()
        break
      case 'End':
        event.preventDefault()
        selectLast()
        break
      case 'Enter':
        event.preventDefault()
        if (typeof selectedIndex === 'number') {
          const x = searchResult.children[selectedIndex] as HTMLElement | undefined
          x?.click()
        }

        break
      case 'k':
      case 'K':
        if (event.ctrlKey || event.metaKey) {
          event.preventDefault()
          openSearch()
        }
        break
      case 'F3':
        event.preventDefault()
        openSearch()
        break
    }

    return 'stop'
  }
  else {
    switch (event.key) {
      case 'k':
      case 'K':
        if (event.ctrlKey || event.metaKey) {
          event.preventDefault()
          openSearch()
        }
        break

      case 'F3':
        event.preventDefault()
        openSearch()
        break
    }
  }
}

function handleCtrlKey() {
  if (ctrlKeyStarted === null) {
    ctrlKeyStarted = Date.now()

    ctrlKeyTimer = window.setTimeout(resetCtrlKey, 400)
  }
  else {
    resetCtrlKey()
    if (isOpen())
      closeSearch()
    else
      openSearch()
  }
}

function resetCtrlKey() {
  window.clearTimeout(ctrlKeyTimer)
  ctrlKeyStarted = null
  ctrlKeyTimer = 0
}

function openSearch() {
  searchOverlay.style.display = 'block'
  if (searchResult.children.length === 0)
    updateSearchResult(searchInput.value)
  searchInput.focus()
}

function closeSearch() {
  searchOverlay.style.display = 'none'
  onCloseCallback?.()
}

function isOpen() {
  return searchOverlay.style.display === 'block'
}
function clearSearch() {
  searchInput.value = ''
  updateSearchResult('')
}

function resetSelection() {
  if (searchInput.value)
    selectedIndex = 0
  else
    selectedIndex = null
  updateSelection()
  searchResult.scrollTo(0, 0)
}

function updateSelection() {
  Array.from(searchResult.getElementsByClassName('selected'))
    .forEach(el => el.classList.remove('selected'))

  if (selectedIndex !== null) {
    const count = searchResult.children.length
    if (count) {
      if (selectedIndex >= count)
        selectedIndex = count - 1
      else if (selectedIndex < 0)
        selectedIndex = 0

      const element = searchResult.children[selectedIndex]!
      element.classList.add('selected')
      element.scrollIntoView({ block: 'center' })
    }
    else {
      selectedIndex = null
    }
  }
}

function selectPrevious() {
  if (selectedIndex !== null)
    selectedIndex -= 1

  updateSelection()
}

function selectNext() {
  if (selectedIndex !== null)
    selectedIndex += 1
  else
    selectedIndex = 0

  updateSelection()
}

function selectPageUp() {
  if (selectedIndex !== null)
    selectedIndex -= 10

  updateSelection()
}

function selectPageDown() {
  if (selectedIndex !== null)
    selectedIndex += 10
  else
    selectedIndex = 10

  updateSelection()
}

function selectFirst() {
  selectedIndex = 0
  updateSelection()
}

function selectLast() {
  selectedIndex = searchResult.children.length - 1
  updateSelection()
}

function onSearch(event: Event) {
  const target = event.target as HTMLInputElement | undefined
  const searchString: string = target?.value ?? ''
  updateSearchResult(searchString)
}

function updateSearchResult(searchString: string) {
  searchResult.style.display = 'none'
  noSearchResult.style.display = 'none'
  searchResult.innerHTML = ''

  // eslint-disable-next-line ts/no-unsafe-member-access
  const searchResults = ((window as any).Playground.allSearchResultEntries as { search: string, html: HTMLElement }[]).filter(
    entry => entry.search.toLowerCase().includes(searchString.toLowerCase()),
  )

  if (searchResults.length === 0) {
    noSearchResult.style.display = 'flex'
  }
  else {
    searchResult.style.display = 'flex'
    searchResult.innerHTML = searchResults.map(entry => entry.html).join('')
  }

  resetSelection()
}

export const Search = {
  openSearch,
  closeSearch,
  clearSearch,
  handleKeyDown,
  onClose,
}
