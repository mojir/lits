import fs from 'node:fs'
import path from 'node:path'
import process from 'node:process'
import { styles } from '../../styles'
import { extractCodeBlocks, parseTutorialMarkdown, renderMarkdown } from '../../formatter/renderMarkdown'

// --- Types ---

export interface TutorialEntry {
  id: string
  title: string
  body: string
}

export interface TutorialFolder {
  title: string
  entries: TutorialEntry[]
}

export type TutorialItem = TutorialEntry | TutorialFolder

export function isTutorialFolder(item: TutorialItem): item is TutorialFolder {
  return 'entries' in item
}

// --- Helpers ---

export function getExamples(tutorial: TutorialEntry): string[][] {
  return extractCodeBlocks(tutorial.body)
}

/**
 * Strip the numeric prefix and convert kebab-case to a display name.
 * E.g. "01-getting-started" → "Getting Started"
 *      "02-language-features" → "Language Features"
 */
function toDisplayName(name: string): string {
  return name
    .replace(/^\d+-/, '')
    .split('-')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ')
}

// --- Filesystem scanning ---

const pagesDir = path.resolve(process.cwd(), 'playground-builder/src/components/tutorials/pages')

function loadMarkdownFile(filePath: string): TutorialEntry {
  const basename = path.basename(filePath, '.md')
  const id = `tutorial-${basename}`
  const content = fs.readFileSync(filePath, 'utf-8')
  const { title, body } = parseTutorialMarkdown(content)
  return { id, title, body }
}

function loadTutorialItems(): TutorialItem[] {
  const entries = fs.readdirSync(pagesDir, { withFileTypes: true })
    .sort((a, b) => a.name.localeCompare(b.name))

  const items: TutorialItem[] = []

  for (const entry of entries) {
    if (entry.isFile() && entry.name.endsWith('.md')) {
      items.push(loadMarkdownFile(path.join(pagesDir, entry.name)))
    }
    else if (entry.isDirectory()) {
      const folderPath = path.join(pagesDir, entry.name)
      const folderTitle = toDisplayName(entry.name)
      const mdFiles = fs.readdirSync(folderPath)
        .filter(f => f.endsWith('.md'))
        .sort((a, b) => a.localeCompare(b))

      const folderEntries = mdFiles.map(f => loadMarkdownFile(path.join(folderPath, f)))

      if (folderEntries.length > 0) {
        items.push({ title: folderTitle, entries: folderEntries })
      }
    }
  }

  return items
}

export const tutorialItems: TutorialItem[] = loadTutorialItems()

/** Flat list of all tutorial entries (for tests and page rendering) */
export const tutorials: TutorialEntry[] = tutorialItems.flatMap(item =>
  isTutorialFolder(item) ? item.entries : [item],
)

// --- Rendering ---

function renderNavLinks(index: number): string {
  const prev = index > 0 ? tutorials[index - 1] : null
  const next = index < tutorials.length - 1 ? tutorials[index + 1] : null

  const prevLink = prev
    ? `<a ${styles('cursor-pointer', 'text-color-gray-400')} onclick="Playground.showPage('${prev.id}', 'smooth')">&larr; ${prev.title}</a>`
    : '<span></span>'
  const nextLink = next
    ? `<a ${styles('cursor-pointer', 'text-color-gray-400')} onclick="Playground.showPage('${next.id}', 'smooth')">${next.title} &rarr;</a>`
    : '<span></span>'

  return `<div ${styles('flex', 'justify-between', 'py-2')}>${prevLink}${nextLink}</div>`
}

function renderTutorialPage(tutorial: TutorialEntry, index: number): string {
  const body = renderMarkdown(tutorial.body, tutorial.id)
  const nav = renderNavLinks(index)

  return `
  <div id="${tutorial.id}" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl', 'mb-6')}>${tutorial.title}</div>
    <div ${styles('mb-6', 'p-4', 'bg-gray-800', 'text-color-gray-300')}>
      ${nav}
      ${body}
      ${nav}
    </div>
  </div>
  `
}

export function getAllTutorialPages(): string {
  return tutorials.map((tutorial, index) => renderTutorialPage(tutorial, index)).join('\n')
}
