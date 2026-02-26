import { createFormatter } from '../../../common/createFormatter'
import { renderExample } from '../renderExample'
import { styles } from '../styles'
import { mdRules } from './rules'
import { renderMermaidToSvg } from './renderMermaid'

// --- Block types ---

interface HeaderBlock {
  type: 'header'
  level: 3 | 4
  text: string
}

interface CodeBlock {
  type: 'code'
  code: string
  options: string[]
}

interface ParagraphBlock {
  type: 'paragraph'
  text: string
}

interface ListBlock {
  type: 'list'
  items: string[]
}

interface BlockquoteBlock {
  type: 'blockquote'
  text: string
}

interface HorizontalRuleBlock {
  type: 'hr'
}

interface MermaidBlock {
  type: 'mermaid'
  code: string
}

type Block = HeaderBlock | CodeBlock | ParagraphBlock | ListBlock | BlockquoteBlock | HorizontalRuleBlock | MermaidBlock

// --- Block parser ---

export function parseMarkdownBlocks(markdown: string): Block[] {
  const lines = markdown.split('\n')
  const blocks: Block[] = []
  let i = 0
  let paragraphLines: string[] = []

  function flushParagraph() {
    const text = paragraphLines.join('\n').trim()
    if (text) {
      blocks.push({ type: 'paragraph', text })
    }
    paragraphLines = []
  }

  while (i < lines.length) {
    const line = lines[i]!

    // Fenced code block
    if (line.startsWith('```')) {
      flushParagraph()
      const optionStr = line.slice(3).trim()
      const isMermaid = optionStr === 'mermaid'
      const options = isMermaid ? [] : (optionStr ? optionStr.split(',') : [])
      i++
      const codeLines: string[] = []
      while (i < lines.length && lines[i]!.trim() !== '```') {
        codeLines.push(lines[i]!)
        i++
      }
      if (i < lines.length)
        i++ // skip closing ```
      if (isMermaid) {
        blocks.push({ type: 'mermaid', code: codeLines.join('\n') })
      }
      else {
        blocks.push({ type: 'code', code: codeLines.join('\n'), options })
      }
      continue
    }

    // Header (## or ###)
    const headerMatch = /^(#{2,3}) (.+)$/.exec(line)
    if (headerMatch) {
      flushParagraph()
      const level = (headerMatch[1]!.length === 2 ? 3 : 4)
      blocks.push({ type: 'header', level, text: headerMatch[2]! })
      i++
      continue
    }

    // Horizontal rule
    if (/^-{3,}$/.test(line.trim())) {
      flushParagraph()
      blocks.push({ type: 'hr' })
      i++
      continue
    }

    // Unordered list
    if (/^\* /.test(line)) {
      flushParagraph()
      const items: string[] = []
      while (i < lines.length && /^\* /.test(lines[i]!)) {
        items.push(lines[i]!.slice(2))
        i++
      }
      blocks.push({ type: 'list', items })
      continue
    }

    // Blockquote
    if (/^> ?/.test(line)) {
      flushParagraph()
      const quoteLines: string[] = []
      while (i < lines.length && /^> ?/.test(lines[i]!)) {
        quoteLines.push(lines[i]!.replace(/^> ?/, ''))
        i++
      }
      blocks.push({ type: 'blockquote', text: quoteLines.join('\n') })
      continue
    }

    // Empty line — paragraph break
    if (line.trim() === '') {
      flushParagraph()
      i++
      continue
    }

    // Regular text line
    paragraphLines.push(line)
    i++
  }

  flushParagraph()
  return blocks
}

// --- Renderer ---

const formatInline = createFormatter(mdRules)

export function renderMarkdown(markdown: string, namePrefix: string): string {
  const blocks = parseMarkdownBlocks(markdown)
  let codeBlockIndex = 0

  return blocks.map((block) => {
    switch (block.type) {
      case 'header':
        return block.level === 3
          ? `<h3 ${styles('text-xl', 'mb-2', 'text-color-gray-200')}>${block.text}</h3>`
          : `<h4 ${styles('text-lg', 'mb-1', 'text-color-gray-200')}>${block.text}</h4>`
      case 'paragraph':
        return `<p ${styles('mb-4')}>${formatInline(block.text)}</p>`
      case 'list':
        return `<ul ${styles('mb-4', 'pl-6')} style="list-style-type: disc;">${
          block.items.map(item => `<li ${styles('mb-1')}>${formatInline(item)}</li>`).join('\n')
        }</ul>`
      case 'blockquote': {
        const paragraphs = block.text.split('\n\n').filter(Boolean)
        const inner = paragraphs.map(p => `<p ${styles('mb-2')}>${formatInline(p)}</p>`).join('\n')
        return `<blockquote ${styles('mb-4', 'pl-4', 'border-l-4', 'border-gray-500', 'italic', 'text-color-gray-400')}>${inner}</blockquote>`
      }
      case 'hr':
        return `<hr ${styles('mb-4', 'border-gray-600')}>`
      case 'mermaid':
        return `<div ${styles('mb-4', 'flex', 'justify-center')}>${renderMermaidToSvg(block.code)}</div>`
      case 'code': {
        const name = `${namePrefix}-${codeBlockIndex++}`
        const noRun = block.options.includes('no-run')
        const noResult = block.options.includes('no-result')
        return `<div ${styles('flex', 'flex-col', 'gap-4', 'bg-gray-700', 'p-4', 'mb-4')} style="overflow-x: auto;">${
          renderExample(block.code, name, { noRun, noResult })
        }</div>`
      }
      default:
        throw new Error(`Unknown block type: ${(block as { type: string }).type}`)
    }
  }).join('\n')
}

// --- Code block extraction (for tests) ---

export function extractCodeBlocks(markdown: string): string[][] {
  return parseMarkdownBlocks(markdown)
    .filter((b): b is CodeBlock => b.type === 'code' && !b.options.includes('no-run'))
    .map(b => b.code.split('\n'))
}

// --- Tutorial markdown parsing ---

export function parseTutorialMarkdown(markdown: string): { title: string, body: string } {
  const lines = markdown.split('\n')
  const titleLineIndex = lines.findIndex(line => /^# .+$/.test(line))
  if (titleLineIndex === -1) {
    throw new Error('Tutorial markdown must have a # title')
  }
  const title = /^# (.+)$/.exec(lines[titleLineIndex]!)![1]!
  const bodyLines = [...lines.slice(0, titleLineIndex), ...lines.slice(titleLineIndex + 1)]
  const body = bodyLines.join('\n').trim()
  return { title, body }
}
