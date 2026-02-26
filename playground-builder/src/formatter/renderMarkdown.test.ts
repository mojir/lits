import { describe, expect, it } from 'vitest'
import { extractCodeBlocks, parseMarkdownBlocks, parseTutorialMarkdown } from './renderMarkdown'

describe('parseMarkdownBlocks', () => {
  it('should parse a simple paragraph', () => {
    const blocks = parseMarkdownBlocks('Hello world')
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Hello world' },
    ])
  })

  it('should parse h3 header (##)', () => {
    const blocks = parseMarkdownBlocks('## My Header')
    expect(blocks).toEqual([
      { type: 'header', level: 3, text: 'My Header' },
    ])
  })

  it('should parse h4 header (###)', () => {
    const blocks = parseMarkdownBlocks('### Sub Header')
    expect(blocks).toEqual([
      { type: 'header', level: 4, text: 'Sub Header' },
    ])
  })

  it('should parse a code block without options', () => {
    const md = [
      '```',
      '10 + 20',
      '```',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'code', code: '10 + 20', options: [] },
    ])
  })

  it('should parse a code block with options', () => {
    const md = [
      '```no-run,no-result',
      'some code',
      '```',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'code', code: 'some code', options: ['no-run', 'no-result'] },
    ])
  })

  it('should parse a multi-line code block', () => {
    const md = [
      '```',
      'let x = 1;',
      'let y = 2;',
      'x + y',
      '```',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'code', code: 'let x = 1;\nlet y = 2;\nx + y', options: [] },
    ])
  })

  it('should parse mixed content', () => {
    const md = [
      'Welcome to Lits.',
      '',
      '## Expressions',
      '',
      'Everything is an expression.',
      '',
      '```',
      '10 + 20',
      '```',
      '',
      'That was simple.',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Welcome to Lits.' },
      { type: 'header', level: 3, text: 'Expressions' },
      { type: 'paragraph', text: 'Everything is an expression.' },
      { type: 'code', code: '10 + 20', options: [] },
      { type: 'paragraph', text: 'That was simple.' },
    ])
  })

  it('should join multi-line paragraphs', () => {
    const md = [
      'Line one',
      'line two',
      'line three',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Line one\nline two\nline three' },
    ])
  })

  it('should separate paragraphs on blank lines', () => {
    const md = [
      'Paragraph one.',
      '',
      'Paragraph two.',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Paragraph one.' },
      { type: 'paragraph', text: 'Paragraph two.' },
    ])
  })

  it('should ignore leading and trailing blank lines', () => {
    const md = [
      '',
      '',
      'Content',
      '',
      '',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Content' },
    ])
  })

  it('should parse a horizontal rule', () => {
    const blocks = parseMarkdownBlocks('---')
    expect(blocks).toEqual([
      { type: 'hr' },
    ])
  })

  it('should parse a long horizontal rule', () => {
    const blocks = parseMarkdownBlocks('-----')
    expect(blocks).toEqual([
      { type: 'hr' },
    ])
  })

  it('should parse an unordered list', () => {
    const md = [
      '* First item',
      '* Second item',
      '* Third item',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'list', items: ['First item', 'Second item', 'Third item'] },
    ])
  })

  it('should parse a blockquote', () => {
    const md = [
      '> This is a quote',
      '> spanning two lines',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'blockquote', text: 'This is a quote\nspanning two lines' },
    ])
  })

  it('should parse a blockquote with multiple paragraphs', () => {
    const md = [
      '> First paragraph',
      '>',
      '> Second paragraph',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'blockquote', text: 'First paragraph\n\nSecond paragraph' },
    ])
  })

  it('should parse list between paragraphs', () => {
    const md = [
      'Before list.',
      '',
      '* Item A',
      '* Item B',
      '',
      'After list.',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Before list.' },
      { type: 'list', items: ['Item A', 'Item B'] },
      { type: 'paragraph', text: 'After list.' },
    ])
  })

  it('should parse blockquote between paragraphs', () => {
    const md = [
      'Some text.',
      '',
      '> A wise quote',
      '',
      'More text.',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'paragraph', text: 'Some text.' },
      { type: 'blockquote', text: 'A wise quote' },
      { type: 'paragraph', text: 'More text.' },
    ])
  })

  it('should parse a mermaid block', () => {
    const md = [
      '```mermaid',
      'graph TD',
      '    A --> B',
      '```',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toEqual([
      { type: 'mermaid', code: 'graph TD\n    A --> B' },
    ])
  })

  it('should not treat mermaid as a code block option', () => {
    const md = [
      '```mermaid',
      'flowchart LR',
      '    X --> Y',
      '```',
    ].join('\n')
    const blocks = parseMarkdownBlocks(md)
    expect(blocks).toHaveLength(1)
    expect(blocks[0]!.type).toBe('mermaid')
    // Should NOT have options array
    expect(blocks[0]).not.toHaveProperty('options')
  })
})

describe('extractCodeBlocks', () => {
  it('should extract code from code blocks', () => {
    const md = [
      'Some text',
      '',
      '```',
      '10 + 20',
      '```',
      '',
      '```',
      'let x = 1;',
      'x + 1',
      '```',
    ].join('\n')
    const result = extractCodeBlocks(md)
    expect(result).toEqual([
      ['10 + 20'],
      ['let x = 1;', 'x + 1'],
    ])
  })

  it('should exclude no-run blocks', () => {
    const md = [
      '```',
      '10 + 20',
      '```',
      '',
      '```no-run',
      'pseudo code',
      '```',
      '',
      '```no-result',
      '30 + 40',
      '```',
    ].join('\n')
    const result = extractCodeBlocks(md)
    expect(result).toEqual([
      ['10 + 20'],
      ['30 + 40'],
    ])
  })

  it('should exclude mermaid blocks', () => {
    const md = [
      '```',
      '10 + 20',
      '```',
      '',
      '```mermaid',
      'graph TD',
      '    A --> B',
      '```',
      '',
      '```',
      '30 + 40',
      '```',
    ].join('\n')
    const result = extractCodeBlocks(md)
    expect(result).toEqual([
      ['10 + 20'],
      ['30 + 40'],
    ])
  })
})

describe('parseTutorialMarkdown', () => {
  it('should extract title and body', () => {
    const md = [
      '# My Tutorial',
      '',
      'Some content here.',
      '',
      '## Section',
      '',
      'More content.',
    ].join('\n')
    const result = parseTutorialMarkdown(md)
    expect(result.title).toBe('My Tutorial')
    expect(result.body).toBe('Some content here.\n\n## Section\n\nMore content.')
  })

  it('should throw if no title found', () => {
    expect(() => parseTutorialMarkdown('No title here')).toThrow('Tutorial markdown must have a # title')
  })

  it('should handle title as first line with no leading blank lines', () => {
    const md = '# Title\n\nBody text'
    const result = parseTutorialMarkdown(md)
    expect(result.title).toBe('Title')
    expect(result.body).toBe('Body text')
  })
})
