import { describe, expect, it } from 'vitest'
import { styles } from '../playground-builder/src/styles'
import { mdRules } from '../playground-builder/src/formatter/rules'
import { createFormatter } from './createFormatter'

describe('textFormatter', () => {
  it('should work with mdRules', () => {
    const formatter = createFormatter(mdRules)
    const input = '***Hello***, **world**!'
    const output = formatter(input)
    expect(output).toBe(
      `<span ${styles('italic')}>Hello</span>, <span ${styles('text-color-gray-300')}>world</span>!`,
    )
  })

  describe('internal links', () => {
    const formatter = createFormatter(mdRules)

    it('should resolve a known function name', () => {
      const output = formatter('See [[ceil]] for details.')
      expect(output).toContain('onclick="Playground.showPage(\'math-ceil\', \'smooth\')"')
      expect(output).toContain('>ceil</a>')
    })

    it('should resolve a special expression', () => {
      const output = formatter('Use [[if]] for branching.')
      expect(output).toContain('onclick="Playground.showPage(\'special-expression-if\', \'smooth\')"')
      expect(output).toContain('>if</a>')
    })

    it('should support custom display text with |', () => {
      const output = formatter('See [[ceil|the ceil function]] for details.')
      expect(output).toContain('onclick="Playground.showPage(\'math-ceil\', \'smooth\')"')
      expect(output).toContain('>the ceil function</a>')
    })

    it('should treat unknown targets as raw page IDs', () => {
      const output = formatter('See [[tutorial-getting-started|Getting Started]].')
      expect(output).toContain('onclick="Playground.showPage(\'tutorial-getting-started\', \'smooth\')"')
      expect(output).toContain('>Getting Started</a>')
    })

    it('should use raw page ID as display when no | provided', () => {
      const output = formatter('Go to [[example-page]].')
      expect(output).toContain('onclick="Playground.showPage(\'example-page\', \'smooth\')"')
      expect(output).toContain('>example-page</a>')
    })
  })

  describe('external links', () => {
    const formatter = createFormatter(mdRules)

    it('should create an external link', () => {
      const output = formatter('See [GitHub](https://github.com/mojir/lits) for source.')
      expect(output).toContain('href="https://github.com/mojir/lits"')
      expect(output).toContain('target="_blank"')
      expect(output).toContain('>GitHub</a>')
    })
  })

  describe('images', () => {
    const formatter = createFormatter(mdRules)

    it('should render an image', () => {
      const output = formatter('![Lits logo](images/lits-logo.svg)')
      expect(output).toContain('src="images/lits-logo.svg"')
      expect(output).toContain('alt="Lits logo"')
      expect(output).toContain('<img ')
    })

    it('should render an image with empty alt', () => {
      const output = formatter('![](images/pic.png)')
      expect(output).toContain('src="images/pic.png"')
      expect(output).toContain('alt=""')
    })

    it('should not confuse image with link', () => {
      const output = formatter('[not an image](https://example.com)')
      expect(output).not.toContain('<img')
      expect(output).toContain('<a ')
    })
  })
})
