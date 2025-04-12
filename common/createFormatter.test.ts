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
})
