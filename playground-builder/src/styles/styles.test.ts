import { describe, expect, it } from 'vitest'
import { styles } from '.'

describe('styles', () => {
  it('should work', () => {
    expect(styles('text-base')).toBe('style="font-size: 1rem;"')
    expect(styles('text-base', 'font-bold')).toBe('style="font-size: 1rem; font-weight: bold;"')
    expect(styles('text-base', 'font-bold', 'text-base')).toBe('style="font-size: 1rem; font-weight: bold;"')
  })
})
