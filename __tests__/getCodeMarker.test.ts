import { describe, expect, it } from 'vitest'
import { getCodeMarker } from '../src/utils/debug/getCodeMarker'

describe('debugTools', () => {
  it('getCodeMarker', () => {
    expect(getCodeMarker({ code: '', position: { line: 1, column: 2 } })).toBe('')
    expect(getCodeMarker({ code: 'foo', position: { line: 1, column: 2 } })).toBe(' ^ ')
  })
})
