import { describe, expect, it } from 'vitest'
import { LitsError } from './errors'

describe('getCodeMarker', () => {
  it('should return the code marker', () => {
    const error1 = new LitsError('Error message', { code: '(str 1)', filePath: 'file.lits', position: { line: 1, column: 1 } })
    expect(error1.getCodeMarker()).toBe('^      ')

    const error2 = new LitsError(new Error('Error message'), { code: '(str 1)', filePath: 'file.lits', position: { line: 1, column: 1 } })
    expect(error2.getCodeMarker()).toBe('^      ')

    const error3 = new LitsError(new Error('Error message'))
    expect(error3.getCodeMarker()).toBeUndefined()
  })
})
