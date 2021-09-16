import { ReturnFromSignal, ReturnSignal } from '../src/errors'
describe('errors', () => {
  test('ReturnFromSignal', () => {
    const err = new ReturnFromSignal('block name', 100)
    expect(err).toBeInstanceOf(ReturnFromSignal)
    expect(err.name).toBe('ReturnFromSignal')
    expect(err.blockName).toBe('block name')
    expect(err.value).toBe(100)
  })
  test('ReturnSignal', () => {
    const err = new ReturnSignal(100)
    expect(err).toBeInstanceOf(ReturnSignal)
    expect(err.name).toBe('ReturnSignal')
    expect(err.value).toBe(100)
  })
})