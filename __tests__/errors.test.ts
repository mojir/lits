import { RecurSignal, UserDefinedError } from '../src/errors'
describe(`errors`, () => {
  test(`RecurSignal`, () => {
    const err = new RecurSignal([100])
    expect(err).toBeInstanceOf(RecurSignal)
    expect(err.name).toBe(`RecurSignal`)
    expect(err.params).toEqual([100])
  })
  test(`UserDefinedError`, () => {
    const err = new UserDefinedError(`A message`, {
      line: 1,
      column: 1,
      code: `(+ 1 2)`,
    })
    expect(err).toBeInstanceOf(UserDefinedError)
    expect(err.name).toBe(`UserDefinedError`)
    expect(err.message).toBe(`A message\n(+ 1 2)\n^      `)
  })
})
