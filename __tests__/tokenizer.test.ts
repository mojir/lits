import { tokenize } from '../src/tokenizer'

describe(`Tokenizer`, () => {
  test(`simple expressions`, () => {
    const tokens = tokenize(`
      (let ((day (* 24 60 60 1000)))
        (* days day)
      )`)
    expect(tokens.length).toBeGreaterThan(0)
  })
  test(`another simple expressions`, () => {
    const tokens = tokenize(`(do-me)`)
    expect(tokens.length).toBeGreaterThan(0)
  })

  test(`comments`, () => {
    expect(tokenize(`'Hi' ;This is a string`)).toEqual([{ type: `string`, value: `Hi`, meta: { line: 1, column: 1 } }])
    expect(tokenize(`'Hi' ;This is a string\n'there'`)).toEqual([
      { type: `string`, value: `Hi`, meta: { line: 1, column: 1 } },
      { type: `string`, value: `there`, meta: { line: 2, column: 1 } },
    ])
  })

  describe(`strings`, () => {
    test(`Unclosed string`, () => {
      expect(() => tokenize(`'Hej`)).toThrow()
    })
    test(`Escaped string`, () => {
      expect(tokenize(`'He\\'j'`)[0]).toEqual({ type: `string`, value: `He'j`, meta: { line: 1, column: 1 } })
      expect(tokenize(`'He\\\\j'`)[0]).toEqual({ type: `string`, value: `He\\j`, meta: { line: 1, column: 1 } })
      expect(tokenize(`'H\\ej'`)[0]).toEqual({ type: `string`, value: `H\\ej`, meta: { line: 1, column: 1 } })
    })
  })

  describe(`regexpShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#'Hej'`)).toEqual([
        { type: `regexpShorthand`, value: `Hej`, options: {}, meta: { line: 1, column: 1 } },
      ])
      expect(tokenize(`#'Hej'g`)).toEqual([
        { type: `regexpShorthand`, value: `Hej`, options: { g: true }, meta: { line: 1, column: 1 } },
      ])
      expect(tokenize(`#'Hej'i`)).toEqual([
        { type: `regexpShorthand`, value: `Hej`, options: { i: true }, meta: { line: 1, column: 1 } },
      ])
      expect(tokenize(`#'Hej'gi`)).toEqual([
        { type: `regexpShorthand`, value: `Hej`, options: { i: true, g: true }, meta: { line: 1, column: 1 } },
      ])
      expect(tokenize(`#'Hej'ig`)).toEqual([
        { type: `regexpShorthand`, value: `Hej`, options: { i: true, g: true }, meta: { line: 1, column: 1 } },
      ])
      expect(() => tokenize(`#'Hej'gg`)).toThrow()
      expect(() => tokenize(`#'Hej'ii`)).toThrow()
      expect(() => tokenize(`#1`)).toThrow()
    })
  })

  describe(`fnShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#(`)).toEqual([
        { type: `fnShorthand`, value: `#`, meta: { line: 1, column: 1 } },
        { type: `paren`, value: `(`, meta: { line: 1, column: 2 } },
      ])
      expect(() => tokenize(`#`)).toThrow()
    })
  })
})
