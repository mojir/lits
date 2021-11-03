import { tokenize } from '../src/tokenizer'

describe(`Tokenizer`, () => {
  test(`simple expressions`, () => {
    const tokens = tokenize(
      `
      (let ((day (* 24 60 60 1000)))
        (* days day)
      )`,
      false,
    )
    expect(tokens.length).toBeGreaterThan(0)
  })
  test(`another simple expressions`, () => {
    const tokens = tokenize(`(do-me)`, false)
    expect(tokens.length).toBeGreaterThan(0)
  })

  test(`comments`, () => {
    expect(tokenize(`'Hi' ;This is a string`, false)).toEqual([{ type: `string`, value: `Hi`, sourceCodeInfo: null }])
    expect(tokenize(`'Hi' ;This is a string\n'there'`, false)).toEqual([
      { type: `string`, value: `Hi`, sourceCodeInfo: null },
      { type: `string`, value: `there`, sourceCodeInfo: null },
    ])
  })

  describe(`strings`, () => {
    test(`Unclosed string`, () => {
      expect(() => tokenize(`'Hej`, false)).toThrow()
    })
    test(`Escaped string`, () => {
      expect(tokenize(`'He\\'j'`, false)[0]).toEqual({
        type: `string`,
        value: `He'j`,
        sourceCodeInfo: null,
      })
      expect(tokenize(`'He\\\\j'`, false)[0]).toEqual({
        type: `string`,
        value: `He\\j`,
        sourceCodeInfo: null,
      })
      expect(tokenize(`'H\\ej'`, false)[0]).toEqual({
        type: `string`,
        value: `H\\ej`,
        sourceCodeInfo: null,
      })
    })
  })

  describe(`regexpShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#'Hej'`, true)).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: {},
          sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#'Hej'` },
        },
      ])
      expect(tokenize(`#'Hej'g`, true)).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { g: true },
          sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#'Hej'g` },
        },
      ])
      expect(tokenize(`#'Hej'i`, true)).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true },
          sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#'Hej'i` },
        },
      ])
      expect(tokenize(`#'Hej'gi`, true)).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true, g: true },
          sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#'Hej'gi` },
        },
      ])
      expect(tokenize(`#'Hej'ig`, true)).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true, g: true },
          sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#'Hej'ig` },
        },
      ])
      expect(() => tokenize(`#'Hej'gg`, true)).toThrow()
      expect(() => tokenize(`#'Hej'ii`, true)).toThrow()
      expect(() => tokenize(`#1`, true)).toThrow()
    })
  })

  describe(`fnShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#(`, true)).toEqual([
        { type: `fnShorthand`, value: `#`, sourceCodeInfo: { line: 1, column: 1, sourceCodeLine: `#(` } },
        { type: `paren`, value: `(`, sourceCodeInfo: { line: 1, column: 2, sourceCodeLine: `#(` } },
      ])
      expect(() => tokenize(`#`, true)).toThrow()
    })
  })
})
