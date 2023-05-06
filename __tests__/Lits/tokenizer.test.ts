import { tokenize } from '../../src/tokenizer'

describe(`Tokenizer`, () => {
  test(`simple expressions`, () => {
    const tokens = tokenize(
      `
      (let ((day (* 24 60 60 1000)))
        (* days day)
      )`,
      { debug: false },
    )
    expect(tokens.length).toBeGreaterThan(0)
  })
  test(`another simple expressions`, () => {
    const tokens = tokenize(`(do-me)`, { debug: false })
    expect(tokens.length).toBeGreaterThan(0)
  })

  test(`forbidden reserved names`, () => {
    expect(() => tokenize(`nil`, { debug: false })).not.toThrow()
    expect(() => tokenize(`false`, { debug: false })).not.toThrow()
    expect(() => tokenize(`true`, { debug: false })).not.toThrow()
    expect(() => tokenize(`null`, { debug: false })).toThrow()
    expect(() => tokenize(`undefined`, { debug: false })).toThrow()
    expect(() => tokenize(`===`, { debug: false })).toThrow()
    expect(() => tokenize(`!==`, { debug: false })).toThrow()
    expect(() => tokenize(`&&`, { debug: false })).toThrow()
    expect(() => tokenize(`||`, { debug: false })).toThrow()
  })

  test(`comments`, () => {
    expect(tokenize(`"Hi" ;This is a string`, { debug: false })).toEqual([{ type: `string`, value: `Hi` }])
    expect(tokenize(`"Hi" ;This is a string\n"there"`, { debug: false })).toEqual([
      { type: `string`, value: `Hi` },
      { type: `string`, value: `there` },
    ])
  })

  describe(`strings`, () => {
    test(`Unclosed string`, () => {
      expect(() => tokenize(`"Hej`, { debug: false })).toThrow()
    })
    test(`Escaped string`, () => {
      expect(tokenize(`"He\\"j"`, { debug: false })[0]).toEqual({
        type: `string`,
        value: `He"j`,
      })
      expect(tokenize(`"He\\\\j"`, { debug: false })[0]).toEqual({
        type: `string`,
        value: `He\\j`,
      })
      expect(tokenize(`"H\\ej"`, { debug: false })[0]).toEqual({
        type: `string`,
        value: `H\\ej`,
      })
    })
  })

  describe(`regexpShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#"Hej"`, { debug: true })).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: {},
          debugInfo: { line: 1, column: 1, code: `#"Hej"`, getLocation: undefined },
        },
      ])
      expect(tokenize(`#"Hej"g`, { debug: true })).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { g: true },
          debugInfo: { line: 1, column: 1, code: `#"Hej"g`, getLocation: undefined },
        },
      ])
      expect(tokenize(`#"Hej"i`, { debug: true })).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true },
          debugInfo: { line: 1, column: 1, code: `#"Hej"i`, getLocation: undefined },
        },
      ])
      expect(tokenize(`#"Hej"gi`, { debug: true })).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true, g: true },
          debugInfo: { line: 1, column: 1, code: `#"Hej"gi`, getLocation: undefined },
        },
      ])
      expect(tokenize(`#"Hej"ig`, { debug: true })).toEqual([
        {
          type: `regexpShorthand`,
          value: `Hej`,
          options: { i: true, g: true },
          debugInfo: { line: 1, column: 1, code: `#"Hej"ig`, getLocation: undefined },
        },
      ])
      expect(() => tokenize(`#"Hej"gg`, { debug: true })).toThrow()
      expect(() => tokenize(`#"Hej"ii`, { debug: true })).toThrow()
      expect(() => tokenize(`#1`, { debug: true })).toThrow()
    })
  })

  describe(`fnShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#(`, { debug: true })).toEqual([
        {
          type: `fnShorthand`,
          value: `#`,
          debugInfo: { line: 1, column: 1, code: `#(`, getLocation: undefined },
        },
        {
          type: `paren`,
          value: `(`,
          debugInfo: { line: 1, column: 2, code: `#(`, getLocation: undefined },
        },
      ])
      expect(() => tokenize(`#`, { debug: true })).toThrow()
    })
  })
})
