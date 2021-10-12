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
    expect(tokenize(`"Hi" ;This is a string`)).toEqual([{ type: `string`, value: `Hi` }])
    expect(tokenize(`"Hi" ;This is a string\n"there"`)).toEqual([
      { type: `string`, value: `Hi` },
      { type: `string`, value: `there` },
    ])
  })

  describe(`strings`, () => {
    test(`Unclosed string`, () => {
      expect(() => tokenize(`"Hej`)).toThrow()
    })
    test(`Escaped string`, () => {
      expect(tokenize(`"He\\"j"`)[0]).toEqual({ type: `string`, value: `He"j` })
      expect(tokenize(`"He\\\\j"`)[0]).toEqual({ type: `string`, value: `He\\j` })
      expect(tokenize(`"H\\ej"`)[0]).toEqual({ type: `string`, value: `H\\ej` })
    })
  })

  describe(`regexpShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#"Hej"`)).toEqual([{ type: `regexpShorthand`, value: `Hej` }])
      expect(() => tokenize(`#1`)).toThrow()
    })
  })

  describe(`fnShorthand`, () => {
    test(`samples`, () => {
      expect(tokenize(`#(`)).toEqual([
        { type: `fnShorthand`, value: `#` },
        { type: `paren`, value: `(` },
      ])
      expect(() => tokenize(`#`)).toThrow()
    })
  })
})
