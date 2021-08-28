import { tokenize } from '../src/tokenizer/Tokenizer'

describe('Tokenizer', () => {
  test('simple expressions', () => {
    const tokens = tokenize(`
      (let ((day (* 24 60 60 1000)))
        (* days day)
      )`)
    expect(tokens.length).toBeGreaterThan(0)
  })
  test('another simple expressions', () => {
    const tokens = tokenize(`(do-me)`)
    console.log(JSON.stringify(tokens, null, 2))
    expect(tokens.length).toBeGreaterThan(0)
  })
})

//  (+ (* 2 7) 3)
