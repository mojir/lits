import { parse } from '../src/parser'
import { tokenize } from '../src/tokenizer'

const program = `
(let [day (* 24 60 60 1000)]
  (* days day)
)`

const optimizableProgram = `
(let [day (* 24 60 60 1000)]
  (* 11 day)
)`

describe(`Parser`, () => {
  test(`simple program`, () => {
    const tokens = tokenize(program, true)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(1)
  })
  test(`empty program`, () => {
    const tokens = tokenize(``, true)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(0)
  })

  test(`optimization`, () => {
    const tokens = tokenize(optimizableProgram, true)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(1)
  })

  test(`Unparsable expression`, () => {
    const tokens = tokenize(`(`, true)
    expect(() => parse(tokens)).toThrow()
  })

  test(`parse for`, () => {
    expect(() => parse(tokenize(`(for [x [1 2 3]] x)`, true))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &let [y (* x x)]] y)`, true))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &let [z x] &let [y (* x x)]] y)`, true))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &when (odd? x)] x)`, true))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &when (odd? x) &when (odd? x)] x)`, true))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x)] x)`, true))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x) &while (odd? x)] x)`, true))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x) &whil (odd? x)] x)`, true))).toThrow()
    expect(() =>
      parse(
        tokenize(
          `(for [x [1 2 3] &when (odd? x) &while (not= x 3) &let [y (* x x)] y [5 10 15] z [100 200 300]] (+ x y z))`,
          true,
        ),
      ),
    ).not.toThrow()
  })
})
