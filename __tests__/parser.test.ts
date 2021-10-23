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
    const tokens = tokenize(program)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(1)
  })
  test(`empty program`, () => {
    const tokens = tokenize(``)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(0)
  })

  test(`optimization`, () => {
    const tokens = tokenize(optimizableProgram)
    const ast = parse(tokens)
    expect(ast.body.length).toBe(1)
  })

  test(`Unparsable expression`, () => {
    const tokens = tokenize(`(`)
    expect(() => parse(tokens)).toThrow()
  })

  test(`parse for`, () => {
    expect(() => parse(tokenize(`(for [x [1 2 3]] x)`))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &let [y (* x x)]] y)`))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &let [z x] &let [y (* x x)]] y)`))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &when (odd? x)] x)`))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &when (odd? x) &when (odd? x)] x)`))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x)] x)`))).not.toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x) &while (odd? x)] x)`))).toThrow()
    expect(() => parse(tokenize(`(for [x [1 2 3] &while (odd? x) &whil (odd? x)] x)`))).toThrow()
    expect(() =>
      parse(
        tokenize(
          `(for [x [1 2 3] &when (odd? x) &while (not= x 3) &let [y (* x x)] y [5 10 15] z [100 200 300]] (+ x y z))`,
        ),
      ),
    ).not.toThrow()
  })
})
