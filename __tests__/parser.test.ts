import { parse } from '../src/parser'
import { tokenize } from '../src/tokenizer'

const program = `
(let ((day (* 24 60 60 1000)))
  (* days day)
)`

const optimizableProgram = `
(let ((day (* 24 60 60 1000)))
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
    const tokens = tokenize(`("s")`)
    expect(() => parse(tokens)).toThrow()
  })
})
