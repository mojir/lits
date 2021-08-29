import { parseProgram } from '../src/parser'
import { tokenize } from '../src/tokenizer'

const program = `
(let ((day (* 24 60 60 1000)))
  (* days day)
)`

describe('Parser', () => {
  test('simple program', () => {
    const tokens = tokenize(program)
    const ast = parseProgram(tokens)
    expect(ast.body.length).toBe(1)
  })
  test('empty program', () => {
    const tokens = tokenize('')
    const ast = parseProgram(tokens)
    expect(ast.body.length).toBe(0)
  })
})
