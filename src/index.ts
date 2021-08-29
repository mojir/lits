import { Context, evaluateAst } from './evaluator'
import { parseProgram } from './parser'
import { tokenize } from './tokenizer'

export function executeProgram(program: string, context: Context = {}): unknown {
  const tokens = tokenize(program)
  const ast = parseProgram(tokens)
  const result = evaluateAst(ast, context)
  return result
}
