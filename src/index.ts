import { Context, evaluateAst } from './evaluator/Evaluator'
import { parseProgram } from './parser/Parser'
import { tokenize } from './tokenizer/Tokenizer'

export function executeProgram(program: string, context: Context = {}): unknown {
  const tokens = tokenize(program)
  const ast = parseProgram(tokens)
  const result = evaluateAst(ast, context)
  return result
}
