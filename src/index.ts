import { evaluate } from './evaluator'
import { Context } from './evaluator/interface'
import { parse } from './parser'
import { Ast } from './parser/interface'
import { tokenize } from './tokenizer'
import { Token } from './tokenizer/interface'

export { evaluate } from './evaluator'
export { Context } from './evaluator/interface'
export { parse } from './parser'
export { Ast } from './parser/interface'
export { tokenize } from './tokenizer'
export { Token } from './tokenizer/interface'

export function lispish(program: string, context: Context = {}): unknown {
  const tokens: Token[] = tokenize(program)
  const ast: Ast = parse(tokens)
  const result = evaluate(ast, context)
  return result
}
