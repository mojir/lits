import { evaluate } from './evaluator'
import { Context, VariableScope } from './evaluator/interface'
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

export function lispish(
  program: string,
  globalVariables: VariableScope = {},
  topScope: Context = { variables: {} },
): unknown {
  const tokens: Token[] = tokenize(program)
  const ast: Ast = parse(tokens)
  const result = evaluate(ast, globalVariables, topScope)
  return result
}

export { normalExpressionKeys, specialExpressionKeys } from './builtin'
export { reservedNames } from './reservedNames'
