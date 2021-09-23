import { evaluate } from '../evaluator'
import { Context, VariableScope } from '../evaluator/interface'
import { parse } from '../parser'
import { Ast } from '../parser/interface'
import { tokenize } from '../tokenizer'
import { Token } from '../tokenizer/interface'

export type EvaluationParams = {
  globalVariables?: VariableScope
  topScope?: Context
}

export class Lispish {
  public tokenize(program: string): Token[] {
    return tokenize(program)
  }

  public parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  public evaluate(ast: Ast, evaluationParams: EvaluationParams = {}): unknown {
    const globalVariables: VariableScope = evaluationParams.globalVariables || {}
    const topScope: Context = evaluationParams.topScope || { functions: {}, variables: {} }
    return evaluate(ast, globalVariables, topScope)
  }

  public run(program: string, evaluationParams: EvaluationParams = {}): unknown {
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const result = this.evaluate(ast, evaluationParams)
    return result
  }
}
