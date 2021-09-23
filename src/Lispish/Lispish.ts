import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
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
  private importScope: Context = { functions: {}, variables: {} }

  public tokenize(program: string): Token[] {
    return tokenize(program)
  }

  public parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  public evaluate(ast: Ast, globalVariables: Record<string, unknown> = {}): unknown {
    const variableScope: VariableScope = Object.keys(globalVariables).reduce((result: VariableScope, key) => {
      result[key] = {
        value: globalVariables[key],
        const: true,
      }
      return result
    }, {})
    return evaluate(ast, variableScope, this.importScope)
  }

  public run(program: string, globalVariables?: Record<string, unknown>): unknown {
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const result = this.evaluate(ast, globalVariables)
    return result
  }

  public import(program: string): void {
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const scope: Context = { functions: {}, variables: {} }
    evaluate(ast, {}, scope)

    const importFunctionKeys = Object.keys(this.importScope.functions)
    for (const key of Object.keys(scope.functions)) {
      if (importFunctionKeys.includes(key)) {
        throw Error(`Import faild, imported function already exists: "${key}"`)
      }
      if (normalExpressionKeys.includes(key)) {
        throw Error(`Import faild, cannot shadow builtin normal expression: "${key}"`)
      }
      if (specialExpressionKeys.includes(key)) {
        throw Error(`Import faild, cannot shadow builtin special expression: "${key}"`)
      }
    }

    const importVariableKeys = Object.keys(this.importScope.variables)
    for (const key of Object.keys(scope.variables)) {
      if (importVariableKeys.includes(key)) {
        throw Error(`Import faild, imported variable already exists "${key}"`)
      }
    }

    Object.assign(this.importScope.functions, scope.functions)
    Object.assign(this.importScope.variables, scope.variables)
  }
}
