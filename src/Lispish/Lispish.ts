import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
import { evaluate } from '../evaluator'
import { Context } from '../evaluator/interface'
import { parse } from '../parser'
import { Ast } from '../parser/interface'
import { tokenize } from '../tokenizer'
import { Token } from '../tokenizer/interface'

type EvaluateParams =
  | {
      globalContext: Context
      vars?: never
    }
  | {
      globalContext?: never
      vars: Record<string, unknown>
    }
  | {
      globalContext?: never
      vars?: never
    }

export class Lispish {
  private importScope: Context = {}

  public tokenize(program: string): Token[] {
    return tokenize(program)
  }

  public parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  public evaluate(ast: Ast, params: EvaluateParams = {}): unknown {
    const globalContext: Context = params.globalContext || {}

    if (params.vars) {
      Object.entries(params.vars).forEach(([key, value]) => {
        globalContext[key] = { constant: true, value }
      })
    }

    return evaluate(ast, globalContext, this.importScope)
  }

  public run(program: string, params?: EvaluateParams): unknown {
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const result = this.evaluate(ast, params)
    return result
  }

  public import(program: string): void {
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const scope: Context = {}
    evaluate(ast, scope, {})

    const importKeys = Object.keys(this.importScope)
    for (const key of Object.keys(scope)) {
      if (importKeys.includes(key)) {
        throw Error(`Import faild, imported function/variable already exists: "${key}"`)
      }
      if (normalExpressionKeys.includes(key)) {
        throw Error(`Import faild, cannot shadow builtin normal expression: "${key}"`)
      }
      if (specialExpressionKeys.includes(key)) {
        throw Error(`Import faild, cannot shadow builtin special expression: "${key}"`)
      }
    }

    Object.assign(this.importScope, scope)
  }
}
