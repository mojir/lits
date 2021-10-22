import { builtin } from '../builtin'
import { assertNameNotDefined } from '../builtin/utils'
import { evaluate } from '../evaluator'
import { Context } from '../evaluator/interface'
import { Obj } from '../interface'
import { parse } from '../parser'
import { Ast } from '../parser/interface'
import { tokenize } from '../tokenizer'
import { Token } from '../tokenizer/interface'
import { toAny } from '../utils'
import { Cache } from './Cache'

type EvaluateParams =
  | {
      globalContext: Context
      vars?: never
    }
  | {
      globalContext?: never
      vars: Obj
    }
  | {
      globalContext?: never
      vars?: never
    }

type LispishConfig = {
  astCacheSize?: number
}

export class Lispish {
  private importScope: Context = {}
  private astCache: Cache<Ast> | null

  constructor(config: LispishConfig = {}) {
    if (config.astCacheSize && config.astCacheSize > 0) {
      this.astCache = new Cache(config.astCacheSize)
    } else {
      this.astCache = null
    }
  }

  public run(program: string, params?: EvaluateParams): unknown {
    const ast = this.generateAst(program)
    const result = this.evaluate(ast, params)
    return result
  }

  public import(program: string, params: EvaluateParams = {}): void {
    const context = getContextFromParams(params)
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    const scope: Context = {}
    evaluate(ast, scope, context)

    const importKeys = Object.keys(this.importScope)
    for (const key of Object.keys(scope)) {
      if (importKeys.includes(key)) {
        throw Error(`Import faild, imported function/variable already exists: "${key}"`)
      }
      assertNameNotDefined(key, [{}, {}], builtin)
    }

    Object.assign(this.importScope, scope)
  }

  private tokenize(program: string): Token[] {
    return tokenize(program)
  }

  private parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  private evaluate(ast: Ast, params: EvaluateParams = {}): unknown {
    const context = getContextFromParams(params)
    return evaluate(ast, context, this.importScope)
  }

  private generateAst(program: string) {
    if (this.astCache) {
      const cachedAst = this.astCache.get(program)
      if (cachedAst) {
        return cachedAst
      }
    }
    const tokens: Token[] = this.tokenize(program)
    const ast: Ast = this.parse(tokens)
    this.astCache?.set(program, ast)
    return ast
  }
}

function getContextFromParams(params: EvaluateParams) {
  const context: Context = params.globalContext || {}

  if (params.vars) {
    Object.entries(params.vars).forEach(([key, value]) => {
      context[key] = { value: toAny(value) }
    })
  }

  return context
}
