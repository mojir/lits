import { LitsFunction } from '..'
import { analyzeAst } from '../analyze'
import { AnalyzeResult } from '../analyze/interface'
import { builtin } from '../builtin'
import { createContextStack, evaluate } from '../evaluator'
import { Context, ContextStack } from '../evaluator/interface'
import { Any, Obj } from '../interface'
import { parse } from '../parser'
import { Ast } from '../parser/interface'
import { tokenize } from '../tokenizer'
import { Token } from '../tokenizer/interface'
import { createContextFromValues } from '../utils'
import { Cache } from './Cache'

export type LocationGetter = (line: number, col: number) => string

export type LitsRuntimeInfo = {
  astCache: Cache | null
  astCacheSize: number | null
  debug: boolean
}

export type LitsParams = {
  contexts?: Context[]
  globals?: Obj
  globalContext?: Context
  getLocation?: LocationGetter
}

type LitsConfig = {
  initialCache?: Record<string, Ast>
  astCacheSize?: number | null
  debug?: boolean
}

export class Lits {
  private astCache: Cache | null
  private astCacheSize: number | null
  private debug: boolean

  constructor(config: LitsConfig = {}) {
    this.debug = config.debug ?? false
    this.astCacheSize = config.astCacheSize ?? null
    if (this.astCacheSize !== 0) {
      this.astCache = new Cache(this.astCacheSize)
      const initialCache = config.initialCache ?? {}
      for (const cacheEntry of Object.keys(initialCache)) {
        this.astCache.set(cacheEntry, initialCache[cacheEntry] as Ast)
      }
    } else {
      this.astCache = null
    }
  }

  public getRuntimeInfo(): LitsRuntimeInfo {
    return {
      astCacheSize: this.astCacheSize,
      astCache: this.astCache,
      debug: this.debug,
    }
  }

  public run(program: string, params: LitsParams = {}): unknown {
    const ast = this.generateAst(program, params.getLocation)
    const result = this.evaluate(ast, params)
    return result
  }

  public context(program: string, params: LitsParams = {}): Context {
    const contextStack = createContextStackFromParams(params)
    const ast = this.generateAst(program, params.getLocation)
    evaluate(ast, contextStack)
    return contextStack.globalContext
  }

  public analyze(program: string): AnalyzeResult {
    const params: LitsParams = {}
    const contextStack = createContextStackFromParams(params)
    const ast = this.generateAst(program, params.getLocation)

    return analyzeAst(ast.body, contextStack, builtin)
  }

  public tokenize(program: string, getLocation?: LocationGetter): Token[] {
    return tokenize(program, { debug: this.debug, getLocation })
  }

  public parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  private evaluate(ast: Ast, params: LitsParams): Any {
    const contextStack = createContextStackFromParams(params)
    return evaluate(ast, contextStack)
  }

  public apply(fn: LitsFunction, fnParams: unknown[], params: LitsParams = {}): Any {
    const fnName = `FN_2eb7b316-471c-5bfa-90cb-d3dfd9164a59`
    const paramsString: string = fnParams
      .map((_, index) => {
        return `${fnName}_${index}`
      })
      .join(` `)
    const program = `(${fnName} ${paramsString})`
    const ast = this.generateAst(program, params.getLocation)

    const globals: Obj = fnParams.reduce(
      (result: Obj, param, index) => {
        result[`${fnName}_${index}`] = param
        return result
      },
      { [fnName]: fn },
    )

    params.globals = { ...params.globals, ...globals }

    return this.evaluate(ast, params)
  }

  private generateAst(untrimmedProgram: string, getLocation: LocationGetter | undefined): Ast {
    const program = untrimmedProgram.trim()

    if (this.astCache) {
      const cachedAst = this.astCache.get(program)
      if (cachedAst) {
        return cachedAst
      }
    }
    const tokens: Token[] = this.tokenize(program, getLocation)
    const ast: Ast = this.parse(tokens)
    this.astCache?.set(program, ast)
    return ast
  }
}

function createContextStackFromParams(params: LitsParams): ContextStack {
  const globalContext: Context = params.globalContext ?? {}
  Object.assign(globalContext, createContextFromValues(params.globals))
  const contextStack = createContextStack([globalContext, ...(params.contexts ?? [])])
  return contextStack
}
