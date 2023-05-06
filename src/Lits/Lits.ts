import { LitsFunction } from '..'
import { findUndefinedSymbols } from '../analyze/undefinedSymbols'
import { UndefinedSymbolEntry } from '../analyze/undefinedSymbols/interface'
import { builtin } from '../builtin'
import { ContextStack } from '../ContextStack'
import { Context } from '../ContextStack/interface'
import { evaluate } from '../evaluator'
import { Any, Obj } from '../interface'
import { parse } from '../parser'
import { Ast } from '../parser/interface'
import { tokenize } from '../tokenizer'
import { Token } from '../tokenizer/interface'
import { Cache } from './Cache'
import { LitsParams, LocationGetter } from './interface'

export type LitsRuntimeInfo = {
  astCache: Cache | null
  astCacheSize: number | null
  debug: boolean
}

export { Type } from '../types/Type'

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
    const contextStack = ContextStack.createFromParams(params)
    const ast = this.generateAst(program, params.getLocation)
    evaluate(ast, contextStack)
    return contextStack.globalContext
  }

  public findUndefinedSymbols(program: string): Set<UndefinedSymbolEntry> {
    const params: LitsParams = {}
    const contextStack = ContextStack.createFromParams(params)
    const ast = this.generateAst(program, params.getLocation)

    return findUndefinedSymbols(ast.body, contextStack, builtin)
  }

  public tokenize(program: string, getLocation?: LocationGetter): Token[] {
    return tokenize(program, { debug: this.debug, getLocation })
  }

  public parse(tokens: Token[]): Ast {
    return parse(tokens)
  }

  private evaluate(ast: Ast, params: LitsParams): Any {
    const contextStack = ContextStack.createFromParams(params)
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
