import { getUndefinedSymbols } from '../getUndefinedSymbols'
import { evaluate, evaluateNode } from '../evaluator'
import { createContextStack } from '../evaluator/ContextStack'
import type { Context } from '../evaluator/interface'
import type { Any, Obj } from '../interface'
import type { Ast, LitsFunction, ParseState } from '../parser/types'
import { tokenize } from '../tokenizer/tokenize'
import type { TokenStream } from '../tokenizer/tokenize'
import { minifyTokenStream } from '../tokenizer/minifyTokenStream'
import { transformSymbolTokens } from '../transformer'
import { untokenize } from '../untokenizer'
import { builtin } from '../builtin'
import { Parser } from '../parser/Parser'
import { AutoCompleter } from '../AutoCompleter/AutoCompleter'
import type { Arity } from '../builtin/interface'
import type { LitsNamespace } from '../builtin/namespaces/interface'
import { assertNamespace } from '../builtin/namespaces/assert'
import { gridNamespace } from '../builtin/namespaces/grid'
import { randomNamespace } from '../builtin/namespaces/random'
import { vectorNamespace } from '../builtin/namespaces/vector'
import { linearAlgebraNamespace } from '../builtin/namespaces/linearAlgebra'
import { matrixNamespace } from '../builtin/namespaces/matrix'
import { numberTheoryNamespace } from '../builtin/namespaces/numberTheory'

import { normalExpressionReference } from '../../reference/index'
import { setNormalExpressionReference } from '../builtin/normalExpressions'
// import { deepEqual } from '../utils'
import { Cache } from './Cache'

setNormalExpressionReference(normalExpressionReference)

const allBuiltinNamespaces: LitsNamespace[] = [
  assertNamespace,
  gridNamespace,
  randomNamespace,
  vectorNamespace,
  linearAlgebraNamespace,
  matrixNamespace,
  numberTheoryNamespace,
]

export interface LitsRuntimeInfo {
  astCache: Cache | null
  astCacheSize: number | null
  debug: boolean
}

export interface JsFunction {
  fn: (...args: any[]) => unknown
  arity?: Arity
  docString?: string
}

export interface ContextParams {
  globalContext?: Context
  contexts?: Context[]
  values?: Record<string, unknown>
  jsFunctions?: Record<string, unknown>
  globalModuleScope?: boolean
}

export interface MinifyParams {
  minify?: boolean
}

export interface FilePathParams {
  filePath?: string
}

interface LitsConfig {
  initialCache?: Record<string, Ast>
  astCacheSize?: number | null
  debug?: boolean
  namespaces?: LitsNamespace[]
}

export class Lits {
  private astCache: Cache | null
  private astCacheSize: number | null
  private debug: boolean
  private namespaces: Map<string, LitsNamespace>

  constructor(config: LitsConfig = {}) {
    this.debug = config.debug ?? false
    this.astCacheSize = config.astCacheSize ?? null
    if (this.astCacheSize) {
      this.astCache = new Cache(this.astCacheSize)
      const initialCache = config.initialCache ?? {}
      for (const cacheEntry of Object.keys(initialCache))
        this.astCache.set(cacheEntry, initialCache[cacheEntry] as Ast)
    }
    else {
      this.astCache = null
    }
    const nsList = config.namespaces ?? allBuiltinNamespaces
    this.namespaces = new Map(nsList.map(ns => [ns.name, ns]))
  }

  public getRuntimeInfo(): LitsRuntimeInfo {
    return {
      astCacheSize: this.astCacheSize,
      astCache: this.astCache,
      debug: this.debug,
    }
  }

  public run(program: string, params: ContextParams & FilePathParams = {}): unknown {
    const ast = this.generateAst(program, params)
    const result = this.evaluate(ast, params)
    // const stringifiedResult = JSON.stringify(result)
    // const parsedResult = JSON.parse(stringifiedResult) as unknown
    // if (!deepEqual(result, parsedResult)) {
    //   throw new Error(`Result is not serializable: ${result} != ${parsedResult}\nstringifiedResult: ${stringifiedResult}\nprogram: ${program}`)
    // }
    return result
  }

  public context(programOrAst: string | Ast, params: ContextParams & FilePathParams = {}): Context {
    const ast = typeof programOrAst === 'string' ? this.generateAst(programOrAst, params) : programOrAst
    const contextStack = createContextStack(params, this.namespaces)
    evaluate(ast, contextStack)
    return contextStack.globalContext
  }

  public getUndefinedSymbols(programOrAst: string | Ast, params: ContextParams = {}): Set<string> {
    const ast = typeof programOrAst === 'string' ? this.generateAst(programOrAst, params) : programOrAst
    const contextStack = createContextStack(params, this.namespaces)
    return getUndefinedSymbols(ast, contextStack, builtin, evaluateNode)
  }

  public tokenize(program: string, tokenizeParams: FilePathParams & MinifyParams = {}): TokenStream {
    const tokenStream = tokenize(program, this.debug, tokenizeParams.filePath)
    return tokenizeParams.minify ? minifyTokenStream(tokenStream, { removeWhiteSpace: false }) : tokenStream
  }

  public parse(tokenStream: TokenStream): Ast {
    tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
    const ast: Ast = {
      body: [],
      hasDebugData: tokenStream.hasDebugData,
    }

    const parseState: ParseState = {
      position: 0,
    }

    ast.body = new Parser(tokenStream, parseState).parse()

    return ast
  }

  public evaluate(ast: Ast, params: ContextParams): Any {
    const contextStack = createContextStack(params, this.namespaces)
    return evaluate(ast, contextStack)
  }

  public transformSymbols(tokenStream: TokenStream, transformer: (symbol: string) => string): TokenStream {
    return transformSymbolTokens(tokenStream, transformer)
  }

  public untokenize(tokenStream: TokenStream): string {
    return untokenize(tokenStream)
  }

  public apply(fn: LitsFunction, fnParams: unknown[], params: ContextParams = {}): Any {
    const fnName = 'FN_2eb7b316_471c_5bfa_90cb_d3dfd9164a59'
    const program = this.generateApplyFunctionCall(fnName, fnParams)

    const ast = this.generateAst(program, params)

    const hostValues: Obj = fnParams.reduce(
      (result: Obj, param, index) => {
        result[`${fnName}_${index}`] = param
        return result
      },
      { [fnName]: fn },
    )

    params.values = { ...params.values, ...hostValues }

    return this.evaluate(ast, params)
  }

  private generateApplyFunctionCall(fnName: string, fnParams: unknown[]) {
    const paramsString: string = fnParams
      .map((_, index) => {
        return `${fnName}_${index}`
      })
      .join(', ')
    return `${fnName}(${paramsString})`
  }

  private generateAst(program: string, params: ContextParams & FilePathParams): Ast {
    if (this.astCache) {
      const cachedAst = this.astCache.get(program)
      if (cachedAst)
        return cachedAst
    }
    const tokenStream = this.tokenize(program, {
      filePath: params.filePath,
    })
    const ast: Ast = this.parse(tokenStream)
    this.astCache?.set(program, ast)
    return ast
  }

  public getAutoCompleter(program: string, position: number, params: ContextParams = {}): AutoCompleter {
    return new AutoCompleter(program, position, this, params)
  }
}
