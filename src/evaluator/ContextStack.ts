import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
import { allNormalExpressions } from '../builtin/normalExpressions'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { LitsError, UndefinedSymbolError } from '../errors'
import type { Any } from '../interface'
import type { ContextParams } from '../Lits/Lits'
import type { NativeJsFunction, NormalBuiltinFunction, SymbolNode, UserDefinedSymbolNode } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asNonUndefined } from '../typeGuards'
import { isNormalBuiltinSymbolNode, isSpecialBuiltinSymbolNode } from '../typeGuards/astNode'
import { toAny } from '../utils'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { Context, LookUpResult } from './interface'
import { isContextEntry } from './interface'

export type ContextStack = ContextStackImpl

export class ContextStackImpl {
  private contexts: Context[]
  public globalContext: Context
  private values?: Record<string, unknown>
  private nativeJsFunctions?: Record<string, NativeJsFunction>
  constructor({
    contexts,
    values: hostValues,
    nativeJsFunctions,
  }: {
    contexts: Context[]
    values?: Record<string, unknown>
    nativeJsFunctions?: Record<string, NativeJsFunction>
  }) {
    this.globalContext = asNonUndefined(contexts[0])
    this.contexts = contexts
    this.values = hostValues
    this.nativeJsFunctions = nativeJsFunctions
  }

  public create(context: Context): ContextStack {
    const globalContext = this.globalContext
    const contextStack = new ContextStackImpl({
      contexts: [context, ...this.contexts],
      values: this.values,
      nativeJsFunctions: this.nativeJsFunctions,
    })
    contextStack.globalContext = globalContext
    return contextStack
  }

  public new(context: Context): ContextStack {
    const contexts = [{}, context]

    return new ContextStackImpl({ contexts })
  }

  public exportValues(values: Record<string, Any>, sourceCodeInfo: SourceCodeInfo | undefined) {
    for (const [name, value] of Object.entries(values)) {
      if (this.globalContext[name]) {
        throw new LitsError(`Cannot redefine exported value "${name}"`, sourceCodeInfo)
      }
      if (specialExpressionKeys.includes(name)) {
        throw new LitsError(`Cannot shadow special expression "${name}"`, sourceCodeInfo)
      }
      if (normalExpressionKeys.includes(name)) {
        throw new LitsError(`Cannot shadow builtin function "${name}"`, sourceCodeInfo)
      }
      this.globalContext[name] = { value }
    }
    if (this.contexts[0] !== this.globalContext) {
      this.addValues(values, sourceCodeInfo)
    }
  }

  public addValues(values: Record<string, Any>, sourceCodeInfo: SourceCodeInfo | undefined) {
    const currentContext = this.contexts[0]!
    for (const [name, value] of Object.entries(values)) {
      if (currentContext[name]) {
        throw new LitsError(`Cannot redefine value "${name}"`, sourceCodeInfo)
      }
      if (specialExpressionKeys.includes(name)) {
        throw new LitsError(`Cannot shadow special expression "${name}"`, sourceCodeInfo)
      }
      if (normalExpressionKeys.includes(name)) {
        throw new LitsError(`Cannot shadow builtin function "${name}"`, sourceCodeInfo)
      }
      currentContext[name] = { value: toAny(value) }
    }
  }

  public getValue(name: string): unknown {
    for (const context of this.contexts) {
      const contextEntry = context[name]
      if (contextEntry)
        return contextEntry.value
    }

    const nativeJsFunction = this.nativeJsFunctions?.[name]
    if (nativeJsFunction)
      return nativeJsFunction

    return this.values?.[name]
  }

  public lookUp(node: UserDefinedSymbolNode): LookUpResult {
    const value = node[1]

    for (const context of this.contexts) {
      const contextEntry = context[value]
      if (contextEntry)
        return contextEntry
    }
    const hostValue = this.values?.[value]
    if (hostValue !== undefined) {
      return {
        value: toAny(hostValue),
      }
    }

    const nativeJsFunction = this.nativeJsFunctions?.[value]
    if (nativeJsFunction) {
      return {
        value: nativeJsFunction,
      }
    }

    return null
  }

  public evaluateSymbol(node: SymbolNode): Any {
    if (isSpecialBuiltinSymbolNode(node)) {
      const functionType = node[1]
      switch (functionType) {
        case specialExpressionTypes['&&']:
        case specialExpressionTypes['||']:
        case specialExpressionTypes.array:
        case specialExpressionTypes.object:
        case specialExpressionTypes['defined?']:
        case specialExpressionTypes.recur:
        case specialExpressionTypes.throw:
        case specialExpressionTypes['??']:
          return {
            [FUNCTION_SYMBOL]: true,
            functionType: 'SpecialBuiltin',
            specialBuiltinSymbolType: functionType,
            sourceCodeInfo: node[2],
          }
        default:
          throw new LitsError(`Unknown special builtin symbol type: ${functionType}`, node[2])
      }
    }
    if (isNormalBuiltinSymbolNode(node)) {
      const type = node[1]
      const normalExpression = allNormalExpressions[type]!
      return {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Builtin',
        normalBuitinSymbolType: type,
        sourceCodeInfo: node[2],
        paramCount: normalExpression.paramCount,
      } satisfies NormalBuiltinFunction
    }
    const lookUpResult = this.lookUp(node)

    if (isContextEntry(lookUpResult))
      return lookUpResult.value

    throw new UndefinedSymbolError(node[1], node[2])
  }
}

export function createContextStack(params: ContextParams = {}): ContextStack {
  const globalContext = params.globalContext ?? {}
  // Contexts are checked from left to right
  const contexts = params.contexts ? [globalContext, ...params.contexts] : [globalContext]
  const contextStack = new ContextStackImpl({
    contexts,
    values: params.values,
    nativeJsFunctions:
      params.jsFunctions
      && Object.entries(params.jsFunctions).reduce((acc: Record<string, NativeJsFunction>, [name, jsFunction]) => {
        if (specialExpressionKeys.includes(name)) {
          console.warn(`Cannot shadow special expression "${name}", ignoring.`)
          return acc
        }
        if (normalExpressionKeys.includes(name)) {
          console.warn(`Cannot shadow builtin function "${name}", ignoring.`)
          return acc
        }
        acc[name] = {
          functionType: 'NativeJsFunction',
          nativeFn: jsFunction,
          name,
          [FUNCTION_SYMBOL]: true,
          paramCount: jsFunction.paramCount ?? {},
        }
        return acc
      }, {}),
  })
  return params.globalModuleScope ? contextStack : contextStack.create({})
}
