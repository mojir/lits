import { builtin, normalExpressionKeys, specialExpressionKeys } from '../builtin'
import { UndefinedSymbolError } from '../errors'
import type { Any } from '../interface'
import type { ContextParams, LazyValue } from '../Lits/Lits'
import type { BuiltinFunction, NativeJsFunction, SymbolNode } from '../parser/types'
import { asNonUndefined } from '../typeGuards'
import { isBuiltinFunction } from '../typeGuards/litsFunction'
import { toAny } from '../utils'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { Context, LookUpResult } from './interface'
import { isContextEntry } from './interface'

export type ContextStack = ContextStackImpl

export class ContextStackImpl {
  private contexts: Context[]
  public globalContext: Context
  private values?: Record<string, unknown>
  private lazyValues?: Record<string, LazyValue>
  private nativeJsFunctions?: Record<string, NativeJsFunction>
  constructor({
    contexts,
    values: hostValues,
    lazyValues: lazyHostValues,
    nativeJsFunctions,
  }: {
    contexts: Context[]
    values?: Record<string, unknown>
    lazyValues?: Record<string, LazyValue>
    nativeJsFunctions?: Record<string, NativeJsFunction>
  }) {
    this.globalContext = asNonUndefined(contexts[0])
    this.contexts = contexts
    this.values = hostValues
    this.lazyValues = lazyHostValues
    this.nativeJsFunctions = nativeJsFunctions
  }

  public create(context: Context): ContextStack {
    const globalContext = this.globalContext
    const contextStack = new ContextStackImpl({
      contexts: [context, ...this.contexts],
      values: this.values,
      lazyValues: this.lazyValues,
      nativeJsFunctions: this.nativeJsFunctions,
    })
    contextStack.globalContext = globalContext
    return contextStack
  }

  public new(context: Context): ContextStack {
    const contexts = [{}, context]

    return new ContextStackImpl({ contexts })
  }

  public exportValues(values: Record<string, Any>) {
    for (const [name, value] of Object.entries(values)) {
      if (this.globalContext[name]) {
        throw new Error(`Cannot redefine exported value "${name}"`)
      }
      if (specialExpressionKeys.includes(name)) {
        throw new Error(`Cannot shadow special expression "${name}"`)
      }
      if (normalExpressionKeys.includes(name)) {
        throw new Error(`Cannot shadow builtin function "${name}"`)
      }
      this.globalContext[name] = { value }
    }
    this.addValues(values)
  }

  public addValues(values: Record<string, Any>) {
    const currentContext = this.contexts[0]!
    for (const [name, value] of Object.entries(values)) {
      if (currentContext[name]) {
        throw new Error(`Cannot redefine value "${name}"`)
      }
      if (specialExpressionKeys.includes(name)) {
        throw new Error(`Cannot shadow special expression "${name}"`)
      }
      if (normalExpressionKeys.includes(name)) {
        throw new Error(`Cannot shadow builtin function "${name}"`)
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
    const lazyHostValue = this.lazyValues?.[name]
    if (lazyHostValue)
      return lazyHostValue.read()

    const nativeJsFunction = this.nativeJsFunctions?.[name]
    if (nativeJsFunction)
      return nativeJsFunction

    return this.values?.[name]
  }

  public lookUp(node: SymbolNode): LookUpResult {
    const value = node.value
    const sourceCodeInfo = node.sourceCodeInfo

    for (const context of this.contexts) {
      const contextEntry = context[value]
      if (contextEntry)
        return contextEntry
    }
    const lazyHostValue = this.lazyValues?.[value]
    if (lazyHostValue !== undefined) {
      return {
        value: toAny(lazyHostValue.read()),
      }
    }
    const hostValue = this.values?.[value]
    if (hostValue !== undefined) {
      return {
        value: toAny(hostValue),
      }
    }
    if (builtin.normalExpressions[value]) {
      const builtinFunction: BuiltinFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Builtin',
        n: value,
      }
      return builtinFunction
    }

    const nativeJsFunction = this.nativeJsFunctions?.[value]
    if (nativeJsFunction) {
      return {
        value: nativeJsFunction,
      }
    }

    return null
  }

  public evaluateName(node: SymbolNode): Any {
    const lookUpResult = this.lookUp(node)

    if (isContextEntry(lookUpResult))
      return lookUpResult.value
    else if (isBuiltinFunction(lookUpResult))
      return lookUpResult

    throw new UndefinedSymbolError(node.value, node.sourceCodeInfo)
  }
}

export function createContextStack(params: ContextParams = {}): ContextStack {
  const globalContext = params.globalContext ?? {}
  // Contexts are checked from left to right
  const contexts = params.contexts ? [globalContext, ...params.contexts] : [globalContext]
  const contextStack = new ContextStackImpl({
    contexts,
    values: params.values,
    lazyValues: params.lazyValues,
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
        }
        return acc
      }, {}),
  })
  return contextStack.create({})
}
