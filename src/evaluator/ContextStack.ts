import type { BuiltinFunction, ExtraData, NameNode, NativeJsFunction } from '../parser/interface'
import { builtin } from '../builtin'
import { toAny } from '../utils'
import type { Any } from '../interface'
import { UndefinedSymbolError } from '../errors'
import type { LazyValue } from '../Lits/Lits'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import { FunctionType } from '../constants/constants'
import { asNonUndefined } from '../typeGuards'
import { isBuiltinFunction } from '../typeGuards/litsFunction'
import { isContextEntry } from './interface'
import type { Context, LookUpResult } from './interface'

export class ContextStack {
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
    this.contexts = contexts
    this.globalContext = asNonUndefined(contexts[0])
    this.values = hostValues
    this.lazyValues = lazyHostValues
    this.nativeJsFunctions = nativeJsFunctions
  }

  public create(context: Context, extraData?: ExtraData): ContextStack {
    const globalContext = this.globalContext
    const contextStack = new ContextStack({
      contexts: [context, ...this.contexts],
      values: this.values,
      lazyValues: extraData ? { ...this.lazyValues, ...extraData } : this.lazyValues,
      nativeJsFunctions: this.nativeJsFunctions,
    })
    contextStack.globalContext = globalContext
    return contextStack
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

  public lookUp(node: NameNode): LookUpResult {
    const value = node.v
    const sourceCodeInfo = node.tkn?.sourceCodeInfo

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
        t: FunctionType.Builtin,
        n: value,
      }
      return builtinFunction
    }

    if (builtin.specialExpressions[value])
      return 'specialExpression'

    const nativeJsFunction = this.nativeJsFunctions?.[value]
    if (nativeJsFunction) {
      return {
        value: nativeJsFunction,
      }
    }

    return null
  }

  public evaluateName(node: NameNode): Any {
    const lookUpResult = this.lookUp(node)

    if (isContextEntry(lookUpResult))
      return lookUpResult.value
    else if (isBuiltinFunction(lookUpResult))
      return lookUpResult

    throw new UndefinedSymbolError(node.v, node.tkn?.sourceCodeInfo)
  }
}
