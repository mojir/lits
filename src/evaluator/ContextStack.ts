import type { SpecialExpression } from '../builtin'
import { builtin, normalExpressionKeys, specialExpressionKeys } from '../builtin'
import { allNormalExpressions } from '../builtin/normalExpressions'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { LitsError, UndefinedSymbolError } from '../errors'
import type { Any } from '../interface'
import type { ContextParams, JsFunction } from '../Lits/Lits'
import type { LitsModule } from '../builtin/modules/interface'
import { type NativeJsFunction, type NormalBuiltinFunction, type SpecialBuiltinFunction, type SymbolNode, type UserDefinedSymbolNode, isJsFunction } from '../parser/types'
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
  private modules: Map<string, LitsModule>
  private valueModules: Map<string, unknown>
  constructor({
    contexts,
    values: hostValues,
    nativeJsFunctions,
    modules,
    valueModules,
  }: {
    contexts: Context[]
    values?: Record<string, unknown>
    nativeJsFunctions?: Record<string, NativeJsFunction>
    modules?: Map<string, LitsModule>
    valueModules?: Map<string, unknown>
  }) {
    this.globalContext = asNonUndefined(contexts[0])
    this.contexts = contexts
    this.values = hostValues
    this.nativeJsFunctions = nativeJsFunctions
    this.modules = modules ?? new Map<string, LitsModule>()
    this.valueModules = valueModules ?? new Map<string, unknown>()
  }

  public getModule(name: string): LitsModule | undefined {
    return this.modules.get(name)
  }

  public getValueModule(name: string): { value: unknown, found: boolean } {
    if (this.valueModules.has(name)) {
      return { value: this.valueModules.get(name), found: true }
    }
    return { value: undefined, found: false }
  }

  public registerValueModule(name: string, value: unknown): void {
    this.valueModules.set(name, value)
  }

  public create(context: Context): ContextStack {
    const globalContext = this.globalContext
    const contextStack = new ContextStackImpl({
      contexts: [context, ...this.contexts],
      values: this.values,
      nativeJsFunctions: this.nativeJsFunctions,
      modules: this.modules,
      valueModules: this.valueModules,
    })
    contextStack.globalContext = globalContext
    return contextStack
  }

  public new(context: Context): ContextStack {
    const contexts = [{}, context]

    return new ContextStackImpl({ contexts, modules: this.modules, valueModules: this.valueModules })
  }

  public addValues(values: Record<string, Any>, sourceCodeInfo: SourceCodeInfo | undefined) {
    const currentContext = this.contexts[0]!
    for (const [name, value] of Object.entries(values)) {
      if (currentContext[name]) {
        throw new LitsError(`Cannot redefine value "${name}"`, sourceCodeInfo)
      }
      const shadowedName = getShadowedBuiltinName(name)
      if (shadowedName) {
        throw new LitsError(`Cannot shadow ${shadowedName}`, sourceCodeInfo)
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
        case specialExpressionTypes['??']: {
          const specialExpression: SpecialExpression = asNonUndefined(builtin.specialExpressions[functionType], node[2])
          return {
            [FUNCTION_SYMBOL]: true,
            functionType: 'SpecialBuiltin',
            specialBuiltinSymbolType: functionType,
            sourceCodeInfo: node[2],
            arity: specialExpression.arity,
          } satisfies SpecialBuiltinFunction
        }
        default:
          throw new LitsError(`Unknown special builtin symbol type: ${functionType}`, node[2])
      }
    }
    if (isNormalBuiltinSymbolNode(node)) {
      const type = node[1]
      const normalExpression = allNormalExpressions[type]!
      const name = normalExpression.name!
      return {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Builtin',
        normalBuiltinSymbolType: type,
        sourceCodeInfo: node[2],
        arity: normalExpression.arity,
        name,
      } satisfies NormalBuiltinFunction
    }
    const lookUpResult = this.lookUp(node)

    if (isContextEntry(lookUpResult))
      return lookUpResult.value

    throw new UndefinedSymbolError(node[1], node[2])
  }
}

function getShadowedBuiltinName(name: string): string | null {
  if (specialExpressionKeys.includes(name))
    return `special expression "${name}"`
  if (normalExpressionKeys.includes(name))
    return `builtin function "${name}"`
  if (name === 'self')
    return `builtin value "${name}"`
  return null
}

function assertNotShadowingBuiltin(name: string): void {
  const shadowedName = getShadowedBuiltinName(name)
  if (shadowedName) {
    throw new LitsError(`Cannot shadow ${shadowedName}`, undefined)
  }
}

export function createContextStack(params: ContextParams = {}, modules?: Map<string, LitsModule>): ContextStack {
  const globalContext = params.globalContext ?? {}
  // Contexts are checked from left to right
  const contexts = params.contexts ? [globalContext, ...params.contexts] : [globalContext]

  // Process bindings: separate plain values from JS functions
  let hostValues: Record<string, unknown> | undefined
  let nativeJsFunctions: Record<string, NativeJsFunction> | undefined

  if (params.bindings) {
    for (const [identifier, entry] of Object.entries(params.bindings)) {
      if (identifier.includes('.')) {
        throw new LitsError(`Dots are not allowed in binding keys: "${identifier}"`, undefined)
      }

      const isFunction = typeof entry === 'function'
      const isJsFunctionObject = isJsFunction(entry)

      if (isFunction || isJsFunctionObject) {
        const jsFunction: JsFunction = isJsFunctionObject ? entry : { fn: entry as (...args: any[]) => unknown }

        assertNotShadowingBuiltin(identifier)
        if (!nativeJsFunctions) {
          nativeJsFunctions = {}
        }
        nativeJsFunctions[identifier] = {
          functionType: 'NativeJsFunction',
          nativeFn: jsFunction,
          name: identifier,
          [FUNCTION_SYMBOL]: true,
          arity: jsFunction.arity ?? {},
          docString: jsFunction.docString ?? '',
        } satisfies NativeJsFunction
      }
      else {
        // Plain value binding
        assertNotShadowingBuiltin(identifier)
        if (!hostValues) {
          hostValues = {}
        }
        hostValues[identifier] = entry
      }
    }
  }

  const contextStack = new ContextStackImpl({
    contexts,
    values: hostValues,
    modules,
    nativeJsFunctions,
  })
  return params.globalModuleScope ? contextStack : contextStack.create({})
}
