/* v8 ignore next 1000 */
// Minimal entry point: core Lits class, types, and type guards.
// No modules or reference data included — import from '@mojir/lits/full'
// for the complete bundle, or import individual modules separately.
export {
  isBuiltinFunction,
  isLitsFunction,
  asLitsFunction,
  assertLitsFunction,
  isUserDefinedFunction,
  asUserDefinedFunction,
  assertUserDefinedFunction,
  isNativeJsFunction,
  asNativeJsFunction,
  assertNativeJsFunction,
} from './typeGuards/litsFunction'
export { type Arity } from './builtin/interface'
export { type LitsFunction, type NativeJsFunction } from './parser/types'
export type { Context } from './evaluator/interface'
export type { Ast } from './parser/types'
export type { SourceCodeInfo } from './tokenizer/token'
export type { Token, TokenType } from './tokenizer/token'
export { normalExpressionKeys, specialExpressionKeys } from './builtin'
export { Lits } from './Lits/Lits'
export type { LitsModule } from './builtin/modules/interface'
export type { LitsBundle } from './bundler/interface'
export { isLitsBundle } from './bundler/interface'
export { type LitsError, isLitsError } from './errors'
export type { ContextParams, FilePathParams, MinifyParams, PureParams, LitsRuntimeInfo, JsFunction } from './Lits/Lits'
export { isGrid, isMatrix, isVector } from './typeGuards/annotatedArrays'
export type { AutoCompleter } from './AutoCompleter/AutoCompleter'

// Effects — standalone functions and types
export { run, runSync } from './effects'
export type { EffectContext, EffectHandler, Handlers, RunResult, RunOptions, RunSyncOptions } from './effects'
