/* v8 ignore next 1000 */
// Minimal entry point: core Lits class, types, and type guards.
// No namespaces or reference data included — import from '@mojir/lits/full'
// for the complete bundle, or import individual namespaces separately.
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
export type { LitsNamespace } from './builtin/namespaces/interface'
export { type LitsError, isLitsError } from './errors'
export type { ContextParams, FilePathParams, MinifyParams, LitsRuntimeInfo, JsFunction } from './Lits/Lits'
export { isGrid, isMatrix, isVector } from './typeGuards/annotatedArrays'
export type { AutoCompleter } from './AutoCompleter/AutoCompleter'
