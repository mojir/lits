/* v8 ignore next 1000 */
// Full entry point: re-exports everything from the minimal entry plus
// all namespaces, reference data, and API helpers.

// Re-export everything from the minimal entry point
// Wire up reference data for the `doc` builtin function.
// In the minimal entry point (src/index.ts), this is not called,
// so `doc` gracefully returns empty strings for built-in functions.
import './initReferenceData'

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

// All built-in namespaces (convenience re-export)
export { allBuiltinNamespaces } from './allNamespaces'

// Individual namespace re-exports
export { assertNamespace } from './builtin/namespaces/assert'
export { gridNamespace } from './builtin/namespaces/grid'
export { randomNamespace } from './builtin/namespaces/random'
export { vectorNamespace } from './builtin/namespaces/vector'
export { linearAlgebraNamespace } from './builtin/namespaces/linearAlgebra'
export { matrixNamespace } from './builtin/namespaces/matrix'
export { numberTheoryNamespace } from './builtin/namespaces/numberTheory'

// Reference data and types
export { apiReference, isCustomReference, isDatatypeReference, isFunctionReference, isShorthandReference } from '../reference'
export type { Argument, CommonReference, CustomReference, DatatypeReference, FunctionReference, Reference, ShorthandReference } from '../reference'
export type { ApiName, FunctionName, ShorthandName, DatatypeName } from '../reference/api'
export { isApiName, isDataType } from '../reference/api'
