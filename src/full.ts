/* v8 ignore next 1000 */
// Full entry point: re-exports everything from the minimal entry plus
// all modules, reference data, and API helpers.

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
export type { LitsModule } from './builtin/modules/interface'
export { type LitsError, isLitsError } from './errors'
export type { ContextParams, FilePathParams, MinifyParams, LitsRuntimeInfo, JsFunction } from './Lits/Lits'
export { isGrid, isMatrix, isVector } from './typeGuards/annotatedArrays'
export type { AutoCompleter } from './AutoCompleter/AutoCompleter'

// All built-in modules (convenience re-export)
export { allBuiltinModules } from './allModules'

// Individual module re-exports
export { assertModule } from './builtin/modules/assert'
export { gridModule } from './builtin/modules/grid'
export { randomModule } from './builtin/modules/random'
export { vectorModule } from './builtin/modules/vector'
export { linearAlgebraModule } from './builtin/modules/linearAlgebra'
export { matrixModule } from './builtin/modules/matrix'
export { numberTheoryModule } from './builtin/modules/numberTheory'

// Reference data and types
export { apiReference, isCustomReference, isDatatypeReference, isFunctionReference, isShorthandReference } from '../reference'
export type { Argument, CommonReference, CustomReference, DatatypeReference, FunctionReference, Reference, ShorthandReference } from '../reference'
export type { ApiName, FunctionName, ShorthandName, DatatypeName } from '../reference/api'
export { isApiName, isDataType } from '../reference/api'
