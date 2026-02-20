/* v8 ignore next 1000 */
// Full entry point: re-exports everything from the minimal entry plus
// all modules, reference data, and API helpers.

// Re-export everything from the minimal entry point
// Wire up reference data for the `doc` builtin function.
// In the minimal entry point (src/index.ts), this is not called,
// so `doc` gracefully returns empty strings for built-in functions.
import './initReferenceData'

export * from './index'

// All built-in modules (convenience re-export)
export { allBuiltinModules } from './allModules'

// Individual module re-exports
export { assertModule } from './builtin/modules/assert'
export { gridModule } from './builtin/modules/grid'
export { randomModule } from './builtin/modules/random'
export { vectorModule } from './builtin/modules/vector'
export { linearAlgebraModule } from './builtin/modules/linear-algebra'
export { matrixModule } from './builtin/modules/matrix'
export { numberTheoryModule } from './builtin/modules/number-theory'
export { mathUtilsModule } from './builtin/modules/math'
export { functionalUtilsModule } from './builtin/modules/functional'
export { bitwiseUtilsModule } from './builtin/modules/bitwise'

// Reference data and types
export { apiReference, isCustomReference, isDatatypeReference, isFunctionReference, isShorthandReference } from '../reference'
export type { Argument, CommonReference, CustomReference, DatatypeReference, FunctionReference, Reference, ShorthandReference } from '../reference'
export type { ApiName, FunctionName, ShorthandName, DatatypeName } from '../reference/api'
export { isApiName, isDataType } from '../reference/api'
