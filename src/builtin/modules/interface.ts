import type { BuiltinNormalExpressions } from '../../builtin/interface'

/**
 * Represents a Lits module that can be imported dynamically.
 * Modules contain a collection of functions that are not part of the core bundle.
 */
export interface LitsModule {
  /** The name of the module (e.g., 'grid', 'vec', 'mat') */
  name: string
  /** The functions provided by this module, keyed by their short name (e.g., 'transpose') */
  functions: BuiltinNormalExpressions
}
