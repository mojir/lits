import type { BuiltinNormalExpressions } from '../builtin/interface'

/**
 * Represents a Lits namespace that can be imported dynamically.
 * Namespaces contain a collection of functions that are not part of the core bundle.
 */
export interface LitsNamespace {
  /** The name of the namespace (e.g., 'grid', 'vec', 'mat') */
  name: string
  /** The functions provided by this namespace, keyed by their full name (e.g., 'grid:transpose') */
  functions: BuiltinNormalExpressions
}
