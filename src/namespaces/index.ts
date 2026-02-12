// Export the namespace interface
export type { LitsNamespace } from './interface'

// Export registry functions
export { registerNamespace, getNamespace, hasNamespace, getNamespaceNames } from './registry'

// Export individual namespaces
export { gridNamespace } from './grid'

// Register built-in namespaces
import { gridNamespace } from './grid'
import { registerNamespace } from './registry'

registerNamespace(gridNamespace)
