// Export the namespace interface
export type { LitsNamespace } from './interface'

// Export registry functions
export { registerNamespace, getNamespace, hasNamespace, getNamespaceNames } from './registry'

// Export individual namespaces
export { gridNamespace } from './grid'
export { randomNamespace } from './random'
export { vectorNamespace } from './vector'

// Register built-in namespaces
import { gridNamespace } from './grid'
import { randomNamespace } from './random'
import { vectorNamespace } from './vector'
import { registerNamespace } from './registry'

registerNamespace(gridNamespace)
registerNamespace(randomNamespace)
registerNamespace(vectorNamespace)
