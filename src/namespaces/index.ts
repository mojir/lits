// Export the namespace interface
// Register built-in namespaces
import { gridNamespace } from './grid'
import { randomNamespace } from './random'
import { vectorNamespace } from './vector'
import { linearAlgebraNamespace } from './linearAlgebra'
import { matrixNamespace } from './matrix'
import { numberTheoryNamespace } from './numberTheory'
import { registerNamespace } from './registry'

export type { LitsNamespace } from './interface'

// Export registry functions
export { registerNamespace, getNamespace, hasNamespace, getNamespaceNames } from './registry'

// Export individual namespaces
export { gridNamespace } from './grid'
export { randomNamespace } from './random'
export { vectorNamespace } from './vector'
export { linearAlgebraNamespace } from './linearAlgebra'
export { matrixNamespace } from './matrix'
export { numberTheoryNamespace } from './numberTheory'

registerNamespace(gridNamespace)
registerNamespace(randomNamespace)
registerNamespace(vectorNamespace)
registerNamespace(linearAlgebraNamespace)
registerNamespace(matrixNamespace)
registerNamespace(numberTheoryNamespace)
