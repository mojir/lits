import type { LitsNamespace } from './builtin/namespaces/interface'
import { assertNamespace } from './builtin/namespaces/assert'
import { gridNamespace } from './builtin/namespaces/grid'
import { randomNamespace } from './builtin/namespaces/random'
import { vectorNamespace } from './builtin/namespaces/vector'
import { linearAlgebraNamespace } from './builtin/namespaces/linearAlgebra'
import { matrixNamespace } from './builtin/namespaces/matrix'
import { numberTheoryNamespace } from './builtin/namespaces/numberTheory'

export const allBuiltinNamespaces: LitsNamespace[] = [
  assertNamespace,
  gridNamespace,
  randomNamespace,
  vectorNamespace,
  linearAlgebraNamespace,
  matrixNamespace,
  numberTheoryNamespace,
]
