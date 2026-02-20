import type { LitsModule } from './builtin/modules/interface'
import { assertModule } from './builtin/modules/assert'
import { gridModule } from './builtin/modules/grid'
import { randomModule } from './builtin/modules/random'
import { vectorModule } from './builtin/modules/vector'
import { linearAlgebraModule } from './builtin/modules/linearAlgebra'
import { matrixModule } from './builtin/modules/matrix'
import { numberTheoryModule } from './builtin/modules/numberTheory'
import { mathUtilsModule } from './builtin/modules/mathUtils'
import { functionalUtilsModule } from './builtin/modules/functionalUtils'
import { stringUtilsModule } from './builtin/modules/stringUtils'
import { collectionUtilsModule } from './builtin/modules/collectionUtils'
import { sequenceUtilsModule } from './builtin/modules/sequenceUtils'
import { bitwiseUtilsModule } from './builtin/modules/bitwiseUtils'

export const allBuiltinModules: LitsModule[] = [
  assertModule,
  gridModule,
  randomModule,
  vectorModule,
  linearAlgebraModule,
  matrixModule,
  numberTheoryModule,
  mathUtilsModule,
  functionalUtilsModule,
  stringUtilsModule,
  collectionUtilsModule,
  sequenceUtilsModule,
  bitwiseUtilsModule,
]
