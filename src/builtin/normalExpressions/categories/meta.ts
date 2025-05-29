import type { FunctionReference } from '../../../../reference'
import { assertNonUndefined } from '../../../typeGuards'
import { assertFunctionLike } from '../../../typeGuards/lits'
import { isLitsFunction } from '../../../typeGuards/litsFunction'
import { toFixedArity } from '../../../utils/arity'
import { generateDocString } from '../../../utils/docString/generateDocString'
import type { Arity, BuiltinNormalExpressions } from '../../interface'

export function getMetaNormalExpression(normalExpressionReference: Record<string, FunctionReference>): BuiltinNormalExpressions {
  return {
    doc: {
      evaluate: ([fn], sourceCodeInfo): string => {
        assertNonUndefined(normalExpressionReference)
        assertFunctionLike(fn, sourceCodeInfo)
        if (!isLitsFunction(fn)) {
          return ''
        }
        if (fn.functionType === 'Builtin') {
          const reference = normalExpressionReference[fn.name]!
          return generateDocString(reference)
        }
        if (fn.functionType === 'UserDefined' || fn.functionType === 'NativeJsFunction') {
          return fn.docString
        }
        return ''
      },
      arity: toFixedArity(1),
    },
    arity: {
      evaluate: ([fn], sourceCodeInfo): Arity => {
        assertFunctionLike(fn, sourceCodeInfo)
        return isLitsFunction(fn) ? fn.arity : toFixedArity(1)
      },
      arity: toFixedArity(1),
    },
  }
}
