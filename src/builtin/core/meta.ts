import type { FunctionReference } from '../../../reference'
import { assertNonUndefined } from '../../typeGuards'
import { assertFunctionLike } from '../../typeGuards/lits'
import { isLitsFunction } from '../../typeGuards/litsFunction'
import { toFixedArity } from '../../utils/arity'
import { generateDocString } from '../../utils/docString/generateDocString'
import type { Arity, BuiltinNormalExpressions } from '../interface'

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
      docs: {
        category: 'Meta',
        returns: { type: 'string' },
        args: { fun: { type: 'function' } },
        variants: [{ argumentNames: ['fun'] }],
        description: 'Returns documentation string of the $fun.',
        examples: [
          'doc(+)',
          `
let add = (x, y) -> {
  """
  Adds two numbers.
  Args:
    x: First number.
    y: Second number.
  Returns:
    Sum of x and y.
  """
  x + y;
};

doc(add)`,
        ],
      },
    },
    arity: {
      evaluate: ([fn], sourceCodeInfo): Arity => {
        assertFunctionLike(fn, sourceCodeInfo)
        return isLitsFunction(fn) ? fn.arity : toFixedArity(1)
      },
      arity: toFixedArity(1),
      docs: {
        category: 'Meta',
        returns: { type: 'object' },
        args: { fun: { type: 'function' } },
        variants: [{ argumentNames: ['fun'] }],
        description: 'Returns arity of the $fun. The arity is an object with the properties: `min` and `max`. If the function has fixed arity, `min` and `max` are equal to the number of required parameters. If no restrictions apply, empty object is returned.',
        examples: [
          'arity(+)',
          'arity(defined?)',
          `
let add = (x, y = 0) -> {
  x + y;
};

arity(add)`,
          `
let foo = (k, ...x) -> {
  k + x;
};
  arity(foo)`,
        ],
      },
    },
  }
}
