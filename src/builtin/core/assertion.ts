import type { Any } from '../../interface'
import { AssertionError } from '../../errors'
import { asAny } from '../../typeGuards/lits'
import { assertString } from '../../typeGuards/string'
import type { BuiltinNormalExpressions } from '../interface'

export const assertionNormalExpression: BuiltinNormalExpressions = {
  assert: {
    evaluate: (params, sourceCodeInfo): Any => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      assertString(message, sourceCodeInfo)
      if (!value)
        throw new AssertionError(message, sourceCodeInfo)

      return asAny(value, sourceCodeInfo)
    },
    arity: { min: 1, max: 2 },
    docs: {
      category: 'assertion',
      description: 'If $value is falsy it throws `AssertionError` with $message. If no $message is provided, message is set to $value.',
      returns: {
        type: 'any',
      },
      args: {
        value: {
          type: 'any',
        },
        message: {
          type: 'string',
        },
      },
      variants: [
        {
          argumentNames: [
            'value',
          ],
        },
        {
          argumentNames: [
            'value',
            'message',
          ],
        },
      ],
      examples: [
        'try assert(0, "Expected a positive value") catch (e) e.message end',
      ],
      seeAlso: ['assertion.assert-truthy', 'assertion.assert-true'],
      hideOperatorForm: true,
    },
  },
}
