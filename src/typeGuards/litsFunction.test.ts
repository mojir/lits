import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import type { LitsFunction } from '../parser/types'
import { createNativeJsFunction } from '../utils'
import { FUNCTION_SYMBOL } from '../utils/symbols'

import { normalExpressionTypes } from '../builtin/normalExpressions'
import {
  asLitsFunction,
  asNativeJsFunction,
  asUserDefinedFunction,
  assertLitsFunction,
  assertNativeJsFunction,
  assertUserDefinedFunction,
  isLitsFunction,
  isNativeJsFunction,
  isUserDefinedFunction,
} from './litsFunction'

describe('litsFunction type guards', () => {
  const lf1: LitsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'UserDefined',
    name: undefined,
    evaluatedfunction: [[], [], {}],
  }
  const lf2: LitsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Builtin',
    normalBuitinSymbolType: normalExpressionTypes['+'] as number,
  }
  const lf3: LitsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Partial',
    function: { a: 10, b: 20 },
    params: [],
  }
  const lf4: LitsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Comp',
    params: ['x'],
  }
  const lf5: LitsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Constantly',
    value: 10,
  }
  const lf6 = createNativeJsFunction(() => undefined)
  const lf7 = createNativeJsFunction(() => undefined, 'native')

  it('isLitsFunction', () => {
    const valid = [lf1, lf2, lf3, lf4, lf5, lf6, lf7]
    const invalid = ['', '1', 0, 1, true, false, null, undefined, [], {}]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isLitsFunction, as: asLitsFunction, assert: assertLitsFunction },
    )
  })

  it('isUserDefinedFunction', () => {
    const valid = [lf1]
    const invalid = [lf2, lf3, lf4, lf5, lf6, lf7, '', '1', 0, 1, true, false, null, undefined, [], {}]

    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isUserDefinedFunction, as: asUserDefinedFunction, assert: assertUserDefinedFunction },
    )
  })

  it('isNativeJsFunction', () => {
    const valid = [lf6, lf7]
    const invalid = [lf1, lf2, lf3, lf4, lf5, '', '1', 0, 1, true, false, null, undefined, [], {}]

    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isNativeJsFunction, as: asNativeJsFunction, assert: assertNativeJsFunction },
    )
  })
})
