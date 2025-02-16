import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import { AstNodeType } from '../constants/constants'
import type { Seq } from '../interface'
import type { AstNode, RegularExpression } from '../parser/interface'
import { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'
import {
  asAny,
  asColl,
  asObj,
  asRegularExpression,
  asSeq,
  asStringOrRegularExpression,
  assertAny,
  assertColl,
  assertObj,
  assertRegularExpression,
  assertSeq,
  assertStringOrRegularExpression,
  isAny,
  isColl,
  isObj,
  isRegularExpression,
  isSeq,
  isStringOrRegularExpression,
} from './lits'

describe('lits type guards', () => {
  it('any', () => {
    const node: AstNode = {
      t: AstNodeType.Name,
      v: 'test',
      debugData: { token: ['Symbol', 'X'], lastToken: ['Symbol', 'X'] },
      p: [],
      n: undefined,
    }

    const valid = [node, 1, 'bar', null, [], {}]
    const invalid = [undefined]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isAny, as: asAny, assert: assertAny },
    )
  })

  it('obj', () => {
    const valid = [{}, { a: 1 }]
    const invalid = [0, { [FUNCTION_SYMBOL]: true }, /test/, [], [1], true, null, undefined]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isObj, as: asObj, assert: assertObj },
    )
  })

  it('regularExpression', () => {
    const regExp: RegularExpression = {
      [REGEXP_SYMBOL]: true,
      s: '^ab',
      f: '',
    }
    // eslint-disable-next-line prefer-regex-literals
    const invalid = [/a/, new RegExp('a'), 0, '0', null, undefined, false, true, [], {}]
    testTypeGuars(
      {
        valid: [regExp],
        invalid,
      },
      { is: isRegularExpression, as: asRegularExpression, assert: assertRegularExpression },
    )
  })

  it('seq', () => {
    const valid: Seq = ['', '1', [], [1, 2, 3]]
    const invalid = [0, 1, true, false, null, undefined, {}]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isSeq, as: asSeq, assert: assertSeq },
    )
  })

  it('stringOrRegularExpression', () => {
    const regExp: RegularExpression = {
      [REGEXP_SYMBOL]: true,
      s: '^ab',
      f: '',
    }
    const valid = ['', '1', regExp]
    const invalid = [/^a/, [], [1, 2, 3], 0, 1, true, false, null, undefined, {}]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isStringOrRegularExpression, as: asStringOrRegularExpression, assert: assertStringOrRegularExpression },
    )
  })

  it('coll', () => {
    const valid = ['2', { a: 1 }, [2]]
    const invalid = [0, null, true, false]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isColl, as: asColl, assert: assertColl },
    )
  })
})
