import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { builtin } from '../builtin'
import { getUndefinedSymbolNames } from '../../__tests__/testUtils'
import { createContextStack } from '../evaluator/ContextStack'
import { findUnresolvedIdentifiers } from './findUnresolvedIdentifiers'
import { calculateOutcomes } from './calculateOutcomes'
import type { Analysis } from '.'

describe('analyze', () => {
  describe('findUnresolvedIdentifiers.', () => {
    for (const lits of [new Lits(), new Lits({ debug: true })]) {
      it('example', () => {
        const program = '(+ a b)'
        const tokens = lits.tokenize(program)
        const ast = lits.parse(tokens)
        const analyzeResult: Analysis = {
          unresolvedIdentifiers: findUnresolvedIdentifiers(ast, createContextStack(), builtin),
          outcomes: [],
        }
        expect(getUndefinedSymbolNames(analyzeResult)).toEqual(new Set(['a', 'b']))
      })
    }
  })
  describe('calculateOutcomes.', () => {
    const lits = new Lits()
    const testSamples: [string, null | unknown[]][] = [
      ['(if true "heads" "tails")', ['heads']],
      // ['(if false "heads" "tails")', ['tails']],
      // ['(str "heads" (if true :- :_) "tails")', ['heads-tails', 'heads_tails']],
      // ['(str "heads" (if true x :_) "tails")', null],
      // ['(write! :foo)', ['foo']],
      // ['(cond 1 :1, 2 :2, 3 :3, :else :X)', ['1']],
      // [`(let
      //     [x (cond 1 :1 2 :2 3 :3)
      //      y (if true :A :B)]
      //     (str x (when x y)))`, ['1A', '1']],
      // [`(let [x (rand!)]
      //     (cond
      //       (< x 0.25) :A
      //       (< x 0.5) :B
      //       (< x 0.75) :C
      //       :else :D))`, ['A', 'B', 'C', 'D']],
      // [`(let [x foo]
      //       (cond
      //         (< x 0.25) :A
      //         (< x 0.5) :B
      //         (< x 0.75) :C
      //         :else :D))`, ['A', 'B', 'C', 'D']],
      // [`(let [x 0.3]
      //           (cond
      //             (< x 0.25) :A
      //             (< x 0.5) :B
      //             (< x 0.75) :C
      //             :else :D))`, ['B']],
    ]

    testSamples.forEach(([program, expectedOutcomes]) => {
      it(`${program} -> ${JSON.stringify(expectedOutcomes)}`, () => {
        const tokens = lits.tokenize(program)
        const ast = lits.parse(tokens)
        const contextStack = createContextStack()
        if (expectedOutcomes === null)
          expect(calculateOutcomes(contextStack, ast.b)).toBeNull()
        else
          expect(calculateOutcomes(contextStack, ast.b)).toEqual(expectedOutcomes)
      })
    })
  })
})
