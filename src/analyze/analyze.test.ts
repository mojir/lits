import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { getUndefinedSymbolNames } from '../../__tests__/testUtils'

describe('analyze', () => {
  const lits = new Lits({ polish: true })
  it('unresolvedIdentifiers', () => {
    expect(getUndefinedSymbolNames(lits.analyze('(+ a 10)'))).toEqual(new Set(['a']))
    expect(getUndefinedSymbolNames(lits.analyze('(def a 10) (+ a 10)'))).toEqual(new Set())
    expect(getUndefinedSymbolNames(lits.analyze('(let [a 10]) (+ a b)'))).toEqual(new Set(['b']))
    expect(getUndefinedSymbolNames(lits.analyze('(do (let [a 10]) (+ a 2)) a'))).toEqual(new Set(['a']))
    expect(getUndefinedSymbolNames(lits.analyze('(do (let [a 10]) (+ a b)) a'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('(let [a 10]) (str :a :b)'))).toEqual(new Set())
    expect(getUndefinedSymbolNames(lits.analyze('(foo bar)'))).toEqual(new Set(['foo', 'bar']))
    expect(getUndefinedSymbolNames(lits.analyze('({:bar (+ a b)})'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('({:bar (+ a b)} :bar)'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('(:bar {:bar (+ a b)})'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('(foo d e)'))).toEqual(new Set(['foo', 'd'])) // e is not reported due to that e is a builtin function: (e) -> 2.718281828459045
    expect(getUndefinedSymbolNames(lits.analyze('(foo d f)'))).toEqual(new Set(['foo', 'd', 'f']))
    expect(
      getUndefinedSymbolNames(
        lits.analyze(`
          (defn foo []
            (let [ data1 1,
                   data2 (+ data1 1),
                   data3 (+ data2 1)])
            data3)`),
      ),
    ).toEqual(new Set([]))
  })
})
