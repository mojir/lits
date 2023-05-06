import { Lits } from '../../src'
import { getUndefinedSymbolNames } from '../testUtils'

describe(`getUndefinedSymbols`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    test(`getUndefinedSymbols`, () => {
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(+ a 10)`))).toEqual(new Set([`a`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(def a 10) (+ a 10)`))).toEqual(new Set())
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(let [a 10] (+ a b))`))).toEqual(new Set([`b`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(let [a 10] (+ a b)) a`))).toEqual(new Set([`a`, `b`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(let [a 10] (str :a :b))`))).toEqual(new Set())
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(foo bar)`))).toEqual(new Set([`foo`, `bar`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`({:bar (+ a b)})`))).toEqual(new Set([`a`, `b`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`({:bar (+ a b)} :bar)`))).toEqual(new Set([`a`, `b`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(:bar {:bar (+ a b)})`))).toEqual(new Set([`a`, `b`]))
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(foo d e)`))).toEqual(new Set([`foo`, `d`])) // e is not reported due to that e is a builtin function: (e) -> 2.718281828459045
      expect(getUndefinedSymbolNames(lits.findUndefinedSymbols(`(foo d f)`))).toEqual(new Set([`foo`, `d`, `f`]))
      expect(
        getUndefinedSymbolNames(
          lits.findUndefinedSymbols(`(defn foo [] (let [data1 1, data2 (+ data1 1) data3 (+ data2 1)] data3))`),
        ),
      ).toEqual(new Set([]))
    })
  }
})
