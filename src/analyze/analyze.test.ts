import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { getUndefinedSymbolNames } from '../../__tests__/testUtils'

describe('analyze', () => {
  const lits = new Lits()
  it('unresolvedIdentifiers', () => {
    expect(getUndefinedSymbolNames(lits.analyze('a + 10'))).toEqual(new Set(['a']))
    expect(getUndefinedSymbolNames(lits.analyze('let a = 10; a + 10'))).toEqual(new Set())
    expect(getUndefinedSymbolNames(lits.analyze('let a = 10; a + b'))).toEqual(new Set(['b']))
    expect(getUndefinedSymbolNames(lits.analyze('do let a = 10; a + 2; end; a'))).toEqual(new Set(['a']))
    expect(getUndefinedSymbolNames(lits.analyze('do let a = 10; a + b; end; a'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('let a = 10; "a" ++ "b"'))).toEqual(new Set())
    expect(getUndefinedSymbolNames(lits.analyze('foo(bar)'))).toEqual(new Set(['foo', 'bar']))
    expect(getUndefinedSymbolNames(lits.analyze('({bar = a + b })'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('{ bar = a + b }.bar'))).toEqual(new Set(['a', 'b']))
    expect(getUndefinedSymbolNames(lits.analyze('foo(d, E)'))).toEqual(new Set(['foo', 'd'])) // E is not reported due to that e is a builtin function: (e) -> 2.718281828459045
    expect(getUndefinedSymbolNames(lits.analyze('foo(d, f)'))).toEqual(new Set(['foo', 'd', 'f']))
    expect(
      getUndefinedSymbolNames(
        lits.analyze(`
          export let foo = [];
          let data1 = 1;
          let data2 = data1 + 1;
          let data3 = data2 + 1;
          data3`),
      ),
    ).toEqual(new Set([]))
  })
})
