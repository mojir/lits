import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

describe('analyze', () => {
  const lits = new Lits()
  it('unresolvedIdentifiers', () => {
    expect((lits.getUndefinedSymbols('a + 10'))).toEqual(new Set(['a']))
    expect((lits.getUndefinedSymbols('let a := 10; a + 10'))).toEqual(new Set())
    expect((lits.getUndefinedSymbols('let a := 10; a + b'))).toEqual(new Set(['b']))
    expect((lits.getUndefinedSymbols('do let a := 10; a + 2; end; a'))).toEqual(new Set(['a']))
    expect((lits.getUndefinedSymbols('do let a := 10; a + b; end; a'))).toEqual(new Set(['a', 'b']))
    expect((lits.getUndefinedSymbols('let a := 10; "a" ++ "b"'))).toEqual(new Set())
    expect((lits.getUndefinedSymbols('foo(bar)'))).toEqual(new Set(['foo', 'bar']))
    expect((lits.getUndefinedSymbols('({bar := a + b })'))).toEqual(new Set(['a', 'b']))
    expect((lits.getUndefinedSymbols('{ bar := a + b }.bar'))).toEqual(new Set(['a', 'b']))
    expect((lits.getUndefinedSymbols('foo(d, E)'))).toEqual(new Set(['foo', 'd'])) // E is not reported due to that e is a builtin function: (e) -> 2.718281828459045
    expect((lits.getUndefinedSymbols('foo(d, f)'))).toEqual(new Set(['foo', 'd', 'f']))
    expect(
      lits.getUndefinedSymbols(`
          export let foo := [];
          let data1 := 1;
          let data2 := data1 + 1;
          let data3 := data2 + 1;
          data3`),
    ).toEqual(new Set([]))
  })
})
