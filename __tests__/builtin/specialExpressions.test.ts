/* eslint-disable no-console */
import type { Mock } from 'vitest'
import { afterEach, beforeEach, describe, expect, it, test, vitest } from 'vitest'
import { Lits } from '../../src/Lits/Lits'
import { UserDefinedError } from '../../src/errors'
import type { Arr } from '../../src/interface'

const lits = new Lits()
const litsDebug = new Lits({ debug: true })

describe('specialExpressions', () => {
  let oldLog: () => void
  let logSpy: Mock<any>
  beforeEach(() => {
    oldLog = console.log
    logSpy = vitest.fn()
    console.log = (...args: unknown[]) => {
      logSpy(...args)
    }
  })
  afterEach(() => {
    console.log = oldLog
  })
  it('error message', () => {
    const litsNoDebug = new Lits()
    let failed = false
    try {
      litsNoDebug.run('throw(slice("An error", 3))')
      failed = true
    }
    catch (error) {
      expect((error as UserDefinedError).message).toBe('error')
    }
    if (failed)
      throw new Error('Should have thrown an error')

    try {
      failed = false
      litsDebug.run('throw(slice("An error", 3))')
      failed = true
    }
    catch (error) {
      expect((error as UserDefinedError).message).toBe(
        'error\nthrow(slice("An error", 3))\n^                          ',
      )
    }
    if (failed)
      throw new Error('Should have thrown an error')
  })

  describe('array.', () => {
    test('spread', () => {
      expect(lits.run('[...[1, 2], 3, ...[4, 5]]')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('let x := [1, 2, 3]; [...x, ...x]')).toEqual([1, 2, 3, 1, 2, 3])
      expect(() => lits.run('[1, ...{}]')).toThrow()
    })
    it('samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('array(1)')).toEqual([1])
      expect(lits.run('array(0, "1", null, true, false, array([]), object())')).toEqual([0, '1', null, true, false, [[]], {}])
    })
    it('shorthand samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[1]')).toEqual([1])
      expect((lits.run('[null]') as Arr)[0]).toEqual(null)
      expect(lits.run('[0, "1", null, true, false, [[]], object()]')).toEqual([0, '1', null, true, false, [[]], {}])
    })
    test('findUnresolvedIdentifiers', () => {
      expect(litsDebug.getUndefinedSymbols('array(1, a, b)')).toEqual(new Set(['a', 'b']))
      expect(litsDebug.getUndefinedSymbols('array(1, 2, 3)')).toEqual(new Set())
      expect(litsDebug.getUndefinedSymbols('[1, ...x]')).toEqual(new Set('x'))
      expect(litsDebug.getUndefinedSymbols('[1, ...[...[x], y]]')).toEqual(new Set(['x', 'y']))
    })
  })

  describe('object.', () => {
    test('spread', () => {
      expect(lits.run('let x := { x := 10 };{ ...x, b := 20 }')).toEqual({ x: 10, b: 20 })
      expect(lits.run('{ ...{ a := 10 }, b := 20 }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ ...{ a := 10 }, a := 20 }')).toEqual({ a: 20 })
      expect(lits.run('{ a := 10, ...{ b := 20 } }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ a := 10, ...{ a := 20 } }')).toEqual({ a: 20 })
      expect(lits.run('{ a := 10, ...{} }')).toEqual({ a: 10 })
      expect(() => lits.run('{ a := 10, ...[] }')).toThrow()
    })
    it('samples', () => {
      expect(lits.run('object()')).toEqual({})
      expect(lits.run('object("x", 1)')).toEqual({ x: 1 })
      expect(lits.run('object("x", null)')).toEqual({ x: null })
      expect(lits.run('{ a := 10, ...{b := 20}}')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ a := 10, ...{a := 20}}')).toEqual({ a: 20 })
      expect(lits.run('object("x", 1, "x", 2)')).toEqual({ x: 2 })
      expect(lits.run('object("a", null, "b", true, "c", false, "d", 0, "e", object("x", []))')).toEqual({
        a: null,
        b: true,
        c: false,
        d: 0,
        e: { x: [] },
      })
      expect(lits.run('let a := "a"; object(a, 1)')).toEqual({ a: 1 })
      expect(() => lits.run('object("x")')).toThrow()
      expect(() => lits.run('object("x")')).toThrow()
      expect(() => lits.run('object("x", 1, "y")')).toThrow()
      expect(() => lits.run('object(0, 1)')).toThrow()
      expect(() => lits.run('object(true, 1)')).toThrow()
      expect(() => lits.run('object(false, 1)')).toThrow()
      expect(() => lits.run('object(null, 1)')).toThrow()
      expect(() => lits.run('object([], 1)')).toThrow()
      expect(() => lits.run('object(object(), 1)')).toThrow()
    })
    test('findUnresolvedIdentifiers', () => {
      expect(litsDebug.getUndefinedSymbols('object("x", 1, a, b)')).toEqual(new Set(['a', 'b']))
      expect(litsDebug.getUndefinedSymbols('object("x", 1, 2, 3)')).toEqual(new Set())
      expect(litsDebug.getUndefinedSymbols('{ x := 1, ...y }')).toEqual(new Set('y'))
      expect(litsDebug.getUndefinedSymbols('{ x := 1, ...{ ...{ a := y }, z := z } }')).toEqual(new Set(['y', 'z']))
    })
  })

  describe('let', () => {
    it('samples', () => {
      expect(lits.run('let a := 10; a')).toBe(10)
      expect(lits.run('let a := 10; do let a := 20; end; a')).toBe(10)
      expect(() => lits.run('let a := 10; (def a 20) a')).toThrow()
      expect(() => lits.run('let true := false)')).toThrow()
      expect(() => lits.run('let 1 := 10)')).toThrow()
      expect(() => lits.run('let null := 10)')).toThrow()
      expect(() => lits.run('let false := 10)')).toThrow()
      expect(() => lits.run('let true := 10)')).toThrow()
      expect(() => lits.run('let [] := 10)')).toThrow()
      expect(() => lits.run('let {} := 10)')).toThrow()
      expect(() => lits.run('let "a" := 10)')).toThrow()
    })

    it('local variable', () => {
      const program = `
      let x := "A";
      write!(x);       // A
      do
        let x := "B";
        write!(x)      // B
      end;
        
      write!(x)        // A - global variable x
      `
      lits.run(program)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'A')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(() => litsDebug.getUndefinedSymbols('export let throw := a + b;')).toThrow()
        expect(() => litsDebug.getUndefinedSymbols('export let + := a + b;')).toThrow()
        expect(() => litsDebug.getUndefinedSymbols('export let foo := a + b; export let foo := a + b;')).toThrow()
        expect(litsDebug.getUndefinedSymbols('export let foo := a + b;')).toEqual(new Set(['a', 'b']))
        expect(lits.getUndefinedSymbols('let foo := a + b; foo')).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('if', () => {
    it('samples', () => {
      expect(litsDebug.run('if true then "A" else "B" end')).toBe('A')
      expect(lits.run('if false then "A" else "B" end')).toBe('B')
      expect(lits.run('if null then "A" else "B" end')).toBe('B')
      expect(lits.run('if true then "A" end')).toBe('A')
      expect(lits.run('if false then "A" end')).toBeNull()
      expect(lits.run('if null then "A" end')).toBeNull()
      expect(lits.run('if "" then "A" else "B" end')).toBe('B')
      expect(lits.run('if "x" then "A" else "B" end')).toBe('A')
      expect(lits.run('if 0 then "A" else "B" end')).toBe('B')
      expect(lits.run('if 1 then "A" else "B" end')).toBe('A')
      expect(lits.run('if -1 then "A" else "B" end')).toBe('A')
      expect(lits.run('if [] then "A" else "B" end')).toBe('A')
      expect(lits.run('if {} then "A" else "B" end')).toBe('A')
      expect(() => lits.run('if')).toThrow()
      expect(() => lits.run('if true)')).toThrow()
    })
    it('that special form \'if\' only evaluate the correct path (true)', () => {
      lits.run('if true then write!("A") else write!("B") end')
      expect(logSpy).toHaveBeenCalledWith('A')
      expect(logSpy).not.toHaveBeenCalledWith('B')
    })
    it('that special form \'if\' only evaluate the correct path (false)', () => {
      lits.run('if false then write!("A") else write!("B") end')
      expect(logSpy).not.toHaveBeenCalledWith('A')
      expect(logSpy).toHaveBeenCalledWith('B')
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((litsDebug.getUndefinedSymbols('if a > b then a else b end'))).toEqual(new Set(['a', 'b']))
        expect((lits.getUndefinedSymbols('if a > b then c else d end'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('unless', () => {
    it('samples', () => {
      expect(litsDebug.run('unless true then "A" else "B" end')).toBe('B')
      expect(litsDebug.run('unless false then "A" else "B" end')).toBe('A')
      expect(lits.run('unless null then "A" else "B" end')).toBe('A')
      expect(lits.run('unless true then "A" end')).toBeNull()
      expect(lits.run('unless false then "A" end')).toBe('A')
      expect(lits.run('unless null then "A" end')).toBe('A')
      expect(lits.run('unless "" then "A" else "B" end')).toBe('A')
      expect(lits.run('unless "x" then "A" else "B" end')).toBe('B')
      expect(lits.run('unless 0 then "A" else "B" end')).toBe('A')
      expect(lits.run('unless 1 then "A" else "B" end')).toBe('B')
      expect(lits.run('unless -1 then "A" else "B" end')).toBe('B')
      expect(lits.run('unless [] then "A" else "B" end')).toBe('B')
      expect(lits.run('unless object() then "A" else "B" end')).toBe('B')
      expect(() => lits.run('unless')).toThrow()
      expect(() => lits.run('unless true)')).toThrow()
    })
    it('that special form \'unless\' only evaluate the correct path (true)', () => {
      lits.run('unless true then write!("A") else write!("B") end')
      expect(logSpy).toHaveBeenCalledWith('B')
      expect(logSpy).not.toHaveBeenCalledWith('A')
    })
    it('that special form \'unless\' only evaluate the correct path (false)', () => {
      lits.run('unless false then write!("A") else write!("B") end')
      expect(logSpy).not.toHaveBeenCalledWith('B')
      expect(logSpy).toHaveBeenCalledWith('A')
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('unless a > b then a else b end'))).toEqual(new Set(['a', 'b']))
        expect((lits.getUndefinedSymbols('unless a > b then c else d end'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('&&', () => {
    it('samples', () => {
      expect(lits.run('0 && 1')).toBe(0)
      expect(lits.run('2 && 1')).toBe(1)
      expect(lits.run('&&()')).toBe(true)
      expect(lits.run('&&(0)')).toBe(0)
      expect(lits.run('&&(0, 1)')).toBe(0)
      expect(lits.run('&&(2, 0)')).toBe(0)
      expect(lits.run('&&(2, 0, 1)')).toBe(0)
      expect(lits.run('&&(2, 3, 0)')).toBe(0)
      expect(lits.run('&&(2, 3, "")')).toBe('')
      expect(lits.run('&&(2, 3, "x")')).toBe('x')
      expect(lits.run('&&(false, 1)')).toBe(false)
      expect(lits.run('&&(1, false)')).toBe(false)
      expect(lits.run('&&(1, null)')).toBe(null)
      expect(lits.run('&&(2, 2, false)')).toBe(false)
      expect(lits.run('&&(3, true, 3)')).toBe(3)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('&&(write!(true), write!(false))')).toBe(false)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, false)
      })
      it('true, 1', () => {
        expect(lits.run('&&(write!(true), write!(1))')).toBe(1)
        expect(logSpy).toHaveBeenNthCalledWith(1, true)
        expect(logSpy).toHaveBeenNthCalledWith(2, 1)
      })
      it('false, true', () => {
        expect(lits.run('&&(write!(false), write!(true))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(true)
      })
      it('false, 0', () => {
        expect(lits.run('&&(write!(false), write!(0))')).toBe(false)
        expect(logSpy).toHaveBeenCalledWith(false)
        expect(logSpy).not.toHaveBeenCalledWith(0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('&&(false, b)'))).toEqual(new Set(['b']))
      })
    })
  })

  describe('||', () => {
    it('samples', () => {
      expect(lits.run('0 || 1')).toBe(1)
      expect(lits.run('2 || 0')).toBe(2)
      expect(lits.run('||()')).toBe(false)
      expect(lits.run('||(0)')).toBe(0)
      expect(lits.run('||(0, 1)')).toBe(1)
      expect(lits.run('||(2, 0)')).toBe(2)
      expect(lits.run('||(null, 0, false)')).toBe(false)
      expect(lits.run('||(null, 0, 1)')).toBe(1)
    })
    describe('short circuit', () => {
      it('true, false', () => {
        expect(lits.run('||(write!(true), write!(false))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(false)
      })
      it('true, 1', () => {
        expect(lits.run('||(write!(true), write!(1))')).toBe(true)
        expect(logSpy).toHaveBeenCalledWith(true)
        expect(logSpy).not.toHaveBeenCalledWith(1)
      })
      it('false, true', () => {
        expect(lits.run('||(write!(false), write!(true))')).toBe(true)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, true)
      })
      it('false, 0', () => {
        expect(lits.run('||(write!(false), write!(0))')).toBe(0)
        expect(logSpy).toHaveBeenNthCalledWith(1, false)
        expect(logSpy).toHaveBeenNthCalledWith(2, 0)
      })
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('||(true, b, c + d)'))).toEqual(new Set(['b', 'c', 'd']))
      })
    })
  })

  describe('cond', () => {
    it('samples', () => {
      expect(lits.run(`
cond
  case true then 10
  case true then 20
end`)).toBe(10)
      expect(lits.run(`
cond
  case false then 10
  case false then 20
end`)).toBeNull()
      expect(lits.run('cond case true then 10 end')).toBe(10)
      expect(lits.run('cond case false then 20 case true then 5 + 5 end')).toBe(10)
      expect(lits.run(`
cond
  case 5 > 10 then 20
  case 10 > 10 then 5 + 5
  case 10 >= 10 then 5 + 5 + 5
end`),
      ).toBe(15)
    })
    it('middle condition true', () => {
      expect(
        lits.run(`
cond
  case 5 > 10 then 20
  case 10 >= 10 then 5 + 5
  case 10 > 10 then 5 + 5 + 5
end`),
      ).toBe(10)
      expect(logSpy).not.toHaveBeenCalled()
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('cond case true then a case false then b case a > 1 then c case true then d end'))).toEqual(
          new Set(['a', 'b', 'c', 'd']),
        )
      })
    })
  })

  describe('switch', () => {
    it('samples', () => {
      expect(lits.run(`
let x := "-";
switch x
  case "-" then 5 + 5
  case 2 then 20
end`)).toBe(10)
      expect(lits.run('switch true case true then 10 end')).toBe(10)
      expect(lits.run('switch true case false then 10 end')).toBeNull()
      expect(lits.run('switch true case false then 20 case true then 10 end')).toBe(10)
      expect(
        lits.run(`
switch 2
  case 0 then 20
  case 1 then 10
  case 2 then 15
end`),
      ).toBe(15)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('switch foo case true then a case false then b case a > 1 then c case true then d end'))).toEqual(
          new Set(['foo', 'a', 'b', 'c', 'd']),
        )
      })
    })
  })

  describe('function', () => {
    test('lexical scoping', () => {
      expect(lits.run(`
      let bar := do 
        let x := 10;
        function foo(a) a * x end;
        foo;
      end;
      
      bar(1)
      `)).toBe(10)
    })

    it('samples', () => {
      expect(lits.run(`
function add(a, b)
  a + b
end;
add(1, 2)`)).toBe(3)
      expect(lits.run('function add() 10 end; add()')).toBe(10)
    })

    test('default argument', () => {
      expect(lits.run(`
function foo(a, b := 10)
  a + b
end;

foo(1)`)).toBe(11)

      expect(lits.run(`
  function foo(a, b := a + 1)
    a + b
  end;
  
  foo(1)`)).toBe(3)

      expect(lits.run(`
    function foo(a, b := a + 1)
      a + b
    end;
    
    foo(1, 1)`)).toBe(2)

      expect(lits.run(`
      function foo(a, b := a + 1, c := a + b)
        a + b + c
      end;
      
      foo(1)`)).toBe(6)
    })

    it('call function', () => {
      expect(lits.run(`
function sum-one-to-n(n)
  if n <= 1 then
    n
  else
    n + sum-one-to-n(n - 1)
  end
end;

sum-one-to-n(10)`)).toBe(55)
      expect(lits.run(`
function applyWithVal(fun, val)
  fun(val)
end;

applyWithVal(inc, 10)`)).toBe(11)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols(`
function foo(a)
  if a = 1 then
    1
  else
    a + foo(a - 1)
  end
end;`))).toEqual(
          new Set(),
        )
        expect((lits.getUndefinedSymbols('export function foo(a, b) str(a, b, c) end;'))).toEqual(new Set(['c']))
        expect((lits.getUndefinedSymbols('function foo(a, b) str(a, b, c) end; foo(x, y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect((lits.getUndefinedSymbols('function add(a, b, ...the-rest) a + b; [a](10) end;'))).toEqual(new Set())
      })
    })
  })

  it('shorthand lambda', () => {
    expect(lits.run('(-> $1 + $2 + $3)(2, 4, 6)')).toBe(12)
    expect(lits.run('(-> if $1 then $2 else $3 end)(2, 4, 6)')).toBe(4)
    expect(lits.run('((a, b, c) -> if a then b else c end)(0, 4, 6)')).toBe(6)
  })

  describe('unresolvedIdentifiers', () => {
    it('samples', () => {
      expect((lits.getUndefinedSymbols('(a, b) -> str(a, b, c)'))).toEqual(new Set(['c']))
      expect((lits.getUndefinedSymbols('let foo := (a, b) -> str(a, b, c); foo(1, x)'))).toEqual(
        new Set(['c', 'x']),
      )
      expect((lits.getUndefinedSymbols('(a, b, ...the-rest) -> do a + b; [a](10) end'))).toEqual(new Set())
    })
  })

  describe('try', () => {
    it('samples', () => {
      expect(lits.run('try 2 / 4 catch (error) 1 end')).toBe(0.5)
      expect(lits.run('try 2 / 4 catch 1 end')).toBe(0.5)
      expect(litsDebug.run('try throw("oops") catch (error) 1 end')).toBe(1)
      expect(litsDebug.run('try throw("oops") catch 1 end')).toBe(1)
      expect(lits.run('try throw ("oops") catch (error) error end')).toBeInstanceOf(Error)
      expect(() => lits.run('try 2 / 4 1)')).toThrow()
      expect(() => lits.run('try 2 / 4 (1))')).toThrow()
      expect(() => lits.run('try 2 / 4 catch ("error") 1 end')).toThrow()
      expect(() => lits.run('try 2 / 4 ratch (error) 1 end')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('try a / b catch (error) str(error, x) end'))).toEqual(
          new Set(['a', 'b', 'x']),
        )
        expect((lits.getUndefinedSymbols('try a / b catch str(error, x) end'))).toEqual(
          new Set(['a', 'b', 'x', 'error']),
        )
      })
    })
  })

  describe('throw', () => {
    it('samples', () => {
      expect(() => lits.run('throw("An error")')).toThrowError(UserDefinedError)
      expect(() => lits.run('throw(slice("An error", 3))')).toThrowError(UserDefinedError)
      expect(() => lits.run('throw("An error" 10)')).not.toThrowError(UserDefinedError)
      expect(() => lits.run('throw("An error" 10)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('throw(a / b)'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('do', () => {
    it('samples', () => {
      expect(lits.run('do [1, 2, 3]; "[1]"; 1 + 2 end')).toBe(3)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('do [a, 2, 3]; "[1]"; 1 + b end'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('recur', () => {
    it('should work with function', () => {
      lits.run(`
function foo(n)
  write!(n);
  if !(zero?(n)) then
    recur(n - 1)
  end
end;
foo(3)`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with the right number of parameters', () => {
      expect(() => lits.run('function foo(n) if !(zero?(n)) then recur() end end; foo(3)')).toThrow()
      expect(() => lits.run('function foo(n) if !(zero?(n)) then recur(n - 1) end end; foo(3)')).not.toThrow()
      expect(() => lits.run('function foo(n) if !(zero?(n)) then recur(n - 1, 1) end end; foo(3)')).toThrow()
      expect(() => lits.run('((n) -> if !(zero?(n)) then recur() end)(3)')).toThrow()
      expect(() => lits.run('((n) -> if !(zero?(n)) then recur(n - 1) end)(3)')).not.toThrow()
      expect(() => lits.run('((n) -> if !(zero?(n)) then recur(n - 1 1) end)(3)')).toThrow()
      expect(() => lits.run('((n) -> if !(zero?(n)) then recur(n - 1 1, 2) end)(3)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('(-> if !(zero?($)) then recur($ - 1) end)(3)')))
          .toEqual(new Set())
        expect((lits.getUndefinedSymbols('(-> if !(zero?($)) then recur($ - a) end)(3)')))
          .toEqual(new Set('a'))
      })
    })
  })

  describe('loop', () => {
    describe('loop expressions', () => {
      it('supports loop expressions', () => {
        expect(lits.run(`
          loop
            let n := 10,
            let sum := 0
          do
            if n = 0 then
              sum
            else
              recur(n - 1, sum + n)
            end
          end`)).toBe(55)
      })
    })

    it('should work with recur', () => {
      lits.run('loop let n := 3 do write!(n); if !(zero?(n)) then recur(n - 1) end end')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with right number of parameters', () => {
      expect(() => litsDebug.run('loop let n := 3 do if !(zero?(n)) then recur() end end')).toThrow()
      expect(() => lits.run('loop let n := 3 do if !(zero?(n)) then recur(n - 1) end end')).not.toThrow()
      expect(() => lits.run('loop let n := 3 do if !(zero?(n)) then recur(n - 1, 2) end end')).toThrow()
      expect(() => lits.run('loop let n := 3 do if !(zero?(n)) then recur(throw(1)) end end')).toThrow()
    })
    it('throw should work', () => {
      expect(() => lits.run('loop let n := 3 do if !(zero?(n)) then throw(recur(n - 1, 2)) end end')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (litsDebug.getUndefinedSymbols('loop let n := 3 do write!(n); if !(zero?(n)) then recur(n - 1) end end')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('loop let n := 3 do write!(x); if !(zero?(n)) then recur(n - 1) end end')),
        ).toEqual(new Set(['x']))
        expect(lits.getUndefinedSymbols('loop let n := 3 + y do write!(n); if !(zero?(x)) then recur(n - 1) end end'))
          .toEqual(new Set(['x', 'y']))
      })
    })
  })

  describe('for', () => {
    it('samples', () => {
      expect(litsDebug.run('for each x in [] do x end')).toEqual([])
      expect(lits.run('for each x in [1, 2, 3] each y in [] do x end')).toEqual([])
      expect(lits.run('for each x in [] each y in [1, 2, 3] do x end')).toEqual([])

      expect(lits.run('for each x in "Al" each y in [1, 2] do repeat(x, y) end'))
        .toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run('for each x in { a := 10, b := 20 } each y in [1, 2], let z := y do repeat(x, z) end')).toEqual([
        [['a', 10]],
        [
          ['a', 10],
          ['a', 10],
        ],
        [['b', 20]],
        [
          ['b', 20],
          ['b', 20],
        ],
      ])
      expect(() => lits.run('for each x in { a := 10, b := 20 } each y in [1, 2], let z := y, let z := y do repeat(x, z) end')).toThrow()
      expect(() => lits.run('for each x in { a := 10, b := 20 } each x in [1, 2] do x end')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (lits.getUndefinedSymbols('for each x in [0, 1, 2, 3, 4, 5], let y := x * 3, when even?(y) do y end')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('for each x in [0, 1, 2, 3, 4, 5], let y := x * 3, while even?(y) do y end')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('for each x in [0, 1, 2, 3, 4, a], let y := x * b, when even?(c) do d end')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
        expect(
          (lits.getUndefinedSymbols('for each x in [0, 1, 2, 3, 4, a], let y := x * b, while even?(c) do d end')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('doseq', () => {
    it('samples', () => {
      expect(lits.run('doseq each x in [] do x end')).toBeNull()
      expect(lits.run('doseq each x in [1, 2, 3] each y in [] do x end')).toBeNull()
      expect(lits.run('doseq each x in [] each y in [1, 2, 3] do x end')).toBeNull()

      expect(lits.run('doseq each x in "Al" each y in [1, 2] do repeat(x, y) end'))
        .toBeNull()
      expect(lits.run('doseq each x in { a := 10, b := 20 } each y in [1, 2] do repeat(x, y) end')).toBeNull()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (lits.getUndefinedSymbols('doseq each x in [0, 1, 2, 3, 4, 5], let y := x * 3, when even?(y) do y end')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('doseq each x in [0, 1, 2, 3, 4, 5], let y := x * 3, while even?(y) do y end')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('doseq each x in [0, 1, 2, 3, 4, a], let y := x * b, when even?(c) do d end')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('defined?', () => {
    it('samples', () => {
      expect(lits.run('defined?(foo)')).toBe(false)
      expect(lits.run('let foo := "foo"; defined?(foo)')).toBe(true)
      expect(lits.run('defined?(+)')).toBe(true)
      expect(lits.run('let foo := null; defined?(foo)')).toBe(true)

      expect(() => lits.run('defined?()')).toThrow()
      expect(() => lits.run('defined?(foo, bar)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('defined?(x)'))).toEqual(new Set(['x']))
      })
    })
  })

  describe('??', () => {
    it('samples', () => {
      expect(lits.run('??(foo)')).toBe(null)
      expect(lits.run('??(foo, 0)')).toBe(0)
      expect(litsDebug.run('??(foo, 0)')).toBe(0)
      expect(litsDebug.run('??(0, 1)')).toBe(0)
      expect(lits.run('??("")')).toBe('')
      expect(lits.run('??(null)')).toBe(null)
      expect(lits.run('??(null, 0)')).toBe(0)
      expect(lits.run('??(false)')).toBe(false)
      expect(lits.run('let foo := "foo"; ??(foo)')).toBe('foo')

      expect(() => lits.run('??()')).toThrow()
      expect(() => lits.run('??(foo, bar)')).toThrow()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('??(x)'))).toEqual(new Set(['x']))
        expect((lits.getUndefinedSymbols('??(x, y)'))).toEqual(new Set(['x', 'y']))
      })
    })
  })
})
