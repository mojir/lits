/* eslint-disable no-console */
import type { Mock } from 'vitest'
import { afterEach, beforeEach, describe, expect, it, test, vitest } from 'vitest'
import { Lits } from '../../src/Lits/Lits'
import { LitsError, UserDefinedError } from '../../src/errors'
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
        'error\nLocation 1:1\nthrow(slice("An error", 3))\n^                          ',
      )
    }
    if (failed)
      throw new Error('Should have thrown an error')
  })

  describe('array.', () => {
    test('spread', () => {
      expect(lits.run('[...[1, 2], 3, ...[4, 5]]')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('let x = [1, 2, 3]; [...x, ...x]')).toEqual([1, 2, 3, 1, 2, 3])
      expect(() => lits.run('[1, ...{}]')).toThrow(LitsError)
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
      expect(litsDebug.getUndefinedSymbols('let {a = b} = {};')).toEqual(new Set(['b']))
      expect(litsDebug.getUndefinedSymbols('export let {a = b} = {};')).toEqual(new Set(['b']))
      expect(litsDebug.getUndefinedSymbols('let foo = ({a = b} = {}) -> { a };')).toEqual(new Set(['b']))
      expect(litsDebug.getUndefinedSymbols('export let foo = ({a = b} = {}) -> { a };')).toEqual(new Set(['b']))
    })
  })

  describe('object.', () => {
    test('spread', () => {
      expect(lits.run('let x = { x: 10 };{ ...x, b: 20 }')).toEqual({ x: 10, b: 20 })
      expect(lits.run('{ ...{ a: 10 }, b: 20 }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ ...{ a: 10 }, a: 20 }')).toEqual({ a: 20 })
      expect(lits.run('{ a: 10, ...{ b: 20 } }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ a: 10, ...{ a: 20 } }')).toEqual({ a: 20 })
      expect(lits.run('{ a: 10, ...{} }')).toEqual({ a: 10 })
      expect(lits.run('{ \'a\': 10, ...{} }')).toEqual({ a: 10 })
      expect(() => lits.run('{ a: 10, ...[] }')).toThrow(LitsError)
    })
    it('samples', () => {
      expect(lits.run('let foo = "foo"; { [foo]: "bar" }')).toEqual({ foo: 'bar' })
      expect(lits.run('object()')).toEqual({})
      expect(lits.run('object("x", 1)')).toEqual({ x: 1 })
      expect(lits.run('object("x", null)')).toEqual({ x: null })
      expect(lits.run('{ a: 10, ...{b: 20}}')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{ a: 10, ...{a: 20}}')).toEqual({ a: 20 })
      expect(lits.run('object("x", 1, "x", 2)')).toEqual({ x: 2 })
      expect(lits.run('object("a", null, "b", true, "c", false, "d", 0, "e", object("x", []))')).toEqual({
        a: null,
        b: true,
        c: false,
        d: 0,
        e: { x: [] },
      })
      expect(lits.run('let a = "a"; object(a, 1)')).toEqual({ a: 1 })
      expect(() => lits.run('{ [10]: "bar" }')).toThrow(LitsError)
      expect(() => lits.run('{ ["x"]: "bar" }')).not.toThrow(LitsError)
      expect(() => lits.run('object("x")')).toThrow(LitsError)
      expect(() => lits.run('object("x")')).toThrow(LitsError)
      expect(() => lits.run('object("x", 1, "y")')).toThrow(LitsError)
      expect(() => lits.run('object(0, 1)')).toThrow(LitsError)
      expect(() => lits.run('object(true, 1)')).toThrow(LitsError)
      expect(() => lits.run('object(false, 1)')).toThrow(LitsError)
      expect(() => lits.run('object(null, 1)')).toThrow(LitsError)
      expect(() => lits.run('object([], 1)')).toThrow(LitsError)
      expect(() => lits.run('object(object(), 1)')).toThrow(LitsError)
    })
    test('findUnresolvedIdentifiers', () => {
      expect(litsDebug.getUndefinedSymbols('{ [foo]: bar }')).toEqual(new Set(['foo', 'bar']))
      expect(litsDebug.getUndefinedSymbols('object("x", 1, a, b)')).toEqual(new Set(['a', 'b']))
      expect(litsDebug.getUndefinedSymbols('object("x", 1, 2, 3)')).toEqual(new Set())
      expect(litsDebug.getUndefinedSymbols('{ x: 1, ...y }')).toEqual(new Set('y'))
      expect(litsDebug.getUndefinedSymbols('{ x: 1, ...{ ...{ a: y }, z: z } }')).toEqual(new Set(['y', 'z']))
    })
  })

  describe('let', () => {
    it('samples', () => {
      expect(lits.run('let a = 10; a')).toBe(10)
      expect(lits.run('let a = 10; { let a = 20; }; a')).toBe(10)
      expect(() => lits.run('let true = false;')).toThrow(LitsError)
      expect(() => lits.run('let 1 = 10;')).toThrow(LitsError)
      expect(() => lits.run('let x:x = 10;')).not.toThrow(LitsError)
      expect(() => lits.run('let x: = 10;')).toThrow(LitsError)
      expect(() => lits.run('let null = 10;')).toThrow(LitsError)
      expect(() => lits.run('let false = 10;')).toThrow(LitsError)
      expect(() => lits.run('let true = 10;')).toThrow(LitsError)
      expect(() => lits.run('let [] = 10;')).toThrow(LitsError)
      expect(() => lits.run('let {} = 10;')).toThrow(LitsError)
      expect(() => lits.run('let { a: 10 };')).toThrow(LitsError)
      expect(() => lits.run('let "a" = 10;')).toThrow(LitsError)
      expect(() => lits.run('{ export let a = 10; }')).toThrow(LitsError)
    })

    it('local variable', () => {
      const program = `
      let x = "A";
      write!(x);       // A
      {
        let x = "B";
        write!(x)      // B
      };
        
      write!(x)        // A - global variable x
      `
      lits.run(program)
      console.log('logSpy', logSpy.mock.calls)
      expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'A')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(() => litsDebug.getUndefinedSymbols('export let throw = a + b;')).toThrow(LitsError)
        expect(() => litsDebug.getUndefinedSymbols('export let + = a + b;')).toThrow(LitsError)
        expect(() => litsDebug.getUndefinedSymbols('export let foo = a + b; export let foo = a + b;')).toThrow(LitsError)
        expect(litsDebug.getUndefinedSymbols('let [a = b] = [];')).toEqual(new Set(['b']))
        expect(litsDebug.getUndefinedSymbols('export let foo = a + b;')).toEqual(new Set(['a', 'b']))
        expect(lits.getUndefinedSymbols('let foo = a + b; foo')).toEqual(new Set(['a', 'b']))
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
      expect(() => lits.run('if')).toThrow(LitsError)
      expect(() => lits.run('if true then end')).toThrow(LitsError)
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
      expect(() => lits.run('unless')).toThrow(LitsError)
      expect(() => lits.run('unless true then end')).toThrow(LitsError)
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
end`)).toBe(15)
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
let x = "-";
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
    test('accessing property on function', () => {
      expect(() => lits.run(`
        let foo = () -> {
          10
        };

        foo.bar
        `)).toThrow()
    })

    test('accessing number on function', () => {
      expect(() => lits.run(`
        let foo = () -> {
          10
        };

        foo[1]
        `)).toThrow()
    })

    test('lexical scoping', () => {
      expect(lits.run(`
      let bar = {
        let x = 10;
        let foo = (a) -> { a * x };
        foo;
      };
      
      bar(1)
      `)).toBe(10)
    })

    it('samples', () => {
      expect(lits.run(`
let add = (a, b) -> {
  a + b
};
add(1, 2)`)).toBe(3)
      expect(lits.run('let add = () -> { 10 }; add()')).toBe(10)
      expect(() => lits.run('let add = (...x = []) -> { x };')).toThrow(LitsError)
      expect(() => lits.run('let \' = 0_lambda\'() -> { 10 };')).toThrow(LitsError)
      expect(() => lits.run('\'0_lambda\'();')).toThrow(LitsError)
    })

    test('default argument', () => {
      expect(lits.run(`
let foo = (a, b = 10) -> {
  a + b
};

foo(1)`)).toBe(11)

      expect(lits.run(`
  let foo = (a, b = a + 1) -> {
    a + b
  };
  
  foo(1)`)).toBe(3)

      expect(lits.run(`
    let foo = (a, b = a + 1) -> {
      a + b
    };
    
    foo(1, 1)`)).toBe(2)

      expect(lits.run(`
      let foo = (a, b = a + 1, c = a + b) -> {
        a + b + c
      };
      
      foo(1)`)).toBe(6)
    })

    it('call function', () => {
      expect(lits.run(`
let sum-one-to-n = (n) -> {
  if n <= 1 then
    n
  else
    n + sum-one-to-n(n - 1)
  end
};

sum-one-to-n(10)`)).toBe(55)
      expect(lits.run(`
let applyWithVal = (fun, val) -> {
  fun(val)
};

applyWithVal(inc, 10)`)).toBe(11)
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols(`
let foo = (a) -> {
  if a == 1 then
    1
  else
    a + self(a - 1)
  end
};`))).toEqual(
          new Set(),
        )
        expect((lits.getUndefinedSymbols('export let foo = (a, b) -> { str(a, b, c) };'))).toEqual(new Set(['c']))
        expect((lits.getUndefinedSymbols('let foo = (a, b) -> { str(a, b, c) }; foo(x, y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect((lits.getUndefinedSymbols('let add = (a, b, ...the-rest) -> { a + b; [a](10) };'))).toEqual(new Set())
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
      expect((lits.getUndefinedSymbols('let foo = (a, b) -> str(a, b, c); foo(1, x)'))).toEqual(
        new Set(['c', 'x']),
      )
      expect((lits.getUndefinedSymbols('(a, b, ...the-rest) -> { a + b; [a](10) }'))).toEqual(new Set())
    })
  })

  describe('try', () => {
    it('samples', () => {
      expect(lits.run('try 2 / 4 catch (error) 1 end')).toBe(0.5)
      expect(lits.run('try 2 / 4 catch 1 end')).toBe(0.5)
      expect(litsDebug.run('try throw("oops") catch (error) 1 end')).toBe(1)
      expect(litsDebug.run('try throw("oops") catch 1 end')).toBe(1)
      expect(lits.run('try throw("oops") catch (error) error end')).toBeInstanceOf(Error)
      expect(() => lits.run('try 2 / 4 1 end)')).toThrow(LitsError)
      expect(() => lits.run('try 2 / 4 end (1))')).toThrow(LitsError)
      expect(() => lits.run('try 2 / 4 catch ("error") 1 end')).toThrow(LitsError)
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
      expect(() => lits.run('throw("An error" 10)')).toThrow(LitsError)
      expect(() => lits.run('throw()')).toThrow(LitsError)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('throw(a / b)'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('block', () => {
    it('samples', () => {
      expect(lits.run('{ [1, 2, 3]; "[1]"; 1 + 2 }')).toBe(3)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect((lits.getUndefinedSymbols('{ [a, 2, 3]; "[1]"; 1 + b }'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('recur', () => {
    it('should work with function', () => {
      lits.run(`
let foo = (n) -> {
  write!(n);
  if !(zero?(n)) then
    recur(n - 1)
  end
};
foo(3)`)
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with the right number of parameters', () => {
      expect(() => lits.run('let foo = (n) -> { if !(zero?(n)) then recur() end }; foo(3)')).toThrow(LitsError)
      expect(() => lits.run('let foo = (n) -> { if !(zero?(n)) then recur(n - 1) end }; foo(3)')).not.toThrow()
      // Too many parameters ok
      expect(() => lits.run('let foo = (n) -> { if !(zero?(n)) then recur(n - 1, 1) end }; foo(3)')).not.toThrow()
      expect(() => lits.run('((n) -> { if !(zero?(n)) then recur() end };)(3)')).toThrow(LitsError)
      expect(() => lits.run('((n) -> if !(zero?(n)) then recur(n - 1) end)(3)')).not.toThrow()
      expect(() => lits.run('((n) -> if !(zero?(n)) recur(n - 1 1) then(3) end')).toThrow(LitsError)
      expect(() => lits.run('((n) -> if !(zero?(n)) recur(n - 1 1, 2) then(3) end')).toThrow(LitsError)
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
          loop(n = 10, sum = 0) -> {
            if n == 0 then
              sum
            else
              recur(n - 1, sum + n)
            end
          }`)).toBe(55)
      })
    })

    it('should work with recur', () => {
      lits.run('loop (n = 3) -> { write!(n); if !(zero?(n)) then recur(n - 1) end}')
      expect(logSpy).toHaveBeenNthCalledWith(1, 3)
      expect(logSpy).toHaveBeenNthCalledWith(2, 2)
      expect(logSpy).toHaveBeenNthCalledWith(3, 1)
      expect(logSpy).toHaveBeenNthCalledWith(4, 0)
    })
    it('recur must be called with right number of parameters', () => {
      expect(() => litsDebug.run('loop (n = 3) -> if !(zero?(n)) then recur() end')).toThrow(LitsError)
      expect(() => lits.run('loop (n = 3) -> if !(zero?(n)) then recur(n - 1) end')).not.toThrow()
      expect(() => lits.run('loop (n = 3) -> if !(zero?(n)) then recur(n - 1, 2) end')).toThrow(LitsError)
      expect(() => lits.run('loop () -> if !(zero?(n)) then recur() end')).toThrow(LitsError)
      expect(() => lits.run('loop (n = 3) -> if !(zero?(n)) then recur(throw(1)) end')).toThrow(LitsError)
    })
    it('throw should work', () => {
      expect(() => lits.run('loop (n = 3) -> if !(zero?(n)) then throw(recur(n - 1, 2)) end')).toThrow(LitsError)
      expect(() => lits.run('loop (n) -> if !(zero?(n)) then recur(n - 1) end')).toThrow(LitsError)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (litsDebug.getUndefinedSymbols('loop (n = 3) -> { write!(n); if !(zero?(n)) then recur(n - 1) end}')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('loop (n = 3) -> { write!(x); if !(zero?(n)) then recur(n - 1) end }')),
        ).toEqual(new Set(['x']))
        expect(lits.getUndefinedSymbols('loop (n = 3 + y) -> { write!(n); if !(zero?(x)) then recur(n - 1) end }'))
          .toEqual(new Set(['x', 'y']))
      })
    })
  })

  describe('for', () => {
    it('samples', () => {
      expect(litsDebug.run('for (x in []) -> x')).toEqual([])
      expect(lits.run('for (x in [1, 2, 3], y in []) -> x')).toEqual([])
      expect(lits.run('for (x in [], y in [1, 2, 3]) -> x')).toEqual([])

      expect(lits.run('for (x in "Al", y in [1, 2]) -> repeat(x, y)'))
        .toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run('for (x in { a: 10, b: 20 }, y in [1, 2] let z = y) -> { repeat(x, z) }')).toEqual([
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
      expect(() => lits.run('for (x in { a: 10, b: 20 }, y in [1, 2] let z = y let z = y) -> repeat(x, z)')).toThrow(LitsError)
      expect(() => lits.run('for (x in { a: 10, b: 20 }, x in [1, 2]) -> x')).toThrow(LitsError)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (lits.getUndefinedSymbols('for (x in [0, 1, 2, 3, 4, 5] let y = x * 3 when even?(y)) -> y')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('for (x in [0, 1, 2, 3, 4, 5] let y = x * 3 while even?(y)) -> y')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('for (x in [0, 1, 2, 3, 4, a] let y = x * b when even?(c)) -> d')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
        expect(
          (lits.getUndefinedSymbols('for (x in [0, 1, 2, 3, 4, a] let y = x * b while even?(c)) -> d')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('doseq', () => {
    it('samples', () => {
      expect(lits.run('doseq (x in []) -> x')).toBeNull()
      expect(lits.run('doseq (x in [1, 2, 3], y in []) -> x')).toBeNull()
      expect(lits.run('doseq (x in [], y in [1, 2, 3]) -> x')).toBeNull()

      expect(lits.run('doseq (x in "Al", y in [1, 2]) -> { repeat(x, y) }'))
        .toBeNull()
      expect(lits.run('doseq (x in { a: 10, b: 20 }, y in [1, 2]) -> repeat(x, y)')).toBeNull()
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(
          (lits.getUndefinedSymbols('doseq (x in [0, 1, 2, 3, 4, 5] let y = x * 3 when even?(y)) -> y')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('doseq (x in [0, 1, 2, 3, 4, 5] let y = x * 3 while even?(y)) -> y')),
        ).toEqual(new Set())
        expect(
          (lits.getUndefinedSymbols('doseq (x in [0, 1, 2, 3, 4, a] let y = x * b when even?(c)) -> d')),
        ).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('defined?', () => {
    it('samples', () => {
      expect(lits.run('defined?(foo)')).toBe(false)
      expect(lits.run('let foo = "foo"; defined?(foo)')).toBe(true)
      expect(lits.run('defined?(+)')).toBe(true)
      expect(lits.run('let foo = null; defined?(foo)')).toBe(true)

      expect(() => lits.run('defined?()')).toThrow(LitsError)
      expect(() => lits.run('defined?(foo, bar)')).toThrow(LitsError)
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
      expect(lits.run('let foo = "foo"; ??(foo)')).toBe('foo')

      expect(() => lits.run('??()')).toThrow(LitsError)
      expect(() => lits.run('??(foo, bar)')).toThrow(LitsError)
    })

    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(lits.getUndefinedSymbols('??(x)')).toEqual(new Set(['x']))
        expect(lits.getUndefinedSymbols('??(x, y)')).toEqual(new Set(['x', 'y']))
      })
    })
  })
  describe('passing special expression as arguments', () => {
    test('samples', () => {
      expect(lits.run(`
let foo = (a, b, c) -> { a(b, c) };
foo(&&, true, false)`)).toBe(false)
    })
  })
})
