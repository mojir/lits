/* eslint-disable no-console */
import type { Mock } from 'vitest'
import { afterEach, beforeEach, describe, expect, it, vitest } from 'vitest'
import { Lits } from '../../src'
import type { UserDefinedError } from '../../src/errors'
import { getLitsVariants, getUndefinedSymbolNames } from '../testUtils'

const lits = getLitsVariants(false)

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

    const litsDebug = new Lits({ debug: true })
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
      // expect(logSpy).toHaveBeenNthCalledWith(1, 'A')
      expect(logSpy).toHaveBeenNthCalledWith(2, 'B')
      expect(logSpy).toHaveBeenNthCalledWith(3, 'A')
    })
    describe('unresolvedIdentifiers', () => {
      it('samples', () => {
        expect(getUndefinedSymbolNames(lits.analyze('let foo := a + b;'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('let foo := a + b; foo'))).toEqual(new Set(['a', 'b']))
      })
    })
  })

  describe('if', () => {
    it('samples', () => {
      expect(lits.run('if true then "A" else "B" end')).toBe('A')
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
        expect(getUndefinedSymbolNames(lits.analyze('if a > b then a else b end'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('if a > b then c else d end'))).toEqual(new Set(['a', 'b', 'c', 'd']))
      })
    })
  })

  describe('unless', () => {
    it('samples', () => {
      expect(lits.run('unless true then "A" else "B" end')).toBe('B')
      expect(lits.run('unless false then "A" else "B" end')).toBe('A')
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
      expect(lits.run('unless (object) then "A" else "B" end')).toBe('B')
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
        expect(getUndefinedSymbolNames(lits.analyze('unless a > b then a else b end'))).toEqual(new Set(['a', 'b']))
        expect(getUndefinedSymbolNames(lits.analyze('unless a > b then c else d end'))).toEqual(new Set(['a', 'b', 'c', 'd']))
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
        expect(getUndefinedSymbolNames(lits.analyze('&&(false, b)'))).toEqual(new Set(['b']))
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
        expect(getUndefinedSymbolNames(lits.analyze('||(true, b, c + d)'))).toEqual(new Set(['b', 'c', 'd']))
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
        expect(getUndefinedSymbolNames(lits.analyze('cond case true then a case false then b case a > 1 then c case true then d end'))).toEqual(
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
        expect(getUndefinedSymbolNames(lits.analyze('switch foo case true then a case false then b case a > 1 then c case true then d end'))).toEqual(
          new Set(['foo', 'a', 'b', 'c', 'd']),
        )
      })
    })
  })

  describe('function', () => {
    it('samples', () => {
      expect(lits.run(`
function add(a, b)
  a + b
end;
add(1, 2)`)).toBe(3)
      expect(lits.run('function add(a, b, let x := 10) a + b + x end; add(1, 2)')).toBe(13)
      expect(lits.run('function add() 10 end; add()')).toBe(10)
    })

    it('call defn function', () => {
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
        expect(getUndefinedSymbolNames(lits.analyze(`
function foo(a)
  if a = 1 then
    1
  else
    a + foo(a - 1)
  end
end;`))).toEqual(
          new Set(),
        )
        expect(
          getUndefinedSymbolNames(lits.analyze('function foo(a, let x := y, let y := z) if a = 1 then 1 else a + foo(a - 1) end end;')),
        ).toEqual(new Set(['y', 'z']))
        expect(getUndefinedSymbolNames(lits.analyze('function foo(a, b) str(a, b, c) end;'))).toEqual(new Set(['c']))
        expect(getUndefinedSymbolNames(lits.analyze('function foo(a, b) str(a, b, c) end; foo(x, y)'))).toEqual(
          new Set(['c', 'x', 'y']),
        )
        expect(getUndefinedSymbolNames(lits.analyze('function add(a, b, ...rest) a + b; [a](10) end;'))).toEqual(new Set())
      })
    })
  })

  //   it('shorthand lambda', () => {
  //     expect(lits.run('(#(+ %1 %2 %3) 2 4 6)')).toBe(12)
  //     expect(lits.run('(#if %1 %2 %3) 2 4 6)')).toBe(4)
  //     expect(lits.run('(#if %1 %2 %3) 0 4 6)')).toBe(6)
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(getUndefinedSymbolNames(lits.analyze('(fn [a b] (str a b c))'))).toEqual(new Set(['c']))
  //       expect(getUndefinedSymbolNames(lits.analyze('(def foo (fn [a b] (str a b c))) (foo 1 x)'))).toEqual(
  //         new Set(['c', 'x']),
  //       )
  //       expect(getUndefinedSymbolNames(lits.analyze('(fn ([a b &rest rest] (+ a b)) ([a] 10))'))).toEqual(new Set())
  //       expect(getUndefinedSymbolNames(lits.analyze('(#if %1 %2 %3) 0 4 6)'))).toEqual(new Set())
  //     })
  //   })
  // })

  // describe('try', () => {
  //   it('samples', () => {
  //     expect(lits.run('(try (/ 2 4) (catch error 1))')).toBe(0.5)
  //     expect(lits.run('(try (throw "oops") (catch error 1))')).toBe(1)
  //     expect(lits.run('(try (throw "oops") (catch error error))')).toBeInstanceOf(Error)
  //     expect(() => lits.run('(try (/ 2 4) 1)')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (1))')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (catch "error" 1))')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (ratch error 1))')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (catch error1 error2 1))')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (catch error 1, 2))')).toThrow()
  //     expect(() => lits.run('(try (/ 2 4) (catch error 1 )2)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(getUndefinedSymbolNames(lits.analyze('(try (/ a b) (catch error (str error x)))'))).toEqual(
  //         new Set(['a', 'b', 'x']),
  //       )
  //     })
  //   })
  // })

  // describe('throw', () => {
  //   it('samples', () => {
  //     expect(() => lits.run('(throw "An error")')).toThrowError(UserDefinedError)
  //     expect(() => lits.run('(throw (slice "An error" 3))')).toThrowError(UserDefinedError)
  //     expect(() => lits.run('(throw "An error" 10)')).not.toThrowError(UserDefinedError)
  //     expect(() => lits.run('(throw "An error" 10)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(getUndefinedSymbolNames(lits.analyze('(throw (/ a b))'))).toEqual(new Set(['a', 'b']))
  //     })
  //   })
  // })

  // describe('do', () => {
  //   it('samples', () => {
  //     expect(lits.run('(do [1, 2 3] "[1]" (+ 1, 2))')).toBe(3)
  //     expect(lits.run('(do (object "a" 1) "a")')).toBe('a')
  //     expect(lits.run('(do)')).toBeNull()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(getUndefinedSymbolNames(lits.analyze('(do [a 2 3] "[1]" (+ 1 b))'))).toEqual(new Set(['a', 'b']))
  //     })
  //   })
  // })

  // describe('recur', () => {
  //   it('should work with defn', () => {
  //     lits.run('(defn foo [n] write!(n) if (! (zero? n)) (recur (dec n)))) (foo 3)')
  //     expect(logSpy).toHaveBeenNthCalledWith(1, 3)
  //     expect(logSpy).toHaveBeenNthCalledWith(2, 2)
  //     expect(logSpy).toHaveBeenNthCalledWith(3, 1)
  //     expect(logSpy).toHaveBeenNthCalledWith(4, 0)
  //   })
  //   it('recur must be called with the right number of parameters', () => {
  //     expect(() => lits.run('(defn foo [n] write!(n) if (! (zero? n)) (recur))) (foo 3)')).toThrow()
  //     expect(() => lits.run('(defn foo [n] write!(n) if (! (zero? n)) (recur (dec n)))) (foo 3)')).not.toThrow()
  //     expect(() => lits.run('(defn foo [n] write!(n) if (! (zero? n)) (recur (dec n) 1))) (foo 3)')).toThrow()
  //     expect(() => lits.run('(defn foo [n] write!(n) if (! (zero? n)) (recur (dec n) 1, 2))) (foo 3)')).toThrow()
  //     expect(() => lits.run('((fn [n] write!(n) if (! (zero? n)) (recur))) 3)')).toThrow()
  //     expect(() => lits.run('((fn [n] write!(n) if (! (zero? n)) (recur (dec n)))) 3)')).not.toThrow()
  //     expect(() => lits.run('((fn [n] write!(n) if (! (zero? n)) (recur (dec n) 1))) 3)')).toThrow()
  //     expect(() => lits.run('((fn [n] write!(n) if (! (zero? n)) (recur (dec n) 1, 2))) 3)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       const lits2 = new Lits()

  //       expect(
  //         findUnresolvedSymbols(lits2.parse(lits2.tokenize('((fn [n] write!(n) if (! (zero? n)) (recur (dec n)))) 3)')).b, createContextStack(), builtin),
  //       ).toEqual(new Set())
  //       expect(
  //         findUnresolvedSymbols(lits2.parse(lits2.tokenize('((fn [n] write!(n) if (! (zero? n)) (recur (- n a)))) 3)')).b, createContextStack(), builtin),
  //       ).toEqual(new Set([{ symbol: 'a' }]))
  //     })
  //   })
  // })

  // describe('loop', () => {
  //   it('should work with recur', () => {
  //     lits.run('(loop [n 3] write!(n) if (! (zero? n)) (recur (dec n))))')
  //     expect(logSpy).toHaveBeenNthCalledWith(1, 3)
  //     expect(logSpy).toHaveBeenNthCalledWith(2, 2)
  //     expect(logSpy).toHaveBeenNthCalledWith(3, 1)
  //     expect(logSpy).toHaveBeenNthCalledWith(4, 0)
  //   })
  //   it('recur must be called with right number of parameters', () => {
  //     expect(() => lits.run('(loop [n 3] write!(n) if (! (zero? n)) (recur (dec n) 2)))')).toThrow()
  //     expect(() => lits.run('(loop [n 3] write!(n) if (! (zero? n)) (recur)))')).toThrow()
  //   })
  //   it('throw should work', () => {
  //     expect(() => lits.run('(loop [n 3] write!(n) if (! (zero? n)) (throw (dec n))))')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(loop [n 3] write!(n) if (! (zero? n)) (recur (dec n))))')),
  //       ).toEqual(new Set())
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(loop [n 3] write!(x n) if (! (zero? n)) (recur (dec n))))')),
  //       ).toEqual(new Set(['x']))
  //       expect(
  //         getUndefinedSymbolNames(
  //           lits.analyze('(loop [n (+ 3 y)] write!(x n) if (! (zero? n)) (recur (dec n))))'),
  //         ),
  //       ).toEqual(new Set(['x', 'y']))
  //     })
  //   })
  // })

  // describe('doseq', () => {
  //   it('samples', () => {
  //     expect(lits.run('(doseq [x []] x)')).toBeNull()
  //     expect(lits.run('(doseq [x [1, 2 3] y []] x)')).toBeNull()
  //     expect(lits.run('(doseq [x [] y [1, 2 3]] x)')).toBeNull()

  //     expect(lits.run('(doseq [x "Al" y [1, 2]] (repeat x y))')).toBeNull()
  //     expect(lits.run('(doseq [x {"a" 10 "b" 20} y [1, 2]] (repeat x y))')).toBeNull()

  //     expect(lits.run('(doseq [x [1, 2] y [1 10]] (* x y))')).toBeNull()
  //     expect(lits.run('(doseq [x [1, 2] &let [z (* x x x)]] z)')).toBeNull()
  //     expect(lits.run('(doseq [x [1, 2] y [x (* 2 x)]] (* x y))')).toBeNull()

  //     expect(lits.run('(doseq [x [0, 1, 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')).toBeNull()
  //     expect(lits.run('(doseq [x [0, 1, 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')).toBeNull()

  //     expect(lits.run('(doseq [x [1, 2 3] y [1, 2 3] &while (<= x y) z [1, 2 3]] [x y z])')).toBeNull()
  //     expect(lits.run('(doseq [x [1, 2 3] y [1, 2 3] z [1, 2 3] &while (<= x y)] [x y z])')).toBeNull()
  //     expect(() => lits.run('(doseq [x [0, 1, 2 3 4 5] &rest [y (* x 3)] &while (even? y)] y)')).toThrow()
  //     expect(() => lits.run('(doseq [x [0, 1, 2 3 4 5] &let [x 10]] y)')).toThrow()
  //     expect(() => lits.run('(doseq x [0, 1, 2 3 4 5] y)')).toThrow()
  //     expect(() => lits.run('(doseq [x [0, 1, 2 3 4 5]] x y)')).toThrow()
  //     expect(() => lits.run('(doseq [x [0, 1, 2 3 4 5] x [10 20]] x)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(doseq [x [0, 1, 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')),
  //       ).toEqual(new Set())
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(doseq [x [0, 1, 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')),
  //       ).toEqual(new Set())
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(doseq [x [0, 1, 2 3 4 a] &let [y (* x b)] &while (even? c)] d)')),
  //       ).toEqual(new Set(['a', 'b', 'c', 'd']))
  //     })
  //   })
  // })

  // describe('for', () => {
  //   it('samples', () => {
  //     expect(lits.run('(for [x []] x)')).toEqual([])
  //     expect(lits.run('(for [x [1, 2 3] y []] x)')).toEqual([])
  //     expect(lits.run('(for [x [] y [1, 2 3]] x)')).toEqual([])

  //     expect(lits.run('(for [x "Al" y [1, 2]] (repeat x y))')).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
  //     expect(lits.run('(for [x {"a" 10 "b" 20} y [1, 2]] (repeat x y))')).toEqual([
  //       [['a', 10]],
  //       [
  //         ['a', 10],
  //         ['a', 10],
  //       ],
  //       [['b', 20]],
  //       [
  //         ['b', 20],
  //         ['b', 20],
  //       ],
  //     ])

  //     expect(lits.run('(for [x [1, 2] y [1 10]] (* x y))')).toEqual([1, 10, 2, 20])
  //     expect(lits.run('(for [x [1, 2] &let [z (* x x x)]] z)')).toEqual([1, 8])
  //     expect(lits.run('(for [x [1, 2] y [x (* 2 x)]] (* x y))')).toEqual([1, 2, 4, 8])

  //     expect(lits.run('(for [x [0, 1, 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')).toEqual([0, 6, 12])
  //     expect(lits.run('(for [x [0, 1, 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')).toEqual([0])

  //     expect(lits.run('(for [x [1, 2 3] y [1, 2 3] &while (<= x y) z [1, 2 3]] [x y z])')).toEqual([
  //       [1, 1, 1],
  //       [1, 1, 2],
  //       [1, 1, 3],
  //       [1, 2, 1],
  //       [1, 2, 2],
  //       [1, 2, 3],
  //       [1, 3, 1],
  //       [1, 3, 2],
  //       [1, 3, 3],
  //     ])
  //     expect(lits.run('(for [x [1, 2 3] y [1, 2 3] z [1, 2 3] &while (<= x y)] [x y z])')).toEqual([
  //       [1, 1, 1],
  //       [1, 1, 2],
  //       [1, 1, 3],
  //       [1, 2, 1],
  //       [1, 2, 2],
  //       [1, 2, 3],
  //       [1, 3, 1],
  //       [1, 3, 2],
  //       [1, 3, 3],
  //       [2, 2, 1],
  //       [2, 2, 2],
  //       [2, 2, 3],
  //       [2, 3, 1],
  //       [2, 3, 2],
  //       [2, 3, 3],
  //       [3, 3, 1],
  //       [3, 3, 2],
  //       [3, 3, 3],
  //     ])
  //     expect(() => lits.run('(for [x [0, 1, 2 3 4 5] &rest [y (* x 3)] &while (even? y)] y)')).toThrow()
  //     expect(() => lits.run('(for [x [0, 1, 2 3 4 5] &let [x 10]] y)')).toThrow()
  //     expect(() => lits.run('(for x [0, 1, 2 3 4 5] y)')).toThrow()
  //     expect(() => lits.run('(for [x [0, 1, 2 3 4 5]] x y)')).toThrow()
  //     expect(() => lits.run('(for [x [0, 1, 2 3 4 5] x [10 20]] x)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(for [x [0, 1, 2 3 4 5] &let [y (* x 3)] &when (even? y)] y)')),
  //       ).toEqual(new Set())
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(for [x [0, 1, 2 3 4 5] &let [y (* x 3)] &while (even? y)] y)')),
  //       ).toEqual(new Set())
  //       expect(
  //         getUndefinedSymbolNames(lits.analyze('(for [x [0, 1, 2 3 4 a] &let [y (* x b)] &when (even? c)] d)')),
  //       ).toEqual(new Set(['a', 'b', 'c', 'd']))
  //     })
  //   })
  // })

  // describe('defined?', () => {
  //   it('samples', () => {
  //     expect(lits.run('(defined? foo)')).toBe(false)
  //     expect(lits.run('(def foo :foo) (defined? foo)')).toBe(true)
  //     expect(lits.run('(defined? +)')).toBe(true)
  //     expect(lits.run('(def foo null) (defined? foo)')).toBe(true)
  //     expect(lits.run('(defined? if)')).toBe(true)

  //     expect(() => lits.run('(defined?)')).toThrow()
  //     expect(() => lits.run('(defined? foo bar)')).toThrow()
  //   })
  // })

  // describe('unresolvedIdentifiers', () => {
  //   it('samples', () => {
  //     expect(getUndefinedSymbolNames(lits.analyze('(defined? x)'))).toEqual(new Set(['x']))
  //   })
  // })

  // describe('??', () => {
  //   it('samples', () => {
  //     expect(lits.run('(?? foo)')).toBe(null)
  //     expect(lits.run('(?? foo 0)')).toBe(0)
  //     expect(lits.run('(?? foo 0)')).toBe(0)
  //     expect(lits.run('(?? 0, 1)')).toBe(0)
  //     expect(lits.run('(?? "")')).toBe('')
  //     expect(lits.run('(?? null)')).toBe(null)
  //     expect(lits.run('(?? null, 0)')).toBe(0)
  //     expect(lits.run('(?? false)')).toBe(false)
  //     expect(lits.run('(def foo :foo) (?? foo)')).toBe('foo')

  //     expect(() => lits.run('(??)')).toThrow()
  //     expect(() => lits.run('(?? foo bar)')).toThrow()
  //   })

  //   describe('unresolvedIdentifiers', () => {
  //     it('samples', () => {
  //       expect(getUndefinedSymbolNames(lits.analyze('(?? x)'))).toEqual(new Set(['x']))
  //       expect(getUndefinedSymbolNames(lits.analyze('(?? x y)'))).toEqual(new Set(['x', 'y']))
  //     })
  //   })
  // })
})
