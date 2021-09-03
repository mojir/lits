/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { lispish } from '../../src'

describe('evaluator', () => {
  let oldLog: () => void
  let logSpy: jest.Mock<any, any>
  beforeEach(() => {
    oldLog = console.log
    logSpy = jest.fn()
    console.log = logSpy
  })
  afterEach(() => {
    console.log = oldLog
  })
  describe('+', () => {
    test('samples', () => {
      expect(lispish('(+)')).toBe(0)
      expect(lispish('(+ 2)')).toBe(2)
      expect(lispish('(+ 2 2)')).toBe(4)
      expect(lispish('(+ -2 2)')).toBe(0)
      expect(lispish('(+ 1 2 3 4)')).toBe(10)
      expect(() => lispish('(+ "1" 2 3 4)')).toThrow()
    })
  })

  describe('*', () => {
    test('samples', () => {
      expect(lispish('(*)')).toBe(1)
      expect(lispish('(* 2)')).toBe(2)
      expect(lispish('(* 2 2)')).toBe(4)
      expect(lispish('(* -2 2)')).toBe(-4)
      expect(lispish('(* 1 2 3 4)')).toBe(24)
      expect(() => lispish('(* "1" 2 3 4)')).toThrow()
    })
  })

  describe('/', () => {
    test('samples', () => {
      expect(lispish('(/)')).toBe(1)
      expect(lispish('(/ 2)')).toBe(1 / 2)
      expect(lispish('(/ 2 2)')).toBe(2 / 2)
      expect(lispish('(/ -2 2)')).toBe(-2 / 2)
      expect(lispish('(/ 1 2 3 4)')).toBe(1 / 2 / 3 / 4)
      expect(() => lispish('(/ "1" 2 3 4)')).toThrow()
    })
  })

  describe('-', () => {
    test('samples', () => {
      expect(lispish('(-)')).toBe(0)
      expect(lispish('(- 2)')).toBe(-2)
      expect(lispish('(- 2 2)')).toBe(2 - 2)
      expect(lispish('(- -2 2)')).toBe(-2 - 2)
      expect(lispish('(- 1 2 3 4)')).toBe(1 - 2 - 3 - 4)
      expect(() => lispish('(- "1" 2 3 4)')).toThrow()
    })
  })

  describe('%', () => {
    test('samples', () => {
      expect(() => lispish('(%)')).toThrow()
      expect(() => lispish('(% 3)')).toThrow()
      expect(() => lispish('(% 3 4 5)')).toThrow()
      expect(lispish('(% 2 1)')).toBe(0)
      expect(lispish('(% 2 2)')).toBe(0)
      expect(lispish('(% 3 2)')).toBe(1)
      expect(lispish('(% 3 -2)')).toBe(1)
      expect(lispish('(% -3 -2)')).toBe(-1)
      expect(lispish('(% -3 2)')).toBe(-1)
      expect(() => lispish('(% 4 0)')).toThrow()
    })
  })

  describe('sqrt', () => {
    test('samples', () => {
      expect(() => lispish('(sqrt)')).toThrow()
      expect(() => lispish('(sqrt 3 4)')).toThrow()
      expect(() => lispish('(sqrt -3)')).toThrow()
      expect(lispish('(sqrt 0)')).toBe(0)
      expect(lispish('(sqrt 1)')).toBe(1)
      expect(lispish('(sqrt 4)')).toBe(2)
    })
  })

  describe('expt', () => {
    test('samples', () => {
      expect(() => lispish('(expt)')).toThrow()
      expect(() => lispish('(expt 3)')).toThrow()
      expect(() => lispish('(expt 3 4 5)')).toThrow()
      expect(lispish('(expt 2 0)')).toBe(1)
      expect(lispish('(expt 2 1)')).toBe(2)
      expect(lispish('(expt 2 2)')).toBe(4)
      expect(lispish('(expt 2 3)')).toBe(8)
      expect(lispish('(expt 16 0.5)')).toBe(4)
      expect(lispish('(expt 10 -1)')).toBe(0.1)
      expect(lispish('(expt 10 -2)')).toBe(0.01)
      expect(lispish('(expt -2 -1)')).toBe(-0.5)
      expect(lispish('(expt -2 -2)')).toBe(0.25)
    })
  })

  describe('round', () => {
    test('samples', () => {
      expect(() => lispish('(round)')).toThrow()
      expect(() => lispish('(round 3 4)')).toThrow()
      expect(lispish('(round 0)')).toBe(0)
      expect(lispish('(round 1)')).toBe(1)
      expect(lispish('(round 0.4)')).toBe(0)
      expect(lispish('(round 0.5)')).toBe(1)
      expect(lispish('(round 0.6)')).toBe(1)
      expect(lispish('(round -0.4)')).toBe(-0)
      expect(lispish('(round -0.5)')).toBe(-0)
      expect(lispish('(round -0.6)')).toBe(-1)
    })
  })

  describe('floor', () => {
    test('samples', () => {
      expect(() => lispish('(floor)')).toThrow()
      expect(() => lispish('(floor 3 4)')).toThrow()
      expect(lispish('(floor 0)')).toBe(0)
      expect(lispish('(floor 1)')).toBe(1)
      expect(lispish('(floor 0.4)')).toBe(0)
      expect(lispish('(floor 0.5)')).toBe(0)
      expect(lispish('(floor 0.6)')).toBe(0)
      expect(lispish('(floor -0.4)')).toBe(-1)
      expect(lispish('(floor -0.5)')).toBe(-1)
      expect(lispish('(floor -0.6)')).toBe(-1)
    })
  })

  describe('ceil', () => {
    test('samples', () => {
      expect(() => lispish('(ceil)')).toThrow()
      expect(() => lispish('(ceil 3 4)')).toThrow()
      expect(lispish('(ceil 0)')).toBe(0)
      expect(lispish('(ceil 1)')).toBe(1)
      expect(lispish('(ceil 0.4)')).toBe(1)
      expect(lispish('(ceil 0.5)')).toBe(1)
      expect(lispish('(ceil 0.6)')).toBe(1)
      expect(lispish('(ceil -0.4)')).toBe(-0)
      expect(lispish('(ceil -0.5)')).toBe(-0)
      expect(lispish('(ceil -0.6)')).toBe(-0)
    })
  })

  describe('random', () => {
    test('samples', () => {
      expect(() => lispish('(random)')).toThrow()
      expect(() => lispish('(random "x")')).toThrow()
      expect(() => lispish('(random 1 2)')).toThrow()
      expect(lispish('(random 0.1)')).toBeLessThan(0.1)
      expect(lispish('(random 0.1)')).toBeGreaterThanOrEqual(0)
    })
  })

  describe('now', () => {
    test('samples', () => {
      expect(() => lispish('(now 1)')).toThrow()
      expect(() => lispish('(now "x")')).toThrow()
      expect(() => lispish('(now undefined)')).toThrow()
      expect(lispish('(now)')).toBeLessThanOrEqual(Date.now())
    })
  })

  describe('aset', () => {
    test('samples', () => {
      expect(() => lispish('(aset 1 1 1)')).toThrow()
      expect(() => lispish('(aset [1 1 1)')).toThrow()
      expect(() => lispish('(aset "x")')).toThrow()
      expect(() => lispish('(aset undefined)')).toThrow()
      expect(lispish('(now)')).toBeLessThanOrEqual(Date.now())
    })
  })

  describe('!=', () => {
    test('samples', () => {
      expect(() => lispish('(!=)')).toThrow()
      expect(lispish('(!= 1)')).toBe(true)
      expect(lispish('(!= 1 1)')).toBe(false)
      expect(lispish('(!= 1 2)')).toBe(true)
      expect(lispish('(!= 1 2 1)')).toBe(false)
      expect(lispish('(!= 1 2 3)')).toBe(true)
      expect(lispish('(!= "1")')).toBe(true)
      expect(lispish('(!= "1" "1")')).toBe(false)
      expect(lispish('(!= "1" "2")')).toBe(true)
      expect(lispish('(!= "1" "2" "1")')).toBe(false)
      expect(lispish('(!= "1" "2" 3)')).toBe(true)
      expect(lispish('(!= null undefined)')).toBe(true)
      expect(lispish('(!= null 0)')).toBe(true)
      expect(lispish('(!= 1 undefined 1)')).toBe(false)
      expect(lispish('(!= 1 true 3)')).toBe(true)
      expect(lispish('(!= 1 false 3)')).toBe(true)
    })
  })

  describe('=', () => {
    test('samples', () => {
      expect(() => lispish('(=)')).toThrow()
      expect(lispish('(= 1)')).toBe(true)
      expect(lispish('(= 1 1)')).toBe(true)
      expect(lispish('(= 1 2)')).toBe(false)
      expect(lispish('(= 1 2 1)')).toBe(false)
      expect(lispish('(= 1 2 3)')).toBe(false)
      expect(lispish('(= "1")')).toBe(true)
      expect(lispish('(= "1" "1")')).toBe(true)
      expect(lispish('(= "1" "2")')).toBe(false)
      expect(lispish('(= "1" "2" "1")')).toBe(false)
      expect(lispish('(= "1" "2" "3")')).toBe(false)
      expect(lispish('(= "2" "2" "2")')).toBe(true)
      expect(lispish('(= 1 "2" 3)')).toBe(false)
      expect(lispish('(= 1 null 3)')).toBe(false)
      expect(lispish('(= 1 undefined 3)')).toBe(false)
      expect(lispish('(= 1 true 3)')).toBe(false)
      expect(lispish('(= 1 false 3)')).toBe(false)
      expect(lispish('(= null null)')).toBe(true)
      expect(lispish('(= undefined undefined)')).toBe(true)
      expect(lispish('(= true true)')).toBe(true)
      expect(lispish('(= false false)')).toBe(true)
      expect(lispish('(= null undefined)')).toBe(false)
    })
  })

  describe('>', () => {
    test('samples', () => {
      expect(() => lispish('(>)')).toThrow()
      expect(lispish('(> 1)')).toBe(true)
      expect(lispish('(> 1 2)')).toBe(false)
      expect(lispish('(> 1 1)')).toBe(false)
      expect(lispish('(> 2 1)')).toBe(true)
      expect(lispish('(> 2 1 2)')).toBe(false)
      expect(lispish('(> 2 1 0)')).toBe(true)
      expect(() => lispish('(> "1")')).toThrow()
      expect(() => lispish('(> "1" "3")')).toThrow()
    })
  })

  describe('<', () => {
    test('samples', () => {
      expect(() => lispish('(<)')).toThrow()
      expect(lispish('(< 1)')).toBe(true)
      expect(lispish('(< 1 2)')).toBe(true)
      expect(lispish('(< 1 1)')).toBe(false)
      expect(lispish('(< 2 1)')).toBe(false)
      expect(lispish('(< 1 2 1)')).toBe(false)
      expect(lispish('(< 0 1 2)')).toBe(true)
      expect(() => lispish('(< "1")')).toThrow()
      expect(() => lispish('(< "1" "3")')).toThrow()
    })
  })

  describe('>=', () => {
    test('samples', () => {
      expect(() => lispish('(>=)')).toThrow()
      expect(lispish('(>= 1)')).toBe(true)
      expect(lispish('(>= 1 2)')).toBe(false)
      expect(lispish('(>= 1 1)')).toBe(true)
      expect(lispish('(>= 2 1)')).toBe(true)
      expect(lispish('(>= 2 1 2)')).toBe(false)
      expect(lispish('(>= 2 1 1)')).toBe(true)
      expect(() => lispish('(>= "1")')).toThrow()
      expect(() => lispish('(>= "1" "3")')).toThrow()
    })
  })

  describe('<=', () => {
    test('samples', () => {
      expect(() => lispish('(<=)')).toThrow()
      expect(lispish('(<= 1)')).toBe(true)
      expect(lispish('(<= 1 2)')).toBe(true)
      expect(lispish('(<= 1 1)')).toBe(true)
      expect(lispish('(<= 2 1)')).toBe(false)
      expect(lispish('(<= 1 2 1)')).toBe(false)
      expect(lispish('(<= 1 2 2)')).toBe(true)
      expect(() => lispish('(<= "1")')).toThrow()
      expect(() => lispish('(<= "1" "3")')).toThrow()
    })
  })

  describe('not', () => {
    test('samples', () => {
      expect(() => lispish('(not)')).toThrow()
      expect(lispish('(not 0)')).toBe(true)
      expect(lispish('(not "")')).toBe(true)
      expect(lispish('(not "0")')).toBe(false)
      expect(lispish('(not 1)')).toBe(false)
      expect(lispish('(not -1)')).toBe(false)
      expect(lispish('(not (array))')).toBe(false)
      expect(lispish('(not false)')).toBe(true)
      expect(lispish('(not true)')).toBe(false)
      expect(lispish('(not null)')).toBe(true)
      expect(lispish('(not undefined)')).toBe(true)
      expect(() => lispish('(not 0 1)')).toThrow()
    })
  })

  describe('stringp', () => {
    test('samples', () => {
      expect(lispish(`(stringp "")`)).toBe(true)
      expect(lispish(`(stringp "x")`)).toBe(true)
      expect(lispish(`(stringp 1)`)).toBe(false)
      expect(lispish(`(stringp 0)`)).toBe(false)
      expect(lispish(`(stringp (array))`)).toBe(false)
      expect(lispish(`(stringp (object))`)).toBe(false)
      expect(lispish(`(stringp null)`)).toBe(false)
      expect(lispish(`(stringp true)`)).toBe(false)
      expect(lispish(`(stringp false)`)).toBe(false)
      expect(lispish(`(stringp undefined)`)).toBe(false)
      expect(() => lispish(`(stringp)`)).toThrow()
      expect(() => lispish(`(stringp "k" "k")`)).toThrow()
    })
  })

  describe('numberp', () => {
    test('samples', () => {
      expect(lispish(`(numberp 1)`)).toBe(true)
      expect(lispish(`(numberp 0)`)).toBe(true)
      expect(lispish(`(numberp -1)`)).toBe(true)
      expect(lispish(`(numberp -1.123)`)).toBe(true)
      expect(lispish(`(numberp 0.123)`)).toBe(true)
      expect(lispish(`(numberp "")`)).toBe(false)
      expect(lispish(`(numberp "x")`)).toBe(false)
      expect(lispish(`(numberp (array))`)).toBe(false)
      expect(lispish(`(numberp (object))`)).toBe(false)
      expect(lispish(`(numberp null)`)).toBe(false)
      expect(lispish(`(numberp false)`)).toBe(false)
      expect(lispish(`(numberp true)`)).toBe(false)
      expect(lispish(`(numberp undefined)`)).toBe(false)
      expect(() => lispish(`(numberp)`)).toThrow()
      expect(() => lispish(`(numberp 1 2)`)).toThrow()
    })
  })

  describe('booleanp', () => {
    test('samples', () => {
      expect(lispish(`(booleanp 1)`)).toBe(false)
      expect(lispish(`(booleanp 0)`)).toBe(false)
      expect(lispish(`(booleanp -1)`)).toBe(false)
      expect(lispish(`(booleanp -1.123)`)).toBe(false)
      expect(lispish(`(booleanp 0.123)`)).toBe(false)
      expect(lispish(`(booleanp "")`)).toBe(false)
      expect(lispish(`(booleanp "x")`)).toBe(false)
      expect(lispish(`(booleanp (array))`)).toBe(false)
      expect(lispish(`(booleanp (object))`)).toBe(false)
      expect(lispish(`(booleanp null)`)).toBe(false)
      expect(lispish(`(booleanp false)`)).toBe(true)
      expect(lispish(`(booleanp true)`)).toBe(true)
      expect(lispish(`(booleanp undefined)`)).toBe(false)
      expect(() => lispish(`(booleanp)`)).toThrow()
      expect(() => lispish(`(booleanp true false)`)).toThrow()
    })
  })

  describe('undefinedp', () => {
    test('samples', () => {
      expect(lispish(`(undefinedp 1)`)).toBe(false)
      expect(lispish(`(undefinedp 0)`)).toBe(false)
      expect(lispish(`(undefinedp -1)`)).toBe(false)
      expect(lispish(`(undefinedp -1.123)`)).toBe(false)
      expect(lispish(`(undefinedp 0.123)`)).toBe(false)
      expect(lispish(`(undefinedp "")`)).toBe(false)
      expect(lispish(`(undefinedp "x")`)).toBe(false)
      expect(lispish(`(undefinedp (array))`)).toBe(false)
      expect(lispish(`(undefinedp (object))`)).toBe(false)
      expect(lispish(`(undefinedp null)`)).toBe(false)
      expect(lispish(`(undefinedp false)`)).toBe(false)
      expect(lispish(`(undefinedp true)`)).toBe(false)
      expect(lispish(`(undefinedp undefined)`)).toBe(true)
      expect(() => lispish(`(undefinedp)`)).toThrow()
      expect(() => lispish(`(undefinedp true false)`)).toThrow()
    })
  })

  describe('nullp', () => {
    test('samples', () => {
      expect(lispish(`(nullp 1)`)).toBe(false)
      expect(lispish(`(nullp 0)`)).toBe(false)
      expect(lispish(`(nullp -1)`)).toBe(false)
      expect(lispish(`(nullp -1.123)`)).toBe(false)
      expect(lispish(`(nullp 0.123)`)).toBe(false)
      expect(lispish(`(nullp "")`)).toBe(false)
      expect(lispish(`(nullp "x")`)).toBe(false)
      expect(lispish(`(nullp (array))`)).toBe(false)
      expect(lispish(`(nullp (object))`)).toBe(false)
      expect(lispish(`(nullp null)`)).toBe(true)
      expect(lispish(`(nullp false)`)).toBe(false)
      expect(lispish(`(nullp true)`)).toBe(false)
      expect(lispish(`(nullp undefined)`)).toBe(false)
      expect(() => lispish(`(nullp)`)).toThrow()
      expect(() => lispish(`(nullp true false)`)).toThrow()
    })
  })

  describe('arrayp', () => {
    test('samples', () => {
      expect(lispish(`(arrayp 1)`)).toBe(false)
      expect(lispish(`(arrayp 0)`)).toBe(false)
      expect(lispish(`(arrayp -1)`)).toBe(false)
      expect(lispish(`(arrayp -1.123)`)).toBe(false)
      expect(lispish(`(arrayp 0.123)`)).toBe(false)
      expect(lispish(`(arrayp "")`)).toBe(false)
      expect(lispish(`(arrayp "x")`)).toBe(false)
      expect(lispish(`(arrayp (array))`)).toBe(true)
      expect(lispish(`(arrayp (object))`)).toBe(false)
      expect(lispish(`(arrayp null)`)).toBe(false)
      expect(lispish(`(arrayp false)`)).toBe(false)
      expect(lispish(`(arrayp true)`)).toBe(false)
      expect(lispish(`(arrayp undefined)`)).toBe(false)
      expect(() => lispish(`(arrayp)`)).toThrow()
      expect(() => lispish(`(arrayp true false)`)).toThrow()
    })
  })

  describe('objectp', () => {
    test('samples', () => {
      expect(lispish(`(objectp 1)`)).toBe(false)
      expect(lispish(`(objectp 0)`)).toBe(false)
      expect(lispish(`(objectp -1)`)).toBe(false)
      expect(lispish(`(objectp -1.123)`)).toBe(false)
      expect(lispish(`(objectp 0.123)`)).toBe(false)
      expect(lispish(`(objectp "")`)).toBe(false)
      expect(lispish(`(objectp "x")`)).toBe(false)
      expect(lispish(`(objectp (array))`)).toBe(false)
      expect(lispish(`(objectp (object "x" 10))`)).toBe(true)
      expect(lispish(`(objectp null)`)).toBe(false)
      expect(lispish(`(objectp false)`)).toBe(false)
      expect(lispish(`(objectp true)`)).toBe(false)
      expect(lispish(`(objectp undefined)`)).toBe(false)
      expect(() => lispish(`(objectp)`)).toThrow()
      expect(() => lispish(`(objectp true false)`)).toThrow()
    })
  })

  describe('substring', () => {
    test('samples', () => {
      expect(() => lispish(`(substring "abcde")`)).toThrow()
      expect(lispish(`(substring "abcde" 0)`)).toBe('abcde')
      expect(lispish(`(substring "abcde" 1)`)).toBe('bcde')
      expect(lispish(`(substring "abcde" 2)`)).toBe('cde')
      expect(lispish(`(substring "abcde" 3)`)).toBe('de')
      expect(lispish(`(substring "abcde" 4)`)).toBe('e')
      expect(lispish(`(substring "abcde" 5)`)).toBe('')
      expect(lispish(`(substring "abcde" 6)`)).toBe('')
      expect(lispish(`(substring "abcde" 0 0)`)).toBe('')
      expect(() => lispish(`(substring "abcde" 1 0)`)).toThrow()
      expect(lispish(`(substring "abcde" 1 1)`)).toBe('')
      expect(lispish(`(substring "abcde" 1 2)`)).toBe('b')
      expect(lispish(`(substring "abcde" 1 3)`)).toBe('bc')
      expect(lispish(`(substring "abcde" 1 4)`)).toBe('bcd')
      expect(lispish(`(substring "abcde" 1 5)`)).toBe('bcde')
      expect(lispish(`(substring "abcde" 1 6)`)).toBe('bcde')
    })
  })

  describe('lenght', () => {
    test('samples', () => {
      expect(lispish(`(length "")`)).toBe(0)
      expect(lispish(`(length "1")`)).toBe(1)
      expect(lispish(`(length "123")`)).toBe(3)
      expect(lispish(`(length (array))`)).toBe(0)
      expect(lispish(`(length (array 1))`)).toBe(1)
      expect(lispish(`(length (array 1 2 3))`)).toBe(3)
      expect(() => lispish(`(length)`)).toThrow()
      expect(() => lispish(`(length "" "")`)).toThrow()
      expect(() => lispish(`(length 12)`)).toThrow()
      expect(() => lispish(`(length false)`)).toThrow()
      expect(() => lispish(`(length true)`)).toThrow()
      expect(() => lispish(`(length null)`)).toThrow()
      expect(() => lispish(`(length undefined)`)).toThrow()
      expect(() => lispish(`(length (object))`)).toThrow()
    })
  })

  describe('concat', () => {
    test('samples', () => {
      expect(lispish(`(concat)`)).toBe('')
      expect(lispish(`(concat "")`)).toBe('')
      expect(lispish(`(concat "1")`)).toBe('1')
      expect(lispish(`(concat "1" "2")`)).toBe('12')
      expect(lispish(`(concat "1" "2" "three" "4")`)).toBe('12three4')
      expect(() => lispish(`(concat 0)`)).toThrow()
      expect(() => lispish(`(concat true)`)).toThrow()
      expect(() => lispish(`(concat "1" false)`)).toThrow()
      expect(() => lispish(`(concat null "m")`)).toThrow()
      expect(() => lispish(`(concat undefined)`)).toThrow()
      expect(() => lispish(`(concat (array))`)).toThrow()
      expect(() => lispish(`(concat (object))`)).toThrow()
    })
  })

  describe('array', () => {
    test('samples', () => {
      expect(lispish(`(array)`)).toEqual([])
      expect(lispish(`(array 1)`)).toEqual([1])
      expect(lispish(`(array 0 "1" null true false undefined (array (array)) (object))`)).toEqual([
        0,
        '1',
        null,
        true,
        false,
        undefined,
        [[]],
        {},
      ])
    })
  })

  describe('object', () => {
    test('samples', () => {
      expect(lispish(`(object)`)).toEqual({})
      expect(lispish(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(lispish(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(lispish(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" (array)))`)).toEqual({
        a: null,
        b: true,
        c: false,
        d: undefined,
        e: { x: [] },
      })
      expect(lispish(`(let ((a "a")) (object a 1))`)).toEqual({ a: 1 })
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x")`)).toThrow()
      expect(() => lispish(`(object "x" 1 "y")`)).toThrow()
      expect(() => lispish(`(object 0 1)`)).toThrow()
      expect(() => lispish(`(object true 1)`)).toThrow()
      expect(() => lispish(`(object false 1)`)).toThrow()
      expect(() => lispish(`(object null 1)`)).toThrow()
      expect(() => lispish(`(object undefined 1)`)).toThrow()
      expect(() => lispish(`(object (array) 1)`)).toThrow()
      expect(() => lispish(`(object (object) 1)`)).toThrow()
    })
  })

  describe('aref', () => {
    test('samples', () => {
      expect(lispish('(aref (array 1 2 3) 1)')).toBe(2)
      expect(lispish('(aref "Albert" 1)')).toBe('l')
      expect(lispish('(aref (array 1 2 3) 3)')).toBeUndefined()
      expect(() => lispish('(aref)')).toThrow()
      expect(() => lispish('(aref (object) 1)')).toThrow()
      expect(() => lispish('(aref null 2)')).toThrow()
      expect(() => lispish('(aref (array 1 2 3) 1 2)')).toThrow()
    })
  })

  describe('write', () => {
    test('samples', () => {
      expect(lispish('(write 1)')).toBe(1)
      expect(lispish('(write "1")')).toBe('1')
      expect(lispish('(write (array))')).toEqual([])
      expect(lispish('(write (object))')).toEqual({})
      expect(lispish('(write null)')).toBe(null)
      expect(lispish('(write undefined)')).toBe(undefined)
      expect(lispish('(write true)')).toBe(true)
      expect(lispish('(write false)')).toBe(false)
    })
    test('that it does console.log', () => {
      lispish('(write 1)')
      expect(logSpy).toHaveBeenCalledWith(1)
    })
  })

  describe('string>', () => {
    test('samples', () => {
      expect(lispish('(string> "albert" "ALBERT")')).toBe(true)
      expect(lispish('(string> "ALBERT" "albert")')).toBe(false)
      expect(lispish('(string> "albert" "alber")')).toBe(true)
      expect(lispish('(string> "albert" "albert")')).toBe(false)
      expect(lispish('(string> "alber" "albert")')).toBe(false)
      expect(() => lispish('(string>)')).toThrow()
      expect(() => lispish('(string> "a")')).toThrow()
      expect(() => lispish('(string> "a", "A", "Q")')).toThrow()
      expect(() => lispish('(string> 2 1)')).toThrow()
      expect(() => lispish('(string> null null)')).toThrow()
      expect(() => lispish('(string> undefined undefined)')).toThrow()
      expect(() => lispish('(string> true true)')).toThrow()
      expect(() => lispish('(string> false false)')).toThrow()
      expect(() => lispish('(string> "a" true)')).toThrow()
      expect(() => lispish('(string> true "a")')).toThrow()
      expect(() => lispish('(string> (array) "a")')).toThrow()
      expect(() => lispish('(string> (object) "a")')).toThrow()
    })
  })

  describe('string<', () => {
    test('samples', () => {
      expect(lispish('(string< "albert" "ALBERT")')).toBe(false)
      expect(lispish('(string< "ALBERT" "albert")')).toBe(true)
      expect(lispish('(string< "albert" "alber")')).toBe(false)
      expect(lispish('(string< "albert" "albert")')).toBe(false)
      expect(lispish('(string< "alber" "albert")')).toBe(true)
      expect(() => lispish('(string<)')).toThrow()
      expect(() => lispish('(string< "a")')).toThrow()
      expect(() => lispish('(string< "a", "A", "Q")')).toThrow()
      expect(() => lispish('(string< 2 1)')).toThrow()
      expect(() => lispish('(string< null null)')).toThrow()
      expect(() => lispish('(string< undefined undefined)')).toThrow()
      expect(() => lispish('(string< true true)')).toThrow()
      expect(() => lispish('(string< false false)')).toThrow()
      expect(() => lispish('(string< "a" true)')).toThrow()
      expect(() => lispish('(string< true "a")')).toThrow()
      expect(() => lispish('(string< (array) "a")')).toThrow()
      expect(() => lispish('(string< (object) "a")')).toThrow()
    })
  })

  describe('string>=', () => {
    test('samples', () => {
      expect(lispish('(string>= "albert" "ALBERT")')).toBe(true)
      expect(lispish('(string>= "ALBERT" "albert")')).toBe(false)
      expect(lispish('(string>= "albert" "alber")')).toBe(true)
      expect(lispish('(string>= "albert" "albert")')).toBe(true)
      expect(lispish('(string>= "alber" "albert")')).toBe(false)
      expect(() => lispish('(string>=)')).toThrow()
      expect(() => lispish('(string>= "a")')).toThrow()
      expect(() => lispish('(string>= "a", "A", "Q")')).toThrow()
      expect(() => lispish('(string>= 2 1)')).toThrow()
      expect(() => lispish('(string>= null null)')).toThrow()
      expect(() => lispish('(string>= undefined undefined)')).toThrow()
      expect(() => lispish('(string>= true true)')).toThrow()
      expect(() => lispish('(string>= false false)')).toThrow()
      expect(() => lispish('(string>= "a" true)')).toThrow()
      expect(() => lispish('(string>= true "a")')).toThrow()
      expect(() => lispish('(string>= (array) "a")')).toThrow()
      expect(() => lispish('(string>= (object) "a")')).toThrow()
    })
  })

  describe('string<=', () => {
    test('samples', () => {
      expect(lispish('(string<= "albert" "ALBERT")')).toBe(false)
      expect(lispish('(string<= "ALBERT" "albert")')).toBe(true)
      expect(lispish('(string<= "albert" "alber")')).toBe(false)
      expect(lispish('(string<= "albert" "albert")')).toBe(true)
      expect(lispish('(string<= "alber" "albert")')).toBe(true)
      expect(() => lispish('(string<=)')).toThrow()
      expect(() => lispish('(string<= "a")')).toThrow()
      expect(() => lispish('(string<= "a", "A", "Q")')).toThrow()
      expect(() => lispish('(string<= 2 1)')).toThrow()
      expect(() => lispish('(string<= null null)')).toThrow()
      expect(() => lispish('(string<= undefined undefined)')).toThrow()
      expect(() => lispish('(string<= true true)')).toThrow()
      expect(() => lispish('(string<= false false)')).toThrow()
      expect(() => lispish('(string<= "a" true)')).toThrow()
      expect(() => lispish('(string<= true "a")')).toThrow()
      expect(() => lispish('(string<= (array) "a")')).toThrow()
      expect(() => lispish('(string<= (object) "a")')).toThrow()
    })
  })
})
