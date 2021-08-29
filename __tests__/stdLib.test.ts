/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { executeProgram } from '../src'

describe('evaluator', () => {
  describe('+', () => {
    test('samples', () => {
      expect(executeProgram('(+)')).toBe(0)
      expect(executeProgram('(+ 2)')).toBe(2)
      expect(executeProgram('(+ 2 2)')).toBe(4)
      expect(executeProgram('(+ -2 2)')).toBe(0)
      expect(executeProgram('(+ 1 2 3 4)')).toBe(10)
      expect(() => executeProgram('(+ "1" 2 3 4)')).toThrow()
    })
  })

  describe('*', () => {
    test('samples', () => {
      expect(executeProgram('(*)')).toBe(1)
      expect(executeProgram('(* 2)')).toBe(2)
      expect(executeProgram('(* 2 2)')).toBe(4)
      expect(executeProgram('(* -2 2)')).toBe(-4)
      expect(executeProgram('(* 1 2 3 4)')).toBe(24)
      expect(() => executeProgram('(* "1" 2 3 4)')).toThrow()
    })
  })

  describe('/', () => {
    test('samples', () => {
      expect(executeProgram('(/)')).toBe(1)
      expect(executeProgram('(/ 2)')).toBe(1 / 2)
      expect(executeProgram('(/ 2 2)')).toBe(2 / 2)
      expect(executeProgram('(/ -2 2)')).toBe(-2 / 2)
      expect(executeProgram('(/ 1 2 3 4)')).toBe(1 / 2 / 3 / 4)
      expect(() => executeProgram('(/ "1" 2 3 4)')).toThrow()
    })
  })

  describe('-', () => {
    test('samples', () => {
      expect(executeProgram('(-)')).toBe(0)
      expect(executeProgram('(- 2)')).toBe(-2)
      expect(executeProgram('(- 2 2)')).toBe(2 - 2)
      expect(executeProgram('(- -2 2)')).toBe(-2 - 2)
      expect(executeProgram('(- 1 2 3 4)')).toBe(1 - 2 - 3 - 4)
      expect(() => executeProgram('(- "1" 2 3 4)')).toThrow()
    })
  })

  describe('%', () => {
    test('samples', () => {
      expect(() => executeProgram('(%)')).toThrow()
      expect(() => executeProgram('(% 3)')).toThrow()
      expect(() => executeProgram('(% 3 4 5)')).toThrow()
      expect(executeProgram('(% 2 1)')).toBe(0)
      expect(executeProgram('(% 2 2)')).toBe(0)
      expect(executeProgram('(% 3 2)')).toBe(1)
      expect(executeProgram('(% 3 -2)')).toBe(1)
      expect(executeProgram('(% -3 -2)')).toBe(-1)
      expect(executeProgram('(% -3 2)')).toBe(-1)
      expect(() => executeProgram('(% 4 0)')).toThrow()
    })
  })

  describe('!=', () => {
    test('samples', () => {
      expect(() => executeProgram('(!=)')).toThrow()
      expect(executeProgram('(!= 1)')).toBe(true)
      expect(executeProgram('(!= 1 1)')).toBe(false)
      expect(executeProgram('(!= 1 2)')).toBe(true)
      expect(executeProgram('(!= 1 2 1)')).toBe(false)
      expect(executeProgram('(!= 1 2 3)')).toBe(true)
      expect(executeProgram('(!= "1")')).toBe(true)
      expect(executeProgram('(!= "1" "1")')).toBe(false)
      expect(executeProgram('(!= "1" "2")')).toBe(true)
      expect(executeProgram('(!= "1" "2" "1")')).toBe(false)
      expect(executeProgram('(!= "1" "2" 3)')).toBe(true)
      expect(executeProgram('(!= null undefined)')).toBe(true)
      expect(executeProgram('(!= null 0)')).toBe(true)
      expect(executeProgram('(!= 1 undefined 1)')).toBe(false)
      expect(executeProgram('(!= 1 true 3)')).toBe(true)
      expect(executeProgram('(!= 1 false 3)')).toBe(true)
    })
  })

  describe('=', () => {
    test('samples', () => {
      expect(() => executeProgram('(=)')).toThrow()
      expect(executeProgram('(= 1)')).toBe(true)
      expect(executeProgram('(= 1 1)')).toBe(true)
      expect(executeProgram('(= 1 2)')).toBe(false)
      expect(executeProgram('(= 1 2 1)')).toBe(false)
      expect(executeProgram('(= 1 2 3)')).toBe(false)
      expect(executeProgram('(= "1")')).toBe(true)
      expect(executeProgram('(= "1" "1")')).toBe(true)
      expect(executeProgram('(= "1" "2")')).toBe(false)
      expect(executeProgram('(= "1" "2" "1")')).toBe(false)
      expect(executeProgram('(= "1" "2" "3")')).toBe(false)
      expect(executeProgram('(= "2" "2" "2")')).toBe(true)
      expect(executeProgram('(= 1 "2" 3)')).toBe(false)
      expect(executeProgram('(= 1 null 3)')).toBe(false)
      expect(executeProgram('(= 1 undefined 3)')).toBe(false)
      expect(executeProgram('(= 1 true 3)')).toBe(false)
      expect(executeProgram('(= 1 false 3)')).toBe(false)
      expect(executeProgram('(= null null)')).toBe(true)
      expect(executeProgram('(= undefined undefined)')).toBe(true)
      expect(executeProgram('(= true true)')).toBe(true)
      expect(executeProgram('(= false false)')).toBe(true)
      expect(executeProgram('(= null undefined)')).toBe(false)
    })
  })

  describe('>', () => {
    test('samples', () => {
      expect(() => executeProgram('(>)')).toThrow()
      expect(executeProgram('(> 1)')).toBe(true)
      expect(executeProgram('(> 1 2)')).toBe(false)
      expect(executeProgram('(> 1 1)')).toBe(false)
      expect(executeProgram('(> 2 1)')).toBe(true)
      expect(executeProgram('(> 2 1 2)')).toBe(false)
      expect(executeProgram('(> 2 1 0)')).toBe(true)
      expect(() => executeProgram('(> "1")')).toThrow()
      expect(() => executeProgram('(> "1" "3")')).toThrow()
    })
  })

  describe('<', () => {
    test('samples', () => {
      expect(() => executeProgram('(<)')).toThrow()
      expect(executeProgram('(< 1)')).toBe(true)
      expect(executeProgram('(< 1 2)')).toBe(true)
      expect(executeProgram('(< 1 1)')).toBe(false)
      expect(executeProgram('(< 2 1)')).toBe(false)
      expect(executeProgram('(< 1 2 1)')).toBe(false)
      expect(executeProgram('(< 0 1 2)')).toBe(true)
      expect(() => executeProgram('(< "1")')).toThrow()
      expect(() => executeProgram('(< "1" "3")')).toThrow()
    })
  })

  describe('>=', () => {
    test('samples', () => {
      expect(() => executeProgram('(>=)')).toThrow()
      expect(executeProgram('(>= 1)')).toBe(true)
      expect(executeProgram('(>= 1 2)')).toBe(false)
      expect(executeProgram('(>= 1 1)')).toBe(true)
      expect(executeProgram('(>= 2 1)')).toBe(true)
      expect(executeProgram('(>= 2 1 2)')).toBe(false)
      expect(executeProgram('(>= 2 1 1)')).toBe(true)
      expect(() => executeProgram('(>= "1")')).toThrow()
      expect(() => executeProgram('(>= "1" "3")')).toThrow()
    })
  })

  describe('<=', () => {
    test('samples', () => {
      expect(() => executeProgram('(<=)')).toThrow()
      expect(executeProgram('(<= 1)')).toBe(true)
      expect(executeProgram('(<= 1 2)')).toBe(true)
      expect(executeProgram('(<= 1 1)')).toBe(true)
      expect(executeProgram('(<= 2 1)')).toBe(false)
      expect(executeProgram('(<= 1 2 1)')).toBe(false)
      expect(executeProgram('(<= 1 2 2)')).toBe(true)
      expect(() => executeProgram('(<= "1")')).toThrow()
      expect(() => executeProgram('(<= "1" "3")')).toThrow()
    })
  })

  describe('and', () => {
    test('samples', () => {
      expect(executeProgram('(and)')).toBe(true)
      expect(executeProgram('(and 0)')).toBe(0)
      expect(executeProgram('(and 0 1)')).toBe(0)
      expect(executeProgram('(and 2 0)')).toBe(0)
      expect(executeProgram('(and 2 0 1)')).toBe(0)
      expect(executeProgram('(and 2 3 0)')).toBe(0)
      expect(executeProgram('(and 2 3 "")')).toBe('')
      expect(executeProgram('(and 2 3 "x")')).toBe('x')
      expect(executeProgram('(and false 1)')).toBe(false)
      expect(executeProgram('(and 1 false)')).toBe(false)
      expect(executeProgram('(and 1 undefined)')).toBe(undefined)
      expect(executeProgram('(and 1 null)')).toBe(null)
      expect(executeProgram('(and 2 2 false)')).toBe(false)
      expect(executeProgram('(and 3 true 3)')).toBe(3)
    })
  })

  describe('or', () => {
    test('samples', () => {
      expect(executeProgram('(or)')).toBe(false)
      expect(executeProgram('(or 0)')).toBe(0)
      expect(executeProgram('(or 0 1)')).toBe(1)
      expect(executeProgram('(or 2 0)')).toBe(2)
      expect(executeProgram('(or null 0 false undefined)')).toBe(undefined)
      expect(executeProgram('(or null 0 1 undefined)')).toBe(1)
    })
  })

  describe('not', () => {
    test('samples', () => {
      expect(() => executeProgram('(not)')).toThrow()
      expect(executeProgram('(not 0)')).toBe(true)
      expect(executeProgram('(not "")')).toBe(true)
      expect(executeProgram('(not "0")')).toBe(false)
      expect(executeProgram('(not 1)')).toBe(false)
      expect(executeProgram('(not -1)')).toBe(false)
      expect(executeProgram('(not (array))')).toBe(false)
      expect(executeProgram('(not false)')).toBe(true)
      expect(executeProgram('(not true)')).toBe(false)
      expect(executeProgram('(not null)')).toBe(true)
      expect(executeProgram('(not undefined)')).toBe(true)
      expect(() => executeProgram('(not 0 1)')).toThrow()
    })
  })

  describe('stringp', () => {
    test('samples', () => {
      expect(executeProgram(`(stringp "")`)).toBe(true)
      expect(executeProgram(`(stringp "x")`)).toBe(true)
      expect(executeProgram(`(stringp 1)`)).toBe(false)
      expect(executeProgram(`(stringp 0)`)).toBe(false)
      expect(executeProgram(`(stringp (array))`)).toBe(false)
      expect(executeProgram(`(stringp (object))`)).toBe(false)
      expect(executeProgram(`(stringp null)`)).toBe(false)
      expect(executeProgram(`(stringp true)`)).toBe(false)
      expect(executeProgram(`(stringp false)`)).toBe(false)
      expect(executeProgram(`(stringp undefined)`)).toBe(false)
      expect(() => executeProgram(`(stringp)`)).toThrow()
      expect(() => executeProgram(`(stringp "k" "k")`)).toThrow()
    })
  })

  describe('numberp', () => {
    test('samples', () => {
      expect(executeProgram(`(numberp 1)`)).toBe(true)
      expect(executeProgram(`(numberp 0)`)).toBe(true)
      expect(executeProgram(`(numberp -1)`)).toBe(true)
      expect(executeProgram(`(numberp -1.123)`)).toBe(true)
      expect(executeProgram(`(numberp 0.123)`)).toBe(true)
      expect(executeProgram(`(numberp "")`)).toBe(false)
      expect(executeProgram(`(numberp "x")`)).toBe(false)
      expect(executeProgram(`(numberp (array))`)).toBe(false)
      expect(executeProgram(`(numberp (object))`)).toBe(false)
      expect(executeProgram(`(numberp null)`)).toBe(false)
      expect(executeProgram(`(numberp false)`)).toBe(false)
      expect(executeProgram(`(numberp true)`)).toBe(false)
      expect(executeProgram(`(numberp undefined)`)).toBe(false)
      expect(() => executeProgram(`(numberp)`)).toThrow()
      expect(() => executeProgram(`(numberp 1 2)`)).toThrow()
    })
  })

  describe('booleanp', () => {
    test('samples', () => {
      expect(executeProgram(`(booleanp 1)`)).toBe(false)
      expect(executeProgram(`(booleanp 0)`)).toBe(false)
      expect(executeProgram(`(booleanp -1)`)).toBe(false)
      expect(executeProgram(`(booleanp -1.123)`)).toBe(false)
      expect(executeProgram(`(booleanp 0.123)`)).toBe(false)
      expect(executeProgram(`(booleanp "")`)).toBe(false)
      expect(executeProgram(`(booleanp "x")`)).toBe(false)
      expect(executeProgram(`(booleanp (array))`)).toBe(false)
      expect(executeProgram(`(booleanp (object))`)).toBe(false)
      expect(executeProgram(`(booleanp null)`)).toBe(false)
      expect(executeProgram(`(booleanp false)`)).toBe(true)
      expect(executeProgram(`(booleanp true)`)).toBe(true)
      expect(executeProgram(`(booleanp undefined)`)).toBe(false)
      expect(() => executeProgram(`(booleanp)`)).toThrow()
      expect(() => executeProgram(`(booleanp true false)`)).toThrow()
    })
  })

  describe('undefinedp', () => {
    test('samples', () => {
      expect(executeProgram(`(undefinedp 1)`)).toBe(false)
      expect(executeProgram(`(undefinedp 0)`)).toBe(false)
      expect(executeProgram(`(undefinedp -1)`)).toBe(false)
      expect(executeProgram(`(undefinedp -1.123)`)).toBe(false)
      expect(executeProgram(`(undefinedp 0.123)`)).toBe(false)
      expect(executeProgram(`(undefinedp "")`)).toBe(false)
      expect(executeProgram(`(undefinedp "x")`)).toBe(false)
      expect(executeProgram(`(undefinedp (array))`)).toBe(false)
      expect(executeProgram(`(undefinedp (object))`)).toBe(false)
      expect(executeProgram(`(undefinedp null)`)).toBe(false)
      expect(executeProgram(`(undefinedp false)`)).toBe(false)
      expect(executeProgram(`(undefinedp true)`)).toBe(false)
      expect(executeProgram(`(undefinedp undefined)`)).toBe(true)
      expect(() => executeProgram(`(undefinedp)`)).toThrow()
      expect(() => executeProgram(`(undefinedp true false)`)).toThrow()
    })
  })

  describe('nullp', () => {
    test('samples', () => {
      expect(executeProgram(`(nullp 1)`)).toBe(false)
      expect(executeProgram(`(nullp 0)`)).toBe(false)
      expect(executeProgram(`(nullp -1)`)).toBe(false)
      expect(executeProgram(`(nullp -1.123)`)).toBe(false)
      expect(executeProgram(`(nullp 0.123)`)).toBe(false)
      expect(executeProgram(`(nullp "")`)).toBe(false)
      expect(executeProgram(`(nullp "x")`)).toBe(false)
      expect(executeProgram(`(nullp (array))`)).toBe(false)
      expect(executeProgram(`(nullp (object))`)).toBe(false)
      expect(executeProgram(`(nullp null)`)).toBe(true)
      expect(executeProgram(`(nullp false)`)).toBe(false)
      expect(executeProgram(`(nullp true)`)).toBe(false)
      expect(executeProgram(`(nullp undefined)`)).toBe(false)
      expect(() => executeProgram(`(nullp)`)).toThrow()
      expect(() => executeProgram(`(nullp true false)`)).toThrow()
    })
  })

  describe('arrayp', () => {
    test('samples', () => {
      expect(executeProgram(`(arrayp 1)`)).toBe(false)
      expect(executeProgram(`(arrayp 0)`)).toBe(false)
      expect(executeProgram(`(arrayp -1)`)).toBe(false)
      expect(executeProgram(`(arrayp -1.123)`)).toBe(false)
      expect(executeProgram(`(arrayp 0.123)`)).toBe(false)
      expect(executeProgram(`(arrayp "")`)).toBe(false)
      expect(executeProgram(`(arrayp "x")`)).toBe(false)
      expect(executeProgram(`(arrayp (array))`)).toBe(true)
      expect(executeProgram(`(arrayp (object))`)).toBe(false)
      expect(executeProgram(`(arrayp null)`)).toBe(false)
      expect(executeProgram(`(arrayp false)`)).toBe(false)
      expect(executeProgram(`(arrayp true)`)).toBe(false)
      expect(executeProgram(`(arrayp undefined)`)).toBe(false)
      expect(() => executeProgram(`(arrayp)`)).toThrow()
      expect(() => executeProgram(`(arrayp true false)`)).toThrow()
    })
  })

  describe('objectp', () => {
    test('samples', () => {
      expect(executeProgram(`(objectp 1)`)).toBe(false)
      expect(executeProgram(`(objectp 0)`)).toBe(false)
      expect(executeProgram(`(objectp -1)`)).toBe(false)
      expect(executeProgram(`(objectp -1.123)`)).toBe(false)
      expect(executeProgram(`(objectp 0.123)`)).toBe(false)
      expect(executeProgram(`(objectp "")`)).toBe(false)
      expect(executeProgram(`(objectp "x")`)).toBe(false)
      expect(executeProgram(`(objectp (array))`)).toBe(false)
      expect(executeProgram(`(objectp (object "x" 10))`)).toBe(true)
      expect(executeProgram(`(objectp null)`)).toBe(false)
      expect(executeProgram(`(objectp false)`)).toBe(false)
      expect(executeProgram(`(objectp true)`)).toBe(false)
      expect(executeProgram(`(objectp undefined)`)).toBe(false)
      expect(() => executeProgram(`(objectp)`)).toThrow()
      expect(() => executeProgram(`(objectp true false)`)).toThrow()
    })
  })

  describe('substring', () => {
    test('samples', () => {
      expect(() => executeProgram(`(substring "abcde")`)).toThrow()
      expect(executeProgram(`(substring "abcde" 0)`)).toBe('abcde')
      expect(executeProgram(`(substring "abcde" 1)`)).toBe('bcde')
      expect(executeProgram(`(substring "abcde" 2)`)).toBe('cde')
      expect(executeProgram(`(substring "abcde" 3)`)).toBe('de')
      expect(executeProgram(`(substring "abcde" 4)`)).toBe('e')
      expect(executeProgram(`(substring "abcde" 5)`)).toBe('')
      expect(executeProgram(`(substring "abcde" 6)`)).toBe('')
      expect(executeProgram(`(substring "abcde" 0 0)`)).toBe('')
      expect(() => executeProgram(`(substring "abcde" 1 0)`)).toThrow()
      expect(executeProgram(`(substring "abcde" 1 1)`)).toBe('')
      expect(executeProgram(`(substring "abcde" 1 2)`)).toBe('b')
      expect(executeProgram(`(substring "abcde" 1 3)`)).toBe('bc')
      expect(executeProgram(`(substring "abcde" 1 4)`)).toBe('bcd')
      expect(executeProgram(`(substring "abcde" 1 5)`)).toBe('bcde')
      expect(executeProgram(`(substring "abcde" 1 6)`)).toBe('bcde')
    })
  })

  describe('lenght', () => {
    test('samples', () => {
      expect(executeProgram(`(length "")`)).toBe(0)
      expect(executeProgram(`(length "1")`)).toBe(1)
      expect(executeProgram(`(length "123")`)).toBe(3)
      expect(executeProgram(`(length (array))`)).toBe(0)
      expect(executeProgram(`(length (array 1))`)).toBe(1)
      expect(executeProgram(`(length (array 1 2 3))`)).toBe(3)
      expect(() => executeProgram(`(length)`)).toThrow()
      expect(() => executeProgram(`(length "" "")`)).toThrow()
      expect(() => executeProgram(`(length 12)`)).toThrow()
      expect(() => executeProgram(`(length false)`)).toThrow()
      expect(() => executeProgram(`(length true)`)).toThrow()
      expect(() => executeProgram(`(length null)`)).toThrow()
      expect(() => executeProgram(`(length undefined)`)).toThrow()
      expect(() => executeProgram(`(length (object))`)).toThrow()
    })
  })

  describe('concat', () => {
    test('samples', () => {
      expect(executeProgram(`(concat)`)).toBe('')
      expect(executeProgram(`(concat "")`)).toBe('')
      expect(executeProgram(`(concat "1")`)).toBe('1')
      expect(executeProgram(`(concat "1" "2")`)).toBe('12')
      expect(executeProgram(`(concat "1" "2" "three" "4")`)).toBe('12three4')
      expect(() => executeProgram(`(concat 0)`)).toThrow()
      expect(() => executeProgram(`(concat true)`)).toThrow()
      expect(() => executeProgram(`(concat "1" false)`)).toThrow()
      expect(() => executeProgram(`(concat null "m")`)).toThrow()
      expect(() => executeProgram(`(concat undefined)`)).toThrow()
      expect(() => executeProgram(`(concat (array))`)).toThrow()
      expect(() => executeProgram(`(concat (object))`)).toThrow()
    })
  })

  describe('array', () => {
    test('samples', () => {
      expect(executeProgram(`(array)`)).toEqual([])
      expect(executeProgram(`(array 1)`)).toEqual([1])
      expect(executeProgram(`(array 0 "1" null true false undefined (array (array)) (object))`)).toEqual([
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
      expect(executeProgram(`(object)`)).toEqual({})
      expect(executeProgram(`(object "x" 1)`)).toEqual({ x: 1 })
      expect(executeProgram(`(object "x" 1 "x" 2)`)).toEqual({ x: 2 })
      expect(executeProgram(`(object "a" null "b" true "c" false "d" undefined "e" (object "x" (array)))`)).toEqual({
        a: null,
        b: true,
        c: false,
        d: undefined,
        e: { x: [] },
      })
      expect(executeProgram(`(let ((a "a")) (object a 1))`)).toEqual({ a: 1 })
      expect(() => executeProgram(`(object "x")`)).toThrow()
      expect(() => executeProgram(`(object "x")`)).toThrow()
      expect(() => executeProgram(`(object "x" 1 "y")`)).toThrow()
      expect(() => executeProgram(`(object 0 1)`)).toThrow()
      expect(() => executeProgram(`(object true 1)`)).toThrow()
      expect(() => executeProgram(`(object false 1)`)).toThrow()
      expect(() => executeProgram(`(object null 1)`)).toThrow()
      expect(() => executeProgram(`(object undefined 1)`)).toThrow()
      expect(() => executeProgram(`(object (array) 1)`)).toThrow()
      expect(() => executeProgram(`(object (object) 1)`)).toThrow()
    })
  })

  describe('aref', () => {
    test('samples', () => {
      expect(executeProgram('(aref (array 1 2 3) 1)')).toBe(2)
      expect(executeProgram('(aref "Albert" 1)')).toBe('l')
      expect(executeProgram('(aref (array 1 2 3) 3)')).toBeUndefined()
      expect(() => executeProgram('(aref)')).toThrow()
      expect(() => executeProgram('(aref (object) 1)')).toThrow()
      expect(() => executeProgram('(aref null 2)')).toThrow()
      expect(() => executeProgram('(aref (array 1 2 3) 1 2)')).toThrow()
    })
  })

  describe('write', () => {
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
    test('samples', () => {
      expect(executeProgram('(write 1)')).toBe(1)
      expect(executeProgram('(write "1")')).toBe('1')
      expect(executeProgram('(write (array))')).toEqual([])
      expect(executeProgram('(write (object))')).toEqual({})
      expect(executeProgram('(write null)')).toBe(null)
      expect(executeProgram('(write undefined)')).toBe(undefined)
      expect(executeProgram('(write true)')).toBe(true)
      expect(executeProgram('(write false)')).toBe(false)
    })
    test('that it does console.log', () => {
      executeProgram('(write 1)')
      expect(logSpy).toHaveBeenCalledWith('LISPISH>', 1)
    })

    describe('string>', () => {
      test('samples', () => {
        expect(executeProgram('(string> "albert" "ALBERT")')).toBe(true)
        expect(executeProgram('(string> "ALBERT" "albert")')).toBe(false)
        expect(executeProgram('(string> "albert" "alber")')).toBe(true)
        expect(executeProgram('(string> "albert" "albert")')).toBe(false)
        expect(executeProgram('(string> "alber" "albert")')).toBe(false)
        expect(() => executeProgram('(string>)')).toThrow()
        expect(() => executeProgram('(string> "a")')).toThrow()
        expect(() => executeProgram('(string> "a", "A", "Q")')).toThrow()
        expect(() => executeProgram('(string> 2 1)')).toThrow()
        expect(() => executeProgram('(string> null null)')).toThrow()
        expect(() => executeProgram('(string> undefined undefined)')).toThrow()
        expect(() => executeProgram('(string> true true)')).toThrow()
        expect(() => executeProgram('(string> false false)')).toThrow()
        expect(() => executeProgram('(string> "a" true)')).toThrow()
        expect(() => executeProgram('(string> true "a")')).toThrow()
        expect(() => executeProgram('(string> (array) "a")')).toThrow()
        expect(() => executeProgram('(string> (object) "a")')).toThrow()
      })
    })

    describe('string<', () => {
      test('samples', () => {
        expect(executeProgram('(string< "albert" "ALBERT")')).toBe(false)
        expect(executeProgram('(string< "ALBERT" "albert")')).toBe(true)
        expect(executeProgram('(string< "albert" "alber")')).toBe(false)
        expect(executeProgram('(string< "albert" "albert")')).toBe(false)
        expect(executeProgram('(string< "alber" "albert")')).toBe(true)
        expect(() => executeProgram('(string<)')).toThrow()
        expect(() => executeProgram('(string< "a")')).toThrow()
        expect(() => executeProgram('(string< "a", "A", "Q")')).toThrow()
        expect(() => executeProgram('(string< 2 1)')).toThrow()
        expect(() => executeProgram('(string< null null)')).toThrow()
        expect(() => executeProgram('(string< undefined undefined)')).toThrow()
        expect(() => executeProgram('(string< true true)')).toThrow()
        expect(() => executeProgram('(string< false false)')).toThrow()
        expect(() => executeProgram('(string< "a" true)')).toThrow()
        expect(() => executeProgram('(string< true "a")')).toThrow()
        expect(() => executeProgram('(string< (array) "a")')).toThrow()
        expect(() => executeProgram('(string< (object) "a")')).toThrow()
      })
    })

    describe('string>=', () => {
      test('samples', () => {
        expect(executeProgram('(string>= "albert" "ALBERT")')).toBe(true)
        expect(executeProgram('(string>= "ALBERT" "albert")')).toBe(false)
        expect(executeProgram('(string>= "albert" "alber")')).toBe(true)
        expect(executeProgram('(string>= "albert" "albert")')).toBe(true)
        expect(executeProgram('(string>= "alber" "albert")')).toBe(false)
        expect(() => executeProgram('(string>=)')).toThrow()
        expect(() => executeProgram('(string>= "a")')).toThrow()
        expect(() => executeProgram('(string>= "a", "A", "Q")')).toThrow()
        expect(() => executeProgram('(string>= 2 1)')).toThrow()
        expect(() => executeProgram('(string>= null null)')).toThrow()
        expect(() => executeProgram('(string>= undefined undefined)')).toThrow()
        expect(() => executeProgram('(string>= true true)')).toThrow()
        expect(() => executeProgram('(string>= false false)')).toThrow()
        expect(() => executeProgram('(string>= "a" true)')).toThrow()
        expect(() => executeProgram('(string>= true "a")')).toThrow()
        expect(() => executeProgram('(string>= (array) "a")')).toThrow()
        expect(() => executeProgram('(string>= (object) "a")')).toThrow()
      })
    })

    describe('string<=', () => {
      test('samples', () => {
        expect(executeProgram('(string<= "albert" "ALBERT")')).toBe(false)
        expect(executeProgram('(string<= "ALBERT" "albert")')).toBe(true)
        expect(executeProgram('(string<= "albert" "alber")')).toBe(false)
        expect(executeProgram('(string<= "albert" "albert")')).toBe(true)
        expect(executeProgram('(string<= "alber" "albert")')).toBe(true)
        expect(() => executeProgram('(string<=)')).toThrow()
        expect(() => executeProgram('(string<= "a")')).toThrow()
        expect(() => executeProgram('(string<= "a", "A", "Q")')).toThrow()
        expect(() => executeProgram('(string<= 2 1)')).toThrow()
        expect(() => executeProgram('(string<= null null)')).toThrow()
        expect(() => executeProgram('(string<= undefined undefined)')).toThrow()
        expect(() => executeProgram('(string<= true true)')).toThrow()
        expect(() => executeProgram('(string<= false false)')).toThrow()
        expect(() => executeProgram('(string<= "a" true)')).toThrow()
        expect(() => executeProgram('(string<= true "a")')).toThrow()
        expect(() => executeProgram('(string<= (array) "a")')).toThrow()
        expect(() => executeProgram('(string<= (object) "a")')).toThrow()
      })
    })

    describe('setq', () => {
      test('samples', () => {
        expect(executeProgram(`(setq a 10) a`)).toBe(10)
        expect(executeProgram(`(setq a 10) (setq a 20) a`)).toBe(20)
        expect(() => executeProgram(`(setq a)`)).toThrow()
        expect(() => executeProgram(`(setq a 10 10)`)).toThrow()
        expect(() => executeProgram(`(setq 1 10)`)).toThrow()
        expect(() => executeProgram(`(setq null 10)`)).toThrow()
        expect(() => executeProgram(`(setq undefined 10)`)).toThrow()
        expect(() => executeProgram(`(setq false 10)`)).toThrow()
        expect(() => executeProgram(`(setq true 10)`)).toThrow()
        expect(() => executeProgram(`(setq (array) 10)`)).toThrow()
        expect(() => executeProgram(`(setq (object) 10)`)).toThrow()
        expect(() => executeProgram(`(setq "a" 10)`)).toThrow()
      })
    })
    describe('if', () => {
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
      test('samples', () => {
        expect(executeProgram(`(if true "A" "B")`)).toBe('A')
        expect(executeProgram(`(if false "A" "B")`)).toBe('B')
        expect(executeProgram(`(if null "A" "B")`)).toBe('B')
        expect(executeProgram(`(if undefined "A" "B")`)).toBe('B')
        expect(executeProgram(`(if "" "A" "B")`)).toBe('B')
        expect(executeProgram(`(if "x" "A" "B")`)).toBe('A')
        expect(executeProgram(`(if 0 "A" "B")`)).toBe('B')
        expect(executeProgram(`(if 1 "A" "B")`)).toBe('A')
        expect(executeProgram(`(if -1 "A" "B")`)).toBe('A')
        expect(executeProgram(`(if (array) "A" "B")`)).toBe('A')
        expect(executeProgram(`(if (object) "A" "B")`)).toBe('A')
        expect(() => executeProgram(`(if)`)).toThrow()
        expect(() => executeProgram(`(if true)`)).toThrow()
        expect(() => executeProgram(`(if true "A")`)).toThrow()
        expect(() => executeProgram(`(if true "A" "B" "Q")`)).toThrow()
      })
      test('That special form "if" only evaluate the correct path (true)', () => {
        executeProgram(`(if true (write "A") (write "B"))`)
        expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'A')
        expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'B')
      })
      test('That special form "if" only evaluate the correct path (false)', () => {
        executeProgram(`(if false (write "A") (write "B"))`)
        expect(logSpy).not.toHaveBeenCalledWith('LISPISH>', 'A')
        expect(logSpy).toHaveBeenCalledWith('LISPISH>', 'B')
      })
    })
  })
})
