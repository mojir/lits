import { describe, expect, it, test } from 'vitest'
import { Lits } from '../Lits/Lits'

const lits = new Lits()
const litsDebug = new Lits({ debug: true })

describe('algebraic operators', () => {
  describe('const E', () => {
    it('samples', () => {
      expect(lits.run('E')).toBe(Math.E)
      expect(lits.run('ε')).toBe(Math.E)
      expect(lits.run('-E')).toBe(-Math.E)
      expect(lits.run('-ε')).toBe(-Math.E)
    })
  })

  describe('test', () => {
    it('samples', () => {
      // litsDebug.run('let a =')
    })
  })

  describe('const MAX_SAFE_INTEGER', () => {
    it('samples', () => {
      expect(lits.run('MAX_SAFE_INTEGER')).toBe(Number.MAX_SAFE_INTEGER)
    })
  })

  describe('const MIN_SAFE_INTEGER', () => {
    it('samples', () => {
      expect(lits.run('MIN_SAFE_INTEGER')).toBe(Number.MIN_SAFE_INTEGER)
    })
  })

  describe('const MAX_VALUE', () => {
    it('samples', () => {
      expect(lits.run('MAX_VALUE')).toBe(Number.MAX_VALUE)
    })
  })

  describe('const MIN_VALUE', () => {
    it('samples', () => {
      expect(lits.run('MIN_VALUE')).toBe(Number.MIN_VALUE)
      expect(() => lits.run('(min_value :1)')).toThrow()
    })
  })

  describe('const DELTA', () => {
    it('samples', () => {
      expect(lits.run('DELTA')).toBe(Number.EPSILON)
    })
  })

  describe('const NaN', () => {
    it('samples', () => {
      expect(lits.run('NaN')).toBeNaN()
    })
  })

  describe('const POSITIVE_INFINITY', () => {
    it('samples', () => {
      expect(lits.run('POSITIVE_INFINITY')).toBe(Number.POSITIVE_INFINITY)
    })
  })

  describe('const NEGATIVE_INFINITY', () => {
    it('samples', () => {
      expect(lits.run('(NEGATIVE_INFINITY)')).toBe(Number.NEGATIVE_INFINITY)
    })
  })

  describe('const PI', () => {
    it('samples', () => {
      expect(lits.run('PI')).toBe(Math.PI)
      expect(lits.run('π')).toBe(Math.PI)
    })
  })

  describe('**', () => {
    test('samples', () => {
      expect(lits.run('2 ** 3')).toBe(8)
      expect(lits.run('2 ** 3 ** 2')).toBe(512)
    })
  })
  describe('*', () => {
    test('samples', () => {
      expect(lits.run('-2 * 3')).toBe(-6)
      expect(lits.run('2 * 3 * 2')).toBe(12)
    })
  })
  describe('/', () => {
    test('samples', () => {
      expect(lits.run('6 / 3')).toBe(2)
      expect(lits.run('6 / 3 / 2')).toBe(1)
    })
  })
  describe('%', () => {
    test('samples', () => {
      expect(lits.run('6 % 3')).toBe(0)
      expect(lits.run('6 % 4')).toBe(2)
      expect(lits.run('-6 % 4')).toBe(-2)
      expect(lits.run('6 % -4')).toBe(2)
      expect(lits.run('-6 % -4')).toBe(-2)
    })
  })
  describe('+', () => {
    test('samples', () => {
      expect(lits.run('2 + 3')).toBe(5)
      expect(lits.run('2 + 3 + 2')).toBe(7)
    })
  })
  describe('-', () => {
    test('samples', () => {
      expect(lits.run('2 - 3')).toBe(-1)
      expect(lits.run('2 - 3 - 2')).toBe(-3)
    })
  })
  describe('<<', () => {
    test('samples', () => {
      expect(lits.run('2 << 3')).toBe(16)
      expect(lits.run('2 << 3 << 2')).toBe(64)
    })
  })
  describe('>>', () => {
    test('samples', () => {
      expect(lits.run('16 >> 3')).toBe(2)
      expect(lits.run('64 >> 3 >> 2')).toBe(2)
      expect(lits.run('-16 >> 2')).toBe(-4)
    })
  })
  describe('>>>', () => {
    test('samples', () => {
      expect(lits.run('16 >>> 3')).toBe(2)
      expect(lits.run('1 >>> 1')).toBe(0)
      expect(lits.run('1 >>> 2')).toBe(0)
      expect(lits.run('-16 >>> 2')).toBe(0x3FFFFFFC)
      expect(lits.run('64 >>> 3 >>> 2')).toBe(2)
    })
  })
  describe('++', () => {
    test('samples', () => {
      expect(lits.run('"Foo" ++ "Bar"')).toBe('FooBar')
      expect(lits.run('2 ++ 3')).toBe('23')
      expect(lits.run('2 ++ 3 * 2')).toBe('26')
      expect(lits.run('"Hello" ++ " " ++ "World"')).toBe('Hello World')
    })
  })
  describe('<', () => {
    test('samples', () => {
      expect(lits.run('2 < 3')).toBe(true)
      expect(lits.run('2 < 2')).toBe(false)
      expect(lits.run('2 < 1')).toBe(false)
    })
  })
  describe('<=', () => {
    test('samples', () => {
      expect(lits.run('2 <= 3')).toBe(true)
      expect(lits.run('2 <= 2')).toBe(true)
      expect(lits.run('2 <= 1')).toBe(false)
    })
  })
  describe('>', () => {
    test('samples', () => {
      expect(lits.run('2 > 3')).toBe(false)
      expect(lits.run('2 > 2')).toBe(false)
      expect(lits.run('2 > 1')).toBe(true)
    })
  })
  describe('>=', () => {
    test('samples', () => {
      expect(lits.run('2 >= 3')).toBe(false)
      expect(lits.run('2 >= 2')).toBe(true)
      expect(lits.run('2 >= 1')).toBe(true)
    })
  })
  describe('=', () => {
    test('samples', () => {
      expect(lits.run('2 = 3')).toBe(false)
      expect(lits.run('2 = 2')).toBe(true)
      expect(lits.run('2 = 1')).toBe(false)
    })
  })
  describe('!=', () => {
    test('samples', () => {
      expect(lits.run('2 != 3')).toBe(true)
      expect(lits.run('2 != 2')).toBe(false)
      expect(lits.run('2 != 1')).toBe(true)
    })
  })
  describe('&', () => {
    test('samples', () => {
      expect(lits.run('0b1001 & 0b1000')).toBe(0b1000)
      expect(lits.run('0b1001 & 0b1000 & 0b0001')).toBe(0b0000)
    })
  })
  describe('|', () => {
    test('samples', () => {
      expect(lits.run('0b1001 | 0b0100')).toBe(0b1101)
      expect(lits.run('0b1001 | 0b0100 | 0b0010')).toBe(0b1111)
    })
  })
  describe('^', () => {
    test('samples', () => {
      expect(lits.run('0b1001 ^ 0b0100')).toBe(0b1101)
      expect(lits.run('0b1001 ^ 0b0100 ^ 0b0010')).toBe(0b1111)
    })
  })
  describe('&&', () => {
    test('samples', () => {
      expect(lits.run('true && true')).toBe(true)
      expect(lits.run('true && false')).toBe(false)
      expect(lits.run('false && true')).toBe(false)
      expect(lits.run('false && false')).toBe(false)
    })
  })
  describe('||', () => {
    test('samples', () => {
      expect(lits.run('true || true')).toBe(true)
      expect(lits.run('true || false')).toBe(true)
      expect(lits.run('false || true')).toBe(true)
      expect(lits.run('false || false')).toBe(false)
    })
  })
  describe('??', () => {
    test('samples', () => {
      expect(lits.run('1 ?? 2')).toBe(1)
      expect(lits.run('null ?? 2')).toBe(2)
    })
  })
  describe('!', () => {
    test('samples', () => {
      expect(lits.run('!(true)')).toBe(false)
      expect(lits.run('!(false)')).toBe(true)
      expect(lits.run('!(500)')).toBe(false)
      expect(lits.run('!(0)')).toBe(true)
      expect(lits.run('!(!(500))')).toBe(true)
      expect(lits.run('!(!(0))')).toBe(false)
    })
  })
  describe('parenthises', () => {
    test('samples', () => {
      expect(lits.run('-(2 + 3) * 2')).toBe(-10)
      expect(lits.run('2 + (3 * 2)')).toBe(8)
    })
  })
  describe('operator presedence', () => {
    test('samples', () => {
      expect(lits.run('1 / 2 + 1 / 2')).toBe(1)
    })
  })
  describe('objects', () => {
    test('samples', () => {
      expect(lits.run('{ a := 2 + 3 }')).toEqual({ a: 5 })
      expect(lits.run('{ a := 10 }')).toEqual({ a: 10 })
      expect(lits.run('{ " " := 10 }')).toEqual({ ' ': 10 })
      expect(lits.run('{ a := 10, b := 2 + 3 }')).toEqual({ a: 10, b: 5 })
      expect(lits.run('{ a := 10, b := 20, c := 2 * (1 - 2) }')).toEqual({ a: 10, b: 20, c: -2 })
    })
  })
  describe('arrays', () => {
    test('samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[2 + 3]')).toEqual([5])
      expect(lits.run('[10]')).toEqual([10])
      expect(lits.run('[10, 2 + 3]')).toEqual([10, 5])
      expect(lits.run('[10, 20, 2 * (1 - 2)]')).toEqual([10, 20, -2])
    })
  })
  describe('numbers', () => {
    test('samples', () => {
      expect(lits.run('5_000_000')).toBe(5000000)
      expect(lits.run('5e2')).toBe(500)
      expect(lits.run('-5.2e-1')).toBe(-0.52)
      expect(lits.run('5')).toBe(5)
      expect(lits.run('-10')).toBe(-10)
      expect(lits.tokenize('-10').tokens).toEqual([
        ['Number', '-10'],
      ])
    })
  })
  describe('strings', () => {
    test('samples', () => {
      expect(lits.run('""')).toBe('')
      expect(lits.run('"Foo"')).toBe('Foo')
      expect(lits.run('"Fo\\no"')).toBe('Fo\no')
    })
  })
  describe('propery accessor', () => {
    test('samples', () => {
      expect(lits.run('{ a := 200 }.a')).toBe(200)
      expect(lits.run('{ a := { b := 1, c := 2 } }.a.c')).toBe(2)
      expect(lits.run('[1, 2, 3][1]')).toBe(2)
    })
  })
  describe('propery accessor with brackets', () => {
    test('samples', () => {
      expect(lits.run('{ a := 200 }["a"]')).toBe(200)
      expect(lits.run('[1, [10, 20, 30], 3][1][1]')).toBe(20)
      expect(lits.run('{ foo := [1, 2, 3] }.foo[2 - 1]')).toBe(2)
      expect(lits.run('{ foo := [1, { bar := 20 }, 3] }.foo[1].bar')).toBe(20)
      expect(lits.run('[1, { bar := 20 }, 3][1].bar')).toBe(20)
    })
  })

  describe('function call', () => {
    test('samples', () => {
      expect(lits.run('max(1, 3, 2)')).toBe(3)
      expect(lits.run('&&(1, 2, 3)')).toBe(3)
      expect(lits.run('||(0, 1, 2)')).toBe(1)
      expect(lits.run('if 1 > 2 then 2 end')).toBe(null)
      expect(lits.run('if 1 < 2 then 2 end')).toBe(2)
      expect(lits.run('remove-at([1, 2, 3], 1)')).toEqual([1, 3])
    })
  })

  describe('if expression', () => {
    test('samples', () => {
      expect(lits.run('if 1 > 2 then 1 else 2 end')).toBe(2)
      expect(lits.run('if 1 > 2 then 1 end')).toBe(null)
      expect(lits.run('if 1 < 2 then 1 else 2 end')).toBe(1)
    })
  })

  describe('unless expression', () => {
    test('samples', () => {
      expect(lits.run('unless 1 < 2 then 1 else 2 end')).toBe(2)
      expect(lits.run('unless 1 < 2 then 1 end')).toBe(null)
      expect(lits.run('unless 1 > 2 then 1 else 2 end')).toBe(1)
    })
  })

  describe('try', () => {
    test('samples', () => {
      expect(lits.run('try 1 + 2 catch 0 end')).toBe(3)
      expect(lits.run('try 1 + "2" catch 0 end')).toBe(0)
      expect(lits.run(`
        try
          let x := "2";
          1 + x
        catch (error)
          error
        end`)).toBeInstanceOf(Error)
    })
  })

  test('misc', () => {
    expect(lits.run('3;2;1;')).toBe(1)
    expect(lits.run('empty?([1, 2 ,3] filter -> $ > 10)')).toBe(true)
    expect(lits.run('empty?([1, 2 ,3] filter -> $ > 1)')).toBe(false)
  })

  describe('debug', () => {
    test('samples', () => {
      expect(litsDebug.run('2 + 3')).toBe(5)
      expect(litsDebug.tokenize('2 + 3').tokens).toEqual([
        [
          'Number',
          '2',
          {
            code: '2 + 3',
            position: {
              column: 1,
              line: 1,
            },
          },
        ],
        [
          'Whitespace',
          ' ',
          {
            code: '2 + 3',
            position: {
              column: 2,
              line: 1,
            },
          },
        ],
        [
          'Operator',
          '+',
          {
            code: '2 + 3',
            position: {
              column: 3,
              line: 1,
            },
          },
        ],
        [
          'Whitespace',
          ' ',
          {
            code: '2 + 3',
            position: {
              column: 4,
              line: 1,
            },
          },
        ],
        [
          'Number',
          '3',
          {
            code: '2 + 3',
            position: {
              column: 5,
              line: 1,
            },
          },
        ],
      ])
      expect(litsDebug.run('-2')).toBe(-2)
      expect(litsDebug.tokenize('-2').tokens).toEqual([
        [
          'Number',
          '-2',
          {
            code: '-2',
            position: {
              column: 1,
              line: 1,
            },
          },
        ],
      ])
    })
  })

  describe('errors', () => {
    test('unknown operator', () => {
      expect(() => lits.run('2 # 3')).toThrow()
      expect(() => lits.run('(1 + 2]')).toThrow()
      expect(() => lits.run('abs 2')).toThrow()
      expect(() => lits.run('{ 2 := 1 }')).toThrow()
      expect(() => lits.run('{ x := 1 y := 2 }')).toThrow()
      expect(() => lits.run('[1 2]')).toThrow()
      expect(() => lits.run('if(1)')).toThrow() // To few parameters
      expect(() => lits.run(']')).toThrow()
    })
  })

  describe('basic arithmetic operations', () => {
    it('evaluates addition', () => {
      expect(lits.run('4 + 5')).toBe(9)
      expect(lits.run('0 + 0')).toBe(0)
      expect(lits.run('-3 + 3')).toBe(0)
    })

    it('evaluates subtraction', () => {
      expect(lits.run('10 - 3')).toBe(7)
      expect(lits.run('3 - 10')).toBe(-7)
      expect(lits.run('0 - 0')).toBe(0)
    })

    it('evaluates multiplication', () => {
      expect(lits.run('3 * 4')).toBe(12)
      expect(lits.run('0 * 5')).toBe(0)
      expect(lits.run('-2 * 3')).toBe(-6)
    })

    it('evaluates division', () => {
      expect(lits.run('12 / 3')).toBe(4)
      expect(lits.run('5 / 2')).toBe(2.5)
      expect(lits.run('0 / 5')).toBe(0)
    })

    it('evaluates remainder (modulo)', () => {
      expect(lits.run('10 % 3')).toBe(1)
      expect(lits.run('10 % 2')).toBe(0)
      expect(lits.run('10 % 10')).toBe(0)
    })

    it('evaluates exponentiation', () => {
      expect(lits.run('2 ** 3')).toBe(8)
      expect(lits.run('2 ** 0')).toBe(1)
      expect(lits.run('0 ** 0')).toBe(1)
    })
  })

  describe('operator precedence', () => {
    it('respects standard precedence rules', () => {
      expect(lits.run('2 + 3 * 4')).toBe(14)
      expect(lits.run('2 * 3 + 4')).toBe(10)
      expect(lits.run('2 ** 3 * 2')).toBe(16)
      expect(lits.run('2 * 3 ** 2')).toBe(18)
    })

    it('handles parentheses correctly', () => {
      expect(lits.run('(2 + 3) * 4')).toBe(20)
      expect(lits.run('2 * (3 + 4)')).toBe(14)
      expect(lits.run('(2 + 3) * (4 + 5)')).toBe(45)
    })

    it('handles nested parentheses', () => {
      expect(lits.run('(2 * (3 + (4 - 2)))')).toBe(10)
      expect(lits.run('((2 + 3) * 4) / 2')).toBe(10)
    })
  })

  describe('comparison operators', () => {
    it('evaluates equality operators', () => {
      expect(lits.run('3 = 3')).toBe(true)
      expect(lits.run('3 = 4')).toBe(false)
      expect(lits.run('3 != 4')).toBe(true)
      expect(lits.run('3 != 3')).toBe(false)
    })

    it('evaluates relational operators', () => {
      expect(lits.run('3 < 4')).toBe(true)
      expect(lits.run('4 < 3')).toBe(false)
      expect(lits.run('3 <= 3')).toBe(true)
      expect(lits.run('3 <= 2')).toBe(false)
      expect(lits.run('4 > 3')).toBe(true)
      expect(lits.run('3 > 4')).toBe(false)
      expect(lits.run('3 >= 3')).toBe(true)
      expect(lits.run('2 >= 3')).toBe(false)
    })
  })

  describe('logical operators', () => {
    it('evaluates logical AND', () => {
      expect(lits.run('true && true')).toBe(true)
      expect(lits.run('true && false')).toBe(false)
      expect(lits.run('false && true')).toBe(false)
      expect(lits.run('false && false')).toBe(false)
    })

    it('evaluates logical OR', () => {
      expect(lits.run('true || true')).toBe(true)
      expect(lits.run('true || false')).toBe(true)
      expect(lits.run('false || true')).toBe(true)
      expect(lits.run('false || false')).toBe(false)
    })

    it('evaluates nullish coalescing', () => {
      expect(lits.run('null ?? 5')).toBe(5)
      expect(lits.run('undefined ?? 5')).toBe(5)
      expect(lits.run('0 ?? 5')).toBe(0)
      expect(lits.run('false ?? 5')).toBe(false)
      expect(lits.run('"" ?? 5')).toBe('')
    })

    it('handles precedence between logical operators', () => {
      expect(lits.run('true && false || true')).toBe(true)
      expect(lits.run('true || false && true')).toBe(true)
      expect(lits.run('(true || false) && false')).toBe(false)
    })
  })

  describe('bitwise operators', () => {
    it('evaluates bitwise AND', () => {
      expect(lits.run('5 & 3')).toBe(1)
      expect(lits.run('12 & 4')).toBe(4)
    })

    it('evaluates bitwise OR', () => {
      expect(lits.run('5 | 3')).toBe(7)
      expect(lits.run('12 | 4')).toBe(12)
    })

    it('evaluates bitwise XOR', () => {
      expect(lits.run('5 ^ 3')).toBe(6)
      expect(lits.run('12 ^ 4')).toBe(8)
    })

    it('evaluates bitwise shifts', () => {
      expect(lits.run('8 << 2')).toBe(32)
      expect(lits.run('8 >> 2')).toBe(2)
      expect(lits.run('-8 >> 2')).toBe(-2)
      expect(lits.run('-8 >>> 2')).toEqual(expect.any(Number)) // Exact value depends on implementation
    })
  })

  describe('export', () => {
    test('samples', () => {
      // expect(lits.run('export let a := 10; a')).toBe(10)
      expect(lits.run(`
        export function foo()
          10
        end;
        foo()`)).toBe(10)
    })
    // expect(() => lits.run('export let a := 10; let a := 2;')).toThrow()
  })

  test('multinine comment', () => {
    expect(lits.run(`
/*******************************************
 *         Simple Lisp expression          *
 ******************************************/

10 + 20`)).toBe(30)
  })

  describe('do', () => {
    test('as operand', () => {
      expect(lits.run(`
        do
          let a := 1 + 2 * 3;
          a
        end + 3`)).toBe(10)
    })
    test('scope', () => {
      expect(lits.run(`
      let a := 1;
      do
        let a := 2;
      end;
      a`)).toBe(1)

      expect(() => lits.run(`
      do
        let a := 2;
      end;
      a`)).toThrow() // a is not defined
    })
  })

  describe('data structures', () => {
    it('supports array literals', () => {
      expect(lits.run('[1, 2, 3]')).toEqual([1, 2, 3])
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[1 + 1, 2 * 2, 3 ** 2]')).toEqual([2, 4, 9])
    })

    it('supports nested arrays', () => {
      expect(lits.run('[1, [2, 3], 4]')).toEqual([1, [2, 3], 4])
      expect(lits.run('[[1, 2], [3, 4]]')).toEqual([[1, 2], [3, 4]])
    })

    it('supports object literals', () => {
      expect(lits.run('{ a := 10, b := 20 }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{}')).toEqual({})
      expect(lits.run('{ x := 1 + 1, y := 2 * 3 }')).toEqual({ x: 2, y: 6 })
    })

    it('supports nested objects', () => {
      expect(lits.run('{ a := 10, b := { c := 20, d := 30 } }')).toEqual({ a: 10, b: { c: 20, d: 30 } })
      expect(lits.run('{ x := [1, 2], y := { z := [3, 4] } }')).toEqual({ x: [1, 2], y: { z: [3, 4] } })
    })

    it('supports property access', () => {
      expect(lits.run('{ a := 10, b := 20 }.a')).toBe(10)
      expect(lits.run('{ a := 10, b := { c := 20 } }.b.c')).toBe(20)
    })

    it('supports array access', () => {
      expect(lits.run('[1, 2, 3][0]')).toBe(1)
      expect(lits.run('[1, 2, 3][1 + 1]')).toBe(3)
      expect(lits.run('[[1, 2], [3, 4]][1][0]')).toBe(3)
    })
  })

  describe('function calls', () => {
    it('supports basic function calls', () => {
      // These tests assume your runtime provides these functions
      expect(lits.run('abs(-5)')).toBe(5)
      expect(lits.run('sin(0)')).toBeCloseTo(0)
      expect(lits.run('cos(0)')).toBeCloseTo(1)
    })

    it('supports function calls with multiple arguments', () => {
      expect(lits.run('max(1, 2, 3)')).toBe(3)
      expect(lits.run('min(1, 2, 3)')).toBe(1)
    })

    it('supports nested function calls', () => {
      expect(lits.run('abs(min(-5, -10))')).toBe(10)
      expect(lits.run('round(sin(3.14159))')).toBeCloseTo(0)
    })

    it('supports function calls with expressions as arguments', () => {
      expect(lits.run('abs(2 - 5)')).toBe(3)
      expect(lits.run('max(1 + 1, 2 + 2, 3 * 1)')).toBe(4)
    })
  })

  describe('let', () => {
    it('supports let bindings', () => {
      expect(lits.run('let a := 10; a')).toBe(10)
      expect(lits.run('let foo := -> $ + 1; foo(1)')).toBe(2)
    })
  })

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

  test('function', () => {
    expect(lits.run(`
function foo()
  42
end;

foo()`)).toBe(42)

    expect(lits.run(`
function foo(...x)
  '+' apply (x filter -> $ > 0)
end;

foo(-1, 0, 1, 2, 3)`)).toBe(6)
  })

  test('cond expression', () => {
    expect(lits.run(`
      let val := 8;

      cond
        case val < 5 then "S"
        case val < 10 then "M"
        case val < 15 then "L"
      end ?? "No match"`)).toBe('M')

    expect(lits.run(`
        let val := 20;

        cond
          case val < 5 then "S"
          case val < 10 then "M"
          case val < 15 then "L"
        end ?? "No match"`)).toBe('No match')
  })
  test('switch expression', () => {
    expect(lits.run(`
    switch "-"
      case "-" then 1
    end`)).toBe(1)
    expect(lits.run(`
      let x := 1;
      switch x
        case 0 then "zero"
        case 1 then "one"
        case 2 then "two"
      end`)).toBe('one')
    expect(lits.run(`
      let x := 10;
      switch x
        case 0 then "zero"
        case 1 then "one"
        case 2 then "two"
      end`)).toBe(null)
  })

  test('doseq', () => {
    expect(lits.run(`
      doseq
        each x in "Al"
        each y in [1, 2]
      do
        x repeat y
      end`)).toBe(null)
  })

  describe('for', () => {
    test('empty collections', () => {
      expect(lits.run(`
        for each x in [] do
          x
        end`)).toEqual([])
      expect(lits.run(`
        for
          each x in [1, 2, 3]
          each y in []
        do
          x
        end`)).toEqual([])
      expect(lits.run(`
        for
          each x in []
          each y in [1, 2, 3]
        do
          x
        end`)).toEqual([])
    })
    test('string and object iteration', () => {
      expect(lits.run(`
        for
          each x in "Al"
          each y in [1, 2]
        do
          x repeat y
        end`)).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run(`
         for
           each x in { a := 10, b := 20 }
           each y in [1, 2]
         do
           repeat(x, y)
         end`)).toEqual([
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
    })
    test('basic iteration with computation', () => {
      expect(lits.run(`
        for
          each x in [1, 2]
          each y in [1, 10]
        do
          x * y
        end`)).toEqual([1, 10, 2, 20])
    })
    test('with computed bindings using let', () => {
      expect(lits.run(`
        for
          each x in [1, 2], let z := x * x * x,
        do
          z
        end`)).toEqual([1, 8])
    })
    test('using previous bindings of subsequent iterations', () => {
      expect(lits.run(`
        for
          each x in [1, 2]
          each y in [x, 2 * x]
        do
          x * y
        end`)).toEqual([1, 2, 4, 8])
    })
    test('with when conditions', () => {
      expect(lits.run(`
        for
          each x in [0, 1, 2, 3, 4, 5], let a := x * 3, let y := a, when even?(y), while y < 10
        do
          y
        end`)).toEqual([0, 6])
    })
    test('with while conditions (early termination)', () => {
      expect(lits.run(`
        for
          each x in [0, 1, 2, 3, 4, 5], let y := x * 3, while even?(y)
        do
          y
        end`)).toEqual([0])
    })
    test('multiple iterations with while', () => {
      expect(lits.run(`
          for
            each x in [1, 2, 3]
            each y in [1, 2, 3], while x <= y
            each z in [1, 2, 3]
          do
            [x, y, z]
          end`)).toEqual([
        [1, 1, 1],
        [1, 1, 2],
        [1, 1, 3],
        [1, 2, 1],
        [1, 2, 2],
        [1, 2, 3],
        [1, 3, 1],
        [1, 3, 2],
        [1, 3, 3],
      ])
    })
    test('complex example with three iterations', () => {
      expect(lits.run(`
        for
          each x in [1, 2, 3]
          each y in [1, 2, 3]
          each z in [1, 2, 3], while x <= y
        do
          [x, y, z]
        end`)).toEqual([
        [1, 1, 1],
        [1, 1, 2],
        [1, 1, 3],
        [1, 2, 1],
        [1, 2, 2],
        [1, 2, 3],
        [1, 3, 1],
        [1, 3, 2],
        [1, 3, 3],
        [2, 2, 1],
        [2, 2, 2],
        [2, 2, 3],
        [2, 3, 1],
        [2, 3, 2],
        [2, 3, 3],
        [3, 3, 1],
        [3, 3, 2],
        [3, 3, 3],
      ])
    })
    test('real world example', () => {
      expect(lits.run(`// Imagine these are coming from a database
        let products := [
          { id := "P1", name := "Phone", price := 500, category := "electronics", stockLevel := 23 },
          { id := "P2", name := "Headphones", price := 150, category := "electronics", stockLevel := 42 },
          { id := "P3", name := "Case", price := 30, category := "accessories", stockLevel := 56 },
        ];
        let customerPreferences := {
          priceLimit := 700,
          preferredCategories := ["electronics", "accessories"],
          recentViews := ["P1", "P3", "P5"]
        };
        
        // Generate personalized bundle recommendations
        for
          // Start with main products
          each mainProduct in products,
            let isInStock := mainProduct.stockLevel > 0,
            let isPreferredCategory := contains?(customerPreferences.preferredCategories, mainProduct.category),
            let isPriceOk := mainProduct.price <= customerPreferences.priceLimit * 0.8,
            when (isInStock && isPreferredCategory && isPriceOk),
            
        
          // Add compatible accessories
          each accessory in products,
            let isCompatible := mainProduct.id != accessory.id && accessory.stockLevel > 0,
            let totalPrice := mainProduct.price + accessory.price,
            let isRecentlyViewed := contains?(customerPreferences.recentViews, accessory.id),
            when (isCompatible && totalPrice <= customerPreferences.priceLimit),
            while totalPrice <= customerPreferences.priceLimit * 0.9,
        
          // For high-value bundles, consider a third complementary item
          each complItem in products,
            let isValid := mainProduct.id != complItem.id && accessory.id != complItem.id && complItem.stockLevel > 0,
            let finalPrice := mainProduct.price + accessory.price + complItem.price,
            let discount := if finalPrice > 500 then 0.1 else 0.05 end,
            let discountedPrice := finalPrice * (1 - discount),
            let matchesPreferences := contains?(customerPreferences.preferredCategories, complItem.category),
            when (isValid && finalPrice <= customerPreferences.priceLimit && matchesPreferences),
            while discountedPrice <= customerPreferences.priceLimit,
        do
          // Return bundle information object
          {
            bundle := [mainProduct, accessory, complItem],
            originalPrice := finalPrice,
            discountedPrice := discountedPrice,
            savingsAmount := discount * finalPrice,
            savingsPercentage := discount * 100
          }
        end
        `)).toEqual([
        {
          bundle: [
            {
              category: 'accessories',
              id: 'P3',
              name: 'Case',
              price: 30,
              stockLevel: 56,
            },
            {
              category: 'electronics',
              id: 'P1',
              name: 'Phone',
              price: 500,
              stockLevel: 23,
            },
            {
              category: 'electronics',
              id: 'P2',
              name: 'Headphones',
              price: 150,
              stockLevel: 42,
            },
          ],
          discountedPrice: 612,
          originalPrice: 680,
          savingsAmount: 68,
          savingsPercentage: 10,
        },
        {
          bundle: [
            {
              category: 'accessories',
              id: 'P3',
              name: 'Case',
              price: 30,
              stockLevel: 56,
            },
            {
              category: 'electronics',
              id: 'P2',
              name: 'Headphones',
              price: 150,
              stockLevel: 42,
            },
            {
              category: 'electronics',
              id: 'P1',
              name: 'Phone',
              price: 500,
              stockLevel: 23,
            },
          ],
          discountedPrice: 612,
          originalPrice: 680,
          savingsAmount: 68,
          savingsPercentage: 10,
        },
      ])
    })
    test('error cases', () => {
      expect(() => lits.run('for each x in [0, 1, 2, 3, 4, 5], let y := x * 3, while even?(y)  y end')).toThrow()
      expect(() => lits.run('for each x in [0, 1, 2, 3, 4, 5], let { x := 10 } do y')).toThrow()
    })
  })

  describe('complex expressions', () => {
    it('handles complex arithmetic expressions', () => {
      expect(lits.run('(2 + 3) * 4 / 2 - 1')).toBe(9)
      expect(lits.run('2 ** 3 + 4 * 2 / (1 + 1)')).toBe(12)
    })

    it('handles complex logical expressions', () => {
      expect(lits.run('(5 > 3) && (10 < 20 || 5 = 5)')).toBe(true)
      expect(lits.run('!(5 < 3) && (3 <= 3 || 4 >= 5)')).toBe(true)
    })

    it('handles expressions combining different operators', () => {
      expect(lits.run('5 + 3 * 2 = 11')).toBe(true)
      expect(lits.run('(5 + 3) * 2 = 16')).toBe(true)
      expect(lits.run('[1, 2, 3][1 + 1] = 3')).toBe(true)
      expect(lits.run('{ a := 10, b := 20 }.a + { a := 5, b := 15 }.b = 25')).toBe(true)
    })

    it('handles complex nested expressions', () => {
      expect(lits.run('{ a := [1, 2, { b := 3 }] }.a[2].b')).toBe(3)
      expect(lits.run('[[1, 2], [3, 4]][1][abs(-1)]')).toBe(4)
    })

    test('regexp shorthands', () => {
      expect(lits.run('"abc" match #"a"')).toBeTruthy()
      expect(lits.run('"abc" match #"d"')).toBeNull()
    })

    it('handles super complex arithmetic expressions', () => {
      const expressions = [
        '((2 + 3) * 4 / 2 - 1) ** 2 % 5 + 6 - 7 * 8 / 9',
        '2 ** 3 * 4 + 5 - 6 / 3 % 2 + (7 - 8) * 9',
        '((10 / 2) + 3) * (4 - 1) ** 2 % 7',
        '2 ** (3 + 1) - 5 / (1 + 1)',
        '((2 + 3) * (4 - 1)) ** 2 % 7 + 6 - 7 * 8 / 9',
        '2 ** (3 * 2) + 4 / (2 - 1) - 5 % 3',
      ]

      for (const expression of expressions) {
        // eslint-disable-next-line ts/no-unsafe-argument, no-eval
        expect(lits.run(expression)).toBeCloseTo(eval(expression))
      }
    })
  })

  describe('error handling', () => {
    it('throws on invalid syntax', () => {
      expect(() => lits.run('4 + ')).toThrow()
      expect(() => lits.run('(4 + 5')).toThrow()
    })
  })

  describe('lambda functions', () => {
    it('supports basic lambda function definitions', () => {
      // Testing the provided lambda function example
      expect(lits.run('(() -> 1)()')).toBe(1)
      expect(lits.run('((x, y) -> x + y)(3, 4)')).toBe(7)
      expect(lits.run('((x, y) -> x + y)(10, -5)')).toBe(5)
    })

    it('supports single argument without parentheses', () => {
      expect(lits.run('(x -> x + 1)(1)')).toBe(2)
      expect(lits.run('((x) -> x + 1)(1)')).toBe(2)
    })

    it('supports lambda functions with let bindings', () => {
      // Support for let bindings
      expect(lits.run('(x -> (y, let x := x) -> x + y)(1)(2)')).toBe(3)
      expect(lits.run(`
(a -> 
  (b, 
    let a := a
  ) -> 
    (
      c, 
      let a := a,
      let b := b,
    ) -> 
      a * b * c
)(2)(3)(4)`)).toBe(24)
    })

    it('supports shorthand lambda function definitions', () => {
    // Testing the provided lambda function example
      expect(lits.run('(-> 1)()')).toBe(1)
      expect(lits.run('(-> $)(1)')).toBe(1)
      expect(lits.run('(-> $1 + $2)(3, 4)')).toBe(7)
    })

    it('supports lambda functions with no parameters', () => {
      expect(lits.run('(() -> 42)()')).toBe(42)
      expect(lits.run('(() -> 10 + 5)()')).toBe(15)
    })

    it('supports lambda functions with rest parameters', () => {
      expect(lits.run('((...args) -> apply(+, args))(1, 2, 3, 4, 5, 6)')).toBe(21)
      expect(lits.run('((first, ...args) -> first + apply(+, args))(1, 2, 3, 4, 5, 6)')).toBe(21)
    })

    it('supports lambda function expressions in data structures', () => {
      expect(lits.run('map([1, 2, 3], (x) -> x * 2)')).toEqual([2, 4, 6])
      expect(lits.run('{ fun := ((x) -> x + 1) }.fun(5)')).toBe(6)
    })

    it('supports complex expressions in lambda functions', () => {
      expect(lits.run('((x, y) -> x ** 2 + y ** 2)(3, 4)')).toBe(25)
      expect(lits.run('((a, b) -> ({ sum := a + b, product := a * b }))(3, 4).sum')).toBe(7)
      expect(lits.run('((a, b) -> ({ sum := a + b, product := a * b }))(3, 4).product')).toBe(12)
    })

    it('supports lambda functions as return values', () => {
      expect(lits.run('((op) -> if op = "add" then ((x, y) -> x + y) else ((x, y) -> x - y) end)("add")(5, 3)')).toBe(8)
      expect(lits.run('((op) -> if op = "add" then ((x, y) -> x + y) else ((x, y) -> x - y) end)("subtract")(5, 3)')).toBe(2)
    })
  })
})
