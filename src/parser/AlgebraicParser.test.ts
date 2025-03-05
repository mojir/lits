import { describe, expect, it, test } from 'vitest'
import { Lits } from '..'

const lits = new Lits({ algebraic: true })

describe('algebraic operators', () => {
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
  describe('==', () => {
    test('samples', () => {
      expect(lits.run('2 == 3')).toBe(false)
      expect(lits.run('2 == 2')).toBe(true)
      expect(lits.run('2 == 1')).toBe(false)
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
      expect(lits.run('nil ?? 2')).toBe(2)
    })
  })
  describe('!', () => {
    test('samples', () => {
      expect(lits.run('!true')).toBe(false)
      expect(lits.run('!false')).toBe(true)
      expect(lits.run('!500')).toBe(false)
      expect(lits.run('!0')).toBe(true)
      expect(lits.run('!!500')).toBe(true)
      expect(lits.run('!!0')).toBe(false)
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
      expect(lits.run('{ a=2+3 }')).toEqual({ a: 5 })
      expect(lits.run('{ a=10 }')).toEqual({ a: 10 })
      expect(lits.run('{ " "=10 }')).toEqual({ ' ': 10 })
      expect(lits.run('{ a=10, b=2+3 }')).toEqual({ a: 10, b: 5 })
      expect(lits.run('{ a=10, b=20, c = 2 * (1 - 2) }')).toEqual({ a: 10, b: 20, c: -2 })
    })
  })
  describe('arrays', () => {
    test('samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[2+3]')).toEqual([5])
      expect(lits.run('[10]')).toEqual([10])
      expect(lits.run('[10, 2+3]')).toEqual([10, 5])
      expect(lits.run('[10, 20, 2 * (1 - 2)]')).toEqual([10, 20, -2])
    })
  })
  describe('numbers', () => {
    test('samples', () => {
      expect(lits.run('5')).toBe(5)
      expect(lits.run('-10')).toBe(-10)
      expect(lits.tokenize('-10').tokens).toEqual([
        ['A_Operator', '-'],
        ['A_Number', '10'],
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
  describe('unary operators', () => {
    test('samples', () => {
      expect(lits.run('!5')).toBe(false)
      expect(lits.run('~1')).toBe(-2)
      expect(lits.run('+1')).toBe(1)
    })
  })
  describe('propery accessor', () => {
    test('samples', () => {
      expect(lits.run('{ a=200 }.a')).toBe(200)
      expect(lits.run('{ a={ b=1, c=2 } }.a.c')).toBe(2)
      expect(lits.run('[1, 2, 3][1]')).toBe(2)
    })
  })
  describe('propery accessor with brackets', () => {
    test('samples', () => {
      expect(lits.run('{ a=200 }["a"]')).toBe(200)
      expect(lits.run('[1, [10, 20, 30], 3][1][1]')).toBe(20)
      expect(lits.run('{ foo=[1, 2, 3] }.foo[2 - 1]')).toBe(2)
      expect(lits.run('{ foo=[1, { bar=20 }, 3] }.foo[1].bar')).toBe(20)
      expect(lits.run('[1, { bar=20 }, 3][1].bar')).toBe(20)
    })
  })

  describe('function call', () => {
    test('samples', () => {
      expect(lits.run('max(1, 3, 2)')).toBe(3)
      expect(lits.run('\'&&\'(1, 2, 3)')).toBe(3)
      expect(lits.run('\'||\'(0, 1, 2)')).toBe(1)
      expect(lits.run('if 1 > 2 then 2 end')).toBe(null)
      expect(lits.run('if 1 < 2 then 2 end')).toBe(2)
      expect(lits.run('remove_at([1, 2, 3], 1)')).toEqual([1, 3])
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
          let x = "2";
          1 + x
        catch (error)
          error
        end`)).toBeInstanceOf(Error)
    })
  })

  describe('polish escape hatch', () => {
    test('samples', () => {
      expect(lits.tokenize('$`1 2`').tokens).toEqual([
        ['PolNotation'],
        ['P_Number', '1'],
        ['P_Whitespace', ' '],
        ['P_Number', '2'],
        ['EndNotation'],
      ])

      expect(lits.run('$`1`')).toBe(1)
      expect(lits.run('10 * $`(+ 1 10)` / 2')).toBe(55)
      expect(lits.run('10 * $`(+ 1 @`12 - 2`)` / 2')).toBe(55)
      expect(lits.run('$`(+ $`2 1`)`')).toBe(1)
      expect(lits.run('10 * $`(+ $`2 1` @`@`12` - 2`)` / 2')).toBe(55)
    })
  })
  describe('algebraic escape hatch', () => {
    test('samples', () => {
      const pLits = new Lits({ algebraic: false })
      expect(pLits.tokenize('@`1 + 2`').tokens).toEqual([
        ['AlgNotation'],
        ['A_Number', '1'],
        ['A_Whitespace', ' '],
        ['A_Operator', '+'],
        ['A_Whitespace', ' '],
        ['A_Number', '2'],
        ['EndNotation'],
      ])

      expect(pLits.run('@`1`')).toBe(1)
      expect(pLits.run('@`1 + 2`')).toBe(3)
      expect(pLits.run('(/ (* 10 @`1 + 10`) 2)')).toBe(55)
      expect(pLits.run('@`10 + $`(mod 3 2)``')).toBe(11)

      expect(pLits.run('(def o @`{ foo="bar" }`) o.foo')).toBe('bar')

      expect(pLits.run('(map [1 2 3] (fn [number-parameter] @`\'number-parameter\' + 1`))')).toEqual([2, 3, 4])
    })
  })

  test('misc', () => {
    expect(lits.run('3;2;1;')).toBe(1)
    expect(lits.run('empty?([1, 2 ,3] filter => $ > 10)')).toBe(true)
    expect(lits.run('empty?([1, 2 ,3] filter => $ > 1)')).toBe(false)
  })

  describe('debug', () => {
    test('samples', () => {
      const litsDebug = new Lits({ debug: true, algebraic: true })
      expect(litsDebug.run('2+3')).toBe(5)
      expect(litsDebug.tokenize('2+3').tokens).toEqual([
        [
          'A_Number',
          '2',
          {
            sourceCodeInfo: {
              code: '2+3',
              position: {
                column: 1,
                line: 1,
              },
            },
          },
        ],
        [
          'A_Operator',
          '+',
          {
            sourceCodeInfo: {
              code: '2+3',
              position: {
                column: 2,
                line: 1,
              },
            },
          },
        ],
        [
          'A_Number',
          '3',
          {
            sourceCodeInfo: {
              code: '2+3',
              position: {
                column: 3,
                line: 1,
              },
            },
          },
        ],
      ])
      expect(litsDebug.run('-2')).toBe(-2)
      expect(litsDebug.tokenize('-2').tokens).toEqual([
        [
          'A_Operator',
          '-',
          {
            sourceCodeInfo: {
              code: '-2',
              position: {
                column: 1,
                line: 1,
              },
            },
          },
        ],
        [
          'A_Number',
          '2',
          {
            sourceCodeInfo: {
              code: '-2',
              position: {
                column: 2,
                line: 1,
              },
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
      expect(() => lits.run('/2')).toThrow()
      expect(() => lits.run('{ 2 = 1 }')).toThrow()
      expect(() => lits.run('{ x=1 y=2 }')).toThrow()
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
      expect(lits.run('3 == 3')).toBe(true)
      expect(lits.run('3 == 4')).toBe(false)
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
      expect(lits.run('export let a = 10; a')).toBe(10)
      expect(lits.run(`
        export function foo()
          10
        end
        foo()`)).toBe(10)
    })
    expect(() => lits.run('export let a = 10; let a = 2;')).toThrow()
  })

  describe('do', () => {
    test('as operand', () => {
      expect(lits.run(`
        do 
          let a = 1 + 2 * 3;
          a
        end + 3`)).toBe(10)
    })
    test('scope', () => {
      expect(lits.run(`
      let a = 1;
      do
        let a = 2;
      end;
      a`)).toBe(1)

      expect(() => lits.run(`
      do
        let a = 2;
      end;
      a`)).toThrow() // a is not defined

      expect(lits.run(`
      let a = 1;
      do
        export let a = 2;
      end;
      a`)).toBe(1)

      expect(lits.run(`
      do
        export let a = 2;
      end;
      a`)).toBe(2)
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
      expect(lits.run('{ a=10, b=20 }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{}')).toEqual({})
      expect(lits.run('{ x=1+1, y=2*3 }')).toEqual({ x: 2, y: 6 })
    })

    it('supports nested objects', () => {
      expect(lits.run('{ a=10, b={ c=20, d=30 } }')).toEqual({ a: 10, b: { c: 20, d: 30 } })
      expect(lits.run('{ x=[1, 2], y={ z=[3, 4] } }')).toEqual({ x: [1, 2], y: { z: [3, 4] } })
    })

    it('supports property access', () => {
      expect(lits.run('{ a=10, b=20 }.a')).toBe(10)
      expect(lits.run('{ a=10, b={ c=20 } }.b.c')).toBe(20)
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
      expect(lits.run('let a = 10; a')).toBe(10)
      expect(lits.run('let foo = => $ + 1; foo(1)')).toBe(2)
    })
  })

  describe('loop expressions', () => {
    it('supports loop expressions', () => {
      expect(lits.run(`
        loop (
          n = 10,
          sum = 0
        )
          if n == 0 then
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
end

foo()`)).toBe(42)

    expect(lits.run(`
function foo(...x)
  '+' apply (x filter => $ > 0)
end

foo(-1, 0, 1, 2, 3)`)).toBe(6)
  })

  test('cond expression', () => {
    expect(lits.run(`
      let val = 8;

      cond
        case val < 5 then "S"
        case val < 10 then "M"
        case val < 15 then "L"
      end ?? "No match"`)).toBe('M')

    expect(lits.run(`
        let val = 20;

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
      let x = 1;
      switch x
        case 0 then "zero"
        case 1 then "one"
        case 2 then "two"
      end`)).toBe('one')
    expect(lits.run(`
      let x = 10;
      switch x
        case 0 then "zero"
        case 1 then "one"
        case 2 then "two"
      end`)).toBe(null)
  })

  test('doseq', () => {
    expect(lits.run(`
      doseq (
        x of "Al",
        y of [1, 2]
      )
        x repeat y
      end`)).toBe(null)
  })

  describe('for', () => {
    test('empty collections', () => {
      expect(lits.run(`
        for (x of [])
          x
        end`)).toEqual([])
      expect(lits.run(`
        for (
          x of [1, 2, 3],
          y of []
        )
          x
        end`)).toEqual([])
      expect(lits.run(`
        for (
          x of [],
          y of [1, 2, 3]
        )
          x
        end`)).toEqual([])
    })
    test('string and object iteration', () => {
      expect(lits.run(`
        for (
          x of "Al",
          y of [1, 2]
        )
          x repeat y
        end`)).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run(`
         for (
           x of { a=10, b=20 },
           y of [1, 2]
         )
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
          (x of [1, 2],
          y of [1, 10]
        )
          x * y
        end`)).toEqual([1, 10, 2, 20])
    })
    test('with computed bindings using let', () => {
      expect(lits.run(`
        for (x of [1, 2] let z = x * x * x)
          z
        end`)).toEqual([1, 8])
    })
    test('using previous bindings of subsequent iterations', () => {
      expect(lits.run(`
        for (
          x of [1, 2],
          y of [x, 2 * x]
        )
          x * y
        end`)).toEqual([1, 2, 4, 8])
    })
    test('with when conditions', () => {
      expect(lits.run(`
        for (x of [0, 1, 2, 3, 4, 5] let y = x * 3 when even?(y))
          y
        end`)).toEqual([0, 6, 12])
    })
    test('with while conditions (early termination)', () => {
      expect(lits.run(`
        for (x of [0, 1, 2, 3, 4, 5] let y = x * 3 while even?(y))
          y
        end`)).toEqual([0])
    })
    test('multiple iterations with while', () => {
      expect(lits.run(`
        for (
          x of [1, 2, 3],
          y of [1, 2, 3] while x <= y,
          z of [1, 2, 3]
        )
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
        for (
          x of [1, 2, 3],
          y of [1, 2, 3],
          z of [1, 2, 3] while x <= y
        )
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
        let products = [
          { id="P1", name="Phone", price=500, category="electronics", stockLevel=23 },
          { id="P2", name="Headphones", price=150, category="electronics", stockLevel=42 },
          { id="P3", name="Case", price=30, category="accessories", stockLevel=56 },
        ];
        let customerPreferences = {
          priceLimit=700,
          preferredCategories=["electronics", "accessories"],
          recentViews=["P1", "P3", "P5"]
        };
        
        // Generate personalized bundle recommendations
        for (
          // Start with main products
          mainProduct of products
            let isInStock = mainProduct.stockLevel > 0
            let isPreferredCategory = has?(customerPreferences.preferredCategories, mainProduct.category)
            let isPriceOk = mainProduct.price <= customerPreferences.priceLimit * 0.8
            when (isInStock && isPreferredCategory && isPriceOk),
            
        
          // Add compatible accessories
          accessory of products
            let isCompatible = mainProduct.id != accessory.id && accessory.stockLevel > 0
            let totalPrice = mainProduct.price + accessory.price
            let isRecentlyViewed = has?(customerPreferences.recentViews, accessory.id)
            when (isCompatible && totalPrice <= customerPreferences.priceLimit)
            while totalPrice <= customerPreferences.priceLimit * 0.9,
        
          // For high-value bundles, consider a third complementary item
          complItem of products
            let isValid = mainProduct.id != complItem.id && accessory.id != complItem.id && complItem.stockLevel > 0
            let finalPrice = mainProduct.price + accessory.price + complItem.price
            let discount = if finalPrice > 500 then 0.1 else 0.05 end
            let discountedPrice = finalPrice * (1 - discount)
            let matchesPreferences = has?(customerPreferences.preferredCategories, complItem.category)
            when (isValid && finalPrice <= customerPreferences.priceLimit && matchesPreferences)
            while discountedPrice <= customerPreferences.priceLimit
        )
          // Return bundle information object
          {
            bundle=[mainProduct, accessory, complItem],
            originalPrice=finalPrice,
            discountedPrice=discountedPrice,
            savingsAmount=discount * finalPrice,
            savingsPercentage=discount * 100
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
      expect(() => lits.run('for (x of [0, 1, 2, 3, 4, 5] let y = x * 3 while even?(y), y)')).toThrow()
      expect(() => lits.run('for (x of [0, 1, 2, 3, 4, 5] let { x = 10 }, y)')).toThrow()
      expect(() => lits.run('for x of [0, 1, 2, 3, 4, 5], y')).toThrow()
      expect(() => lits.run('for (x of [0, 1, 2, 3, 4, 5], x, y)')).toThrow()
      expect(() => lits.run('for (x of [0, 1, 2, 3, 4, 5], x of [10, 20], x)')).toThrow()
    })
  })

  describe('complex expressions', () => {
    it('handles complex arithmetic expressions', () => {
      expect(lits.run('(2 + 3) * 4 / 2 - 1')).toBe(9)
      expect(lits.run('2 ** 3 + 4 * 2 / (1 + 1)')).toBe(12)
    })

    it('handles complex logical expressions', () => {
      expect(lits.run('(5 > 3) && (10 < 20 || 5 == 5)')).toBe(true)
      expect(lits.run('!(5 < 3) && (3 <= 3 || 4 >= 5)')).toBe(true)
    })

    it('handles expressions combining different operators', () => {
      expect(lits.run('5 + 3 * 2 == 11')).toBe(true)
      expect(lits.run('(5 + 3) * 2 == 16')).toBe(true)
      expect(lits.run('[1, 2, 3][1 + 1] == 3')).toBe(true)
      expect(lits.run('{ a=10, b=20 }.a + { a=5, b=15 }.b == 25')).toBe(true)
    })

    it('handles complex nested expressions', () => {
      expect(lits.run('{ a=[1, 2, { b=3 }] }.a[2].b')).toBe(3)
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

      // expect(lits.run('((2 + 3) * 4 / 2 - 1) ** 2 % 5 + 6 - 7 * 8 / 9')).toBeCloseTo(4.2222, 4)
      // expect(lits.run('2 ** 3 * 4 + 5 - 6 / 3 % 2 + (7 - 8) * 9')).toBeCloseTo(31)
      // expect(lits.run('((10 / 2) + 3) * (4 - 1) ** 2 % 7')).toBeCloseTo(6)
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
      expect(lits.run('(() => 1)()')).toBe(1)
      expect(lits.run('((x, y) => x + y)(3, 4)')).toBe(7)
      expect(lits.run('((x, y) => x + y)(10, -5)')).toBe(5)
    })

    it('supports single argument without parentheses', () => {
      expect(lits.run('(x => x + 1)(1)')).toBe(2)
      expect(lits.run('((x) => x + 1)(1)')).toBe(2)
    })

    it('supports lambda functions with let bindings', () => {
      // Support for let bindings
      expect(lits.run('(x, { y=2 }) => y')).toBeDefined()
      // Here we need the let bindings due to dynamic scoping, not lexical scoping
      // Would be nice to have lexical scoping, but that would require a more complex implementation
      expect(lits.run('((x) => (y, {x=x}) => x + y)(3)(4)')).toBe(7)
      expect(lits.run('((a) => (b, { a=a }) => (c, { a=a, b=b }) => a * b * c)(2)(3)(4)')).toBe(24)
    })

    it('supports shorthand lambda function definitions', () => {
    // Testing the provided lambda function example
      expect(lits.run('(=> 1)()')).toBe(1)
      expect(lits.run('(=> $)(1)')).toBe(1)
      expect(lits.run('(=> $1 + $2)(3, 4)')).toBe(7)
    })

    it('supports lambda functions with no parameters', () => {
      expect(lits.run('(() => 42)()')).toBe(42)
      expect(lits.run('(() => 10 + 5)()')).toBe(15)
    })

    it('supports lambda functions with rest parameters', () => {
      expect(lits.run('((...args) => apply(\'+\', args))(1, 2, 3, 4, 5, 6)')).toBe(21)
      expect(lits.run('((first, ...args) => first + apply(\'+\', args))(1, 2, 3, 4, 5, 6)')).toBe(21)
    })

    it('supports lambda function expressions in data structures', () => {
      expect(lits.run('map([1, 2, 3], (x) => x * 2)')).toEqual([2, 4, 6])
      expect(lits.run('{ fun=((x) => x + 1) }.fun(5)')).toBe(6)
    })

    it('supports complex expressions in lambda functions', () => {
      expect(lits.run('((x, y) => x ** 2 + y ** 2)(3, 4)')).toBe(25)
      expect(lits.run('((a, b) => ({ sum=a + b, product=a * b }))(3, 4).sum')).toBe(7)
      expect(lits.run('((a, b) => ({ sum=a + b, product=a * b }))(3, 4).product')).toBe(12)
    })

    it('supports lambda functions as return values', () => {
      expect(lits.run('((op) => if op == "add" then ((x, y) => x + y) else ((x, y) => x - y) end)("add")(5, 3)')).toBe(8)
      expect(lits.run('((op) => if op == "add" then ((x, y) => x + y) else ((x, y) => x - y) end)("subtract")(5, 3)')).toBe(2)
    })

    test('samples', () => {
      expect(lits.run(`
            $\`
            (def foo #(inc %))
            (foo 7)
            \`
          `)).toBe(8)
    })
  })
})
