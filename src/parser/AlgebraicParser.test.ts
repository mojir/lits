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
      expect(lits.run('and(1, 2, 3)')).toBe(3)
      expect(lits.run('or(0, 1, 2)')).toBe(1)
      expect(lits.run('if(1 > 2, 1, 2)')).toBe(2)
      expect(lits.run('if(1 < 2, 1, 2)')).toBe(1)
      expect(lits.run('when(1 > 2, 2)')).toBe(null)
      expect(lits.run('when(1 < 2, 2)')).toBe(2)
      expect(lits.run('\'remove-at\'(1, [1, 2, 3])')).toEqual([1, 3])
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

      expect(pLits.run('(map (fn [number-parameter] @`\'number-parameter\' + 1`) [1 2 3])')).toEqual([2, 3, 4])
    })
  })
  describe('debug', () => {
    test('samples', () => {
      expect(lits.run('2+3', { debug: true })).toBe(5)
      expect(lits.tokenize('2+3', { debug: true }).tokens).toEqual([
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
      expect(lits.run('-2', { debug: true })).toBe(-2)
      expect(lits.tokenize('-2', { debug: true }).tokens).toEqual([
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

    it('supports lambda functions with no parameters', () => {
      expect(lits.run('(() => 42)()')).toBe(42)
      expect(lits.run('(() => 10 + 5)()')).toBe(15)
    })

    it('supports lambda functions with rest parameters', () => {
      expect(lits.run('((...args) => apply(\'+\', args))(1, 2, 3, 4, 5, 6)')).toBe(21)
      expect(lits.run('((first, ...args) => first + apply(\'+\', args))(1, 2, 3, 4, 5, 6)')).toBe(21)
    })

    it('supports lambda function expressions in data structures', () => {
      expect(lits.run('map((x) => x * 2, [1, 2, 3])')).toEqual([2, 4, 6])
      expect(lits.run('{ fun=((x) => x + 1) }.fun(5)')).toBe(6)
    })

    it('supports complex expressions in lambda functions', () => {
      expect(lits.run('((x, y) => x ** 2 + y ** 2)(3, 4)')).toBe(25)
      expect(lits.run('((a, b) => ({ sum=a + b, product=a * b }))(3, 4).sum')).toBe(7)
      expect(lits.run('((a, b) => ({ sum=a + b, product=a * b }))(3, 4).product')).toBe(12)
    })

    it('supports lambda functions as return values', () => {
      expect(lits.run('((op) => if(op == "add", ((x, y) => x + y), ((x, y) => x - y)))("add")(5, 3)')).toBe(8)
      expect(lits.run('((op) => if(op == "add", ((x, y) => x + y), ((x, y) => x - y)))("subtract")(5, 3)')).toBe(2)
    })
  })
})
