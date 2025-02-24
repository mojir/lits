import { describe, expect, test } from 'vitest'
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
})
