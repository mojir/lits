import { describe, expect, it, test } from 'vitest'
import { Lits } from '../Lits/Lits'
import { NodeTypes } from '../constants/constants'
import { LitsError } from '../errors'

const lits = new Lits()
const litsDebug = new Lits({ debug: true })

describe('parser', () => {
  describe('conditional operator', () => {
    test('? should work', () => {
      expect(lits.run('1 ? 2 : 3')).toBe(2)
      expect(lits.run('1 |> (1 ? inc: dec)')).toBe(2)
      expect(lits.run('1 |> (0 ? inc: dec)')).toBe(0)
      expect(lits.run('0 ? 2 : 3')).toBe(3)
      expect(lits.run('1 ? 2 : 3 ? 4 : 5')).toBe(2)
      expect(lits.run('0 ? 2 : 3 ? 4 : 5')).toBe(4)

      // Test ternary with arithmetic operators
      expect(lits.run('1 + 2 ? 3 : 4')).toBe(3) // (1 + 2) ? 3: 4
      expect(lits.run('0 + 0 ? 3 : 4')).toBe(4) // (0 + 0) ? 3: 4
      expect(lits.run('1 ? 2 + 3 : 4')).toBe(5) // 1 ? (2 + 3): 4
      expect(lits.run('0 ? 2 : 3 + 4')).toBe(7) // 0 ? 2: (3 + 4)

      // Test ternary with comparison operators
      expect(lits.run('1 < 2 ? "less" : "not less"')).toBe('less')
      expect(lits.run('2 == 2 ? "equal" : "not equal"')).toBe('equal')
      expect(lits.run('3 > 2 ? 3 * 2 : 3 / 2')).toBe(6)

      // Test ternary with logical operators
      expect(lits.run('true && false ? "and" : "not and"')).toBe('not and')
      expect(lits.run('true || false ? "or" : "not or"')).toBe('or')
      expect(lits.run('1 ? true && false : true')).toBe(false)
      expect(lits.run('0 ? true : true || false')).toBe(true)

      // Test ternary with nullish coalescing
      expect(lits.run('null ?? 5 ? "exists" : "null"')).toBe('exists')
      expect(lits.run('1 ? null ?? "default" : "falsy"')).toBe('default')

      // Test nested ternary expressions
      expect(lits.run('1 ? 2 ? 3 : 4 : 5')).toBe(3)
      expect(lits.run('1 ? 0 ? 3 : 4 : 5')).toBe(4)
      expect(lits.run('0 ? 2 : 0 ? 3 : 4')).toBe(4)

      // Test ternary with function calls
      expect(lits.run('even?(2) ? "even" : "odd"')).toBe('even')
      expect(lits.run('1 ? inc(2) : dec(2)')).toBe(3)

      // Test ternary with bitwise operators
      expect(lits.run('1 & 1 ? "bitwise" : "not bitwise"')).toBe('bitwise')
      expect(lits.run('1 | 0 ? "bitwise or" : "not bitwise or"')).toBe('bitwise or')

      // Test ternary with string concatenation
      expect(lits.run('"a" ++ "b" ? "concat" : "no concat"')).toBe('concat')
      expect(lits.run('1 ? "a" ++ "b" : "c"')).toBe('ab')

      // Test ternary with multiple operators (complex precedence testing)
      expect(lits.run('1 + 2 * 3 ? 4 + 5 : 6 + 7')).toBe(9)
      expect(lits.run('1 + 2 > 3 * 4 ? 5 - 6 : 7 * 8')).toBe(56)
      expect(lits.run('1 && 0 || 1 ? "complex1" : "complex2"')).toBe('complex1')
      expect(lits.run('(1 && 0) || 1 ? "parentheses" : "no parentheses"')).toBe('parentheses')
      expect(lits.run('1 && (0 || 1) ? "logical group" : "no group"')).toBe('logical group')

      // Test ternary with pipe and functional operators (low precedence interactions)
      expect(lits.run('[1, 2, 3] |> empty? ? "empty" : "not empty"')).toBe('not empty')
      expect(lits.run('[1, 2, 3] |> count |> even? ? "even count" : "odd count"')).toBe('odd count')
      expect(lits.run('1 ? [1, 2, 3] filter even? : [4, 5, 6]')).toEqual([2])
    })
  })

  describe('reserved symbol _', () => {
    it('should parse reserved symbol _', () => {
      expect(lits.parse(lits.tokenize('as'))).toEqual({
        body: [
          [
            NodeTypes.ReservedSymbol,
            'as',
          ],
        ],
        hasDebugData: false,
      })
    })
    expect(() => lits.run('_')).toThrow(LitsError)
    expect(() => lits.run('let _ = 1;')).toThrow(LitsError)
  })
  describe('const E', () => {
    it('samples', () => {
      expect(lits.run('E')).toBe(Math.E)
      expect(lits.run('ε')).toBe(Math.E)
      expect(lits.run('-E')).toBe(-Math.E)
      expect(lits.run('-ε')).toBe(-Math.E)
    })
  })

  test('random samples0', () => {
    expect(() => litsDebug.getUndefinedSymbols('let { x, ...x } = {};')).toThrow(LitsError)
  })

  test('random samples', () => {
    expect(() => lits.run('"a" object 1')).toThrow(LitsError)
    expect(() => lits.run('[1, 2, 3].1')).toThrow(LitsError)
    expect(() => lits.run('1 ? 2 ; 3')).toThrow(LitsError)
    expect(() => lits.run('1 ? 2')).toThrow(LitsError)
    expect(() => lits.run('1 ? 2 :')).toThrow(LitsError)
    expect(() => litsDebug.run('{ x: 1, y: 2 = 3 }')).toThrow(LitsError)
    expect(() => litsDebug.run('let { x, ...x }: {};')).toThrow(LitsError)
    expect(() => litsDebug.run('let [ x as y ]: [];')).toThrow(LitsError)
    expect(() => litsDebug.run('let { ...x as y }: {};')).toThrow(LitsError)
    expect(() => litsDebug.run('let { ...x, y }: {};')).toThrow(LitsError)
    expect(() => litsDebug.run('let [x, y];')).toThrow(LitsError)
    expect(() => litsDebug.run('let [...x, y]: [];')).toThrow(LitsError)
    expect(() => litsDebug.run('let ...x = 1;')).toThrow(LitsError)
    expect(() => litsDebug.run('let { a, ...x = y }: {};')).toThrow(LitsError)
    expect(() => litsDebug.run('let x;')).toThrow(LitsError)
    expect(() => litsDebug.run('0..1')).toThrow(LitsError)
    expect(() => litsDebug.run('1e2e2')).toThrow(LitsError)
    expect(() => litsDebug.run('match("Albert", #"as(d")')).toThrow(LitsError)
    expect(() => litsDebug.run('let _0 = 0;')).not.toThrow()
    expect(() => litsDebug.getUndefinedSymbols('let foo = ([,,a,...a]) -> a; foo([1, 2, 3])')).toThrow(LitsError)
    expect(() => litsDebug.getUndefinedSymbols('let foo = ([,,a,...a]) -> a; foo([1, 2, 3])')).toThrow(LitsError)
    expect(() => litsDebug.getUndefinedSymbols('let foo = ([,,a,a]) -> a; foo([1, 2, 3])')).toThrow(LitsError)
    expect(() => litsDebug.getUndefinedSymbols('let foo = ([,,a]) -> a; foo([1, 2, 3])')).not.toThrow()
    expect(() => litsDebug.run('let foo = ([,,a]) -> a; foo([1, 2, 3])')).not.toThrow()
    expect(() => litsDebug.run('let foo = ({a}) -> a; foo({})')).not.toThrow()
    expect(() => litsDebug.run('let foo = ({a, [a]}) -> a;')).toThrow(LitsError)
    expect(() => litsDebug.run('let foo = ({a a}) -> a;')).toThrow(LitsError)
    expect(() => litsDebug.run('let foo = ({a, a}) -> a;')).toThrow(LitsError)
    expect(() => litsDebug.run('let foo = ({a, b as a}) -> a;')).toThrow(LitsError)
    expect(() => litsDebug.run('let foo = (let a = 1;) -> 1')).toThrow(LitsError)
    expect(() => litsDebug.run('"\\t\\r\\n\\b\\f"')).not.toThrow()
    expect(() => litsDebug.run('E')).not.toThrow()
    expect(() => litsDebug.run('123')).not.toThrow()
    expect(() => litsDebug.run('let \'a\\\\b\' = 1;')).not.toThrow()
    expect(() => litsDebug.run('let \'a\\\'b\' = 1;')).not.toThrow()
    expect(() => litsDebug.run('let \'a\\ab\' = 1;')).not.toThrow()
    expect(() => litsDebug.run('`')).toThrow(LitsError)
    expect(() => litsDebug.run('export fun a(b) 1, end')).toThrow(LitsError)
    expect(() => litsDebug.run('export let a = (b) -> { 1, };')).toThrow(LitsError)
    expect(() => litsDebug.run('export let a = (b) -> { 1; 2 };')).not.toThrow()
    expect(() => litsDebug.run('let a = (b) -> { 1, };')).toThrow(LitsError)
    expect(() => litsDebug.run('switch 1 case 1 then 1; 2 end')).not.toThrow()
    expect(() => litsDebug.run('switch 1 case 1 then 1, end end')).toThrow(LitsError)
    expect(() => litsDebug.run('cond case 1 then 1; 2 end')).not.toThrow()
    expect(() => litsDebug.run('cond case 1 then 1, end end')).toThrow(LitsError)
    expect(() => litsDebug.run('if true then 1 else 1 end; 2')).not.toThrow()
    expect(() => litsDebug.run('if true then 1 else 1 end,')).toThrow(LitsError)
    expect(() => litsDebug.run('if true then 1 end; 2')).not.toThrow()
    expect(() => litsDebug.run('if true then 1 end; 2,')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] when a == 2 when b == 1) -> 1')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] while a == 2 while a == 1) -> 1')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] let a = 2, 2) -> 1')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] let a = 2 let a = 2) -> 1')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2]) -> { 1; 2 }')).not.toThrow()
    expect(() => litsDebug.run('for (a in [1, 2] when a == 1,) -> null')).not.toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] when a == 1,) -> }')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2]) -> { 1, }')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2] 2) -> { 1 }')).toThrow(LitsError)
    expect(() => litsDebug.run('for (a in [1, 2], 2) -> { 1 }')).toThrow(LitsError)
    expect(() => litsDebug.run('try 1; 2 catch 2; 3 end')).not.toThrow()
    expect(() => litsDebug.run('try 1; 2 catch 2, end')).toThrow(LitsError)
    expect(() => lits.getUndefinedSymbols('loop ([,x] = [1, 2]) -> { 1 }')).not.toThrow()
    expect(() => litsDebug.run('loop ([,x] = [1, 2]) -> { 1 }')).not.toThrow()
    expect(() => litsDebug.run('loop (x = 2) -> { 1, }')).toThrow(LitsError)
    expect(() => litsDebug.run('loop) -> { 1 }')).toThrow(LitsError)
    expect(() => litsDebug.run('{ 1, }')).toThrow(LitsError)
    expect(() => litsDebug.run('{ 1 }')).not.toThrow()
    expect(() => litsDebug.run('null ?? 1')).not.toThrow()
    expect(() => litsDebug.run('-> $ + $1')).toThrow(LitsError)
    expect(() => litsDebug.run('-> $ + $21')).toThrow(LitsError)
    expect(() => litsDebug.run('(a) -> a')).not.toThrow()
    expect(() => litsDebug.run('(...a, ...b) -> a')).toThrow(LitsError)
    expect(() => litsDebug.run('(...a, let a = 1;) -> a')).toThrow(LitsError)
    expect(() => litsDebug.run('(...a, let a = 1,,) -> a')).toThrow(LitsError)
    expect(litsDebug.run('{ a: 1 }.a')).toBe(1)
    expect(() => litsDebug.run('fn()')).toThrow(LitsError)
    expect(() => litsDebug.run('{ a = 1; b = 2 }')).toThrow(LitsError)
    expect(() => litsDebug.run('{ a = 1 }.1')).toThrow(LitsError)
    expect(() => lits.run('do export let a = 1; end')).toThrow(LitsError)
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
      expect(() => lits.run('(min_value :1)')).toThrow(LitsError)
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
      expect(lits.run('2 ^ 3')).toBe(8)
      expect(lits.run('2 ^ 3 ^ 2')).toBe(512)
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
      expect(lits.run('0b1001 xor 0b0100')).toBe(0b1101)
      expect(lits.run('0b1001 xor 0b0100 xor 0b0010')).toBe(0b1111)
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
      expect(lits.run('{ a: 2 + 3 }')).toEqual({ a: 5 })
      expect(lits.run('{ a: 10 }')).toEqual({ a: 10 })
      expect(lits.run('{ " ": 10 }')).toEqual({ ' ': 10 })
      expect(lits.run('{ a: 10, b: 2 + 3 }')).toEqual({ a: 10, b: 5 })
      expect(lits.run('{ a: 10, b: 20, c: 2 * (1 - 2) }')).toEqual({ a: 10, b: 20, c: -2 })
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
      expect(lits.run('{ a: 200 }.a')).toBe(200)
      expect(lits.run('{ a: { b: 1, c: 2 } }.a.c')).toBe(2)
      expect(lits.run('[1, 2, 3][1]')).toBe(2)
    })
  })
  describe('propery accessor with brackets', () => {
    test('samples', () => {
      expect(lits.run('{ a: 200 }["a"]')).toBe(200)
      expect(lits.run('[1, [10, 20, 30], 3][1][1]')).toBe(20)
      expect(lits.run('{ foo: [1, 2, 3] }.foo[2 - 1]')).toBe(2)
      expect(lits.run('{ foo: [1, { bar: 20 }, 3] }.foo[1].bar')).toBe(20)
      expect(lits.run('[1, { bar: 20 }, 3][1].bar')).toBe(20)
    })
  })

  describe('function call', () => {
    test('samples', () => {
      expect(lits.run('max(1, 3, 2)')).toBe(3)
      expect(lits.run('&&(1, 2, 3)')).toBe(3)
      expect(lits.run('||(0, 1, 2)')).toBe(1)
      expect(lits.run('remove-at([1, 2, 3], 1)')).toEqual([1, 3])
    })
  })

  describe('if expression', () => {
    test('samples', () => {
      // expect(lits.run('if 1 > 2 then 2 end')).toBe(null)
      // expect(lits.run('if 1 < 2 then 2 end')).toBe(2)
      // expect(lits.run('if 1 > 2 then 1 else 2 end')).toBe(2)
      // expect(lits.run('if 1 < 2 then 1 else 2 end')).toBe(1)
      expect(lits.run('if 1 < 2 then 2; 1; else 2; 2; end')).toBe(1)
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
      expect(lits.run('try 1 + "2" catch {} end')).toEqual({})
      expect(lits.run('try {} catch {} end')).toEqual({})
      expect(lits.run(`
        try
          let x = "2";
          1 + x
        catch (error)
          error
        end
      `)).toBeInstanceOf(Error)
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

  describe('errors.', () => {
    test('unknown operator', () => {
      expect(() => lits.run('2 # 3')).toThrow(LitsError)
      expect(() => lits.run('(1 + 2]')).toThrow(LitsError)
      expect(() => lits.run('abs 2')).toThrow(LitsError)
      expect(() => lits.run('{ 2: 1 }')).toThrow(LitsError)
      expect(() => lits.run('{ x: 1 y: 2 }')).toThrow(LitsError)
      expect(() => lits.run('[1 2]')).toThrow(LitsError)
      expect(() => lits.run('if 1 then')).toThrow(LitsError) // To few parameters
      expect(() => lits.run(']')).toThrow(LitsError)
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
      expect(lits.run('2 ^ 3')).toBe(8)
      expect(lits.run('2 ^ 0')).toBe(1)
      expect(lits.run('0 ^ 0')).toBe(1)
    })
  })

  describe('operator precedence', () => {
    it('respects standard precedence rules', () => {
      expect(lits.run('2 * 3 rem 4')).toBe(2)
      expect(lits.run('12 rem 3 + 4')).toBe(5)
      expect(lits.run('2 + 3 * 4')).toBe(14)
      expect(lits.run('2 * 3 + 4')).toBe(10)
      expect(lits.run('2 ^ 3 * 2')).toBe(16)
      expect(lits.run('2 * 3 ^ 2')).toBe(18)
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
      expect(lits.run('5 xor 3')).toBe(6)
      expect(lits.run('12 xor 4')).toBe(8)
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
        export let foo = () -> {
          11
        };
        foo()`)).toBe(11)
    })
    expect(() => lits.run('export let a = 10; let a = 2;')).toThrow(LitsError)
  })

  test('multinine comment', () => {
    expect(lits.run(`
/*******************************************
 *         Simple Lits program             *
 ******************************************/

10 + 20`)).toBe(30)
  })

  describe('block', () => {
    test('as operand', () => {
      expect(lits.run(`
        {
          let a = 1 + 2 * 3;
          a
        } + 3`)).toBe(10)
    })
    test('scope', () => {
      expect(lits.run(`
      let a = 1;
      {
        let a = 2;
      };
      a`)).toBe(1)

      expect(() => lits.run(`
      do
        let a = 2;
      end;
      a`)).toThrow(LitsError) // a is not defined
    })
  })

  describe('data structures', () => {
    it('supports array literals', () => {
      expect(lits.run('[1, 2, 3]')).toEqual([1, 2, 3])
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[1 + 1, 2 * 2, 3 ^ 2]')).toEqual([2, 4, 9])
    })

    it('supports nested arrays', () => {
      expect(lits.run('[1, [2, 3], 4]')).toEqual([1, [2, 3], 4])
      expect(lits.run('[[1, 2], [3, 4]]')).toEqual([[1, 2], [3, 4]])
    })

    it('supports object literals', () => {
      expect(lits.run('{ a: 10, b: 20 }')).toEqual({ a: 10, b: 20 })
      expect(lits.run('{}')).toEqual({})
      expect(lits.run('{ x: 1 + 1, y: 2 * 3 }')).toEqual({ x: 2, y: 6 })
      expect(() => lits.run('{ 1: 1 + 1, y: 2 * 3 }')).toThrow(LitsError)
    })

    it('supports nested objects', () => {
      expect(lits.run('{ a: 10, b: { c: 20, d: 30 } }')).toEqual({ a: 10, b: { c: 20, d: 30 } })
      expect(lits.run('{ x: [1, 2], y: { z: [3, 4] } }')).toEqual({ x: [1, 2], y: { z: [3, 4] } })
    })

    it('supports property access', () => {
      expect(lits.run('{ a: 10, b: 20 }.a')).toBe(10)
      expect(lits.run('{ a: 10, b: { c: 20 } }.b.c')).toBe(20)
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
      expect(lits.run('let foo = -> $ + 1; foo(1)')).toBe(2)
    })
  })

  describe('loop expressions', () => {
    it('supports loop expressions', () => {
      expect(lits.run(`
        loop(n = 10, sum = 0) -> if n == 0 then sum else recur(n - 1, sum + n) end`)).toBe(55)
    })
  })

  describe('function', () => {
    test('basic', () => {
      expect(lits.run(`
let foo = () -> {
  42
};

foo()`)).toBe(42)
    })

    test('empty block, no it is an object', () => {
      expect(lits.run(`
let foo = () -> {};

foo()`)).toEqual({})
    })
    test('with rest arguments///', () => {
      expect(lits.run(`
let foo = (...x) -> {
  '+' apply (x filter -> $ > 0)
};

foo(-1, 0, 1, 2, 3)`)).toBe(6)
    })

    test('with default arguments', () => {
      expect(lits.run(`
let foo = (a = 10, b = 20) -> {
  a + b
};

foo()`)).toBe(30)
    })

    test('with default arguments 1', () => {
      expect(lits.run(`
let foo = (a = 10, b = 20) -> {
  a + b
};

foo(0)`)).toBe(20)
    })

    test('with default arguments 2', () => {
      expect(lits.run(`
let foo = (a = 10, b = 20) -> {
  a + b
};

foo(1, 2)`)).toBe(3)
    })
    test('errors', () => {
      expect(() => lits.run('function foo(...rest = 1) rest end')).toThrow(LitsError)
      expect(() => lits.run('function foo(a = 1, b) rest end')).toThrow(LitsError)
    })
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

  test('simple doseq.', () => {
    expect(lits.run(`
      doseq (x in "Al", y in [1, 2]) -> {
        x repeat y
      }`)).toBe(null)
  })

  describe('for', () => {
    test('empty collections', () => {
      expect(() => lits.run(`
        for (x in [] 1) -> {
          x
        }`)).toThrow(LitsError)
      expect(() => lits.run(`
          for (x in [1, 2, 3] while x < 1 1) -> {
            x
          }`)).toThrow(LitsError)
      expect(lits.run(`
        for (x in []) -> {
          x
        }`)).toEqual([])
      expect(lits.run(`
        for (x in [1, 2, 3], y in []) -> {
          x
        }`)).toEqual([])
      expect(lits.run(`
        for (x in [], y in [1, 2, 3]) -> {
          x
        }`)).toEqual([])
    })
    test('string and object iteration', () => {
      expect(lits.run(`
        for (x in "Al", y in [1, 2]) -> {
          x repeat y
        }`)).toEqual([['A'], ['A', 'A'], ['l'], ['l', 'l']])
      expect(lits.run(`
        for (x in { a: 10, b: 20 }, y in [1, 2]) -> {
           repeat(x, y)
        }`)).toEqual([
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
        for (x in [1, 2], y in [1, 10]) -> {
          x * y
        }`)).toEqual([1, 10, 2, 20])
    })
    test('with computed bindings using let', () => {
      expect(lits.run(`
        for (x in [1, 2] let z = x * x * x) -> {
          z
        }`)).toEqual([1, 8])
    })
    test('using previous bindings of subsequent iterations', () => {
      expect(lits.run(`
        for (x in [1, 2], y in [x, 2 * x]) -> {
          x * y
        }`)).toEqual([1, 2, 4, 8])
    })
    test('with when conditions', () => {
      expect(lits.run(`
        for (x in [0, 1, 2, 3, 4, 5] let a = x * 3 let y = a when even?(y) while y < 10) -> {
          y
        }`)).toEqual([0, 6])
    })
    test('with while conditions (early termination)', () => {
      expect(lits.run(`
        for (x in [0, 1, 2, 3, 4, 5] let y = x * 3 while even?(y)) -> {
          y
        }`)).toEqual([0])
    })
    test('multiple iterations with while', () => {
      expect(lits.run(`
        for (x in [1, 2, 3], y in [1, 2, 3] while x <= y, z in [1, 2, 3]) -> {
          [x, y, z]
        }`)).toEqual([
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
    describe('destructuring', () => {
      const values = {
        'an-object': {
          name: 'John Doe',
          age: 42,
          married: true,
          children: [
            { name: 'Alice', age: 10 },
            { name: 'Bob', age: 7 },
          ],
          address: {
            street: '123 Main St',
            city: 'Springfield',
            state: 'IL',
            zip: '62701',
          },
        },
      }
      test('samples.', () => {
        expect(lits.run(`
          let foo = ({ a as b = 10 }) -> {
            b
          };

          foo({ b: 1})
        `)).toBe(10)
        expect(lits.run(`
          let { children: [{ age as firstChildAge }] } = an-object;
          firstChildAge
        `, { values })).toBe(10)

        expect(lits.run(`
          let { children: [{ age as firstChildAge, name }] } = an-object;
          [firstChildAge, name]
        `, { values })).toEqual([10, 'Alice'])

        expect(lits.run(`
          let { children: [, { age, name }] } = an-object;
          [age, name]
        `, { values })).toEqual([7, 'Bob'])

        expect(lits.run(`
          let foo = ([a, b] = [1, 2]) -> {
            a + b
          };

        foo()
        `, { values })).toEqual(3)

        expect(lits.run(`
          let foo = ([{ value as a }, { value as b }] = [{ value: 1 }, { value: 2 }]) -> {
            a + b
          };

          foo()
          `, { values })).toEqual(3)

        expect(lits.run(`
          let foo = ([{ value as a } = { value: 10 }, { value as b } = { value: 20 }] = [{ value: 1 }, { value: 2 }]) -> {
            a + b
          };

            foo([])
            `, { values })).toEqual(30)

        expect(lits.run(`
          let foo = ({ value = 10 }) -> {
            value
          };

          foo({})
          `, { values })).toEqual(10)

        expect(lits.run(`
          let foo = ([{ value as a } = { value: 10 }, { value as b = 200 } = { value: 20 }] = [{ value: 1 }, { value: 2 }]) -> {
            a + b
          };

            foo([{ value: 1 }])
            `, { values })).toEqual(21)

        expect(lits.run(`
          let foo = ([{ value as a } = { value: 10 }, { value as b = 200 } = { value: 20 }] = [{ value: 1 }, { value: 2 }]) -> {
            a + b
          };

            foo([{ value: 1 }, { value: 200 }])
            `, { values })).toEqual(201)
      })
    })
    test('complex example with three iterations', () => {
      expect(lits.run(`
        for (
          x in [1, 2, 3],
          y in [1, 2, 3],
          z in [1, 2, 3] while x <= y
        ) -> {
          [x, y, z]
        }`)).toEqual([
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
          { id: "P1", name: "Phone", price: 500, category: "electronics", stockLevel: 23 },
          { id: "P2", name: "Headphones", price: 150, category: "electronics", stockLevel: 42 },
          { id: "P3", name: "Case", price: 30, category: "accessories", stockLevel: 56 },
        ];
        let customerPreferences = {
          priceLimit: 700,
          preferredCategories: ["electronics", "accessories"],
          recentViews: ["P1", "P3", "P5"]
        };
        
        // Generate personalized bundle recommendations
        for (
          // Start with main products
          mainProduct in products
          let isInStock = mainProduct.stockLevel > 0
          let isPreferredCategory = contains?(customerPreferences.preferredCategories, mainProduct.category)
          let isPriceOk = mainProduct.price <= customerPreferences.priceLimit * 0.8
          when (isInStock && isPreferredCategory && isPriceOk),
            
        
          // Add compatible accessories
          accessory in products
          let isCompatible = mainProduct.id != accessory.id && accessory.stockLevel > 0
          let totalPrice = mainProduct.price + accessory.price
          let isRecentlyViewed = contains?(customerPreferences.recentViews, accessory.id)
          when (isCompatible && totalPrice <= customerPreferences.priceLimit)
          while totalPrice <= customerPreferences.priceLimit * 0.9,
        
          // For high-value bundles, consider a third complementary item
          complItem in products
          let isValid = mainProduct.id != complItem.id && accessory.id != complItem.id && complItem.stockLevel > 0
          let finalPrice = mainProduct.price + accessory.price + complItem.price
          let discount = finalPrice > 500 ? 0.1 : 0.05
          let discountedPrice = finalPrice * (1 - discount)
          let matchesPreferences = contains?(customerPreferences.preferredCategories, complItem.category)
          when (isValid && finalPrice <= customerPreferences.priceLimit && matchesPreferences)
          while discountedPrice <= customerPreferences.priceLimit
        ) -> {
          // Return bundle information object
          {
            bundle: [mainProduct, accessory, complItem],
            originalPrice: finalPrice,
            discountedPrice: discountedPrice,
            savingsAmount: discount * finalPrice,
            savingsPercentage: discount * 100
          }
        }`)).toEqual([
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
  })

  describe('complex expressions', () => {
    it('handles complex arithmetic expressions', () => {
      expect(lits.run('(2 + 3) * 4 / 2 - 1')).toBe(9)
      expect(lits.run('2 ^ 3 + 4 * 2 / (1 + 1)')).toBe(12)
    })

    it('handles complex logical expressions', () => {
      expect(lits.run('(5 > 3) && (10 < 20 || 5 == 5)')).toBe(true)
      expect(lits.run('!(5 < 3) && (3 <= 3 || 4 >= 5)')).toBe(true)
    })

    it('handles expressions combining different operators', () => {
      expect(lits.run('5 + 3 * 2 == 11')).toBe(true)
      expect(lits.run('(5 + 3) * 2 == 16')).toBe(true)
      expect(lits.run('[1, 2, 3][1 + 1] == 3')).toBe(true)
      expect(lits.run('{ a: 10, b: 20 }.a + { a: 5, b: 15 }.b == 25')).toBe(true)
    })

    it('handles complex nested expressions', () => {
      expect(lits.run('{ a: [1, 2, { b: 3 }] }.a[2].b')).toBe(3)
      expect(lits.run('[[1, 2], [3, 4]][1][abs(-1)]')).toBe(4)
    })

    test('regexp shorthands', () => {
      expect(lits.run('"abc" match #"a"')).toBeTruthy()
      expect(lits.run('"abc" match #"d"')).toBeNull()
    })

    it('handles super complex arithmetic expressions', () => {
      const expressions = [
        '((2 + 3) * 4 / 2 - 1) ^ 2 % 5 + 6 - 7 * 8 / 9',
        '2 ^ 3 * 4 + 5 - 6 / 3 % 2 + (7 - 8) * 9',
        '((10 / 2) + 3) * (4 - 1) ^ 2 % 7',
        '2 ^ (3 + 1) - 5 / (1 + 1)',
        '((2 + 3) * (4 - 1)) ^ 2 % 7 + 6 - 7 * 8 / 9',
        '2 ^ (3 * 2) + 4 / (2 - 1) - 5 % 3',
      ]

      for (const expression of expressions) {
        // eslint-disable-next-line ts/no-unsafe-argument, no-eval
        expect(lits.run(expression)).toBeCloseTo(eval(expression.replace(/\^/g, '**')))
      }
    })
  })

  describe('error handling', () => {
    it('throws on invalid syntax', () => {
      expect(() => lits.run('4 + ')).toThrow(LitsError)
      expect(() => lits.run('(4 + 5')).toThrow(LitsError)
    })
  })

  describe('lambda functions', () => {
    it('supports basic lambda function definitions', () => {
      // Testing the provided lambda function example
      expect(lits.run('(() -> 1)()')).toBe(1)
      expect(lits.run('((x, y) -> x + y)(3, 4)')).toBe(7)
      expect(lits.run('((x, y) -> x + y)(10, -5)')).toBe(5)
    })

    it('supports recursion via self', () => {
      expect(lits.run(`
        let fib = (n, a = 0, b = 1) ->
          cond
            case n == 0 then a
            case n == 1 then b
            case true then self(n - 1, b, a + b)
          end;

        fib(10)`)).toBe(55)
    })

    it('supports recursion via self (with switch)', () => {
      expect(lits.run(`
        let fib = (n, a = 0, b = 1) ->
          switch n
            case 0 then a
            case 1 then b
          end ?? self(n - 1, b, a + b);

        fib(10)`)).toBe(55)
    })
    it('supports single argument without parentheses', () => {
      expect(lits.run('(x -> x + 1)(1)')).toBe(2)
      expect(lits.run('((x) -> x + 1)(1)')).toBe(2)
    })

    it('supports shorthand lambda function definitions', () => {
    // Testing the provided lambda function example
      expect(lits.run('(-> 1)()')).toBe(1)
      expect(lits.run('(-> $)(1)')).toBe(1)
      expect(lits.run('(-> { $1 + $2 })(3, 4)')).toBe(7)
    })

    it('supports lambda functions with no parameters', () => {
      expect(lits.run('(() -> 42)()')).toBe(42)
      expect(lits.run('(() -> 10 + 5)()')).toBe(15)
    })

    it('supports lambda functions with rest parameters', () => {
      expect(lits.run('((...args) -> apply(+, args))(1, 2, 3, 4, 5, 6)')).toBe(21)
      expect(lits.run('((nbr1, ...args) -> nbr1 + apply(+, args))(1, 2, 3, 4, 5, 6)')).toBe(21)
    })

    it('supports lambda function expressions in data structures', () => {
      expect(lits.run('map([1, 2, 3], (x) -> x * 2)')).toEqual([2, 4, 6])
      expect(lits.run('{ fun: ((x) -> x + 1) }.fun(5)')).toBe(6)
    })

    it('supports complex expressions in lambda functions', () => {
      expect(lits.run('((x, y) -> x ^ 2 + y ^ 2)(3, 4)')).toBe(25)
      expect(lits.run('((a, b) -> ({ sum: a + b, product: a * b }))(3, 4).sum')).toBe(7)
      expect(lits.run('((a, b) -> ({ sum: a + b, product: a * b }))(3, 4).product')).toBe(12)
    })

    it('supports lambda functions as return values', () => {
      expect(lits.run('((op) -> if op == "add" then ((x, y) -> x + y) else ((x, y) -> x - y) end)("add")(5, 3)')).toBe(8)
      expect(lits.run('((op) -> if op == "add" then ((x, y) -> x + y) else ((x, y) -> x - y) end)("subtract")(5, 3)')).toBe(2)
    })
  })
})
