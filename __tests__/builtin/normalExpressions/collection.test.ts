import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'
import { collectionUtilsModule } from '../../../src/builtin/modules/collectionUtils'

const lits = new Lits()

describe('collection functions', () => {
  describe('filter', () => {
    it('samples', () => {
      expect(lits.run('filter([1, "2", 3], number?)')).toEqual([1, 3])
      expect(lits.run('filter([], number?)')).toEqual([])
      expect(lits.run('filter([1, "2", 3], null?)')).toEqual([])
      expect(lits.run('filter([0, 1, 2, 3, 4, 5, 6, 7], -> zero?($ mod 3))')).toEqual([0, 3, 6])
      expect(lits.run('filter("aAbBcC", -> $ >= "a")')).toBe('abc')
      expect(lits.run('filter({ a: 1, b: 2 }, odd?)')).toEqual({ a: 1 })
      expect(() => lits.run('filter(+)')).toThrow(LitsError)
      expect(() => lits.run('filter()')).toThrow(LitsError)
      expect(() => lits.run('filter([1], number? 2)')).toThrow(LitsError)
    })
  })

  describe('map', () => {
    it('samples', () => {
      expect(lits.run('map([1, "2", 3], number?)')).toEqual([true, false, true])
      expect(lits.run('map([], number?)')).toEqual([])
      expect(lits.run('map([1, 2, 3], -> 2 * $)')).toEqual([2, 4, 6])
      expect(lits.run('map("ABCDE", "12345", ++)')).toBe('A1B2C3D4E5')
      expect(lits.run('map([1, 2, 3], [1, 2], +)')).toEqual([2, 4])
      expect(lits.run('map("AaBbCc", -> if $1 >= "a" then "-" else "+" end)')).toBe('+-+-+-')
      expect(() => lits.run('map("AaBbCc", -> if $1 >= "a" 0 else 1 end)')).toThrow(LitsError)
      expect(lits.run('map([1, "2", 3], null?)')).toEqual([false, false, false])
      expect(lits.run('map([0, 1, 2, 3, 4, 5, 6, 7], -> zero?($ mod 3))')).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lits.run('map([0, 1, 2, 3, 4, 5, 6, 7], inc)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(lits.run('map({ a: 1, b: 2 }, inc)')).toEqual({ a: 2, b: 3 })
      expect(lits.run('map({ a: 1, b: 2 }, { a: 10, b: 20 }, +)')).toEqual({ a: 11, b: 22 })
      expect(() => lits.run('map({ a: 1, b: 2 }, { c: 10, b: 20 }, +)')).toThrow(LitsError)
      expect(() => lits.run('map({ a: 1, b: 2 }, { b: 20 }, +)')).toThrow(LitsError)
      expect(() => lits.run('map(+)')).toThrow(LitsError)
      expect(() => lits.run('map()')).toThrow(LitsError)
      expect(() => lits.run('map(1 number?)')).toThrow(LitsError)
    })
  })

  describe('reduce', () => {
    it('samples', () => {
      let program = `
      let countChars = (stringArray) -> do
        reduce(
          stringArray,
          (sum, s) -> sum + count(s),
          0
        )
      end;

      countChars(["First", "Second", "Third"])
      `
      expect(lits.run(program)).toBe(16)

      program = `
      let longestLength = (stringArray) -> do
        reduce(
          stringArray,
          (sum, s) ->
            if sum > count(s) then
              sum
            else
              count(s)
            end,
            0
          )
      end;

      longestLength(["First", "Second", "Third"])
      // `
      expect(lits.run(program)).toBe(6)

      expect(lits.run('reduce([1, 2, 3, 4, 5], +, 0)')).toBe(15)
      expect(lits.run('reduce([], +, 0)')).toBe(0)
      expect(lits.run('reduce([1], +, 0)')).toBe(1)
      expect(lits.run('reduce([1, 2], +, 0)')).toBe(3)
      expect(lits.run('reduce([], +, 1)')).toBe(1)
      expect(lits.run('reduce([2, 3], +, 1)')).toBe(6)
      expect(lits.run('reduce([1, 2, 3], +, 0)')).toBe(6)
      expect(lits.run('reduce([], +, 0)')).toBe(0)
      expect(lits.run('reduce([], +, 1)')).toBe(1)

      expect(lits.run('reduce("Albert", (x, y) -> ++(x, "-", y), "")')).toBe('-A-l-b-e-r-t')
      expect(lits.run('reduce("Albert", (x, y) -> ++(x, "-", y), ">")')).toBe('>-A-l-b-e-r-t')
      expect(lits.run('reduce("", (x, y) -> ++(x, "-", y), ">")')).toBe('>')

      expect(lits.run('reduce({ a: 1, b: 2 }, +, 0)')).toBe(3)
      expect(lits.run('reduce({}, +, 0)')).toBe(0)

      expect(() => lits.run('reduce([1, 2, 3], +)')).toThrow(LitsError)
      expect(() => lits.run('reduce(+)')).toThrow(LitsError)
      expect(() => lits.run('reduce()')).toThrow(LitsError)
      expect(() => lits.run('reduce(1, +2)')).toThrow(LitsError)
    })
  })

  describe('count', () => {
    it('samples', () => {
      expect(lits.run('count([])')).toBe(0)
      expect(lits.run('count([1])')).toBe(1)
      expect(lits.run('count([1, 2, 3])')).toBe(3)
      expect(lits.run('count({})')).toBe(0)
      expect(lits.run('count({ a: 1, b: 2, })')).toBe(2)
      expect(lits.run('count("")')).toBe(0)
      expect(lits.run('count("Albert")')).toBe(6)
      expect(lits.run('count(null)')).toBe(0)

      expect(() => lits.run('count()')).toThrow(LitsError)
      expect(() => lits.run('count([], [])')).toThrow(LitsError)
      expect(() => lits.run('count(12)')).toThrow(LitsError)
      expect(() => lits.run('count(false)')).toThrow(LitsError)
      expect(() => lits.run('count(true)')).toThrow(LitsError)
    })
  })

  describe('get', () => {
    it('samples', () => {
      expect(lits.run('[1, 2, 3] get 1')).toBe(2)
      expect(lits.run('"Albert" get 7')).toBeNull()

      expect(lits.run('get([], 1)')).toBeNull()
      expect(lits.run('get([1], 1)')).toBeNull()
      expect(lits.run('get([1, 2, 3], 1)')).toBe(2)
      expect(lits.run('get([], 1, "x")')).toBe('x')
      expect(lits.run('get([1], 1, "x")')).toBe('x')
      expect(lits.run('get([1, 2, 3], 1, "x")')).toBe(2)
      expect(lits.run('get([1, 2, 3], -1)')).toBeNull()
      expect(lits.run('get([1, 2, 3], -1, "x")')).toBe('x')

      expect(lits.run('get("Albert", 1)')).toBe('l')
      expect(lits.run('get("Albert", 7)')).toBeNull()
      expect(lits.run('get("Albert", -1)')).toBeNull()
      expect(lits.run('get("Albert", -1, "x")')).toBe('x')
      expect(lits.run('get("", 0)')).toBeNull()

      expect(lits.run('get({}, "a")')).toBeNull()
      expect(lits.run('get({ a: 1, b: 2, }, "a")')).toBe(1)
      expect(lits.run('get({}, "a", "x")')).toBe('x')
      expect(lits.run('get({ a: 1, b: 2, }, "a")')).toBe(1)

      expect(lits.run('get(null, 1)')).toBeNull()
      expect(lits.run('get(null, 1, 99)')).toBe(99)

      expect(() => lits.run('get()')).toThrow(LitsError)
      expect(() => lits.run('get([])')).toThrow(LitsError)
      expect(() => lits.run('get(12)')).toThrow(LitsError)
      expect(() => lits.run('get(12, 1)')).toThrow(LitsError)
      expect(() => lits.run('get(false)')).toThrow(LitsError)
      expect(() => lits.run('get(false, 2)')).toThrow(LitsError)
      expect(() => lits.run('get(true)')).toThrow(LitsError)
      expect(() => lits.run('get(null)')).toThrow(LitsError)
    })
  })

  describe('contains?', () => {
    it('samples', () => {
      expect(lits.run('[1, 2, 3] contains? 3')).toBe(true)
      expect(lits.run('[1, 2, [3]] contains? [3]')).toBe(true)
      expect(lits.run('"Albert" contains? "bert"')).toBe(true)

      expect(lits.run('contains?([], 1)')).toBe(false)
      expect(lits.run('contains?([1], 1)')).toBe(true)
      expect(lits.run('contains?([1, 2, 3], 1)')).toBe(true)
      expect(lits.run('contains?({}, "a")')).toBe(false)
      expect(lits.run('contains?(object("a", 1, "b", 2), "a")')).toBe(true)
      expect(lits.run('contains?([], "1")')).toBe(false)
      expect(lits.run('contains?([1], "1")')).toBe(false)
      expect(lits.run('contains?([1, 2, 3], "1")')).toBe(false)
      expect(lits.run('contains?({}, "1")')).toBe(false)
      expect(lits.run('contains?(object("a", 1, "b", "2"), "2")')).toBe(false)
      expect(lits.run('contains?(object("a", 1, "b", "2"), "a")')).toBe(true)
      expect(lits.run('contains?("Albert", "A")')).toBe(true)
      expect(lits.run('contains?("Albert", "lb")')).toBe(true)
      expect(lits.run('contains?("Albert", "al")')).toBe(false)
      expect(lits.run('contains?("Albert", "xxx")')).toBe(false)

      expect(lits.run('contains?(null, 1)')).toBe(false)
      expect(lits.run('contains?(null, "foo")')).toBe(false)

      expect(() => lits.run('contains?("")')).toThrow(LitsError)
      expect(() => lits.run('contains?([])')).toThrow(LitsError)
      expect(() => lits.run('contains?("123")')).toThrow(LitsError)
      expect(() => lits.run('contains?()')).toThrow(LitsError)
      expect(() => lits.run('contains?(12)')).toThrow(LitsError)
      expect(() => lits.run('contains?(false)')).toThrow(LitsError)
      expect(() => lits.run('contains?(true)')).toThrow(LitsError)
      expect(() => lits.run('contains?(null)')).toThrow(LitsError)
    })
  })

  describe('assoc', () => {
    it('samples', () => {
      expect(lits.run('assoc([1, 2, 3], 0, "1")')).toEqual(['1', 2, 3])
      expect(lits.run('assoc([1, 2, 3], 1, "2")')).toEqual([1, '2', 3])
      expect(lits.run('let a = [1, 2, 3]; assoc(a, 1, "2")')).toEqual([1, '2', 3])
      expect(lits.run('let a = [1, 2, 3]; assoc(a, 1, "2"); a')).toEqual([1, 2, 3])
      expect(lits.run('assoc([1, 2, 3], 3, "4")')).toEqual([1, 2, 3, '4'])

      expect(lits.run('assoc({}, "a", "1")')).toEqual({ a: '1' })

      expect(lits.run('assoc({a: 1, b: 2}, "a", "1")')).toEqual({ a: '1', b: 2 })
      expect(lits.run('assoc({a: 1, b: 2}, "b", "2")')).toEqual({ a: 1, b: '2' })
      expect(lits.run('let o = {a: 1, b: 2}; assoc(o, "a", "1")')).toEqual({ a: '1', b: 2 })
      expect(lits.run('let o = {a: 1, b: 2}; assoc(o, "a", "1"); o')).toEqual({ a: 1, b: 2 })

      expect(lits.run('assoc("1", 0, "2")')).toBe('2')
      expect(lits.run('assoc("Albert", 6, "!")')).toBe('Albert!')

      expect(() => lits.run('assoc("Albert", 7, "!")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1, 2, 3], 4, "4")')).toThrow(LitsError)
      expect(() => lits.run('assoc({}, 0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc(null, 0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc(true, 0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc(false, 0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc(1, 0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc("1", 0, "22")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1], "0", "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1], true, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1], false, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1], [], "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1], null, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc(0, "2")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1, 2, 3], -1, "x")')).toThrow(LitsError)
      expect(() => lits.run('assoc([1, 2, 3], 4, "x")')).toThrow(LitsError)
      expect(() => lits.run('assoc()')).toThrow(LitsError)
      expect(() => lits.run('assoc([])')).toThrow(LitsError)
      expect(() => lits.run('assoc([], 0)')).toThrow(LitsError)
      expect(() => lits.run('assoc([], 0, "x", "y")')).toThrow(LitsError)
      expect(() => lits.run('assoc([],, "a", "1")')).toThrow(LitsError)
    })
  })

  describe('++', () => {
    it('samples', () => {
      expect(lits.run('"Albert" ++ "Mojir"')).toBe('AlbertMojir')
      expect(lits.run('"Albert" ++ " Mojir"')).toBe('Albert Mojir')
      expect(lits.run('++("Albert", "Mojir", " ")')).toBe('AlbertMojir ')
      expect(lits.run('++("Albert", " ", "Mojir", 1)')).toBe('Albert Mojir1')
      expect(lits.run('++("Albert", "Mojir", " and ")')).toBe('AlbertMojir and ')
      expect(lits.run('++("Albert")')).toBe('Albert')

      expect(lits.run('[1, 2] ++ [3, 4]')).toEqual([1, 2, 3, 4])
      expect(lits.run('{ a: 1, b: 2 } ++ { b: 20, c: 30 }')).toEqual({ a: 1, b: 20, c: 30 })
      expect(lits.run('"Al" ++ "bert"')).toEqual('Albert')

      expect(lits.run('++([])')).toEqual([])
      expect(lits.run('++([1])')).toEqual([1])
      expect(lits.run('++([1], [2], [3, 4])')).toEqual([1, 2, 3, 4])
      expect(lits.run('++([1, 2, 3], [])')).toEqual([1, 2, 3])

      expect(lits.run('++({a: 1, b: 2}, {b: 1, c: 2})')).toEqual({ a: 1, b: 1, c: 2 })
      expect(lits.run('++({}, {a: 1, b: 2})')).toEqual({ a: 1, b: 2 })

      expect(lits.run('++("1", "23")')).toBe('123')
      expect(lits.run('++("1", "")')).toBe('1')
      expect(lits.run('++("1")')).toBe('1')
      expect(lits.run('++(0)')).toBe('0')

      expect(() => lits.run('++()')).toThrow(LitsError)
      expect(() => lits.run('++([1], "2")')).toThrow(LitsError)
      expect(() => lits.run('++("1", ["2"])')).toThrow(LitsError)
      expect(() => lits.run('++(true)')).toThrow(LitsError)
      expect(() => lits.run('++("1", false)')).toThrow(LitsError)
      expect(() => lits.run('++(null, "m")')).toThrow(LitsError)
    })
  })
})

describe('collection-Utils module functions', () => {
  const imp = 'let cu = import("Collection"); '
  for (const mlits of [new Lits({ modules: [collectionUtilsModule] }), new Lits({ modules: [collectionUtilsModule], debug: true })]) {
    describe('filteri', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.filteri([1, "2", 3], -> odd?($2))`)).toEqual(['2'])
        expect(mlits.run(`${imp}cu.filteri([], -> odd?($2))`)).toEqual([])
        expect(mlits.run(`${imp}cu.filteri("Albert", -> odd?($2))`)).toEqual('let')
        expect(mlits.run(`${imp}cu.filteri({ a: 1, b: 2 }, -> $2 == "a")`)).toEqual({ a: 1 })
        expect(() => mlits.run(`${imp}cu.filteri(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.filteri()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.filteri([1], number? 2)`)).toThrow(LitsError)
      })
    })

    describe('mapi', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.mapi([1, "2", 3], -> $2)`)).toEqual([0, 1, 2])
        expect(mlits.run(`${imp}cu.mapi([], number?)`)).toEqual([])
        expect(mlits.run(`${imp}cu.mapi([1, 2, 3], -> $1 + $2)`)).toEqual([1, 3, 5])
        expect(mlits.run(`${imp}cu.mapi("ABCDE", -> $2 ++ $1)`)).toBe('0A1B2C3D4E')
        expect(mlits.run(`${imp}cu.mapi({ a: 1, b: 2 }, -> $2 ++ $1)`)).toEqual({ a: 'a1', b: 'b2' })
        expect(() => mlits.run(`${imp}cu.mapi({ a: 1, b: 2 }, { b: 20 }, +)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.mapi(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.mapi()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.mapi(1 number?)`)).toThrow(LitsError)
      })
    })

    describe('reducei', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.reducei([1, 2, 3, 4, 5], -> $1 + $3, 0)`)).toBe(10)
        expect(mlits.run(`${imp}cu.reducei([], -> $1 + $3, 0)`)).toBe(0)
        expect(mlits.run(`${imp}cu.reducei("Albert", (acc, char, index) -> acc ++ index ++ char, "")`)).toBe('0A1l2b3e4r5t')
        expect(mlits.run(`${imp}cu.reducei("", (acc, char, index) -> acc ++ index ++ char, "")`)).toBe('')
        expect(mlits.run(`${imp}cu.reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")`)).toBe('ab')
        expect(mlits.run(`${imp}cu.reducei({}, -> $1 ++ $3, "")`)).toBe('')

        expect(() => mlits.run(`${imp}cu.reducei([1, 2, 3], +)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei(1, +2)`)).toThrow(LitsError)
      })
    })

    describe('reduce-right', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.reduce-right([1, 2, 3, 4, 5], +, 0)`)).toBe(15)
        expect(mlits.run(`${imp}cu.reduce-right([], +, 0)`)).toBe(0)
        expect(mlits.run(`${imp}cu.reduce-right([1], +, 0)`)).toBe(1)
        expect(mlits.run(`${imp}cu.reduce-right([1, 2], +, 0)`)).toBe(3)
        expect(mlits.run(`${imp}cu.reduce-right([1, 2, 3], +, 0)`)).toBe(6)
        expect(mlits.run(`${imp}cu.reduce-right([], +, 0)`)).toBe(0)
        expect(mlits.run(`${imp}cu.reduce-right([], +, 0)`)).toBe(0)
        expect(mlits.run(`${imp}cu.reduce-right(["1", "2", "3"], str, "")`)).toBe('321')

        expect(mlits.run(`${imp}cu.reduce-right("Albert", (x, y) -> ++(x, "-", y), "")`)).toBe('-t-r-e-b-l-A')
        expect(mlits.run(`${imp}cu.reduce-right("Albert", (x, y) -> ++(x, "-", y), ">")`)).toBe('>-t-r-e-b-l-A')
        expect(mlits.run(`${imp}cu.reduce-right("", (x, y) -> ++(x, "-", y), ">")`)).toBe('>')

        expect(mlits.run(`${imp}cu.reduce-right({ a: 1, b: 2 }, +, 0)`)).toBe(3)
        expect(mlits.run(`${imp}cu.reduce-right({}, +, 0)`)).toBe(0)

        expect(() => mlits.run(`${imp}cu.reduce-right(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reduce-right()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reduce-right(1, +, 2)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reduce-right([1, 2], +)`)).toThrow(LitsError)
      })
    })

    describe('reducei-right', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.reducei-right([1, 2, 3, 4, 5], -> $1 + $3, 0)`)).toBe(10)
        expect(mlits.run(`${imp}cu.reducei-right([], -> $1 + $3, 0)`)).toBe(0)
        expect(mlits.run(`${imp}cu.reducei-right("Albert", (acc, char, index) -> acc ++ index ++ char, "")`)).toBe('5t4r3e2b1l0A')
        expect(mlits.run(`${imp}cu.reducei-right("", (acc, char, index) -> acc ++ index ++ char, "")`)).toBe('')
        expect(mlits.run(`${imp}cu.reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")`)).toBe('ba')
        expect(mlits.run(`${imp}cu.reducei-right({}, -> $1 ++ $3, "")`)).toBe('')

        expect(() => mlits.run(`${imp}cu.reducei-right([1, 2, 3], +)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei-right(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei-right()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reducei-right(1, +2)`)).toThrow(LitsError)
      })
    })

    describe('reductions', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.reductions([1, 2, 3, 4, 5], +, 0)`)).toEqual([0, 1, 3, 6, 10, 15])
        expect(mlits.run(`${imp}cu.reductions([], +, 0)`)).toEqual([0])
        expect(mlits.run(`${imp}cu.reductions([1], +, 0)`)).toEqual([0, 1])
        expect(mlits.run(`${imp}cu.reductions([1, 2], +, 0)`)).toEqual([0, 1, 3])
        expect(mlits.run(`${imp}cu.reductions([], +, 1)`)).toEqual([1])
        expect(mlits.run(`${imp}cu.reductions([2, 3], +, 1)`)).toEqual([1, 3, 6])
        expect(mlits.run(`${imp}cu.reductions([1, 2, 3], +, 0)`)).toEqual([0, 1, 3, 6])
        expect(mlits.run(`${imp}cu.reductions([], +, 0)`)).toEqual([0])
        expect(mlits.run(`${imp}cu.reductions([], +, 1)`)).toEqual([1])

        expect(mlits.run(`${imp}cu.reductions("Albert", (x, y) -> ++(x, "-", y), "")`)).toEqual([
          '',
          '-A',
          '-A-l',
          '-A-l-b',
          '-A-l-b-e',
          '-A-l-b-e-r',
          '-A-l-b-e-r-t',
        ])
        expect(mlits.run(`${imp}cu.reductions("Albert", (x, y) -> ++(x, "-", y), ">")`)).toEqual([
          '>',
          '>-A',
          '>-A-l',
          '>-A-l-b',
          '>-A-l-b-e',
          '>-A-l-b-e-r',
          '>-A-l-b-e-r-t',
        ])
        expect(mlits.run(`${imp}cu.reductions("", (x, y) -> ++(x, "-", y), ">")`)).toEqual(['>'])

        expect(mlits.run(`${imp}cu.reductions({ a: 1, b: 2 }, +, 0)`)).toEqual([0, 1, 3])
        expect(mlits.run(`${imp}cu.reductions({}, +, 0)`)).toEqual([0])

        expect(() => mlits.run(`${imp}cu.reductions(null +)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reductions(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reductions()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.reductions(1, +, 2)`)).toThrow(LitsError)
      })
    })

    describe('reductionsi', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.reductionsi([1, 2, 3, 4, 5], -> $1 + $3, 0)`)).toEqual([0, 0, 1, 3, 6, 10])
        expect(mlits.run(`${imp}cu.reductionsi([], -> $1 + $3, 0)`)).toEqual([0])
        expect(mlits.run(`${imp}cu.reductionsi("Albert", (x, v, i) -> x ++ i ++ v, "")`)).toEqual([
          '',
          '0A',
          '0A1l',
          '0A1l2b',
          '0A1l2b3e',
          '0A1l2b3e4r',
          '0A1l2b3e4r5t',
        ])
        expect(mlits.run(`${imp}cu.reductionsi("", (x, v, i) -> x ++ i ++ v, "")`)).toEqual([''])
        expect(mlits.run(`${imp}cu.reductionsi({ a: 1, b: 2 }, -> $ ++ $3, "")`)).toEqual(['', 'a', 'ab'])
        expect(mlits.run(`${imp}cu.reductionsi({}, -> $ ++ $3, "")`)).toEqual([''])
      })
    })

    describe('get-in', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 0])`)).toBe('Albert')
        expect(mlits.run(`${imp}cu.get-in([1, 2, 3], [1])`)).toBe(2)

        expect(mlits.run(`${imp}cu.get-in([], [1])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in([1], [1])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in([1, 2, 3], [1])`)).toBe(2)
        expect(mlits.run(`${imp}cu.get-in([[1, 2, 3], [4, {a: 2}, 6]], [1, 1, "a"])`)).toBe(2)
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 0])`)).toBe('Albert')
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 0, 5])`)).toBe('t')
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 0, 5, 0, 0, 0, 0, 0, 0])`)).toBe('t')
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 2], "DEFAULT")`)).toBe('DEFAULT')
        expect(mlits.run(`${imp}cu.get-in({a: ["Albert", "Mojir"]}, ["a", 2, "x"], "DEFAULT")`)).toBe('DEFAULT')

        expect(mlits.run(`${imp}cu.get-in(null, [], "DEFAULT")`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in(null, [1], "DEFAULT")`)).toBe('DEFAULT')
        expect(mlits.run(`${imp}cu.get-in([], [], "DEFAULT")`)).toEqual([])
        expect(mlits.run(`${imp}cu.get-in([1, 2], [1], "DEFAULT")`)).toBe(2)
        expect(mlits.run(`${imp}cu.get-in([1, 2], [1, 2], "DEFAULT")`)).toBe('DEFAULT')
        expect(mlits.run(`${imp}cu.get-in([], [1], "DEFAULT")`)).toBe('DEFAULT')
        expect(mlits.run(`${imp}cu.get-in(2, [1], "DEFAULT")`)).toBe('DEFAULT')
        expect(mlits.run(`${imp}cu.get-in(2, [], "DEFAULT")`)).toBe(2)

        expect(mlits.run(`${imp}cu.get-in(null, [])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in(null, [1])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in([], [])`)).toEqual([])
        expect(mlits.run(`${imp}cu.get-in([1, 2], [1])`)).toBe(2)
        expect(mlits.run(`${imp}cu.get-in([1, 2], [1, 2])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in([], [1])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in(2, [1])`)).toBeNull()
        expect(mlits.run(`${imp}cu.get-in(2, [])`)).toBe(2)

        expect(mlits.run(`${imp}cu.get-in("Albert", [])`)).toBe('Albert')
        expect(mlits.run(`${imp}cu.get-in("Albert", [0])`)).toBe('A')
        expect(mlits.run(`${imp}cu.get-in("Albert", ["0"])`)).toBeNull()

        expect(mlits.run(`${imp}cu.get-in("Albert", null, "DEFAULT")`)).toBe('Albert')

        expect(() => mlits.run(`${imp}cu.get-in()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.get-in([])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.get-in(12)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.get-in(false)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.get-in(true)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.get-in(null)`)).toThrow(LitsError)
      })
    })

    describe('assoc-in', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.assoc-in("Albert", [0], "a")`)).toEqual('albert')
        expect(mlits.run(`${imp}cu.assoc-in("Albert", [6], "!")`)).toEqual('Albert!')
        expect(() => mlits.run(`${imp}cu.assoc-in("Albert", [7], "!")`)).toThrow(LitsError)
        expect(mlits.run(`${imp}cu.assoc-in({}, ["a", "b", "c"], "Albert")`)).toEqual({ a: { b: { c: 'Albert' } } })
        expect(mlits.run(`${imp}cu.assoc-in([1, 2, 3], [0], "1")`)).toEqual(['1', 2, 3])
        expect(mlits.run(`${imp}cu.assoc-in([1, 2, [1, 2, 3]], [2, 1], "2")`)).toEqual([1, 2, [1, '2', 3]])
        expect(mlits.run(`${imp}cu.assoc-in([1, 2, "albert"], [2, 0], "A")`)).toEqual([1, 2, 'Albert'])
        expect(mlits.run(`${imp}cu.assoc-in([1, 2, {name: "albert"}], [2, "name"], "A")`)).toEqual([1, 2, { name: 'A' }])
        expect(mlits.run(`${imp}cu.assoc-in([1, 2, {name: "albert"}], [2, "name", 0], "A")`)).toEqual([1, 2, { name: 'Albert' }])
        expect(() => mlits.run(`${imp}cu.assoc-in([1, 2, {name: "albert"}], ["2", "name", 0], "A")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.assoc-in([1, 2, {name: "albert"}], [2, 1, 0], "A")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.assoc-in([1, 2, {name: "albert"}], [2, "name", "a"], "A")`)).toThrow(LitsError)
      })
    })

    describe('not-empty', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.not-empty([])`)).toBeNull()
        expect(mlits.run(`${imp}cu.not-empty([0])`)).toEqual([0])
        expect(mlits.run(`${imp}cu.not-empty({})`)).toBeNull()
        expect(mlits.run(`${imp}cu.not-empty({a: 2})`)).toEqual({ a: 2 })
        expect(mlits.run(`${imp}cu.not-empty("")`)).toBeNull()
        expect(mlits.run(`${imp}cu.not-empty("Albert")`)).toEqual('Albert')

        expect(mlits.run(`${imp}cu.not-empty(null)`)).toBeNull()

        expect(() => mlits.run(`${imp}cu.not-empty()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-empty(true)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-empty(false)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-empty(10)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-empty((regexp "^start"))`)).toThrow(LitsError)
      })
    })

    describe('every?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.every?([1, 2, 3], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?("abc", x -> x >= "a")`)).toBe(true)

        expect(mlits.run(`${imp}cu.every?([1, 2, 3], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?(["1", "2", "3"], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.every?([], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?("", number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?({}, number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?([2, 4, 6], (x -> zero?(x mod 2)))`)).toBe(true)

        expect(mlits.run(`${imp}cu.every?("abc", x -> x >= "a")`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?("abC", x -> x >= "a")`)).toBe(false)
        expect(mlits.run(`${imp}cu.every?({a: 2, b: 4}, -> even?(second($)))`)).toBe(true)
        expect(mlits.run(`${imp}cu.every?({a: 2, b: 3}, -> even?(second($)))`)).toBe(false)
        expect(mlits.run(`${imp}cu.every?({a: 2, b: 3}, -> even?(second($)))`)).toBe(false)
        expect(() => mlits.run(`${imp}cu.every?(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.every?([])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.every?()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.every?([1], number?, 2)`)).toThrow(LitsError)
      })
    })

    describe('not-every?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.not-every?(["1", "2", "3"], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-every?(["1", 2, "3"], number?)`)).toBe(true)

        expect(mlits.run(`${imp}cu.not-every?([1, 2, 3], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?(["1", "2", "3"], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-every?([], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?("", number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?({}, number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?([2, 4, 6], x -> zero?(x mod 2))`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?("abc", x -> x >= "a")`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?("abC", x -> x >= "a")`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-every?({a: 2, b: 4}, -> even?(second($)))`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-every?({a: 2, b: 3}, -> even?(second($)))`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-every?({a: 2, b: 3}, -> even?(second($)))`)).toBe(true)
        expect(() => mlits.run(`${imp}cu.not-every?(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-every?([])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-every?()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-every?([1], number?, 2)`)).toThrow(LitsError)
      })
    })

    describe('any?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.any?([1, "2", 3], number?)`)).toBe(true)

        expect(mlits.run(`${imp}cu.any?([1, 2, 3], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?([1, "2", 3], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?(["1", "2", "3"], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?([], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?("", number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?({}, number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?([1, 3, 6], x -> zero?(x mod 2))`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?([1, 3, 5], x -> zero?(x mod 2))`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?("abc", x -> x >= "a")`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?("abC", x -> x >= "a")`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?("ABC", x -> x >= "a")`)).toBe(false)
        expect(mlits.run(`${imp}cu.any?({a: 2, b: 4}, -> even?(second($)))`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?({a: 2, b: 3}, -> even?(second($)))`)).toBe(true)
        expect(mlits.run(`${imp}cu.any?({a: 1, b: 3}, -> even?(second($)))`)).toBe(false)
        expect(() => mlits.run(`${imp}cu.any?(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.any?([])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.any?()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.any?([1], number?, 2)`)).toThrow(LitsError)
      })
    })

    describe('not-any?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}cu.not-any?(["1", "2", "3"], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?(["1", "2", 3], number?)`)).toBe(false)

        expect(mlits.run(`${imp}cu.not-any?([1, 2, 3], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?([1, "2", 3], number?)`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?(["1", "2", "3"], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?([], number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?("", number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?({}, number?)`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?([1, 3, 6], x -> zero?(x mod 2))`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?([1, 3, 5], x -> zero?(x mod 2))`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?("abc", x -> x >= "a")`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?("abC", x -> x >= "a")`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?("ABC", x -> x >= "a")`)).toBe(true)
        expect(mlits.run(`${imp}cu.not-any?({a: 2, b: 4}, -> even?(second($)))`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?({a: 2, b: 3}, -> even?(second($)))`)).toBe(false)
        expect(mlits.run(`${imp}cu.not-any?({a: 1, b: 3}, -> even?(second($)))`)).toBe(true)
        expect(() => mlits.run(`${imp}cu.not-any?(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-any?([])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-any?()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}cu.not-any?([1], number?, 2)`)).toThrow(LitsError)
      })
    })

    describe('update', () => {
      it('samples', () => {
        expect(
          mlits.run(
            `${imp}let x = "Albert"; cu.update(x, 3, val -> if null?(val) then "!" else upper-case(val) end)`,
          ),
        ).toEqual('AlbErt')
        expect(
          mlits.run(
            `${imp}let x = "Albert"; cu.update(x, 6, val -> if null?(val) then "!" else upper-case(val) end)`,
          ),
        ).toEqual('Albert!')

        expect(mlits.run(`${imp}let x = [0, 1, 2, 3]; cu.update(x, 3, inc)`)).toEqual([0, 1, 2, 4])
        expect(mlits.run(`${imp}let x = [0, 1, 2, 3]; cu.update(x, 4, identity)`)).toEqual([0, 1, 2, 3, null])

        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update(x, "a", inc)`)).toEqual({ a: 2, b: 2 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update(x, "a", +, 10)`)).toEqual({ a: 11, b: 2 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update(x, "a", val -> if even?(val) then 0 else inc(val) end)`)).toEqual({
          a: 2,
          b: 2,
        })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; "c"(x)`)).toEqual(null)
        expect(mlits.run(`${imp}cu.update({}, "a", val -> if null?(val) then 0 end)`)).toEqual({ a: 0 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update(x, "c", val -> if null?(val) then 0 else inc(val) end)`)).toEqual({
          a: 1,
          b: 2,
          c: 0,
        })
        expect(() => mlits.run(`${imp}cu.update(number?, [1], 2)`)).toThrow(LitsError)
      })
    })

    describe('update-in', () => {
      it('samples', () => {
        expect(
          mlits.run(
            `${imp}let x = "Albert"; cu.update-in(x, [3], val -> if null?(val) then "!" else upper-case(val) end)`,
          ),
        ).toEqual('AlbErt')
        expect(
          mlits.run(
            `${imp}let x = "Albert"; cu.update-in(x, [6], val -> if null?(val) then "!" else upper-case(val) end)`,
          ),
        ).toEqual('Albert!')

        expect(mlits.run(`${imp}let x = [0, 1, 2, 3]; cu.update-in(x, [3], inc)`)).toEqual([0, 1, 2, 4])
        expect(mlits.run(`${imp}let x = [0, 1, 2, 3]; cu.update-in(x, [4], identity)`)).toEqual([0, 1, 2, 3, null])

        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update-in(x, ["a"], inc)`)).toEqual({ a: 2, b: 2 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update-in(x, ["a"], +, 10)`)).toEqual({ a: 11, b: 2 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update-in(x, ["a"], val -> if even?(val) then 0 else inc(val) end)`)).toEqual({
          a: 2,
          b: 2,
        })
        expect(mlits.run(`${imp}cu.update-in({}, ["a"], val -> if null?(val) then 0 end)`)).toEqual({ a: 0 })
        expect(mlits.run(`${imp}let x = {a: 1, b: 2}; cu.update-in(x, ["c"], val -> if null?(val) then 0 else inc(val) end)`)).toEqual({
          a: 1,
          b: 2,
          c: 0,
        })
        expect(mlits.run(`${imp}cu.update-in({a: [1, 2, 3]}, ["a", 1], val -> if null?(val) then 0 end)`)).toEqual({
          a: [1, null, 3],
        })
        expect(mlits.run(`${imp}cu.update-in({a: [1, null, 3]}, ["a", 1], val -> if null?(val) then 0 end)`)).toEqual({
          a: [1, 0, 3],
        })
        expect(mlits.run(`${imp}cu.update-in({a: [1, "Albert", 3]}, ["a", 1, 0], val -> if null?(val) then "?" else "!" end)`)).toEqual({
          a: [1, '!lbert', 3],
        })
        expect(mlits.run(`${imp}cu.update-in({a: [1, "", 3]}, ["a", 1, 0], val -> if null?(val) then "?" else "!" end)`)).toEqual({
          a: [1, '?', 3],
        })
      })
    })
  }
})
