import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'
import { sequenceUtilsModule } from '../../../src/builtin/modules/sequence'

const lits = new Lits()
const litsInstances = [lits, new Lits({ debug: true })]
describe('sequence functions', () => {
  describe('nth', () => {
    it('array samples', () => {
      for (const lits2 of litsInstances) {
        expect(lits2.run('nth([1, 2, 3], 1)')).toBe(2)
        expect(lits2.run('nth([], 0)')).toBeNull()
        expect(lits2.run('nth([1, 2, 3], 3)')).toBeNull()
        expect(lits2.run('nth([1, 2, 3], -1)')).toBeNull()
        expect(lits2.run('nth([1, 2, 3], -4)')).toBeNull()
        expect(() => lits2.run('nth()')).toThrow(LitsError)
        expect(() => lits2.run('nth({}, 1)')).toThrow(LitsError)
        expect(() => lits2.run('nth([1, 2, 3])')).toThrow(LitsError)
        expect(() => lits2.run('nth([1, 2, 3], 1, 2, 3)')).toThrow(LitsError)
      }
    })

    it('string samples', () => {
      expect(lits.run('nth("A string", 1)')).toBe(' ')
      expect(lits.run('nth("A string", 3)')).toBe('t')
      expect(lits.run('nth("A string", -3)')).toBeNull()
      expect(lits.run('nth("A string", 30)')).toBeNull()
      expect(lits.run('nth("A string", -30)')).toBeNull()
      expect(() => lits.run('nth("A string")')).toThrow(LitsError)
      expect(() => lits.run('nth("A string", 1, 2, 3)')).toThrow(LitsError)
    })

    it('default values', () => {
      expect(lits.run('nth([1, 2, 3], 1, 99)')).toBe(2)
      expect(lits.run('nth([1, 2, 3], 3, 99)')).toBe(99)
      expect(lits.run('nth([1, 2, 3], -1, 99)')).toBe(99)
      expect(lits.run('nth([1, 2, 3], -4, 99)')).toBe(99)
      expect(lits.run('nth("A string", 1, 99)')).toBe(' ')
      expect(lits.run('nth("A string", 3, 99)')).toBe('t')
      expect(lits.run('nth("A string", -3, 99)')).toBe(99)
      expect(lits.run('nth("A string", 30, 99)')).toBe(99)
      expect(lits.run('nth("A string", -30, 99)')).toBe(99)
    })

    it('null sequence', () => {
      expect(lits.run('nth(null, 0)')).toBeNull()
      expect(lits.run('nth(null, 0, 99)')).toBe(99)
    })
  })

  describe('slice', () => {
    it('samples', () => {
      expect(lits.run('slice([1, 2, 3], 0)')).toEqual([1, 2, 3])
      expect(lits.run('slice([1, 2, 3], 1)')).toEqual([2, 3])
      expect(lits.run('slice([1, 2, 3], -1)')).toEqual([3])
      expect(lits.run('slice([1, 2, 3], -3)')).toEqual([1, 2, 3])
      expect(lits.run('slice([1, 2, 3], -4)')).toEqual([1, 2, 3])
      expect(lits.run('slice([1, 2, 3], 3)')).toEqual([])
      expect(lits.run('slice([1, 2, 3], 4)')).toEqual([])
      expect(lits.run('slice([1, 2, 3], 0, 0)')).toEqual([])
      expect(lits.run('slice([1, 2, 3], 0, 1)')).toEqual([1])
      expect(lits.run('slice([1, 2, 3], 0, 10)')).toEqual([1, 2, 3])
      expect(lits.run('slice([1, 2, 3], 0, -1)')).toEqual([1, 2])

      expect(lits.run('slice("Albert", 0)')).toBe('Albert')
      expect(lits.run('slice("Albert", 1)')).toBe('lbert')
      expect(lits.run('slice("Albert", -1)')).toBe('t')
      expect(lits.run('slice("Albert", -3)')).toBe('ert')
      expect(lits.run('slice("Albert", -4)')).toBe('bert')
      expect(lits.run('slice("Albert", -5)')).toBe('lbert')
      expect(lits.run('slice("Albert", -6)')).toBe('Albert')
      expect(lits.run('slice("Albert", -7)')).toBe('Albert')
      expect(lits.run('slice("Albert", 4)')).toBe('rt')
      expect(lits.run('slice("Albert", 5)')).toBe('t')
      expect(lits.run('slice("Albert", 6)')).toBe('')
      expect(lits.run('slice("Albert", 0, 0)')).toBe('')
      expect(lits.run('slice("Albert", 0, 1)')).toBe('A')
      expect(lits.run('slice("Albert", 0, 10)')).toBe('Albert')
      expect(lits.run('slice("Albert", 0, -1)')).toBe('Alber')

      expect(() => lits.run('slice([1, 2, 3], 1, 2, 3)')).toThrow(LitsError)
      expect(() => lits.run('slice()')).toThrow(LitsError)
      expect(() => lits.run('slice("Albert")')).toThrow(LitsError)
      expect(() => lits.run('slice({},)')).toThrow(LitsError)
      expect(() => lits.run('slice(null, 2)')).toThrow(LitsError)
    })
  })

  describe('index-of', () => {
    it('samples', () => {
      expect(lits.run('index-of(["1", "2", 3], "2")')).toEqual(1)
      expect(lits.run('index-of(["1", "2", "3"], "4")')).toBeNull()
      expect(lits.run('index-of([], 1)')).toBeNull()
      expect(lits.run('index-of(null, 1)')).toBeNull()
      expect(lits.run('index-of("AlbertAlbert", "l")')).toBe(1)
      expect(lits.run('index-of("Albert", "ert")')).toBe(3)
      expect(lits.run('index-of("Albert", "z")')).toBeNull()
      expect(lits.run('index-of([1], 2)')).toBeNull()
      expect(() => lits.run('index-of(+)')).toThrow(LitsError)
      expect(() => lits.run('index-of()')).toThrow(LitsError)
    })
  })

  describe('some', () => {
    it('samples', () => {
      expect(lits.run('some("Albert", -> "l" == $)')).toBe('l')

      expect(lits.run('some(null, number?)')).toBeNull()
      expect(lits.run('some(["1", "2", 3], number?)')).toBe(3)
      expect(lits.run('some(["1", "2", "3"], number?)')).toBeNull()
      expect(lits.run('some([], number?)')).toBeNull()
      expect(lits.run('some([1, 2, 3, 4, 5, 6, 7], -> zero?($ mod 3))')).toBe(3)

      expect(lits.run('some("Aa", -> $ >= "a")')).toBe('a')
      expect(lits.run('some("Aa", -> $ >= "z")')).toBeNull()

      expect(() => lits.run('some(+)')).toThrow(LitsError)
      expect(() => lits.run('some()')).toThrow(LitsError)
      expect(() => lits.run('some([1], number? 2)')).toThrow(LitsError)
    })
  })

  describe('first', () => {
    it('samples', () => {
      expect(lits.run('first([1, 2, 3])')).toEqual(1)
      expect(lits.run('first(["1"])')).toEqual('1')
      expect(lits.run('first([])')).toBeNull()
      expect(lits.run('first("AB")')).toBe('A')
      expect(lits.run('first("A")')).toBe('A')
      expect(lits.run('first("")')).toBeNull()
      expect(lits.run('first(null)')).toBeNull()

      expect(() => lits.run('first()')).toThrow(LitsError)
      expect(() => lits.run('first(true)')).toThrow(LitsError)
      expect(() => lits.run('first(false)')).toThrow(LitsError)
      expect(() => lits.run('first(object())')).toThrow(LitsError)
      expect(() => lits.run('first(10)')).toThrow(LitsError)
    })
  })

  describe('second', () => {
    it('samples', () => {
      expect(lits.run('second([1, 2, 3])')).toEqual(2)
      expect(lits.run('second(["1"])')).toBeNull()
      expect(lits.run('second([])')).toBeNull()

      expect(lits.run('second("ABC")')).toBe('B')
      expect(lits.run('second("AB")')).toBe('B')
      expect(lits.run('second("A")')).toBeNull()
      expect(lits.run('second("")')).toBeNull()

      expect(lits.run('second(null)')).toBeNull()

      expect(() => lits.run('second()')).toThrow(LitsError)
      expect(() => lits.run('second(true)')).toThrow(LitsError)
      expect(() => lits.run('second(false)')).toThrow(LitsError)
      expect(() => lits.run('second(object())')).toThrow(LitsError)
      expect(() => lits.run('second(10)')).toThrow(LitsError)
    })
  })

  describe('reverse', () => {
    it('samples', () => {
      expect(lits.run('reverse([1, 2, 3])')).toEqual([3, 2, 1])
      expect(lits.run('reverse(["1"])')).toEqual(['1'])
      expect(lits.run('reverse([])')).toEqual([])
      expect(lits.run('reverse("albert")')).toBe('trebla')
      expect(lits.run('reverse("A 1")')).toBe('1 A')
      expect(lits.run('reverse("")')).toBe('')

      expect(lits.run('reverse(null)')).toBeNull()

      expect(() => lits.run('reverse()')).toThrow(LitsError)
      expect(() => lits.run('reverse("word1", "word2")')).toThrow(LitsError)
      expect(() => lits.run('reverse()')).toThrow(LitsError)
      expect(() => lits.run('reverse(true)')).toThrow(LitsError)
      expect(() => lits.run('reverse(false)')).toThrow(LitsError)
      expect(() => lits.run('reverse(object())')).toThrow(LitsError)
      expect(() => lits.run('reverse(10)')).toThrow(LitsError)
    })
    it('returns a new array instance', () => {
      const program = `
        let l = [1, 2, 3];
        not(l identical? reverse(l))
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('last', () => {
    it('samples', () => {
      expect(lits.run('last([1, 2, 3])')).toEqual(3)
      expect(lits.run('last(["1"])')).toEqual('1')
      expect(lits.run('last([])')).toBeNull()
      expect(lits.run('last("Albert")')).toBe('t')
      expect(lits.run('last("1")')).toBe('1')
      expect(lits.run('last("")')).toBeNull()

      expect(lits.run('last(null)')).toBeNull()

      expect(() => lits.run('last()')).toThrow(LitsError)
      expect(() => lits.run('last(true)')).toThrow(LitsError)
      expect(() => lits.run('last(false)')).toThrow(LitsError)
      expect(() => lits.run('last(object())')).toThrow(LitsError)
      expect(() => lits.run('last(10)')).toThrow(LitsError)
    })
  })

  describe('rest', () => {
    it('samples', () => {
      expect(lits.run('rest([1, 2, 3])')).toEqual([2, 3])
      expect(lits.run('rest([1, 2])')).toEqual([2])
      expect(lits.run('rest(["1"])')).toEqual([])
      expect(lits.run('rest([])')).toEqual([])
      expect(lits.run('rest("Albert")')).toEqual('lbert')
      expect(lits.run('rest("A")')).toEqual('')
      expect(lits.run('rest("")')).toEqual('')

      expect(() => lits.run('rest()')).toThrow(LitsError)
      expect(() => lits.run('rest(true)')).toThrow(LitsError)
      expect(() => lits.run('rest(false)')).toThrow(LitsError)
      expect(() => lits.run('rest(null)')).toThrow(LitsError)
      expect(() => lits.run('rest(object())')).toThrow(LitsError)
      expect(() => lits.run('rest(10)')).toThrow(LitsError)
    })
  })

  describe('next', () => {
    it('samples', () => {
      expect(lits.run('next([1, 2, 3])')).toEqual([2, 3])
      expect(lits.run('next([1, 2])')).toEqual([2])
      expect(lits.run('next(["1"])')).toBeNull()
      expect(lits.run('next([])')).toBeNull()
      expect(lits.run('next("Albert")')).toEqual('lbert')
      expect(lits.run('next("A")')).toBeNull()
      expect(lits.run('next("")')).toBeNull()

      expect(() => lits.run('next()')).toThrow(LitsError)
      expect(() => lits.run('next(true)')).toThrow(LitsError)
      expect(() => lits.run('next(false)')).toThrow(LitsError)
      expect(() => lits.run('next(null)')).toThrow(LitsError)
      expect(() => lits.run('next(object())')).toThrow(LitsError)
      expect(() => lits.run('next(10)')).toThrow(LitsError)
    })
  })

  describe('push', () => {
    it('samples', () => {
      expect(lits.run('push([1, 2, 3], 0)')).toEqual([1, 2, 3, 0])
      expect(lits.run('push([1, 2, 3], 1, "2")')).toEqual([1, 2, 3, 1, '2'])
      expect(lits.run('let l = [1, 2, 3]; push(l, 1, "2")')).toEqual([1, 2, 3, 1, '2'])
      expect(lits.run('let l = [1, 2, 3]; push(l, 1, "2"); l')).toEqual([1, 2, 3])
      expect(lits.run('push("Albert", "!")')).toBe('Albert!')
      expect(lits.run('push("Albert", "!", "?")')).toBe('Albert!?')
      expect(lits.run('push("", "!", "?")')).toBe('!?')

      expect(() => lits.run('push("Albert", "!?")')).toThrow(LitsError)
      expect(() => lits.run('push([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('push({}, "2")')).toThrow(LitsError)
      expect(() => lits.run('push(null, 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push(true 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push(false 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push(1, 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push("1", 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push(0 "2")')).toThrow(LitsError)
      expect(() => lits.run('push()')).toThrow(LitsError)
    })
  })

  describe('pop', () => {
    it('samples', () => {
      expect(lits.run('pop([1, 2, 3])')).toEqual([1, 2])
      expect(lits.run('pop([])')).toEqual([])
      expect(lits.run('let l = [1, 2, 3]; pop(l); l')).toEqual([1, 2, 3])
      expect(lits.run('let l = [1, 2, 3]; pop(l)')).toEqual([1, 2])
      expect(lits.run('let l = []; pop(l); l')).toEqual([])
      expect(lits.run('pop("Albert")')).toBe('Alber')
      expect(lits.run('pop("1")')).toBe('')
      expect(lits.run('pop("")')).toBe('')

      expect(() => lits.run('pop(object())')).toThrow(LitsError)
      expect(() => lits.run('pop(null)')).toThrow(LitsError)
      expect(() => lits.run('pop(true)')).toThrow(LitsError)
      expect(() => lits.run('pop(false)')).toThrow(LitsError)
      expect(() => lits.run('pop(1)')).toThrow(LitsError)
      expect(() => lits.run('pop()')).toThrow(LitsError)
    })
  })

  describe('sort', () => {
    it('samples', () => {
      expect(lits.run('sort([3, 1, 2], (a, b) -> cond case a < b then -1 case a > b then 1 case true then 0 end)')).toEqual([1, 2, 3])
      expect(lits.run('sort([3, 1, 2], (a, b) -> cond case a > b then -1 case a < b then 1 case true then 0 end)')).toEqual([3, 2, 1])
      expect(lits.run('sort([], (a, b) -> cond case a > b then -1 case a < b then 1 case true then 0 end)')).toEqual([])

      expect(lits.run('sort("Albert", (a, b) -> cond case a < b then 1 case a > b then -1 case true then 0 end)')).toBe('trlebA')

      expect(lits.run('sort("Albert")')).toBe('Abelrt')

      expect(() => lits.run('sort(10, (a, b) -> cond case a > b then -1 case a < b then 1 case true then -1 end)')).toThrow(LitsError)
      expect(() => lits.run('sort((a, b) -> cond case a > b then -1 case a < b then 1 case true then -1 end)')).toThrow(LitsError)
      expect(() => lits.run('sort()')).toThrow(LitsError)
    })
  })

  describe('join', () => {
    it('samples', () => {
      expect(lits.run('join(["Albert", "Mojir"], ", ")')).toBe('Albert, Mojir')
      expect(lits.run('join(["Albert", 10], ", ")')).toBe('Albert, 10')
      expect(lits.run('join(map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str), ", ")')).toBe('0, 1, 2, 3, 4, 5, 6, 7, 8, 9')
      expect(() => lits.run('join((map [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str) ", ", 5)')).toThrow(LitsError)
      expect(() => lits.run('join(["Albert", "Mojir"], ", ", -1)')).toThrow(LitsError)
      expect(() => lits.run('join(["Albert", "Mojir"])')).toThrow(LitsError)
    })
  })
})

describe('sequence-Utils module functions', () => {
  const imp = 'let su = import("sequence"); '
  for (const mlits of [new Lits({ modules: [sequenceUtilsModule] }), new Lits({ modules: [sequenceUtilsModule], debug: true })]) {
    describe('position', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.position(["1", "2", 3], number?)`)).toEqual(2)
        expect(mlits.run(`${imp}su.position(["1", "2", "3"], number?)`)).toBeNull()
        expect(mlits.run(`${imp}su.position([], number?)`)).toBeNull()
        expect(mlits.run(`${imp}su.position(null, number?)`)).toBeNull()
        expect(mlits.run(`${imp}su.position([1, 2, 3, 4, 5, 6, 7], -> zero?($ mod 3))`)).toEqual(2)
        expect(mlits.run(`${imp}su.position("Aa", -> $ >= "a")`)).toBe(1)
        expect(mlits.run(`${imp}su.position("Aa", -> $ == "z")`)).toBeNull()
        expect(() => mlits.run(`${imp}su.position(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.position()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.position([1], number? 2)`)).toThrow(LitsError)
      })
    })

    describe('last-index-of', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.last-index-of(["1", "2", 3], "2")`)).toEqual(1)
        expect(mlits.run(`${imp}su.last-index-of(["1", "2", "3"], "4")`)).toBeNull()
        expect(mlits.run(`${imp}su.last-index-of([], 1)`)).toBeNull()
        expect(mlits.run(`${imp}su.last-index-of(null, 1)`)).toBeNull()
        expect(mlits.run(`${imp}su.last-index-of("AlbertAlbert", "l")`)).toBe(7)
        expect(mlits.run(`${imp}su.last-index-of("Albert", "ert")`)).toBe(3)
        expect(mlits.run(`${imp}su.last-index-of("Albert", "z")`)).toBeNull()
        expect(mlits.run(`${imp}su.last-index-of([1], 2)`)).toBeNull()
        expect(() => mlits.run(`${imp}su.last-index-of(+)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.last-index-of()`)).toThrow(LitsError)
      })
    })

    describe('unshift', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.unshift([1, 2, 3], 0)`)).toEqual([0, 1, 2, 3])
        expect(mlits.run(`${imp}su.unshift([1, 2, 3], 1, "2")`)).toEqual([1, '2', 1, 2, 3])
        expect(mlits.run(`${imp}let l = [1, 2, 3]; su.unshift(l, 1, "2"); l`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}let l = [1, 2, 3]; su.unshift(l, 1, "2")`)).toEqual([1, '2', 1, 2, 3])
        expect(mlits.run(`${imp}su.unshift("lbert", "A")`)).toBe('Albert')

        expect(() => mlits.run(`${imp}su.unshift([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift({}, "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift(null, 0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift(true 0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift(false 0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift(1, 0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift("1", 0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift(0 "2")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.unshift()`)).toThrow(LitsError)
      })
    })

    describe('shift', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.shift([1, 2, 3])`)).toEqual([2, 3])
        expect(mlits.run(`${imp}su.shift([])`)).toEqual([])
        expect(mlits.run(`${imp}let l = [1, 2, 3]; su.shift(l); l`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}let l = [1, 2, 3]; su.shift(l)`)).toEqual([2, 3])
        expect(mlits.run(`${imp}let l = []; su.shift(l); l`)).toEqual([])
        expect(mlits.run(`${imp}su.shift("Albert")`)).toBe('lbert')
        expect(mlits.run(`${imp}su.shift("1")`)).toBe('')
        expect(mlits.run(`${imp}su.shift("")`)).toBe('')

        expect(() => mlits.run(`${imp}su.shift(object())`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.shift(null)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.shift(true)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.shift(false)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.shift(1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.shift()`)).toThrow(LitsError)
      })
    })

    describe('take', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.take([1, 2, 3], 2)`)).toEqual([1, 2])
        expect(mlits.run(`${imp}su.take([], 2)`)).toEqual([])
        expect(mlits.run(`${imp}su.take([1, 2, 3], 20)`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}su.take([1, 2, 3], 0)`)).toEqual([])
        expect(mlits.run(`${imp}su.take("Albert", 2)`)).toEqual('Al')
        expect(mlits.run(`${imp}su.take("Albert", 2.01)`)).toEqual('Alb')

        expect(() => mlits.run(`${imp}su.take({},)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take(null, 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take(true 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take(false 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take("Hej", "1")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take([1, 2, 3], 1, 2)`)).toThrow(LitsError)
      })

      it('new array created', () => {
        const program = `${imp}
          let l1 = [1, 2, 3];
          let l2 = su.take(l1, 2);
          l1 != l2
        `
        expect(mlits.run(program)).toBe(true)
      })
    })

    describe('take-last', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.take-last([1, 2, 3], 2)`)).toEqual([2, 3])
        expect(mlits.run(`${imp}su.take-last([1, 2, 3], 20)`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}su.take-last([1, 2, 3], 0)`)).toEqual([])
        expect(mlits.run(`${imp}su.take-last([1, 2, 3], 0.01)`)).toEqual([3])

        expect(() => mlits.run(`${imp}su.take-last(object())`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last(null)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last(true)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last(false)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last("1")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-last([1, 2, 3], 1, 2)`)).toThrow(LitsError)
      })

      it('new array created', () => {
        const program = `${imp}
          let l1 = [1, 2, 3];
          let l2 = su.take-last(l1, 2);
          l1 != l2
        `
        expect(mlits.run(program)).toBe(true)
      })
    })

    describe('take-while', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.take-while([1, 2, 3, 2, 1], -> $ < 3)`)).toEqual([1, 2])
        expect(mlits.run(`${imp}su.take-while([1, 2, 3, 2, 1], -> $ > 3)`)).toEqual([])
        expect(mlits.run(`${imp}su.take-while("abcdabcd", -> $ <= "c")`)).toEqual('abc')
        expect(mlits.run(`${imp}su.take-while([1, 2, 3], -> $ < 10)`)).toEqual([1, 2, 3])

        expect(() => mlits.run(`${imp}su.take-while({}, -> $ < 3))`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while(null, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while(true, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while(false, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while([1, 2, 3], 10)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.take-while([1, 2, 3], -> $ < 3 1)`)).toThrow(LitsError)
      })
      it('new array created', () => {
        const program = `${imp}
          let l1 = [1, 2, 3];
          let l2 = su.take-while(l1, -> $ < 3);
          l1 != l2
        `
        expect(mlits.run(program)).toBe(true)
      })
    })

    describe('drop', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.drop([1, 2, 3], 2)`)).toEqual([3])
        expect(mlits.run(`${imp}su.drop([1, 2, 3], 20)`)).toEqual([])
        expect(mlits.run(`${imp}su.drop([1, 2, 3], 0)`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}su.drop("Albert", 2)`)).toEqual('bert')
        expect(mlits.run(`${imp}su.drop([1, 2, 3], 0.5)`)).toEqual([2, 3])
        expect(mlits.run(`${imp}su.drop("Albert", -2)`)).toEqual('Albert')

        expect(() => mlits.run(`${imp}su.drop({},)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop(null, 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop(true 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop(false 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop("Hej", "1")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop([1, 2, 3], 1, 2)`)).toThrow(LitsError)
      })

      it('new array created', () => {
        const program = `${imp}
          let l1 = [1, 2, 3];
          let l2 = su.drop(l1, 2);
          l1 != l2
        `
        expect(mlits.run(program)).toBe(true)
      })
    })

    describe('drop-last', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.drop-last([1, 2, 3], 2)`)).toEqual([1])
        expect(mlits.run(`${imp}su.drop-last([1, 2, 3], 20)`)).toEqual([])
        expect(mlits.run(`${imp}su.drop-last([1, 2, 3], 0)`)).toEqual([1, 2, 3])
        expect(mlits.run(`${imp}su.drop-last("Albert", 2)`)).toEqual('Albe')
        expect(mlits.run(`${imp}su.drop-last([1, 2, 3], 0.5)`)).toEqual([1, 2])
        expect(mlits.run(`${imp}su.drop-last("Albert", -2)`)).toEqual('Albert')

        expect(() => mlits.run(`${imp}su.drop-last({},)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last(null, 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last(true 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last(false 1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last("Hej", "1")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-last([1, 2, 3], 1, 2)`)).toThrow(LitsError)
      })
    })

    describe('drop-while', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.drop-while([1, 2, 3, 2, 1], -> $ < 3)`)).toEqual([3, 2, 1])
        expect(mlits.run(`${imp}su.drop-while([1, 2, 3, 2, 1], -> $ > 3)`)).toEqual([1, 2, 3, 2, 1])
        expect(mlits.run(`${imp}su.drop-while("abcdab", -> $ <= "c")`)).toEqual('dab')
        expect(mlits.run(`${imp}su.drop-while([1, 2, 3], -> $ < 10)`)).toEqual([])
        expect(mlits.run(`${imp}su.drop-while("abc", -> true)`)).toEqual('')

        expect(() => mlits.run(`${imp}su.drop-while({}, -> $ < 3))`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while(null, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while(true, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while(false, -> $ < 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while([1, 2, 3], 10)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while([1, 2, 3])`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.drop-while([1, 2, 3], -> $ < 3 1)`)).toThrow(LitsError)
      })
      it('new array created', () => {
        const program = `${imp}
          let l1 = [1, 2, 3];
          let l2 = su.take-while(l1, -> $ < 3);
          l1 != l2
        `
        expect(mlits.run(program)).toBe(true)
      })
    })

    describe('sort-by', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.sort-by(["Albert", "Mojir", "Nina"], count)`)).toEqual(['Nina', 'Mojir', 'Albert'])
        expect(mlits.run(`${imp}su.sort-by(["Albert", "Mojir", "Nina"], count, (a, b) -> b - a)`)).toEqual([
          'Albert',
          'Mojir',
          'Nina',
        ])
        expect(mlits.run(`${imp}su.sort-by("Albert", lower-case)`)).toEqual('Abelrt')
        expect(mlits.run(`${imp}su.sort-by("Albert", lower-case, (a, b) -> compare(b, a))`)).toEqual(
          'trlebA',
        )
        expect(() => mlits.run(`${imp}su.sort-by()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.sort-by("a")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.sort-by({} "a")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.sort-by(3 "a")`)).toThrow(LitsError)
      })
    })

    describe('distinct', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.distinct([1, 2, 3, 1, 3, 5])`)).toEqual([1, 2, 3, 5])
        expect(mlits.run(`${imp}su.distinct([])`)).toEqual([])
        expect(mlits.run(`${imp}su.distinct("Albert Mojir")`)).toBe('Albert Moji')
        expect(mlits.run(`${imp}su.distinct("")`)).toBe('')
        expect(() => mlits.run(`${imp}su.distinct()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.distinct([], [])`)).toThrow(LitsError)
      })
    })

    describe('remove', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.remove([1, 2, 3, 1, 3, 5], even?)`)).toEqual([1, 3, 1, 3, 5])
        expect(mlits.run(`${imp}su.remove("Albert Mojir", -> contains?("aoueiyAOUEIY", $1))`)).toBe('lbrt Mjr')
        expect(() => mlits.run(`${imp}su.remove()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove("Albert Mojir")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove(=> contains?("aoueiyAOUEIY", $1))`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove("Albert", => contains?("aoueiyAOUEIY", $1) "Mojir")`)).toThrow(LitsError)
      })
    })

    describe('remove-at', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.remove-at([1, 2, 3, 4, 5], -1)`)).toEqual([1, 2, 3, 4])
        expect(mlits.run(`${imp}su.remove-at([1, 2, 3, 4, 5], 0)`)).toEqual([2, 3, 4, 5])
        expect(mlits.run(`${imp}su.remove-at([1, 2, 3, 4, 5], 2)`)).toEqual([1, 2, 4, 5])
        expect(mlits.run(`${imp}su.remove-at([1, 2, 3, 4, 5], 4)`)).toEqual([1, 2, 3, 4])
        expect(mlits.run(`${imp}su.remove-at([1, 2, 3, 4, 5], 5)`)).toEqual([1, 2, 3, 4, 5])
        expect(mlits.run(`${imp}su.remove-at("Mojir", -1)`)).toEqual('Moji')
        expect(mlits.run(`${imp}su.remove-at("Mojir", 0)`)).toEqual('ojir')
        expect(mlits.run(`${imp}su.remove-at("Mojir", 2)`)).toEqual('Moir')
        expect(mlits.run(`${imp}su.remove-at("Mojir", 4)`)).toEqual('Moji')
        expect(mlits.run(`${imp}su.remove-at("Mojir", 5)`)).toEqual('Mojir')
        expect(() => mlits.run(`${imp}su.remove-at()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove-at("Albert Mojir")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove-at(1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.remove-at("Albert", 1, 2`)).toThrow(LitsError)
      })
    })

    describe('split-at', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.split-at([1, 2, 3, 4, 5], 2)`)).toEqual([
          [1, 2],
          [3, 4, 5],
        ])
        expect(mlits.run(`${imp}su.split-at([1, 2, 3, 4, 5], 0)`)).toEqual([[], [1, 2, 3, 4, 5]])
        expect(mlits.run(`${imp}su.split-at([1, 2, 3, 4, 5], -1)`)).toEqual([[1, 2, 3, 4], [5]])
        expect(mlits.run(`${imp}su.split-at([1, 2, 3, 4, 5], 100)`)).toEqual([[1, 2, 3, 4, 5], []])
        expect(mlits.run(`${imp}su.split-at("Albert", 2)`)).toEqual(['Al', 'bert'])
        expect(mlits.run(`${imp}su.split-at("Albert", 0)`)).toEqual(['', 'Albert'])
        expect(mlits.run(`${imp}su.split-at("Albert", -2)`)).toEqual(['Albe', 'rt'])
        expect(mlits.run(`${imp}su.split-at("Albert", 100)`)).toEqual(['Albert', ''])

        expect(() => mlits.run(`${imp}su.split-at([1, 2, 3, 4, 5], 0.01)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.split-at()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.split-at(3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.split-at("Albert", 3 "Mojir")`)).toThrow(LitsError)
      })
    })

    describe('split-with', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.split-with([1, 2, 3, 4, 5], -> $1 < 3)`)).toEqual([
          [1, 2],
          [3, 4, 5],
        ])
        expect(mlits.run(`${imp}su.split-with([1, 2, 3, 4, 5], -> $1 > 3)`)).toEqual([[], [1, 2, 3, 4, 5]])
        expect(mlits.run(`${imp}su.split-with([1, 2, 3, 4, 5], -> $1 < 10)`)).toEqual([[1, 2, 3, 4, 5], []])

        expect(mlits.run(`${imp}su.split-with("Albert", -> $1 <= "Z")`)).toEqual(['A', 'lbert'])
        expect(mlits.run(`${imp}su.split-with("Albert", -> $1 > "Z")`)).toEqual(['', 'Albert'])
        expect(mlits.run(`${imp}su.split-with("Albert", -> $1 <= "z")`)).toEqual(['Albert', ''])

        expect(() => mlits.run(`${imp}su.split-with()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.split-with(-> $1 <= "Z")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.split-with("Albert", -> $1 <= "Z", "Mojir")`)).toThrow(LitsError)
      })
    })

    describe('frequencies', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.frequencies(["Albert", "Mojir", "Nina", "Mojir"])`)).toEqual({ Albert: 1, Nina: 1, Mojir: 2 })
        expect(mlits.run(`${imp}su.frequencies("Pneumonoultramicroscopicsilicovolcanoconiosis")`)).toEqual({
          P: 1,
          a: 2,
          c: 6,
          e: 1,
          i: 6,
          l: 3,
          m: 2,
          n: 4,
          o: 9,
          p: 1,
          r: 2,
          s: 4,
          t: 1,
          u: 2,
          v: 1,
        })
        expect(() => mlits.run(`${imp}su.frequencies()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.frequencies({})`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.frequencies(3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.frequencies("", "")`)).toThrow(LitsError)
      })
    })

    describe('group-by', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.group-by([{name: "Albert"}, {name: "Albert"}, {name: "Mojir"}], "name")`)).toEqual({
          Albert: [{ name: 'Albert' }, { name: 'Albert' }],
          Mojir: [{ name: 'Mojir' }],
        })
        expect(mlits.run(`${imp}su.group-by("Albert Mojir", -> if "aoueiAOUEI" contains? $1 then "vowel" else "other" end)`)).toEqual({
          other: ['l', 'b', 'r', 't', ' ', 'M', 'j', 'r'],
          vowel: ['A', 'e', 'o', 'i'],
        })
        expect(() => mlits.run(`${imp}su.group-by()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.group-by("a")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.group-by("a" {})`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.group-by("a" 3)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.group-by("", "a", "")`)).toThrow(LitsError)
      })
    })

    describe('partition', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.partition(range(20), 4)`)).toEqual([
          [0, 1, 2, 3],
          [4, 5, 6, 7],
          [8, 9, 10, 11],
          [12, 13, 14, 15],
          [16, 17, 18, 19],
        ])
        expect(mlits.run(`${imp}su.partition(range(22), 4)`)).toEqual([
          [0, 1, 2, 3],
          [4, 5, 6, 7],
          [8, 9, 10, 11],
          [12, 13, 14, 15],
          [16, 17, 18, 19],
        ])
        expect(mlits.run(`${imp}su.partition(range(20), 4, 6)`)).toEqual([
          [0, 1, 2, 3],
          [6, 7, 8, 9],
          [12, 13, 14, 15],
        ])
        expect(mlits.run(`${imp}su.partition(range(20), 4, 3)`)).toEqual([
          [0, 1, 2, 3],
          [3, 4, 5, 6],
          [6, 7, 8, 9],
          [9, 10, 11, 12],
          [12, 13, 14, 15],
          [15, 16, 17, 18],
        ])
        expect(mlits.run(`${imp}su.partition(range(20), 3, 6, ["a"])`)).toEqual([
          [0, 1, 2],
          [6, 7, 8],
          [12, 13, 14],
          [18, 19, 'a'],
        ])
        expect(mlits.run(`${imp}su.partition(range(20), 4, 6, ["a"])`)).toEqual([
          [0, 1, 2, 3],
          [6, 7, 8, 9],
          [12, 13, 14, 15],
          [18, 19, 'a'],
        ])
        expect(mlits.run(`${imp}su.partition(range(20), 4, 6, ["a", "b", "c", "d"])`)).toEqual([
          [0, 1, 2, 3],
          [6, 7, 8, 9],
          [12, 13, 14, 15],
          [18, 19, 'a', 'b'],
        ])
        expect(mlits.run(`${imp}su.partition(["a", "b", "c", "d", "e", "f"], 3, 1)`)).toEqual([
          ['a', 'b', 'c'],
          ['b', 'c', 'd'],
          ['c', 'd', 'e'],
          ['d', 'e', 'f'],
        ])
        expect(mlits.run(`${imp}su.partition([1, 2, 3, 4], 10)`)).toEqual([])
        expect(mlits.run(`${imp}su.partition([1, 2, 3, 4], 10, 10)`)).toEqual([])
        expect(mlits.run(`${imp}su.partition([1, 2, 3, 4], 10, 10, [])`)).toEqual([[1, 2, 3, 4]])
        expect(mlits.run(`${imp}su.partition([1, 2, 3, 4], 10, 10, null)`)).toEqual([[1, 2, 3, 4]])
        expect(mlits.run(`${imp}su.partition("superfragilistic", 5)`)).toEqual(['super', 'fragi', 'listi'])
        expect(mlits.run(`${imp}su.partition("superfragilistic", 5, 5, null)`)).toEqual(['super', 'fragi', 'listi', 'c'])
        expect(mlits.run(`${imp}let foo = [5, 6, 7, 8]; su.partition(foo, 2, 1, foo)`)).toEqual([
          [5, 6],
          [6, 7],
          [7, 8],
          [8, 5],
        ])
        expect(() => mlits.run(`${imp}su.partition[1, 2, 3, 4], 0)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.partition1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.partition[1])`)).toThrow(LitsError)
      })
    })

    describe('partition-all', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)`)).toEqual([
          [0, 1, 2, 3],
          [4, 5, 6, 7],
          [8, 9],
        ])
        expect(mlits.run(`${imp}su.partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 2, 4)`)).toEqual([
          [0, 1],
          [4, 5],
          [8, 9],
        ])
        expect(() => mlits.run(`${imp}su.partition-all(1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.partition-all([1])`)).toThrow(LitsError)
      })
    })

    describe('partition-by', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.partition-by([1, 2, 3, 4, 5], -> 3 == $1)`)).toEqual([[1, 2], [3], [4, 5]])
        expect(mlits.run(`${imp}su.partition-by([1, 1, 1, 2, 2, 3, 3], odd?)`)).toEqual([
          [1, 1, 1],
          [2, 2],
          [3, 3],
        ])
        expect(mlits.run(`${imp}su.partition-by("Leeeeeerrroyyy", identity)`)).toEqual(['L', 'eeeeee', 'rrr', 'o', 'yyy'])
        expect(() => mlits.run(`${imp}su.partition-by(odd?)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.partition-by([1, 2, 3])`)).toThrow(LitsError)
      })
    })

    describe('starts-with?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.starts-with?([1, 2, 3], 1)`)).toBe(true)
        expect(mlits.run(`${imp}su.starts-with?([1, 2, 3], 2)`)).toBe(false)
        expect(mlits.run(`${imp}su.starts-with?([1, 2, 3], [1])`)).toBe(false)

        expect(mlits.run(`${imp}su.starts-with?("Albert", "Al")`)).toBe(true)
        expect(mlits.run(`${imp}su.starts-with?("Albert", "al")`)).toBe(false)
        expect(mlits.run(`${imp}su.starts-with?("Albert", "")`)).toBe(true)
        expect(mlits.run(`${imp}su.starts-with?("", "")`)).toBe(true)
        expect(mlits.run(`${imp}su.starts-with?("Albert", "Albert")`)).toBe(true)
        expect(mlits.run(`${imp}su.starts-with?("Albert", "Albert ")`)).toBe(false)
        expect(() => mlits.run(`${imp}su.starts-with?("Albert", "foo", 2)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.starts-with?("Albert")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.starts-with?(`)).toThrow(LitsError)
      })
    })

    describe('ends-with?', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.ends-with?([1, 2, 3], 3)`)).toBe(true)
        expect(mlits.run(`${imp}su.ends-with?([1, 2, 3], 2)`)).toBe(false)
        expect(mlits.run(`${imp}su.ends-with?([1, 2, 3], [3])`)).toBe(false)

        expect(mlits.run(`${imp}su.ends-with?("Albert", "rt")`)).toBe(true)
        expect(mlits.run(`${imp}su.ends-with?("Albert", "RT")`)).toBe(false)
        expect(mlits.run(`${imp}su.ends-with?("Albert", "")`)).toBe(true)
        expect(mlits.run(`${imp}su.ends-with?("", "")`)).toBe(true)
        expect(mlits.run(`${imp}su.ends-with?("Albert", "Albert")`)).toBe(true)
        expect(mlits.run(`${imp}su.ends-with?("Albert", ", Albert")`)).toBe(false)
        expect(() => mlits.run(`${imp}su.ends-with?("Albert", "foo", 2)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.ends-with?("Albert")`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.ends-with?()`)).toThrow(LitsError)
      })
    })
    describe('interleave', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.interleave([1, 2, 3], [4, 5, 6])`)).toEqual([1, 4, 2, 5, 3, 6])
        expect(mlits.run(`${imp}su.interleave([1, 2, 3], [4, 5, 6], [7, 8, 9])`)).toEqual([1, 4, 7, 2, 5, 8, 3, 6, 9])
        expect(mlits.run(`${imp}su.interleave([1, 2, 3], [4, 5, 6], [7, 8])`)).toEqual([1, 4, 7, 2, 5, 8])
        expect(mlits.run(`${imp}su.interleave([1, 2, 3], [4, 5, 6], [7])`)).toEqual([1, 4, 7])
        expect(mlits.run(`${imp}su.interleave([1, 2, 3], [4, 5, 6], [7], [8, 9])`)).toEqual([1, 4, 7, 8])
        expect(mlits.run(`${imp}su.interleave([], [4, 5, 6], [7], [8, 9])`)).toEqual([])
        expect(mlits.run(`${imp}su.interleave("Albert", "Mojir")`)).toEqual('AMlobjeirr')

        expect(() => mlits.run(`${imp}su.interleave()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.interleave(1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.interleave([1, 2, 3], "asd")`)).toThrow(LitsError)
      })
    })
    describe('interpose', () => {
      it('samples', () => {
        expect(mlits.run(`${imp}su.interpose([1, 2, 3, 4], "a")`)).toEqual([1, 'a', 2, 'a', 3, 'a', 4])
        expect(mlits.run(`${imp}su.interpose([1, 2, 3], "a")`)).toEqual([1, 'a', 2, 'a', 3])
        expect(mlits.run(`${imp}su.interpose([1], "a")`)).toEqual([1])
        expect(mlits.run(`${imp}su.interpose([], "a")`)).toEqual([])
        expect(mlits.run(`${imp}su.interpose("Albert", ":")`)).toEqual('A:l:b:e:r:t')
        expect(() => mlits.run(`${imp}su.interpose()`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.interpose(1)`)).toThrow(LitsError)
        expect(() => mlits.run(`${imp}su.interpose("a", 1)`)).toThrow(LitsError)
      })
    })
  }
})
