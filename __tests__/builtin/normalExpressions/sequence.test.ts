import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'

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

  describe('position', () => {
    it('samples', () => {
      expect(lits.run('position(["1", "2", 3], number?)')).toEqual(2)
      expect(lits.run('position(["1", "2", "3"], number?)')).toBeNull()
      expect(lits.run('position([], number?)')).toBeNull()
      expect(lits.run('position(null, number?)')).toBeNull()
      expect(lits.run('position([1, 2, 3, 4, 5, 6, 7], -> zero?($ mod 3))')).toEqual(2)
      expect(lits.run('position("Aa", -> $ >= "a")')).toBe(1)
      expect(lits.run('position("Aa", -> $ = "z")')).toBeNull()
      expect(() => lits.run('position(+)')).toThrow(LitsError)
      expect(() => lits.run('position()')).toThrow(LitsError)
      expect(() => lits.run('position([1], number? 2)')).toThrow(LitsError)
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

  describe('last-index-of', () => {
    it('samples', () => {
      expect(lits.run('last-index-of(["1", "2", 3], "2")')).toEqual(1)
      expect(lits.run('last-index-of(["1", "2", "3"], "4")')).toBeNull()
      expect(lits.run('last-index-of([], 1)')).toBeNull()
      expect(lits.run('last-index-of(null, 1)')).toBeNull()
      expect(lits.run('last-index-of("AlbertAlbert", "l")')).toBe(7)
      expect(lits.run('last-index-of("Albert", "ert")')).toBe(3)
      expect(lits.run('last-index-of("Albert", "z")')).toBeNull()
      expect(lits.run('last-index-of([1], 2)')).toBeNull()
      expect(() => lits.run('last-index-of(+)')).toThrow(LitsError)
      expect(() => lits.run('last-index-of()')).toThrow(LitsError)
    })
  })

  describe('some', () => {
    it('samples', () => {
      expect(lits.run('some("Albert", -> "l" = $)')).toBe('l')

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
        let l := [1, 2, 3];
        !(l identical? reverse(l))
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
      expect(lits.run('let l := [1, 2, 3]; push(l, 1, "2")')).toEqual([1, 2, 3, 1, '2'])
      expect(lits.run('let l := [1, 2, 3]; push(l, 1, "2"); l')).toEqual([1, 2, 3])
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
      expect(lits.run('let l := [1, 2, 3]; pop(l); l')).toEqual([1, 2, 3])
      expect(lits.run('let l := [1, 2, 3]; pop(l)')).toEqual([1, 2])
      expect(lits.run('let l := []; pop(l); l')).toEqual([])
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

  describe('unshift', () => {
    it('samples', () => {
      expect(lits.run('unshift([1, 2, 3], 0)')).toEqual([0, 1, 2, 3])
      expect(lits.run('unshift([1, 2, 3], 1, "2")')).toEqual([1, '2', 1, 2, 3])
      expect(lits.run('let l := [1, 2, 3]; unshift(l, 1, "2"); l')).toEqual([1, 2, 3])
      expect(lits.run('let l := [1, 2, 3]; unshift(l, 1, "2")')).toEqual([1, '2', 1, 2, 3])
      expect(lits.run('unshift("lbert", "A")')).toBe('Albert')

      expect(() => lits.run('unshift([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('unshift({}, "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift(null, 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift(true 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift(false 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift(1, 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift("1", 0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift(0 "2")')).toThrow(LitsError)
      expect(() => lits.run('unshift()')).toThrow(LitsError)
    })
  })

  describe('shift', () => {
    it('samples', () => {
      expect(lits.run('shift([1, 2, 3])')).toEqual([2, 3])
      expect(lits.run('shift([])')).toEqual([])
      expect(lits.run('let l := [1, 2, 3]; shift(l); l')).toEqual([1, 2, 3])
      expect(lits.run('let l := [1, 2, 3]; shift(l)')).toEqual([2, 3])
      expect(lits.run('let l := []; shift(l); l')).toEqual([])
      expect(lits.run('shift("Albert")')).toBe('lbert')
      expect(lits.run('shift("1")')).toBe('')
      expect(lits.run('shift("")')).toBe('')

      expect(() => lits.run('shift(object())')).toThrow(LitsError)
      expect(() => lits.run('shift(null)')).toThrow(LitsError)
      expect(() => lits.run('shift(true)')).toThrow(LitsError)
      expect(() => lits.run('shift(false)')).toThrow(LitsError)
      expect(() => lits.run('shift(1)')).toThrow(LitsError)
      expect(() => lits.run('shift()')).toThrow(LitsError)
    })
  })

  describe('take', () => {
    it('samples', () => {
      expect(lits.run('take([1, 2, 3], 2)')).toEqual([1, 2])
      expect(lits.run('take([], 2)')).toEqual([])
      expect(lits.run('take([1, 2, 3], 20)')).toEqual([1, 2, 3])
      expect(lits.run('take([1, 2, 3], 0)')).toEqual([])
      expect(lits.run('take("Albert", 2)')).toEqual('Al')
      expect(lits.run('take("Albert", 2.01)')).toEqual('Alb')

      expect(() => lits.run('take({},)')).toThrow(LitsError)
      expect(() => lits.run('take(null, 1)')).toThrow(LitsError)
      expect(() => lits.run('take(true 1)')).toThrow(LitsError)
      expect(() => lits.run('take(false 1)')).toThrow(LitsError)
      expect(() => lits.run('take("Hej", "1")')).toThrow(LitsError)
      expect(() => lits.run('take()')).toThrow(LitsError)
      expect(() => lits.run('take([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('take([1, 2, 3], 1, 2)')).toThrow(LitsError)
    })

    it('new array created', () => {
      const program = `
        let l1 := [1, 2, 3];
        let l2 := take(l1, 2);
        l1 != l2
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('take-last', () => {
    it('samples', () => {
      expect(lits.run('take-last([1, 2, 3], 2)')).toEqual([2, 3])
      expect(lits.run('take-last([1, 2, 3], 20)')).toEqual([1, 2, 3])
      expect(lits.run('take-last([1, 2, 3], 0)')).toEqual([])
      expect(lits.run('take-last([1, 2, 3], 0.01)')).toEqual([3])

      expect(() => lits.run('take-last(object())')).toThrow(LitsError)
      expect(() => lits.run('take-last(null)')).toThrow(LitsError)
      expect(() => lits.run('take-last(true)')).toThrow(LitsError)
      expect(() => lits.run('take-last(false)')).toThrow(LitsError)
      expect(() => lits.run('take-last("1")')).toThrow(LitsError)
      expect(() => lits.run('take-last()')).toThrow(LitsError)
      expect(() => lits.run('take-last([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('take-last([1, 2, 3], 1, 2)')).toThrow(LitsError)
    })

    it('new array created', () => {
      const program = `
        let l1 := [1, 2, 3];
        let l2 := take-last(l1, 2);
        l1 != l2
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('take-while', () => {
    it('samples', () => {
      expect(lits.run('take-while([1, 2, 3, 2, 1], -> $ < 3)')).toEqual([1, 2])
      expect(lits.run('take-while([1, 2, 3, 2, 1], -> $ > 3)')).toEqual([])
      expect(lits.run('take-while("abcdabcd", -> $ <= "c")')).toEqual('abc')

      expect(() => lits.run('take-while({}, -> $ < 3))')).toThrow(LitsError)
      expect(() => lits.run('take-while(null, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('take-while(true, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('take-while(false, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('take-while([1, 2, 3], 10)')).toThrow(LitsError)
      expect(() => lits.run('take-while()')).toThrow(LitsError)
      expect(() => lits.run('take-while([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('take-while([1, 2, 3], -> $ < 3 1)')).toThrow(LitsError)
    })
    it('new array created', () => {
      const program = `
        let l1 := [1, 2, 3];
        let l2 := take-while(l1, -> $ < 3);
        l1 != l2
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('drop', () => {
    it('samples', () => {
      expect(lits.run('drop([1, 2, 3], 2)')).toEqual([3])
      expect(lits.run('drop([1, 2, 3], 20)')).toEqual([])
      expect(lits.run('drop([1, 2, 3], 0)')).toEqual([1, 2, 3])
      expect(lits.run('drop("Albert", 2)')).toEqual('bert')
      expect(lits.run('drop([1, 2, 3], 0.5)')).toEqual([2, 3])
      expect(lits.run('drop("Albert", -2)')).toEqual('Albert')

      expect(() => lits.run('drop({},)')).toThrow(LitsError)
      expect(() => lits.run('drop(null, 1)')).toThrow(LitsError)
      expect(() => lits.run('drop(true 1)')).toThrow(LitsError)
      expect(() => lits.run('drop(false 1)')).toThrow(LitsError)
      expect(() => lits.run('drop("Hej", "1")')).toThrow(LitsError)
      expect(() => lits.run('drop()')).toThrow(LitsError)
      expect(() => lits.run('drop([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('drop([1, 2, 3], 1, 2)')).toThrow(LitsError)
    })

    it('new array created', () => {
      const program = `
        let l1 := [1, 2, 3];
        let l2 := drop(l1, 2);
        l1 != l2
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('drop-last', () => {
    it('samples', () => {
      expect(lits.run('drop-last([1, 2, 3], 2)')).toEqual([1])
      expect(lits.run('drop-last([1, 2, 3], 20)')).toEqual([])
      expect(lits.run('drop-last([1, 2, 3], 0)')).toEqual([1, 2, 3])
      expect(lits.run('drop-last("Albert", 2)')).toEqual('Albe')
      expect(lits.run('drop-last([1, 2, 3], 0.5)')).toEqual([1, 2])
      expect(lits.run('drop-last("Albert", -2)')).toEqual('Albert')

      expect(() => lits.run('drop-last({},)')).toThrow(LitsError)
      expect(() => lits.run('drop-last(null, 1)')).toThrow(LitsError)
      expect(() => lits.run('drop-last(true 1)')).toThrow(LitsError)
      expect(() => lits.run('drop-last(false 1)')).toThrow(LitsError)
      expect(() => lits.run('drop-last("Hej", "1")')).toThrow(LitsError)
      expect(() => lits.run('drop-last()')).toThrow(LitsError)
      expect(() => lits.run('drop-last([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('drop-last([1, 2, 3], 1, 2)')).toThrow(LitsError)
    })
  })

  describe('drop-while', () => {
    it('samples', () => {
      expect(lits.run('drop-while([1, 2, 3, 2, 1], -> $ < 3)')).toEqual([3, 2, 1])
      expect(lits.run('drop-while([1, 2, 3, 2, 1], -> $ > 3)')).toEqual([1, 2, 3, 2, 1])
      expect(lits.run('drop-while("abcdab", -> $ <= "c")')).toEqual('dab')

      expect(() => lits.run('drop-while({}, -> $ < 3))')).toThrow(LitsError)
      expect(() => lits.run('drop-while(null, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('drop-while(true, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('drop-while(false, -> $ < 3)')).toThrow(LitsError)
      expect(() => lits.run('drop-while([1, 2, 3], 10)')).toThrow(LitsError)
      expect(() => lits.run('drop-while()')).toThrow(LitsError)
      expect(() => lits.run('drop-while([1, 2, 3])')).toThrow(LitsError)
      expect(() => lits.run('drop-while([1, 2, 3], -> $ < 3 1)')).toThrow(LitsError)
    })
    it('new array created', () => {
      const program = `
        let l1 := [1, 2, 3];
        let l2 := take-while(l1, -> $ < 3);
        l1 != l2
      `
      expect(lits.run(program)).toBe(true)
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

  describe('distinct', () => {
    it('samples', () => {
      expect(lits.run('distinct([1, 2, 3, 1, 3, 5])')).toEqual([1, 2, 3, 5])
      expect(lits.run('distinct([])')).toEqual([])
      expect(lits.run('distinct("Albert Mojir")')).toBe('Albert Moji')
      expect(lits.run('distinct("")')).toBe('')
      expect(() => lits.run('distinct()')).toThrow(LitsError)
      expect(() => lits.run('distinct([], [])')).toThrow(LitsError)
    })
  })

  describe('remove', () => {
    it('samples', () => {
      expect(lits.run('remove([1, 2, 3, 1, 3, 5], even?)')).toEqual([1, 3, 1, 3, 5])
      expect(lits.run('remove("Albert Mojir", -> contains?("aoueiyAOUEIY", $1))')).toBe('lbrt Mjr')
      expect(() => lits.run('remove()')).toThrow(LitsError)
      expect(() => lits.run('remove("Albert Mojir")')).toThrow(LitsError)
      expect(() => lits.run('remove(=> contains?("aoueiyAOUEIY", $1))')).toThrow(LitsError)
      expect(() => lits.run('remove("Albert", => contains?("aoueiyAOUEIY", $1) "Mojir")')).toThrow(LitsError)
    })
  })

  describe('remove-at', () => {
    it('samples', () => {
      expect(lits.run('remove-at([1, 2, 3, 4, 5], -1)')).toEqual([1, 2, 3, 4])
      expect(lits.run('remove-at([1, 2, 3, 4, 5], 0)')).toEqual([2, 3, 4, 5])
      expect(lits.run('remove-at([1, 2, 3, 4, 5], 2)')).toEqual([1, 2, 4, 5])
      expect(lits.run('remove-at([1, 2, 3, 4, 5], 4)')).toEqual([1, 2, 3, 4])
      expect(lits.run('remove-at([1, 2, 3, 4, 5], 5)')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('remove-at("Mojir", -1)')).toEqual('Moji')
      expect(lits.run('remove-at("Mojir", 0)')).toEqual('ojir')
      expect(lits.run('remove-at("Mojir", 2)')).toEqual('Moir')
      expect(lits.run('remove-at("Mojir", 4)')).toEqual('Moji')
      expect(lits.run('remove-at("Mojir", 5)')).toEqual('Mojir')
      expect(() => lits.run('remove-at()')).toThrow(LitsError)
      expect(() => lits.run('remove-at("Albert Mojir")')).toThrow(LitsError)
      expect(() => lits.run('remove-at(1)')).toThrow(LitsError)
      expect(() => lits.run('remove-at("Albert", 1, 2')).toThrow(LitsError)
    })
  })

  describe('split-at', () => {
    it('samples', () => {
      expect(lits.run('split-at([1, 2, 3, 4, 5], 2)')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('split-at([1, 2, 3, 4, 5], 0)')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('split-at([1, 2, 3, 4, 5], -1)')).toEqual([[1, 2, 3, 4], [5]])
      expect(lits.run('split-at([1, 2, 3, 4, 5], 100)')).toEqual([[1, 2, 3, 4, 5], []])
      expect(lits.run('split-at("Albert", 2)')).toEqual(['Al', 'bert'])
      expect(lits.run('split-at("Albert", 0)')).toEqual(['', 'Albert'])
      expect(lits.run('split-at("Albert", -2)')).toEqual(['Albe', 'rt'])
      expect(lits.run('split-at("Albert", 100)')).toEqual(['Albert', ''])

      expect(() => lits.run('split-at([1, 2, 3, 4, 5], 0.01)')).toThrow(LitsError)
      expect(() => lits.run('split-at()')).toThrow(LitsError)
      expect(() => lits.run('split-at(3)')).toThrow(LitsError)
      expect(() => lits.run('split-at("Albert", 3 "Mojir")')).toThrow(LitsError)
    })
  })

  describe('split-with', () => {
    it('samples', () => {
      expect(lits.run('split-with([1, 2, 3, 4, 5], -> $1 < 3)')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('split-with([1, 2, 3, 4, 5], -> $1 > 3)')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('split-with([1, 2, 3, 4, 5], -> $1 < 10)')).toEqual([[1, 2, 3, 4, 5], []])

      expect(lits.run('split-with("Albert", -> $1 <= "Z")')).toEqual(['A', 'lbert'])
      expect(lits.run('split-with("Albert", -> $1 > "Z")')).toEqual(['', 'Albert'])
      expect(lits.run('split-with("Albert", -> $1 <= "z")')).toEqual(['Albert', ''])

      expect(() => lits.run('split-with()')).toThrow(LitsError)
      expect(() => lits.run('split-with(-> $1 <= "Z")')).toThrow(LitsError)
      expect(() => lits.run('split-with("Albert", -> $1 <= "Z", "Mojir")')).toThrow(LitsError)
    })
  })

  describe('frequencies', () => {
    it('samples', () => {
      expect(lits.run('frequencies(["Albert", "Mojir", "Nina", "Mojir"])')).toEqual({ Albert: 1, Nina: 1, Mojir: 2 })
      expect(lits.run('frequencies("Pneumonoultramicroscopicsilicovolcanoconiosis")')).toEqual({
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
      expect(() => lits.run('frequencies()')).toThrow(LitsError)
      expect(() => lits.run('frequencies({})')).toThrow(LitsError)
      expect(() => lits.run('frequencies(3)')).toThrow(LitsError)
      expect(() => lits.run('frequencies("", "")')).toThrow(LitsError)
    })
  })

  describe('group-by', () => {
    it('samples', () => {
      expect(lits.run('group-by([{name := "Albert"}, {name := "Albert"}, {name := "Mojir"}], "name")')).toEqual({
        Albert: [{ name: 'Albert' }, { name: 'Albert' }],
        Mojir: [{ name: 'Mojir' }],
      })
      expect(lits.run('group-by("Albert Mojir", -> if "aoueiAOUEI" contains? $ then "vowel" else "other" end)')).toEqual({
        other: ['l', 'b', 'r', 't', ' ', 'M', 'j', 'r'],
        vowel: ['A', 'e', 'o', 'i'],
      })
      expect(() => lits.run('group-by()')).toThrow(LitsError)
      expect(() => lits.run('group-by("a")')).toThrow(LitsError)
      expect(() => lits.run('group-by("a" {})')).toThrow(LitsError)
      expect(() => lits.run('group-by("a" 3)')).toThrow(LitsError)
      expect(() => lits.run('group-by("", "a", "")')).toThrow(LitsError)
    })
  })

  describe('sort-by', () => {
    it('samples', () => {
      expect(lits.run('sort-by(["Albert", "Mojir", "Nina"], count)')).toEqual(['Nina', 'Mojir', 'Albert'])
      expect(lits.run('sort-by(["Albert", "Mojir", "Nina"], count, (a, b) -> b - a)')).toEqual([
        'Albert',
        'Mojir',
        'Nina',
      ])
      expect(lits.run('sort-by("Albert", lower-case)')).toEqual('Abelrt')
      expect(lits.run('sort-by("Albert", lower-case, (a, b) -> to-char-code(b) - to-char-code(a))')).toEqual(
        'trlebA',
      )
      expect(() => lits.run('sort-by()')).toThrow(LitsError)
      expect(() => lits.run('sort-by("a")')).toThrow(LitsError)
      expect(() => lits.run('sort-by({} "a")')).toThrow(LitsError)
      expect(() => lits.run('sort-by(3 "a")')).toThrow(LitsError)
    })
  })

  describe('partition', () => {
    it('samples', () => {
      expect(lits.run('partition(range(20), 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('partition(range(22), 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('partition(range(20), 4, 6)')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
      ])
      expect(lits.run('partition(range(20), 4, 3)')).toEqual([
        [0, 1, 2, 3],
        [3, 4, 5, 6],
        [6, 7, 8, 9],
        [9, 10, 11, 12],
        [12, 13, 14, 15],
        [15, 16, 17, 18],
      ])
      expect(lits.run('partition(range(20), 3, 6, ["a"])')).toEqual([
        [0, 1, 2],
        [6, 7, 8],
        [12, 13, 14],
        [18, 19, 'a'],
      ])
      expect(lits.run('partition(range(20), 4, 6, ["a"])')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a'],
      ])
      expect(lits.run('partition(range(20), 4, 6, ["a", "b", "c", "d"])')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a', 'b'],
      ])
      expect(lits.run('partition(["a", "b", "c", "d", "e", "f"], 3, 1)')).toEqual([
        ['a', 'b', 'c'],
        ['b', 'c', 'd'],
        ['c', 'd', 'e'],
        ['d', 'e', 'f'],
      ])
      expect(lits.run('partition([1, 2, 3, 4], 10)')).toEqual([])
      expect(lits.run('partition([1, 2, 3, 4], 10, 10)')).toEqual([])
      expect(lits.run('partition([1, 2, 3, 4], 10, 10, [])')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('partition([1, 2, 3, 4], 10, 10, null)')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('partition("superfragilistic", 5)')).toEqual(['super', 'fragi', 'listi'])
      expect(lits.run('partition("superfragilistic", 5, 5, null)')).toEqual(['super', 'fragi', 'listi', 'c'])
      expect(lits.run('let foo := [5, 6, 7, 8]; partition(foo, 2, 1, foo)')).toEqual([
        [5, 6],
        [6, 7],
        [7, 8],
        [8, 5],
      ])
      expect(() => lits.run('partition[1, 2, 3, 4], 0)')).toThrow(LitsError)
      expect(() => lits.run('partition1)')).toThrow(LitsError)
      expect(() => lits.run('partition[1])')).toThrow(LitsError)
    })
  })

  describe('partition-all', () => {
    it('samples', () => {
      expect(lits.run('partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9],
      ])
      expect(lits.run('partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 2, 4)')).toEqual([
        [0, 1],
        [4, 5],
        [8, 9],
      ])
      expect(() => lits.run('partition-all(1)')).toThrow(LitsError)
      expect(() => lits.run('partition-all([1])')).toThrow(LitsError)
    })
  })

  describe('partition-by', () => {
    it('samples', () => {
      expect(lits.run('partition-by([1, 2, 3, 4, 5], -> 3 = $1)')).toEqual([[1, 2], [3], [4, 5]])
      expect(lits.run('partition-by([1, 1, 1, 2, 2, 3, 3], odd?)')).toEqual([
        [1, 1, 1],
        [2, 2],
        [3, 3],
      ])
      expect(lits.run('partition-by("Leeeeeerrroyyy", identity)')).toEqual(['L', 'eeeeee', 'rrr', 'o', 'yyy'])
      expect(() => lits.run('partition-by(odd?)')).toThrow(LitsError)
      expect(() => lits.run('partition-by([1, 2, 3])')).toThrow(LitsError)
    })
  })

  describe('starts-with?', () => {
    it('samples', () => {
      expect(lits.run('starts-with?([1, 2, 3], 1)')).toBe(true)
      expect(lits.run('starts-with?([1, 2, 3], 2)')).toBe(false)
      expect(lits.run('starts-with?([1, 2, 3], [1])')).toBe(false)

      expect(lits.run('starts-with?("Albert", "Al")')).toBe(true)
      expect(lits.run('starts-with?("Albert", "al")')).toBe(false)
      expect(lits.run('starts-with?("Albert", "")')).toBe(true)
      expect(lits.run('starts-with?("", "")')).toBe(true)
      expect(lits.run('starts-with?("Albert", "Albert")')).toBe(true)
      expect(lits.run('starts-with?("Albert", "Albert ")')).toBe(false)
      expect(() => lits.run('starts-with?("Albert", "foo", 2)')).toThrow(LitsError)
      expect(() => lits.run('starts-with?("Albert")')).toThrow(LitsError)
      expect(() => lits.run('starts-with?(')).toThrow(LitsError)
    })
  })

  describe('ends-with?', () => {
    it('samples', () => {
      expect(lits.run('ends-with?([1, 2, 3], 3)')).toBe(true)
      expect(lits.run('ends-with?([1, 2, 3], 2)')).toBe(false)
      expect(lits.run('ends-with?([1, 2, 3], [3])')).toBe(false)

      expect(lits.run('ends-with?("Albert", "rt")')).toBe(true)
      expect(lits.run('ends-with?("Albert", "RT")')).toBe(false)
      expect(lits.run('ends-with?("Albert", "")')).toBe(true)
      expect(lits.run('ends-with?("", "")')).toBe(true)
      expect(lits.run('ends-with?("Albert", "Albert")')).toBe(true)
      expect(lits.run('ends-with?("Albert", ", Albert")')).toBe(false)
      expect(() => lits.run('ends-with?("Albert", "foo", 2)')).toThrow(LitsError)
      expect(() => lits.run('ends-with?("Albert")')).toThrow(LitsError)
      expect(() => lits.run('ends-with?()')).toThrow(LitsError)
    })
  })
  describe('interleave', () => {
    it('samples', () => {
      expect(lits.run('interleave([1, 2, 3], [4, 5, 6])')).toEqual([1, 4, 2, 5, 3, 6])
      expect(lits.run('interleave([1, 2, 3], [4, 5, 6], [7, 8, 9])')).toEqual([1, 4, 7, 2, 5, 8, 3, 6, 9])
      expect(lits.run('interleave([1, 2, 3], [4, 5, 6], [7, 8])')).toEqual([1, 4, 7, 2, 5, 8])
      expect(lits.run('interleave([1, 2, 3], [4, 5, 6], [7])')).toEqual([1, 4, 7])
      expect(lits.run('interleave([1, 2, 3], [4, 5, 6], [7], [8, 9])')).toEqual([1, 4, 7, 8])
      expect(lits.run('interleave([], [4, 5, 6], [7], [8, 9])')).toEqual([])
      expect(lits.run('interleave("Albert", "Mojir")')).toEqual('AMlobjeirr')

      expect(() => lits.run('interleave()')).toThrow(LitsError)
      expect(() => lits.run('interleave(1)')).toThrow(LitsError)
      expect(() => lits.run('interleave([1, 2, 3], "asd")')).toThrow(LitsError)
    })
  })
  describe('interpose', () => {
    it('samples', () => {
      expect(lits.run('interpose([1, 2, 3, 4], "a")')).toEqual([1, 'a', 2, 'a', 3, 'a', 4])
      expect(lits.run('interpose([1, 2, 3], "a")')).toEqual([1, 'a', 2, 'a', 3])
      expect(lits.run('interpose([1], "a")')).toEqual([1])
      expect(lits.run('interpose([], "a")')).toEqual([])
      expect(lits.run('interpose("Albert", ":")')).toEqual('A:l:b:e:r:t')
      expect(() => lits.run('interpose()')).toThrow(LitsError)
      expect(() => lits.run('interpose(1)')).toThrow(LitsError)
      expect(() => lits.run('interpose("a", 1)')).toThrow(LitsError)
    })
  })
})
