import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'

const lits = new Lits({ polish: true })
const litsInstances = [lits, new Lits({ debug: true, polish: true })]
describe('sequence functions', () => {
  describe('nth', () => {
    it('array samples', () => {
      for (const lits2 of litsInstances) {
        expect(lits2.run('(nth [1 2 3] 1)')).toBe(2)
        expect(lits2.run('(nth [] 0)')).toBeNull()
        expect(lits2.run('(nth [1 2 3] 3)')).toBeNull()
        expect(lits2.run('(nth [1 2 3] -1)')).toBeNull()
        expect(lits2.run('(nth [1 2 3] -4)')).toBeNull()
        expect(() => lits2.run('(nth)')).toThrow()
        expect(() => lits2.run('(nth (object) 1)')).toThrow()
        expect(() => lits2.run('(nth [1 2 3])')).toThrow()
        expect(() => lits2.run('(nth [1 2 3] 1 2 3)')).toThrow()
      }
    })

    it('string samples', () => {
      expect(lits.run('(nth "A string" 1)')).toBe(' ')
      expect(lits.run('(nth "A string" 3)')).toBe('t')
      expect(lits.run('(nth "A string" -3)')).toBeNull()
      expect(lits.run('(nth "A string" 30)')).toBeNull()
      expect(lits.run('(nth "A string" -30)')).toBeNull()
      expect(() => lits.run('(nth "A string")')).toThrow()
      expect(() => lits.run('(nth "A string" 1 2 3)')).toThrow()
    })

    it('default values', () => {
      expect(lits.run('(nth [1 2 3] 1 99)')).toBe(2)
      expect(lits.run('(nth [1 2 3] 3 99)')).toBe(99)
      expect(lits.run('(nth [1 2 3] -1 99)')).toBe(99)
      expect(lits.run('(nth [1 2 3] -4 99)')).toBe(99)
      expect(lits.run('(nth "A string" 1 99)')).toBe(' ')
      expect(lits.run('(nth "A string" 3 99)')).toBe('t')
      expect(lits.run('(nth "A string" -3 99)')).toBe(99)
      expect(lits.run('(nth "A string" 30 99)')).toBe(99)
      expect(lits.run('(nth "A string" -30 99)')).toBe(99)
    })

    it('null sequence', () => {
      expect(lits.run('(nth null 0)')).toBeNull()
      expect(lits.run('(nth null 0 99)')).toBe(99)
    })
  })

  describe('slice', () => {
    it('samples', () => {
      expect(lits.run('(slice [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(slice [1 2 3] 0)')).toEqual([1, 2, 3])
      expect(lits.run('(slice [1 2 3] 1)')).toEqual([2, 3])
      expect(lits.run('(slice [1 2 3] -1)')).toEqual([3])
      expect(lits.run('(slice [1 2 3] -3)')).toEqual([1, 2, 3])
      expect(lits.run('(slice [1 2 3] -4)')).toEqual([1, 2, 3])
      expect(lits.run('(slice [1 2 3] 3)')).toEqual([])
      expect(lits.run('(slice [1 2 3] 4)')).toEqual([])
      expect(lits.run('(slice [1 2 3] 0 0)')).toEqual([])
      expect(lits.run('(slice [1 2 3] 0 1)')).toEqual([1])
      expect(lits.run('(slice [1 2 3] 0 10)')).toEqual([1, 2, 3])
      expect(lits.run('(slice [1 2 3] 0 -1)')).toEqual([1, 2])

      expect(lits.run('(slice "Albert")')).toBe('Albert')
      expect(lits.run('(slice "Albert" 0)')).toBe('Albert')
      expect(lits.run('(slice "Albert" 1)')).toBe('lbert')
      expect(lits.run('(slice "Albert" -1)')).toBe('t')
      expect(lits.run('(slice "Albert" -3)')).toBe('ert')
      expect(lits.run('(slice "Albert" -4)')).toBe('bert')
      expect(lits.run('(slice "Albert" -5)')).toBe('lbert')
      expect(lits.run('(slice "Albert" -6)')).toBe('Albert')
      expect(lits.run('(slice "Albert" -7)')).toBe('Albert')
      expect(lits.run('(slice "Albert" 4)')).toBe('rt')
      expect(lits.run('(slice "Albert" 5)')).toBe('t')
      expect(lits.run('(slice "Albert" 6)')).toBe('')
      expect(lits.run('(slice "Albert" 0 0)')).toBe('')
      expect(lits.run('(slice "Albert" 0 1)')).toBe('A')
      expect(lits.run('(slice "Albert" 0 10)')).toBe('Albert')
      expect(lits.run('(slice "Albert" 0 -1)')).toBe('Alber')

      expect(() => lits.run('(slice [1 2 3] 1 2 3)')).toThrow()
      expect(() => lits.run('(slice)')).toThrow()
      expect(() => lits.run('(slice (object) 1)')).toThrow()
      expect(() => lits.run('(slice null 2)')).toThrow()
    })
  })

  describe('reductions', () => {
    it('samples', () => {
      expect(lits.run('(reductions [1 2 3 4 5] +)')).toEqual([1, 3, 6, 10, 15])
      expect(lits.run('(reductions [] +)')).toEqual([0])
      expect(lits.run('(reductions [1] +)')).toEqual([1])
      expect(lits.run('(reductions [1 2] +)')).toEqual([1, 3])
      expect(lits.run('(reductions [] + 1)')).toEqual([1])
      expect(lits.run('(reductions [2 3] + 1)')).toEqual([1, 3, 6])
      expect(lits.run('(reductions [1 2 3] + 0)')).toEqual([0, 1, 3, 6])
      expect(lits.run('(reductions [] + 0)')).toEqual([0])
      expect(lits.run('(reductions [] + 1)')).toEqual([1])

      expect(lits.run('(reductions "Albert" (fn [x y] (concat x "-" y)))')).toEqual([
        'A',
        'A-l',
        'A-l-b',
        'A-l-b-e',
        'A-l-b-e-r',
        'A-l-b-e-r-t',
      ])
      expect(lits.run('(reductions "Albert" (fn [x y] (concat x "-" y)) ">")')).toEqual([
        '>',
        '>-A',
        '>-A-l',
        '>-A-l-b',
        '>-A-l-b-e',
        '>-A-l-b-e-r',
        '>-A-l-b-e-r-t',
      ])
      expect(lits.run('(reductions "" (fn [x y] (concat x "-" y)) ">")')).toEqual(['>'])

      expect(() => lits.run('(reductions null +)')).toThrow()
      expect(() => lits.run('(reductions +)')).toThrow()
      expect(() => lits.run('(reductions)')).toThrow()
      expect(() => lits.run('(reductions 1 + 2)')).toThrow()
    })
  })

  describe('reduce', () => {
    it('samples', () => {
      let program = `
      (defn countChars [stringArray]
        (reduce
          stringArray
          (fn [sum str] (+ sum (count str)))
          0
        )
      )

      (countChars ["First" "Second" "Third"])
      `
      expect(lits.run(program)).toBe(16)

      program = `
      (defn longestLength [stringArray]
      (reduce
          stringArray
          (fn [sum str]
            (if (> sum (count str))
              sum
              (count str)
            )
          )
          0
        )
      )

      (longestLength ["First" "Second" "Third"])
      `
      expect(lits.run(program)).toBe(6)

      expect(lits.run('(reduce [1 2 3 4 5] +)')).toBe(15)
      expect(lits.run('(reduce [] +)')).toBe(0)
      expect(lits.run('(reduce [1] +)')).toBe(1)
      expect(lits.run('(reduce [1 2] +)')).toBe(3)
      expect(lits.run('(reduce [] + 1)')).toBe(1)
      expect(lits.run('(reduce [2 3] + 1)')).toBe(6)
      expect(lits.run('(reduce [1 2 3] + 0)')).toBe(6)
      expect(lits.run('(reduce [] + 0)')).toBe(0)
      expect(lits.run('(reduce [] + 1)')).toBe(1)

      expect(lits.run('(reduce "Albert" (fn [x y] (concat x "-" y)))')).toBe('A-l-b-e-r-t')
      expect(lits.run('(reduce "Albert" (fn [x y] (concat x "-" y)) ">")')).toBe('>-A-l-b-e-r-t')
      expect(lits.run('(reduce "" (fn [x y] (concat x "-" y)) ">")')).toBe('>')

      expect(() => lits.run('(reduce +)')).toThrow()
      expect(() => lits.run('(reduce)')).toThrow()
      expect(() => lits.run('(reduce 1 +2)')).toThrow()
    })
  })

  describe('reduce_right', () => {
    it('samples', () => {
      expect(lits.run('(reduce_right [1 2 3 4 5] +)')).toBe(15)
      expect(lits.run('(reduce_right [] +)')).toBe(0)
      expect(lits.run('(reduce_right [1] +)')).toBe(1)
      expect(lits.run('(reduce_right [1 2] +)')).toBe(3)
      expect(lits.run('(reduce_right [1 2 3] + 0)')).toBe(6)
      expect(lits.run('(reduce_right [] + 0)')).toBe(0)
      expect(lits.run('(reduce_right [] + 0)')).toBe(0)
      expect(lits.run('(reduce_right [:1 :2 :3] str "")')).toBe('321')
      expect(lits.run('(reduce_right [:1 :2 :3] str)')).toBe('321')

      expect(lits.run('(reduce_right "Albert" (fn [x y] (concat x "-" y)))')).toBe('t-r-e-b-l-A')
      expect(lits.run('(reduce_right "Albert" (fn [x y] (concat x "-" y)) ">")')).toBe('>-t-r-e-b-l-A')
      expect(lits.run('(reduce_right "" (fn [x y] (concat x "-" y)) ">")')).toBe('>')

      expect(() => lits.run('(reduce_right +)')).toThrow()
      expect(() => lits.run('(reduce_right)')).toThrow()
      expect(() => lits.run('(reduce_right 1 + 2)')).toThrow()
    })
  })
  describe('filter', () => {
    it('samples', () => {
      expect(lits.run('(filter [1 :2 3] number?)')).toEqual([1, 3])
      expect(lits.run('(filter [] number?)')).toEqual([])
      expect(lits.run('(filter [1 :2 3] null?)')).toEqual([])
      expect(lits.run('(filter [0 1 2 3 4 5 6 7] (fn [x] (zero? (mod x 3))))')).toEqual([0, 3, 6])
      expect(lits.run('(filter "aAbBcC" (fn [x] (>= x :a)))')).toBe('abc')
      expect(() => lits.run('(filter +)')).toThrow()
      expect(() => lits.run('(filter)')).toThrow()
      expect(() => lits.run('(filter [1] number? 2)')).toThrow()
    })
  })

  describe('position', () => {
    it('samples', () => {
      expect(lits.run('(position [:1 :2 3] number?)')).toEqual(2)
      expect(lits.run('(position [:1 :2 :3] number?)')).toBeNull()
      expect(lits.run('(position [] number?)')).toBeNull()
      expect(lits.run('(position null number?)')).toBeNull()
      expect(lits.run('(position [1 2 3 4 5 6 7] (fn [x] (zero? (mod x 3))))')).toEqual(2)
      expect(lits.run('(position "Aa" (fn [x] (>= x :a)))')).toBe(1)
      expect(lits.run('(position "Aa" (fn [x] (= x :z)))')).toBeNull()
      expect(() => lits.run('(position +)')).toThrow()
      expect(() => lits.run('(position)')).toThrow()
      expect(() => lits.run('(position [1] number? 2)')).toThrow()
    })
  })

  describe('index_of', () => {
    it('samples', () => {
      expect(lits.run('(index_of [:1 :2 3] :2)')).toEqual(1)
      expect(lits.run('(index_of [:1 :2 :3] :4)')).toBeNull()
      expect(lits.run('(index_of [] 1)')).toBeNull()
      expect(lits.run('(index_of null 1)')).toBeNull()
      expect(lits.run('(index_of "AlbertAlbert" :l)')).toBe(1)
      expect(lits.run('(index_of "Albert" "ert")')).toBe(3)
      expect(lits.run('(index_of "Albert" :z)')).toBeNull()
      expect(lits.run('(index_of [1] 2)')).toBeNull()
      expect(() => lits.run('(index_of +)')).toThrow()
      expect(() => lits.run('(index_of)')).toThrow()
    })
  })

  describe('last_index_of', () => {
    it('samples', () => {
      expect(lits.run('(last_index_of [:1 :2 3] :2)')).toEqual(1)
      expect(lits.run('(last_index_of [:1 :2 :3] :4)')).toBeNull()
      expect(lits.run('(last_index_of [] 1)')).toBeNull()
      expect(lits.run('(last_index_of null 1)')).toBeNull()
      expect(lits.run('(last_index_of "AlbertAlbert" :l)')).toBe(7)
      expect(lits.run('(last_index_of "Albert" "ert")')).toBe(3)
      expect(lits.run('(last_index_of "Albert" :z)')).toBeNull()
      expect(lits.run('(last_index_of [1] 2)')).toBeNull()
      expect(() => lits.run('(last_index_of +)')).toThrow()
      expect(() => lits.run('(last_index_of)')).toThrow()
    })
  })

  describe('some', () => {
    it('samples', () => {
      expect(lits.run('(some :Albert #(= :l %))')).toBe('l')

      expect(lits.run('(some null number?)')).toBeNull()
      expect(lits.run('(some [:1 :2 3] number?)')).toBe(3)
      expect(lits.run('(some [:1 :2 :3] number?)')).toBeNull()
      expect(lits.run('(some [] number?)')).toBeNull()
      expect(lits.run('(some [1 2 3 4 5 6 7] (fn [x] (zero? (mod x 3))))')).toBe(3)

      expect(lits.run('(some "Aa" (fn [x] (>= x :a)))')).toBe('a')
      expect(lits.run('(some "Aa" (fn [x] (>= x :z)))')).toBeNull()

      expect(() => lits.run('(some +)')).toThrow()
      expect(() => lits.run('(some)')).toThrow()
      expect(() => lits.run('(some [1] number? 2)')).toThrow()
    })
  })

  describe('map', () => {
    it('samples', () => {
      expect(lits.run('(map [1 :2 3] number?)')).toEqual([true, false, true])
      expect(lits.run('(map [] number?)')).toEqual([])
      expect(lits.run('(map [1 2 3] #(* 2 %))')).toEqual([2, 4, 6])
      expect(lits.run('(map "AaBbCc" (fn [x] (if (>= x :a) "-" "+")))')).toBe('+-+-+-')
      expect(() => lits.run('(map "AaBbCc" (fn [x] (if (>= x :a) 0 1)))')).toThrow()
      expect(lits.run('(map [1 :2 3] null?)')).toEqual([false, false, false])
      expect(lits.run('(map [0 1 2 3 4 5 6 7] (fn [x] (zero? (mod x 3))))')).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lits.run('(map [0 1 2 3 4 5 6 7] inc)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(() => lits.run('(map [1 2 3] [1 2] +)')).toThrow()
      expect(() => lits.run('(map +)')).toThrow()
      expect(() => lits.run('(map)')).toThrow()
      expect(() => lits.run('(map 1 number?)')).toThrow()
    })
  })

  describe('first', () => {
    it('samples', () => {
      expect(lits.run('(first [1 2 3])')).toEqual(1)
      expect(lits.run('(first [:1])')).toEqual('1')
      expect(lits.run('(first [])')).toBeNull()
      expect(lits.run('(first "AB")')).toBe('A')
      expect(lits.run('(first :A)')).toBe('A')
      expect(lits.run('(first "")')).toBeNull()
      expect(lits.run('(first null)')).toBeNull()

      expect(() => lits.run('(first')).toThrow()
      expect(() => lits.run('(first true)')).toThrow()
      expect(() => lits.run('(first false)')).toThrow()
      expect(() => lits.run('(first (object))')).toThrow()
      expect(() => lits.run('(first 10)')).toThrow()
    })
  })

  describe('second', () => {
    it('samples', () => {
      expect(lits.run('(second [1 2 3])')).toEqual(2)
      expect(lits.run('(second [:1])')).toBeNull()
      expect(lits.run('(second [])')).toBeNull()

      expect(lits.run('(second "ABC")')).toBe('B')
      expect(lits.run('(second "AB")')).toBe('B')
      expect(lits.run('(second :A)')).toBeNull()
      expect(lits.run('(second "")')).toBeNull()

      expect(lits.run('(second null)')).toBeNull()

      expect(() => lits.run('(second')).toThrow()
      expect(() => lits.run('(second true)')).toThrow()
      expect(() => lits.run('(second false)')).toThrow()
      expect(() => lits.run('(second (object))')).toThrow()
      expect(() => lits.run('(second 10)')).toThrow()
    })
  })

  describe('reverse', () => {
    it('samples', () => {
      expect(lits.run('(reverse [1 2 3])')).toEqual([3, 2, 1])
      expect(lits.run('(reverse [:1])')).toEqual(['1'])
      expect(lits.run('(reverse [])')).toEqual([])
      expect(lits.run('(reverse "albert")')).toBe('trebla')
      expect(lits.run('(reverse "A 1")')).toBe('1 A')
      expect(lits.run('(reverse "")')).toBe('')

      expect(lits.run('(reverse null)')).toBeNull()

      expect(() => lits.run('(reverse)')).toThrow()
      expect(() => lits.run('(reverse "word1" "word2")')).toThrow()
      expect(() => lits.run('(reverse')).toThrow()
      expect(() => lits.run('(reverse true)')).toThrow()
      expect(() => lits.run('(reverse false)')).toThrow()
      expect(() => lits.run('(reverse (object))')).toThrow()
      expect(() => lits.run('(reverse 10)')).toThrow()
    })
    it('returns a new array instance', () => {
      const program = `
        (def l [1 2 3])
        (!= l (reverse l))
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('last', () => {
    it('samples', () => {
      expect(lits.run('(last [1 2 3])')).toEqual(3)
      expect(lits.run('(last [:1])')).toEqual('1')
      expect(lits.run('(last [])')).toBeNull()
      expect(lits.run('(last "Albert")')).toBe('t')
      expect(lits.run('(last :1)')).toBe('1')
      expect(lits.run('(last "")')).toBeNull()

      expect(lits.run('(last null)')).toBeNull()

      expect(() => lits.run('(last')).toThrow()
      expect(() => lits.run('(last true)')).toThrow()
      expect(() => lits.run('(last false)')).toThrow()
      expect(() => lits.run('(last (object))')).toThrow()
      expect(() => lits.run('(last 10)')).toThrow()
    })
  })

  describe('rest', () => {
    it('samples', () => {
      expect(lits.run('(rest [1 2 3])')).toEqual([2, 3])
      expect(lits.run('(rest [1 2])')).toEqual([2])
      expect(lits.run('(rest [:1])')).toEqual([])
      expect(lits.run('(rest [])')).toEqual([])
      expect(lits.run('(rest "Albert")')).toEqual('lbert')
      expect(lits.run('(rest :A)')).toEqual('')
      expect(lits.run('(rest "")')).toEqual('')

      expect(() => lits.run('(rest')).toThrow()
      expect(() => lits.run('(rest true)')).toThrow()
      expect(() => lits.run('(rest false)')).toThrow()
      expect(() => lits.run('(rest null)')).toThrow()
      expect(() => lits.run('(rest (object))')).toThrow()
      expect(() => lits.run('(rest 10)')).toThrow()
    })
  })

  describe('nthrest', () => {
    it('samples', () => {
      expect(lits.run('(nthrest [1 2 3 4 5 6 7 8 9] 4)')).toEqual([5, 6, 7, 8, 9])
      expect(lits.run('(nthrest [1 2 3 4 5 6 7 8 9] 4.1)')).toEqual([6, 7, 8, 9])
      expect(lits.run('(nthrest [1 2 3 4 5 6 7 8 9] -1)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lits.run('(nthrest [1 2] 0)')).toEqual([1, 2])
      expect(lits.run('(nthrest [:1] 1)')).toEqual([])
      expect(lits.run('(nthrest [] 0)')).toEqual([])
      expect(lits.run('(nthrest "Albert" 3)')).toBe('ert')
      expect(lits.run('(nthrest :A 1)')).toBe('')
      expect(lits.run('(nthrest "" 0)')).toBe('')

      expect(() => lits.run('(nthrest [1 2 3]')).toThrow()
      expect(() => lits.run('(nthrest [1 2 3] :1')).toThrow()
      expect(() => lits.run('(nthrest [1 2 3] null')).toThrow()
      expect(() => lits.run('(nthrest [1 2 3] {}')).toThrow()
      expect(() => lits.run('(nthrest true)')).toThrow()
      expect(() => lits.run('(nthrest false)')).toThrow()
      expect(() => lits.run('(nthrest null)')).toThrow()
      expect(() => lits.run('(nthrest (object))')).toThrow()
      expect(() => lits.run('(nthrest 10)')).toThrow()
    })
  })

  describe('next', () => {
    it('samples', () => {
      expect(lits.run('(next [1 2 3])')).toEqual([2, 3])
      expect(lits.run('(next [1 2])')).toEqual([2])
      expect(lits.run('(next [:1])')).toBeNull()
      expect(lits.run('(next [])')).toBeNull()
      expect(lits.run('(next "Albert")')).toEqual('lbert')
      expect(lits.run('(next :A)')).toBeNull()
      expect(lits.run('(next "")')).toBeNull()

      expect(() => lits.run('(next')).toThrow()
      expect(() => lits.run('(next true)')).toThrow()
      expect(() => lits.run('(next false)')).toThrow()
      expect(() => lits.run('(next null)')).toThrow()
      expect(() => lits.run('(next (object))')).toThrow()
      expect(() => lits.run('(next 10)')).toThrow()
    })
  })

  describe('nthnext', () => {
    it('samples', () => {
      expect(lits.run('(nthnext [1 2 3 4 5 6 7 8 9] 4)')).toEqual([5, 6, 7, 8, 9])
      expect(lits.run('(nthnext [1 2 3 4 5 6 7 8 9] 10)')).toBeNull()
      expect(lits.run('(nthnext [1 2 3 4 5 6 7 8 9] 4.1)')).toEqual([6, 7, 8, 9])
      expect(lits.run('(nthnext [1 2 3 4 5 6 7 8 9] -1)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lits.run('(nthnext [1 2] 0)')).toEqual([1, 2])
      expect(lits.run('(nthnext [:1] 1)')).toBeNull()
      expect(lits.run('(nthnext [] 0)')).toBeNull()
      expect(lits.run('(nthnext "Albert" 3)')).toBe('ert')
      expect(lits.run('(nthnext :A 1)')).toBeNull()
      expect(lits.run('(nthnext "" 0)')).toBeNull()

      expect(() => lits.run('(nthnext [1 2 3]')).toThrow()
      expect(() => lits.run('(nthnext [1 2 3] :1')).toThrow()
      expect(() => lits.run('(nthnext [1 2 3] null')).toThrow()
      expect(() => lits.run('(nthnext [1 2 3] {}')).toThrow()
      expect(() => lits.run('(nthnext true)')).toThrow()
      expect(() => lits.run('(nthnext false)')).toThrow()
      expect(() => lits.run('(nthnext null)')).toThrow()
      expect(() => lits.run('(nthnext (object))')).toThrow()
      expect(() => lits.run('(nthnext 10)')).toThrow()
    })
  })

  describe('push', () => {
    it('samples', () => {
      expect(lits.run('(push [1 2 3] 0)')).toEqual([1, 2, 3, 0])
      expect(lits.run('(push [1 2 3] 1 :2)')).toEqual([1, 2, 3, 1, '2'])
      expect(lits.run('(def l [1 2 3]) (push l 1 :2)')).toEqual([1, 2, 3, 1, '2'])
      expect(lits.run('(def l [1 2 3]) (push l 1 :2) l')).toEqual([1, 2, 3])
      expect(lits.run('(push "Albert" "!")')).toBe('Albert!')
      expect(lits.run('(push "Albert" "!" "?")')).toBe('Albert!?')
      expect(lits.run('(push "" "!" "?")')).toBe('!?')

      expect(() => lits.run('(push "Albert" "!?")')).toThrow()
      expect(() => lits.run('(push [1 2 3])')).toThrow()
      expect(() => lits.run('(push (object) 0 :2)')).toThrow()
      expect(() => lits.run('(push null 0 :2)')).toThrow()
      expect(() => lits.run('(push true 0 :2)')).toThrow()
      expect(() => lits.run('(push false 0 :2)')).toThrow()
      expect(() => lits.run('(push 1 0 :2)')).toThrow()
      expect(() => lits.run('(push :1 0 :2)')).toThrow()
      expect(() => lits.run('(push 0 :2)')).toThrow()
      expect(() => lits.run('(push)')).toThrow()
    })
  })

  describe('pop', () => {
    it('samples', () => {
      expect(lits.run('(pop [1 2 3])')).toEqual([1, 2])
      expect(lits.run('(pop [])')).toEqual([])
      expect(lits.run('(def l [1 2 3]) (pop l) l')).toEqual([1, 2, 3])
      expect(lits.run('(def l [1 2 3]) (pop l)')).toEqual([1, 2])
      expect(lits.run('(def l []) (pop l) l')).toEqual([])
      expect(lits.run('(pop "Albert")')).toBe('Alber')
      expect(lits.run('(pop :1)')).toBe('')
      expect(lits.run('(pop "")')).toBe('')

      expect(() => lits.run('(pop (object))')).toThrow()
      expect(() => lits.run('(pop null)')).toThrow()
      expect(() => lits.run('(pop true)')).toThrow()
      expect(() => lits.run('(pop false)')).toThrow()
      expect(() => lits.run('(pop 1)')).toThrow()
      expect(() => lits.run('(pop)')).toThrow()
    })
  })

  describe('unshift', () => {
    it('samples', () => {
      expect(lits.run('(unshift [1 2 3] 0)')).toEqual([0, 1, 2, 3])
      expect(lits.run('(unshift [1 2 3] 1 :2)')).toEqual([1, '2', 1, 2, 3])
      expect(lits.run('(def l [1 2 3]) (unshift l 1 :2) l')).toEqual([1, 2, 3])
      expect(lits.run('(def l [1 2 3]) (unshift l 1 :2)')).toEqual([1, '2', 1, 2, 3])
      expect(lits.run('(unshift "lbert" :A)')).toBe('Albert')

      expect(() => lits.run('(unshift [1 2 3])')).toThrow()
      expect(() => lits.run('(unshift (object) 0 :2)')).toThrow()
      expect(() => lits.run('(unshift null 0 :2)')).toThrow()
      expect(() => lits.run('(unshift true 0 :2)')).toThrow()
      expect(() => lits.run('(unshift false 0 :2)')).toThrow()
      expect(() => lits.run('(unshift 1 0 :2)')).toThrow()
      expect(() => lits.run('(unshift :1 0 :2)')).toThrow()
      expect(() => lits.run('(unshift 0 :2)')).toThrow()
      expect(() => lits.run('(unshift)')).toThrow()
    })
  })

  describe('shift', () => {
    it('samples', () => {
      expect(lits.run('(shift [1 2 3])')).toEqual([2, 3])
      expect(lits.run('(shift [])')).toEqual([])
      expect(lits.run('(def l [1 2 3]) (shift l) l')).toEqual([1, 2, 3])
      expect(lits.run('(def l [1 2 3]) (shift l)')).toEqual([2, 3])
      expect(lits.run('(def l []) (shift l) l')).toEqual([])
      expect(lits.run('(shift "Albert")')).toBe('lbert')
      expect(lits.run('(shift :1)')).toBe('')
      expect(lits.run('(shift "")')).toBe('')

      expect(() => lits.run('(shift (object))')).toThrow()
      expect(() => lits.run('(shift null)')).toThrow()
      expect(() => lits.run('(shift true)')).toThrow()
      expect(() => lits.run('(shift false)')).toThrow()
      expect(() => lits.run('(shift 1)')).toThrow()
      expect(() => lits.run('(shift)')).toThrow()
    })
  })

  describe('take', () => {
    it('samples', () => {
      expect(lits.run('(take [1 2 3] 2)')).toEqual([1, 2])
      expect(lits.run('(take [] 2)')).toEqual([])
      expect(lits.run('(take [1 2 3] 20)')).toEqual([1, 2, 3])
      expect(lits.run('(take [1 2 3] 0)')).toEqual([])
      expect(lits.run('(take "Albert" 2)')).toEqual('Al')
      expect(lits.run('(take "Albert" 2.01)')).toEqual('Alb')

      expect(() => lits.run('(take (object) 1)')).toThrow()
      expect(() => lits.run('(take null 1)')).toThrow()
      expect(() => lits.run('(take true 1)')).toThrow()
      expect(() => lits.run('(take false 1)')).toThrow()
      expect(() => lits.run('(take "Hej" :1)')).toThrow()
      expect(() => lits.run('(take)')).toThrow()
      expect(() => lits.run('(take [1 2 3])')).toThrow()
      expect(() => lits.run('(take [1 2 3] 1 2)')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take l1 2))
        (!= l1 l2)
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('take_last', () => {
    it('samples', () => {
      expect(lits.run('(take_last [1 2 3] 2)')).toEqual([2, 3])
      expect(lits.run('(take_last [1 2 3] 20)')).toEqual([1, 2, 3])
      expect(lits.run('(take_last [1 2 3] 0)')).toEqual([])
      expect(lits.run('(take_last [1 2 3] 0.01)')).toEqual([3])

      expect(() => lits.run('(take_last (object))')).toThrow()
      expect(() => lits.run('(take_last null)')).toThrow()
      expect(() => lits.run('(take_last true)')).toThrow()
      expect(() => lits.run('(take_last false)')).toThrow()
      expect(() => lits.run('(take_last :1)')).toThrow()
      expect(() => lits.run('(take_last)')).toThrow()
      expect(() => lits.run('(take_last [1 2 3])')).toThrow()
      expect(() => lits.run('(take_last [1 2 3] 1 2)')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take_last l1 2))
        (!= l1 l2)
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('take_while', () => {
    it('samples', () => {
      expect(lits.run('(take_while [1 2 3 2 1] (fn [x] (< x 3)))')).toEqual([1, 2])
      expect(lits.run('(take_while [1 2 3 2 1] (fn [x] (> x 3)))')).toEqual([])
      expect(lits.run('(take_while "abcdabcd" (fn [x] (<= x :c)))')).toEqual('abc')

      expect(() => lits.run('(take_while (object) (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(take_while null (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(take_while true (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(take_while false (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(take_while [1 2 3] 10)')).toThrow()
      expect(() => lits.run('(take_while)')).toThrow()
      expect(() => lits.run('(take_while [1 2 3])')).toThrow()
      expect(() => lits.run('(take_while [1 2 3] (fn [x] (< x 3)) 1)')).toThrow()
    })
    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take_while l1 (fn [x] (< x 3))))
        (!= l1 l2)
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('drop', () => {
    it('samples', () => {
      expect(lits.run('(drop [1 2 3] 2)')).toEqual([3])
      expect(lits.run('(drop [1 2 3] 20)')).toEqual([])
      expect(lits.run('(drop [1 2 3] 0)')).toEqual([1, 2, 3])
      expect(lits.run('(drop "Albert" 2)')).toEqual('bert')
      expect(lits.run('(drop [1 2 3] 0.5)')).toEqual([2, 3])
      expect(lits.run('(drop "Albert" -2)')).toEqual('Albert')

      expect(() => lits.run('(drop (object) 1)')).toThrow()
      expect(() => lits.run('(drop null 1)')).toThrow()
      expect(() => lits.run('(drop true 1)')).toThrow()
      expect(() => lits.run('(drop false 1)')).toThrow()
      expect(() => lits.run('(drop "Hej" :1)')).toThrow()
      expect(() => lits.run('(drop)')).toThrow()
      expect(() => lits.run('(drop [1 2 3])')).toThrow()
      expect(() => lits.run('(drop [1 2 3] 1 2)')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (drop l1 2))
        (!= l1 l2)
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('drop_last', () => {
    it('samples', () => {
      expect(lits.run('(drop_last [1 2 3] 2)')).toEqual([1])
      expect(lits.run('(drop_last [1 2 3] 20)')).toEqual([])
      expect(lits.run('(drop_last [1 2 3] 0)')).toEqual([1, 2, 3])
      expect(lits.run('(drop_last "Albert" 2)')).toEqual('Albe')
      expect(lits.run('(drop_last [1 2 3] 0.5)')).toEqual([1, 2])
      expect(lits.run('(drop_last "Albert" -2)')).toEqual('Albert')

      expect(() => lits.run('(drop_last (object) 1)')).toThrow()
      expect(() => lits.run('(drop_last null 1)')).toThrow()
      expect(() => lits.run('(drop_last true 1)')).toThrow()
      expect(() => lits.run('(drop_last false 1)')).toThrow()
      expect(() => lits.run('(drop_last "Hej" :1)')).toThrow()
      expect(() => lits.run('(drop_last)')).toThrow()
      expect(() => lits.run('(drop_last [1 2 3])')).toThrow()
      expect(() => lits.run('(drop_last [1 2 3] 1 2)')).toThrow()
    })
  })

  describe('drop_while', () => {
    it('samples', () => {
      expect(lits.run('(drop_while [1 2 3 2 1] (fn [x] (< x 3)))')).toEqual([3, 2, 1])
      expect(lits.run('(drop_while [1 2 3 2 1] (fn [x] (> x 3)))')).toEqual([1, 2, 3, 2, 1])
      expect(lits.run('(drop_while "abcdab" (fn [x] (<= x :c)))')).toEqual('dab')

      expect(() => lits.run('(drop_while (object) (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(drop_while null (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(drop_while true (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(drop_while false (fn [x] (< x 3)))')).toThrow()
      expect(() => lits.run('(drop_while [1 2 3] 10)')).toThrow()
      expect(() => lits.run('(drop_while)')).toThrow()
      expect(() => lits.run('(drop_while [1 2 3])')).toThrow()
      expect(() => lits.run('(drop_while [1 2 3] (fn [x] (< x 3)) 1)')).toThrow()
    })
    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take_while l1 (fn [x] (< x 3))))
        (!= l1 l2)
      `
      expect(lits.run(program)).toBe(true)
    })
  })

  describe('sort', () => {
    it('samples', () => {
      expect(lits.run('(sort [3 1 2] (fn [a b] (cond (< a b) -1 (> a b) 1 true 0)))')).toEqual([1, 2, 3])
      expect(lits.run('(sort [3 1 2] (fn [a b] (cond (> a b) -1 (< a b) 1 true 0)))')).toEqual([3, 2, 1])
      expect(lits.run('(sort [] (fn [a b] (cond (> a b) -1 (< a b) 1 true 0)))')).toEqual([])

      expect(lits.run('(sort "Albert" (fn [a b] (cond (< a b) 1 (> a b) -1 true 0)))')).toBe('trlebA')

      expect(lits.run('(sort "Albert")')).toBe('Abelrt')
      expect(
        lits.run(
          '(sort [1 true 2 false  -100 null (regexp "abc") (regexp "ABC") [] [1 2 3] [0 1 2] [0 0 0 0] {:a 1} {} "Albert" "albert"])',
        ),
      ).toMatchSnapshot()

      expect(() => lits.run('(sort 10 (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)))')).toThrow()
      expect(() => lits.run('(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)))')).toThrow()
      expect(() => lits.run('(sort)')).toThrow()
    })
  })

  describe('join', () => {
    it('samples', () => {
      expect(lits.run('(join ["Albert" "Mojir"] " ")')).toBe('Albert Mojir')
      expect(lits.run('(join (map [0 1 2 3 4 5 6 7 8 9] str) ", ")')).toBe('0, 1, 2, 3, 4, 5, 6, 7, 8, 9')
      expect(() => lits.run('(join (map [0 1 2 3 4 5 6 7 8 9] str) ", " 5)')).toThrow()
      expect(() => lits.run('(join ["Albert" "Mojir"] " " -1)')).toThrow()
      expect(() => lits.run('(join ["Albert" "Mojir"])')).toThrow()
      expect(() => lits.run('(join ["Albert" 10] " ")')).toThrow()
    })
  })

  describe('distinct', () => {
    it('samples', () => {
      expect(lits.run('(distinct [1 2 3 1 3 5])')).toEqual([1, 2, 3, 5])
      expect(lits.run('(distinct [])')).toEqual([])
      expect(lits.run('(distinct "Albert Mojir")')).toBe('Albert Moji')
      expect(lits.run('(distinct "")')).toBe('')
      expect(() => lits.run('(distinct)')).toThrow()
      expect(() => lits.run('(distinct [] [])')).toThrow()
    })
  })

  describe('remove', () => {
    it('samples', () => {
      expect(lits.run('(remove [1 2 3 1 3 5] even?)')).toEqual([1, 3, 1, 3, 5])
      expect(lits.run('(remove "Albert Mojir" #(has? "aoueiyAOUEIY" %1))')).toBe('lbrt Mjr')
      expect(() => lits.run('(remove)')).toThrow()
      expect(() => lits.run('(remove "Albert Mojir")')).toThrow()
      expect(() => lits.run('(remove #(has? "aoueiyAOUEIY" %1))')).toThrow()
      expect(() => lits.run('(remove "Albert" #(has? "aoueiyAOUEIY" %1) "Mojir")')).toThrow()
    })
  })

  describe('remove_at', () => {
    it('samples', () => {
      expect(lits.run('(remove_at [1 2 3 4 5] -1)')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('(remove_at [1 2 3 4 5] 0)')).toEqual([2, 3, 4, 5])
      expect(lits.run('(remove_at [1 2 3 4 5] 2)')).toEqual([1, 2, 4, 5])
      expect(lits.run('(remove_at [1 2 3 4 5] 4)')).toEqual([1, 2, 3, 4])
      expect(lits.run('(remove_at [1 2 3 4 5] 5)')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('(remove_at "Mojir" -1)')).toEqual('Mojir')
      expect(lits.run('(remove_at "Mojir" 0)')).toEqual('ojir')
      expect(lits.run('(remove_at "Mojir" 2)')).toEqual('Moir')
      expect(lits.run('(remove_at "Mojir" 4)')).toEqual('Moji')
      expect(lits.run('(remove_at "Mojir" 5)')).toEqual('Mojir')
      expect(() => lits.run('(remove_at)')).toThrow()
      expect(() => lits.run('(remove_at "Albert Mojir")')).toThrow()
      expect(() => lits.run('(remove_at 1)')).toThrow()
      expect(() => lits.run('(remove_at "Albert" 1 2')).toThrow()
    })
  })

  describe('split_at', () => {
    it('samples', () => {
      expect(lits.run('(split_at [1 2 3 4 5] 2)')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('(split_at [1 2 3 4 5] 0.01)')).toEqual([[1], [2, 3, 4, 5]])
      expect(lits.run('(split_at [1 2 3 4 5] 0)')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split_at [1 2 3 4 5] -1)')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split_at [1 2 3 4 5] 100)')).toEqual([[1, 2, 3, 4, 5], []])
      expect(lits.run('(split_at "Albert" 2)')).toEqual(['Al', 'bert'])
      expect(lits.run('(split_at "Albert" 0.01)')).toEqual(['A', 'lbert'])
      expect(lits.run('(split_at "Albert" 0)')).toEqual(['', 'Albert'])
      expect(lits.run('(split_at "Albert" -1)')).toEqual(['', 'Albert'])
      expect(lits.run('(split_at "Albert" 100)')).toEqual(['Albert', ''])

      expect(() => lits.run('(split_at)')).toThrow()
      expect(() => lits.run('(split_at 3)')).toThrow()
      expect(() => lits.run('(split_at "Albert" 3 "Mojir")')).toThrow()
    })
  })

  describe('split_with', () => {
    it('samples', () => {
      expect(lits.run('(split_with [1 2 3 4 5] #(< %1 3))')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('(split_with [1 2 3 4 5] #(> %1 3))')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split_with [1 2 3 4 5] #(< %1 10))')).toEqual([[1, 2, 3, 4, 5], []])

      expect(lits.run('(split_with "Albert" #(<= %1 :Z))')).toEqual(['A', 'lbert'])
      expect(lits.run('(split_with "Albert" #(> %1 :Z))')).toEqual(['', 'Albert'])
      expect(lits.run('(split_with "Albert" #(<= %1 :z))')).toEqual(['Albert', ''])

      expect(() => lits.run('(split_with)')).toThrow()
      expect(() => lits.run('(split_with #(<= %1 :Z))')).toThrow()
      expect(() => lits.run('(split_with "Albert" #(<= %1 :Z) "Mojir")')).toThrow()
    })
  })

  describe('frequencies', () => {
    it('samples', () => {
      expect(lits.run('(frequencies ["Albert" "Mojir" "Nina" "Mojir"])')).toEqual({ Albert: 1, Nina: 1, Mojir: 2 })
      expect(lits.run('(frequencies "Pneumonoultramicroscopicsilicovolcanoconiosis")')).toEqual({
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
      expect(() => lits.run('(frequencies)')).toThrow()
      expect(() => lits.run('(frequencies {})')).toThrow()
      expect(() => lits.run('(frequencies 3)')).toThrow()
      expect(() => lits.run('(frequencies "" "")')).toThrow()
    })
  })

  describe('group_by', () => {
    it('samples', () => {
      expect(lits.run('(group_by [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}] "name")')).toEqual({
        Albert: [{ name: 'Albert' }, { name: 'Albert' }],
        Mojir: [{ name: 'Mojir' }],
      })
      expect(lits.run('(group_by "Albert Mojir" (fn [char] (if (has? "aoueiAOUEI" char) "vowel" "other")))')).toEqual({
        other: ['l', 'b', 'r', 't', ' ', 'M', 'j', 'r'],
        vowel: ['A', 'e', 'o', 'i'],
      })
      expect(() => lits.run('(group_by)')).toThrow()
      expect(() => lits.run('(group_by :a)')).toThrow()
      expect(() => lits.run('(group_by :a {})')).toThrow()
      expect(() => lits.run('(group_by :a 3)')).toThrow()
      expect(() => lits.run('(group_by "" :a "")')).toThrow()
    })
  })

  describe('sort_by', () => {
    it('samples', () => {
      expect(lits.run('(sort_by ["Albert" "Mojir" "Nina"] count)')).toEqual(['Nina', 'Mojir', 'Albert'])
      expect(lits.run('(sort_by ["Albert" "Mojir" "Nina"] count (fn [a b] (- b a)))')).toEqual([
        'Albert',
        'Mojir',
        'Nina',
      ])
      expect(lits.run('(sort_by "Albert" lower_case)')).toEqual('Abelrt')
      expect(lits.run('(sort_by "Albert" lower_case (fn [a b] (- (to_char_code b) (to_char_code a))))')).toEqual(
        'trlebA',
      )
      expect(() => lits.run('(sort_by)')).toThrow()
      expect(() => lits.run('(sort_by :a)')).toThrow()
      expect(() => lits.run('(sort_by {} :a)')).toThrow()
      expect(() => lits.run('(sort_by 3 :a)')).toThrow()
      expect(() => lits.run('(sort_by "" :a "")')).toThrow()
    })
  })

  describe('partition', () => {
    it('samples', () => {
      expect(lits.run('(partition (range 20) 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('(partition (range 22) 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('(partition (range 20) 4 6)')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
      ])
      expect(lits.run('(partition (range 20) 4 3)')).toEqual([
        [0, 1, 2, 3],
        [3, 4, 5, 6],
        [6, 7, 8, 9],
        [9, 10, 11, 12],
        [12, 13, 14, 15],
        [15, 16, 17, 18],
      ])
      expect(lits.run('(partition (range 20) 3 6 [:a])')).toEqual([
        [0, 1, 2],
        [6, 7, 8],
        [12, 13, 14],
        [18, 19, 'a'],
      ])
      expect(lits.run('(partition (range 20) 4 6 [:a])')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a'],
      ])
      expect(lits.run('(partition (range 20) 4 6 [:a :b :c :d])')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a', 'b'],
      ])
      expect(lits.run('(partition [:a :b :c :d :e :f] 3 1)')).toEqual([
        ['a', 'b', 'c'],
        ['b', 'c', 'd'],
        ['c', 'd', 'e'],
        ['d', 'e', 'f'],
      ])
      expect(lits.run('(partition [1 2 3 4] 10)')).toEqual([])
      expect(lits.run('(partition [1 2 3 4] 10 10)')).toEqual([])
      expect(lits.run('(partition [1 2 3 4] 10 10 [])')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('(partition [1 2 3 4] 10 10 null)')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('(partition "superfragilistic" 5)')).toEqual(['super', 'fragi', 'listi'])
      expect(lits.run('(partition "superfragilistic" 5 5 null)')).toEqual(['super', 'fragi', 'listi', 'c'])
      expect(lits.run('(def foo [5 6 7 8]) (partition foo 2 1 foo)')).toEqual([
        [5, 6],
        [6, 7],
        [7, 8],
        [8, 5],
      ])
      expect(() => lits.run('(partition [1 2 3 4] 0)')).toThrow()
      expect(() => lits.run('(partition 1)')).toThrow()
      expect(() => lits.run('(partition [1])')).toThrow()
    })
  })

  describe('partition_all', () => {
    it('samples', () => {
      expect(lits.run('(partition_all [0 1 2 3 4 5 6 7 8 9] 4)')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9],
      ])
      expect(lits.run('(partition_all [0 1 2 3 4 5 6 7 8 9] 2 4)')).toEqual([
        [0, 1],
        [4, 5],
        [8, 9],
      ])
      expect(() => lits.run('(partition_all 1)')).toThrow()
      expect(() => lits.run('(partition_all [1])')).toThrow()
    })
  })

  describe('partition_by', () => {
    it('samples', () => {
      expect(lits.run('(partition_by [1 2 3 4 5] #(= 3 %1))')).toEqual([[1, 2], [3], [4, 5]])
      expect(lits.run('(partition_by [1 1 1 2 2 3 3] odd?)')).toEqual([
        [1, 1, 1],
        [2, 2],
        [3, 3],
      ])
      expect(lits.run('(partition_by "Leeeeeerrroyyy" identity)')).toEqual(['L', 'eeeeee', 'rrr', 'o', 'yyy'])
      expect(() => lits.run('(partition_by odd?)')).toThrow()
      expect(() => lits.run('(partition_by [1 2 3])')).toThrow()
    })
  })

  describe('starts_with?', () => {
    it('samples', () => {
      expect(lits.run('(starts_with? [1, 2, 3] 1)')).toBe(true)
      expect(lits.run('(starts_with? [1, 2, 3] 2)')).toBe(false)
      expect(lits.run('(starts_with? [1, 2, 3] [1])')).toBe(false)

      expect(lits.run('(starts_with? "Albert" "Al")')).toBe(true)
      expect(lits.run('(starts_with? "Albert" "al")')).toBe(false)
      expect(lits.run('(starts_with? "Albert" "")')).toBe(true)
      expect(lits.run('(starts_with? "" "")')).toBe(true)
      expect(lits.run('(starts_with? "Albert" "Albert")')).toBe(true)
      expect(lits.run('(starts_with? "Albert" "Albert ")')).toBe(false)
      expect(() => lits.run('(starts_with? "Albert" "foo" 2)')).toThrow()
      expect(() => lits.run('(starts_with? "Albert")')).toThrow()
      expect(() => lits.run('(starts_with?)')).toThrow()
    })
  })

  describe('ends_with?', () => {
    it('samples', () => {
      expect(lits.run('(ends_with? [1, 2, 3] 3)')).toBe(true)
      expect(lits.run('(ends_with? [1, 2, 3] 2)')).toBe(false)
      expect(lits.run('(ends_with? [1, 2, 3] [3])')).toBe(false)

      expect(lits.run('(ends_with? "Albert" "rt")')).toBe(true)
      expect(lits.run('(ends_with? "Albert" "RT")')).toBe(false)
      expect(lits.run('(ends_with? "Albert" "")')).toBe(true)
      expect(lits.run('(ends_with? "" "")')).toBe(true)
      expect(lits.run('(ends_with? "Albert" "Albert")')).toBe(true)
      expect(lits.run('(ends_with? "Albert" " Albert")')).toBe(false)
      expect(() => lits.run('(ends_with? "Albert" "foo" 2)')).toThrow()
      expect(() => lits.run('(ends_with? "Albert")')).toThrow()
      expect(() => lits.run('(ends_with?)')).toThrow()
    })
  })
  describe('interleave', () => {
    it('samples', () => {
      expect(lits.run('(interleave [1 2 3] [4 5 6])')).toEqual([1, 4, 2, 5, 3, 6])
      expect(lits.run('(interleave [1 2 3] [4 5 6] [7 8 9])')).toEqual([1, 4, 7, 2, 5, 8, 3, 6, 9])
      expect(lits.run('(interleave [1 2 3] [4 5 6] [7 8])')).toEqual([1, 4, 7, 2, 5, 8])
      expect(lits.run('(interleave [1 2 3] [4 5 6] [7])')).toEqual([1, 4, 7])
      expect(lits.run('(interleave [1 2 3] [4 5 6] [7] [8 9])')).toEqual([1, 4, 7, 8])
      expect(lits.run('(interleave [] [4 5 6] [7] [8 9])')).toEqual([])
      expect(lits.run('(interleave "Albert" "Mojir")')).toEqual('AMlobjeirr')

      expect(() => lits.run('(interleave)')).toThrow()
      expect(() => lits.run('(interleave 1)')).toThrow()
      expect(() => lits.run('(interleave [1, 2, 3] "asd")')).toThrow()
    })
  })
  describe('interpose', () => {
    it('samples', () => {
      expect(lits.run('(interpose [1 2 3 4] :a)')).toEqual([1, 'a', 2, 'a', 3, 'a', 4])
      expect(lits.run('(interpose [1 2 3] :a)')).toEqual([1, 'a', 2, 'a', 3])
      expect(lits.run('(interpose [1] :a)')).toEqual([1])
      expect(lits.run('(interpose [] :a)')).toEqual([])
      expect(lits.run('(interpose "Albert" ":")')).toEqual('A:l:b:e:r:t')
      expect(() => lits.run('(interpose)')).toThrow()
      expect(() => lits.run('(interpose 1)')).toThrow()
      expect(() => lits.run('(interpose :a 1)')).toThrow()
    })
  })
})
