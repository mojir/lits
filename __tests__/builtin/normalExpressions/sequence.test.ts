import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'

const lits = new Lits()
const litsInstances = [lits, new Lits({ debug: true })]
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

    it('nil sequence', () => {
      expect(lits.run('(nth nil 0)')).toBeNull()
      expect(lits.run('(nth nil 0 99)')).toBe(99)
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
      expect(() => lits.run('(slice nil 2)')).toThrow()
    })
  })

  describe('reductions', () => {
    it('samples', () => {
      // expect(lits.run('(reductions + [1 2 3 4 5])')).toEqual([1, 3, 6, 10, 15])
      // expect(lits.run('(reductions + [])')).toEqual([0])
      // expect(lits.run('(reductions + [1])')).toEqual([1])
      // expect(lits.run('(reductions + [1 2])')).toEqual([1, 3])
      // expect(lits.run('(reductions + 1 [])')).toEqual([1])
      // expect(lits.run('(reductions + 1 [2 3])')).toEqual([1, 3, 6])
      // expect(lits.run('(reductions + 0 [1 2 3])')).toEqual([0, 1, 3, 6])
      // expect(lits.run('(reductions + 0 [])')).toEqual([0])
      // expect(lits.run('(reductions + 1 [])')).toEqual([1])

      expect(lits.run('(reductions (fn [x y] (concat x "-" y)) "Albert")')).toEqual([
        'A',
        'A-l',
        'A-l-b',
        'A-l-b-e',
        'A-l-b-e-r',
        'A-l-b-e-r-t',
      ])
      // expect(lits.run('(reductions (fn [x y] (concat x "-" y)) ">" "Albert")')).toEqual([
      //   '>',
      //   '>-A',
      //   '>-A-l',
      //   '>-A-l-b',
      //   '>-A-l-b-e',
      //   '>-A-l-b-e-r',
      //   '>-A-l-b-e-r-t',
      // ])
      // expect(lits.run('(reductions (fn [x y] (concat x "-" y)) ">" "")')).toEqual(['>'])

      // expect(() => lits.run('(reductions + nil)')).toThrow()
      // expect(() => lits.run('(reductions +)')).toThrow()
      // expect(() => lits.run('(reductions)')).toThrow()
      // expect(() => lits.run('(reductions + 1 2)')).toThrow()
    })
  })

  describe('reduce', () => {
    it('samples', () => {
      let program = `
      (defn countChars [stringArray]
        (reduce
          (fn [sum str] (+ sum (count str)))
          0
          stringArray
        )
      )

      (countChars ["First" "Second" "Third"])
      `
      expect(lits.run(program)).toBe(16)

      program = `
      (defn longestLength [stringArray]
        (reduce
          (fn [sum str]
            (if (> sum (count str))
              sum
              (count str)
            )
          )
          0
          stringArray
        )
      )

      (longestLength ["First" "Second" "Third"])
      `
      expect(lits.run(program)).toBe(6)

      expect(lits.run('(reduce + [1 2 3 4 5])')).toBe(15)
      expect(lits.run('(reduce + [])')).toBe(0)
      expect(lits.run('(reduce + [1])')).toBe(1)
      expect(lits.run('(reduce + [1 2])')).toBe(3)
      expect(lits.run('(reduce + 1 [])')).toBe(1)
      expect(lits.run('(reduce + 1 [2 3])')).toBe(6)
      expect(lits.run('(reduce + 0 [1 2 3])')).toBe(6)
      expect(lits.run('(reduce + 0 [])')).toBe(0)
      expect(lits.run('(reduce + 1 [])')).toBe(1)

      expect(lits.run('(reduce (fn [x y] (concat x "-" y)) "Albert")')).toBe('A-l-b-e-r-t')
      expect(lits.run('(reduce (fn [x y] (concat x "-" y)) ">" "Albert")')).toBe('>-A-l-b-e-r-t')
      expect(lits.run('(reduce (fn [x y] (concat x "-" y)) ">" "")')).toBe('>')

      expect(() => lits.run('(reduce +)')).toThrow()
      expect(() => lits.run('(reduce)')).toThrow()
      expect(() => lits.run('(reduce + 1 2)')).toThrow()
    })
  })

  describe('reduce-right', () => {
    it('samples', () => {
      expect(lits.run('(reduce-right + [1 2 3 4 5])')).toBe(15)
      expect(lits.run('(reduce-right + [])')).toBe(0)
      expect(lits.run('(reduce-right + [1])')).toBe(1)
      expect(lits.run('(reduce-right + [1 2])')).toBe(3)
      expect(lits.run('(reduce-right + 0 [1 2 3])')).toBe(6)
      expect(lits.run('(reduce-right + 0 [])')).toBe(0)
      expect(lits.run('(reduce-right + 0 [])')).toBe(0)
      expect(lits.run('(reduce-right str "" [:1 :2 :3])')).toBe('321')
      expect(lits.run('(reduce-right str [:1 :2 :3])')).toBe('321')

      expect(lits.run('(reduce-right (fn [x y] (concat x "-" y)) "Albert")')).toBe('t-r-e-b-l-A')
      expect(lits.run('(reduce-right (fn [x y] (concat x "-" y)) ">" "Albert")')).toBe('>-t-r-e-b-l-A')
      expect(lits.run('(reduce-right (fn [x y] (concat x "-" y)) ">" "")')).toBe('>')

      expect(() => lits.run('(reduce-right +)')).toThrow()
      expect(() => lits.run('(reduce-right)')).toThrow()
      expect(() => lits.run('(reduce-right + 1 2)')).toThrow()
    })
  })
  describe('filter', () => {
    it('samples', () => {
      expect(lits.run('(filter [1 :2 3] number?)')).toEqual([1, 3])
      expect(lits.run('(filter [] number?)')).toEqual([])
      expect(lits.run('(filter [1 :2 3] nil?)')).toEqual([])
      expect(lits.run('(filter [0 1 2 3 4 5 6 7] (fn [x] (zero? (mod x 3))))')).toEqual([0, 3, 6])
      expect(lits.run('(filter "aAbBcC" (fn [x] (>= x :a)))')).toBe('abc')
      expect(() => lits.run('(filter +)')).toThrow()
      expect(() => lits.run('(filter)')).toThrow()
      expect(() => lits.run('(filter [1] number? 2)')).toThrow()
    })
  })

  describe('position', () => {
    it('samples', () => {
      expect(lits.run('(position number? [:1 :2 3])')).toEqual(2)
      expect(lits.run('(position number? [:1 :2 :3])')).toBeNull()
      expect(lits.run('(position number? [])')).toBeNull()
      expect(lits.run('(position number? nil)')).toBeNull()
      expect(lits.run('(position (fn [x] (zero? (mod x 3))) [1 2 3 4 5 6 7])')).toEqual(2)
      expect(lits.run('(position (fn [x] (>= x :a)) "Aa")')).toBe(1)
      expect(lits.run('(position (fn [x] (= x :z)) "Aa")')).toBeNull()
      expect(() => lits.run('(position +)')).toThrow()
      expect(() => lits.run('(position)')).toThrow()
      expect(() => lits.run('(position number? [1] 2)')).toThrow()
    })
  })

  describe('index-of', () => {
    it('samples', () => {
      expect(lits.run('(index-of [:1 :2 3] :2)')).toEqual(1)
      expect(lits.run('(index-of [:1 :2 :3] :4)')).toBeNull()
      expect(lits.run('(index-of [] 1)')).toBeNull()
      expect(lits.run('(index-of nil 1)')).toBeNull()
      expect(lits.run('(index-of "Albert" :l)')).toBe(1)
      expect(lits.run('(index-of "Albert" "ert")')).toBe(3)
      expect(lits.run('(index-of "Albert" :z)')).toBeNull()
      expect(lits.run('(index-of [1] 2)')).toBeNull()
      expect(() => lits.run('(index-of +)')).toThrow()
      expect(() => lits.run('(index-of)')).toThrow()
    })
  })

  describe('some', () => {
    it('samples', () => {
      expect(lits.run('(some #(= :l %) :Albert)')).toBe('l')

      expect(lits.run('(some number? nil)')).toBeNull()
      expect(lits.run('(some number? [:1 :2 3])')).toBe(3)
      expect(lits.run('(some number? [:1 :2 :3])')).toBeNull()
      expect(lits.run('(some number? [])')).toBeNull()
      expect(lits.run('(some (fn [x] (zero? (mod x 3))) [1 2 3 4 5 6 7])')).toBe(3)

      expect(lits.run('(some (fn [x] (>= x :a)) "Aa")')).toBe('a')
      expect(lits.run('(some (fn [x] (>= x :z)) "Aa")')).toBeNull()

      expect(() => lits.run('(some +)')).toThrow()
      expect(() => lits.run('(some)')).toThrow()
      expect(() => lits.run('(some number? [1] 2)')).toThrow()
    })
  })

  describe('map', () => {
    it('samples', () => {
      expect(lits.run('(map number? [1 :2 3])')).toEqual([true, false, true])
      expect(lits.run('(map number? [])')).toEqual([])
      expect(lits.run('(map + [1 2 3] [1 2 3])')).toEqual([2, 4, 6])
      expect(lits.run('(map max [2 6 3] [2 4 7] [1 6 2])')).toEqual([2, 6, 7])
      expect(lits.run('(map (fn [x] (if (>= x :a) "-" "+")) "AaBbCc")')).toBe('+-+-+-')
      expect(() => lits.run('(map (fn [x] (if (>= x :a) 0 1)) "AaBbCc")')).toThrow()
      expect(
        lits.run(
          `
          (defn maxChar [char & chars]
            (loop [cs chars result char]
              (if (empty? cs)
                result
                (recur
                  (rest cs)
                  (if (> (chars 0) result) (chars 0) result)
                )
              )
            )
          )

          (map maxChar "263" "247" "162")
          `,
        ),
      ).toEqual('267')
      expect(lits.run('(map nil? [1 :2 3])')).toEqual([false, false, false])
      expect(lits.run('(map (fn [x] (zero? (mod x 3))) [0 1 2 3 4 5 6 7])')).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lits.run('(map inc [0 1 2 3 4 5 6 7])')).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(() => lits.run('(map + [1 2 3] [1 2])')).toThrow()
      expect(() => lits.run('(map +)')).toThrow()
      expect(() => lits.run('(map)')).toThrow()
      expect(() => lits.run('(map number? [1] 2)')).toThrow()
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
      expect(lits.run('(first nil)')).toBeNull()

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

      expect(lits.run('(second nil)')).toBeNull()

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

      expect(lits.run('(reverse nil)')).toBeNull()

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
        (not= l (reverse l))
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

      expect(lits.run('(last nil)')).toBeNull()

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
      expect(() => lits.run('(rest nil)')).toThrow()
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
      expect(() => lits.run('(nthrest [1 2 3] nil')).toThrow()
      expect(() => lits.run('(nthrest [1 2 3] {}')).toThrow()
      expect(() => lits.run('(nthrest true)')).toThrow()
      expect(() => lits.run('(nthrest false)')).toThrow()
      expect(() => lits.run('(nthrest nil)')).toThrow()
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
      expect(() => lits.run('(next nil)')).toThrow()
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
      expect(() => lits.run('(nthnext [1 2 3] nil')).toThrow()
      expect(() => lits.run('(nthnext [1 2 3] {}')).toThrow()
      expect(() => lits.run('(nthnext true)')).toThrow()
      expect(() => lits.run('(nthnext false)')).toThrow()
      expect(() => lits.run('(nthnext nil)')).toThrow()
      expect(() => lits.run('(nthnext (object))')).toThrow()
      expect(() => lits.run('(nthnext 10)')).toThrow()
    })
  })
  describe('cons', () => {
    it('samples', () => {
      expect(lits.run('(cons 0 [1 2 3])')).toEqual([0, 1, 2, 3])
      expect(lits.run('(cons 0 [:1])')).toEqual([0, '1'])
      expect(lits.run('(cons 0 [])')).toEqual([0])
      expect(lits.run('(cons :A "Mojir")')).toEqual('AMojir')

      expect(() => lits.run('(const "Ab" "Mojir")')).toThrow()
      expect(() => lits.run('(cons')).toThrow()
      expect(() => lits.run('(cons 1 :1)')).toThrow()
      expect(() => lits.run('(cons 1 true)')).toThrow()
      expect(() => lits.run('(cons 1 false)')).toThrow()
      expect(() => lits.run('(cons 1 nil)')).toThrow()
      expect(() => lits.run('(cons 1 (object))')).toThrow()
      expect(() => lits.run('(cons 1 10)')).toThrow()
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
      expect(() => lits.run('(push nil 0 :2)')).toThrow()
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
      expect(() => lits.run('(pop nil)')).toThrow()
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
      expect(() => lits.run('(unshift nil 0 :2)')).toThrow()
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
      expect(() => lits.run('(shift nil)')).toThrow()
      expect(() => lits.run('(shift true)')).toThrow()
      expect(() => lits.run('(shift false)')).toThrow()
      expect(() => lits.run('(shift 1)')).toThrow()
      expect(() => lits.run('(shift)')).toThrow()
    })
  })

  describe('take', () => {
    it('samples', () => {
      expect(lits.run('(take 2 [1 2 3])')).toEqual([1, 2])
      expect(lits.run('(take 2 [])')).toEqual([])
      expect(lits.run('(take 20 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(take 0 [1 2 3])')).toEqual([])
      expect(lits.run('(take 2 "Albert")')).toEqual('Al')
      expect(lits.run('(take 2.01 "Albert")')).toEqual('Alb')

      expect(() => lits.run('(take 1 (object))')).toThrow()
      expect(() => lits.run('(take 1 nil)')).toThrow()
      expect(() => lits.run('(take 1 true)')).toThrow()
      expect(() => lits.run('(take 1 false)')).toThrow()
      expect(() => lits.run('(take :1 "Hej")')).toThrow()
      expect(() => lits.run('(take)')).toThrow()
      expect(() => lits.run('(take [1 2 3])')).toThrow()
      expect(() => lits.run('(take 1 2 [1 2 3])')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take 2 l1))
        (= l1 l2)
      `
      expect(lits.run(program)).toBe(false)
    })
  })

  describe('take-last', () => {
    it('samples', () => {
      expect(lits.run('(take-last 2 [1 2 3])')).toEqual([2, 3])
      expect(lits.run('(take-last 20 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(take-last 0 [1 2 3])')).toEqual([])
      expect(lits.run('(take-last 0.01 [1 2 3])')).toEqual([3])

      expect(() => lits.run('(take-last (object))')).toThrow()
      expect(() => lits.run('(take-last nil)')).toThrow()
      expect(() => lits.run('(take-last true)')).toThrow()
      expect(() => lits.run('(take-last false)')).toThrow()
      expect(() => lits.run('(take-last :1)')).toThrow()
      expect(() => lits.run('(take-last)')).toThrow()
      expect(() => lits.run('(take-last [1 2 3])')).toThrow()
      expect(() => lits.run('(take-last 1 2 [1 2 3])')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take-last 2 l1))
        (= l1 l2)
      `
      expect(lits.run(program)).toBe(false)
    })
  })

  describe('take-while', () => {
    it('samples', () => {
      expect(lits.run('(take-while (fn [x] (< x 3)) [1 2 3 2 1])')).toEqual([1, 2])
      expect(lits.run('(take-while (fn [x] (> x 3)) [1 2 3 2 1])')).toEqual([])
      expect(lits.run('(take-while (fn [x] (<= x :c)) "abcdabcd")')).toEqual('abc')

      expect(() => lits.run('(take-while (fn [x] (< x 3)) (object))')).toThrow()
      expect(() => lits.run('(take-while (fn [x] (< x 3)) nil)')).toThrow()
      expect(() => lits.run('(take-while (fn [x] (< x 3)) true)')).toThrow()
      expect(() => lits.run('(take-while (fn [x] (< x 3)) false)')).toThrow()
      expect(() => lits.run('(take-while 10 [1 2 3])')).toThrow()
      expect(() => lits.run('(take-while)')).toThrow()
      expect(() => lits.run('(take-while [1 2 3])')).toThrow()
      expect(() => lits.run('(take-while (fn [x] (< x 3)) [1 2 3] 1)')).toThrow()
    })
    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take-while (fn [x] (< x 3)) l1))
        (= l1 l2)
      `
      expect(lits.run(program)).toBe(false)
    })
  })

  describe('drop', () => {
    it('samples', () => {
      expect(lits.run('(drop 2 [1 2 3])')).toEqual([3])
      expect(lits.run('(drop 20 [1 2 3])')).toEqual([])
      expect(lits.run('(drop 0 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(drop 2 "Albert")')).toEqual('bert')
      expect(lits.run('(drop 0.5 [1 2 3])')).toEqual([2, 3])
      expect(lits.run('(drop -2 "Albert")')).toEqual('Albert')

      expect(() => lits.run('(drop 1 (object))')).toThrow()
      expect(() => lits.run('(drop 1 nil)')).toThrow()
      expect(() => lits.run('(drop 1 true)')).toThrow()
      expect(() => lits.run('(drop 1 false)')).toThrow()
      expect(() => lits.run('(drop :1 "Hej")')).toThrow()
      expect(() => lits.run('(drop)')).toThrow()
      expect(() => lits.run('(drop [1 2 3])')).toThrow()
      expect(() => lits.run('(drop 1 2 [1 2 3])')).toThrow()
    })

    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (drop 2 l1))
        (= l1 l2)
      `
      expect(lits.run(program)).toBe(false)
    })
  })

  describe('drop-last', () => {
    it('samples', () => {
      expect(lits.run('(drop-last 2 [1 2 3])')).toEqual([1])
      expect(lits.run('(drop-last 20 [1 2 3])')).toEqual([])
      expect(lits.run('(drop-last 0 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(drop-last 2 "Albert")')).toEqual('Albe')
      expect(lits.run('(drop-last 0.5 [1 2 3])')).toEqual([1, 2])
      expect(lits.run('(drop-last -2 "Albert")')).toEqual('Albert')

      expect(() => lits.run('(drop-last 1 (object))')).toThrow()
      expect(() => lits.run('(drop-last 1 nil)')).toThrow()
      expect(() => lits.run('(drop-last 1 true)')).toThrow()
      expect(() => lits.run('(drop-last 1 false)')).toThrow()
      expect(() => lits.run('(drop-last :1 "Hej")')).toThrow()
      expect(() => lits.run('(drop-last)')).toThrow()
      expect(() => lits.run('(drop-last [1 2 3])')).toThrow()
      expect(() => lits.run('(drop-last 1 2 [1 2 3])')).toThrow()
    })
  })

  describe('drop-while', () => {
    it('samples', () => {
      expect(lits.run('(drop-while (fn [x] (< x 3)) [1 2 3 2 1])')).toEqual([3, 2, 1])
      expect(lits.run('(drop-while (fn [x] (> x 3)) [1 2 3 2 1])')).toEqual([1, 2, 3, 2, 1])
      expect(lits.run('(drop-while (fn [x] (<= x :c)) "abcdab")')).toEqual('dab')

      expect(() => lits.run('(drop-while (fn [x] (< x 3)) (object))')).toThrow()
      expect(() => lits.run('(drop-while (fn [x] (< x 3)) nil)')).toThrow()
      expect(() => lits.run('(drop-while (fn [x] (< x 3)) true)')).toThrow()
      expect(() => lits.run('(drop-while (fn [x] (< x 3)) false)')).toThrow()
      expect(() => lits.run('(drop-while 10 [1 2 3])')).toThrow()
      expect(() => lits.run('(drop-while)')).toThrow()
      expect(() => lits.run('(drop-while [1 2 3])')).toThrow()
      expect(() => lits.run('(drop-while (fn [x] (< x 3)) [1 2 3] 1)')).toThrow()
    })
    it('new array created', () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take-while (fn [x] (< x 3)) l1))
        (= l1 l2)
      `
      expect(lits.run(program)).toBe(false)
    })
  })

  describe('sort', () => {
    it('samples', () => {
      expect(lits.run('(sort (fn [a b] (cond (< a b) -1 (> a b) 1 true 0)) [3 1 2])')).toEqual([1, 2, 3])
      expect(lits.run('(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true 0)) [3 1 2])')).toEqual([3, 2, 1])
      expect(lits.run('(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true 0)) [])')).toEqual([])

      expect(lits.run('(sort (fn [a b] (cond (< a b) 1 (> a b) -1 true 0)) "Albert")')).toBe('trlebA')

      expect(lits.run('(sort "Albert")')).toBe('Abelrt')
      expect(
        lits.run(
          '(sort [1 true 2 false  -100 nil (regexp "abc") (regexp "ABC") [] [1 2 3] [0 1 2] [0 0 0 0] {:a 1} {} "Albert" "albert"])',
        ),
      ).toMatchSnapshot()

      expect(() => lits.run('(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)) 10)')).toThrow()
      expect(() => lits.run('(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)))')).toThrow()
      expect(() => lits.run('(sort)')).toThrow()
    })
  })

  describe('join', () => {
    it('samples', () => {
      expect(lits.run('(join ["Albert" "Mojir"] " ")')).toBe('Albert Mojir')
      expect(lits.run('(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", ")')).toBe('0, 1, 2, 3, 4, 5, 6, 7, 8, 9')
      expect(() => lits.run('(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", " 5)')).toThrow()
      expect(() => lits.run('(join ["Albert" "Mojir"] " " -1)')).toThrow()
      expect(() => lits.run('(join ["Albert" "Mojir"])')).toThrow()
      expect(() => lits.run('(join ["Albert" 10] " ")')).toThrow()
    })
  })

  describe('random-sample!', () => {
    it('samples', () => {
      expect(lits.run('(random-sample! 1 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(random-sample! 1.9 [1 2 3])')).toEqual([1, 2, 3])
      expect(lits.run('(random-sample! 0 [1 2 3])')).toEqual([])
      expect(lits.run('(random-sample! -1.9 [1 2 3])')).toEqual([])

      expect(lits.run('(random-sample! 1 "Albert")')).toEqual('Albert')
      expect(lits.run('(random-sample! 0 "Albert")')).toEqual('')

      expect(() => lits.run('(random-sample! [1 2 3])')).toThrow()
      expect(() => lits.run('(random-sample! :1 [1 2 3])')).toThrow()
      expect(() => lits.run('(random-sample! 1)')).toThrow()
    })
  })

  describe('rand-nth!', () => {
    it('samples', () => {
      expect(lits.run('(rand-nth! [])')).toBeNull()
      expect(lits.run('(rand-nth! "")')).toBeNull()
      expect([1, 2, 3].includes(lits.run('(rand-nth! [1 2 3])') as number)).toBe(true)
      expect(typeof lits.run('(rand-nth! "Albert")')).toBe('string')
    })
  })

  describe('shuffle!', () => {
    it('samples', () => {
      expect(lits.run('(shuffle! [1 2 3])')).not.toEqual([1, 2, 3]) // Due to the shuffle algorithm, it will always differ
      expect(lits.run('(shuffle! "Albert")')).not.toBe('Albert') // Due to the shuffle algorithm, it will always differ
      expect(lits.run('(shuffle! [1 2])')).toEqual([2, 1]) // Due to the shuffle algorithm, first element connot be the same after shuffle
      expect(lits.run('(shuffle! [1])')).toEqual([1])
      expect(lits.run('(shuffle! [])')).toEqual([])
      expect(() => lits.run('(shuffle!)')).toThrow()
      expect(() => lits.run('(shuffle! [] [])')).toThrow()
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
      expect(lits.run('(remove even? [1 2 3 1 3 5])')).toEqual([1, 3, 1, 3, 5])
      expect(lits.run('(remove #(has? "aoueiyAOUEIY" %1) "Albert Mojir")')).toBe('lbrt Mjr')
      expect(() => lits.run('(remove)')).toThrow()
      expect(() => lits.run('(remove "Albert Mojir")')).toThrow()
      expect(() => lits.run('(remove #(has? "aoueiyAOUEIY" %1))')).toThrow()
      expect(() => lits.run('(remove #(has? "aoueiyAOUEIY" %1) "Albert" "Mojir")')).toThrow()
    })
  })

  describe('remove-at', () => {
    it('samples', () => {
      expect(lits.run('(remove-at -1 [1 2 3 4 5])')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('(remove-at 0 [1 2 3 4 5])')).toEqual([2, 3, 4, 5])
      expect(lits.run('(remove-at 2 [1 2 3 4 5])')).toEqual([1, 2, 4, 5])
      expect(lits.run('(remove-at 4 [1 2 3 4 5])')).toEqual([1, 2, 3, 4])
      expect(lits.run('(remove-at 5 [1 2 3 4 5])')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('(remove-at -1 "Mojir")')).toEqual('Mojir')
      expect(lits.run('(remove-at 0 "Mojir")')).toEqual('ojir')
      expect(lits.run('(remove-at 2 "Mojir")')).toEqual('Moir')
      expect(lits.run('(remove-at 4 "Mojir")')).toEqual('Moji')
      expect(lits.run('(remove-at 5 "Mojir")')).toEqual('Mojir')
      expect(() => lits.run('(remove-at)')).toThrow()
      expect(() => lits.run('(remove-at "Albert Mojir")')).toThrow()
      expect(() => lits.run('(remove-at 1)')).toThrow()
      expect(() => lits.run('(remove-at 1 "Albert" 2')).toThrow()
    })
  })

  describe('split-at', () => {
    it('samples', () => {
      expect(lits.run('(split-at 2 [1 2 3 4 5])')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('(split-at 0.01 [1 2 3 4 5])')).toEqual([[1], [2, 3, 4, 5]])
      expect(lits.run('(split-at 0 [1 2 3 4 5])')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split-at -1 [1 2 3 4 5])')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split-at 100 [1 2 3 4 5])')).toEqual([[1, 2, 3, 4, 5], []])
      expect(lits.run('(split-at 2 "Albert")')).toEqual(['Al', 'bert'])
      expect(lits.run('(split-at 0.01 "Albert")')).toEqual(['A', 'lbert'])
      expect(lits.run('(split-at 0 "Albert")')).toEqual(['', 'Albert'])
      expect(lits.run('(split-at -1 "Albert")')).toEqual(['', 'Albert'])
      expect(lits.run('(split-at 100 "Albert")')).toEqual(['Albert', ''])

      expect(() => lits.run('(split-at)')).toThrow()
      expect(() => lits.run('(split-at 3)')).toThrow()
      expect(() => lits.run('(split-at 3 "Albert" "Mojir")')).toThrow()
    })
  })

  describe('split-with', () => {
    it('samples', () => {
      expect(lits.run('(split-with #(< %1 3) [1 2 3 4 5])')).toEqual([
        [1, 2],
        [3, 4, 5],
      ])
      expect(lits.run('(split-with #(> %1 3) [1 2 3 4 5])')).toEqual([[], [1, 2, 3, 4, 5]])
      expect(lits.run('(split-with #(< %1 10) [1 2 3 4 5])')).toEqual([[1, 2, 3, 4, 5], []])

      expect(lits.run('(split-with #(<= %1 :Z) "Albert")')).toEqual(['A', 'lbert'])
      expect(lits.run('(split-with #(> %1 :Z) "Albert")')).toEqual(['', 'Albert'])
      expect(lits.run('(split-with #(<= %1 :z) "Albert")')).toEqual(['Albert', ''])

      expect(() => lits.run('(split-with)')).toThrow()
      expect(() => lits.run('(split-with #(<= %1 :Z))')).toThrow()
      expect(() => lits.run('(split-with #(<= %1 :Z) "Albert" "Mojir")')).toThrow()
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

  describe('group-by', () => {
    it('samples', () => {
      expect(lits.run('(group-by "name" [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}])')).toEqual({
        Albert: [{ name: 'Albert' }, { name: 'Albert' }],
        Mojir: [{ name: 'Mojir' }],
      })
      expect(lits.run('(group-by (fn [char] (if (has? "aoueiAOUEI" char) "vowel" "other")) "Albert Mojir")')).toEqual({
        other: ['l', 'b', 'r', 't', ' ', 'M', 'j', 'r'],
        vowel: ['A', 'e', 'o', 'i'],
      })
      expect(() => lits.run('(group-by)')).toThrow()
      expect(() => lits.run('(group-by :a)')).toThrow()
      expect(() => lits.run('(group-by :a {})')).toThrow()
      expect(() => lits.run('(group-by :a 3)')).toThrow()
      expect(() => lits.run('(group-by :a "" "")')).toThrow()
    })
  })

  describe('sort-by', () => {
    it('samples', () => {
      expect(lits.run('(sort-by count ["Albert" "Mojir" "Nina"])')).toEqual(['Nina', 'Mojir', 'Albert'])
      expect(lits.run('(sort-by count (fn [a b] (- b a)) ["Albert" "Mojir" "Nina"])')).toEqual([
        'Albert',
        'Mojir',
        'Nina',
      ])
      expect(lits.run('(sort-by lower-case "Albert")')).toEqual('Abelrt')
      expect(lits.run('(sort-by lower-case (fn [a b] (- (to-char-code b) (to-char-code a))) "Albert")')).toEqual(
        'trlebA',
      )
      expect(() => lits.run('(sort-by)')).toThrow()
      expect(() => lits.run('(sort-by :a)')).toThrow()
      expect(() => lits.run('(sort-by :a {})')).toThrow()
      expect(() => lits.run('(sort-by :a 3)')).toThrow()
      expect(() => lits.run('(sort-by :a "" "")')).toThrow()
    })
  })

  describe('partition', () => {
    it('samples', () => {
      expect(lits.run('(partition 4 (range 20))')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('(partition 4 (range 22))')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
        [16, 17, 18, 19],
      ])
      expect(lits.run('(partition 4 6 (range 20))')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
      ])
      expect(lits.run('(partition 4 3 (range 20))')).toEqual([
        [0, 1, 2, 3],
        [3, 4, 5, 6],
        [6, 7, 8, 9],
        [9, 10, 11, 12],
        [12, 13, 14, 15],
        [15, 16, 17, 18],
      ])
      expect(lits.run('(partition 3 6 [:a] (range 20))')).toEqual([
        [0, 1, 2],
        [6, 7, 8],
        [12, 13, 14],
        [18, 19, 'a'],
      ])
      expect(lits.run('(partition 4 6 [:a] (range 20))')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a'],
      ])
      expect(lits.run('(partition 4 6 [:a :b :c :d] (range 20))')).toEqual([
        [0, 1, 2, 3],
        [6, 7, 8, 9],
        [12, 13, 14, 15],
        [18, 19, 'a', 'b'],
      ])
      expect(lits.run('(partition 3 1 [:a :b :c :d :e :f])')).toEqual([
        ['a', 'b', 'c'],
        ['b', 'c', 'd'],
        ['c', 'd', 'e'],
        ['d', 'e', 'f'],
      ])
      expect(lits.run('(partition 10 [1 2 3 4])')).toEqual([])
      expect(lits.run('(partition 10 10 [1 2 3 4])')).toEqual([])
      expect(lits.run('(partition 10 10 [] [1 2 3 4])')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('(partition 10 10 nil [1 2 3 4])')).toEqual([[1, 2, 3, 4]])
      expect(lits.run('(partition 5 "superfragilistic")')).toEqual(['super', 'fragi', 'listi'])
      expect(lits.run('(partition 5 5 nil "superfragilistic")')).toEqual(['super', 'fragi', 'listi', 'c'])
      expect(lits.run('(def foo [5 6 7 8]) (partition 2 1 foo foo)')).toEqual([
        [5, 6],
        [6, 7],
        [7, 8],
        [8, 5],
      ])
      expect(() => lits.run('(partition 0 [1 2 3 4])')).toThrow()
      expect(() => lits.run('(partition 1)')).toThrow()
      expect(() => lits.run('(partition [1])')).toThrow()
    })
  })

  describe('partition-all', () => {
    it('samples', () => {
      expect(lits.run('(partition-all 4 [0 1 2 3 4 5 6 7 8 9])')).toEqual([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [8, 9],
      ])
      expect(lits.run('(partition-all 2 4 [0 1 2 3 4 5 6 7 8 9])')).toEqual([
        [0, 1],
        [4, 5],
        [8, 9],
      ])
      expect(() => lits.run('(partition-all 1)')).toThrow()
      expect(() => lits.run('(partition-all [1])')).toThrow()
    })
  })

  describe('partition-by', () => {
    it('samples', () => {
      expect(lits.run('(partition-by #(= 3 %1) [1 2 3 4 5])')).toEqual([[1, 2], [3], [4, 5]])
      expect(lits.run('(partition-by odd? [1 1 1 2 2 3 3])')).toEqual([
        [1, 1, 1],
        [2, 2],
        [3, 3],
      ])
      expect(lits.run('(partition-by identity "Leeeeeerrroyyy")')).toEqual(['L', 'eeeeee', 'rrr', 'o', 'yyy'])
      expect(() => lits.run('(partition-by odd?)')).toThrow()
      expect(() => lits.run('(partition-by [1 2 3])')).toThrow()
    })
  })
})
