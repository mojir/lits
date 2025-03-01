import { describe, expect, it } from 'vitest'
import { getLitsVariants } from '../../testUtils'

const lits = getLitsVariants()

describe('collection functions', () => {
  describe('count', () => {
    it('samples', () => {
      expect(lits.run('(count [])')).toBe(0)
      expect(lits.run('(count [1])')).toBe(1)
      expect(lits.run('(count [1 2 3])')).toBe(3)
      expect(lits.run('(count (object))')).toBe(0)
      expect(lits.run('(\'count\' (object :a 1 :b 2))')).toBe(2)
      expect(lits.run('(count "")')).toBe(0)
      expect(lits.run('(count "Albert")')).toBe(6)
      expect(lits.run('(count nil)')).toBe(0)

      expect(() => lits.run('(count)')).toThrow()
      expect(() => lits.run('(count [] [])')).toThrow()
      expect(() => lits.run('(count 12)')).toThrow()
      expect(() => lits.run('(count false)')).toThrow()
      expect(() => lits.run('(count true)')).toThrow()
      expect(() => lits.run('(count undefined)')).toThrow()
    })
  })

  describe('get', () => {
    it('samples', () => {
      expect(lits.run('(get [] 1)')).toBeNull()
      expect(lits.run('(get [1] 1)')).toBeNull()
      expect(lits.run('(get [1 2 3] 1)')).toBe(2)
      expect(lits.run('(get [] 1 :x)')).toBe('x')
      expect(lits.run('(get [1] 1 :x)')).toBe('x')
      expect(lits.run('(get [1 2 3] 1 :x)')).toBe(2)
      expect(lits.run('(get [1 2 3] -1)')).toBeNull()
      expect(lits.run('(get [1 2 3] -1 :x)')).toBe('x')

      expect(lits.run('(get "Albert" 1)')).toBe('l')
      expect(lits.run('(get "Albert" 7)')).toBeNull()
      expect(lits.run('(get "Albert" -1)')).toBeNull()
      expect(lits.run('(get "Albert" -1 :x)')).toBe('x')
      expect(lits.run('(get "" 0)')).toBeNull()

      expect(lits.run('(get (object) :a)')).toBeNull()
      expect(lits.run('(get (object :a 1 :b 2) :a)')).toBe(1)
      expect(lits.run('(get (object) :a :x)')).toBe('x')
      expect(lits.run('(get (object :a 1 :b 2) :a)')).toBe(1)

      expect(lits.run('(get nil 1)')).toBeNull()
      expect(lits.run('(get nil 1 99)')).toBe(99)

      expect(() => lits.run('(get)')).toThrow()
      expect(() => lits.run('(get [])')).toThrow()
      expect(() => lits.run('(get 12)')).toThrow()
      expect(() => lits.run('(get 12 1)')).toThrow()
      expect(() => lits.run('(get false)')).toThrow()
      expect(() => lits.run('(get false 2)')).toThrow()
      expect(() => lits.run('(get true)')).toThrow()
      expect(() => lits.run('(get nil)')).toThrow()
      expect(() => lits.run('(get undefined)')).toThrow()
    })
  })

  describe('get_in', () => {
    it('samples', () => {
      expect(lits.run('(get_in [] [1])')).toBeNull()
      expect(lits.run('(get_in [1] [1])')).toBeNull()
      expect(lits.run('(get_in [1 2 3] [1])')).toBe(2)
      expect(lits.run('(get_in [[1 2 3] [4 {:a 2} 6]] [1 1 :a])')).toBe(2)
      expect(lits.run('(get_in {:a ["Albert" "Mojir"]} [:a 0])')).toBe('Albert')
      expect(lits.run('(get_in {:a ["Albert" "Mojir"]} [:a 0 5])')).toBe('t')
      expect(lits.run('(get_in {:a ["Albert" "Mojir"]} [:a 0 5 0 0 0 0 0 0])')).toBe('t')
      expect(lits.run('(get_in {:a ["Albert" "Mojir"]} [:a 2] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get_in {:a ["Albert" "Mojir"]} [:a 2 :x] "DEFAULT")')).toBe('DEFAULT')

      expect(lits.run('(get_in nil [] "DEFAULT")')).toBeNull()
      expect(lits.run('(get_in nil [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get_in [] [] "DEFAULT")')).toEqual([])
      expect(lits.run('(get_in [1 2] [1] "DEFAULT")')).toBe(2)
      expect(lits.run('(get_in [1 2] [1 2] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get_in [] [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get_in 2 [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get_in 2 [] "DEFAULT")')).toBe(2)

      expect(lits.run('(get_in nil [])')).toBeNull()
      expect(lits.run('(get_in nil [1])')).toBeNull()
      expect(lits.run('(get_in [] [])')).toEqual([])
      expect(lits.run('(get_in [1 2] [1])')).toBe(2)
      expect(lits.run('(get_in [1 2] [1 2])')).toBeNull()
      expect(lits.run('(get_in [] [1])')).toBeNull()
      expect(lits.run('(get_in 2 [1])')).toBeNull()
      expect(lits.run('(get_in 2 [])')).toBe(2)

      expect(lits.run('(get_in "Albert" [])')).toBe('Albert')
      expect(lits.run('(get_in "Albert" [0])')).toBe('A')
      expect(lits.run('(get_in "Albert" [:0])')).toBeNull()

      expect(lits.run('(get_in "Albert" nil "DEFAULT")')).toBe('Albert')

      expect(() => lits.run('(get_in)')).toThrow()
      expect(() => lits.run('(get_in [])')).toThrow()
      expect(() => lits.run('(get_in 12)')).toThrow()
      expect(() => lits.run('(get_in false)')).toThrow()
      expect(() => lits.run('(get_in true)')).toThrow()
      expect(() => lits.run('(get_in nil)')).toThrow()
      expect(() => lits.run('(get_in undefined)')).toThrow()
    })
  })

  describe('contains?', () => {
    it('samples', () => {
      expect(lits.run('(contains? [] 1)')).toBe(false)
      expect(lits.run('(contains? [1] 1)')).toBe(false)
      expect(lits.run('(contains? [1 2 3] 1)')).toBe(true)
      expect(lits.run('(contains? (object) :a)')).toBe(false)
      expect(lits.run('(contains? (object :a 1 :b 2) :a)')).toBe(true)
      expect(lits.run('(contains? [] :1)')).toBe(false)
      expect(lits.run('(contains? [1] :1)')).toBe(false)
      expect(lits.run('(contains? [1 2 3] :1)')).toBe(false)
      expect(lits.run('(contains? (object) 1)')).toBe(false)
      expect(lits.run('(contains? (object :a 1 :b 2) 2)')).toBe(false)
      expect(lits.run('(contains? "Albert" 0)')).toBe(true)
      expect(lits.run('(contains? "Albert" 5)')).toBe(true)
      expect(lits.run('(contains? "Albert" 6)')).toBe(false)
      expect(lits.run('(contains? "Albert" -1)')).toBe(false)

      expect(lits.run('(contains? nil 1)')).toBe(false)
      expect(lits.run('(contains? nil :foo)')).toBe(false)

      expect(() => lits.run('(contains? "")')).toThrow()
      expect(() => lits.run('(contains? [])')).toThrow()
      expect(() => lits.run('(contains? "123")')).toThrow()
      expect(() => lits.run('(contains?)')).toThrow()
      expect(() => lits.run('(contains? [] [])')).toThrow()
      expect(() => lits.run('(contains? 12)')).toThrow()
      expect(() => lits.run('(contains? false)')).toThrow()
      expect(() => lits.run('(contains? true)')).toThrow()
      expect(() => lits.run('(contains? nil)')).toThrow()
      expect(() => lits.run('(contains? undefined)')).toThrow()
    })
  })

  describe('has?', () => {
    it('samples', () => {
      expect(lits.run('(has? [] 1)')).toBe(false)
      expect(lits.run('(has? [1] 1)')).toBe(true)
      expect(lits.run('(has? [1 2 3] 0)')).toBe(false)
      expect(lits.run('(has? (object) :a)')).toBe(false)
      expect(lits.run('(has? (object :a 1 :b 2) 1)')).toBe(true)
      expect(lits.run('(has? (object :a 1 :b 2) :a)')).toBe(false)
      expect(lits.run('(has? [] :1)')).toBe(false)
      expect(lits.run('(has? [1] :1)')).toBe(false)
      expect(lits.run('(has? [1 2 3] :1)')).toBe(false)
      expect(lits.run('(has? (object) 1)')).toBe(false)
      expect(lits.run('(has? "Albert" :A)')).toBe(true)
      expect(lits.run('(has? "Albert" :a)')).toBe(false)
      expect(lits.run('(has? "Albert" :bert)')).toBe(true)
      expect(lits.run('(has? "123" "12")')).toBe(true)
      expect(lits.run('(has? "Albert" 1)')).toBe(false)

      expect(lits.run('(has? nil 1)')).toBe(false)
      expect(lits.run('(has? nil nil)')).toBe(false)

      expect(() => lits.run('(has? "")')).toThrow()
      expect(() => lits.run('(has? [])')).toThrow()
      expect(() => lits.run('(has? "123")')).toThrow()
      expect(() => lits.run('(has?)')).toThrow()
      expect(() => lits.run('(has? 12 [])')).toThrow()
      expect(() => lits.run('(has? 12)')).toThrow()
      expect(() => lits.run('(has? false)')).toThrow()
      expect(() => lits.run('(has? true)')).toThrow()
      expect(() => lits.run('(has? nil)')).toThrow()
    })
  })

  describe('has_some?', () => {
    it('samples', () => {
      expect(lits.run('(has_some? [] [1])')).toBe(false)
      expect(lits.run('(has_some? [1] [])')).toBe(false)
      expect(lits.run('(has_some? [1] nil)')).toBe(false)
      expect(lits.run('(has_some? [1] [1])')).toBe(true)
      expect(lits.run('(has_some? [1 2 3] [0])')).toBe(false)
      expect(lits.run('(has_some? [1 2 3] [0 1])')).toBe(true)
      expect(lits.run('(has_some? (object) [:a])')).toBe(false)
      expect(lits.run('(has_some? (object :a 1 :b 2) [0])')).toBe(false)
      expect(lits.run('(has_some? (object :a 1 :b 2) [0 1])')).toBe(true)
      expect(lits.run('(has_some? "Albert" "xyz")')).toBe(false)
      expect(lits.run('(has_some? "Albert" "")')).toBe(false)
      expect(lits.run('(has_some? "Albert" ["Alb" "ert"])')).toBe(false)
      expect(lits.run('(has_some? "Albert" ["A"])')).toBe(true)
      expect(lits.run('(has_some? "Albert" "xyzl")')).toBe(true)
      expect(lits.run('(has_some? [:a :b :c :d] "xyz")')).toBe(false)
      expect(lits.run('(has_some? [:a :b :c :d] "xyzc")')).toBe(true)

      expect(lits.run('(has_some? nil "abc")')).toBe(false)
      expect(lits.run('(has_some? nil [0, 1, nil])')).toBe(false)
      expect(lits.run('(has_some? nil nil)')).toBe(false)

      expect(() => lits.run('(has_some? [] [1] 1)')).toThrow()
      expect(() => lits.run('(has_some? [] 4)')).toThrow()
      expect(() => lits.run('(has_some? [] true)')).toThrow()
      expect(() => lits.run('(has_some? [] false)')).toThrow()
      expect(() => lits.run('(has_some? [] odd?)')).toThrow()
      expect(() => lits.run('(has_some? [] {})')).toThrow()
      expect(() => lits.run('(has_some? true [1])')).toThrow()
      expect(() => lits.run('(has_some? false [1])')).toThrow()
      expect(() => lits.run('(has_some? odd? [1])')).toThrow()
      expect(() => lits.run('(has_some? 3 [1])')).toThrow()
    })
  })

  describe('has_every?', () => {
    it('samples', () => {
      expect(lits.run('(has_every? [] [1])')).toBe(false)
      expect(lits.run('(has_every? [1] [])')).toBe(true)
      expect(lits.run('(has_every? [1] nil)')).toBe(true)
      expect(lits.run('(has_every? [1] [1])')).toBe(true)
      expect(lits.run('(has_every? [1 2 3] [0 1])')).toBe(false)
      expect(lits.run('(has_every? [1 2 3] [1 2])')).toBe(true)
      expect(lits.run('(has_every? (object) [:a])')).toBe(false)
      expect(lits.run('(has_every? (object :a 1 :b 2) [0 1])')).toBe(false)
      expect(lits.run('(has_every? (object :a 1 :b 2) [1 2])')).toBe(true)
      expect(lits.run('(has_every? "Albert" "xyz")')).toBe(false)
      expect(lits.run('(has_every? "Albert" ["Alb" "ert"])')).toBe(false)
      expect(lits.run('(has_every? "Albert" ["A"])')).toBe(true)
      expect(lits.run('(has_every? "Albert" "treblA")')).toBe(true)
      expect(lits.run('(has_every? [:a :b :c :d] "xyz")')).toBe(false)
      expect(lits.run('(has_every? [:a :b :c :d] "dcba")')).toBe(true)

      expect(lits.run('(has_every? nil "abc")')).toBe(false)
      expect(lits.run('(has_every? nil [0, 1, nil])')).toBe(false)
      expect(lits.run('(has_every? nil nil)')).toBe(false)
      expect(lits.run('(has_every? [1, 2, 3] nil)')).toBe(true)

      expect(() => lits.run('(has_every? [] [1] 1)')).toThrow()
      expect(() => lits.run('(has_every? [] 4)')).toThrow()
      expect(() => lits.run('(has_every? [] true)')).toThrow()
      expect(() => lits.run('(has_every? [] false)')).toThrow()
      expect(() => lits.run('(has_every? [] odd?)')).toThrow()
      expect(() => lits.run('(has_every? [] {})')).toThrow()
      expect(() => lits.run('(has_every? true [1])')).toThrow()
      expect(() => lits.run('(has_every? false [1])')).toThrow()
      expect(() => lits.run('(has_every? odd? [1])')).toThrow()
      expect(() => lits.run('(has_every? 3 [1])')).toThrow()
    })
  })

  describe('assoc', () => {
    it('samples', () => {
      expect(lits.run('(assoc [1 2 3] 0 :1)')).toEqual(['1', 2, 3])
      expect(lits.run('(assoc [1 2 3] 1 :2)')).toEqual([1, '2', 3])
      expect(lits.run('(def a [1 2 3]) (assoc a 1 :2)')).toEqual([1, '2', 3])
      expect(lits.run('(def a [1 2 3]) (assoc a 1 :2) a')).toEqual([1, 2, 3])
      expect(lits.run('(assoc [1 2 3] 3 :4)')).toEqual([1, 2, 3, '4'])

      expect(lits.run('(assoc {} :a :1)')).toEqual({ a: '1' })

      expect(lits.run('(assoc {:a 1 :b 2} :a :1)')).toEqual({ a: '1', b: 2 })
      expect(lits.run('(assoc {:a 1 :b 2} :b :2)')).toEqual({ a: 1, b: '2' })
      expect(lits.run('(def o {:a 1 :b 2}) (assoc o :a :1)')).toEqual({ a: '1', b: 2 })
      expect(lits.run('(def o {:a 1 :b 2}) (assoc o :a :1) o')).toEqual({ a: 1, b: 2 })

      expect(lits.run('(assoc :1 0 :2)')).toBe('2')
      expect(lits.run('(assoc "Albert" 6 "!")')).toBe('Albert!')

      expect(() => lits.run('(assoc "Albert" 7 "!")')).toThrow()
      expect(() => lits.run('(assoc [1 2 3] 4 :4)')).toThrow()
      expect(() => lits.run('(assoc (object) 0 :2)')).toThrow()
      expect(() => lits.run('(assoc nil 0 :2)')).toThrow()
      expect(() => lits.run('(assoc undefined 0 :2)')).toThrow()
      expect(() => lits.run('(assoc true 0 :2)')).toThrow()
      expect(() => lits.run('(assoc false 0 :2)')).toThrow()
      expect(() => lits.run('(assoc 1 0 :2)')).toThrow()
      expect(() => lits.run('(assoc :1 0 "22")')).toThrow()
      expect(() => lits.run('(assoc [1] :0 :2)')).toThrow()
      expect(() => lits.run('(assoc [1] true :2)')).toThrow()
      expect(() => lits.run('(assoc [1] false :2)')).toThrow()
      expect(() => lits.run('(assoc [1] [] :2)')).toThrow()
      expect(() => lits.run('(assoc [1] nil :2)')).toThrow()
      expect(() => lits.run('(assoc [1] undefined :2)')).toThrow()
      expect(() => lits.run('(assoc 0 :2)')).toThrow()
      expect(() => lits.run('(assoc [1 2 3] -1 :x)')).toThrow()
      expect(() => lits.run('(assoc [1 2 3] 4 :x)')).toThrow()
      expect(() => lits.run('(assoc)')).toThrow()
      expect(() => lits.run('(assoc [])')).toThrow()
      expect(() => lits.run('(assoc [] 0)')).toThrow()
      expect(() => lits.run('(assoc [] 0 :x :y)')).toThrow()
      expect(() => lits.run('(assoc [] :a :1)')).toThrow()
    })
  })

  describe('assoc_in', () => {
    it('samples', () => {
      expect(lits.run('(assoc_in "Albert" [0] :a)')).toEqual('albert')
      expect(lits.run('(assoc_in "Albert" [6] "!")')).toEqual('Albert!')
      expect(() => lits.run('(assoc_in "Albert" [7] "!")')).toThrow()
      expect(lits.run('(assoc_in {} [:a :b :c] "Albert")')).toEqual({ a: { b: { c: 'Albert' } } })
      expect(lits.run('(assoc_in [1 2 3] [0] :1)')).toEqual(['1', 2, 3])
      expect(lits.run('(assoc_in [1 2 [1 2 3]] [2 1] :2)')).toEqual([1, 2, [1, '2', 3]])
      expect(lits.run('(assoc_in [1 2 "albert"] [2 0] :A)')).toEqual([1, 2, 'Albert'])
      expect(lits.run('(assoc_in [1 2 {"name" "albert"}] [2 "name"] :A)')).toEqual([1, 2, { name: 'A' }])
      expect(lits.run('(assoc_in [1 2 {"name" "albert"}] [2 "name" 0] :A)')).toEqual([1, 2, { name: 'Albert' }])
      expect(() => lits.run('(assoc_in [1 2 {"name" "albert"}] [:2 "name" 0] :A)')).toThrow()
      expect(() => lits.run('(assoc_in [1 2 {"name" "albert"}] [2 1 0] :A)')).toThrow()
      expect(() => lits.run('(assoc_in [1 2 {"name" "albert"}] [2 "name" :a] :A)')).toThrow()
    })
  })

  describe('concat', () => {
    it('samples', () => {
      expect(lits.run('(concat [])')).toEqual([])
      expect(lits.run('(concat [1])')).toEqual([1])
      expect(lits.run('(concat [1] [2] [3 4])')).toEqual([1, 2, 3, 4])
      expect(lits.run('(concat [1 2 3] [])')).toEqual([1, 2, 3])

      expect(lits.run('(concat {:a 1 :b 2} {:b 1 :c 2})')).toEqual({ a: 1, b: 1, c: 2 })
      expect(lits.run('(concat {} {:a 1 :b 2})')).toEqual({ a: 1, b: 2 })

      expect(lits.run('(concat :1 "23")')).toBe('123')
      expect(lits.run('(concat :1 "")')).toBe('1')
      expect(lits.run('(concat :1)')).toBe('1')

      expect(() => lits.run('(concat)')).toThrow()
      expect(() => lits.run('(concat [1] :2)')).toThrow()
      expect(() => lits.run('(concat :1 [:2])')).toThrow()
      expect(() => lits.run('(concat 0)')).toThrow()
      expect(() => lits.run('(concat true)')).toThrow()
      expect(() => lits.run('(concat :1 false)')).toThrow()
      expect(() => lits.run('(concat nil :m)')).toThrow()
      expect(() => lits.run('(concat undefined)')).toThrow()
    })
  })

  describe('not_empty', () => {
    it('samples', () => {
      expect(lits.run('(not_empty [])')).toBeNull()
      expect(lits.run('(not_empty [0])')).toEqual([0])
      expect(lits.run('(not_empty {})')).toBeNull()
      expect(lits.run('(not_empty {:a 2})')).toEqual({ a: 2 })
      expect(lits.run('(not_empty "")')).toBeNull()
      expect(lits.run('(not_empty "Albert")')).toEqual('Albert')

      expect(lits.run('(not_empty nil)')).toBeNull()

      expect(() => lits.run('(not_empty)')).toThrow()
      expect(() => lits.run('(not_empty)')).toThrow()
      expect(() => lits.run('(not_empty true)')).toThrow()
      expect(() => lits.run('(not_empty false)')).toThrow()
      expect(() => lits.run('(not_empty undefined)')).toThrow()
      expect(() => lits.run('(not_empty 10)')).toThrow()
      expect(() => lits.run('(not_empty (regexp "^start"))')).toThrow()
    })
  })

  describe('every?', () => {
    it('samples', () => {
      expect(lits.run('(every? [1 2 3] number?)')).toBe(true)
      expect(lits.run('(every? [:1 :2 :3] number?)')).toBe(false)
      expect(lits.run('(every? [] number?)')).toBe(true)
      expect(lits.run('(every? "" number?)')).toBe(true)
      expect(lits.run('(every? {} number?)')).toBe(true)
      expect(lits.run('(every? [2 4 6] (fn [x] (zero? (mod x 2))))')).toBe(true)
      expect(lits.run('(every? "abc" (fn [x] (>= x :a)))')).toBe(true)
      expect(lits.run('(every? "abC" (fn [x] (>= x :a)))')).toBe(false)
      expect(lits.run('(every? {:a 2 :b 4} #(even? (second %1)))')).toBe(true)
      expect(lits.run('(every? {:a 2 :b 3} #(even? (second %1)))')).toBe(false)
      expect(lits.run('(every? {:a 2 :b 3} #(even? (second %1)))')).toBe(false)
      expect(() => lits.run('(every? +)')).toThrow()
      expect(() => lits.run('(every? [])')).toThrow()
      expect(() => lits.run('(every?)')).toThrow()
      expect(() => lits.run('(every? [1] number? 2)')).toThrow()
    })
  })

  describe('not_every?', () => {
    it('samples', () => {
      expect(lits.run('(not_every? [1 2 3] number?)')).toBe(false)
      expect(lits.run('(not_every? [:1 :2 :3] number?)')).toBe(true)
      expect(lits.run('(not_every? [] number?)')).toBe(false)
      expect(lits.run('(not_every? "" number?)')).toBe(false)
      expect(lits.run('(not_every? {} number?)')).toBe(false)
      expect(lits.run('(not_every? [2 4 6] (fn [x] (zero? (mod x 2))))')).toBe(false)
      expect(lits.run('(not_every? "abc" (fn [x] (>= x :a)))')).toBe(false)
      expect(lits.run('(not_every? "abC" (fn [x] (>= x :a)))')).toBe(true)
      expect(lits.run('(not_every? {:a 2 :b 4} #(even? (second %1)))')).toBe(false)
      expect(lits.run('(not_every? {:a 2 :b 3} #(even? (second %1)))')).toBe(true)
      expect(lits.run('(not_every? {:a 2 :b 3} #(even? (second %1)))')).toBe(true)
      expect(() => lits.run('(not_every? +)')).toThrow()
      expect(() => lits.run('(not_every? [])')).toThrow()
      expect(() => lits.run('(not_every?)')).toThrow()
      expect(() => lits.run('(not_every? [1] number? 2)')).toThrow()
    })
  })

  describe('any?', () => {
    it('samples', () => {
      expect(lits.run('(any? [1 2 3] number?)')).toBe(true)
      expect(lits.run('(any? [1 :2 3] number?)')).toBe(true)
      expect(lits.run('(any? [:1 :2 :3] number?)')).toBe(false)
      expect(lits.run('(any? [] number?)')).toBe(false)
      expect(lits.run('(any? "" number?)')).toBe(false)
      expect(lits.run('(any? {} number?)')).toBe(false)
      expect(lits.run('(any? [1 3 6] (fn [x] (zero? (mod x 2))))')).toBe(true)
      expect(lits.run('(any? [1 3 5] (fn [x] (zero? (mod x 2))))')).toBe(false)
      expect(lits.run('(any? "abc" (fn [x] (>= x :a)))')).toBe(true)
      expect(lits.run('(any? "abC" (fn [x] (>= x :a)))')).toBe(true)
      expect(lits.run('(any? "ABC" (fn [x] (>= x :a)))')).toBe(false)
      expect(lits.run('(any? {:a 2 :b 4} #(even? (second %1)))')).toBe(true)
      expect(lits.run('(any? {:a 2 :b 3} #(even? (second %1)))')).toBe(true)
      expect(lits.run('(any? {:a 1 :b 3} #(even? (second %1)))')).toBe(false)
      expect(() => lits.run('(any? +)')).toThrow()
      expect(() => lits.run('(any? [])')).toThrow()
      expect(() => lits.run('(any?)')).toThrow()
      expect(() => lits.run('(any? [1] number? 2)')).toThrow()
    })
  })

  describe('not_any?', () => {
    it('samples', () => {
      expect(lits.run('(not_any? [1 2 3] number?)')).toBe(false)
      expect(lits.run('(not_any? [1 :2 3] number?)')).toBe(false)
      expect(lits.run('(not_any? [:1 :2 :3] number?)')).toBe(true)
      expect(lits.run('(not_any? [] number?)')).toBe(true)
      expect(lits.run('(not_any? "" number?)')).toBe(true)
      expect(lits.run('(not_any? {} number?)')).toBe(true)
      expect(lits.run('(not_any? [1 3 6] (fn [x] (zero? (mod x 2))))')).toBe(false)
      expect(lits.run('(not_any? [1 3 5] (fn [x] (zero? (mod x 2))))')).toBe(true)
      expect(lits.run('(not_any? "abc" (fn [x] (>= x :a)))')).toBe(false)
      expect(lits.run('(not_any? "abC" (fn [x] (>= x :a)))')).toBe(false)
      expect(lits.run('(not_any? "ABC" (fn [x] (>= x :a)))')).toBe(true)
      expect(lits.run('(not_any? {:a 2 :b 4} #(even? (second %1)))')).toBe(false)
      expect(lits.run('(not_any? {:a 2 :b 3} #(even? (second %1)))')).toBe(false)
      expect(lits.run('(not_any? {:a 1 :b 3} #(even? (second %1)))')).toBe(true)
      expect(() => lits.run('(not_any? +)')).toThrow()
      expect(() => lits.run('(not_any? [])')).toThrow()
      expect(() => lits.run('(not_any?)')).toThrow()
      expect(() => lits.run('(not_any? [1] number? 2)')).toThrow()
    })
  })

  describe('update', () => {
    it('samples', () => {
      expect(
        lits.run(
          '(def x "Albert") (update x 3 (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albfrt')
      expect(
        lits.run(
          '(def x "Albert") (update x 6 (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albert!')

      expect(lits.run('(def x [0, 1, 2, 3]) (update x 3 inc)')).toEqual([0, 1, 2, 4])
      expect(lits.run('(def x [0, 1, 2, 3]) (update x 4 identity)')).toEqual([0, 1, 2, 3, null])

      expect(lits.run('(def x {:a 1 :b 2}) (update x :a inc)')).toEqual({ a: 2, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update x :a + 10)')).toEqual({ a: 11, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update x :a (fn [val] (if (even? val) 0 (inc val))))')).toEqual({
        a: 2,
        b: 2,
      })
      expect(lits.run('(def x {:a 1 :b 2}) (:c x)')).toEqual(null)
      expect(lits.run('(update {} :a (fn [val] (when (nil? val) 0)))')).toEqual({ a: 0 })
      expect(lits.run('(def x {:a 1 :b 2}) (update x :c (fn [val] (if (nil? val) 0 (inc val))))')).toEqual({
        a: 1,
        b: 2,
        c: 0,
      })
      expect(() => lits.run('(update number? [1] 2)')).toThrow()
    })
  })

  describe('update_in', () => {
    it('samples', () => {
      expect(
        lits.run(
          '(def x "Albert") (update_in x [3] (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albfrt')
      expect(
        lits.run(
          '(def x "Albert") (update_in x [6] (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albert!')

      expect(lits.run('(def x [0, 1, 2, 3]) (update_in x [3] inc)')).toEqual([0, 1, 2, 4])
      expect(lits.run('(def x [0, 1, 2, 3]) (update_in x [4] identity)')).toEqual([0, 1, 2, 3, null])

      expect(lits.run('(def x {:a 1 :b 2}) (update_in x [:a] inc)')).toEqual({ a: 2, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update_in x [:a] + 10)')).toEqual({ a: 11, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update_in x [:a] (fn [val] (if (even? val) 0 (inc val))))')).toEqual({
        a: 2,
        b: 2,
      })
      expect(lits.run('(update_in {} [:a] (fn [val] (when (nil? val) 0)))')).toEqual({ a: 0 })
      expect(lits.run('(def x {:a 1 :b 2}) (update_in x [:c] (fn [val] (if (nil? val) 0 (inc val))))')).toEqual({
        a: 1,
        b: 2,
        c: 0,
      })
      expect(lits.run('(update_in {:a [1 2 3]} [:a 1] (fn [val] (when (nil? val) 0)))')).toEqual({
        a: [1, null, 3],
      })
      expect(lits.run('(update_in {:a [1 nil 3]} [:a 1] (fn [val] (when (nil? val) 0)))')).toEqual({
        a: [1, 0, 3],
      })
      expect(lits.run('(update_in {:a [1 "Albert" 3]} [:a 1 0] (fn [val] (if (nil? val) "?" "!")))')).toEqual({
        a: [1, '!lbert', 3],
      })
      expect(lits.run('(update_in {:a [1 "" 3]} [:a 1 0] (fn [val] (if (nil? val) "?" "!")))')).toEqual({
        a: [1, '?', 3],
      })
    })
  })
})
