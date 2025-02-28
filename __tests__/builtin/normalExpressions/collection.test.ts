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
      expect(lits.run('(count (object :a 1 :b 2))')).toBe(2)
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

  describe('get-in', () => {
    it('samples', () => {
      expect(lits.run('(get-in [] [1])')).toBeNull()
      expect(lits.run('(get-in [1] [1])')).toBeNull()
      expect(lits.run('(get-in [1 2 3] [1])')).toBe(2)
      expect(lits.run('(get-in [[1 2 3] [4 {:a 2} 6]] [1 1 :a])')).toBe(2)
      expect(lits.run('(get-in {:a ["Albert" "Mojir"]} [:a 0])')).toBe('Albert')
      expect(lits.run('(get-in {:a ["Albert" "Mojir"]} [:a 0 5])')).toBe('t')
      expect(lits.run('(get-in {:a ["Albert" "Mojir"]} [:a 0 5 0 0 0 0 0 0])')).toBe('t')
      expect(lits.run('(get-in {:a ["Albert" "Mojir"]} [:a 2] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get-in {:a ["Albert" "Mojir"]} [:a 2 :x] "DEFAULT")')).toBe('DEFAULT')

      expect(lits.run('(get-in nil [] "DEFAULT")')).toBeNull()
      expect(lits.run('(get-in nil [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get-in [] [] "DEFAULT")')).toEqual([])
      expect(lits.run('(get-in [1 2] [1] "DEFAULT")')).toBe(2)
      expect(lits.run('(get-in [1 2] [1 2] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get-in [] [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get-in 2 [1] "DEFAULT")')).toBe('DEFAULT')
      expect(lits.run('(get-in 2 [] "DEFAULT")')).toBe(2)

      expect(lits.run('(get-in nil [])')).toBeNull()
      expect(lits.run('(get-in nil [1])')).toBeNull()
      expect(lits.run('(get-in [] [])')).toEqual([])
      expect(lits.run('(get-in [1 2] [1])')).toBe(2)
      expect(lits.run('(get-in [1 2] [1 2])')).toBeNull()
      expect(lits.run('(get-in [] [1])')).toBeNull()
      expect(lits.run('(get-in 2 [1])')).toBeNull()
      expect(lits.run('(get-in 2 [])')).toBe(2)

      expect(lits.run('(get-in "Albert" [])')).toBe('Albert')
      expect(lits.run('(get-in "Albert" [0])')).toBe('A')
      expect(lits.run('(get-in "Albert" [:0])')).toBeNull()

      expect(lits.run('(get-in "Albert" nil "DEFAULT")')).toBe('Albert')

      expect(() => lits.run('(get-in)')).toThrow()
      expect(() => lits.run('(get-in [])')).toThrow()
      expect(() => lits.run('(get-in 12)')).toThrow()
      expect(() => lits.run('(get-in false)')).toThrow()
      expect(() => lits.run('(get-in true)')).toThrow()
      expect(() => lits.run('(get-in nil)')).toThrow()
      expect(() => lits.run('(get-in undefined)')).toThrow()
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

  describe('has-some?', () => {
    it('samples', () => {
      expect(lits.run('(has-some? [] [1])')).toBe(false)
      expect(lits.run('(has-some? [1] [])')).toBe(false)
      expect(lits.run('(has-some? [1] nil)')).toBe(false)
      expect(lits.run('(has-some? [1] [1])')).toBe(true)
      expect(lits.run('(has-some? [1 2 3] [0])')).toBe(false)
      expect(lits.run('(has-some? [1 2 3] [0 1])')).toBe(true)
      expect(lits.run('(has-some? (object) [:a])')).toBe(false)
      expect(lits.run('(has-some? (object :a 1 :b 2) [0])')).toBe(false)
      expect(lits.run('(has-some? (object :a 1 :b 2) [0 1])')).toBe(true)
      expect(lits.run('(has-some? "Albert" "xyz")')).toBe(false)
      expect(lits.run('(has-some? "Albert" "")')).toBe(false)
      expect(lits.run('(has-some? "Albert" ["Alb" "ert"])')).toBe(false)
      expect(lits.run('(has-some? "Albert" ["A"])')).toBe(true)
      expect(lits.run('(has-some? "Albert" "xyzl")')).toBe(true)
      expect(lits.run('(has-some? [:a :b :c :d] "xyz")')).toBe(false)
      expect(lits.run('(has-some? [:a :b :c :d] "xyzc")')).toBe(true)

      expect(lits.run('(has-some? nil "abc")')).toBe(false)
      expect(lits.run('(has-some? nil [0, 1, nil])')).toBe(false)
      expect(lits.run('(has-some? nil nil)')).toBe(false)

      expect(() => lits.run('(has-some? [] [1] 1)')).toThrow()
      expect(() => lits.run('(has-some? [] 4)')).toThrow()
      expect(() => lits.run('(has-some? [] true)')).toThrow()
      expect(() => lits.run('(has-some? [] false)')).toThrow()
      expect(() => lits.run('(has-some? [] odd?)')).toThrow()
      expect(() => lits.run('(has-some? [] {})')).toThrow()
      expect(() => lits.run('(has-some? true [1])')).toThrow()
      expect(() => lits.run('(has-some? false [1])')).toThrow()
      expect(() => lits.run('(has-some? odd? [1])')).toThrow()
      expect(() => lits.run('(has-some? 3 [1])')).toThrow()
    })
  })

  describe('has-every?', () => {
    it('samples', () => {
      expect(lits.run('(has-every? [] [1])')).toBe(false)
      expect(lits.run('(has-every? [1] [])')).toBe(true)
      expect(lits.run('(has-every? [1] nil)')).toBe(true)
      expect(lits.run('(has-every? [1] [1])')).toBe(true)
      expect(lits.run('(has-every? [1 2 3] [0 1])')).toBe(false)
      expect(lits.run('(has-every? [1 2 3] [1 2])')).toBe(true)
      expect(lits.run('(has-every? (object) [:a])')).toBe(false)
      expect(lits.run('(has-every? (object :a 1 :b 2) [0 1])')).toBe(false)
      expect(lits.run('(has-every? (object :a 1 :b 2) [1 2])')).toBe(true)
      expect(lits.run('(has-every? "Albert" "xyz")')).toBe(false)
      expect(lits.run('(has-every? "Albert" ["Alb" "ert"])')).toBe(false)
      expect(lits.run('(has-every? "Albert" ["A"])')).toBe(true)
      expect(lits.run('(has-every? "Albert" "treblA")')).toBe(true)
      expect(lits.run('(has-every? [:a :b :c :d] "xyz")')).toBe(false)
      expect(lits.run('(has-every? [:a :b :c :d] "dcba")')).toBe(true)

      expect(lits.run('(has-every? nil "abc")')).toBe(false)
      expect(lits.run('(has-every? nil [0, 1, nil])')).toBe(false)
      expect(lits.run('(has-every? nil nil)')).toBe(false)
      expect(lits.run('(has-every? [1, 2, 3] nil)')).toBe(true)

      expect(() => lits.run('(has-every? [] [1] 1)')).toThrow()
      expect(() => lits.run('(has-every? [] 4)')).toThrow()
      expect(() => lits.run('(has-every? [] true)')).toThrow()
      expect(() => lits.run('(has-every? [] false)')).toThrow()
      expect(() => lits.run('(has-every? [] odd?)')).toThrow()
      expect(() => lits.run('(has-every? [] {})')).toThrow()
      expect(() => lits.run('(has-every? true [1])')).toThrow()
      expect(() => lits.run('(has-every? false [1])')).toThrow()
      expect(() => lits.run('(has-every? odd? [1])')).toThrow()
      expect(() => lits.run('(has-every? 3 [1])')).toThrow()
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

  describe('assoc-in', () => {
    it('samples', () => {
      expect(lits.run('(assoc-in "Albert" [0] :a)')).toEqual('albert')
      expect(lits.run('(assoc-in "Albert" [6] "!")')).toEqual('Albert!')
      expect(() => lits.run('(assoc-in "Albert" [7] "!")')).toThrow()
      expect(lits.run('(assoc-in {} [:a :b :c] "Albert")')).toEqual({ a: { b: { c: 'Albert' } } })
      expect(lits.run('(assoc-in [1 2 3] [0] :1)')).toEqual(['1', 2, 3])
      expect(lits.run('(assoc-in [1 2 [1 2 3]] [2 1] :2)')).toEqual([1, 2, [1, '2', 3]])
      expect(lits.run('(assoc-in [1 2 "albert"] [2 0] :A)')).toEqual([1, 2, 'Albert'])
      expect(lits.run('(assoc-in [1 2 {"name" "albert"}] [2 "name"] :A)')).toEqual([1, 2, { name: 'A' }])
      expect(lits.run('(assoc-in [1 2 {"name" "albert"}] [2 "name" 0] :A)')).toEqual([1, 2, { name: 'Albert' }])
      expect(() => lits.run('(assoc-in [1 2 {"name" "albert"}] [:2 "name" 0] :A)')).toThrow()
      expect(() => lits.run('(assoc-in [1 2 {"name" "albert"}] [2 1 0] :A)')).toThrow()
      expect(() => lits.run('(assoc-in [1 2 {"name" "albert"}] [2 "name" :a] :A)')).toThrow()
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

  describe('not-empty', () => {
    it('samples', () => {
      expect(lits.run('(not-empty [])')).toBeNull()
      expect(lits.run('(not-empty [0])')).toEqual([0])
      expect(lits.run('(not-empty {})')).toBeNull()
      expect(lits.run('(not-empty {:a 2})')).toEqual({ a: 2 })
      expect(lits.run('(not-empty "")')).toBeNull()
      expect(lits.run('(not-empty "Albert")')).toEqual('Albert')

      expect(lits.run('(not-empty nil)')).toBeNull()

      expect(() => lits.run('(not-empty)')).toThrow()
      expect(() => lits.run('(not-empty)')).toThrow()
      expect(() => lits.run('(not-empty true)')).toThrow()
      expect(() => lits.run('(not-empty false)')).toThrow()
      expect(() => lits.run('(not-empty undefined)')).toThrow()
      expect(() => lits.run('(not-empty 10)')).toThrow()
      expect(() => lits.run('(not-empty (regexp "^start"))')).toThrow()
    })
  })

  describe('every?', () => {
    it('samples', () => {
      expect(lits.run('(every? number? [1 2 3])')).toBe(true)
      expect(lits.run('(every? number? [:1 :2 :3])')).toBe(false)
      expect(lits.run('(every? number? [])')).toBe(true)
      expect(lits.run('(every? number? "")')).toBe(true)
      expect(lits.run('(every? number? {})')).toBe(true)
      expect(lits.run('(every? (fn [x] (zero? (mod x 2))) [2 4 6])')).toBe(true)
      expect(lits.run('(every? (fn [x] (>= x :a)) "abc")')).toBe(true)
      expect(lits.run('(every? (fn [x] (>= x :a)) "abC")')).toBe(false)
      expect(lits.run('(every? #(even? (second %1)) {:a 2 :b 4})')).toBe(true)
      expect(lits.run('(every? #(even? (second %1)) {:a 2 :b 3})')).toBe(false)
      expect(lits.run('(every? #(even? (second %1)) {:a 2 :b 3})')).toBe(false)
      expect(() => lits.run('(every? +)')).toThrow()
      expect(() => lits.run('(every?)')).toThrow()
      expect(() => lits.run('(every? number? [1] 2)')).toThrow()
    })
  })

  describe('not-every?', () => {
    it('samples', () => {
      expect(lits.run('(not-every? number? [1 2 3])')).toBe(false)
      expect(lits.run('(not-every? number? [:1 :2 :3])')).toBe(true)
      expect(lits.run('(not-every? number? [])')).toBe(false)
      expect(lits.run('(not-every? number? "")')).toBe(false)
      expect(lits.run('(not-every? number? {})')).toBe(false)
      expect(lits.run('(not-every? (fn [x] (zero? (mod x 2))) [2 4 6])')).toBe(false)
      expect(lits.run('(not-every? (fn [x] (>= x :a)) "abc")')).toBe(false)
      expect(lits.run('(not-every? (fn [x] (>= x :a)) "abC")')).toBe(true)
      expect(lits.run('(not-every? #(even? (second %1)) {:a 2 :b 4})')).toBe(false)
      expect(lits.run('(not-every? #(even? (second %1)) {:a 2 :b 3})')).toBe(true)
      expect(lits.run('(not-every? #(even? (second %1)) {:a 2 :b 3})')).toBe(true)
      expect(() => lits.run('(not-every? +)')).toThrow()
      expect(() => lits.run('(not-every?)')).toThrow()
      expect(() => lits.run('(not-every? number? [1] 2)')).toThrow()
    })
  })

  describe('any?', () => {
    it('samples', () => {
      expect(lits.run('(any? number? [1 2 3])')).toBe(true)
      expect(lits.run('(any? number? [1 :2 3])')).toBe(true)
      expect(lits.run('(any? number? [:1 :2 :3])')).toBe(false)
      expect(lits.run('(any? number? [])')).toBe(false)
      expect(lits.run('(any? number? "")')).toBe(false)
      expect(lits.run('(any? number? {})')).toBe(false)
      expect(lits.run('(any? (fn [x] (zero? (mod x 2))) [1 3 6])')).toBe(true)
      expect(lits.run('(any? (fn [x] (zero? (mod x 2))) [1 3 5])')).toBe(false)
      expect(lits.run('(any? (fn [x] (>= x :a)) "abc")')).toBe(true)
      expect(lits.run('(any? (fn [x] (>= x :a)) "abC")')).toBe(true)
      expect(lits.run('(any? (fn [x] (>= x :a)) "ABC")')).toBe(false)
      expect(lits.run('(any? #(even? (second %1)) {:a 2 :b 4})')).toBe(true)
      expect(lits.run('(any? #(even? (second %1)) {:a 2 :b 3})')).toBe(true)
      expect(lits.run('(any? #(even? (second %1)) {:a 1 :b 3})')).toBe(false)
      expect(() => lits.run('(any? +)')).toThrow()
      expect(() => lits.run('(any?)')).toThrow()
      expect(() => lits.run('(any? number? [1] 2)')).toThrow()
    })
  })

  describe('not-any?', () => {
    it('samples', () => {
      expect(lits.run('(not-any? number? [1 2 3])')).toBe(false)
      expect(lits.run('(not-any? number? [1 :2 3])')).toBe(false)
      expect(lits.run('(not-any? number? [:1 :2 :3])')).toBe(true)
      expect(lits.run('(not-any? number? [])')).toBe(true)
      expect(lits.run('(not-any? number? "")')).toBe(true)
      expect(lits.run('(not-any? number? {})')).toBe(true)
      expect(lits.run('(not-any? (fn [x] (zero? (mod x 2))) [1 3 6])')).toBe(false)
      expect(lits.run('(not-any? (fn [x] (zero? (mod x 2))) [1 3 5])')).toBe(true)
      expect(lits.run('(not-any? (fn [x] (>= x :a)) "abc")')).toBe(false)
      expect(lits.run('(not-any? (fn [x] (>= x :a)) "abC")')).toBe(false)
      expect(lits.run('(not-any? (fn [x] (>= x :a)) "ABC")')).toBe(true)
      expect(lits.run('(not-any? #(even? (second %1)) {:a 2 :b 4})')).toBe(false)
      expect(lits.run('(not-any? #(even? (second %1)) {:a 2 :b 3})')).toBe(false)
      expect(lits.run('(not-any? #(even? (second %1)) {:a 1 :b 3})')).toBe(true)
      expect(() => lits.run('(not-any? +)')).toThrow()
      expect(() => lits.run('(not-any?)')).toThrow()
      expect(() => lits.run('(not-any? number? [1] 2)')).toThrow()
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

  describe('update-in', () => {
    it('samples', () => {
      expect(
        lits.run(
          '(def x "Albert") (update-in x [3] (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albfrt')
      expect(
        lits.run(
          '(def x "Albert") (update-in x [6] (fn [val] (if (nil? val) "!" (from-char-code (inc (to-char-code val))))))',
        ),
      ).toEqual('Albert!')

      expect(lits.run('(def x [0, 1, 2, 3]) (update-in x [3] inc)')).toEqual([0, 1, 2, 4])
      expect(lits.run('(def x [0, 1, 2, 3]) (update-in x [4] identity)')).toEqual([0, 1, 2, 3, null])

      expect(lits.run('(def x {:a 1 :b 2}) (update-in x [:a] inc)')).toEqual({ a: 2, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update-in x [:a] + 10)')).toEqual({ a: 11, b: 2 })
      expect(lits.run('(def x {:a 1 :b 2}) (update-in x [:a] (fn [val] (if (even? val) 0 (inc val))))')).toEqual({
        a: 2,
        b: 2,
      })
      expect(lits.run('(update-in {} [:a] (fn [val] (when (nil? val) 0)))')).toEqual({ a: 0 })
      expect(lits.run('(def x {:a 1 :b 2}) (update-in x [:c] (fn [val] (if (nil? val) 0 (inc val))))')).toEqual({
        a: 1,
        b: 2,
        c: 0,
      })
      expect(lits.run('(update-in {:a [1 2 3]} [:a 1] (fn [val] (when (nil? val) 0)))')).toEqual({
        a: [1, null, 3],
      })
      expect(lits.run('(update-in {:a [1 nil 3]} [:a 1] (fn [val] (when (nil? val) 0)))')).toEqual({
        a: [1, 0, 3],
      })
      expect(lits.run('(update-in {:a [1 "Albert" 3]} [:a 1 0] (fn [val] (if (nil? val) "?" "!")))')).toEqual({
        a: [1, '!lbert', 3],
      })
      expect(lits.run('(update-in {:a [1 "" 3]} [:a 1 0] (fn [val] (if (nil? val) "?" "!")))')).toEqual({
        a: [1, '?', 3],
      })
    })
  })
})
