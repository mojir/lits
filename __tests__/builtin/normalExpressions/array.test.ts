import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`array functions`, () => {
  describe(`array`, () => {
    test(`samples`, () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`(array 1)`)).toEqual([1])
      expect((lispish.run(`(array undefined)`) as unknown[])[0]).toEqual(undefined)
      expect(lispish.run(`(array 0 "1" null true false undefined (array []) (object))`)).toEqual([
        0,
        `1`,
        null,
        true,
        false,
        undefined,
        [[]],
        {},
      ])
    })
    test(`shorthand samples`, () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`[1]`)).toEqual([1])
      expect((lispish.run(`[undefined]`) as unknown[])[0]).toEqual(undefined)
      expect(lispish.run(`[0 "1" null true false undefined [[]] (object)]`)).toEqual([
        0,
        `1`,
        null,
        true,
        false,
        undefined,
        [[]],
        {},
      ])
    })
  })

  describe(`range`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(range 0)`)).toEqual([])
      expect(lispish.run(`(range 5)`)).toEqual([0, 1, 2, 3, 4])
      expect(lispish.run(`(range -5)`)).toEqual([0, -1, -2, -3, -4])
      expect(lispish.run(`(range 5 1)`)).toEqual([5, 4, 3, 2])
      expect(lispish.run(`(range 1 5)`)).toEqual([1, 2, 3, 4])
      expect(lispish.run(`(range 5 1 -2)`)).toEqual([5, 3])
      expect(lispish.run(`(range 0 0.5 0.125)`)).toEqual([0, 0.125, 0.25, 0.375])
      expect(() => lispish.run(`(range)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 1 1)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 0)`)).toThrow()
      expect(() => lispish.run(`(range 0 0 0)`)).toThrow()
      expect(() => lispish.run(`(range 1 'x')`)).toThrow()
      expect(() => lispish.run(`(range false 1 2)`)).toThrow()
      expect(() => lispish.run(`(range 0 2 'y')`)).toThrow()
      expect(() => lispish.run(`(range (object) 'x' 'y')`)).toThrow()
    })
  })

  describe(`nth`, () => {
    test(`array samples`, () => {
      expect(lispish.run(`(nth [1 2 3] 1)`)).toBe(2)
      expect(lispish.run(`(nth [1 2 3] 3)`)).toBeUndefined()
      expect(lispish.run(`(nth [1 2 3] -1)`)).toBe(3)
      expect(lispish.run(`(nth [1 2 3] -4)`)).toBeUndefined()
      expect(() => lispish.run(`(nth)`)).toThrow()
      expect(() => lispish.run(`(nth (object) 1)`)).toThrow()
      expect(() => lispish.run(`(nth null 2)`)).toThrow()
      expect(() => lispish.run(`(nth [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(nth [1 2 3] 1 2)`)).toThrow()
    })

    test(`string samples`, () => {
      expect(lispish.run(`(nth "A string" 1)`)).toBe(` `)
      expect(lispish.run(`(nth "A string" 3)`)).toBe(`t`)
      expect(lispish.run(`(nth "A string" -3)`)).toBe(`i`)
      expect(lispish.run(`(nth "A string" 30)`)).toBeUndefined()
      expect(lispish.run(`(nth "A string" -30)`)).toBeUndefined()
      expect(() => lispish.run(`(nth "A string")`)).toThrow()
      expect(() => lispish.run(`(nth "A string" 1 2)`)).toThrow()
    })
  })

  describe(`slice`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(slice [1 2 3])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(slice [1 2 3] 0)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(slice [1 2 3] 1)`)).toEqual([2, 3])
      expect(lispish.run(`(slice [1 2 3] -1)`)).toEqual([3])
      expect(lispish.run(`(slice [1 2 3] -3)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(slice [1 2 3] -4)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(slice [1 2 3] 3)`)).toEqual([])
      expect(lispish.run(`(slice [1 2 3] 4)`)).toEqual([])
      expect(lispish.run(`(slice [1 2 3] 0 0)`)).toEqual([])
      expect(lispish.run(`(slice [1 2 3] 0 1)`)).toEqual([1])
      expect(lispish.run(`(slice [1 2 3] 0 10)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(slice [1 2 3] 0 -1)`)).toEqual([1, 2])

      expect(() => lispish.run(`(slice [1 2 3] 1 2 3)`)).toThrow()
      expect(() => lispish.run(`(slice "Albert" 1)`)).toThrow()
      expect(() => lispish.run(`(slice)`)).toThrow()
      expect(() => lispish.run(`(slice (object) 1)`)).toThrow()
      expect(() => lispish.run(`(slice null 2)`)).toThrow()
    })
  })

  describe(`reduce`, () => {
    test(`samples`, () => {
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
      expect(lispish.run(program)).toBe(16)

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
      expect(lispish.run(program)).toBe(6)

      expect(lispish.run(`(reduce + [1 2 3 4 5])`)).toBe(15)
      expect(lispish.run(`(reduce + [])`)).toBe(0)
      expect(lispish.run(`(reduce + [1])`)).toBe(1)
      expect(lispish.run(`(reduce + [1 2])`)).toBe(3)
      expect(lispish.run(`(reduce + 1 [])`)).toBe(1)
      expect(lispish.run(`(reduce + 1 [2 3])`)).toBe(6)
      expect(lispish.run(`(reduce + 0 [1 2 3])`)).toBe(6)
      expect(lispish.run(`(reduce + 0 [])`)).toBe(0)
      expect(lispish.run(`(reduce + 1 [])`)).toBe(1)
      expect(() => lispish.run(`(reduce +)`)).toThrow()
      expect(() => lispish.run(`(reduce)`)).toThrow()
      expect(() => lispish.run(`(reduce + 1 2)`)).toThrow()
    })
  })

  describe(`reduce-right`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(reduce-right + [1 2 3 4 5])`)).toBe(15)
      expect(lispish.run(`(reduce-right + [])`)).toBe(0)
      expect(lispish.run(`(reduce-right + [1])`)).toBe(1)
      expect(lispish.run(`(reduce-right + [1 2])`)).toBe(3)
      expect(lispish.run(`(reduce-right + 0 [1 2 3])`)).toBe(6)
      expect(lispish.run(`(reduce-right + 0 [])`)).toBe(0)
      expect(lispish.run(`(reduce-right + 0 [])`)).toBe(0)
      expect(lispish.run(`(reduce-right str "" ["1" "2" "3"])`)).toBe(`321`)
      expect(lispish.run(`(reduce-right str ["1" "2" "3"])`)).toBe(`321`)
      expect(() => lispish.run(`(reduce-right +)`)).toThrow()
      expect(() => lispish.run(`(reduce-right)`)).toThrow()
      expect(() => lispish.run(`(reduce-right + 1 2)`)).toThrow()
    })
  })
  describe(`filter`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(filter number? [1 "2" 3])`)).toEqual([1, 3])
      expect(lispish.run(`(filter number? [])`)).toEqual([])
      expect(lispish.run(`(filter null? [1 "2" 3])`)).toEqual([])
      expect(lispish.run(`(filter (fn [x] (zero? (mod x 3))) [0 1 2 3 4 5 6 7])`)).toEqual([0, 3, 6])
      expect(() => lispish.run(`(filter +)`)).toThrow()
      expect(() => lispish.run(`(filter)`)).toThrow()
      expect(() => lispish.run(`(filter number? [1] 2)`)).toThrow()
    })
  })

  describe(`position`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(position number? ["1" "2" 3])`)).toEqual(2)
      expect(lispish.run(`(position number? ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(position number? [])`)).toBeUndefined()
      expect(lispish.run(`(position (fn [x] (zero? (mod x 3))) [1 2 3 4 5 6 7])`)).toEqual(2)
      expect(() => lispish.run(`(position +)`)).toThrow()
      expect(() => lispish.run(`(position)`)).toThrow()
      expect(() => lispish.run(`(position number? [1] 2)`)).toThrow()
    })
  })

  describe(`index-of`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(index-of "2" ["1" "2" 3])`)).toEqual(1)
      expect(lispish.run(`(index-of "4" ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(index-of 1 [])`)).toBeUndefined()
      expect(() => lispish.run(`(index-of +)`)).toThrow()
      expect(() => lispish.run(`(index-of)`)).toThrow()
      expect(() => lispish.run(`(index-of [1] 2)`)).toThrow()
    })
  })

  describe(`some`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(some number? ["1" "2" 3])`)).toBe(3)
      expect(lispish.run(`(some number? ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(some number? [])`)).toBeUndefined()
      expect(lispish.run(`(some (fn [x] (zero? (mod x 3))) [1 2 3 4 5 6 7])`)).toBe(3)
      expect(() => lispish.run(`(some +)`)).toThrow()
      expect(() => lispish.run(`(some)`)).toThrow()
      expect(() => lispish.run(`(some number? [1] 2)`)).toThrow()
    })
  })

  describe(`every?`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(every? number? [1 2 3])`)).toBe(true)
      expect(lispish.run(`(every? number? ["1" "2" "3"])`)).toBe(false)
      expect(lispish.run(`(every? number? [])`)).toBe(false)
      expect(lispish.run(`(every? (fn [x] (zero? (mod x 2))) [2 4 6])`)).toBe(true)
      expect(() => lispish.run(`(every? +)`)).toThrow()
      expect(() => lispish.run(`(every?)`)).toThrow()
      expect(() => lispish.run(`(every? number? [1] 2)`)).toThrow()
    })
  })

  describe(`map`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(map number? [1 "2" 3])`)).toEqual([true, false, true])
      expect(lispish.run(`(map number? [])`)).toEqual([])
      expect(lispish.run(`(map + [1 2 3] [1 2 3])`)).toEqual([2, 4, 6])
      expect(lispish.run(`(map max [2 6 3] [2 4 7] [1 6 2])`)).toEqual([2, 6, 7])
      expect(lispish.run(`(map null? [1 "2" 3])`)).toEqual([false, false, false])
      expect(lispish.run(`(map (fn [x] (zero? (mod x 3))) [0 1 2 3 4 5 6 7])`)).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lispish.run(`(map inc [0 1 2 3 4 5 6 7])`)).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(() => lispish.run(`(map + [1 2 3] [1 2])`)).toThrow()
      expect(() => lispish.run(`(map +)`)).toThrow()
      expect(() => lispish.run(`(map)`)).toThrow()
      expect(() => lispish.run(`(map number? [1] 2)`)).toThrow()
    })
  })
  describe(`first`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(first [1 2 3])`)).toEqual(1)
      expect(lispish.run(`(first ["1"])`)).toEqual(`1`)
      expect(lispish.run(`(first [])`)).toBeUndefined()

      expect(() => lispish.run(`(first`)).toThrow()
      expect(() => lispish.run(`(first "1")`)).toThrow()
      expect(() => lispish.run(`(first true)`)).toThrow()
      expect(() => lispish.run(`(first false)`)).toThrow()
      expect(() => lispish.run(`(first null)`)).toThrow()
      expect(() => lispish.run(`(first undefined)`)).toThrow()
      expect(() => lispish.run(`(first (object))`)).toThrow()
      expect(() => lispish.run(`(first 10)`)).toThrow()
    })
  })

  describe(`second`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(second [1 2 3])`)).toEqual(2)
      expect(lispish.run(`(second ["1"])`)).toBeUndefined()
      expect(lispish.run(`(second [])`)).toBeUndefined()

      expect(() => lispish.run(`(second`)).toThrow()
      expect(() => lispish.run(`(second "1")`)).toThrow()
      expect(() => lispish.run(`(second true)`)).toThrow()
      expect(() => lispish.run(`(second false)`)).toThrow()
      expect(() => lispish.run(`(second null)`)).toThrow()
      expect(() => lispish.run(`(second undefined)`)).toThrow()
      expect(() => lispish.run(`(second (object))`)).toThrow()
      expect(() => lispish.run(`(second 10)`)).toThrow()
    })
  })

  describe(`reverse`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(reverse [1 2 3])`)).toEqual([3, 2, 1])
      expect(lispish.run(`(reverse ["1"])`)).toEqual([`1`])
      expect(lispish.run(`(reverse [])`)).toEqual([])
      expect(lispish.run(`(reverse "albert")`)).toBe(`trebla`)
      expect(lispish.run(`(reverse "A 1")`)).toBe(`1 A`)
      expect(lispish.run(`(reverse "")`)).toBe(``)

      expect(() => lispish.run(`(reverse)`)).toThrow()
      expect(() => lispish.run(`(reverse "word1" "word2")`)).toThrow()
      expect(() => lispish.run(`(reverse`)).toThrow()
      expect(() => lispish.run(`(reverse true)`)).toThrow()
      expect(() => lispish.run(`(reverse false)`)).toThrow()
      expect(() => lispish.run(`(reverse null)`)).toThrow()
      expect(() => lispish.run(`(reverse undefined)`)).toThrow()
      expect(() => lispish.run(`(reverse (object))`)).toThrow()
      expect(() => lispish.run(`(reverse 10)`)).toThrow()
    })
    test(`returns a new array instance`, () => {
      const program = `
        (def l [1 2 3])
        (not= l (reverse l))
      `
      expect(lispish.run(program)).toBe(true)
    })
  })

  describe(`last`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(last [1 2 3])`)).toEqual(3)
      expect(lispish.run(`(last ["1"])`)).toEqual(`1`)
      expect(lispish.run(`(last [])`)).toBeUndefined()

      expect(() => lispish.run(`(last`)).toThrow()
      expect(() => lispish.run(`(last "1")`)).toThrow()
      expect(() => lispish.run(`(last true)`)).toThrow()
      expect(() => lispish.run(`(last false)`)).toThrow()
      expect(() => lispish.run(`(last null)`)).toThrow()
      expect(() => lispish.run(`(last undefined)`)).toThrow()
      expect(() => lispish.run(`(last (object))`)).toThrow()
      expect(() => lispish.run(`(last 10)`)).toThrow()
    })
  })

  describe(`rest`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(rest [1 2 3])`)).toEqual([2, 3])
      expect(lispish.run(`(rest [1 2])`)).toEqual([2])
      expect(lispish.run(`(rest ["1"])`)).toEqual([])
      expect(lispish.run(`(rest [])`)).toEqual([])
      expect(lispish.run(`(rest "Albert")`)).toEqual(`lbert`)
      expect(lispish.run(`(rest "A")`)).toEqual(``)
      expect(lispish.run(`(rest "")`)).toEqual(``)

      expect(() => lispish.run(`(rest`)).toThrow()
      expect(() => lispish.run(`(rest true)`)).toThrow()
      expect(() => lispish.run(`(rest false)`)).toThrow()
      expect(() => lispish.run(`(rest null)`)).toThrow()
      expect(() => lispish.run(`(rest undefined)`)).toThrow()
      expect(() => lispish.run(`(rest (object))`)).toThrow()
      expect(() => lispish.run(`(rest 10)`)).toThrow()
    })
  })

  describe(`next`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(next [1 2 3])`)).toEqual([2, 3])
      expect(lispish.run(`(next [1 2])`)).toEqual([2])
      expect(lispish.run(`(next ["1"])`)).toBeUndefined()
      expect(lispish.run(`(next [])`)).toBeUndefined()
      expect(lispish.run(`(next "Albert")`)).toEqual(`lbert`)
      expect(lispish.run(`(next "A")`)).toBeUndefined()
      expect(lispish.run(`(next "")`)).toBeUndefined()

      expect(() => lispish.run(`(next`)).toThrow()
      expect(() => lispish.run(`(next true)`)).toThrow()
      expect(() => lispish.run(`(next false)`)).toThrow()
      expect(() => lispish.run(`(next null)`)).toThrow()
      expect(() => lispish.run(`(next undefined)`)).toThrow()
      expect(() => lispish.run(`(next (object))`)).toThrow()
      expect(() => lispish.run(`(next 10)`)).toThrow()
    })
  })

  describe(`cons`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(cons 0 [1 2 3])`)).toEqual([0, 1, 2, 3])
      expect(lispish.run(`(cons 0 ["1"])`)).toEqual([0, `1`])
      expect(lispish.run(`(cons 0 [])`)).toEqual([0])

      expect(() => lispish.run(`(cons`)).toThrow()
      expect(() => lispish.run(`(cons 1 "1")`)).toThrow()
      expect(() => lispish.run(`(cons 1 true)`)).toThrow()
      expect(() => lispish.run(`(cons 1 false)`)).toThrow()
      expect(() => lispish.run(`(cons 1 null)`)).toThrow()
      expect(() => lispish.run(`(cons 1 undefined)`)).toThrow()
      expect(() => lispish.run(`(cons 1 (object))`)).toThrow()
      expect(() => lispish.run(`(cons 1 10)`)).toThrow()
    })
  })

  describe(`push`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(push [1 2 3] 0)`)).toEqual([1, 2, 3, 0])
      expect(lispish.run(`(push [1 2 3] 1 "2")`)).toEqual([1, 2, 3, 1, `2`])
      expect(lispish.run(`(def l [1 2 3]) (push l 1 "2")`)).toEqual([1, 2, 3, 1, `2`])
      expect(lispish.run(`(def l [1 2 3]) (push l 1 "2") l`)).toEqual([1, 2, 3])

      expect(() => lispish.run(`(push [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(push (object) 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push null 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push undefined 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push true 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push false 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push 1 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push "1" 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push 0 "2")`)).toThrow()
      expect(() => lispish.run(`(push)`)).toThrow()
    })
  })

  describe(`pop`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(pop [1 2 3])`)).toEqual([1, 2])
      expect(lispish.run(`(pop [1 2 undefined])`)).toEqual([1, 2])
      expect(lispish.run(`(pop [])`)).toEqual([])
      expect(lispish.run(`(def l [1 2 3]) (pop l) l`)).toEqual([1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (pop l)`)).toEqual([1, 2])
      expect(lispish.run(`(def l []) (pop l) l`)).toEqual([])

      expect(() => lispish.run(`(pop (object))`)).toThrow()
      expect(() => lispish.run(`(pop null)`)).toThrow()
      expect(() => lispish.run(`(pop undefined)`)).toThrow()
      expect(() => lispish.run(`(pop true)`)).toThrow()
      expect(() => lispish.run(`(pop false)`)).toThrow()
      expect(() => lispish.run(`(pop 1)`)).toThrow()
      expect(() => lispish.run(`(pop "1")`)).toThrow()
      expect(() => lispish.run(`(pop)`)).toThrow()
    })
  })

  describe(`unshift`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(unshift [1 2 3] 0)`)).toEqual([0, 1, 2, 3])
      expect(lispish.run(`(unshift [1 2 3] 1 "2")`)).toEqual([1, `2`, 1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (unshift l 1 "2") l`)).toEqual([1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (unshift l 1 "2")`)).toEqual([1, `2`, 1, 2, 3])

      expect(() => lispish.run(`(unshift [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(unshift (object) 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift null 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift undefined 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift true 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift false 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift 1 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift "1" 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift 0 "2")`)).toThrow()
      expect(() => lispish.run(`(unshift)`)).toThrow()
    })
  })

  describe(`shift`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(shift [1 2 3])`)).toEqual([2, 3])
      expect(lispish.run(`(shift [undefined 2 3])`)).toEqual([2, 3])
      expect(lispish.run(`(shift [])`)).toEqual([])
      expect(lispish.run(`(def l [1 2 3]) (shift l) l`)).toEqual([1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (shift l)`)).toEqual([2, 3])
      expect(lispish.run(`(def l []) (shift l) l`)).toEqual([])

      expect(() => lispish.run(`(shift (object))`)).toThrow()
      expect(() => lispish.run(`(shift null)`)).toThrow()
      expect(() => lispish.run(`(shift undefined)`)).toThrow()
      expect(() => lispish.run(`(shift true)`)).toThrow()
      expect(() => lispish.run(`(shift false)`)).toThrow()
      expect(() => lispish.run(`(shift 1)`)).toThrow()
      expect(() => lispish.run(`(shift "1")`)).toThrow()
      expect(() => lispish.run(`(shift)`)).toThrow()
    })
  })

  describe(`take`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(take [1 2 3] 2)`)).toEqual([1, 2])
      expect(lispish.run(`(take [1 2 3] 20)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(take [1 2 3] 0)`)).toEqual([])

      expect(() => lispish.run(`(take [1 2 3] 0.5)`)).toThrow()
      expect(() => lispish.run(`(take (object))`)).toThrow()
      expect(() => lispish.run(`(take null)`)).toThrow()
      expect(() => lispish.run(`(take undefined)`)).toThrow()
      expect(() => lispish.run(`(take true)`)).toThrow()
      expect(() => lispish.run(`(take false)`)).toThrow()
      expect(() => lispish.run(`(take "1")`)).toThrow()
      expect(() => lispish.run(`(take)`)).toThrow()
      expect(() => lispish.run(`(take [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take [1 2 3] 1 2)`)).toThrow()
    })

    test(`new array created`, () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take l1 2))
        (= l1 l2)
      `
      expect(lispish.run(program)).toBe(false)
    })
  })

  describe(`take-last`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(take-last [1 2 3] 2)`)).toEqual([2, 3])
      expect(lispish.run(`(take-last [1 2 3] 20)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(take-last [1 2 3] 0)`)).toEqual([])

      expect(() => lispish.run(`(take-last [1 2 3] 0.5)`)).toThrow()
      expect(() => lispish.run(`(take-last (object))`)).toThrow()
      expect(() => lispish.run(`(take-last null)`)).toThrow()
      expect(() => lispish.run(`(take-last undefined)`)).toThrow()
      expect(() => lispish.run(`(take-last true)`)).toThrow()
      expect(() => lispish.run(`(take-last false)`)).toThrow()
      expect(() => lispish.run(`(take-last "1")`)).toThrow()
      expect(() => lispish.run(`(take-last)`)).toThrow()
      expect(() => lispish.run(`(take-last [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take-last [1 2 3] 1 2)`)).toThrow()
    })

    test(`new array created`, () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take-last l1 2))
        (= l1 l2)
      `
      expect(lispish.run(program)).toBe(false)
    })
  })

  describe(`take-while`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(take-while (fn [x] (< x 3)) [1 2 3 2 1])`)).toEqual([1, 2])
      expect(lispish.run(`(take-while (fn [x] (> x 3)) [1 2 3 2 1])`)).toEqual([])

      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) (object))`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) null)`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) undefined)`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) true)`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) false)`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) "1")`)).toThrow()
      expect(() => lispish.run(`(take-while 10 [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take-while)`)).toThrow()
      expect(() => lispish.run(`(take-while [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take-while (fn [x] (< x 3)) [1 2 3] 1)`)).toThrow()
    })
    test(`new array created`, () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take-while (fn [x] (< x 3)) l1))
        (= l1 l2)
      `
      expect(lispish.run(program)).toBe(false)
    })
  })

  describe(`sort`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(sort (fn [a b] (cond ((< a b) -1) ((> a b) 1) (true -1))) [3 1 2])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true -1))) [3 1 2])`)).toEqual([3, 2, 1])
      expect(lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true -1))) [])`)).toEqual([])
      expect(() => lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true -1))) 10)`)).toThrow()
      expect(() => lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true -1))))`)).toThrow()
      expect(() => lispish.run(`(sort [10])`)).toThrow()
      expect(() => lispish.run(`(sort)`)).toThrow()
    })
  })

  describe(`join`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(join ["Albert" "Mojir"] " ")`)).toBe(`Albert Mojir`)
      expect(lispish.run(`(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", ")`)).toBe(
        `0, 1, 2, 3, 4, 5, 6, 7, 8, 9`,
      )
      expect(() => lispish.run(`(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", " 5)`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" "Mojir"] " " -1)`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" 10] " ")`)).toThrow()
    })
  })

  describe(`random-sample`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(random-sample 1 [1 2 3])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(random-sample 1.9 [1 2 3])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(random-sample 0 [1 2 3])`)).toEqual([])
      expect(lispish.run(`(random-sample -1.9 [1 2 3])`)).toEqual([])
      expect(() => lispish.run(`(random-sample [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(random-sample "1" [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(random-sample 1)`)).toThrow()
    })
  })

  describe(`repeat`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(repeat 3 5)`)).toEqual([5, 5, 5])
      expect(lispish.run(`(repeat 3 "5")`)).toEqual([`5`, `5`, `5`])
      expect(lispish.run(`(repeat 1 "5")`)).toEqual([`5`])
      expect(lispish.run(`(repeat 0 "5")`)).toEqual([])
      expect(() => lispish.run(`(repeat 1.3 "5")`)).toThrow()
      expect(() => lispish.run(`(repeat -10 "5")`)).toThrow()
      expect(() => lispish.run(`(repeat 10)`)).toThrow()
      expect(() => lispish.run(`(repeat "5")`)).toThrow()
      expect(() => lispish.run(`(repeat)`)).toThrow()
    })
  })

  describe(`array as function`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(def nameArray ["Albert" "Mojir"]) (nameArray 0)`)).toBe(`Albert`)
      expect(lispish.run(`(["Albert" "Mojir"] 0)`)).toBe(`Albert`)
      expect(lispish.run(`((cons 1 [2 3]) 1)`)).toBe(2)
      expect(() => lispish.run(`(["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`(["Albert" "Mojir"] "0")`)).toThrow()
      expect(() => lispish.run(`(["Albert" "Mojir"] 0 1)`)).toThrow()
      expect(() => lispish.run(`("Albert" 0)`)).toThrow()
      expect(() => lispish.run(`((+ 1 2) 0)`)).toThrow()
    })
  })
})
