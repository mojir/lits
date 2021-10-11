import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`array functions`, () => {
  describe(`nth`, () => {
    test(`array samples`, () => {
      expect(lispish.run(`(nth [1 2 3] 1)`)).toBe(2)
      expect(lispish.run(`(nth [1 2 3] 3)`)).toBeUndefined()
      expect(lispish.run(`(nth [1 2 3] -1)`)).toBeUndefined()
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
      expect(lispish.run(`(nth "A string" -3)`)).toBeUndefined()
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

      expect(lispish.run(`(slice "Albert")`)).toBe(`Albert`)
      expect(lispish.run(`(slice "Albert" 0)`)).toBe(`Albert`)
      expect(lispish.run(`(slice "Albert" 1)`)).toBe(`lbert`)
      expect(lispish.run(`(slice "Albert" -1)`)).toBe(`t`)
      expect(lispish.run(`(slice "Albert" -3)`)).toBe(`ert`)
      expect(lispish.run(`(slice "Albert" -4)`)).toBe(`bert`)
      expect(lispish.run(`(slice "Albert" -5)`)).toBe(`lbert`)
      expect(lispish.run(`(slice "Albert" -6)`)).toBe(`Albert`)
      expect(lispish.run(`(slice "Albert" -7)`)).toBe(`Albert`)
      expect(lispish.run(`(slice "Albert" 4)`)).toBe(`rt`)
      expect(lispish.run(`(slice "Albert" 5)`)).toBe(`t`)
      expect(lispish.run(`(slice "Albert" 6)`)).toBe(``)
      expect(lispish.run(`(slice "Albert" 0 0)`)).toBe(``)
      expect(lispish.run(`(slice "Albert" 0 1)`)).toBe(`A`)
      expect(lispish.run(`(slice "Albert" 0 10)`)).toBe(`Albert`)
      expect(lispish.run(`(slice "Albert" 0 -1)`)).toBe(`Alber`)

      expect(() => lispish.run(`(slice [1 2 3] 1 2 3)`)).toThrow()
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

      expect(lispish.run(`(reduce (fn [x y] (concat x "-" y)) "Albert")`)).toBe(`A-l-b-e-r-t`)
      expect(lispish.run(`(reduce (fn [x y] (concat x "-" y)) ">" "Albert")`)).toBe(`>-A-l-b-e-r-t`)
      expect(lispish.run(`(reduce (fn [x y] (concat x "-" y)) ">" "")`)).toBe(`>`)

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

      expect(lispish.run(`(reduce-right (fn [x y] (concat x "-" y)) "Albert")`)).toBe(`t-r-e-b-l-A`)
      expect(lispish.run(`(reduce-right (fn [x y] (concat x "-" y)) ">" "Albert")`)).toBe(`>-t-r-e-b-l-A`)
      expect(lispish.run(`(reduce-right (fn [x y] (concat x "-" y)) ">" "")`)).toBe(`>`)

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
      expect(lispish.run(`(filter (fn [x] (string>= x "a")) "aAbBcC")`)).toBe(`abc`)
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
      expect(lispish.run(`(position (fn [x] (string>= x "a")) "Aa")`)).toBe(1)
      expect(lispish.run(`(position (fn [x] (= x "z")) "Aa")`)).toBeUndefined()
      expect(() => lispish.run(`(position +)`)).toThrow()
      expect(() => lispish.run(`(position)`)).toThrow()
      expect(() => lispish.run(`(position number? [1] 2)`)).toThrow()
    })
  })

  describe(`index-of`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(index-of ["1" "2" 3] "2")`)).toEqual(1)
      expect(lispish.run(`(index-of ["1" "2" "3"] "4")`)).toBeUndefined()
      expect(lispish.run(`(index-of [] 1)`)).toBeUndefined()
      expect(lispish.run(`(index-of "Albert" "l")`)).toBe(1)
      expect(lispish.run(`(index-of "Albert" "ert")`)).toBe(3)
      expect(lispish.run(`(index-of "Albert" "z")`)).toBeUndefined()
      expect(lispish.run(`(index-of [1] 2)`)).toBeUndefined()
      expect(() => lispish.run(`(index-of +)`)).toThrow()
      expect(() => lispish.run(`(index-of)`)).toThrow()
    })
  })

  describe(`some`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(some number? ["1" "2" 3])`)).toBe(3)
      expect(lispish.run(`(some number? ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(some number? [])`)).toBeUndefined()
      expect(lispish.run(`(some (fn [x] (zero? (mod x 3))) [1 2 3 4 5 6 7])`)).toBe(3)

      expect(lispish.run(`(some (fn [x] (string>= x "a")) "Aa")`)).toBe(`a`)
      expect(lispish.run(`(some (fn [x] (string>= x "z")) "Aa")`)).toBeUndefined()

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
      expect(lispish.run(`(every? (fn [x] (string>= x "a")) "abc")`)).toBe(true)
      expect(lispish.run(`(every? (fn [x] (string>= x "a")) "abC")`)).toBe(false)
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
      expect(lispish.run(`(map (fn [x] (if (string>= x "a") "-" "+")) "AaBbCc")`)).toBe(`+-+-+-`)
      expect(() => lispish.run(`(map (fn [x] (if (string>= x "a") 0 1)) "AaBbCc")`)).toThrow()
      expect(
        lispish.run(
          `
          (defn maxChar [char &rest chars]
            (loop [cs chars result char]
              (if (empty? cs)
                result
                (recur
                  (rest cs)
                  (if (string> (chars 0) result) (chars 0) result)
                )
              )
            )
          )

          (map maxChar "263" "247" "162")
          `,
        ),
      ).toEqual(`267`)
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
      expect(lispish.run(`(first "AB")`)).toBe(`A`)
      expect(lispish.run(`(first "A")`)).toBe(`A`)
      expect(lispish.run(`(first "")`)).toBeUndefined()

      expect(() => lispish.run(`(first`)).toThrow()
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

      expect(lispish.run(`(second "ABC")`)).toBe(`B`)
      expect(lispish.run(`(second "AB")`)).toBe(`B`)
      expect(lispish.run(`(second "A")`)).toBeUndefined()
      expect(lispish.run(`(second "")`)).toBeUndefined()

      expect(() => lispish.run(`(second`)).toThrow()
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
      expect(lispish.run(`(last "Albert")`)).toBe(`t`)
      expect(lispish.run(`(last "1")`)).toBe(`1`)
      expect(lispish.run(`(last "")`)).toBeUndefined()

      expect(() => lispish.run(`(last`)).toThrow()
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
      expect(lispish.run(`(cons "A" "Mojir")`)).toEqual(`AMojir`)

      expect(() => lispish.run(`(const "Ab" "Mojir")`)).toThrow()
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
      expect(lispish.run(`(push "Albert" "!")`)).toBe(`Albert!`)
      expect(lispish.run(`(push "Albert" "!" "?")`)).toBe(`Albert!?`)
      expect(lispish.run(`(push "" "!" "?")`)).toBe(`!?`)

      expect(() => lispish.run(`(push "Albert" "!?")`)).toThrow()
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
      expect(lispish.run(`(pop "Albert")`)).toBe(`Alber`)
      expect(lispish.run(`(pop "1")`)).toBe(``)
      expect(lispish.run(`(pop "")`)).toBe(``)

      expect(() => lispish.run(`(pop (object))`)).toThrow()
      expect(() => lispish.run(`(pop null)`)).toThrow()
      expect(() => lispish.run(`(pop undefined)`)).toThrow()
      expect(() => lispish.run(`(pop true)`)).toThrow()
      expect(() => lispish.run(`(pop false)`)).toThrow()
      expect(() => lispish.run(`(pop 1)`)).toThrow()
      expect(() => lispish.run(`(pop)`)).toThrow()
    })
  })

  describe(`unshift`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(unshift [1 2 3] 0)`)).toEqual([0, 1, 2, 3])
      expect(lispish.run(`(unshift [1 2 3] 1 "2")`)).toEqual([1, `2`, 1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (unshift l 1 "2") l`)).toEqual([1, 2, 3])
      expect(lispish.run(`(def l [1 2 3]) (unshift l 1 "2")`)).toEqual([1, `2`, 1, 2, 3])
      expect(lispish.run(`(unshift "lbert" "A")`)).toBe(`Albert`)

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
      expect(lispish.run(`(shift "Albert")`)).toBe(`lbert`)
      expect(lispish.run(`(shift "1")`)).toBe(``)
      expect(lispish.run(`(shift "")`)).toBe(``)

      expect(() => lispish.run(`(shift (object))`)).toThrow()
      expect(() => lispish.run(`(shift null)`)).toThrow()
      expect(() => lispish.run(`(shift undefined)`)).toThrow()
      expect(() => lispish.run(`(shift true)`)).toThrow()
      expect(() => lispish.run(`(shift false)`)).toThrow()
      expect(() => lispish.run(`(shift 1)`)).toThrow()
      expect(() => lispish.run(`(shift)`)).toThrow()
    })
  })

  describe(`take`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(take 2 [1 2 3])`)).toEqual([1, 2])
      expect(lispish.run(`(take 20 [1 2 3])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(take 0 [1 2 3])`)).toEqual([])
      expect(lispish.run(`(take 2 "Albert")`)).toEqual(`Al`)

      expect(() => lispish.run(`(take 0.5 [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take 1 (object))`)).toThrow()
      expect(() => lispish.run(`(take 1 null)`)).toThrow()
      expect(() => lispish.run(`(take 1 undefined)`)).toThrow()
      expect(() => lispish.run(`(take 1 true)`)).toThrow()
      expect(() => lispish.run(`(take 1 false)`)).toThrow()
      expect(() => lispish.run(`(take "1" "Hej")`)).toThrow()
      expect(() => lispish.run(`(take)`)).toThrow()
      expect(() => lispish.run(`(take [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take 1 2 [1 2 3])`)).toThrow()
    })

    test(`new array created`, () => {
      const program = `
        (def l1 [1 2 3])
        (def l2 (take 2 l1))
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
      expect(lispish.run(`(take-while (fn [x] (string<= x "c")) "abcdabcd")`)).toEqual(`abc`)

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
      expect(lispish.run(`(sort (fn [a b] (cond ((< a b) -1) ((> a b) 1) (true 0))) [3 1 2])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true 0))) [3 1 2])`)).toEqual([3, 2, 1])
      expect(lispish.run(`(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true 0))) [])`)).toEqual([])

      expect(lispish.run(`(sort (fn [a b] (cond ((string> a b) 1) ((string< a b) -1) (true 0))) "Albert")`)).toBe(
        `Abelrt`,
      )

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

      expect(lispish.run(`(random-sample 1 "Albert")`)).toEqual(`Albert`)
      expect(lispish.run(`(random-sample 0 "Albert")`)).toEqual(``)

      expect(() => lispish.run(`(random-sample [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(random-sample "1" [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(random-sample 1)`)).toThrow()
    })
  })
})
