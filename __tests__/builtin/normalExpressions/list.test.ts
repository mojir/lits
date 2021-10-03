import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe('list functions', () => {
  describe('list', () => {
    test('samples', () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`(list 1)`)).toEqual([1])
      expect((lispish.run(`(list undefined)`) as unknown[])[0]).toEqual(undefined)
      expect(lispish.run(`(list 0 "1" null true false undefined (list []) (object))`)).toEqual([
        0,
        '1',
        null,
        true,
        false,
        undefined,
        [[]],
        {},
      ])
    })
    test('shorthand samples', () => {
      expect(lispish.run(`[]`)).toEqual([])
      expect(lispish.run(`[1]`)).toEqual([1])
      expect((lispish.run(`[undefined]`) as unknown[])[0]).toEqual(undefined)
      expect(lispish.run(`[0 "1" null true false undefined [[]] (object)]`)).toEqual([
        0,
        '1',
        null,
        true,
        false,
        undefined,
        [[]],
        {},
      ])
    })
  })

  describe('listf', () => {
    test('samples', () => {
      expect(lispish.run(`(listf 5 1)`)).toEqual([1, 1, 1, 1, 1])
      expect(lispish.run(`(listf 5 (listf 2 2))`)).toEqual([
        [2, 2],
        [2, 2],
        [2, 2],
        [2, 2],
        [2, 2],
      ])
      expect(() => lispish.run(`(listf)`)).toThrow()
      expect(() => lispish.run(`(listf 0 'x')`)).toThrow()
      expect(() => lispish.run(`(listf 1 'x' 'y')`)).toThrow()
      expect(() => lispish.run(`(listf false 'x' 'y')`)).toThrow()
      expect(() => lispish.run(`(listf null 'x' 'y')`)).toThrow()
      expect(() => lispish.run(`(listf (object) 'x' 'y')`)).toThrow()
    })
  })

  describe('range', () => {
    test('samples', () => {
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

  describe('lenght', () => {
    test('samples', () => {
      expect(lispish.run(`(length [])`)).toBe(0)
      expect(lispish.run(`(length [1])`)).toBe(1)
      expect(lispish.run(`(length [1 2 3])`)).toBe(3)
      expect(() => lispish.run(`(length "")`)).toThrow()
      expect(() => lispish.run(`(length "1")`)).toThrow()
      expect(() => lispish.run(`(length "123")`)).toThrow()
      expect(() => lispish.run(`(length)`)).toThrow()
      expect(() => lispish.run(`(length [] [])`)).toThrow()
      expect(() => lispish.run(`(length 12)`)).toThrow()
      expect(() => lispish.run(`(length false)`)).toThrow()
      expect(() => lispish.run(`(length true)`)).toThrow()
      expect(() => lispish.run(`(length null)`)).toThrow()
      expect(() => lispish.run(`(length undefined)`)).toThrow()
      expect(() => lispish.run(`(length (object))`)).toThrow()
    })
  })

  describe('append', () => {
    test('samples', () => {
      expect(lispish.run(`(append [])`)).toEqual([])
      expect(lispish.run(`(append [1])`)).toEqual([1])
      expect(lispish.run(`(append [1] [2] [3 4])`)).toEqual([1, 2, 3, 4])
      expect(lispish.run(`(append [1 2 3] [])`)).toEqual([1, 2, 3])
      expect(() => lispish.run(`(append)`)).toThrow()
      expect(() => lispish.run(`(append [1] "2")`)).toThrow()
      expect(() => lispish.run(`(append "1")`)).toThrow()
      expect(() => lispish.run(`(append "1" ["2"])`)).toThrow()
      expect(() => lispish.run(`(append 0)`)).toThrow()
      expect(() => lispish.run(`(append true)`)).toThrow()
      expect(() => lispish.run(`(append "1" false)`)).toThrow()
      expect(() => lispish.run(`(append null "m")`)).toThrow()
      expect(() => lispish.run(`(append undefined)`)).toThrow()
      expect(() => lispish.run(`(append (object))`)).toThrow()
    })
  })

  describe('at', () => {
    test('array samples', () => {
      expect(lispish.run(`(at [1 2 3] 1)`)).toBe(2)
      expect(lispish.run(`(at [1 2 3] 3)`)).toBeUndefined()
      expect(lispish.run(`(at [1 2 3] -1)`)).toBe(3)
      expect(lispish.run(`(at [1 2 3] -4)`)).toBeUndefined()
      expect(() => lispish.run(`(at)`)).toThrow()
      expect(() => lispish.run(`(at (object) 1)`)).toThrow()
      expect(() => lispish.run(`(at null 2)`)).toThrow()
      expect(() => lispish.run(`(at [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(at [1 2 3] 1 2)`)).toThrow()
    })

    test('string samples', () => {
      expect(lispish.run(`(at "A string" 1)`)).toBe(' ')
      expect(lispish.run(`(at "A string" 3)`)).toBe('t')
      expect(lispish.run(`(at "A string" -3)`)).toBe('i')
      expect(lispish.run(`(at "A string" 30)`)).toBeUndefined()
      expect(lispish.run(`(at "A string" -30)`)).toBeUndefined()
      expect(() => lispish.run(`(at "A string")`)).toThrow()
      expect(() => lispish.run(`(at "A string" 1 2)`)).toThrow()
    })
  })

  describe('slice', () => {
    test('samples', () => {
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

  describe('splice', () => {
    test('samples', () => {
      expect(lispish.run(`(setq l [1 2 3]) (splice l 0)`)).toEqual([1, 2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 0) l`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1)`)).toEqual([2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1) l`)).toEqual([1])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 0)`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 0) l`)).toEqual([1, 2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1)`)).toEqual([2])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1) l`)).toEqual([1, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 2)`)).toEqual([2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 2) l`)).toEqual([1])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 100)`)).toEqual([2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 100) l`)).toEqual([1])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1 "x")`)).toEqual([2])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1 "x") l`)).toEqual([1, 'x', 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 0 "x")`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 0 "x") l`)).toEqual([1, 'x', 2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 100 0 "x")`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 100 0 "x") l`)).toEqual([1, 2, 3, 'x'])
      expect(lispish.run(`(setq l [1 2 3]) (splice l -1 0 "x")`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l -1 0 "x") l`)).toEqual([1, 2, 'x', 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l -100 0 "x")`)).toEqual([])
      expect(lispish.run(`(setq l [1 2 3]) (splice l -100 0 "x") l`)).toEqual(['x', 1, 2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1 "x" "y" "z")`)).toEqual([2])
      expect(lispish.run(`(setq l [1 2 3]) (splice l 1 1 "x" "y" "z") l`)).toEqual([1, 'x', 'y', 'z', 3])
      expect(() => lispish.run(`(splice [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(splice))`)).toThrow()
      expect(() => lispish.run(`(splice [1 2 3] "1")`)).toThrow()
      expect(() => lispish.run(`(splice [1 2 3] 1 "2")`)).toThrow()
      expect(() => lispish.run(`(splice (object) 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice true 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice false 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice "1" 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice 1 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice null 1 2)`)).toThrow()
      expect(() => lispish.run(`(splice undefined 1 2)`)).toThrow()
    })
  })

  describe('reduce', () => {
    test('samples', () => {
      let program = `
      (defun countChars (stringArray)
        (reduce
          (lambda (sum str) (+ sum (string-length str)))
          stringArray
          0
        )
      )

      (countChars ["First" "Second" "Third"])
      `
      expect(lispish.run(program)).toBe(16)

      program = `
      (defun longestLength (stringArray)
        (reduce
          (lambda (sum str)
            (if (> sum (string-length str))
              sum
              (string-length str)
            )
          )
          stringArray
          0
        )
      )

      (longestLength ["First" "Second" "Third"])
      `
      expect(lispish.run(program)).toBe(6)

      expect(lispish.run(`(reduce (function +) [1 2 3] 0)`)).toBe(6)
      expect(lispish.run(`(reduce (function +) [] 0)`)).toBe(0)
      expect(lispish.run(`(reduce (function +) [] 1)`)).toBe(1)
      expect(() => lispish.run(`(reduce (function +) [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(reduce (function +))`)).toThrow()
      expect(() => lispish.run(`(reduce)`)).toThrow()
      expect(() => lispish.run(`(reduce (function +) 1 2)`)).toThrow()
    })
  })

  describe('reduce-right', () => {
    test('samples', () => {
      expect(lispish.run(`(reduce-right (function +) [1 2 3] 0)`)).toBe(6)
      expect(lispish.run(`(reduce-right (function +) [] 0)`)).toBe(0)
      expect(lispish.run(`(reduce-right (function +) [] 1)`)).toBe(1)
      expect(lispish.run(`(reduce-right (function concat) ["1" "2" "3"] "")`)).toBe('321')
      expect(() => lispish.run(`(reduce-right (function +))`)).toThrow()
      expect(() => lispish.run(`(reduce-right)`)).toThrow()
      expect(() => lispish.run(`(reduce-right (function +) 1 2)`)).toThrow()
    })
  })
  describe('filter', () => {
    test('samples', () => {
      expect(lispish.run(`(filter (function number?) [1 "2" 3])`)).toEqual([1, 3])
      expect(lispish.run(`(filter (function number?) [])`)).toEqual([])
      expect(lispish.run(`(filter (function null?) [1 "2" 3])`)).toEqual([])
      expect(lispish.run(`(filter (lambda (x) (zero? (% x 3))) [0 1 2 3 4 5 6 7])`)).toEqual([0, 3, 6])
      expect(() => lispish.run(`(filter (function +))`)).toThrow()
      expect(() => lispish.run(`(filter)`)).toThrow()
      expect(() => lispish.run(`(filter (function number?) [1] 2)`)).toThrow()
    })
  })

  describe('find', () => {
    test('samples', () => {
      expect(lispish.run(`(find (function number?) ["1" "2" 3])`)).toEqual(3)
      expect(lispish.run(`(find (function number?) ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(find (function number?) [])`)).toBeUndefined()
      expect(lispish.run(`(find (lambda (x) (zero? (% x 3))) [1 2 3 4 5 6 7])`)).toEqual(3)
      expect(() => lispish.run(`(find (function +))`)).toThrow()
      expect(() => lispish.run(`(find)`)).toThrow()
      expect(() => lispish.run(`(find (function number?) [1] 2)`)).toThrow()
    })
  })

  describe('position', () => {
    test('samples', () => {
      expect(lispish.run(`(position (function number?) ["1" "2" 3])`)).toEqual(2)
      expect(lispish.run(`(position (function number?) ["1" "2" "3"])`)).toBeUndefined()
      expect(lispish.run(`(position (function number?) [])`)).toBeUndefined()
      expect(lispish.run(`(position (lambda (x) (zero? (% x 3))) [1 2 3 4 5 6 7])`)).toEqual(2)
      expect(() => lispish.run(`(position (function +))`)).toThrow()
      expect(() => lispish.run(`(position)`)).toThrow()
      expect(() => lispish.run(`(position (function number?) [1] 2)`)).toThrow()
    })
  })

  describe('some', () => {
    test('samples', () => {
      expect(lispish.run(`(some (function number?) ["1" "2" 3])`)).toBe(true)
      expect(lispish.run(`(some (function number?) ["1" "2" "3"])`)).toBe(false)
      expect(lispish.run(`(some (function number?) [])`)).toBe(false)
      expect(lispish.run(`(some (lambda (x) (zero? (% x 3))) [1 2 3 4 5 6 7])`)).toBe(true)
      expect(() => lispish.run(`(some (function +))`)).toThrow()
      expect(() => lispish.run(`(some)`)).toThrow()
      expect(() => lispish.run(`(some (function number?) [1] 2)`)).toThrow()
    })
  })

  describe('every', () => {
    test('samples', () => {
      expect(lispish.run(`(every (function number?) [1 2 3])`)).toBe(true)
      expect(lispish.run(`(every (function number?) ["1" "2" "3"])`)).toBe(false)
      expect(lispish.run(`(every (function number?) [])`)).toBe(false)
      expect(lispish.run(`(every (lambda (x) (zero? (% x 2))) [2 4 6])`)).toBe(true)
      expect(() => lispish.run(`(every (function +))`)).toThrow()
      expect(() => lispish.run(`(every)`)).toThrow()
      expect(() => lispish.run(`(every (function number?) [1] 2)`)).toThrow()
    })
  })

  describe('map', () => {
    test('samples', () => {
      expect(lispish.run(`(map (function number?) [1 "2" 3])`)).toEqual([true, false, true])
      expect(lispish.run(`(map (function number?) [])`)).toEqual([])
      expect(lispish.run(`(map #'+ [1 2 3] [1 2 3])`)).toEqual([2, 4, 6])
      expect(lispish.run(`(map #'max [2 6 3] [2 4 7] [1 6 2])`)).toEqual([2, 6, 7])
      expect(lispish.run(`(map (function null?) [1 "2" 3])`)).toEqual([false, false, false])
      expect(lispish.run(`(map (lambda (x) (zero? (% x 3))) [0 1 2 3 4 5 6 7])`)).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lispish.run(`(map (function 1+) [0 1 2 3 4 5 6 7])`)).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(() => lispish.run(`(map #'+ [1 2 3] [1 2])`)).toThrow()
      expect(() => lispish.run(`(map (function +))`)).toThrow()
      expect(() => lispish.run(`(map)`)).toThrow()
      expect(() => lispish.run(`(map (function number?) [1] 2)`)).toThrow()
    })
  })
  describe('first', () => {
    test('samples', () => {
      expect(lispish.run(`(first [1 2 3])`)).toEqual(1)
      expect(lispish.run(`(first ["1"])`)).toEqual('1')
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

  describe('second', () => {
    test('samples', () => {
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

  describe('reverse', () => {
    test('samples', () => {
      expect(lispish.run(`(reverse [1 2 3])`)).toEqual([3, 2, 1])
      expect(lispish.run(`(reverse ["1"])`)).toEqual(['1'])
      expect(lispish.run(`(reverse [])`)).toEqual([])

      expect(() => lispish.run(`(reverse`)).toThrow()
      expect(() => lispish.run(`(reverse "1")`)).toThrow()
      expect(() => lispish.run(`(reverse true)`)).toThrow()
      expect(() => lispish.run(`(reverse false)`)).toThrow()
      expect(() => lispish.run(`(reverse null)`)).toThrow()
      expect(() => lispish.run(`(reverse undefined)`)).toThrow()
      expect(() => lispish.run(`(reverse (object))`)).toThrow()
      expect(() => lispish.run(`(reverse 10)`)).toThrow()
    })
    test('returns a new array instance', () => {
      const program = `
        (setq l [1 2 3])
        (!= l (reverse l))
      `
      expect(lispish.run(program)).toBe(true)
    })
  })

  describe('last', () => {
    test('samples', () => {
      expect(lispish.run(`(last [1 2 3])`)).toEqual(3)
      expect(lispish.run(`(last ["1"])`)).toEqual('1')
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

  describe('rest', () => {
    test('samples', () => {
      expect(lispish.run(`(rest [1 2 3])`)).toEqual([2, 3])
      expect(lispish.run(`(rest [1 2])`)).toEqual([2])
      expect(lispish.run(`(rest ["1"])`)).toEqual(undefined)
      expect(lispish.run(`(rest [])`)).toEqual(undefined)

      expect(() => lispish.run(`(rest`)).toThrow()
      expect(() => lispish.run(`(rest "1")`)).toThrow()
      expect(() => lispish.run(`(rest true)`)).toThrow()
      expect(() => lispish.run(`(rest false)`)).toThrow()
      expect(() => lispish.run(`(rest null)`)).toThrow()
      expect(() => lispish.run(`(rest undefined)`)).toThrow()
      expect(() => lispish.run(`(rest (object))`)).toThrow()
      expect(() => lispish.run(`(rest 10)`)).toThrow()
    })
  })

  describe('cons', () => {
    test('samples', () => {
      expect(lispish.run(`(cons 0 [1 2 3])`)).toEqual([0, 1, 2, 3])
      expect(lispish.run(`(cons 0 ["1"])`)).toEqual([0, '1'])
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

  describe('selt', () => {
    test('samples', () => {
      expect(lispish.run(`(selt [1 2 3] 0 "1")`)).toEqual(['1', 2, 3])
      expect(lispish.run(`(selt [1 2 3] 1 "2")`)).toEqual([1, '2', 3])

      expect(() => lispish.run(`(selt [1 2 3] 3 "4")`)).toThrow()
      expect(() => lispish.run(`(selt (object) 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt null 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt undefined 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt true 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt false 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt 1 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt "1" 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] "0" "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] true "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] false "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] [] "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] null "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1] undefined "2")`)).toThrow()
      expect(() => lispish.run(`(selt 0 "2")`)).toThrow()
      expect(() => lispish.run(`(selt [1 2 3] -1 "x")`)).toThrow()
      expect(() => lispish.run(`(selt [1 2 3] 4 "x")`)).toThrow()
      expect(() => lispish.run(`(selt)`)).toThrow()
      expect(() => lispish.run(`(selt [])`)).toThrow()
      expect(() => lispish.run(`(selt [] 0)`)).toThrow()
      expect(() => lispish.run(`(selt [] 0 "x" "y")`)).toThrow()
    })
  })

  describe('push', () => {
    test('samples', () => {
      expect(lispish.run(`(push [1 2 3] 0)`)).toEqual([1, 2, 3, 0])
      expect(lispish.run(`(push [1 2 3] 1 "2")`)).toEqual([1, 2, 3, 1, '2'])
      expect(lispish.run(`(setq l [1 2 3]) (push l 1 "2") l`)).toEqual([1, 2, 3, 1, '2'])

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

  describe('pop', () => {
    test('samples', () => {
      expect(lispish.run(`(pop [1 2 3])`)).toBe(3)
      expect(lispish.run(`(pop [1 2 undefined])`)).toBeUndefined()
      expect(lispish.run(`(pop [])`)).toBeUndefined()
      expect(lispish.run(`(setq l [1 2 3]) (pop l) l`)).toEqual([1, 2])
      expect(lispish.run(`(setq l []) (pop l) l`)).toEqual([])

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

  describe('unshift', () => {
    test('samples', () => {
      expect(lispish.run(`(unshift [1 2 3] 0)`)).toEqual([0, 1, 2, 3])
      expect(lispish.run(`(unshift [1 2 3] 1 "2")`)).toEqual([1, '2', 1, 2, 3])
      expect(lispish.run(`(setq l [1 2 3]) (unshift l 1 "2") l`)).toEqual([1, '2', 1, 2, 3])

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

  describe('shift', () => {
    test('samples', () => {
      expect(lispish.run(`(shift [1 2 3])`)).toBe(1)
      expect(lispish.run(`(shift [undefined 2 3])`)).toBeUndefined()
      expect(lispish.run(`(shift [])`)).toBeUndefined()
      expect(lispish.run(`(setq l [1 2 3]) (shift l) l`)).toEqual([2, 3])
      expect(lispish.run(`(setq l []) (shift l) l`)).toEqual([])

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

  describe('take', () => {
    test('samples', () => {
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
    test('new list created', () => {
      const program = `
        (setq l1 [1 2 3])
        (setq l2 (take l1 2))
        (= l1 l2)
      `
      expect(lispish.run(program)).toBe(false)
    })
  })

  describe('take-while', () => {
    test('samples', () => {
      expect(lispish.run(`(take-while (lambda (x) (< x 3)) [1 2 3 2 1])`)).toEqual([1, 2])
      expect(lispish.run(`(take-while (lambda (x) (> x 3)) [1 2 3 2 1])`)).toEqual([])

      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) (object))`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) null)`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) undefined)`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) true)`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) false)`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) "1")`)).toThrow()
      expect(() => lispish.run(`(take-while 10 [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take-while)`)).toThrow()
      expect(() => lispish.run(`(take-while [1 2 3])`)).toThrow()
      expect(() => lispish.run(`(take-while (lambda (x) (< x 3)) [1 2 3] 1)`)).toThrow()
    })
    test('new list created', () => {
      const program = `
        (setq l1 [1 2 3])
        (setq l2 (take-while (lambda (x) (< x 3)) l1))
        (= l1 l2)
      `
      expect(lispish.run(program)).toBe(false)
    })
  })

  describe('sort', () => {
    test('samples', () => {
      expect(lispish.run(`(sort (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (true -1))) [3 1 2])`)).toEqual([1, 2, 3])
      expect(lispish.run(`(sort (lambda (a b) (cond ((> a b) -1) ((< a b) 1) (true -1))) [3 1 2])`)).toEqual([3, 2, 1])
      expect(lispish.run(`(sort (lambda (a b) (cond ((> a b) -1) ((< a b) 1) (true -1))) [])`)).toEqual([])
      expect(() => lispish.run(`(sort (lambda (a b) (cond ((> a b) -1) ((< a b) 1) (true -1))) 10)`)).toThrow()
      expect(() => lispish.run(`(sort (lambda (a b) (cond ((> a b) -1) ((< a b) 1) (true -1))))`)).toThrow()
      expect(() => lispish.run(`(sort [10])`)).toThrow()
      expect(() => lispish.run(`(sort)`)).toThrow()
    })
  })

  describe('join', () => {
    test('samples', () => {
      expect(lispish.run(`(join ["Albert" "Mojir"] " ")`)).toBe('Albert Mojir')
      expect(lispish.run(`(join (map #'number-to-string [0 1 2 3 4 5 6 7 8 9]) ", ")`)).toBe(
        '0, 1, 2, 3, 4, 5, 6, 7, 8, 9',
      )
      expect(() => lispish.run(`(join (map #'number-to-string [0 1 2 3 4 5 6 7 8 9]) ", " 5)`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" "Mojir"] " " -1)`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" "Mojir"])`)).toThrow()
      expect(() => lispish.run(`(join ["Albert" 10] " ")`)).toThrow()
    })
  })

  describe('includes', () => {
    test('samples', () => {
      expect(lispish.run(`(includes "Mojir" ["Albert" "Mojir"])`)).toBe(true)
      expect(lispish.run(`(includes 42 ["Albert" "Mojir" 42])`)).toBe(true)
      expect(lispish.run(`(includes 43 ["Albert" "Mojir" 42])`)).toBe(false)
      expect(lispish.run(`(includes undefined ["Albert" "Mojir" 42])`)).toBe(false)
      expect(lispish.run(`(includes undefined ["Albert" "Mojir" 42 undefined])`)).toBe(true)
      expect(() => lispish.run(`(includes "Albert")`)).toThrow()
      expect(() => lispish.run(`(includes ["Albert"])`)).toThrow()
      expect(() => lispish.run(`(includes "Albert" ["Albert"] 0)`)).toThrow()
    })
  })
})
