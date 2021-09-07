import { lispish } from '../../../src'

describe('list functions', () => {
  describe('list', () => {
    test('samples', () => {
      expect(lispish(`(list)`)).toEqual([])
      expect(lispish(`(list 1)`)).toEqual([1])
      expect(lispish(`(list 0 "1" null true false undefined (list (list)) (object))`)).toEqual([
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

  describe('lenght', () => {
    test('samples', () => {
      expect(lispish(`(length (list))`)).toBe(0)
      expect(lispish(`(length (list 1))`)).toBe(1)
      expect(lispish(`(length (list 1 2 3))`)).toBe(3)
      expect(() => lispish(`(length "")`)).toThrow()
      expect(() => lispish(`(length "1")`)).toThrow()
      expect(() => lispish(`(length "123")`)).toThrow()
      expect(() => lispish(`(length)`)).toThrow()
      expect(() => lispish(`(length (list) (list))`)).toThrow()
      expect(() => lispish(`(length 12)`)).toThrow()
      expect(() => lispish(`(length false)`)).toThrow()
      expect(() => lispish(`(length true)`)).toThrow()
      expect(() => lispish(`(length null)`)).toThrow()
      expect(() => lispish(`(length undefined)`)).toThrow()
      expect(() => lispish(`(length (object))`)).toThrow()
    })
  })

  describe('append', () => {
    test('samples', () => {
      expect(lispish(`(append)`)).toEqual([])
      expect(lispish(`(append (list))`)).toEqual([])
      expect(lispish(`(append (list 1))`)).toEqual([1])
      expect(lispish(`(append (list 1) "2")`)).toEqual([1, '2'])
      expect(lispish(`(append (list 1) (list 2))`)).toEqual([1, 2])
      expect(lispish(`(append (list 1 2 3) (list))`)).toEqual([1, 2, 3])
      expect(() => lispish(`(append "1")`)).toThrow()
      expect(() => lispish(`(append "1" (list "2"))`)).toThrow()
      expect(() => lispish(`(append 0)`)).toThrow()
      expect(() => lispish(`(append true)`)).toThrow()
      expect(() => lispish(`(append "1" false)`)).toThrow()
      expect(() => lispish(`(append null "m")`)).toThrow()
      expect(() => lispish(`(append undefined)`)).toThrow()
      expect(() => lispish(`(append (object))`)).toThrow()
    })
  })

  describe('elt', () => {
    test('samples', () => {
      expect(lispish('(elt (list 1 2 3) 1)')).toBe(2)
      expect(lispish('(elt (list 1 2 3) 3)')).toBeUndefined()
      expect(() => lispish('(elt (list 1 2 3) -1)')).toThrow()
      expect(() => lispish('(elt "Albert" 1)')).toThrow()
      expect(() => lispish('(elt)')).toThrow()
      expect(() => lispish('(elt (object) 1)')).toThrow()
      expect(() => lispish('(elt null 2)')).toThrow()
      expect(() => lispish('(elt (list 1 2 3))')).toThrow()
      expect(() => lispish('(elt (list 1 2 3) 1 2)')).toThrow()
    })
  })

  describe('slice', () => {
    test('samples', () => {
      expect(lispish('(slice (list 1 2 3))')).toEqual([1, 2, 3])
      expect(lispish('(slice (list 1 2 3) 0)')).toEqual([1, 2, 3])
      expect(lispish('(slice (list 1 2 3) 1)')).toEqual([2, 3])
      expect(lispish('(slice (list 1 2 3) -1)')).toEqual([3])
      expect(lispish('(slice (list 1 2 3) -3)')).toEqual([1, 2, 3])
      expect(lispish('(slice (list 1 2 3) -4)')).toEqual([1, 2, 3])
      expect(lispish('(slice (list 1 2 3) 3)')).toEqual([])
      expect(lispish('(slice (list 1 2 3) 4)')).toEqual([])
      expect(lispish('(slice (list 1 2 3) 0 0)')).toEqual([])
      expect(lispish('(slice (list 1 2 3) 0 1)')).toEqual([1])
      expect(lispish('(slice (list 1 2 3) 0 10)')).toEqual([1, 2, 3])
      expect(lispish('(slice (list 1 2 3) 0 -1)')).toEqual([1, 2])

      expect(() => lispish('(slice (list 1 2 3) 1 2 3)')).toThrow()
      expect(() => lispish('(slice "Albert" 1)')).toThrow()
      expect(() => lispish('(slice)')).toThrow()
      expect(() => lispish('(slice (object) 1)')).toThrow()
      expect(() => lispish('(slice null 2)')).toThrow()
    })
  })

  describe('reduce', () => {
    test('samples', () => {
      let program = `
      (defun countChars (stringArray)
        (reduce
          (lambda (sum str) (+ sum (stringLength str)))
          stringArray
          0
        )
      )

      (countChars (list "First" "Second" "Third"))
      `
      expect(lispish(program)).toBe(16)

      program = `
      (defun longestLength (stringArray)
        (reduce
          (lambda (sum str)
            (if (> sum (stringLength str))
              sum
              (stringLength str)
            )
          )
          stringArray
          0
        )
      )

      (longestLength (list "First" "Second" "Third"))
      `
      expect(lispish(program)).toBe(6)

      expect(lispish(`(reduce (function +) (list 1 2 3) 0)`)).toBe(6)
      expect(lispish(`(reduce (function +) (list) 0)`)).toBe(0)
      expect(lispish(`(reduce (function +) (list) 1)`)).toBe(1)
      expect(() => lispish(`(reduce (function +) (list 1 2 3))`)).toThrow()
      expect(() => lispish(`(reduce (function +))`)).toThrow()
      expect(() => lispish(`(reduce)`)).toThrow()
      expect(() => lispish(`(reduce (function +) 1 2)`)).toThrow()
    })
  })

  describe('reduceRight', () => {
    test('samples', () => {
      expect(lispish(`(reduceRight (function +) (list 1 2 3) 0)`)).toBe(6)
      expect(lispish(`(reduceRight (function +) (list) 0)`)).toBe(0)
      expect(lispish(`(reduceRight (function +) (list) 1)`)).toBe(1)
      expect(lispish(`(reduceRight (function concat) (list "1" "2" "3") "")`)).toBe('321')
      expect(() => lispish(`(reduceRight (function +))`)).toThrow()
      expect(() => lispish(`(reduceRight)`)).toThrow()
      expect(() => lispish(`(reduceRight (function +) 1 2)`)).toThrow()
    })
  })
  describe('filter', () => {
    test('samples', () => {
      expect(lispish(`(filter (function numberp) (list 1 "2" 3))`)).toEqual([1, 3])
      expect(lispish(`(filter (function numberp) (list))`)).toEqual([])
      expect(lispish(`(filter (function nullp) (list 1 "2" 3))`)).toEqual([])
      expect(lispish(`(filter (lambda (x) (zerop (% x 3))) (list 0 1 2 3 4 5 6 7))`)).toEqual([0, 3, 6])
      expect(() => lispish(`(filter (function +))`)).toThrow()
      expect(() => lispish(`(filter)`)).toThrow()
      expect(() => lispish(`(filter (function numberp) (list 1) 2)`)).toThrow()
    })
  })

  describe('map', () => {
    test('samples', () => {
      expect(lispish(`(map (function numberp) (list 1 "2" 3))`)).toEqual([true, false, true])
      expect(lispish(`(map (function numberp) (list))`)).toEqual([])
      expect(lispish(`(map (function nullp) (list 1 "2" 3))`)).toEqual([false, false, false])
      expect(lispish(`(map (lambda (x) (zerop (% x 3))) (list 0 1 2 3 4 5 6 7))`)).toEqual([
        true,
        false,
        false,
        true,
        false,
        false,
        true,
        false,
      ])
      expect(lispish(`(map (function 1+) (list 0 1 2 3 4 5 6 7))`)).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
      expect(() => lispish(`(map (function +))`)).toThrow()
      expect(() => lispish(`(map)`)).toThrow()
      expect(() => lispish(`(map (function numberp) (list 1) 2)`)).toThrow()
    })
  })
  describe('first', () => {
    test('samples', () => {
      expect(lispish('(first (list 1 2 3))')).toEqual(1)
      expect(lispish('(first (list "1"))')).toEqual('1')
      expect(lispish('(first (list))')).toBeUndefined()

      expect(() => lispish('(first')).toThrow()
      expect(() => lispish('(first "1")')).toThrow()
      expect(() => lispish('(first true)')).toThrow()
      expect(() => lispish('(first false)')).toThrow()
      expect(() => lispish('(first null)')).toThrow()
      expect(() => lispish('(first undefined)')).toThrow()
      expect(() => lispish('(first (object))')).toThrow()
      expect(() => lispish('(first 10)')).toThrow()
    })
  })

  describe('reverse', () => {
    test('samples', () => {
      expect(lispish('(reverse (list 1 2 3))')).toEqual([3, 2, 1])
      expect(lispish('(reverse (list "1"))')).toEqual(['1'])
      expect(lispish('(reverse (list))')).toEqual([])

      expect(() => lispish('(reverse')).toThrow()
      expect(() => lispish('(reverse "1")')).toThrow()
      expect(() => lispish('(reverse true)')).toThrow()
      expect(() => lispish('(reverse false)')).toThrow()
      expect(() => lispish('(reverse null)')).toThrow()
      expect(() => lispish('(reverse undefined)')).toThrow()
      expect(() => lispish('(reverse (object))')).toThrow()
      expect(() => lispish('(reverse 10)')).toThrow()
    })
  })

  describe('last', () => {
    test('samples', () => {
      expect(lispish('(last (list 1 2 3))')).toEqual(3)
      expect(lispish('(last (list "1"))')).toEqual('1')
      expect(lispish('(last (list))')).toBeUndefined()

      expect(() => lispish('(last')).toThrow()
      expect(() => lispish('(last "1")')).toThrow()
      expect(() => lispish('(last true)')).toThrow()
      expect(() => lispish('(last false)')).toThrow()
      expect(() => lispish('(last null)')).toThrow()
      expect(() => lispish('(last undefined)')).toThrow()
      expect(() => lispish('(last (object))')).toThrow()
      expect(() => lispish('(last 10)')).toThrow()
    })
  })

  describe('rest', () => {
    test('samples', () => {
      expect(lispish('(rest (list 1 2 3))')).toEqual([2, 3])
      expect(lispish('(rest (list 1 2))')).toEqual([2])
      expect(lispish('(rest (list "1"))')).toEqual([])
      expect(lispish('(rest (list))')).toEqual([])

      expect(() => lispish('(rest')).toThrow()
      expect(() => lispish('(rest "1")')).toThrow()
      expect(() => lispish('(rest true)')).toThrow()
      expect(() => lispish('(rest false)')).toThrow()
      expect(() => lispish('(rest null)')).toThrow()
      expect(() => lispish('(rest undefined)')).toThrow()
      expect(() => lispish('(rest (object))')).toThrow()
      expect(() => lispish('(rest 10)')).toThrow()
    })
  })

  describe('cons', () => {
    test('samples', () => {
      expect(lispish('(cons 0 (list 1 2 3))')).toEqual([0, 1, 2, 3])
      expect(lispish('(cons 0 (list "1"))')).toEqual([0, '1'])
      expect(lispish('(cons 0 (list))')).toEqual([0])

      expect(() => lispish('(cons')).toThrow()
      expect(() => lispish('(cons 1 "1")')).toThrow()
      expect(() => lispish('(cons 1 true)')).toThrow()
      expect(() => lispish('(cons 1 false)')).toThrow()
      expect(() => lispish('(cons 1 null)')).toThrow()
      expect(() => lispish('(cons 1 undefined)')).toThrow()
      expect(() => lispish('(cons 1 (object))')).toThrow()
      expect(() => lispish('(cons 1 10)')).toThrow()
    })
  })
})
