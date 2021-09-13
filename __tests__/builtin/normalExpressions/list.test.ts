import { lispish } from '../../../src'

describe('list functions', () => {
  describe('list', () => {
    test('samples', () => {
      expect(lispish(`(list)`)).toEqual([])
      expect(lispish(`(list 1)`)).toEqual([1])
      expect((lispish(`(list undefined)`) as unknown[])[0]).toEqual(undefined)
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

  describe('listf', () => {
    test('samples', () => {
      expect(lispish(`(listf 5 1)`)).toEqual([1, 1, 1, 1, 1])
      expect(lispish(`(listf 5 (listf 2 2))`)).toEqual([
        [2, 2],
        [2, 2],
        [2, 2],
        [2, 2],
        [2, 2],
      ])
      expect(() => lispish(`(listf)`)).toThrow()
      expect(() => lispish(`(listf 0 'x')`)).toThrow()
      expect(() => lispish(`(listf 1 'x' 'y')`)).toThrow()
      expect(() => lispish(`(listf false 'x' 'y')`)).toThrow()
      expect(() => lispish(`(listf null 'x' 'y')`)).toThrow()
      expect(() => lispish(`(listf (object) 'x' 'y')`)).toThrow()
    })
  })

  describe('range', () => {
    test('samples', () => {
      expect(lispish(`(range 0)`)).toEqual([])
      expect(lispish(`(range 5)`)).toEqual([0, 1, 2, 3, 4])
      expect(lispish(`(range -5)`)).toEqual([0, -1, -2, -3, -4])
      expect(lispish(`(range 5 1)`)).toEqual([5, 4, 3, 2])
      expect(lispish(`(range 1 5)`)).toEqual([1, 2, 3, 4])
      expect(lispish(`(range 5 1 -2)`)).toEqual([5, 3])
      expect(lispish(`(range 0 0.5 0.125)`)).toEqual([0, 0.125, 0.25, 0.375])
      expect(() => lispish(`(range)`)).toThrow()
      expect(() => lispish(`(range 0 2 1 1)`)).toThrow()
      expect(() => lispish(`(range 0 2 0)`)).toThrow()
      expect(() => lispish(`(range 0 0 0)`)).toThrow()
      expect(() => lispish(`(range 1 'x')`)).toThrow()
      expect(() => lispish(`(range false 1 2)`)).toThrow()
      expect(() => lispish(`(range 0 2 'y')`)).toThrow()
      expect(() => lispish(`(range (object) 'x' 'y')`)).toThrow()
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
      expect(lispish(`(append (list))`)).toEqual([])
      expect(lispish(`(append (list 1))`)).toEqual([1])
      expect(lispish(`(append (list 1) (list 2) (list 3 4))`)).toEqual([1, 2, 3, 4])
      expect(lispish(`(append (list 1 2 3) (list))`)).toEqual([1, 2, 3])
      expect(() => lispish(`(append)`)).toThrow()
      expect(() => lispish(`(append (list 1) "2")`)).toThrow()
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

  describe('splice', () => {
    test('samples', () => {
      expect(lispish('(setq l (list 1 2 3)) (splice l 0)')).toEqual([1, 2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 0) l')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1)')).toEqual([2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1) l')).toEqual([1])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 0)')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 0) l')).toEqual([1, 2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1)')).toEqual([2])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1) l')).toEqual([1, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 2)')).toEqual([2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 2) l')).toEqual([1])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 100)')).toEqual([2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 100) l')).toEqual([1])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1 "x")')).toEqual([2])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1 "x") l')).toEqual([1, 'x', 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 0 "x")')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 0 "x") l')).toEqual([1, 'x', 2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 100 0 "x")')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l 100 0 "x") l')).toEqual([1, 2, 3, 'x'])
      expect(lispish('(setq l (list 1 2 3)) (splice l -1 0 "x")')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l -1 0 "x") l')).toEqual([1, 2, 'x', 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l -100 0 "x")')).toEqual([])
      expect(lispish('(setq l (list 1 2 3)) (splice l -100 0 "x") l')).toEqual(['x', 1, 2, 3])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1 "x" "y" "z")')).toEqual([2])
      expect(lispish('(setq l (list 1 2 3)) (splice l 1 1 "x" "y" "z") l')).toEqual([1, 'x', 'y', 'z', 3])
      expect(() => lispish('(splice (list 1 2 3))')).toThrow()
      expect(() => lispish('(splice))')).toThrow()
      expect(() => lispish('(splice (list 1 2 3) "1")')).toThrow()
      expect(() => lispish('(splice (list 1 2 3) 1 "2")')).toThrow()
      expect(() => lispish('(splice (object) 1 2)')).toThrow()
      expect(() => lispish('(splice true 1 2)')).toThrow()
      expect(() => lispish('(splice false 1 2)')).toThrow()
      expect(() => lispish('(splice "1" 1 2)')).toThrow()
      expect(() => lispish('(splice 1 1 2)')).toThrow()
      expect(() => lispish('(splice null 1 2)')).toThrow()
      expect(() => lispish('(splice undefined 1 2)')).toThrow()
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

      (countChars (list "First" "Second" "Third"))
      `
      expect(lispish(program)).toBe(16)

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

  describe('reduce-right', () => {
    test('samples', () => {
      expect(lispish(`(reduce-right (function +) (list 1 2 3) 0)`)).toBe(6)
      expect(lispish(`(reduce-right (function +) (list) 0)`)).toBe(0)
      expect(lispish(`(reduce-right (function +) (list) 1)`)).toBe(1)
      expect(lispish(`(reduce-right (function concat) (list "1" "2" "3") "")`)).toBe('321')
      expect(() => lispish(`(reduce-right (function +))`)).toThrow()
      expect(() => lispish(`(reduce-right)`)).toThrow()
      expect(() => lispish(`(reduce-right (function +) 1 2)`)).toThrow()
    })
  })
  describe('filter', () => {
    test('samples', () => {
      expect(lispish(`(filter (function number?) (list 1 "2" 3))`)).toEqual([1, 3])
      expect(lispish(`(filter (function number?) (list))`)).toEqual([])
      expect(lispish(`(filter (function null?) (list 1 "2" 3))`)).toEqual([])
      expect(lispish(`(filter (lambda (x) (zero? (% x 3))) (list 0 1 2 3 4 5 6 7))`)).toEqual([0, 3, 6])
      expect(() => lispish(`(filter (function +))`)).toThrow()
      expect(() => lispish(`(filter)`)).toThrow()
      expect(() => lispish(`(filter (function number?) (list 1) 2)`)).toThrow()
    })
  })

  describe('map', () => {
    test('samples', () => {
      expect(lispish(`(map (function number?) (list 1 "2" 3))`)).toEqual([true, false, true])
      expect(lispish(`(map (function number?) (list))`)).toEqual([])
      expect(lispish(`(map (function null?) (list 1 "2" 3))`)).toEqual([false, false, false])
      expect(lispish(`(map (lambda (x) (zero? (% x 3))) (list 0 1 2 3 4 5 6 7))`)).toEqual([
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
      expect(() => lispish(`(map (function number?) (list 1) 2)`)).toThrow()
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

  describe('selt', () => {
    test('samples', () => {
      expect(lispish('(selt (list 1 2 3) 0 "1")')).toEqual(['1', 2, 3])
      expect(lispish('(selt (list 1 2 3) 1 "2")')).toEqual([1, '2', 3])

      expect(() => lispish('(selt (list 1 2 3) 3 "4")')).toThrow()
      expect(() => lispish('(selt (object) 0 "2")')).toThrow()
      expect(() => lispish('(selt null 0 "2")')).toThrow()
      expect(() => lispish('(selt undefined 0 "2")')).toThrow()
      expect(() => lispish('(selt true 0 "2")')).toThrow()
      expect(() => lispish('(selt false 0 "2")')).toThrow()
      expect(() => lispish('(selt 1 0 "2")')).toThrow()
      expect(() => lispish('(selt "1" 0 "2")')).toThrow()
      expect(() => lispish('(selt (list 1) "0" "2")')).toThrow()
      expect(() => lispish('(selt (list 1) true "2")')).toThrow()
      expect(() => lispish('(selt (list 1) false "2")')).toThrow()
      expect(() => lispish('(selt (list 1) (list) "2")')).toThrow()
      expect(() => lispish('(selt (list 1) null "2")')).toThrow()
      expect(() => lispish('(selt (list 1) undefined "2")')).toThrow()
      expect(() => lispish('(selt 0 "2")')).toThrow()
      expect(() => lispish('(selt (list 1 2 3) -1 "x")')).toThrow()
      expect(() => lispish('(selt (list 1 2 3) 4 "x")')).toThrow()
      expect(() => lispish('(selt)')).toThrow()
      expect(() => lispish('(selt (list))')).toThrow()
      expect(() => lispish('(selt (list) 0)')).toThrow()
      expect(() => lispish('(selt (list) 0 "x" "y")')).toThrow()
    })
  })

  describe('push', () => {
    test('samples', () => {
      expect(lispish('(push (list 1 2 3) 0)')).toEqual([1, 2, 3, 0])
      expect(lispish('(push (list 1 2 3) 1 "2")')).toEqual([1, 2, 3, 1, '2'])
      expect(lispish('(setq l (list 1 2 3)) (push l 1 "2") l')).toEqual([1, 2, 3, 1, '2'])

      expect(() => lispish('(push (list 1 2 3))')).toThrow()
      expect(() => lispish('(push (object) 0 "2")')).toThrow()
      expect(() => lispish('(push null 0 "2")')).toThrow()
      expect(() => lispish('(push undefined 0 "2")')).toThrow()
      expect(() => lispish('(push true 0 "2")')).toThrow()
      expect(() => lispish('(push false 0 "2")')).toThrow()
      expect(() => lispish('(push 1 0 "2")')).toThrow()
      expect(() => lispish('(push "1" 0 "2")')).toThrow()
      expect(() => lispish('(push 0 "2")')).toThrow()
      expect(() => lispish('(push)')).toThrow()
    })
  })

  describe('pop', () => {
    test('samples', () => {
      expect(lispish('(pop (list 1 2 3))')).toBe(3)
      expect(lispish('(pop (list 1 2 undefined))')).toBeUndefined()
      expect(lispish('(pop (list))')).toBeUndefined()
      expect(lispish('(setq l (list 1 2 3)) (pop l) l')).toEqual([1, 2])
      expect(lispish('(setq l (list)) (pop l) l')).toEqual([])

      expect(() => lispish('(pop (object))')).toThrow()
      expect(() => lispish('(pop null)')).toThrow()
      expect(() => lispish('(pop undefined)')).toThrow()
      expect(() => lispish('(pop true)')).toThrow()
      expect(() => lispish('(pop false)')).toThrow()
      expect(() => lispish('(pop 1)')).toThrow()
      expect(() => lispish('(pop "1")')).toThrow()
      expect(() => lispish('(pop)')).toThrow()
    })
  })

  describe('unshift', () => {
    test('samples', () => {
      expect(lispish('(unshift (list 1 2 3) 0)')).toBe(4)
      expect(lispish('(unshift (list 1 2 3) 1 "2")')).toBe(5)
      expect(lispish('(unshift (list 1 2 3))')).toBe(3)
      expect(lispish('(setq l (list 1 2 3)) (unshift l 1 "2") l')).toEqual([1, '2', 1, 2, 3])
      expect(lispish('(setq l (list 1 2 3)) (unshift l) l')).toEqual([1, 2, 3])

      expect(() => lispish('(unshift (object) 0 "2")')).toThrow()
      expect(() => lispish('(unshift null 0 "2")')).toThrow()
      expect(() => lispish('(unshift undefined 0 "2")')).toThrow()
      expect(() => lispish('(unshift true 0 "2")')).toThrow()
      expect(() => lispish('(unshift false 0 "2")')).toThrow()
      expect(() => lispish('(unshift 1 0 "2")')).toThrow()
      expect(() => lispish('(unshift "1" 0 "2")')).toThrow()
      expect(() => lispish('(unshift 0 "2")')).toThrow()
      expect(() => lispish('(unshift)')).toThrow()
    })
  })

  describe('shift', () => {
    test('samples', () => {
      expect(lispish('(shift (list 1 2 3))')).toBe(1)
      expect(lispish('(shift (list undefined 2 3))')).toBeUndefined()
      expect(lispish('(shift (list))')).toBeUndefined()
      expect(lispish('(setq l (list 1 2 3)) (shift l) l')).toEqual([2, 3])
      expect(lispish('(setq l (list)) (shift l) l')).toEqual([])

      expect(() => lispish('(shift (object))')).toThrow()
      expect(() => lispish('(shift null)')).toThrow()
      expect(() => lispish('(shift undefined)')).toThrow()
      expect(() => lispish('(shift true)')).toThrow()
      expect(() => lispish('(shift false)')).toThrow()
      expect(() => lispish('(shift 1)')).toThrow()
      expect(() => lispish('(shift "1")')).toThrow()
      expect(() => lispish('(shift)')).toThrow()
    })
  })
})
