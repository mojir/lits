import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'

describe('pattern matching (match)', () => {
  const lits = new Lits()
  const litsDebug = new Lits({ debug: true })

  for (const l of [lits, litsDebug]) {
    describe('literal patterns', () => {
      it('should match number literals', () => {
        expect(l.run('match 1 case 1 then "one" case 2 then "two" end')).toBe('one')
        expect(l.run('match 2 case 1 then "one" case 2 then "two" end')).toBe('two')
        expect(l.run('match 3 case 1 then "one" case 2 then "two" end')).toBeNull()
      })

      it('should match string literals', () => {
        expect(l.run('match "hello" case "hello" then 1 case "world" then 2 end')).toBe(1)
        expect(l.run('match "world" case "hello" then 1 case "world" then 2 end')).toBe(2)
        expect(l.run('match "other" case "hello" then 1 case "world" then 2 end')).toBeNull()
      })

      it('should match boolean literals', () => {
        expect(l.run('match true case true then "yes" case false then "no" end')).toBe('yes')
        expect(l.run('match false case true then "yes" case false then "no" end')).toBe('no')
      })

      it('should match null literal', () => {
        expect(l.run('match null case null then "nothing" case 1 then "one" end')).toBe('nothing')
        expect(l.run('match 1 case null then "nothing" case 1 then "one" end')).toBe('one')
      })

      it('should use deepEqual for matching', () => {
        expect(l.run('match 0 case 0 then "zero" end')).toBe('zero')
      })
    })

    describe('wildcard pattern', () => {
      it('should match anything with _', () => {
        expect(l.run('match 42 case _ then "matched" end')).toBe('matched')
        expect(l.run('match "hello" case _ then "matched" end')).toBe('matched')
        expect(l.run('match null case _ then "matched" end')).toBe('matched')
      })

      it('should work as catch-all after other patterns', () => {
        expect(l.run('match 3 case 1 then "one" case 2 then "two" case _ then "other" end')).toBe('other')
      })
    })

    describe('variable binding patterns', () => {
      it('should bind matched value to variable', () => {
        expect(l.run('match 42 case x then x + 1 end')).toBe(43)
      })

      it('should bind and make available in body', () => {
        expect(l.run('match "hello" case greeting then greeting ++ "!" end')).toBe('hello!')
      })
    })

    describe('array patterns', () => {
      it('should match empty array', () => {
        expect(l.run('match [] case [] then "empty" case _ then "not empty" end')).toBe('empty')
      })

      it('should match array with exact length', () => {
        expect(l.run('match [1, 2] case [x] then "one" case [x, y] then "two" case _ then "other" end')).toBe('two')
      })

      it('should bind array elements to variables', () => {
        expect(l.run('match [10, 20] case [x, y] then x + y end')).toBe(30)
      })

      it('should match array with literal elements', () => {
        expect(l.run('match [1, 2, 3] case [1, x, 3] then x end')).toBe(2)
        expect(l.run('match [2, 2, 3] case [1, x, 3] then x case _ then "no match" end')).toBe('no match')
      })

      it('should match array with rest pattern', () => {
        expect(l.run('match [1, 2, 3, 4] case [x, ...xs] then xs end')).toEqual([2, 3, 4])
      })

      it('should match single element with rest', () => {
        expect(l.run('match [1] case [x, ...xs] then xs end')).toEqual([])
      })

      it('should not match array when too few elements', () => {
        expect(l.run('match [1] case [x, y] then "two" case _ then "no" end')).toBe('no')
      })

      it('should not match non-array values against array pattern', () => {
        expect(l.run('match 42 case [x] then "array" case _ then "not array" end')).toBe('not array')
        expect(l.run('match "hello" case [x] then "array" case _ then "not array" end')).toBe('not array')
      })

      it('should support nested array patterns', () => {
        expect(l.run('match [[1, 2], [3, 4]] case [[a, b], [c, d]] then a + b + c + d end')).toBe(10)
      })
    })

    describe('object patterns', () => {
      it('should match and bind object properties', () => {
        expect(l.run('match { name: "Alice", age: 30 } case { name, age } then name ++ " is " ++ str(age) end')).toBe('Alice is 30')
      })

      it('should match object with literal property values', () => {
        expect(l.run(`
match { type: "click", x: 10, y: 20 }
  case { type: "click", x, y } then x + y
  case { type: "keydown", key } then key
  case _ then "unknown"
end`)).toBe(30)
      })

      it('should match second case when first literal doesn\'t match', () => {
        expect(l.run(`
match { type: "keydown", key: "Enter" }
  case { type: "click", x, y } then x + y
  case { type: "keydown", key } then key
  case _ then "unknown"
end`)).toBe('Enter')
      })

      it('should match object with rest pattern', () => {
        expect(l.run('match { a: 1, b: 2, c: 3 } case { a, ...r } then r end')).toEqual({ b: 2, c: 3 })
      })

      it('should not match non-object values against object pattern', () => {
        expect(l.run('match 42 case { x } then "object" case _ then "not object" end')).toBe('not object')
        expect(l.run('match [1, 2] case { x } then "object" case _ then "not object" end')).toBe('not object')
      })

      it('should match object with nested object patterns', () => {
        expect(l.run(`
match { user: { name: "Alice", profile: { email: "alice@example.com" } } }
  case { user: { name, profile: { email } } } then name ++ ": " ++ email
end`)).toBe('Alice: alice@example.com')
      })

      it('should match object with nested array patterns', () => {
        expect(l.run(`
match { scores: [10, 20, 30] }
  case { scores: [hd, ...tl] } then hd
end`)).toBe(10)
      })

      it('should handle default values in object patterns', () => {
        expect(l.run('match {} case { name = "Anonymous" } then name end')).toBe('Anonymous')
        expect(l.run('match { name: "Alice" } case { name = "Anonymous" } then name end')).toBe('Alice')
      })

      it('should use `as` for renaming in object patterns', () => {
        expect(l.run('match { name: "Alice" } case { name as n } then n end')).toBe('Alice')
      })
    })

    describe('guard clauses (when)', () => {
      it('should check guard after pattern match', () => {
        expect(l.run(`
match 5
  case x when x > 10 then "big"
  case x when x > 0 then "small positive"
  case x then "non-positive"
end`)).toBe('small positive')
      })

      it('should use bound variables in guard', () => {
        expect(l.run(`
match { age: 25 }
  case { age } when age >= 18 then "adult"
  case { age } then "minor"
end`)).toBe('adult')
      })

      it('should fall through to next case when guard fails', () => {
        expect(l.run(`
match { role: "admin", name: "Alice" }
  case { role: "admin", name } when name == "Bob" then "Admin Bob"
  case { role: "admin", name } then "Admin: " ++ name
  case _ then "unknown"
end`)).toBe('Admin: Alice')
      })

      it('should combine with literal patterns and guards', () => {
        expect(l.run(`
match [1, 2, 3]
  case [x, y, z] when x + y + z > 10 then "big sum"
  case [x, y, z] when x + y + z > 5 then "medium sum"
  case [x, y, z] then "small sum: " ++ str(x + y + z)
end`)).toBe('medium sum')
      })
    })

    describe('mixed patterns', () => {
      it('should handle complex nested matching', () => {
        expect(l.run(`
match { data: { users: [{ name: "Alice" }, { name: "Bob" }] } }
  case { data: { users: [hd, ...tl] } } then hd
end`)).toEqual({ name: 'Alice' })
      })

      it('should try patterns in order and return first match', () => {
        expect(l.run(`
match [1, 2]
  case [] then "empty"
  case [x] then "one"
  case [x, y] then "two: " ++ str(x) ++ ", " ++ str(y)
  case _ then "many"
end`)).toBe('two: 1, 2')
      })

      it('should return null when no pattern matches', () => {
        expect(l.run('match [1, 2, 3] case 1 then "one" case "hello" then "greeting" end')).toBeNull()
      })
    })

    describe('practical examples', () => {
      it('should handle http response matching', () => {
        const prog = `
let handle-response = (response) ->
  match response
    case { status: 200, body } then "OK: " ++ body
    case { status: 404 } then "Not found"
    case { status } when status >= 500 then "Server error"
    case _ then "Unknown"
  end;

handle-response({ status: 200, body: "Hello" })`
        expect(l.run(prog)).toBe('OK: Hello')
      })

      it('should handle recursive list processing', () => {
        const prog = `
let sum-list = (lst) ->
  match lst
    case [] then 0
    case [x, ...xs] then x + sum-list(xs)
  end;

sum-list([1, 2, 3, 4, 5])`
        expect(l.run(prog)).toBe(15)
      })

      it('should handle point/coordinate matching', () => {
        const prog = `
let describe-point = (point) ->
  match point
    case [0, 0] then "origin"
    case [0, y] then "y-axis"
    case [x, 0] then "x-axis"
    case [x, y] then "point at " ++ str(x) ++ ", " ++ str(y)
  end;

[describe-point([0, 0]), describe-point([0, 5]), describe-point([3, 0]), describe-point([3, 4])]`
        expect(l.run(prog)).toEqual(['origin', 'y-axis', 'x-axis', 'point at 3, 4'])
      })
    })

    describe('error cases', () => {
      it('should reject non-literal value after : in object pattern', () => {
        expect(() => l.run('match { a: 1 } case { a: b } then b end')).toThrow()
      })
    })

    describe('edge cases for coverage', () => {
      it('should use default value when variable pattern matches null', () => {
        expect(l.run('match null case x = 10 then x end')).toBe(10)
      })

      it('should bind null when variable pattern matches null without default', () => {
        expect(l.run('match null case x then x end')).toBeNull()
      })

      it('should not match object literal constraint when key is missing', () => {
        expect(l.run('match { a: 1 } case { a: 1, b: 2 } then "matched" case _ then "no" end')).toBe('no')
      })

      it('should use default when object key is missing in pattern', () => {
        expect(l.run('match { a: 1 } case { a, b = 99 } then a + b end')).toBe(100)
      })

      it('should bind null when object key is missing without default', () => {
        expect(l.run('match { a: 1 } case { a, b } then str(a) ++ str(b) end')).toBe('1')
      })

      it('should not match array with rest when too few elements for rest position', () => {
        expect(l.run('match [] case [x, y, ...xs] then "matched" case _ then "no" end')).toBe('no')
      })

      it('should use default value for missing array element in pattern', () => {
        expect(l.run('match [1, null] case [x, y = 20] then x + y end')).toBe(21)
      })

      it('should handle skipped positions in array pattern', () => {
        expect(l.run('match [1, 2, 3] case [x,, z] then x + z end')).toBe(4)
      })

      it('should support wildcard inside array patterns', () => {
        expect(l.run('match [1, 2, 3] case [_, x, _] then x end')).toBe(2)
        expect(l.run('match [1, 2] case [_, x, _] then x case _ then "no" end')).toBe('no')
      })

      it('should reject array when nested literal does not match', () => {
        expect(l.run('match [1, 3] case [1, 2] then "yes" case _ then "no" end')).toBe('no')
      })

      it('should match empty object pattern against any object', () => {
        expect(l.run('match { a: 1, b: 2 } case {} then "matched" end')).toBe('matched')
        expect(l.run('match {} case {} then "matched" end')).toBe('matched')
      })

      it('should not trigger default for falsy non-null array values (0, false, empty string)', () => {
        expect(l.run('match [0] case [x = 99] then x end')).toBe(0)
        expect(l.run('match [false] case [x = 99] then x end')).toBe(false)
        expect(l.run('match [""] case [x = 99] then x end')).toBe('')
      })

      it('should trigger default for null object values', () => {
        expect(l.run('match { a: null } case { a = 99 } then a end')).toBe(99)
      })

      it('should not trigger default for falsy non-null object values (0, false, empty string)', () => {
        expect(l.run('match { a: 0 } case { a = 99 } then a end')).toBe(0)
        expect(l.run('match { a: false } case { a = 99 } then a end')).toBe(false)
        expect(l.run('match { a: "" } case { a = 99 } then a end')).toBe('')
      })

      it('should handle skipped positions before rest in array pattern', () => {
        expect(l.run('match [1, 2, 3, 4, 5] case [,, ...xs] then xs end')).toEqual([3, 4, 5])
      })
    })

    describe('undefined symbols analysis', () => {
      it('should recognize bound pattern variables as defined', () => {
        expect(l.getUndefinedSymbols('match val case x then x end')).toEqual(
          new Set(['val']),
        )
      })

      it('should recognize destructured variables as defined', () => {
        expect(l.getUndefinedSymbols('match val case { name, age } then name ++ str(age) end')).toEqual(
          new Set(['val']),
        )
      })

      it('should handle guard expressions', () => {
        expect(l.getUndefinedSymbols('match val case x when x > threshold then x end')).toEqual(
          new Set(['val', 'threshold']),
        )
      })

      it('should not leak pattern variables across cases', () => {
        expect(l.getUndefinedSymbols('match val case x then x case y then y end')).toEqual(
          new Set(['val']),
        )
      })
    })
  }
})
