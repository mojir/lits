import { describe, expect, test } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { LitsError } from '../src/errors'

const lits = new Lits()

describe('lits Destructuring', () => {
  // Basic object destructuring
  describe('basic object destructuring', () => {
    test('simple property extraction', () => {
      expect(lits.run(`
        let { name } = { name: "Alice" };
        name
      `)).toBe('Alice')
    })

    test('multiple property extraction', () => {
      expect(lits.run(`
        let { name, age } = { name: "Bob", age: 30 };
        name ++ " is " ++ str(age)
      `)).toBe('Bob is 30')
    })

    test('property not in object returns null', () => {
      expect(lits.run(`
        let { missing } = { name: "Charlie" };
        missing
      `)).toBe(null)
    })
  })

  // Renaming with 'as'
  describe('renaming with "as"', () => {
    test('basic property renaming', () => {
      expect(lits.run(`
        let { name as userName } = { name: "Dave" };
        userName
      `)).toBe('Dave')
    })

    test('multiple renames', () => {
      expect(lits.run(`
        let { firstName as name, age as years } = { firstName: "Eve", age: 28 };
        name ++ " is " ++ str(years) ++ " years old"
      `)).toBe('Eve is 28 years old')
    })

    test('renaming with original property name still inaccessible', () => {
      expect(lits.run(`
        let { name as userName } = { name: "Frank" };
        defined?(name)
      `)).toBe(false)
    })
  })

  // Default values
  describe('default values', () => {
    test('default when property is missing', () => {
      expect(lits.run(`
        let { name = "Anonymous" } = {};
        name
      `)).toBe('Anonymous')
    })

    test('default not used when property exists', () => {
      expect(lits.run(`
        let { name = "Anonymous" } = { name: "Grace" };
        name
      `)).toBe('Grace')
    })

    test('multiple defaults', () => {
      expect(lits.run(`
        let { name = "Anonymous", age = 0 } = {};
        name ++ ":" ++ str(age)
      `)).toBe('Anonymous:0')
    })

    test('null values does not use default', () => {
      expect(lits.run(`
        let { name = "Anonymous" } = { name: null };
        name
      `)).toBeNull()
    })
  })

  // Combining renaming and defaults
  describe('combining renaming and defaults', () => {
    test('rename with default', () => {
      expect(lits.run(`
        let { name as userName = "Anonymous" } = {};
        userName
      `)).toBe('Anonymous')
    })

    test('rename with existing value', () => {
      expect(lits.run(`
        let { name as userName = "Anonymous" } = { name: "Helen" };
        userName
      `)).toBe('Helen')
    })
  })

  // Nested destructuring
  describe('nested destructuring', () => {
    test('basic nested property', () => {
      expect(lits.run(`
        let { user { name }} = { user: { name: "Ian" }};
        name
      `)).toBe('Ian')
    })

    test('multiple nested properties', () => {
      expect(lits.run(`
        let { user { name, age }} = { user: { name: "Jane", age: 27 }};
        name ++ ":" ++ str(age)
      `)).toBe('Jane:27')
    })

    test('deeply nested properties', () => {
      expect(lits.run(`
        let { user { profile { email }}} = { user: { profile: { email: "kevin@example.com" }}};
        email
      `)).toBe('kevin@example.com')
    })

    test('nested property from missing parent throws', () => {
      expect(() => lits.run(`
        let { user { name }}: {};
        name
      `)).toThrow(LitsError)
    })
  })

  // Defaults in nested structures
  describe('defaults in nested structures', () => {
    test('default for nested property', () => {
      expect(lits.run(`
        let { user { name = "Anonymous" }} = { user: {}};
        name
      `)).toBe('Anonymous')
    })

    test('default for entire nested object', () => {
      expect(lits.run(`
        let { user = { name: "Anonymous" }} = {};
        user.name
      `)).toBe('Anonymous')
    })

    test('default for nested structure pattern', () => {
      expect(lits.run(`
        let { user { name } = { name: "Default" }} = {};
        name
      `)).toBe('Default')
    })
  })

  // Array destructuring
  describe('array destructuring', () => {
    test('basic array elements', () => {
      expect(lits.run(`
        let [one, two] = [1, 2, 3];
        one + two
      `)).toBe(3)
    })

    test('skipping elements', () => {
      expect(lits.run(`
        let [one, , third] = [1, 2, 3];
        one + third
      `)).toBe(4)
    })

    test('elements beyond array length are null', () => {
      expect(lits.run(`
        let [one, two, third] = [1];
        [one, two, third]
      `)).toEqual([1, null, null])
    })
  })

  // Array defaults
  describe('array destructuring with defaults', () => {
    test('default for missing array element', () => {
      expect(lits.run(`
        let [one, two = 2] = [1];
        one + two
      `)).toBe(3)
    })

    test('multiple defaults in arrays', () => {
      expect(lits.run(`
        let [one = 1, two = 2] = [];
        one + two
      `)).toBe(3)
    })

    test('default for skipped element', () => {
      expect(lits.run(`
        let [, two = 2] = [];
        two
      `)).toBe(2)
    })
  })

  // Rest pattern
  describe('rest pattern', () => {
    test('basic rest in array', () => {
      expect(lits.run(`
        let [one, ...others] = [1, 2, 3, 4];
        [one, others]
      `)).toEqual([1, [2, 3, 4]])
    })

    test('empty rest in array', () => {
      expect(lits.run(`
        let [one, ...others] = [1];
        [one, others]
      `)).toEqual([1, []])
    })

    test('rest in object', () => {
      expect(lits.run(`
        let { name, ...others } = { name: "Linda", age: 31, city: "Boston" };
        [name, others]
      `)).toEqual(['Linda', { age: 31, city: 'Boston' }])
    })

    test('empty rest in object', () => {
      expect(lits.run(`
        let { name, ...others } = { name: "Marcus" };
        [name, others]
      `)).toEqual(['Marcus', {}])
    })
  })

  // Function parameters
  describe('destructuring in function parameters', () => {
    test('basic parameter destructuring', () => {
      expect(lits.run(`
        function greet({ name }) {
          "Hello, " ++ name;
        };
        greet({ name: "Pat" });
      `)).toBe('Hello, Pat')
    })

    test('parameter with default', () => {
      expect(lits.run(`
        function greet({ name = "friend" }) {
          "Hello, " ++ name;
        };
        greet({});
      `)).toBe('Hello, friend')
    })

    test('parameter with rename', () => {
      expect(lits.run(`
        function foo({ a as b = 10 }) {
          b;
        };
        foo({ b: 1 });
      `)).toBe(10)
    })

    test('nested parameter destructuring', () => {
      expect(lits.run(`
        function processUser({ profile { name, age }}) {
          name ++ " is " ++ str(age);
        };
        processUser({ profile: { name: "Quinn", age: 29 }});
      `)).toBe('Quinn is 29')
    })

    test('array parameter destructuring', () => {
      expect(lits.run(`
        function processCoords([x, y]) {
          x + y;
        };
        processCoords([3, 4]);
      `)).toBe(7)
    })
  })

  // Edge cases
  describe('edge cases', () => {
    test('destructuring a number should fail gracefully', () => {
      expect(lits.run(`
        try {
          let { value } = 42;
          "Should not reach here";
        } catch (e) {
          "Error caught";
        };
      `)).toBe('Error caught')
    })

    test('destructuring shadowing', () => {
      expect(lits.run(`
        let name = "outer";
        let result = {
          let { name } = { name: "inner" };
          name;
        };
        [name, result]
      `)).toEqual(['outer', 'inner'])
    })

    test('empty destructuring pattern', () => {
      expect(lits.run(`
        let {} = { a: 1 };
        "No error"
      `)).toBe('No error')
    })

    test('empty array destructuring', () => {
      expect(lits.run(`
        let [] = [1, 2, 3];
        "No error"
      `)).toBe('No error')
    })
  })

  // Combinations
  describe('complex combinations', () => {
    test('mix of all features', () => {
      expect(lits.run(`
        let { 
          name as userName = "Guest",
          profile { 
            age = 0,
            contact { email as userEmail = "none" }
          },
          settings = { theme: "light" },
          scores as userScores = [],
          ...others
        } = { name: "Sam", profile: { contact: {} }};
        
        [userName, age, userEmail, settings.theme, userScores, others]
      `)).toEqual(['Sam', 0, 'none', 'light', [], {}])
    })

    test('array and object combined', () => {
      expect(lits.run(`
        let [{ name }, { age }] = [{ name: "Tina" }, { age: 33 }];
        name ++ " is " ++ str(age)
      `)).toBe('Tina is 33')
    })

    test('object with array property', () => {
      expect(lits.run(`
        let { name, scores [one, two] } = { name: "Uma", scores: [85, 92] };
        name ++ ": " ++ str(one + two)
      `)).toBe('Uma: 177')
    })
  })
})
