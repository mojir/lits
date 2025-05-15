import { describe, expect, test } from 'vitest'
import { Lits } from '../src/Lits/Lits'

const lits = new Lits()

describe('lits Lexical Scoping', () => {
  // Basic lexical scoping
  describe('basic lexical scoping', () => {
    test('function accesses variable from parent scope', () => {
      expect(lits.run(`
        let bar := {
          let x := 10;
          function foo(a) { a * x };
          foo;
        };
        
        bar(1)
      `)).toBe(10)
    })

    test('outer scope variables are accessible in nested functions', () => {
      expect(lits.run(`
        let outer := 5;
        
        function makeMultiplier() {
          function multiply(n) { n * outer };
          multiply;
        };
        
        let multiplier := makeMultiplier();
        multiplier(10)
      `)).toBe(50)
    })
  })

  // Variable visibility and shadowing
  describe('variable visibility and shadowing', () => {
    test('inner scope variables don\'t leak to outer scope', () => {
      expect(lits.run(`
        let result := {
          let outer := 10;
          {
            let inner := 20;
            outer;  // Should return outer value, not inner
          };
        };
        result
      `)).toBe(10)
    })

    test('variable shadowing works correctly', () => {
      expect(lits.run(`
        let x := 5;
        let result := {
          let x := 10;
          x;  // Should return the shadowed value
        };
        result
      `)).toBe(10)
    })

    test('original variable remains unchanged after shadowing', () => {
      expect(lits.run(`
        let x := 5;
        {
          let x := 10;
          // Shadow x in inner scope
        };
        x  // Should still be 5
      `)).toBe(5)
    })
  })

  // Closure behavior
  describe('closure behavior', () => {
    test('closures capture the lexical environment', () => {
      expect(lits.run(`
        function makeCounter() {
          let counter := 0;
          function increment() {
            let counter := counter + 1;
            counter;
          };
          increment;
        };
        
        let counter := makeCounter();
        counter()  // Should be 1
      `)).toBe(1)
    })

    test('multiple closure instances maintain separate state', () => {
      expect(lits.run(`
        function makeCounter() {
          let counter := 0;
          function increment() {
            let counter := counter + 1;
            counter;
          };
          increment;
        };
        
        let counter1 := makeCounter();
        let counter2 := makeCounter();
        
        counter1();
        counter1();
        counter2();
      `)).toBe(1)
    })
  })

  // Lambda functions
  describe('lambda functions', () => {
    test('lambda functions capture lexical scope', () => {
      expect(lits.run(`
        let x := 10;
        let addX := y -> x + y;
        
        let result := {
          let x := 20;  // Shadow x
          addX(5);      // Should use x=10 from closure, not x=20
        };
        
        result
      `)).toBe(15)
    })

    test('nested lambdas with multiple variable captures', () => {
      expect(lits.run(`
        let x := 1;
        let y := 2;
        
        let nestedFunc := {
          let z := 3;
          x -> y -> z -> x + y + z;
        };
        
        nestedFunc(10)(20)(30)
      `)).toBe(60)
    })

    test('constract is no longer needed for nested lambdas', () => {
      expect(lits.run(`
        let x := 10;
        let nested := x -> y -> x + y;  // With lexical scope, x is captured
        
        nested(5)(3)
      `)).toBe(8)
    })
  })

  // Recursive functions
  describe('recursive functions', () => {
    test('recursive functions work with lexical scope', () => {
      expect(lits.run(`
        function factorial(n) {
          if (n <= 1) {
            1
          } else {
            n * factorial(n - 1)
          }
        };
        
        factorial(5)
      `)).toBe(120)
    })
  })

  // Function parameters
  describe('function parameters', () => {
    test('function parameters shadow outer variables', () => {
      expect(lits.run(`
        let x := 10;
        
        function test(x) {
          x;  // Should use parameter x, not outer x
        };
        
        test(20)
      `)).toBe(20)
    })

    test('modified variables in upper scope are visible in inner scope', () => {
      expect(lits.run(`
        let result := {
          let x := 1;
          let inner := {
            let x := x + 1;  // Should see x=1 and create a new x=2
            x;
          };
          inner;
        };
        
        result
      `)).toBe(2)
    })
  })

  // Control structures
  describe('control structures', () => {
    test('lexical scoping in if statements', () => {
      expect(lits.run(`
        let x := 10;
        
        if (true) {
          let x := 20;
          x;  // Should return inner x
        } else {
          x;  // Should return outer x
        }
      `)).toBe(20)
    })

    test('lexical scoping in for comprehensions', () => {
      expect(lits.run(`
        let x := "outer";
        
        let result := for (x in ["inner1", "inner2"]) {
          x;  // Should use loop variable x, not outer x
        };
        
        first(result)
      `)).toBe('inner1')
    })
  })
})
