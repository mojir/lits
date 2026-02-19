import { describe, expect, test } from 'vitest'
import { Lits } from '../src/Lits/Lits'

const lits = new Lits()

describe('lits Lexical Scoping', () => {
  // Basic lexical scoping
  describe('basic lexical scoping', () => {
    test('function accesses variable from parent scope', () => {
      expect(lits.run(`
        let bar = do
          let x = 10;
          let foo = (a) -> do a * x end;
          foo;
        end;
        
        bar(1)
      `)).toBe(10)
    })

    test('outer scope variables are accessible in nested functions', () => {
      expect(lits.run(`
        let outer = 5;
        
        let makeMultiplier = () -> do
          let multiply = (n) -> do n * outer end;
          multiply;
        end;
        
        let multiplier = makeMultiplier();
        multiplier(10)
      `)).toBe(50)
    })
  })

  // Variable visibility and shadowing
  describe('variable visibility and shadowing', () => {
    test('inner scope variables don\'t leak to outer scope', () => {
      expect(lits.run(`
        let result = do
          let outer = 10;
          do
            let inner = 20;
            outer;  // Should return outer value, not inner
          end;
        end;
        result
      `)).toBe(10)
    })

    test('variable shadowing works correctly', () => {
      expect(lits.run(`
        let x = 5;
        let result = do
          let x = 10;
          x;  // Should return the shadowed value
        end;
        result
      `)).toBe(10)
    })

    test('original variable remains unchanged after shadowing', () => {
      expect(lits.run(`
        let x = 5;
        do
          let x = 10;
          // Shadow x in inner scope
        end;
        x  // Should still be 5
      `)).toBe(5)
    })
  })

  // Closure behavior
  describe('closure behavior', () => {
    test('closures capture the lexical environment', () => {
      expect(lits.run(`
        let makeCounter = () -> do
          let counter = 0;
          let increment = () -> do
            let counter = counter + 1;
            counter;
          end;
          increment;
        end;
        
        let counter = makeCounter();
        counter()  // Should be 1
      `)).toBe(1)
    })

    test('multiple closure instances maintain separate state', () => {
      expect(lits.run(`
        let makeCounter = () -> do
          let counter = 0;
          let increment = () -> do
            let counter = counter + 1;
            counter;
          end;
          increment;
        end;
        
        let counter1 = makeCounter();
        let counter2 = makeCounter();
        
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
        let x = 10;
        let addX = y -> x + y;
        
        let result = do
          let x = 20;  // Shadow x
          addX(5);      // Should use x=10 from closure, not x=20
        end;
        
        result
      `)).toBe(15)
    })

    test('nested lambdas with multiple variable captures', () => {
      expect(lits.run(`
        let x = 1;
        let y = 2;
        
        let nestedFunc = do
          let z = 3;
          x -> y -> z -> x + y + z;
        end;
        
        nestedFunc(10)(20)(30)
      `)).toBe(60)
    })

    test('constract is no longer needed for nested lambdas', () => {
      expect(lits.run(`
        let x = 10;
        let nested = x -> y -> x + y;  // With lexical scope, x is captured
        
        nested(5)(3)
      `)).toBe(8)
    })
  })

  // Recursive functions
  describe('recursive functions', () => {
    test('recursive functions work with lexical scope', () => {
      expect(lits.run(`
        let factorial = (n) -> do
          if n <= 1 then
            1
          else
            n * self(n - 1)
          end
        end;
        
        factorial(5)
      `)).toBe(120)
    })
  })

  // Function parameters
  describe('function parameters', () => {
    test('function parameters shadow outer variables', () => {
      expect(lits.run(`
        let x = 10;
        
        let test = (x) -> do
          x;  // Should use parameter x, not outer x
        end;
        
        test(20)
      `)).toBe(20)
    })

    test('modified variables in upper scope are visible in inner scope', () => {
      expect(lits.run(`
        let result = do
          let x = 1;
          let inner = do
            let x = x + 1;  // Should see x=1 and create a new x=2
            x;
          end;
          inner;
        end;
        
        result
      `)).toBe(2)
    })
  })

  // Control structures
  describe('control structures', () => {
    test('lexical scoping in if statements', () => {
      expect(lits.run(`
        let x = 10;
        
        if true then
          let x = 20;
          x;  // Should return inner x
        else
          x;  // Should return outer x
        end
      `)).toBe(20)
    })

    test('lexical scoping in for comprehensions', () => {
      expect(lits.run(`
        let x = "outer";
        
        let result = for (x in ["inner1", "inner2"]) -> do
          x;  // Should use loop variable x, not outer x
        end;
        
        first(result)
      `)).toBe('inner1')
    })
  })
})
