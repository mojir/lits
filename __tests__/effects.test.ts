import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'

const lits = new Lits()

describe('phase 2 — Local Effect Handling', () => {
  describe('2a: effect(name) special expression', () => {
    it('should return an effect reference', () => {
      const result = lits.run('effect(lits.log)')
      expect(result).toHaveProperty('name', 'lits.log')
    })

    it('should support dotted names', () => {
      const result = lits.run('effect(llm.complete)')
      expect(result).toHaveProperty('name', 'llm.complete')
    })

    it('should support deeply dotted names', () => {
      const result = lits.run('effect(com.myco.human.approve)')
      expect(result).toHaveProperty('name', 'com.myco.human.approve')
    })

    it('should support single-part names', () => {
      const result = lits.run('effect(simple)')
      expect(result).toHaveProperty('name', 'simple')
    })

    it('should return the same reference for the same name', () => {
      const result = lits.run('==(effect(llm.complete), effect(llm.complete))')
      expect(result).toBe(true)
    })

    it('should return different references for different names', () => {
      const result = lits.run('==(effect(llm.complete), effect(llm.summarize))')
      expect(result).toBe(false)
    })

    it('should be a first-class value (stored in variables)', () => {
      const result = lits.run(`
        let eff = effect(llm.complete);
        eff
      `)
      expect(result).toHaveProperty('name', 'llm.complete')
    })
  })

  describe('2b: perform(eff, ...args) special expression', () => {
    it('should perform an effect with a local handler', () => {
      const result = lits.run(`
        try
          perform(effect(my.effect), "hello")
        with
          case effect(my.effect) then ([msg]) -> upper-case(msg)
        end
      `)
      expect(result).toBe('HELLO')
    })

    it('should perform an effect with no arguments', () => {
      const result = lits.run(`
        try
          perform(effect(my.value))
        with
          case effect(my.value) then ([]) -> 42
        end
      `)
      expect(result).toBe(42)
    })

    it('should perform an effect with multiple arguments', () => {
      const result = lits.run(`
        try
          perform(effect(my.add), 10, 20)
        with
          case effect(my.add) then ([a, b]) -> a + b
        end
      `)
      expect(result).toBe(30)
    })

    it('should pass arguments as an array to the handler', () => {
      const result = lits.run(`
        try
          perform(effect(my.count), "a", "b", "c")
        with
          case effect(my.count) then (args) -> count(args)
        end
      `)
      expect(result).toBe(3)
    })

    it('should throw on unhandled effect', () => {
      expect(() => lits.run('perform(effect(unhandled.effect), "arg")')).toThrow('Unhandled effect')
    })

    it('should use effect references from variables', () => {
      const result = lits.run(`
        let eff = effect(my.effect);
        try
          perform(eff, "world")
        with
          case eff then ([msg]) -> "hello " ++ msg
        end
      `)
      expect(result).toBe('hello world')
    })
  })

  describe('2c: TryWithFrame handler dispatch', () => {
    it('should match handlers by effect name', () => {
      const result = lits.run(`
        try
          perform(effect(a), 1) + perform(effect(b), 2)
        with
          case effect(a) then ([x]) -> x * 10
          case effect(b) then ([x]) -> x * 100
        end
      `)
      expect(result).toBe(210) // 10 + 200
    })

    it('should use the first matching handler', () => {
      const result = lits.run(`
        let eff = effect(my.eff);
        try
          perform(eff, "test")
        with
          case eff then ([x]) -> "first: " ++ x
          case eff then ([x]) -> "second: " ++ x
        end
      `)
      expect(result).toBe('first: test')
    })

    it('should delegate to outer try/with when no local match', () => {
      const result = lits.run(`
        try
          try
            perform(effect(outer.eff), "value")
          with
            case effect(inner.eff) then ([x]) -> "inner: " ++ x
          end
        with
          case effect(outer.eff) then ([x]) -> "outer: " ++ x
        end
      `)
      expect(result).toBe('outer: value')
    })

    it('should nest try/with blocks correctly', () => {
      const result = lits.run(`
        try
          let a = try
            perform(effect(inner), "a")
          with
            case effect(inner) then ([x]) -> "inner(" ++ x ++ ")"
          end;
          a ++ " + " ++ perform(effect(outer), "b")
        with
          case effect(outer) then ([x]) -> "outer(" ++ x ++ ")"
        end
      `)
      expect(result).toBe('inner(a) + outer(b)')
    })

    it('should remove TryWithFrame after match — handlers run outside scope', () => {
      // If the handler calls perform with the same effect, it should NOT match
      // the same try/with (the frame was removed). It should either match an
      // outer handler or fail as unhandled.
      const result = lits.run(`
        try
          try
            perform(effect(my.eff), "original")
          with
            case effect(my.eff) then ([x]) -> perform(effect(my.eff), x ++ "+delegated")
          end
        with
          case effect(my.eff) then ([x]) -> "caught: " ++ x
        end
      `)
      expect(result).toBe('caught: original+delegated')
    })

    it('should allow handler return value to be the resume value', () => {
      const result = lits.run(`
        try
          let x = perform(effect(my.eff), 5);
          x * 2
        with
          case effect(my.eff) then ([n]) -> n + 10
        end
      `)
      expect(result).toBe(30) // (5 + 10) * 2
    })

    it('should allow effects inside handler body (delegating to outer)', () => {
      const result = lits.run(`
        try
          try
            perform(effect(my.eff), "msg")
          with
            case effect(my.eff) then ([x]) -> perform(effect(lits.log), x)
          end
        with
          case effect(lits.log) then ([x]) -> "logged: " ++ x
        end
      `)
      expect(result).toBe('logged: msg')
    })

    it('should skip TryWithFrame on success (no effect performed)', () => {
      const result = lits.run(`
        try
          42
        with
          case effect(my.eff) then ([x]) -> x * 100
        end
      `)
      expect(result).toBe(42)
    })
  })

  describe('2d: TryCatch + TryWith interaction', () => {
    it('should handle errors with catch, not with with-handlers', () => {
      const result = lits.run(`
        try
          throw("boom")
        with
          case effect(my.eff) then ([x]) -> x
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('caught: boom')
    })

    it('should handle effects with with-handlers, not with catch', () => {
      const result = lits.run(`
        try
          perform(effect(my.eff), "data")
        with
          case effect(my.eff) then ([x]) -> "handled: " ++ x
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('handled: data')
    })

    it('should let errors in handlers propagate to outer try/catch', () => {
      const result = lits.run(`
        try
          try
            perform(effect(my.eff), "data")
          with
            case effect(my.eff) then ([x]) -> throw("handler error: " ++ x)
          catch (e)
            "inner catch: " ++ e.message
          end
        catch (e)
          "outer catch: " ++ e.message
        end
      `)
      // The error from the handler should NOT be caught by the inner catch.
      // It should propagate to the outer catch.
      expect(result).toBe('outer catch: handler error: data')
    })

    it('should handle combined try/with/catch where body errors are caught', () => {
      const result = lits.run(`
        try
          throw("body error")
        with
          case effect(my.eff) then ([x]) -> x
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('caught: body error')
    })

    it('should handle combined try/with/catch where effects are handled', () => {
      const result = lits.run(`
        try
          perform(effect(my.eff), "hello")
        with
          case effect(my.eff) then ([x]) -> upper-case(x)
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('HELLO')
    })

    it('should not catch errors from try body in with-handler scope', () => {
      // Error in body goes to catch, not to with
      const result = lits.run(`
        try
          do
            throw("body boom");
            perform(effect(my.eff), "should not reach")
          end
        with
          case effect(my.eff) then ([x]) -> x
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('caught: body boom')
    })

    it('should handle unhandled effect error in outer catch', () => {
      const result = lits.run(`
        try
          perform(effect(no.handler), "data")
        catch (e)
          "caught: " ++ e.message
        end
      `)
      expect(result).toBe('caught: Unhandled effect: \'no.handler\'')
    })
  })

  describe('2e: effects as first-class values', () => {
    it('should pass effect references as function arguments', () => {
      const result = lits.run(`
        let handle-it = (eff, value) ->
          try
            perform(eff, value)
          with
            case eff then ([x]) -> x * 2
          end;
        handle-it(effect(my.eff), 21)
      `)
      expect(result).toBe(42)
    })

    it('should store effect references in data structures', () => {
      const result = lits.run(`
        let effects = [effect(a), effect(b)];
        try
          perform(effects[0], 1) + perform(effects[1], 2)
        with
          case effect(a) then ([x]) -> x * 10
          case effect(b) then ([x]) -> x * 100
        end
      `)
      expect(result).toBe(210)
    })

    it('should compare effect references correctly', () => {
      const result = lits.run(`
        let eff1 = effect(same.name);
        let eff2 = effect(same.name);
        let eff3 = effect(different.name);
        [==(eff1, eff2), ==(eff1, eff3)]
      `)
      expect(result).toEqual([true, false])
    })
  })
})
