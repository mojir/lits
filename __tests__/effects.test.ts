import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { resume, run, runSync } from '../src/effects'
import type { Handlers } from '../src/evaluator/effectTypes'
import { mathUtilsModule } from '../src/builtin/modules/math'

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

describe('phase 3 — Host Async API', () => {
  describe('3a: runSync standalone function', () => {
    it('should evaluate a simple expression', () => {
      const result = runSync('[1, 2, 3] |> map(_, -> $ * $)')
      expect(result).toEqual([1, 4, 9])
    })

    it('should accept bindings with JS functions', () => {
      const result = runSync('double(21)', {
        bindings: { double: (x: number) => x * 2 },
      })
      expect(result).toBe(42)
    })

    it('should accept plain value bindings', () => {
      const result = runSync('x + y', {
        bindings: { x: 10, y: 32 },
      })
      expect(result).toBe(42)
    })

    it('should support modules', () => {
      const result = runSync('let m = import(math); m.ln(1)', {
        modules: [mathUtilsModule],
      })
      expect(result).toBe(0)
    })
  })

  describe('3a: run standalone function', () => {
    it('should return completed result for simple expression', async () => {
      const result = await run('[1, 2, 3] |> reduce(_, +, 0)')
      expect(result).toEqual({ type: 'completed', value: 6 })
    })

    it('should accept plain value bindings', async () => {
      const result = await run('x + y', {
        bindings: { x: 10, y: 32 },
      })
      expect(result).toEqual({ type: 'completed', value: 42 })
    })

    it('should support modules', async () => {
      const result = await run('let m = import(math); m.ln(1)', {
        modules: [mathUtilsModule],
      })
      expect(result).toEqual({ type: 'completed', value: 0 })
    })

    it('should return error result for runtime errors', async () => {
      const result = await run('throw("boom")')
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('boom')
      }
    })

    it('should return error result for syntax errors', async () => {
      const result = await run('(((')
      expect(result.type).toBe('error')
    })
  })

  describe('3b: host handler — sync resume', () => {
    it('should resume with a synchronous value', async () => {
      const result = await run(`
        perform(effect(my.double), 21)
      `, {
        handlers: {
          'my.double': async ({ args, resume: doResume }) => {
            doResume((args[0] as number) * 2)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 42 })
    })

    it('should resume with a computed value', async () => {
      const result = await run(`
        let msg = perform(effect(my.greet), "world");
        msg
      `, {
        handlers: {
          'my.greet': async ({ args, resume: doResume }) => {
            doResume(`Hello, ${args[0]}!`)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'Hello, world!' })
    })

    it('should handle multiple host effects sequentially', async () => {
      const result = await run(`
        let a = perform(effect(my.add), 10, 20);
        let b = perform(effect(my.add), a, 12);
        b
      `, {
        handlers: {
          'my.add': async ({ args, resume: doResume }) => {
            doResume((args[0] as number) + (args[1] as number))
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 42 })
    })

    it('should resume with no-arg effect', async () => {
      const result = await run(`
        perform(effect(my.now))
      `, {
        handlers: {
          'my.now': async ({ resume: doResume }) => {
            doResume(1234567890)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 1234567890 })
    })
  })

  describe('3b: host handler — async resume', () => {
    it('should resume with an async value (promise)', async () => {
      const result = await run(`
        perform(effect(my.fetch), "data")
      `, {
        handlers: {
          'my.fetch': async ({ args, resume: doResume }) => {
            const value = await Promise.resolve(`fetched: ${args[0]}`)
            doResume(value)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'fetched: data' })
    })

    it('should resume with a promise value directly', async () => {
      const result = await run(`
        perform(effect(my.delayed), 42)
      `, {
        handlers: {
          'my.delayed': async ({ args, resume: doResume }) => {
            doResume(Promise.resolve(args[0]!))
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 42 })
    })

    it('should handle async errors from promise resume', async () => {
      const result = await run(`
        try
          perform(effect(my.fail), "oops")
        catch (e)
          "caught: " ++ e.message
        end
      `, {
        handlers: {
          'my.fail': async ({ resume: doResume }) => {
            doResume(Promise.reject(new Error('async failure')))
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toContain('caught: async failure')
      }
    })
  })

  describe('3b: host handler — unhandled effect', () => {
    it('should return error for unhandled effect with no handlers', async () => {
      const result = await run('perform(effect(no.handler), "data")')
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('Unhandled effect: \'no.handler\'')
      }
    })

    it('should return error for unhandled effect with non-matching handlers', async () => {
      const result = await run('perform(effect(missing.handler), "x")', {
        handlers: {
          'other.handler': async ({ resume: doResume }) => { doResume(null) },
        },
      })
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('Unhandled effect: \'missing.handler\'')
      }
    })
  })

  describe('3b: host handler — error handling', () => {
    it('should catch host handler errors in Lits try/catch', async () => {
      const result = await run(`
        try
          perform(effect(my.fail))
        catch (e)
          "caught: " ++ e.message
        end
      `, {
        handlers: {
          'my.fail': async () => {
            throw new Error('handler boom')
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toContain('caught: handler boom')
      }
    })

    it('should propagate handler errors as RunResult.error when no try/catch', async () => {
      const result = await run(`
        perform(effect(my.fail))
      `, {
        handlers: {
          'my.fail': async () => {
            throw new Error('handler error')
          },
        },
      })
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('handler error')
      }
    })
  })

  describe('3b: local handlers take precedence over host handlers', () => {
    it('should use local try/with handler instead of host handler', async () => {
      const result = await run(`
        try
          perform(effect(my.eff), "test")
        with
          case effect(my.eff) then ([x]) -> "local: " ++ x
        end
      `, {
        handlers: {
          'my.eff': async ({ args, resume: doResume }) => {
            doResume(`host: ${args[0]}`)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'local: test' })
    })

    it('should delegate to host handler when local handler does not match', async () => {
      const result = await run(`
        try
          perform(effect(other.eff), "test")
        with
          case effect(my.eff) then ([x]) -> "local: " ++ x
        end
      `, {
        handlers: {
          'other.eff': async ({ args, resume: doResume }) => {
            doResume(`host: ${args[0]}`)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'host: test' })
    })

    it('should delegate from local handler to host handler via perform', async () => {
      const result = await run(`
        try
          perform(effect(my.eff), "msg")
        with
          case effect(my.eff) then ([x]) -> perform(effect(my.eff), x ++ "+enriched")
        end
      `, {
        handlers: {
          'my.eff': async ({ args, resume: doResume }) => {
            doResume(`host(${args[0]})`)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'host(msg+enriched)' })
    })
  })

  describe('3b: host handler — suspend', () => {
    it('should return suspended result when handler calls suspend', async () => {
      const result = await run(`
        let x = perform(effect(my.wait), "please approve");
        "approved: " ++ x
      `, {
        handlers: {
          'my.wait': async ({ args, suspend }) => {
            suspend({ payload: args[0] })
          },
        },
      })
      expect(result.type).toBe('suspended')
      if (result.type === 'suspended') {
        expect(result.meta).toEqual({ payload: 'please approve' })
        expect(result.blob).toBeDefined()
        expect(typeof result.blob).toBe('string')
        expect(result.blob.length).toBeGreaterThan(0)
      }
    })

    it('should return suspended result with no meta', async () => {
      const result = await run(`
        perform(effect(my.pause))
      `, {
        handlers: {
          'my.pause': async ({ suspend }) => {
            suspend()
          },
        },
      })
      expect(result.type).toBe('suspended')
      if (result.type === 'suspended') {
        expect(result.meta).toBeUndefined()
      }
    })
  })

  describe('3c: AbortSignal', () => {
    it('should provide an abort signal to the handler', async () => {
      let receivedSignal: AbortSignal | undefined
      const result = await run(`
        perform(effect(my.check))
      `, {
        handlers: {
          'my.check': async ({ signal, resume: doResume }) => {
            receivedSignal = signal
            doResume(true)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: true })
      expect(receivedSignal).toBeDefined()
      expect(receivedSignal!.aborted).toBe(false)
    })
  })

  describe('3d: end-to-end integration', () => {
    it('should run a multi-step effect workflow', async () => {
      const log: string[] = []
      const result = await run(`
        let llm = effect(llm.complete);
        let summary = perform(llm, "Summarize this doc");
        let critique = perform(llm, "Critique: " ++ summary);
        { summary: summary, critique: critique }
      `, {
        handlers: {
          'llm.complete': async ({ args, resume: doResume }) => {
            log.push(args[0] as string)
            doResume(`[result for: ${args[0]}]`)
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        const value = result.value as Record<string, string>
        expect(value.summary).toBe('[result for: Summarize this doc]')
        expect(value.critique).toBe('[result for: Critique: [result for: Summarize this doc]]')
      }
      expect(log).toEqual([
        'Summarize this doc',
        'Critique: [result for: Summarize this doc]',
      ])
    })

    it('should combine local and host handlers in one program', async () => {
      const result = await run(`
        let llm = effect(llm.complete);
        let log-eff = effect(my.log);

        try
          let msg = perform(llm, "prompt");
          perform(log-eff, msg)
        with
          case log-eff then ([msg]) -> "logged: " ++ msg
        end
      `, {
        handlers: {
          'llm.complete': async ({ args, resume: doResume }) => {
            doResume(`LLM says: ${args[0]}`)
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 'logged: LLM says: prompt' })
    })

    it('should handle try/catch around host effect', async () => {
      const result = await run(`
        try
          perform(effect(my.risky))
        catch (e)
          "recovered: " ++ e.message
        end
      `, {
        handlers: {
          'my.risky': async () => {
            throw new Error('infrastructure failure')
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toContain('recovered: infrastructure failure')
      }
    })

    it('should work with bindings and handlers together', async () => {
      const result = await run(`
        let result = perform(effect(my.compute), x, y);
        result
      `, {
        bindings: { x: 10, y: 32 },
        handlers: {
          'my.compute': async ({ args, resume: doResume }) => {
            doResume((args[0] as number) + (args[1] as number))
          },
        },
      })
      expect(result).toEqual({ type: 'completed', value: 42 })
    })

    it('should handle the LLM example from the API contract', async () => {
      const result = await run(`
        let llm     = effect(llm.complete);
        let approve = effect(com.myco.human.approve);

        let report   = perform(llm, "Generate Q4 report");
        let decision = perform(approve, report);

        if decision.approved then
          perform(llm, "Finalize: " ++ report)
        else
          "Rejected: " ++ decision.reason
        end
      `, {
        handlers: {
          'llm.complete': async ({ args, resume: doResume }) => {
            doResume(`[LLM: ${args[0]}]`)
          },
          'com.myco.human.approve': async ({ resume: doResume }) => {
            doResume({ approved: true, reason: null })
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toBe('[LLM: Finalize: [LLM: Generate Q4 report]]')
      }
    })

    it('should handle the approval rejection case', async () => {
      const result = await run(`
        let llm     = effect(llm.complete);
        let approve = effect(com.myco.human.approve);

        let report   = perform(llm, "Generate Q4 report");
        let decision = perform(approve, report);

        if decision.approved then
          perform(llm, "Finalize: " ++ report)
        else
          "Rejected: " ++ decision.reason
        end
      `, {
        handlers: {
          'llm.complete': async ({ args, resume: doResume }) => {
            doResume(`[LLM: ${args[0]}]`)
          },
          'com.myco.human.approve': async ({ resume: doResume }) => {
            doResume({ approved: false, reason: 'Budget exceeded' })
          },
        },
      })
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toBe('Rejected: Budget exceeded')
      }
    })
  })
})

describe('phase 4 — Suspension & Resume', () => {
  describe('4a: serialization format', () => {
    it('should produce a valid JSON blob on suspend', async () => {
      const result = await run(`
        perform(effect(my.wait), "data")
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend({ info: 'test' }) },
        },
      })
      expect(result.type).toBe('suspended')
      if (result.type === 'suspended') {
        const parsed = JSON.parse(result.blob) as { version: number, k: unknown[], contextStacks: unknown[] }
        expect(parsed.version).toBe(1)
        expect(parsed.k).toBeDefined()
        expect(parsed.contextStacks).toBeDefined()
        expect(Array.isArray(parsed.contextStacks)).toBe(true)
      }
    })

    it('should include meta in the result', async () => {
      const result = await run(`
        perform(effect(my.wait))
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend({ assignedTo: 'team-a' }) },
        },
      })
      expect(result.type).toBe('suspended')
      if (result.type === 'suspended') {
        expect(result.meta).toEqual({ assignedTo: 'team-a' })
      }
    })

    it('should handle suspend with no meta', async () => {
      const result = await run(`
        perform(effect(my.wait))
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(result.type).toBe('suspended')
      if (result.type === 'suspended') {
        expect(result.meta).toBeUndefined()
      }
    })
  })

  describe('4b: resume() API', () => {
    it('should resume a simple suspended program', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        x * 2
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 21)
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should resume with a string value', async () => {
      const r1 = await run(`
        let name = perform(effect(my.ask), "What is your name?");
        "Hello, " ++ name ++ "!"
      `, {
        handlers: {
          'my.ask': async ({ suspend, args }) => { suspend({ prompt: args[0] }) },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 'Alice')
      expect(r2).toEqual({ type: 'completed', value: 'Hello, Alice!' })
    })

    it('should resume with an object value', async () => {
      const r1 = await run(`
        let decision = perform(effect(my.approve), "report");
        if decision.approved then
          "Approved!"
        else
          "Rejected: " ++ decision.reason
        end
      `, {
        handlers: {
          'my.approve': async ({ suspend, args }) => { suspend({ doc: args[0] }) },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, { approved: false, reason: 'Budget exceeded' })
      expect(r2).toEqual({ type: 'completed', value: 'Rejected: Budget exceeded' })
    })

    it('should resume with null value', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        null?(x)
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, null)
      expect(r2).toEqual({ type: 'completed', value: true })
    })

    it('should preserve variables defined before suspend', async () => {
      const r1 = await run(`
        let a = 10;
        let b = 20;
        let c = perform(effect(my.wait));
        a + b + c
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 12)
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should preserve closures across suspend/resume', async () => {
      const r1 = await run(`
        let multiplier = 3;
        let scale = (x) -> x * multiplier;
        let value = perform(effect(my.wait));
        scale(value)
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 14)
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should handle multiple suspensions (re-suspend on resume)', async () => {
      const handlers: Handlers = {
        'my.step': async ({ args, suspend }) => {
          suspend({ step: args[0] })
        },
      }
      const r1 = await run(`
        let a = perform(effect(my.step), 1);
        let b = perform(effect(my.step), 2);
        a + b
      `, { handlers })

      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return
      expect(r1.meta).toEqual({ step: 1 })

      // Resume first suspension
      const r2 = await resume(r1.blob, 10, { handlers })
      expect(r2.type).toBe('suspended')
      if (r2.type !== 'suspended')
        return
      expect(r2.meta).toEqual({ step: 2 })

      // Resume second suspension
      const r3 = await resume(r2.blob, 32)
      expect(r3).toEqual({ type: 'completed', value: 42 })
    })

    it('should support handlers on resume', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        perform(effect(my.compute), x)
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
          'my.compute': async ({ args, resume: r }) => { r((args[0] as number) * 2) },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      // Resume with handlers so my.compute works
      const r2 = await resume(r1.blob, 21, {
        handlers: {
          'my.compute': async ({ args, resume: r }) => { r((args[0] as number) * 2) },
        },
      })
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should support bindings on resume', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        x + offset
      `, {
        bindings: { offset: 32 },
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      // Resume with bindings
      const r2 = await resume(r1.blob, 10, {
        bindings: { offset: 32 },
      })
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should return error for invalid blob JSON', async () => {
      const result = await resume('not-json', 42)
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('Invalid suspension blob')
      }
    })

    it('should return error for wrong version', async () => {
      const result = await resume(JSON.stringify({ version: 999, k: [], contextStacks: [] }), 42)
      expect(result.type).toBe('error')
      if (result.type === 'error') {
        expect(result.error.message).toContain('Unsupported suspension blob version')
      }
    })

    it('should handle errors after resume', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        throw("error: " ++ x)
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 'boom')
      expect(r2.type).toBe('error')
      if (r2.type === 'error') {
        expect(r2.error.message).toContain('error: boom')
      }
    })

    it('should handle try/catch after resume', async () => {
      const r1 = await run(`
        try
          let x = perform(effect(my.wait));
          if x == "bad" then throw("bad input") else x end
        catch (e)
          "caught: " ++ e.message
        end
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 'bad')
      expect(r2).toEqual({ type: 'completed', value: 'caught: bad input' })
    })
  })

  describe('4c: NativeJsFunction not in blob', () => {
    it('should use host values before suspend without them leaking into blob', async () => {
      // Host values (plain data in bindings) are available during evaluation.
      // After suspend, the blob should not contain NativeJsFunctions.
      const r1 = await run(`
        let doubled = factor * 5;
        let x = perform(effect(my.wait));
        doubled + x
      `, {
        bindings: { factor: 2 },
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      // Resume — the blob is valid and doesn't contain JS functions
      const r2 = await resume(r1.blob, 7)
      expect(r2).toEqual({ type: 'completed', value: 17 }) // 2*5 + 7
    })
  })

  describe('4d: end-to-end suspension workflow', () => {
    it('should complete a full suspend-store-resume cycle', async () => {
      // Simulate: Process 1 runs source, suspends at approval
      const source = `
        let report = perform(effect(llm.complete), "Generate Q4 report");
        let decision = perform(effect(com.myco.approve), report);
        if decision.approved then
          perform(effect(llm.complete), "Finalize: " ++ report)
        else
          "Rejected: " ++ decision.reason
        end
      `
      const handlers: Handlers = {
        'llm.complete': async ({ args, resume: doResume }) => {
          doResume(`[LLM: ${args[0]}]`)
        },
        'com.myco.approve': async ({ args, suspend }) => {
          suspend({ payload: args[0], assignedTo: 'finance-team' })
        },
      }

      const r1 = await run(source, { handlers })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      // Simulate: Store blob in database
      const storedBlob = r1.blob
      const storedMeta = r1.meta as Record<string, unknown>
      expect(storedMeta.assignedTo).toBe('finance-team')
      expect(storedMeta.payload).toBe('[LLM: Generate Q4 report]')

      // Simulate: Process 2 loads blob and resumes with approval
      const r2 = await resume(storedBlob, { approved: true, reason: null }, { handlers })
      expect(r2.type).toBe('completed')
      if (r2.type === 'completed') {
        expect(r2.value).toBe('[LLM: Finalize: [LLM: Generate Q4 report]]')
      }
    })

    it('should handle rejection in suspend-resume cycle', async () => {
      const source = `
        let x = perform(effect(my.wait));
        if x.approved then "Yes" else "No: " ++ x.reason end
      `
      const r1 = await run(source, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend({ type: 'approval' }) },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, { approved: false, reason: 'denied' })
      expect(r2).toEqual({ type: 'completed', value: 'No: denied' })
    })

    it('should handle multi-step workflow with several suspensions', async () => {
      const source = `
        let step1 = perform(effect(my.step), "step1");
        let step2 = perform(effect(my.step), "step2");
        let step3 = perform(effect(my.step), "step3");
        [step1, step2, step3]
      `
      const handlers: Handlers = {
        'my.step': async ({ args, suspend }) => {
          suspend({ step: args[0] })
        },
      }

      const r1 = await run(source, { handlers })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return
      expect((r1.meta as Record<string, unknown>).step).toBe('step1')

      const r2 = await resume(r1.blob, 'A', { handlers })
      expect(r2.type).toBe('suspended')
      if (r2.type !== 'suspended')
        return
      expect((r2.meta as Record<string, unknown>).step).toBe('step2')

      const r3 = await resume(r2.blob, 'B', { handlers })
      expect(r3.type).toBe('suspended')
      if (r3.type !== 'suspended')
        return
      expect((r3.meta as Record<string, unknown>).step).toBe('step3')

      const r4 = await resume(r3.blob, 'C')
      expect(r4).toEqual({ type: 'completed', value: ['A', 'B', 'C'] })
    })

    it('should work with local try/with handlers after resume', async () => {
      const r1 = await run(`
        let x = perform(effect(my.wait));
        try
          perform(effect(my.local), x)
        with
          case effect(my.local) then ([v]) -> upper-case(v)
        end
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 'hello')
      expect(r2).toEqual({ type: 'completed', value: 'HELLO' })
    })

    it('should preserve deep nesting and closures across resume', async () => {
      const r1 = await run(`
        let make-adder = (n) -> (x) -> n + x;
        let add5 = make-adder(5);
        let input = perform(effect(my.wait));
        add5(input)
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 37)
      expect(r2).toEqual({ type: 'completed', value: 42 })
    })

    it('should handle loop/recur state after resume', async () => {
      const r1 = await run(`
        let factor = perform(effect(my.wait));
        loop(i = 0, acc = 0) ->
          if i >= 5 then acc
          else recur(i + 1, acc + i * factor)
          end
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      // factor = 2, sum = 0*2 + 1*2 + 2*2 + 3*2 + 4*2 = 20
      const r2 = await resume(r1.blob, 2)
      expect(r2).toEqual({ type: 'completed', value: 20 })
    })

    it('should handle arrays and objects across resume', async () => {
      const r1 = await run(`
        let data = { name: "test", values: [1, 2, 3] };
        let extra = perform(effect(my.wait));
        { name: data.name, values: push(data.values, extra), count: count(data.values) + 1 }
      `, {
        handlers: {
          'my.wait': async ({ suspend }) => { suspend() },
        },
      })
      expect(r1.type).toBe('suspended')
      if (r1.type !== 'suspended')
        return

      const r2 = await resume(r1.blob, 4)
      expect(r2).toEqual({
        type: 'completed',
        value: { name: 'test', values: [1, 2, 3, 4], count: 4 },
      })
    })
  })
})
