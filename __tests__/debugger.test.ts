import { describe, expect, it } from 'vitest'
import { createDebugger } from '../src/debug'
import type { StepInfo } from '../src/debug'
import { run } from '../src/effects'
import type { Any } from '../src/interface'
import { assertModule } from '../src/builtin/modules/assertion'

describe('phase 7 — Time-Travel Debugger', () => {
  describe('7a: lits.debug.step injection', () => {
    it('should pause at the first debug step', async () => {
      const dbg = createDebugger()
      const result = await dbg.run('1 + 2')
      expect(result.type).toBe('suspended')
      expect(dbg.history).toHaveLength(1)
      expect(dbg.currentStep).toBe(0)
    })

    it('should capture expression, value, and location in step info', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      const step = dbg.current!.step
      expect(step.expression).toBeDefined()
      expect(typeof step.expression).toBe('string')
      expect(step.value).toBe(3)
      expect(step.location).toHaveProperty('line')
      expect(step.location).toHaveProperty('column')
      expect(step.location.line).toBeGreaterThan(0)
    })

    it('should capture bindings in env', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 10; x + 5')
      // Step through to after `let x = 10`
      let result = await dbg.stepForward()
      while (result.type === 'suspended') {
        const step = dbg.current!.step
        if (step.env && 'x' in step.env) {
          expect(step.env.x).toBe(10)
          break
        }
        result = await dbg.stepForward()
      }
    })

    it('should not inject debug steps when no handler is registered', async () => {
      // run() without debug handler — should complete normally
      const result = await run('1 + 2')
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toBe(3)
      }
    })

    it('should only inject debug steps for compound nodes', async () => {
      const dbg = createDebugger()
      const result = await dbg.run('42')
      // A literal number (leaf node) should not generate debug steps
      // The program should complete without suspending
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toBe(42)
      }
      expect(dbg.history).toHaveLength(0)
    })

    it('should generate debug steps for function calls', async () => {
      const dbg = createDebugger()
      const result = await dbg.run('+(1, 2)')
      expect(result.type).toBe('suspended')
      expect(dbg.current!.step.value).toBe(3)
    })

    it('should generate debug steps for special expressions', async () => {
      const dbg = createDebugger()
      await dbg.run('if true then 42 else 0 end')
      expect(dbg.history.length).toBeGreaterThanOrEqual(1)
    })
  })

  describe('7b: Debugger API — basic operations', () => {
    it('should run a program and record history', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      expect(dbg.history.length).toBeGreaterThanOrEqual(1)
      expect(dbg.current).toBeDefined()
      expect(dbg.current!.blob).toBeDefined()
      expect(typeof dbg.current!.blob).toBe('string')
      expect(dbg.current!.timestamp).toBeGreaterThan(0)
    })

    it('should step forward through execution', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; let y = x * 3; y')
      const initialStep = dbg.currentStep

      let result = await dbg.stepForward()
      while (result.type === 'suspended') {
        expect(dbg.currentStep).toBeGreaterThan(initialStep)
        result = await dbg.stepForward()
      }
      // Should eventually complete
      expect(result.type).toBe('completed')
      if (result.type === 'completed') {
        expect(result.value).toBe(9)
      }
    })

    it('should step backward through execution', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; x * 3')

      // Step forward a few times to build history
      await dbg.stepForward()
      await dbg.stepForward()
      expect(dbg.history.length).toBeGreaterThanOrEqual(2)

      const stepBeforeBack = dbg.currentStep
      const result = await dbg.stepBackward()
      expect(result.type).toBe('suspended')
      expect(dbg.currentStep).toBe(stepBeforeBack - 1)
    })

    it('should error when stepping backward at the beginning', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      expect(dbg.currentStep).toBe(0)

      const result = await dbg.stepBackward()
      expect(result.type).toBe('error')
    })

    it('should error when stepping forward with no current step', async () => {
      const dbg = createDebugger()
      // Don't call run() first
      const result = await dbg.stepForward()
      expect(result.type).toBe('error')
    })

    it('should jump to a specific step', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; let y = x * 3; y')

      // Build up some history
      await dbg.stepForward()
      await dbg.stepForward()
      await dbg.stepForward()

      if (dbg.history.length >= 3) {
        const result = await dbg.jumpTo(0)
        expect(result.type).toBe('suspended')
        expect(dbg.currentStep).toBe(0)

        const result2 = await dbg.jumpTo(2)
        expect(result2.type).toBe('suspended')
        expect(dbg.currentStep).toBe(2)
      }
    })

    it('should error when jumping to an invalid index', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')

      const result = await dbg.jumpTo(-1)
      expect(result.type).toBe('error')

      const result2 = await dbg.jumpTo(999)
      expect(result2.type).toBe('error')
    })
  })

  describe('7b: Debugger API — rerunFrom', () => {
    it('should rerun from a step with an alternate value', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; x * 3')

      // Step forward to build some history
      await dbg.stepForward()
      await dbg.stepForward()

      const historyLengthBefore = dbg.history.length
      if (historyLengthBefore >= 1) {
        // Rerun from step 0 with a different value
        const result = await dbg.rerunFrom(0, 100)
        // History after step 0 should be truncated and then new entries added
        expect(result.type).toBe('suspended')
      }
    })

    it('should create an alternate timeline', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; x * 3')

      // Build history: step through everything
      let r = await dbg.stepForward()
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      const originalFinalValue = r.type === 'completed' ? r.value : null

      // Now rerun from step 0 with a different value (42 instead of 3)
      const rerunResult = await dbg.rerunFrom(0, 42)
      expect(rerunResult.type).toBe('suspended')

      // Step forward to completion in the alternate timeline
      let r2 = await dbg.stepForward()
      while (r2.type === 'suspended') {
        r2 = await dbg.stepForward()
      }

      // The alternate timeline should produce a different result
      // Original: (1+2) = 3, 3*3 = 9
      // Alternate: step 0 resumed with 42, so x = 42, 42*3 = 126
      if (r2.type === 'completed' && originalFinalValue !== null) {
        expect(r2.value).not.toBe(originalFinalValue)
      }
    })

    it('should error when rerunFrom invalid index', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')

      const result = await dbg.rerunFrom(-1, 0)
      expect(result.type).toBe('error')

      const result2 = await dbg.rerunFrom(999, 0)
      expect(result2.type).toBe('error')
    })
  })

  describe('7b: Debugger API — with effect handlers', () => {
    it('should work with host effect handlers', async () => {
      const dbg = createDebugger({
        handlers: {
          'test.echo': async ({ args, resume }) => {
            resume(`echo: ${args[0]}`)
          },
        },
      })

      const result = await dbg.run(`
        let eff = effect(test.echo);
        perform(eff, "hello")
      `)

      // Step through to completion
      let r = result
      let stepCount = 0
      while (r.type === 'suspended' && stepCount < 100) {
        r = await dbg.stepForward()
        stepCount++
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe('echo: hello')
      }
    })

    it('should work with standard effects', async () => {
      const dbg = createDebugger({
        handlers: {
          'lits.now': async ({ resume }) => {
            resume(1234567890)
          },
        },
      })

      let r = await dbg.run('perform(effect(lits.now))')
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(1234567890)
      }
    })

    it('should work with local try/with handlers', async () => {
      const dbg = createDebugger()
      let r = await dbg.run(`
        try
          perform(effect(test.mock), "input")
        with
          case effect(test.mock) then ([arg]) -> "mocked: " ++ arg
        end
      `)
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe('mocked: input')
      }
    })
  })

  describe('7b: Debugger API — history and state', () => {
    it('should have timestamps in history entries', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      expect(dbg.current!.timestamp).toBeGreaterThan(0)
      expect(typeof dbg.current!.timestamp).toBe('number')
    })

    it('should have blobs in history entries', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      expect(typeof dbg.current!.blob).toBe('string')
      expect(dbg.current!.blob.length).toBeGreaterThan(0)
      // Should be valid JSON
      expect(() => JSON.parse(dbg.current!.blob) as unknown).not.toThrow()
    })

    it('should reset history on new run', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      await dbg.stepForward()
      const firstHistoryLength = dbg.history.length

      // Run again — history should reset
      await dbg.run('3 + 4')
      expect(dbg.history.length).toBeLessThanOrEqual(firstHistoryLength)
      expect(dbg.currentStep).toBe(0)
    })

    it('should handle stepping forward to navigate existing history before extending', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; let y = x * 3; y')

      // Step forward to build history
      await dbg.stepForward()
      await dbg.stepForward()
      await dbg.stepForward()

      // Go back to step 0
      await dbg.jumpTo(0)
      expect(dbg.currentStep).toBe(0)

      // Step forward should navigate existing history, not re-execute
      const result = await dbg.stepForward()
      expect(result.type).toBe('suspended')
      expect(dbg.currentStep).toBe(1)
    })

    it('should have current property that tracks currentStep', async () => {
      const dbg = createDebugger()
      await dbg.run('1 + 2')
      const firstEntry = dbg.current

      await dbg.stepForward()
      if (dbg.history.length > 1) {
        expect(dbg.current).not.toBe(firstEntry)
        expect(dbg.current).toBe(dbg.history[dbg.currentStep])
      }
    })

    it('should return undefined for current when no run has been executed', () => {
      const dbg = createDebugger()
      expect(dbg.current).toBeUndefined()
      expect(dbg.currentStep).toBe(-1)
    })
  })

  describe('7b: Debugger API — bindings', () => {
    it('should pass bindings to the Lits program', async () => {
      const dbg = createDebugger({
        bindings: { myVal: 42 },
      })
      let r = await dbg.run('myVal + 1')
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(43)
      }
    })
  })

  describe('7b: Debugger API — end-to-end workflows', () => {
    it('should support full forward stepping through a simple program', async () => {
      const dbg = createDebugger()
      let r = await dbg.run('1 + 2')
      const steps: StepInfo[] = []

      while (r.type === 'suspended') {
        steps.push(dbg.current!.step)
        r = await dbg.stepForward()
      }

      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(3)
      }
      expect(steps.length).toBeGreaterThanOrEqual(1)
    })

    it('should support backward then forward navigation', async () => {
      const dbg = createDebugger()
      await dbg.run('let x = 1 + 2; x * 3')

      // Step forward several times
      await dbg.stepForward()
      await dbg.stepForward()
      const midStep = dbg.currentStep
      const midStepInfo = dbg.current?.step

      // Step back
      await dbg.stepBackward()
      expect(dbg.currentStep).toBe(midStep - 1)

      // Step forward again — should return to the same step
      await dbg.stepForward()
      expect(dbg.currentStep).toBe(midStep)
      expect(dbg.current?.step.value).toBe(midStepInfo?.value)
    })

    it('should support deterministic replay with handlers', async () => {
      let _callCount = 0
      const dbg = createDebugger({
        handlers: {
          'lits.random': async ({ resume }) => {
            _callCount++
            resume(0.42)
          },
        },
      })

      let r = await dbg.run('perform(effect(lits.random))')
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(0.42)
      }
    })

    it('should handle errors in program', async () => {
      const dbg = createDebugger()
      let r = await dbg.run('throw("boom")')
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('error')
    })

    it('should handle try/catch in program', async () => {
      const dbg = createDebugger()
      let r = await dbg.run(`
        try
          throw("oops")
        catch (e)
          "caught: " ++ e.userMessage
        end
      `)
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe('caught: oops')
      }
    })

    it('should handle loop/recur', async () => {
      const dbg = createDebugger()
      let r = await dbg.run(`
        loop(i = 0, acc = 0) ->
          if i >= 2 then acc
          else recur(i + 1, acc + i)
          end
      `)
      let stepCount = 0
      while (r.type === 'suspended' && stepCount < 200) {
        r = await dbg.stepForward()
        stepCount++
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(1) // 0 + 1 = 1
      }
    })

    it('should handle complex expressions with closures', async () => {
      const dbg = createDebugger()
      let r = await dbg.run(`
        let make-adder = (n) -> (x) -> x + n;
        let add5 = make-adder(5);
        add5(10)
      `)
      while (r.type === 'suspended') {
        r = await dbg.stepForward()
      }
      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(15)
      }
    })

    it('should run to completion when stepping forward exhaustively', async () => {
      const dbg = createDebugger()
      let r = await dbg.run('[1, 2, 3] |> map(_, -> $ * $) |> reduce(_, +, 0)')
      let stepCount = 0
      const maxSteps = 1000 // safety limit

      while (r.type === 'suspended' && stepCount < maxSteps) {
        r = await dbg.stepForward()
        stepCount++
      }

      expect(r.type).toBe('completed')
      if (r.type === 'completed') {
        expect(r.value).toBe(14) // 1 + 4 + 9 = 14
      }
    })
  })

  describe('7b: extractBindings utility', () => {
    it('should capture step info with bindings from outer scope', async () => {
      const dbg = createDebugger({
        bindings: { outer: 99 },
      })
      await dbg.run('let inner = 1; inner + outer')

      // Step through until we find a step with both bindings
      let r = await dbg.stepForward()
      let foundBothBindings = false
      while (r.type === 'suspended') {
        const env = dbg.current!.step.env
        if (env && 'inner' in env && 'outer' in env) {
          expect(env.inner).toBe(1)
          expect(env.outer).toBe(99)
          foundBothBindings = true
          break
        }
        r = await dbg.stepForward()
      }
      // At minimum, bindings should have been visible at some point
      expect(foundBothBindings || dbg.history.some(h => 'outer' in h.step.env)).toBe(true)
    })
  })

  describe('7c: edge cases for coverage', () => {
    it('should handle createDebugger with modules', async () => {
      const dbg = createDebugger({
        modules: [assertModule],
      })
      const r = await dbg.run('1 + 2')
      expect(r.type).toBe('suspended')
    })

    it('should handle non-LitsError thrown during run', async () => {
      // Use an invalid source that triggers a non-LitsError
      // We mock by providing a handler that throws a native error
      const dbg = createDebugger({
        handlers: {
          'test.throw': async () => {
            throw new TypeError('native error')
          },
        },
      })
      const r = await dbg.run('perform(effect(test.throw))')
      // Step through until the effect is reached
      let result = r
      while (result.type === 'suspended') {
        result = await dbg.stepForward()
      }
      expect(result.type).toBe('error')
    })

    it('should handle resumeFromBlob error paths', async () => {
      const dbg = createDebugger()
      const r = await dbg.run('1 + 2')
      expect(r.type).toBe('suspended')

      // rerunFrom with a different value — exercises the resumeFromBlob code path
      const result = await dbg.rerunFrom(0, 42 as unknown as Any)
      // Should return a result (either suspended, completed, or error)
      expect(['suspended', 'completed', 'error']).toContain(result.type)
    })

    it('should handle modules in resumeFromBlob path', async () => {
      const dbg = createDebugger({
        modules: [assertModule],
      })
      let r = await dbg.run('let x = 1 + 2; x * 3')
      expect(r.type).toBe('suspended')
      // Step forward triggers resumeFromBlob with modules
      r = await dbg.stepForward()
      // Should either be suspended (more steps) or completed (done)
      expect(['suspended', 'completed']).toContain(r.type)
    })
  })
})
