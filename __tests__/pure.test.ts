import { describe, expect, it } from 'vitest'
import { Lits } from '../src/Lits/Lits'
import { randomModule } from '../src/builtin/modules/random'
import type { LitsBundle } from '../src/bundler/interface'

describe('pure mode', () => {
  describe('core builtins', () => {
    it('should allow pure functions in pure mode', () => {
      const lits = new Lits()
      expect(lits.run('1 + 2', { pure: true })).toBe(3)
    })

    it('should block write! in pure mode', () => {
      const lits = new Lits()
      expect(() => lits.run('write!("hello")', { pure: true })).toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })

    it('should allow write! without pure mode', () => {
      const lits = new Lits()
      expect(lits.run('write!("hello")')).toBe('hello')
    })

    it('should not block write! in a dead branch', () => {
      const lits = new Lits()
      expect(lits.run('if false then write!("x") else 42 end', { pure: true })).toBe(42)
    })
  })

  describe('module functions', () => {
    it('should block random! in pure mode', () => {
      const lits = new Lits({ modules: [randomModule] })
      expect(() => lits.run('let { random! } = import(random); random!()', { pure: true })).toThrow(
        'Cannot call impure function \'random!\' in pure mode',
      )
    })

    it('should allow random! without pure mode', () => {
      const lits = new Lits({ modules: [randomModule] })
      const result = lits.run('let { random! } = import(random); random!()')
      expect(typeof result).toBe('number')
    })
  })

  describe('native JS functions', () => {
    it('should block impure native JS functions in pure mode', () => {
      const lits = new Lits()
      expect(() => lits.run('sideEffect()', {
        pure: true,
        bindings: { sideEffect: () => 42 },
      })).toThrow(
        'Cannot call impure native function \'sideEffect\' in pure mode',
      )
    })

    it('should allow impure native JS functions without pure mode', () => {
      const lits = new Lits()
      expect(lits.run('sideEffect()', {
        bindings: { sideEffect: () => 42 },
      })).toBe(42)
    })

    it('should block bare function bindings in pure mode', () => {
      const lits = new Lits()
      expect(() => lits.run('myFn()', {
        pure: true,
        bindings: { myFn: () => 42 },
      })).toThrow(
        'Cannot call impure native function \'myFn\' in pure mode',
      )
    })
  })

  describe('write! as value', () => {
    it('should block write! passed as a higher-order function in pure mode', () => {
      const lits = new Lits()
      expect(() => lits.run('let f = write!; f("hi")', { pure: true })).toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })
  })

  describe('async', () => {
    it('should block write! in async pure mode', async () => {
      const lits = new Lits()
      await expect(lits.async.run('write!("hello")', { pure: true })).rejects.toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })

    it('should allow pure code in async pure mode', async () => {
      const lits = new Lits()
      await expect(lits.async.run('1 + 2', { pure: true })).resolves.toBe(3)
    })
  })

  describe('file modules in bundles', () => {
    it('should block impure code in file module evaluation', () => {
      const lits = new Lits()
      const bundle: LitsBundle = {
        program: 'let m = import(mymod); m.value',
        fileModules: [
          ['mymod', 'write!("side effect"); { value: 42 }'],
        ],
      }
      expect(() => lits.run(bundle)).toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })

    it('should allow file modules that define impure functions without calling them', () => {
      const lits = new Lits()
      const bundle: LitsBundle = {
        program: 'let m = import(mymod); m.greet("world")',
        fileModules: [
          ['mymod', '{ greet: -> "hello " ++ $1 }'],
        ],
      }
      expect(lits.run(bundle)).toBe('hello world')
    })

    it('should enforce pure file modules even when caller does not pass pure', () => {
      const lits = new Lits()
      const bundle: LitsBundle = {
        program: '42',
        fileModules: [
          ['mymod', 'write!("side effect"); null'],
        ],
      }
      // File modules are always pure, regardless of caller's pure flag
      expect(() => lits.run(bundle)).toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })

    it('should allow impure code in the main program when pure is not set', () => {
      const lits = new Lits()
      const bundle: LitsBundle = {
        program: 'write!("hello")',
        fileModules: [],
      }
      expect(lits.run(bundle)).toBe('hello')
    })

    it('should block impure code in the main program when pure is set', () => {
      const lits = new Lits()
      const bundle: LitsBundle = {
        program: 'write!("hello")',
        fileModules: [],
      }
      expect(() => lits.run(bundle, { pure: true })).toThrow(
        'Cannot call impure function \'write!\' in pure mode',
      )
    })
  })
})
