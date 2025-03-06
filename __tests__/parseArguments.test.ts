import { describe, expect, it } from 'vitest'
import { Lits } from '../src'

describe('parseArguments', () => {
  for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
    describe('defn', () => {
      it('& params', () => {
        expect(() => lits.run('(defn \'foo\' [&rest a] a)')).not.toThrow()
        expect(() => lits.run('(defn foo [&rest 1] a)')).toThrow()
        expect(() => lits.run('(defn foo [x &rest a] a)')).not.toThrow()
        expect(() => lits.run('(defn foo [x &rest] a)')).toThrow()
        expect(() => lits.run('(defn foo [x &rest a b] a)')).toThrow()
        expect(() => lits.run('(defn foo [x &rest a] a) (foo 1)')).not.toThrow()
        expect(() => lits.run('(defn foo [x &rest a] a) (foo)')).toThrow()
        expect(() => lits.run('(defn foo [&rest a] a) (foo +)')).not.toThrow()
        const program = `
        (defn foo [first &rest rest] rest)
        (foo 1 2 3)
      `
        expect(lits.run(program)).toEqual([2, 3])
      })
      it('modifier combinations', () => {
        expect(lits.run('((fn [&rest b] [b]))')).toEqual([[]])
        expect(lits.run('((fn [&rest b] [b]) 1)')).toEqual([[1]])
        expect(lits.run('((fn [&rest b] [b]) 1 2)')).toEqual([[1, 2]])
        expect(lits.run('((fn [&rest b] [b]) 1 2 3)')).toEqual([[1, 2, 3]])
        expect(lits.run('((fn [a &rest d] [a d]) 1)')).toEqual([1, []])
        expect(lits.run('((fn [a &rest d] [a d]) 1 2)')).toEqual([1, [2]])
        expect(lits.run('((fn [a &rest d] [a d]) 1 2 3)')).toEqual([1, [2, 3]])
        expect(lits.run('((fn [a &rest d] [a d]) 1 2 3 4)')).toEqual([1, [2, 3, 4]])
        expect(() => lits.run('(fn [&rest &rest a] a)')).toThrow()
        expect(() => lits.run('(fn [&when a] a)')).toThrow()
        expect(() => lits.run('(fn [&while a] a)')).toThrow()
      })
    })
  }
})
