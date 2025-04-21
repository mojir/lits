import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'

describe('object functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('keys', () => {
      it('samples', () => {
        expect(lits.run('object()')).toEqual({})
        // expect(lits.run('keys(object())')).toEqual([])
        // expect(lits.run('keys(object("x", 1))')).toEqual(['x'])
        // expect(lits.run('keys(object("x", null, "y", 2))')).toEqual(['x', 'y'])
        // expect(() => lits.run('keys()')).toThrow(LitsError)
        // expect(() => lits.run('keys(0)')).toThrow(LitsError)
        // expect(() => lits.run('keys(true)')).toThrow(LitsError)
        // expect(() => lits.run('keys(false)')).toThrow(LitsError)
        // expect(() => lits.run('keys(null)')).toThrow(LitsError)
        // expect(() => lits.run('keys([1])')).toThrow(LitsError)
      })
    })

    describe('vals', () => {
      it('samples', () => {
        expect(lits.run('vals(object())')).toEqual([])
        expect(lits.run('vals(object("x", 1))')).toEqual([1])
        expect(lits.run('vals(object("x", null, "y", 2))')).toEqual([null, 2])
        expect(() => lits.run('vals()')).toThrow(LitsError)
        expect(() => lits.run('vals(object("x") object("x"))')).toThrow(LitsError)
        expect(() => lits.run('vals(0)')).toThrow(LitsError)
        expect(() => lits.run('vals(true)')).toThrow(LitsError)
        expect(() => lits.run('vals(false)')).toThrow(LitsError)
        expect(() => lits.run('vals(null)')).toThrow(LitsError)
        expect(() => lits.run('vals([1])')).toThrow(LitsError)
      })
    })

    describe('entries', () => {
      it('samples', () => {
        expect(lits.run('entries(object())')).toEqual([])
        expect(lits.run('entries(object("x", 1))')).toEqual([['x', 1]])
        expect(lits.run('entries(object("x", null, "y", 2))')).toEqual([
          ['x', null],
          ['y', 2],
        ])
        expect(() => lits.run('entries()')).toThrow(LitsError)
        expect(() => lits.run('entries(object("x") object("x"))')).toThrow(LitsError)
        expect(() => lits.run('entries(0)')).toThrow(LitsError)
        expect(() => lits.run('entries(true)')).toThrow(LitsError)
        expect(() => lits.run('entries(false)')).toThrow(LitsError)
        expect(() => lits.run('entries(null)')).toThrow(LitsError)
        expect(() => lits.run('entries([1])')).toThrow(LitsError)
      })
    })

    describe('find', () => {
      it('samples', () => {
        expect(lits.run('find(object("x", 1), "a")')).toBeNull()
        expect(lits.run('find(object("x", 1), "x")')).toEqual(['x', 1])
        expect(lits.run('find(object("x", 1, "y", 2), "x")')).toEqual(['x', 1])
        expect(() => lits.run('find()')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"), object("x"))')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"), null)')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"), true)')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"), false)')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"), "x" "y")')).toThrow(LitsError)
        expect(() => lits.run('find(object("x"))')).toThrow(LitsError)
        expect(() => lits.run('find([], "x")')).toThrow(LitsError)
        expect(() => lits.run('find(null, "x")')).toThrow(LitsError)
        expect(() => lits.run('find(false, "x")')).toThrow(LitsError)
        expect(() => lits.run('find(4, "x")')).toThrow(LitsError)
      })
    })

    describe('dissoc', () => {
      it('samples', () => {
        expect(lits.run('dissoc(object(), "x")')).toEqual({})
        expect(lits.run('dissoc(object("x", 1, "y", 2), "x")')).toEqual({ y: 2 })
        expect(lits.run('dissoc(object("x", 1), "")')).toEqual({ x: 1 })
        expect(lits.run('dissoc(object("x", object()), "x")')).toEqual({})
        expect(() => lits.run('dissoc()')).toThrow(LitsError)
        expect(() => lits.run('dissoc(object("x", 1) 1)')).toThrow(LitsError)
        expect(() => lits.run('dissoc(object("x"), object("x"))')).toThrow(LitsError)
        expect(() => lits.run('dissoc(0, "x")')).toThrow(LitsError)
        expect(() => lits.run('dissoc(true, "x")')).toThrow(LitsError)
        expect(() => lits.run('dissoc(false, "x")')).toThrow(LitsError)
        expect(() => lits.run('dissoc(null, "x")')).toThrow(LitsError)
        expect(() => lits.run('dissoc([1], "x")')).toThrow(LitsError)
      })
      it('delete atribute', () => {
        const program = `
        let obj := { x := 10 };
        dissoc(obj, "x");
        obj
      `
        expect(lits.run(program)).toEqual({ x: 10 })
      })

      it('delete unexisting attribute', () => {
        const program = `
        let obj := { x := 10 };
        dissoc(obj, "y");
        obj
      `
        expect(lits.run(program)).toEqual({ x: 10 })
      })
    })

    describe('merge', () => {
      it('samples', () => {
        expect(lits.run('merge(object("x", 10))')).toEqual({ x: 10 })
        expect(lits.run('merge(object("x", 10), object("y", 20))')).toEqual({ x: 10, y: 20 })
        expect(lits.run('merge(object("x", 10), object("x", 5))')).toEqual({ x: 5 })
        expect(lits.run('merge({}, { x := 10 }, { y := 10 }, { z := 10 })')).toEqual({
          x: 10,
          y: 10,
          z: 10,
        })
        expect(lits.run('merge()')).toBeNull()
        expect(() => lits.run('merge(1)')).toThrow(LitsError)
        expect(() => lits.run('merge(:1)')).toThrow(LitsError)
        expect(() => lits.run('merge(true)')).toThrow(LitsError)
        expect(() => lits.run('merge(false)')).toThrow(LitsError)
        expect(() => lits.run('merge(null)')).toThrow(LitsError)
        expect(() => lits.run('merge((array))')).toThrow(LitsError)
      })

      describe('merge-with', () => {
        it('samples', () => {
          expect(lits.run('merge-with(object("x", 10), object("y", 20), +)')).toEqual({
            x: 10,
            y: 20,
          })
          expect(lits.run('merge-with(object("x", 10), object("x", 15, "y", 20), +)')).toEqual({
            x: 25,
            y: 20,
          })
          expect(lits.run('merge-with(object("x", 10), object("x", 20), object("x", 30), object("x", 40), -)')).toEqual({
            x: -80,
          })
          expect(() => lits.run('merge-with(+)')).toThrow(LitsError)
          expect(() => lits.run('merge-with()')).toThrow(LitsError)
          expect(() => lits.run('merge-with(+, "kjh")')).toThrow(LitsError)
          expect(() => lits.run('merge-with(+, [1, 2, 3])')).toThrow(LitsError)
        })
      })

      it('merge returns new object', () => {
        const program = `
        let obj1 := object("x", 10);
        let obj2 := merge(obj1);
        identical?(obj1, obj2)
      `
        expect(lits.run(program)).toBe(false)
      })
    })

    describe('zipmap', () => {
      it('samples', () => {
        expect(lits.run('zipmap(["a", "b", "c"], [10, null, [1, 2, 3]])')).toEqual({ a: 10, b: null, c: [1, 2, 3] })
        expect(lits.run('zipmap(["a", "b"], [10, null, [1, 2, 3]])')).toEqual({ a: 10, b: null })
        expect(lits.run('zipmap(["a", "b", "c"], [10, null])')).toEqual({ a: 10, b: null })
        expect(lits.run('zipmap(["a", "b", "c"], [])')).toEqual({})
        expect(lits.run('zipmap([], [10, null, [1, 2, 3]])')).toEqual({})
        expect(lits.run('zipmap([], [])')).toEqual({})
        expect(() => lits.run('zipmap([])')).toThrow(LitsError)
        expect(() => lits.run('zipmap("abc", [])')).toThrow(LitsError)
        expect(() => lits.run('zipmap([], "abc)')).toThrow(LitsError)
        expect(() => lits.run('zipmap([], [], [])')).toThrow(LitsError)
      })
    })

    describe('select-keys', () => {
      it('samples', () => {
        expect(lits.run('select-keys({a := 1, b := 2, c := 3}, ["a", "b"])')).toEqual({ a: 1, b: 2 })
        expect(lits.run('select-keys({a := 1}, ["a", "b"])')).toEqual({ a: 1 })
        expect(() => lits.run('select-keys({a := 1})')).toThrow(LitsError)
        expect(() => lits.run('select-keys({a := 1}, "a")')).toThrow(LitsError)
        expect(() => lits.run('select-keys({a := 1}, ["a"], ["a"])')).toThrow(LitsError)
      })
    })
  }
})
