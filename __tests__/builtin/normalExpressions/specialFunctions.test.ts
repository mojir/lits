import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'

describe('specialFunctions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('string as function', () => {
      it('samples', () => {
        expect(lits.run('let person := { firstName := "Albert", lastName := "Mojir" }; "firstName"(person)')).toBe('Albert')
        expect(lits.run('"firstName"({ firstName := "Albert", lastName := "Mojir" })')).toBe('Albert')
        expect(lits.run('"lastName"({ firstName := "Albert", lastName := "Mojir" })')).toBe('Mojir')
        expect(lits.run('"x"({ firstName := "Albert", lastName := "Mojir" })')).toBeNull()
        expect(lits.run('"Albert"(2)')).toBe('b')
        expect(lits.run('"Albert"(12)')).toBeNull()
        expect(() => lits.run('"firstName"({ firstName := "Albert", lastName := "Mojir" }, 1)')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }()')).toThrow()
        expect(() => lits.run('0({ firstName := "Albert", lastName := "Mojir" })')).toThrow()
        expect(() => lits.run('{}({ firstName := "Albert", lastName := "Mojir" })')).toThrow()
        expect(() => lits.run('[]({ firstName := "Albert", lastName := "Mojir" })')).toThrow()
      })
    })

    describe('object as function', () => {
      it('samples', () => {
        expect(lits.run('let person := { firstName := "Albert", lastName := "Mojir" }; person("firstName")')).toBe('Albert')
        expect(lits.run('{ firstName := "Albert", lastName := "Mojir" }("firstName")')).toBe('Albert')
        expect(lits.run('{ firstName := "Albert", lastName := "Mojir" }("lastName")')).toBe('Mojir')
        expect(lits.run('{ firstName := "Albert", lastName := "Mojir" }("x")')).toBeNull()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }()')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }(1)')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }(null)')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }(true)')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }(false)')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }({})')).toThrow()
        expect(() => lits.run('{ firstName := "Albert", lastName := "Mojir" }([])')).toThrow()
      })
    })

    describe('array as function', () => {
      it('samples', () => {
        expect(lits.run('let name-array := ["Albert", "Mojir"]; name-array(0)')).toBe('Albert')
        expect(lits.run('["Albert", "Mojir"](0)')).toBe('Albert')
        expect(lits.run('unshift([2, 3], 1)(1)')).toBe(2)
        expect(lits.run('"Albert"(0)')).toBe('A')
        expect(lits.run('"Albert"(10)')).toBeNull()
        expect(() => lits.run('["Albert", "Mojir"]()')).toThrow()
        expect(() => lits.run('["Albert", "Mojir"]("0")')).toThrow()
        expect(() => lits.run('["Albert", "Mojir"](0, 1)')).toThrow()
      })
    })

    describe('number as function', () => {
      it('samples', () => {
        expect(lits.run('let name-array := ["Albert", "Mojir"]; 0(name-array)')).toBe('Albert')
        expect(lits.run('0(["Albert", "Mojir"])')).toBe('Albert')
        expect(lits.run('3(["Albert", "Mojir"])')).toBeNull()
        expect(lits.run('1(unshift([2, 3], 1))')).toBe(2)
        expect(lits.run('1("Albert")')).toBe('l')
        expect(lits.run('10("Albert")')).toBeNull()
        expect(() => lits.run('"0"(["Albert", "Mojir"])')).toThrow()
        expect(() => lits.run('0(1, ["Albert", "Mojir"])')).toThrow()
        expect(() => lits.run('0(1 + 2)')).toThrow()
      })
    })
  }
})
