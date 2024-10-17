import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'

describe('specialFunctions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('string as function', () => {
      it('samples', () => {
        expect(lits.run('(def person {"firstName" "Albert", "lastName", "Mojir"}) ("firstName" person)')).toBe('Albert')
        expect(lits.run('("firstName" {"firstName" "Albert", "lastName", "Mojir"})')).toBe('Albert')
        expect(lits.run('("lastName" {"firstName" "Albert", "lastName", "Mojir"})')).toBe('Mojir')
        expect(lits.run('(:x {"firstName" "Albert", "lastName", "Mojir"})')).toBeNull()
        expect(lits.run('("Albert" 2)')).toBe('b')
        expect(lits.run('("Albert" 12)')).toBeNull()
        expect(() => lits.run('("firstName" {"firstName" "Albert", "lastName", "Mojir"} 1)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('(1 {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('(nil {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('(undefined {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('(true {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('(false {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('({} {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('([] {"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
      })
    })

    describe('object as function', () => {
      it('samples', () => {
        expect(lits.run('(def person {"firstName" "Albert", "lastName", "Mojir"}) (person "firstName")')).toBe('Albert')
        expect(lits.run('({"firstName" "Albert", "lastName", "Mojir"} "firstName")')).toBe('Albert')
        expect(lits.run('({"firstName" "Albert", "lastName", "Mojir"} "lastName")')).toBe('Mojir')
        expect(lits.run('({"firstName" "Albert", "lastName", "Mojir"} :x)')).toBeNull()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"})')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} 1)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} nil)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} undefined)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} true)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} false)')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} {})')).toThrow()
        expect(() => lits.run('({"firstName" "Albert", "lastName", "Mojir"} [])')).toThrow()
      })
    })

    describe('array as function', () => {
      it('samples', () => {
        expect(lits.run('(def nameArray ["Albert" "Mojir"]) (nameArray 0)')).toBe('Albert')
        expect(lits.run('(["Albert" "Mojir"] 0)')).toBe('Albert')
        expect(lits.run('((cons 1 [2 3]) 1)')).toBe(2)
        expect(lits.run('("Albert" 0)')).toBe('A')
        expect(lits.run('("Albert" 10)')).toBeNull()
        expect(() => lits.run('(["Albert" "Mojir"])')).toThrow()
        expect(() => lits.run('(["Albert" "Mojir"] :0)')).toThrow()
        expect(() => lits.run('(["Albert" "Mojir"] 0 1)')).toThrow()
        expect(() => lits.run('((+ 1 2) 0)')).toThrow()
      })
    })

    describe('number as function', () => {
      it('samples', () => {
        expect(lits.run('(def nameArray ["Albert" "Mojir"]) (0 nameArray)')).toBe('Albert')
        expect(lits.run('(0 ["Albert" "Mojir"])')).toBe('Albert')
        expect(lits.run('(3 ["Albert" "Mojir"])')).toBeNull()
        expect(lits.run('(1 (cons 1 [2 3]))')).toBe(2)
        expect(lits.run('(1 "Albert")')).toBe('l')
        expect(lits.run('(10 "Albert")')).toBeNull()
        expect(() => lits.run('(["Albert" "Mojir"])')).toThrow()
        expect(() => lits.run('(:0 ["Albert" "Mojir"])')).toThrow()
        expect(() => lits.run('(0 1 ["Albert" "Mojir"])')).toThrow()
        expect(() => lits.run('((regexp "kj") "Albert")')).toThrow()
        expect(() => lits.run('(0 (+ 1 2))')).toThrow()
      })
    })
  }
})
