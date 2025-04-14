import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

const lits = new Lits()
describe('random', () => {
  describe('!:random', () => {
    it('should return a random number between 0 and 1', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random()')
        expect(r).toBeLessThan(1)
        expect(r).toBeGreaterThanOrEqual(0)
      }
    })
  })
  describe('!:random-int', () => {
    it('should return a random integer between 0 and 10', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-int(0, 10)')
        expect(r).toBeLessThan(10)
        expect(r).toBeGreaterThanOrEqual(0)
      }
    })
  })
  describe('!:random-int-inclusive', () => {
    it('should return a random integer between 0 and 10 inclusive', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-int-inclusive(0, 10)')
        expect(Number.isInteger(r)).toBe(true)
        expect(r).toBeLessThanOrEqual(10)
        expect(r).toBeGreaterThanOrEqual(0)
      }
    })
  })
  describe('!:random-float', () => {
    it('should return a random float between 0 and 10', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-float(0, 10)')
        expect(r).toBeLessThan(10)
        expect(r).toBeGreaterThanOrEqual(0)
      }
    })
  })
  describe('!:random-boolean', () => {
    it('should return a random boolean', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-boolean()')
        expect(typeof r).toBe('boolean')
      }
    })
    it('should return true', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-boolean(1)')
        expect(r).toBe(true)
      }
    })
    it('should return false', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-boolean(0)')
        expect(r).toBe(false)
      }
    })
  })
  describe('!:random-item', () => {
    it('should return a random item from the array', () => {
      const array = [1, 2, 3, 4, 5]
      const r = lits.run(`!:random-item(${JSON.stringify(array)})`)
      expect(array).toContain(r)
    })
  })
  describe('!:random-sample', () => {
    it('should return a random sample of 3 items from the array', () => {
      const array = [1, 2, 3, 4, 5]
      const r = lits.run(`!:random-sample(${JSON.stringify(array)}, 3)`) as number[]
      expect(r.length).toBe(3)
      r.forEach((item) => {
        expect(array).toContain(item)
      })
    })
    it('should throw an error when sampling from an empty array', () => {
      const array: any[] = []
      expect(() => {
        lits.run(`!:random-sample(${JSON.stringify(array)}, 3)`)
      }).toThrow(LitsError)
    })
  })
  describe('!:random-sample-unique', () => {
    it('should return a random sample of 3 unique items from the array', () => {
      const array = [1, 2, 3, 4, 5]
      const r = lits.run(`!:random-sample-unique(${JSON.stringify(array)}, 3)`) as number[]
      expect(r.length).toBe(3)
      r.forEach((item) => {
        expect(array).toContain(item)
      })
    })
    it('should throw an error when sampling more items than available', () => {
      const array = [1, 2, 3]
      expect(() => {
        lits.run(`!:random-sample-unique(${JSON.stringify(array)}, 4)`)
      }).toThrowError(LitsError)
    })
  })
  describe('!:shufffle', () => {
    it('should return a shuffled array', () => {
      const array = [1, 2, 3, 4, 5]
      const r = lits.run(`!:shuffle(${JSON.stringify(array)})`) as number[]
      expect(r.length).toBe(5)
      r.forEach((item) => {
        expect(array).toContain(item)
      })
      expect(r).not.toEqual(array)
    })
  })
  describe('!:random-normal', () => {
    it('should return a random number from a normal distribution', () => {
      expect(typeof lits.run('!:random-normal(0, 1)')).toBe('number')
    })
  })
  describe('!:random-exponential', () => {
    it('should return a random number from an exponential distribution', () => {
      expect(typeof lits.run('!:random-exponential(1)')).toBe('number')
    })
  })
  describe('!:random-binomial', () => {
    it('should return a rundom number from a binomial distribution', () => {
      expect(typeof lits.run('!:random-binomial(2, 0.5)')).toBe('number')
    })
  })
  describe('!:random-poisson', () => {
    it('should return a random number from a poisson distribution', () => {
      expect(typeof lits.run('!:random-poisson(1.5)')).toBe('number')
    })
  })
  describe('!:random-gamma', () => {
    it('should return a random number from a gamma distribution', () => {
      expect(typeof lits.run('!:random-gamma(0.5, 0.5)')).toBe('number')
    })
  })
  describe('!:random-pareto', () => {
    it('should return a random number from a pareto distribution', () => {
      expect(typeof lits.run('!:random-pareto(1)')).toBe('number')
    })
  })
  describe('!:uuid', () => {
    it('should return a random UUID', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:uuid()')
        expect(typeof r).toBe('string')
        expect(r).toMatch(
          /^[0-9a-f]{8}-[0-9a-f]{4}-[1-9][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/,
        )
      }
    })
  })
  describe('!:random-char', () => {
    it('should return a random character from the given string', () => {
      for (let i = 0; i < 1000; i++) {
        const str = 'abcde'
        const r = lits.run(`!:random-char(${JSON.stringify(str)})`)
        expect(str).toContain(r)
      }
    })
  })
  describe('!:random-string', () => {
    it('should return a random string of length 10', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-string(10, "abcde")') as string
        expect(typeof r).toBe('string')
        expect(r.length).toBe(10)
        for (const char of r) {
          expect('abcde').toContain(char)
        }
      }
    })
  })
  describe('!:random-id', () => {
    it('should return a random id of length 10', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-id(10)') as string
        expect(typeof r).toBe('string')
        expect(r.length).toBe(10)
        expect(r).toMatch(/^[a-z0-9]+$/i)
      }
    })
  })
  describe('!:random-color', () => {
    it('should return a random color', () => {
      for (let i = 0; i < 1000; i++) {
        const r = lits.run('!:random-color()') as string
        expect(typeof r).toBe('string')
        expect(r).toMatch(/^#[0-9a-f]{6}$/i)
      }
    })
  })
})
