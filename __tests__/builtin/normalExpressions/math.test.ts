import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'
import { mathUtilsModule } from '../../../src/builtin/modules/math'

describe('math functions', () => {
  const lits = new Lits()
  describe('inc', () => {
    it('samples', () => {
      expect(lits.run('inc(2.5)')).toBe(3.5)
      expect(lits.run('inc(1)')).toBe(2)
      expect(lits.run('inc(0)')).toBe(1)
      expect(lits.run('inc(-1)')).toBe(0)
      expect(lits.run('inc(-2.5)')).toBe(-1.5)
      expect(() => lits.run('inc()')).toThrow(LitsError)
      expect(() => lits.run('inc(1, 1)')).toThrow(LitsError)
      expect(() => lits.run('inc("1")')).toThrow(LitsError)
      expect(() => lits.run('inc(false)')).toThrow(LitsError)
      expect(() => lits.run('inc(true)')).toThrow(LitsError)
      expect(() => lits.run('inc(null)')).toThrow(LitsError)
      expect(() => lits.run('inc(boolean)')).toThrow(LitsError)
      expect(() => lits.run('inc({})')).toThrow(LitsError)
    })
    it('should increment a vector', () => {
      expect(lits.run('inc([1, 2, 3])')).toEqual([2, 3, 4])
      expect(lits.run('inc([-1, -2, -3])')).toEqual([0, -1, -2])
      expect(lits.run('inc([0])')).toEqual([1])
      expect(lits.run('inc([])')).toEqual([])
    })
    it('should increment a matrix', () => {
      expect(lits.run('inc([[1, 2], [3, 4]])')).toEqual([[2, 3], [4, 5]])
      expect(lits.run('inc([[-1, -2], [-3, -4]])')).toEqual([[0, -1], [-2, -3]])
      expect(lits.run('inc([[0]])')).toEqual([[1]])
    })
  })

  describe('dec', () => {
    it('samples', () => {
      expect(lits.run('dec(2.5)')).toBe(1.5)
      expect(lits.run('dec(1)')).toBe(0)
      expect(lits.run('dec(0)')).toBe(-1)
      expect(lits.run('dec(-1)')).toBe(-2)
      expect(lits.run('dec(-2.5)')).toBe(-3.5)
      expect(() => lits.run('dec()')).toThrow(LitsError)
      expect(() => lits.run('dec(1, 1)')).toThrow(LitsError)
      expect(() => lits.run('dec("1")')).toThrow(LitsError)
      expect(() => lits.run('dec(false)')).toThrow(LitsError)
      expect(() => lits.run('dec(true)')).toThrow(LitsError)
      expect(() => lits.run('dec(null)')).toThrow(LitsError)
      expect(() => lits.run('dec(boolean)')).toThrow(LitsError)
      expect(() => lits.run('dec({})')).toThrow(LitsError)
    })
    it('should decrement a vector', () => {
      expect(lits.run('dec([1, 2, 3])')).toEqual([0, 1, 2])
      expect(lits.run('dec([-1, -2, -3])')).toEqual([-2, -3, -4])
      expect(lits.run('dec([0])')).toEqual([-1])
      expect(lits.run('dec([])')).toEqual([])
    })
    it('should decrement a matrix', () => {
      expect(lits.run('dec([[1, 2], [3, 4]])')).toEqual([[0, 1], [2, 3]])
      expect(lits.run('dec([[-1, -2], [-3, -4]])')).toEqual([[-2, -3], [-4, -5]])
      expect(lits.run('dec([[0]])')).toEqual([[-1]])
    })
  })

  describe('+', () => {
    it('samples', () => {
      expect(lits.run('+()')).toBe(0)
      expect(lits.run('+(2, 3, 4)')).toBe(9)
      expect(lits.run('2 + 2')).toBe(4)
      expect(lits.run('-2 + 2')).toBe(0)
      expect(lits.run('1 + 2 + 3 + 4')).toBe(10)
      expect(() => lits.run('"1" + 2')).toThrow(LitsError)
    })
    it('should add vectors element-wise', () => {
      expect(lits.run('+([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('[1, 2, 3] + [4, 5, 6]')).toEqual([5, 7, 9])
      expect(lits.run('+([1, 2, 3], [4, 5, 6])')).toEqual([5, 7, 9])
      expect(lits.run('+([1, 2, 3], [4, 5, 6], [7, 8, 9])')).toEqual([12, 15, 18])
      expect(lits.run('+([1], [2])')).toEqual([3])
      expect(lits.run('+([], [])')).toEqual([])
      expect(() => lits.run('+([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('+([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('+([1], [])')).toThrowError(LitsError)
    })
    it('should add vectors and scalars', () => {
      expect(lits.run('+([1, 2, 3], 4)')).toEqual([5, 6, 7])
      expect(lits.run('+([1], 4)')).toEqual([5])
      expect(lits.run('+([], 4)')).toEqual([])
      expect(lits.run('+(4, [1, 2, 3])')).toEqual([5, 6, 7])
      expect(lits.run('+(4, [1])')).toEqual([5])
      expect(lits.run('+(4, [])')).toEqual([])
      expect(lits.run('+(4, [1, 2, 3], 5)')).toEqual([10, 11, 12])
      expect(() => lits.run('+([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('+([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('+([1], [])')).toThrowError(LitsError)
    })
    it('should throw if incompatible operands', () => {
      expect(() => lits.run('+([[1, 2]], [3, 4])')).toThrowError(LitsError)
      expect(() => lits.run('+([[1, 2]], [[3, 4], [5, 6]])')).toThrowError(LitsError)
    })
  })

  describe('*', () => {
    it('samples', () => {
      expect(lits.run('*()')).toBe(1)
      expect(lits.run('*(2)')).toBe(2)
      expect(lits.run('*(1, 2, 3)')).toBe(6)
      expect(lits.run('2 * 2')).toBe(4)
      expect(lits.run('-2 * 2')).toBe(-4)
      expect(lits.run('1 * 2 * 3 * 4')).toBe(24)
      expect(() => lits.run('"1" * 2')).toThrow(LitsError)
    })
    it('should multiply two vectors element-wise', () => {
      expect(lits.run('*([1, 2, 3], [4, 5, 6])')).toEqual([4, 10, 18])
      expect(lits.run('*(2, [1, 2, 3])')).toEqual([2, 4, 6])
      expect(lits.run('*(2, [1, 2, 3], 2)')).toEqual([4, 8, 12])
      expect(lits.run('*(2, [1, 2, 3], [2, 2, 2])')).toEqual([4, 8, 12])
      expect(lits.run('*([1], [2])')).toEqual([2])
      expect(lits.run('*([], [])')).toEqual([])
      expect(() => lits.run('*([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('*([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('*([1], [])')).toThrowError(LitsError)
    })
  })

  describe('/', () => {
    it('samples', () => {
      expect(lits.run('/()')).toBe(1)
      expect(lits.run('/(2)')).toBe(0.5)
      expect(lits.run('/(8, 4, 2)')).toBe(1)
      expect(lits.run('2 / 5')).toBe(2 / 5)
      expect(lits.run('2 / 2')).toBe(2 / 2)
      expect(lits.run('-2 / 2')).toBe(-2 / 2)
      expect(lits.run('1 / 2 / 3 / 4')).toBe(1 / 2 / 3 / 4)
      expect(() => lits.run('"1" / 2')).toThrow(LitsError)
    })
    it('should divide two vectors element-wise', () => {
      expect(lits.run('/([1, 2, 3], [4, 5, 6])')).toEqual([0.25, 0.4, 0.5])
      expect(lits.run('/([1, 2, 3], 2)')).toEqual([0.5, 1, 1.5])
      expect(lits.run('/(12, [1, 2, 3], 2)')).toEqual([6, 3, 2])
      expect(lits.run('/([1], [2])')).toEqual([0.5])
      expect(lits.run('/([], [])')).toEqual([])
      expect(() => lits.run('/([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('/([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('/([1], [])')).toThrowError(LitsError)
    })
  })

  describe('-', () => {
    it('samples', () => {
      expect(lits.run('-()')).toBe(0)
      expect(lits.run('-(1)')).toBe(-1)
      expect(lits.run('-(1, 2, 3)')).toBe(1 - 2 - 3)
      expect(lits.run('2 - 2')).toBe(2 - 2)
      expect(lits.run('-2 - 2')).toBe(-2 - 2)
      expect(lits.run('1 - 2 - 3 - 4')).toBe(1 - 2 - 3 - 4)
      expect(() => lits.run('"1" - 2')).toThrow(LitsError)
    })
    it('strange bug', () => {
      expect(lits.run('let a = 0; let b = 2; a - b')).toBe(-2)
    })
    it('should subtract vectors', () => {
      expect(lits.run('-([1, 2, 3], [4, 5, 6])')).toEqual([-3, -3, -3])
      expect(lits.run('-([1, 2, 3], [4, 5, 6], -3)')).toEqual([0, 0, 0])
      expect(lits.run('-(10, [1, 2, 3])')).toEqual([9, 8, 7])
      expect(lits.run('-([1], [2])')).toEqual([-1])
      expect(lits.run('-([], [])')).toEqual([])
      expect(() => lits.run('-([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('-([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('-([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('-([1], [])')).toThrowError(LitsError)
    })
  })

  describe('sqrt', () => {
    it('samples', () => {
      expect(() => lits.run('sqrt()')).toThrow(LitsError)
      expect(() => lits.run('sqrt(3, 4)')).toThrow(LitsError)
      expect(() => lits.run('sqrt(-3)')).toThrow(LitsError)
      expect(lits.run('sqrt(0)')).toBe(0)
      expect(lits.run('sqrt(1)')).toBe(1)
      expect(lits.run('sqrt(4)')).toBe(2)
    })
    it('should take the square root of a vector', () => {
      expect(lits.run('sqrt([1, 4, 9])')).toEqual([1, 2, 3])
      expect(lits.run('sqrt([0])')).toEqual([0])
      expect(lits.run('sqrt([])')).toEqual([])
    })
    it('should take the square root of a matrix', () => {
      expect(lits.run('sqrt([[1, 4], [9, 16]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('sqrt([[0]])')).toEqual([[0]])
    })
  })

  describe('cbrt', () => {
    it('samples', () => {
      expect(() => lits.run('cbrt()')).toThrow(LitsError)
      expect(() => lits.run('cbrt(3, 4)')).toThrow(LitsError)
      expect(lits.run('cbrt(-8)')).toBe(-2)
      expect(lits.run('cbrt(0)')).toBe(0)
      expect(lits.run('cbrt(1)')).toBe(1)
      expect(lits.run('cbrt(8)')).toBe(2)
      expect(lits.run('cbrt(12)')).toBe(Math.cbrt(12))
    })
    it('should take the cube root of a vector', () => {
      expect(lits.run('cbrt([1, 8, 27])')).toEqual([1, 2, 3])
      expect(lits.run('cbrt([0])')).toEqual([0])
      expect(lits.run('cbrt([])')).toEqual([])
    })
    it('should take the cube root of a matrix', () => {
      expect(lits.run('cbrt([[1, 8], [27, 64]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('cbrt([[0]])')).toEqual([[0]])
    })
  })

  describe('^', () => {
    it('samples', () => {
      expect(() => lits.run('^()')).toThrow(LitsError)
      expect(() => lits.run('^(3)')).toThrow(LitsError)
      expect(() => lits.run('^(3, 4, 5)')).toThrow(LitsError)
      expect(lits.run('2 ^ 3 ^ 2')).toBe(2 ** 3 ** 2)
      expect(lits.run('^(2, 0)')).toBe(1)
      expect(lits.run('^(2, 0)')).toBe(1)
      expect(lits.run('^(2, 1)')).toBe(2)
      expect(lits.run('^(2, 2)')).toBe(4)
      expect(lits.run('^(2, 3)')).toBe(8)
      expect(lits.run('^(16, 0.5)')).toBe(4)
      expect(lits.run('^(10, -1)')).toBe(0.1)
      expect(lits.run('^(10, -2)')).toBe(0.01)
      expect(lits.run('^(-2, -1)')).toBe(-0.5)
      expect(lits.run('^(-2, -2)')).toBe(0.25)
    })
    it('should exponentiate two vectors element-wise', () => {
      expect(lits.run('^([1, 2, 3], [4, 5, 6])')).toEqual([1, 32, 729])
      expect(lits.run('^([1], [2])')).toEqual([1])
      expect(lits.run('^([], [])')).toEqual([])
      expect(() => lits.run('^([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('^([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('^([1], [])')).toThrowError(LitsError)
    })
    it('should exponentiate a vector by a scalar', () => {
      expect(lits.run('^([1, 2, 3], 2)')).toEqual([1, 4, 9])
      expect(lits.run('^([1], 2)')).toEqual([1])
      expect(lits.run('^([], 2)')).toEqual([])
      expect(() => lits.run('^([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('^([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('^([1], [])')).toThrowError(LitsError)
    })
    it('should exponentiate a scalar by a vector', () => {
      expect(lits.run('^(2, [1, 2, 3])')).toEqual([2, 4, 8])
      expect(lits.run('^(2, [1])')).toEqual([2])
      expect(lits.run('^(2, [])')).toEqual([])
      expect(() => lits.run('^([2], [3, 4, 5])')).toThrowError(LitsError)
    })
  })

  describe('round', () => {
    it('samples', () => {
      expect(() => lits.run('round()')).toThrow(LitsError)
      expect(() => lits.run('round(3, 4, 5)')).toThrow(LitsError)
      expect(lits.run('round(0)')).toBe(0)
      expect(lits.run('round(1)')).toBe(1)
      expect(lits.run('round(0.4)')).toBe(0)
      expect(lits.run('round(0.5)')).toBe(1)
      expect(lits.run('round(0.6)')).toBe(1)
      expect(lits.run('round(-0.4)')).toBe(-0)
      expect(lits.run('round(-0.5)')).toBe(-0)
      expect(lits.run('round(-0.6)')).toBe(-1)
      expect(lits.run('round(-0.125, 1)')).toBe(-0.1)
      expect(lits.run('round(0.125, 2)')).toBe(0.13)
    })
    it('should round a vector', () => {
      expect(lits.run('round([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('round([1.4, 2.5, 3.6])')).toEqual([1, 3, 4])
      expect(lits.run('round([0])')).toEqual([0])
      expect(lits.run('round([])')).toEqual([])
    })
    it('should round a matrix', () => {
      expect(lits.run('round([[1, 2], [3, 4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('round([[1.4, 2.5], [3.6, 4.7]])')).toEqual([[1, 3], [4, 5]])
      expect(lits.run('round([[0]])')).toEqual([[0]])
    })
  })

  describe('floor', () => {
    it('samples', () => {
      expect(() => lits.run('floor()')).toThrow(LitsError)
      expect(() => lits.run('floor(3, 4)')).toThrow(LitsError)
      expect(lits.run('floor(0)')).toBe(0)
      expect(lits.run('floor(1)')).toBe(1)
      expect(lits.run('floor(0.4)')).toBe(0)
      expect(lits.run('floor(0.5)')).toBe(0)
      expect(lits.run('floor(0.6)')).toBe(0)
      expect(lits.run('floor(-0.4)')).toBe(-1)
      expect(lits.run('floor(-0.5)')).toBe(-1)
      expect(lits.run('floor(-0.6)')).toBe(-1)
    })
    it('should floor a vector', () => {
      expect(lits.run('floor([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('floor([1.4, 2.5, 3.6])')).toEqual([1, 2, 3])
      expect(lits.run('floor([0])')).toEqual([0])
      expect(lits.run('floor([])')).toEqual([])
    })
    it('should floor a matrix', () => {
      expect(lits.run('floor([[1, 2], [3, 4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('floor([[1.4, 2.5], [3.6, 4.7]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('floor([[0]])')).toEqual([[0]])
    })
  })

  describe('ceil', () => {
    it('samples', () => {
      expect(() => lits.run('ceil()')).toThrow(LitsError)
      expect(() => lits.run('ceil(3, 4)')).toThrow(LitsError)
      expect(lits.run('ceil(0)')).toBe(0)
      expect(lits.run('ceil(1)')).toBe(1)
      expect(lits.run('ceil(0.4)')).toBe(1)
      expect(lits.run('ceil(0.5)')).toBe(1)
      expect(lits.run('ceil(0.6)')).toBe(1)
      expect(lits.run('ceil(-0.4)')).toBe(-0)
      expect(lits.run('ceil(-0.5)')).toBe(-0)
      expect(lits.run('ceil(-0.6)')).toBe(-0)
    })
    it('should ceil a vector', () => {
      expect(lits.run('ceil([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('ceil([1.4, 2.5, 3.6])')).toEqual([2, 3, 4])
      expect(lits.run('ceil([0])')).toEqual([0])
      expect(lits.run('ceil([])')).toEqual([])
    })
    it('should ceil a matrix', () => {
      expect(lits.run('ceil([[1, 2], [3, 4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('ceil([[1.4, 2.5], [3.6, 4.7]])')).toEqual([[2, 3], [4, 5]])
      expect(lits.run('ceil([[0]])')).toEqual([[0]])
    })
  })

  describe('min', () => {
    it('samples', () => {
      expect(lits.run('1 min -2')).toBe(-2)
      expect(lits.run('min(1)')).toBe(1)
      expect(lits.run('min(1, -2)')).toBe(-2)
      expect(lits.run('min(3, 1, 2 )')).toBe(1)
      expect(() => lits.run('min()')).toThrow(LitsError)
      expect(() => lits.run('min("1")')).toThrow(LitsError)
      expect(() => lits.run('min("1", "3")')).toThrow(LitsError)
    })
  })

  describe('max', () => {
    it('samples', () => {
      expect(lits.run('1 max -2')).toBe(1)
      expect(lits.run('max(1)')).toBe(1)
      expect(lits.run('max(1, -2)')).toBe(1)
      expect(lits.run('max(3, 1, 2)')).toBe(3)
      expect(() => lits.run('max()')).toThrow(LitsError)
      expect(() => lits.run('max("1")')).toThrow(LitsError)
      expect(() => lits.run('max("1", "3")')).toThrow(LitsError)
    })
  })

  describe('abs', () => {
    it('samples', () => {
      expect(lits.run('abs(2)')).toBe(2)
      expect(lits.run('abs(-2)')).toBe(2)
      expect(lits.run('abs(-0)')).toBe(0)
      expect(() => lits.run('abs()')).toThrow(LitsError)
      expect(() => lits.run('abs(1, 2)')).toThrow(LitsError)
    })
    it('should take the absolute value of a vector', () => {
      expect(lits.run('abs([1, -2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('abs([-1, -2, -3])')).toEqual([1, 2, 3])
      expect(lits.run('abs([0])')).toEqual([0])
      expect(lits.run('abs([])')).toEqual([])
    })
    it('should take the absolute value of a matrix', () => {
      expect(lits.run('abs([[1, -2], [3, -4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('abs([[-1, -2], [-3, -4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('abs([[0]])')).toEqual([[0]])
    })
  })

  describe('sign', () => {
    it('samples', () => {
      expect(lits.run('sign(2)')).toBe(1)
      expect(lits.run('sign(-2)')).toBe(-1)
      expect(lits.run('sign(-0)')).toBe(-0)
      expect(lits.run('sign(0)')).toBe(0)
      expect(() => lits.run('sign()')).toThrow(LitsError)
      expect(() => lits.run('sign(1, 2)')).toThrow(LitsError)
    })
    it('should take the sign of a vector', () => {
      expect(lits.run('sign([1, -2, 3])')).toEqual([1, -1, 1])
      expect(lits.run('sign([-1, -2, -3])')).toEqual([-1, -1, -1])
      expect(lits.run('sign([0])')).toEqual([0])
      expect(lits.run('sign([])')).toEqual([])
    })
    it('should take the sign of a matrix', () => {
      expect(lits.run('sign([[1, -2], [3, -4]])')).toEqual([[1, -1], [1, -1]])
      expect(lits.run('sign([[-1, -2], [-3, -4]])')).toEqual([[-1, -1], [-1, -1]])
      expect(lits.run('sign([[0]])')).toEqual([[0]])
    })
  })

  describe('ln', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { ln } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}ln(0.1)`)).toBe(Math.log(0.1))
      expect(mlits.run(`${m}ln(1)`)).toBe(Math.log(1))
      expect(mlits.run(`${m}ln(100)`)).toBe(Math.log(100))
      expect(() => mlits.run(`${m}ln(-2)`)).toThrow(LitsError)
      expect(mlits.run(`${m}ln(0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(mlits.run(`${m}ln(-0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(() => mlits.run(`${m}ln()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}ln(1, 2)`)).toThrow(LitsError)
    })
    it('should take the natural logarithm of a vector', () => {
      expect(mlits.run(`${m}ln([1, 2, 3])`)).toEqual([0, Math.log(2), Math.log(3)])
      expect(mlits.run(`${m}ln([0])`)).toEqual([Number.NEGATIVE_INFINITY])
      expect(mlits.run(`${m}ln([])`)).toEqual([])
    })
    it('should take the natural logarithm of a matrix', () => {
      expect(mlits.run(`${m}ln([[1, 2], [3, 4]])`)).toEqual([[0, Math.log(2)], [Math.log(3), Math.log(4)]])
      expect(mlits.run(`${m}ln([[0]])`)).toEqual([[Number.NEGATIVE_INFINITY]])
      expect(mlits.run(`${m}ln([[-1]])`)).toEqual([[Number.NaN]])
    })
  })

  describe('log2', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { log2 } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}log2(0.1)`)).toBe(Math.log2(0.1))
      expect(mlits.run(`${m}log2(1)`)).toBe(Math.log2(1))
      expect(mlits.run(`${m}log2(100)`)).toBe(Math.log2(100))
      expect(() => mlits.run(`${m}log2(-2)`)).toThrow(LitsError)
      expect(mlits.run(`${m}log2(0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(mlits.run(`${m}log2(-0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(() => mlits.run(`${m}log2()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}log2(1, 2)`)).toThrow(LitsError)
    })
    it('should take the base 2 logarithm of a vector', () => {
      expect(mlits.run(`${m}log2([1, 2, 3])`)).toEqual([0, 1, Math.log2(3)])
      expect(mlits.run(`${m}log2([0])`)).toEqual([Number.NEGATIVE_INFINITY])
      expect(mlits.run(`${m}log2([])`)).toEqual([])
    })
    it('should take the base 2 logarithm of a matrix', () => {
      expect(mlits.run(`${m}log2([[1, 2], [3, 4]])`)).toEqual([[0, 1], [Math.log2(3), Math.log2(4)]])
      expect(mlits.run(`${m}log2([[0]])`)).toEqual([[Number.NEGATIVE_INFINITY]])
    })
  })

  describe('log10', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { log10 } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}log10(0.1)`)).toBe(Math.log10(0.1))
      expect(mlits.run(`${m}log10(1)`)).toBe(Math.log10(1))
      expect(mlits.run(`${m}log10(100)`)).toBe(Math.log10(100))
      expect(() => mlits.run(`${m}log10(-2)`)).toThrow(LitsError)
      expect(mlits.run(`${m}log10(0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(mlits.run(`${m}log10(-0)`)).toBe(Number.NEGATIVE_INFINITY)
      expect(() => mlits.run(`${m}log10()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}log10(1, 2)`)).toThrow(LitsError)
    })
    it('should take the base 10 logarithm of a vector', () => {
      expect(mlits.run(`${m}log10([1, 2, 3])`)).toEqual([0, Math.log10(2), Math.log10(3)])
      expect(mlits.run(`${m}log10([0])`)).toEqual([Number.NEGATIVE_INFINITY])
      expect(mlits.run(`${m}log10([])`)).toEqual([])
    })
    it('should take the base 10 logarithm of a matrix', () => {
      expect(mlits.run(`${m}log10([[1, 2], [3, 4]])`)).toEqual([[0, Math.log10(2)], [Math.log10(3), Math.log10(4)]])
      expect(mlits.run(`${m}log10([[0]])`)).toEqual([[Number.NEGATIVE_INFINITY]])
    })
  })

  describe('trunc', () => {
    it('samples', () => {
      expect(lits.run('trunc(0)')).toBe(0)
      expect(lits.run('trunc(0.123)')).toBe(0)
      expect(lits.run('trunc(0.999)')).toBe(0)
      expect(lits.run('trunc(-0.99)')).toBe(-0)
      expect(lits.run('trunc(-0.1)')).toBe(-0)
      expect(() => lits.run('trunc()')).toThrow(LitsError)
      expect(() => lits.run('trunc(100, 200)')).toThrow(LitsError)
    })
    it('should truncate a vector', () => {
      expect(lits.run('trunc([1, 2, 3])')).toEqual([1, 2, 3])
      expect(lits.run('trunc([0.1, -0.1])')).toEqual([0, -0])
      expect(lits.run('trunc([])')).toEqual([])
    })
    it('should truncate a matrix', () => {
      expect(lits.run('trunc([[1, 2], [3, 4]])')).toEqual([[1, 2], [3, 4]])
      expect(lits.run('trunc([[0.1, -0.1], [-0.1, 0.1]])')).toEqual([[0, -0], [-0, 0]])
      expect(lits.run('trunc([[0]])')).toEqual([[0]])
    })
  })

  describe('sin', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { sin } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}sin(0)`)).toBe(Math.sin(0))
      expect(mlits.run(`${m}sin(0.1)`)).toBe(Math.sin(0.1))
      expect(mlits.run(`${m}sin(-0.1)`)).toBe(Math.sin(-0.1))
      expect(mlits.run(`${m}sin(1)`)).toBe(Math.sin(1))
      expect(mlits.run(`${m}sin(100)`)).toBe(Math.sin(100))
      expect(() => mlits.run(`${m}sin()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}sin(1, 2)`)).toThrow(LitsError)
    })
    it('should take the sine of a vector', () => {
      expect(mlits.run(`${m}sin([0, 0.1, -0.1])`)).toEqual([Math.sin(0), Math.sin(0.1), Math.sin(-0.1)])
      expect(mlits.run(`${m}sin([])`)).toEqual([])
    })
    it('should take the sine of a matrix', () => {
      expect(mlits.run(`${m}sin([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.sin(0), Math.sin(0.1)], [Math.sin(-0.1), Math.sin(1)]])
      expect(mlits.run(`${m}sin([[0]])`)).toEqual([[Math.sin(0)]])
    })
  })
  describe('cos', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { cos } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}cos(0)`)).toBe(Math.cos(0))
      expect(mlits.run(`${m}cos(0.1)`)).toBe(Math.cos(0.1))
      expect(mlits.run(`${m}cos(-0.1)`)).toBe(Math.cos(-0.1))
      expect(mlits.run(`${m}cos(1)`)).toBe(Math.cos(1))
      expect(mlits.run(`${m}cos(100)`)).toBe(Math.cos(100))
      expect(() => mlits.run(`${m}cos()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}cos(1, 2)`)).toThrow(LitsError)
    })
    it('should take the cosine of a vector', () => {
      expect(mlits.run(`${m}cos([0, 0.1, -0.1])`)).toEqual([Math.cos(0), Math.cos(0.1), Math.cos(-0.1)])
      expect(mlits.run(`${m}cos([])`)).toEqual([])
    })
    it('should take the cosine of a matrix', () => {
      expect(mlits.run(`${m}cos([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.cos(0), Math.cos(0.1)], [Math.cos(-0.1), Math.cos(1)]])
      expect(mlits.run(`${m}cos([[0]])`)).toEqual([[Math.cos(0)]])
    })
  })
  describe('tan', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { tan } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}tan(0)`)).toBe(Math.tan(0))
      expect(mlits.run(`${m}tan(0.1)`)).toBe(Math.tan(0.1))
      expect(mlits.run(`${m}tan(-0.1)`)).toBe(Math.tan(-0.1))
      expect(mlits.run(`${m}tan(1)`)).toBe(Math.tan(1))
      expect(mlits.run(`${m}tan(100)`)).toBe(Math.tan(100))
      expect(() => mlits.run(`${m}tan()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}tan(1, 2)`)).toThrow(LitsError)
    })
    it('should take the tangent of a vector', () => {
      expect(mlits.run(`${m}tan([0, 0.1, -0.1])`)).toEqual([Math.tan(0), Math.tan(0.1), Math.tan(-0.1)])
      expect(mlits.run(`${m}tan([])`)).toEqual([])
    })
    it('should take the tangent of a matrix', () => {
      expect(mlits.run(`${m}tan([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.tan(0), Math.tan(0.1)], [Math.tan(-0.1), Math.tan(1)]])
      expect(mlits.run(`${m}tan([[0]])`)).toEqual([[Math.tan(0)]])
    })
  })

  describe('sinh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { sinh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}sinh(0)`)).toBe(Math.sinh(0))
      expect(mlits.run(`${m}sinh(0.1)`)).toBe(Math.sinh(0.1))
      expect(mlits.run(`${m}sinh(-0.1)`)).toBe(Math.sinh(-0.1))
      expect(mlits.run(`${m}sinh(1)`)).toBe(Math.sinh(1))
      expect(mlits.run(`${m}sinh(100)`)).toBe(Math.sinh(100))
      expect(() => mlits.run(`${m}sinh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}sinh(1, 2)`)).toThrow(LitsError)
    })
    it('should take the hyperbolic sine of a vector', () => {
      expect(mlits.run(`${m}sinh([0, 0.1, -0.1])`)).toEqual([Math.sinh(0), Math.sinh(0.1), Math.sinh(-0.1)])
      expect(mlits.run(`${m}sinh([])`)).toEqual([])
    })
    it('should take the hyperbolic sine of a matrix', () => {
      expect(mlits.run(`${m}sinh([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.sinh(0), Math.sinh(0.1)], [Math.sinh(-0.1), Math.sinh(1)]])
      expect(mlits.run(`${m}sinh([[0]])`)).toEqual([[Math.sinh(0)]])
    })
  })
  describe('cosh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { cosh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}cosh(0)`)).toBe(Math.cosh(0))
      expect(mlits.run(`${m}cosh(0.1)`)).toBe(Math.cosh(0.1))
      expect(mlits.run(`${m}cosh(-0.1)`)).toBe(Math.cosh(-0.1))
      expect(mlits.run(`${m}cosh(1)`)).toBe(Math.cosh(1))
      expect(mlits.run(`${m}cosh(100)`)).toBe(Math.cosh(100))
      expect(() => mlits.run(`${m}cosh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}cosh(1, 2)`)).toThrow(LitsError)
    })
    it('should take the hyperbolic cosine of a vector', () => {
      expect(mlits.run(`${m}cosh([0, 0.1, -0.1])`)).toEqual([Math.cosh(0), Math.cosh(0.1), Math.cosh(-0.1)])
      expect(mlits.run(`${m}cosh([])`)).toEqual([])
    })
    it('should take the hyperbolic cosine of a matrix', () => {
      expect(mlits.run(`${m}cosh([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.cosh(0), Math.cosh(0.1)], [Math.cosh(-0.1), Math.cosh(1)]])
      expect(mlits.run(`${m}cosh([[0]])`)).toEqual([[Math.cosh(0)]])
    })
  })
  describe('tanh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { tanh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}tanh(0)`)).toBe(Math.tanh(0))
      expect(mlits.run(`${m}tanh(0.1)`)).toBe(Math.tanh(0.1))
      expect(mlits.run(`${m}tanh(-0.1)`)).toBe(Math.tanh(-0.1))
      expect(mlits.run(`${m}tanh(1)`)).toBe(Math.tanh(1))
      expect(mlits.run(`${m}tanh(100)`)).toBe(Math.tanh(100))
      expect(() => mlits.run(`${m}tanh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}tanh(1, 2)`)).toThrow(LitsError)
    })
    it('should take the hyperbolic tangent of a vector', () => {
      expect(mlits.run(`${m}tanh([0, 0.1, -0.1])`)).toEqual([Math.tanh(0), Math.tanh(0.1), Math.tanh(-0.1)])
      expect(mlits.run(`${m}tanh([])`)).toEqual([])
    })
    it('should take the hyperbolic tangent of a matrix', () => {
      expect(mlits.run(`${m}tanh([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.tanh(0), Math.tanh(0.1)], [Math.tanh(-0.1), Math.tanh(1)]])
      expect(mlits.run(`${m}tanh([[0]])`)).toEqual([[Math.tanh(0)]])
    })
  })

  describe('asin', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { asin } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}asin(0)`)).toBe(Math.asin(0))
      expect(mlits.run(`${m}asin(0.1)`)).toBe(Math.asin(0.1))
      expect(mlits.run(`${m}asin(-0.1)`)).toBe(Math.asin(-0.1))
      expect(mlits.run(`${m}asin(1)`)).toBe(Math.asin(1))
      expect(() => mlits.run(`${m}asin(100)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}asin()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}asin(1, 2)`)).toThrow(LitsError)
    })
    it('should take the arcsine of a vector', () => {
      expect(mlits.run(`${m}asin([0, 0.1, -0.1])`)).toEqual([Math.asin(0), Math.asin(0.1), Math.asin(-0.1)])
      expect(mlits.run(`${m}asin([])`)).toEqual([])
    })
    it('should take the arcsine of a matrix', () => {
      expect(mlits.run(`${m}asin([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.asin(0), Math.asin(0.1)], [Math.asin(-0.1), Math.asin(1)]])
      expect(mlits.run(`${m}asin([[0]])`)).toEqual([[Math.asin(0)]])
    })
  })
  describe('acos', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { acos } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}acos(0)`)).toBe(Math.acos(0))
      expect(mlits.run(`${m}acos(0.1)`)).toBe(Math.acos(0.1))
      expect(mlits.run(`${m}acos(-0.1)`)).toBe(Math.acos(-0.1))
      expect(mlits.run(`${m}acos(1)`)).toBe(Math.acos(1))
      expect(() => mlits.run(`${m}acos(100)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acos()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acos(1, 2)`)).toThrow(LitsError)
    })
    it('should take the arccosine of a vector', () => {
      expect(mlits.run(`${m}acos([0, 0.1, -0.1])`)).toEqual([Math.acos(0), Math.acos(0.1), Math.acos(-0.1)])
      expect(mlits.run(`${m}acos([])`)).toEqual([])
    })
    it('should take the arccosine of a matrix', () => {
      expect(mlits.run(`${m}acos([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.acos(0), Math.acos(0.1)], [Math.acos(-0.1), Math.acos(1)]])
      expect(mlits.run(`${m}acos([[0]])`)).toEqual([[Math.acos(0)]])
    })
  })
  describe('atan', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { atan } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}atan(0)`)).toBe(Math.atan(0))
      expect(mlits.run(`${m}atan(0.1)`)).toBe(Math.atan(0.1))
      expect(mlits.run(`${m}atan(-0.1)`)).toBe(Math.atan(-0.1))
      expect(mlits.run(`${m}atan(1)`)).toBe(Math.atan(1))
      expect(mlits.run(`${m}atan(100)`)).toBe(Math.atan(100))
      expect(() => mlits.run(`${m}atan()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}atan(1, 2)`)).toThrow(LitsError)
    })
    it('should take the arctangent of a vector', () => {
      expect(mlits.run(`${m}atan([0, 0.1, -0.1])`)).toEqual([Math.atan(0), Math.atan(0.1), Math.atan(-0.1)])
      expect(mlits.run(`${m}atan([])`)).toEqual([])
    })
    it('should take the arctangent of a matrix', () => {
      expect(mlits.run(`${m}atan([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.atan(0), Math.atan(0.1)], [Math.atan(-0.1), Math.atan(1)]])
      expect(mlits.run(`${m}atan([[0]])`)).toEqual([[Math.atan(0)]])
    })
  })

  describe('asinh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { asinh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}asinh(0)`)).toBe(Math.asinh(0))
      expect(mlits.run(`${m}asinh(0.1)`)).toBe(Math.asinh(0.1))
      expect(mlits.run(`${m}asinh(-0.1)`)).toBe(Math.asinh(-0.1))
      expect(mlits.run(`${m}asinh(1)`)).toBe(Math.asinh(1))
      expect(mlits.run(`${m}asinh(100)`)).toBe(Math.asinh(100))
      expect(() => mlits.run(`${m}asinh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}asinh(1, 2)`)).toThrow(LitsError)
    })
    it('should take the hyperbolic arcsine of a vector', () => {
      expect(mlits.run(`${m}asinh([0, 0.1, -0.1])`)).toEqual([Math.asinh(0), Math.asinh(0.1), Math.asinh(-0.1)])
      expect(mlits.run(`${m}asinh([])`)).toEqual([])
    })
    it('should take the hyperbolic arcsine of a matrix', () => {
      expect(mlits.run(`${m}asinh([[0, 0.1], [-0.1, 1]])`)).toEqual([[Math.asinh(0), Math.asinh(0.1)], [Math.asinh(-0.1), Math.asinh(1)]])
      expect(mlits.run(`${m}asinh([[0]])`)).toEqual([[Math.asinh(0)]])
    })
  })
  describe('acosh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { acosh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}acosh(1)`)).toBe(Math.acosh(1))
      expect(mlits.run(`${m}acosh(100)`)).toBe(Math.acosh(100))
      expect(() => mlits.run(`${m}acosh(0.1)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acosh(-0.1)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acosh(0)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acosh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}acosh(1, 2)`)).toThrow(LitsError)
    })
    it('should take the hyperbolic arccosine of a vector', () => {
      expect(mlits.run(`${m}acosh([1, 100])`)).toEqual([Math.acosh(1), Math.acosh(100)])
      expect(mlits.run(`${m}acosh([])`)).toEqual([])
    })
    it('should take the hyperbolic arccosine of a matrix', () => {
      expect(mlits.run(`${m}acosh([[1, 100], [0.1, -0.1]])`)).toEqual([[Math.acosh(1), Math.acosh(100)], [Math.acosh(0.1), Math.acosh(-0.1)]])
      expect(mlits.run(`${m}acosh([[0]])`)).toEqual([[Math.acosh(0)]])
    })
  })
  describe('atanh', () => {
    const mlits = new Lits({ modules: [mathUtilsModule] })
    const m = 'let { atanh } = import(math); '
    it('samples', () => {
      expect(mlits.run(`${m}atanh(0)`)).toBe(Math.atanh(0))
      expect(mlits.run(`${m}atanh(0.1)`)).toBe(Math.atanh(0.1))
      expect(mlits.run(`${m}atanh(-0.1)`)).toBe(Math.atanh(-0.1))
      expect(mlits.run(`${m}atanh(1)`)).toBe(Number.POSITIVE_INFINITY)
      expect(() => mlits.run(`${m}atanh(100)`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}atanh()`)).toThrow(LitsError)
      expect(() => mlits.run(`${m}atanh(1, 2)`)).toThrow(LitsError)
    })
  })

  describe('quot', () => {
    it('samples', () => {
      expect(lits.run('quot(13.75, 3.25)')).toBe(4)
      expect(lits.run('quot(-13.75, 3.25)')).toBe(-4)
      expect(lits.run('quot(13.75, -3.25)')).toBe(-4)
      expect(lits.run('quot(-13.75, -3.25)')).toBe(4)
      expect(() => lits.run('quot()')).toThrow(LitsError)
      expect(() => lits.run('quot(1)')).toThrow(LitsError)
      expect(() => lits.run('quot(1, 2, 3)')).toThrow(LitsError)
    })
    it('should take the integer division of a vector', () => {
      expect(lits.run('quot([1, 2, 3], 2)')).toEqual([0, 1, 1])
      expect(lits.run('quot([1, 2, 3], [3, 2, 1])')).toEqual([0, 1, 3])
      expect(lits.run('quot([1], 2)')).toEqual([0])
      expect(lits.run('quot([], 2)')).toEqual([])
      expect(() => lits.run('quot([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('quot([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('quot([1], [])')).toThrowError(LitsError)
    })
    it('should take the integer division of a matrix', () => {
      expect(lits.run('quot([[1, 2], [3, 4]], 2)')).toEqual([[0, 1], [1, 2]])
      expect(lits.run('quot([[1, 2], [3, 4]], [[4, 3], [2, 1]])')).toEqual([[0, 0], [1, 4]])
      expect(lits.run('quot([[1], [2]], 2)')).toEqual([[0], [1]])
    })
  })

  describe('mod', () => {
    it('samples', () => {
      expect(() => lits.run('mod()')).toThrow(LitsError)
      expect(() => lits.run('mod(3)')).toThrow(LitsError)
      expect(() => lits.run('mod(3, 4, 5)')).toThrow(LitsError)
      expect(lits.run('3 mod 2')).toBe(1)
      expect(lits.run('mod(13.75, 3.25)')).toBe(0.75)
      expect(lits.run('mod(-13.75, 3.25)')).toBe(2.5)
      expect(lits.run('mod(13.75, -3.25)')).toBe(-2.5)
      expect(lits.run('mod(-13.75, -3.25)')).toBe(-0.75)
      expect(lits.run('mod(2, 1)')).toBe(0)
      expect(lits.run('mod(2, 2)')).toBe(0)
      expect(lits.run('mod(3, 2)')).toBe(1)
      expect(lits.run('mod(3, -2)')).toBe(-1)
      expect(lits.run('mod(-3, -2)')).toBe(-1)
      expect(lits.run('mod(-3, 2)')).toBe(1)
      expect(() => lits.run('mod(4, 0)')).toThrow(LitsError)
      expect(() => lits.run('mod(4, 0, 3)')).toThrow(LitsError)
    })
    it('should take the modulus of a vector', () => {
      expect(lits.run('mod([1, 2, 3], 2)')).toEqual([1, 0, 1])
      expect(lits.run('mod([1, 2, 3], [3, 2, 1])')).toEqual([1, 0, 0])
      expect(lits.run('mod([1], 2)')).toEqual([1])
      expect(lits.run('mod([], 2)')).toEqual([])
      expect(() => lits.run('mod([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('mod([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('mod([1], [])')).toThrowError(LitsError)
    })
    it('should take the modulus of a matrix', () => {
      expect(lits.run('mod([[1, 2], [3, 4]], 2)')).toEqual([[1, 0], [1, 0]])
      expect(lits.run('mod([[1, 2], [3, 4]], [[4, 3], [2, 1]])')).toEqual([[1, 2], [1, 0]])
      expect(lits.run('mod([[1], [2]], 2)')).toEqual([[1], [0]])
    })
  })

  describe('%', () => {
    it('samples', () => {
      expect(lits.run('13.75 % 3.25')).toBe(0.75)
      expect(lits.run('-13.75 % 3.25')).toBe(-0.75)
      expect(lits.run('%(13.75, 3.25)')).toBe(0.75)
      expect(lits.run('%(-13.75, 3.25)')).toBe(-0.75)
      expect(lits.run('%(-13.75, 3.25)')).toBe(-0.75)
      expect(lits.run('%(13.75, -3.25)')).toBe(0.75)
      expect(lits.run('%(-13.75, -3.25)')).toBe(-0.75)
      expect(() => lits.run('%()')).toThrow(LitsError)
      expect(() => lits.run('%(1)')).toThrow(LitsError)
      expect(() => lits.run('%(1, 2, 3)')).toThrow(LitsError)
    })
    it('should take the remainder of a vector', () => {
      expect(lits.run('%([1, 2, 3], 2)')).toEqual([1, 0, 1])
      expect(lits.run('%([1, 2, 3], [3, 2, 1])')).toEqual([1, 0, 0])
      expect(lits.run('%([1], 2)')).toEqual([1])
      expect(lits.run('%([], 2)')).toEqual([])
      expect(() => lits.run('%([1, 2], [3, 4, 5])')).toThrowError(LitsError)
      expect(() => lits.run('%([], [1])')).toThrowError(LitsError)
      expect(() => lits.run('%([1], [])')).toThrowError(LitsError)
    })
    it('should take the remainder of a matrix', () => {
      expect(lits.run('%([[1, 2], [3, 4]], 2)')).toEqual([[1, 0], [1, 0]])
      expect(lits.run('%([[1, 2], [3, 4]], [[4, 3], [2, 1]])')).toEqual([[1, 2], [1, 0]])
      expect(lits.run('%([[1], [2]], 2)')).toEqual([[1], [0]])
    })
  })
})
