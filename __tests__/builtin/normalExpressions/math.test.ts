import { lispish } from '../../../src'

describe('math functions', () => {
  describe('1+', () => {
    test('samples', () => {
      expect(lispish('(1+ 2.5)')).toBe(3.5)
      expect(lispish('(1+ 1)')).toBe(2)
      expect(lispish('(1+ 0)')).toBe(1)
      expect(lispish('(1+ -1)')).toBe(0)
      expect(lispish('(1+ -2.5)')).toBe(-1.5)
      expect(() => lispish('(1+)')).toThrow()
      expect(() => lispish('(1+ 1 1)')).toThrow()
      expect(() => lispish('(1+ "1")')).toThrow()
      expect(() => lispish('(1+ false)')).toThrow()
      expect(() => lispish('(1+ true)')).toThrow()
      expect(() => lispish('(1+ null)')).toThrow()
      expect(() => lispish('(1+ boolean)')).toThrow()
      expect(() => lispish('(1+ (list))')).toThrow()
      expect(() => lispish('(1+ (object))')).toThrow()
    })
  })

  describe('1+', () => {
    test('samples', () => {
      expect(lispish('(1- 2.5)')).toBe(1.5)
      expect(lispish('(1- 1)')).toBe(0)
      expect(lispish('(1- 0)')).toBe(-1)
      expect(lispish('(1- -1)')).toBe(-2)
      expect(lispish('(1- -2.5)')).toBe(-3.5)
      expect(() => lispish('(1-)')).toThrow()
      expect(() => lispish('(1- 1 1)')).toThrow()
      expect(() => lispish('(1- "1")')).toThrow()
      expect(() => lispish('(1- false)')).toThrow()
      expect(() => lispish('(1- true)')).toThrow()
      expect(() => lispish('(1- null)')).toThrow()
      expect(() => lispish('(1- boolean)')).toThrow()
      expect(() => lispish('(1- (list))')).toThrow()
      expect(() => lispish('(1- (object))')).toThrow()
    })
  })

  describe('+', () => {
    test('samples', () => {
      expect(lispish('(+)')).toBe(0)
      expect(lispish('(+ 2)')).toBe(2)
      expect(lispish('(+ 2 2)')).toBe(4)
      expect(lispish('(+ -2 2)')).toBe(0)
      expect(lispish('(+ 1 2 3 4)')).toBe(10)
      expect(() => lispish('(+ "1" 2 3 4)')).toThrow()
    })
  })

  describe('*', () => {
    test('samples', () => {
      expect(lispish('(*)')).toBe(1)
      expect(lispish('(* 2)')).toBe(2)
      expect(lispish('(* 2 2)')).toBe(4)
      expect(lispish('(* -2 2)')).toBe(-4)
      expect(lispish('(* 1 2 3 4)')).toBe(24)
      expect(() => lispish('(* "1" 2 3 4)')).toThrow()
    })
  })

  describe('/', () => {
    test('samples', () => {
      expect(lispish('(/)')).toBe(1)
      expect(lispish('(/ 2)')).toBe(1 / 2)
      expect(lispish('(/ 2 2)')).toBe(2 / 2)
      expect(lispish('(/ -2 2)')).toBe(-2 / 2)
      expect(lispish('(/ 1 2 3 4)')).toBe(1 / 2 / 3 / 4)
      expect(() => lispish('(/ "1" 2 3 4)')).toThrow()
    })
  })

  describe('-', () => {
    test('samples', () => {
      expect(lispish('(-)')).toBe(0)
      expect(lispish('(- 2)')).toBe(-2)
      expect(lispish('(- 2 2)')).toBe(2 - 2)
      expect(lispish('(- -2 2)')).toBe(-2 - 2)
      expect(lispish('(- 1 2 3 4)')).toBe(1 - 2 - 3 - 4)
      expect(() => lispish('(- "1" 2 3 4)')).toThrow()
    })
  })

  describe('%', () => {
    test('samples', () => {
      expect(() => lispish('(%)')).toThrow()
      expect(() => lispish('(% 3)')).toThrow()
      expect(() => lispish('(% 3 4 5)')).toThrow()
      expect(lispish('(% 2 1)')).toBe(0)
      expect(lispish('(% 2 2)')).toBe(0)
      expect(lispish('(% 3 2)')).toBe(1)
      expect(lispish('(% 3 -2)')).toBe(1)
      expect(lispish('(% -3 -2)')).toBe(-1)
      expect(lispish('(% -3 2)')).toBe(-1)
      expect(() => lispish('(% 4 0)')).toThrow()
    })
  })

  describe('sqrt', () => {
    test('samples', () => {
      expect(() => lispish('(sqrt)')).toThrow()
      expect(() => lispish('(sqrt 3 4)')).toThrow()
      expect(() => lispish('(sqrt -3)')).toThrow()
      expect(lispish('(sqrt 0)')).toBe(0)
      expect(lispish('(sqrt 1)')).toBe(1)
      expect(lispish('(sqrt 4)')).toBe(2)
    })
  })

  describe('expt', () => {
    test('samples', () => {
      expect(() => lispish('(expt)')).toThrow()
      expect(() => lispish('(expt 3)')).toThrow()
      expect(() => lispish('(expt 3 4 5)')).toThrow()
      expect(lispish('(expt 2 0)')).toBe(1)
      expect(lispish('(expt 2 1)')).toBe(2)
      expect(lispish('(expt 2 2)')).toBe(4)
      expect(lispish('(expt 2 3)')).toBe(8)
      expect(lispish('(expt 16 0.5)')).toBe(4)
      expect(lispish('(expt 10 -1)')).toBe(0.1)
      expect(lispish('(expt 10 -2)')).toBe(0.01)
      expect(lispish('(expt -2 -1)')).toBe(-0.5)
      expect(lispish('(expt -2 -2)')).toBe(0.25)
    })
  })

  describe('round', () => {
    test('samples', () => {
      expect(() => lispish('(round)')).toThrow()
      expect(() => lispish('(round 3 4)')).toThrow()
      expect(lispish('(round 0)')).toBe(0)
      expect(lispish('(round 1)')).toBe(1)
      expect(lispish('(round 0.4)')).toBe(0)
      expect(lispish('(round 0.5)')).toBe(1)
      expect(lispish('(round 0.6)')).toBe(1)
      expect(lispish('(round -0.4)')).toBe(-0)
      expect(lispish('(round -0.5)')).toBe(-0)
      expect(lispish('(round -0.6)')).toBe(-1)
    })
  })

  describe('floor', () => {
    test('samples', () => {
      expect(() => lispish('(floor)')).toThrow()
      expect(() => lispish('(floor 3 4)')).toThrow()
      expect(lispish('(floor 0)')).toBe(0)
      expect(lispish('(floor 1)')).toBe(1)
      expect(lispish('(floor 0.4)')).toBe(0)
      expect(lispish('(floor 0.5)')).toBe(0)
      expect(lispish('(floor 0.6)')).toBe(0)
      expect(lispish('(floor -0.4)')).toBe(-1)
      expect(lispish('(floor -0.5)')).toBe(-1)
      expect(lispish('(floor -0.6)')).toBe(-1)
    })
  })

  describe('ceil', () => {
    test('samples', () => {
      expect(() => lispish('(ceil)')).toThrow()
      expect(() => lispish('(ceil 3 4)')).toThrow()
      expect(lispish('(ceil 0)')).toBe(0)
      expect(lispish('(ceil 1)')).toBe(1)
      expect(lispish('(ceil 0.4)')).toBe(1)
      expect(lispish('(ceil 0.5)')).toBe(1)
      expect(lispish('(ceil 0.6)')).toBe(1)
      expect(lispish('(ceil -0.4)')).toBe(-0)
      expect(lispish('(ceil -0.5)')).toBe(-0)
      expect(lispish('(ceil -0.6)')).toBe(-0)
    })
  })

  describe('random', () => {
    test('samples', () => {
      expect(() => lispish('(random)')).toThrow()
      expect(() => lispish('(random "x")')).toThrow()
      expect(() => lispish('(random 1 2)')).toThrow()
      expect(lispish('(random 0.1)')).toBeLessThan(0.1)
      expect(lispish('(random 0.1)')).toBeGreaterThanOrEqual(0)
    })
  })

  describe('>', () => {
    test('samples', () => {
      expect(() => lispish('(>)')).toThrow()
      expect(lispish('(> 1)')).toBe(true)
      expect(lispish('(> 1 2)')).toBe(false)
      expect(lispish('(> 1 1)')).toBe(false)
      expect(lispish('(> 2 1)')).toBe(true)
      expect(lispish('(> 2 1 2)')).toBe(false)
      expect(lispish('(> 2 1 0)')).toBe(true)
      expect(() => lispish('(> "1")')).toThrow()
      expect(() => lispish('(> "1" "3")')).toThrow()
    })
  })

  describe('<', () => {
    test('samples', () => {
      expect(() => lispish('(<)')).toThrow()
      expect(lispish('(< 1)')).toBe(true)
      expect(lispish('(< 1 2)')).toBe(true)
      expect(lispish('(< 1 1)')).toBe(false)
      expect(lispish('(< 2 1)')).toBe(false)
      expect(lispish('(< 1 2 1)')).toBe(false)
      expect(lispish('(< 0 1 2)')).toBe(true)
      expect(() => lispish('(< "1")')).toThrow()
      expect(() => lispish('(< "1" "3")')).toThrow()
    })
  })

  describe('>=', () => {
    test('samples', () => {
      expect(() => lispish('(>=)')).toThrow()
      expect(lispish('(>= 1)')).toBe(true)
      expect(lispish('(>= 1 2)')).toBe(false)
      expect(lispish('(>= 1 1)')).toBe(true)
      expect(lispish('(>= 2 1)')).toBe(true)
      expect(lispish('(>= 2 1 2)')).toBe(false)
      expect(lispish('(>= 2 1 1)')).toBe(true)
      expect(() => lispish('(>= "1")')).toThrow()
      expect(() => lispish('(>= "1" "3")')).toThrow()
    })
  })

  describe('<=', () => {
    test('samples', () => {
      expect(() => lispish('(<=)')).toThrow()
      expect(lispish('(<= 1)')).toBe(true)
      expect(lispish('(<= 1 2)')).toBe(true)
      expect(lispish('(<= 1 1)')).toBe(true)
      expect(lispish('(<= 2 1)')).toBe(false)
      expect(lispish('(<= 1 2 1)')).toBe(false)
      expect(lispish('(<= 1 2 2)')).toBe(true)
      expect(() => lispish('(<= "1")')).toThrow()
      expect(() => lispish('(<= "1" "3")')).toThrow()
    })
  })
})
