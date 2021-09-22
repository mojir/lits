import { lispish } from '../../../src'

describe('math functions', () => {
  describe('1+', () => {
    test('samples', () => {
      expect(lispish(`(1+ 2.5)`)).toBe(3.5)
      expect(lispish(`(1+ 1)`)).toBe(2)
      expect(lispish(`(1+ 0)`)).toBe(1)
      expect(lispish(`(1+ -1)`)).toBe(0)
      expect(lispish(`(1+ -2.5)`)).toBe(-1.5)
      expect(() => lispish(`(1+)`)).toThrow()
      expect(() => lispish(`(1+ 1 1)`)).toThrow()
      expect(() => lispish(`(1+ "1")`)).toThrow()
      expect(() => lispish(`(1+ false)`)).toThrow()
      expect(() => lispish(`(1+ true)`)).toThrow()
      expect(() => lispish(`(1+ null)`)).toThrow()
      expect(() => lispish(`(1+ boolean)`)).toThrow()
      expect(() => lispish(`(1+ '())`)).toThrow()
      expect(() => lispish(`(1+ (object))`)).toThrow()
    })
  })

  describe('1+', () => {
    test('samples', () => {
      expect(lispish(`(1- 2.5)`)).toBe(1.5)
      expect(lispish(`(1- 1)`)).toBe(0)
      expect(lispish(`(1- 0)`)).toBe(-1)
      expect(lispish(`(1- -1)`)).toBe(-2)
      expect(lispish(`(1- -2.5)`)).toBe(-3.5)
      expect(() => lispish(`(1-)`)).toThrow()
      expect(() => lispish(`(1- 1 1)`)).toThrow()
      expect(() => lispish(`(1- "1")`)).toThrow()
      expect(() => lispish(`(1- false)`)).toThrow()
      expect(() => lispish(`(1- true)`)).toThrow()
      expect(() => lispish(`(1- null)`)).toThrow()
      expect(() => lispish(`(1- boolean)`)).toThrow()
      expect(() => lispish(`(1- '())`)).toThrow()
      expect(() => lispish(`(1- (object))`)).toThrow()
    })
  })

  describe('+', () => {
    test('samples', () => {
      expect(lispish(`(+)`)).toBe(0)
      expect(lispish(`(+ 2)`)).toBe(2)
      expect(lispish(`(+ 2 2)`)).toBe(4)
      expect(lispish(`(+ -2 2)`)).toBe(0)
      expect(lispish(`(+ 1 2 3 4)`)).toBe(10)
      expect(() => lispish(`(+ "1" 2 3 4)`)).toThrow()
    })
  })

  describe('*', () => {
    test('samples', () => {
      expect(lispish(`(*)`)).toBe(1)
      expect(lispish(`(* 2)`)).toBe(2)
      expect(lispish(`(* 2 2)`)).toBe(4)
      expect(lispish(`(* -2 2)`)).toBe(-4)
      expect(lispish(`(* 1 2 3 4)`)).toBe(24)
      expect(() => lispish(`(* "1" 2 3 4)`)).toThrow()
    })
  })

  describe('/', () => {
    test('samples', () => {
      expect(lispish(`(/)`)).toBe(1)
      expect(lispish(`(/ 2)`)).toBe(1 / 2)
      expect(lispish(`(/ 2 2)`)).toBe(2 / 2)
      expect(lispish(`(/ -2 2)`)).toBe(-2 / 2)
      expect(lispish(`(/ 1 2 3 4)`)).toBe(1 / 2 / 3 / 4)
      expect(() => lispish(`(/ "1" 2 3 4)`)).toThrow()
    })
  })

  describe('-', () => {
    test('samples', () => {
      expect(lispish(`(-)`)).toBe(0)
      expect(lispish(`(- 2)`)).toBe(-2)
      expect(lispish(`(- 2 2)`)).toBe(2 - 2)
      expect(lispish(`(- -2 2)`)).toBe(-2 - 2)
      expect(lispish(`(- 1 2 3 4)`)).toBe(1 - 2 - 3 - 4)
      expect(() => lispish(`(- "1" 2 3 4)`)).toThrow()
    })
  })

  describe('%', () => {
    test('samples', () => {
      expect(() => lispish(`(%)`)).toThrow()
      expect(() => lispish(`(% 3)`)).toThrow()
      expect(() => lispish(`(% 3 4 5)`)).toThrow()
      expect(lispish(`(% 2 1)`)).toBe(0)
      expect(lispish(`(% 2 2)`)).toBe(0)
      expect(lispish(`(% 3 2)`)).toBe(1)
      expect(lispish(`(% 3 -2)`)).toBe(1)
      expect(lispish(`(% -3 -2)`)).toBe(-1)
      expect(lispish(`(% -3 2)`)).toBe(-1)
      expect(() => lispish(`(% 4 0)`)).toThrow()
    })
  })

  describe('sqrt', () => {
    test('samples', () => {
      expect(() => lispish(`(sqrt)`)).toThrow()
      expect(() => lispish(`(sqrt 3 4)`)).toThrow()
      expect(() => lispish(`(sqrt -3)`)).toThrow()
      expect(lispish(`(sqrt 0)`)).toBe(0)
      expect(lispish(`(sqrt 1)`)).toBe(1)
      expect(lispish(`(sqrt 4)`)).toBe(2)
    })
  })

  describe('cbrt', () => {
    test('samples', () => {
      expect(() => lispish(`(cbrt)`)).toThrow()
      expect(() => lispish(`(cbrt 3 4)`)).toThrow()
      expect(() => lispish(`(cbrt -3)`)).toThrow()
      expect(lispish(`(cbrt 0)`)).toBe(0)
      expect(lispish(`(cbrt 1)`)).toBe(1)
      expect(lispish(`(cbrt 8)`)).toBe(2)
      expect(lispish(`(cbrt 12)`)).toBe(Math.cbrt(12))
    })
  })

  describe('pow', () => {
    test('samples', () => {
      expect(() => lispish(`(pow)`)).toThrow()
      expect(() => lispish(`(pow 3)`)).toThrow()
      expect(() => lispish(`(pow 3 4 5)`)).toThrow()
      expect(lispish(`(pow 2 0)`)).toBe(1)
      expect(lispish(`(pow 2 1)`)).toBe(2)
      expect(lispish(`(pow 2 2)`)).toBe(4)
      expect(lispish(`(pow 2 3)`)).toBe(8)
      expect(lispish(`(pow 16 0.5)`)).toBe(4)
      expect(lispish(`(pow 10 -1)`)).toBe(0.1)
      expect(lispish(`(pow 10 -2)`)).toBe(0.01)
      expect(lispish(`(pow -2 -1)`)).toBe(-0.5)
      expect(lispish(`(pow -2 -2)`)).toBe(0.25)
    })
  })

  describe('round', () => {
    test('samples', () => {
      expect(() => lispish(`(round)`)).toThrow()
      expect(() => lispish(`(round 3 4 5)`)).toThrow()
      expect(lispish(`(round 0)`)).toBe(0)
      expect(lispish(`(round 1)`)).toBe(1)
      expect(lispish(`(round 0.4)`)).toBe(0)
      expect(lispish(`(round 0.5)`)).toBe(1)
      expect(lispish(`(round 0.6)`)).toBe(1)
      expect(lispish(`(round -0.4)`)).toBe(-0)
      expect(lispish(`(round -0.5)`)).toBe(-0)
      expect(lispish(`(round -0.6)`)).toBe(-1)
      expect(lispish(`(round -0.125 1)`)).toBe(-0.1)
      expect(lispish(`(round 0.125 2)`)).toBe(0.13)
    })
  })

  describe('floor', () => {
    test('samples', () => {
      expect(() => lispish(`(floor)`)).toThrow()
      expect(() => lispish(`(floor 3 4)`)).toThrow()
      expect(lispish(`(floor 0)`)).toBe(0)
      expect(lispish(`(floor 1)`)).toBe(1)
      expect(lispish(`(floor 0.4)`)).toBe(0)
      expect(lispish(`(floor 0.5)`)).toBe(0)
      expect(lispish(`(floor 0.6)`)).toBe(0)
      expect(lispish(`(floor -0.4)`)).toBe(-1)
      expect(lispish(`(floor -0.5)`)).toBe(-1)
      expect(lispish(`(floor -0.6)`)).toBe(-1)
    })
  })

  describe('ceil', () => {
    test('samples', () => {
      expect(() => lispish(`(ceil)`)).toThrow()
      expect(() => lispish(`(ceil 3 4)`)).toThrow()
      expect(lispish(`(ceil 0)`)).toBe(0)
      expect(lispish(`(ceil 1)`)).toBe(1)
      expect(lispish(`(ceil 0.4)`)).toBe(1)
      expect(lispish(`(ceil 0.5)`)).toBe(1)
      expect(lispish(`(ceil 0.6)`)).toBe(1)
      expect(lispish(`(ceil -0.4)`)).toBe(-0)
      expect(lispish(`(ceil -0.5)`)).toBe(-0)
      expect(lispish(`(ceil -0.6)`)).toBe(-0)
    })
  })

  describe('random', () => {
    test('samples', () => {
      expect(() => lispish(`(random)`)).toThrow()
      expect(() => lispish(`(random "x")`)).toThrow()
      expect(() => lispish(`(random 1 2)`)).toThrow()
      expect(lispish(`(random 0.1)`)).toBeLessThan(0.1)
      expect(lispish(`(random 0.1)`)).toBeGreaterThanOrEqual(0)
    })
  })

  describe('>', () => {
    test('samples', () => {
      expect(() => lispish(`(>)`)).toThrow()
      expect(lispish(`(> 1)`)).toBe(true)
      expect(lispish(`(> 1 2)`)).toBe(false)
      expect(lispish(`(> 1 1)`)).toBe(false)
      expect(lispish(`(> 2 1)`)).toBe(true)
      expect(lispish(`(> 2 1 2)`)).toBe(false)
      expect(lispish(`(> 2 1 0)`)).toBe(true)
      expect(() => lispish(`(> "1")`)).toThrow()
      expect(() => lispish(`(> "1" "3")`)).toThrow()
    })
  })

  describe('<', () => {
    test('samples', () => {
      expect(() => lispish(`(<)`)).toThrow()
      expect(lispish(`(< 1)`)).toBe(true)
      expect(lispish(`(< 1 2)`)).toBe(true)
      expect(lispish(`(< 1 1)`)).toBe(false)
      expect(lispish(`(< 2 1)`)).toBe(false)
      expect(lispish(`(< 1 2 1)`)).toBe(false)
      expect(lispish(`(< 0 1 2)`)).toBe(true)
      expect(() => lispish(`(< "1")`)).toThrow()
      expect(() => lispish(`(< "1" "3")`)).toThrow()
    })
  })

  describe('>=', () => {
    test('samples', () => {
      expect(() => lispish(`(>=)`)).toThrow()
      expect(lispish(`(>= 1)`)).toBe(true)
      expect(lispish(`(>= 1 2)`)).toBe(false)
      expect(lispish(`(>= 1 1)`)).toBe(true)
      expect(lispish(`(>= 2 1)`)).toBe(true)
      expect(lispish(`(>= 2 1 2)`)).toBe(false)
      expect(lispish(`(>= 2 1 1)`)).toBe(true)
      expect(() => lispish(`(>= "1")`)).toThrow()
      expect(() => lispish(`(>= "1" "3")`)).toThrow()
    })
  })

  describe('<=', () => {
    test('samples', () => {
      expect(() => lispish(`(<=)`)).toThrow()
      expect(lispish(`(<= 1)`)).toBe(true)
      expect(lispish(`(<= 1 2)`)).toBe(true)
      expect(lispish(`(<= 1 1)`)).toBe(true)
      expect(lispish(`(<= 2 1)`)).toBe(false)
      expect(lispish(`(<= 1 2 1)`)).toBe(false)
      expect(lispish(`(<= 1 2 2)`)).toBe(true)
      expect(() => lispish(`(<= "1")`)).toThrow()
      expect(() => lispish(`(<= "1" "3")`)).toThrow()
    })
  })

  describe('min', () => {
    test('samples', () => {
      expect(lispish(`(min 1)`)).toBe(1)
      expect(lispish(`(min 1 -2)`)).toBe(-2)
      expect(lispish(`(min 3 1 2 )`)).toBe(1)
      expect(() => lispish(`(min)`)).toThrow()
      expect(() => lispish(`(min "1")`)).toThrow()
      expect(() => lispish(`(min "1" "3")`)).toThrow()
    })
  })

  describe('max', () => {
    test('samples', () => {
      expect(lispish(`(max 1)`)).toBe(1)
      expect(lispish(`(max 1 -2)`)).toBe(1)
      expect(lispish(`(max 3 1 2 )`)).toBe(3)
      expect(() => lispish(`(max)`)).toThrow()
      expect(() => lispish(`(max "1")`)).toThrow()
      expect(() => lispish(`(max "1" "3")`)).toThrow()
    })
  })

  describe('e', () => {
    test('samples', () => {
      expect(lispish(`(e)`)).toBe(Math.E)
      expect(() => lispish(`(e "1")`)).toThrow()
    })
  })

  describe('pi', () => {
    test('samples', () => {
      expect(lispish(`(pi)`)).toBe(Math.PI)
      expect(() => lispish(`(pi 1)`)).toThrow()
    })
  })

  describe('abs', () => {
    test('samples', () => {
      expect(lispish(`(abs 2)`)).toBe(2)
      expect(lispish(`(abs -2)`)).toBe(2)
      expect(lispish(`(abs -0)`)).toBe(0)
      expect(() => lispish(`(abs)`)).toThrow()
      expect(() => lispish(`(abs 1 2)`)).toThrow()
    })
  })

  describe('sign', () => {
    test('samples', () => {
      expect(lispish(`(sign 2)`)).toBe(1)
      expect(lispish(`(sign -2)`)).toBe(-1)
      expect(lispish(`(sign -0)`)).toBe(-0)
      expect(lispish(`(sign 0)`)).toBe(0)
      expect(() => lispish(`(sign)`)).toThrow()
      expect(() => lispish(`(sign 1 2)`)).toThrow()
    })
  })

  describe('exp', () => {
    test('samples', () => {
      expect(lispish(`(exp 1)`)).toBe(Math.exp(1))
      expect(lispish(`(exp -2)`)).toBe(Math.exp(-2))
      expect(lispish(`(exp -0)`)).toBe(Math.exp(-0))
      expect(lispish(`(exp 0)`)).toBe(Math.exp(0))
      expect(() => lispish(`(exp)`)).toThrow()
      expect(() => lispish(`(exp 1 2)`)).toThrow()
    })
  })

  describe('log', () => {
    test('samples', () => {
      expect(lispish(`(log 0.1)`)).toBe(Math.log(0.1))
      expect(lispish(`(log 1)`)).toBe(Math.log(1))
      expect(lispish(`(log 100)`)).toBe(Math.log(100))
      expect(() => lispish(`(log -2)`)).toThrow()
      expect(() => lispish(`(log 0)`)).toThrow()
      expect(() => lispish(`(log -0)`)).toThrow()
      expect(() => lispish(`(log)`)).toThrow()
      expect(() => lispish(`(log 1 2)`)).toThrow()
    })
  })

  describe('log2', () => {
    test('samples', () => {
      expect(lispish(`(log2 0.1)`)).toBe(Math.log2(0.1))
      expect(lispish(`(log2 1)`)).toBe(Math.log2(1))
      expect(lispish(`(log2 100)`)).toBe(Math.log2(100))
      expect(() => lispish(`(log2 -2)`)).toThrow()
      expect(() => lispish(`(log2 0)`)).toThrow()
      expect(() => lispish(`(log2 -0)`)).toThrow()
      expect(() => lispish(`(log2)`)).toThrow()
      expect(() => lispish(`(log2 1 2)`)).toThrow()
    })
  })

  describe('log10', () => {
    test('samples', () => {
      expect(lispish(`(log10 0.1)`)).toBe(Math.log10(0.1))
      expect(lispish(`(log10 1)`)).toBe(Math.log10(1))
      expect(lispish(`(log10 100)`)).toBe(Math.log10(100))
      expect(() => lispish(`(log10 -2)`)).toThrow()
      expect(() => lispish(`(log10 0)`)).toThrow()
      expect(() => lispish(`(log10 -0)`)).toThrow()
      expect(() => lispish(`(log10)`)).toThrow()
      expect(() => lispish(`(log10 1 2)`)).toThrow()
    })
  })

  describe('trunc', () => {
    test('samples', () => {
      expect(lispish(`(trunc 0)`)).toBe(0)
      expect(lispish(`(trunc 0.123)`)).toBe(0)
      expect(lispish(`(trunc 0.999)`)).toBe(0)
      expect(lispish(`(trunc -0.99)`)).toBe(-0)
      expect(lispish(`(trunc -0.1)`)).toBe(-0)
      expect(() => lispish(`(trunc)`)).toThrow()
      expect(() => lispish(`(trunc 100 200)`)).toThrow()
    })
  })

  describe('sin', () => {
    test('samples', () => {
      expect(lispish(`(sin 0)`)).toBe(Math.sin(0))
      expect(lispish(`(sin 0.1)`)).toBe(Math.sin(0.1))
      expect(lispish(`(sin -0.1)`)).toBe(Math.sin(-0.1))
      expect(lispish(`(sin 1)`)).toBe(Math.sin(1))
      expect(lispish(`(sin 100)`)).toBe(Math.sin(100))
      expect(() => lispish(`(sin)`)).toThrow()
      expect(() => lispish(`(sin 1 2)`)).toThrow()
    })
  })
  describe('cos', () => {
    test('samples', () => {
      expect(lispish(`(cos 0)`)).toBe(Math.cos(0))
      expect(lispish(`(cos 0.1)`)).toBe(Math.cos(0.1))
      expect(lispish(`(cos -0.1)`)).toBe(Math.cos(-0.1))
      expect(lispish(`(cos 1)`)).toBe(Math.cos(1))
      expect(lispish(`(cos 100)`)).toBe(Math.cos(100))
      expect(() => lispish(`(cos)`)).toThrow()
      expect(() => lispish(`(cos 1 2)`)).toThrow()
    })
  })
  describe('tan', () => {
    test('samples', () => {
      expect(lispish(`(tan 0)`)).toBe(Math.tan(0))
      expect(lispish(`(tan 0.1)`)).toBe(Math.tan(0.1))
      expect(lispish(`(tan -0.1)`)).toBe(Math.tan(-0.1))
      expect(lispish(`(tan 1)`)).toBe(Math.tan(1))
      expect(lispish(`(tan 100)`)).toBe(Math.tan(100))
      expect(() => lispish(`(tan)`)).toThrow()
      expect(() => lispish(`(tan 1 2)`)).toThrow()
    })
  })

  describe('sinh', () => {
    test('samples', () => {
      expect(lispish(`(sinh 0)`)).toBe(Math.sinh(0))
      expect(lispish(`(sinh 0.1)`)).toBe(Math.sinh(0.1))
      expect(lispish(`(sinh -0.1)`)).toBe(Math.sinh(-0.1))
      expect(lispish(`(sinh 1)`)).toBe(Math.sinh(1))
      expect(lispish(`(sinh 100)`)).toBe(Math.sinh(100))
      expect(() => lispish(`(sinh)`)).toThrow()
      expect(() => lispish(`(sinh 1 2)`)).toThrow()
    })
  })
  describe('cosh', () => {
    test('samples', () => {
      expect(lispish(`(cosh 0)`)).toBe(Math.cosh(0))
      expect(lispish(`(cosh 0.1)`)).toBe(Math.cosh(0.1))
      expect(lispish(`(cosh -0.1)`)).toBe(Math.cosh(-0.1))
      expect(lispish(`(cosh 1)`)).toBe(Math.cosh(1))
      expect(lispish(`(cosh 100)`)).toBe(Math.cosh(100))
      expect(() => lispish(`(cosh)`)).toThrow()
      expect(() => lispish(`(cosh 1 2)`)).toThrow()
    })
  })
  describe('tanh', () => {
    test('samples', () => {
      expect(lispish(`(tanh 0)`)).toBe(Math.tanh(0))
      expect(lispish(`(tanh 0.1)`)).toBe(Math.tanh(0.1))
      expect(lispish(`(tanh -0.1)`)).toBe(Math.tanh(-0.1))
      expect(lispish(`(tanh 1)`)).toBe(Math.tanh(1))
      expect(lispish(`(tanh 100)`)).toBe(Math.tanh(100))
      expect(() => lispish(`(tanh)`)).toThrow()
      expect(() => lispish(`(tanh 1 2)`)).toThrow()
    })
  })

  describe('asin', () => {
    test('samples', () => {
      expect(lispish(`(asin 0)`)).toBe(Math.asin(0))
      expect(lispish(`(asin 0.1)`)).toBe(Math.asin(0.1))
      expect(lispish(`(asin -0.1)`)).toBe(Math.asin(-0.1))
      expect(lispish(`(asin 1)`)).toBe(Math.asin(1))
      expect(() => lispish(`(asin 100)`)).toThrow()
      expect(() => lispish(`(asin)`)).toThrow()
      expect(() => lispish(`(asin 1 2)`)).toThrow()
    })
  })
  describe('acos', () => {
    test('samples', () => {
      expect(lispish(`(acos 0)`)).toBe(Math.acos(0))
      expect(lispish(`(acos 0.1)`)).toBe(Math.acos(0.1))
      expect(lispish(`(acos -0.1)`)).toBe(Math.acos(-0.1))
      expect(lispish(`(acos 1)`)).toBe(Math.acos(1))
      expect(() => lispish(`(acos 100)`)).toThrow()
      expect(() => lispish(`(acos)`)).toThrow()
      expect(() => lispish(`(acos 1 2)`)).toThrow()
    })
  })
  describe('atan', () => {
    test('samples', () => {
      expect(lispish(`(atan 0)`)).toBe(Math.atan(0))
      expect(lispish(`(atan 0.1)`)).toBe(Math.atan(0.1))
      expect(lispish(`(atan -0.1)`)).toBe(Math.atan(-0.1))
      expect(lispish(`(atan 1)`)).toBe(Math.atan(1))
      expect(lispish(`(atan 100)`)).toBe(Math.atan(100))
      expect(() => lispish(`(atan)`)).toThrow()
      expect(() => lispish(`(atan 1 2)`)).toThrow()
    })
  })

  describe('asinh', () => {
    test('samples', () => {
      expect(lispish(`(asinh 0)`)).toBe(Math.asinh(0))
      expect(lispish(`(asinh 0.1)`)).toBe(Math.asinh(0.1))
      expect(lispish(`(asinh -0.1)`)).toBe(Math.asinh(-0.1))
      expect(lispish(`(asinh 1)`)).toBe(Math.asinh(1))
      expect(lispish(`(asinh 100)`)).toBe(Math.asinh(100))
      expect(() => lispish(`(asinh)`)).toThrow()
      expect(() => lispish(`(asinh 1 2)`)).toThrow()
    })
  })
  describe('acosh', () => {
    test('samples', () => {
      expect(lispish(`(acosh 1)`)).toBe(Math.acosh(1))
      expect(lispish(`(acosh 100)`)).toBe(Math.acosh(100))
      expect(() => lispish(`(acosh 0.1)`)).toThrow()
      expect(() => lispish(`(acosh -0.1)`)).toThrow()
      expect(() => lispish(`(acosh 0)`)).toThrow()
      expect(() => lispish(`(acosh)`)).toThrow()
      expect(() => lispish(`(acosh 1 2)`)).toThrow()
    })
  })
  describe('atanh', () => {
    test('samples', () => {
      expect(lispish(`(atanh 0)`)).toBe(Math.atanh(0))
      expect(lispish(`(atanh 0.1)`)).toBe(Math.atanh(0.1))
      expect(lispish(`(atanh -0.1)`)).toBe(Math.atanh(-0.1))
      expect(() => lispish(`(atanh 1)`)).toThrow()
      expect(() => lispish(`(atanh 100)`)).toThrow()
      expect(() => lispish(`(atanh)`)).toThrow()
      expect(() => lispish(`(atanh 1 2)`)).toThrow()
    })
  })
})
