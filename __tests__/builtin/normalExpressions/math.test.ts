import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`math functions`, () => {
  describe(`inc`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(inc 2.5)`)).toBe(3.5)
      expect(lispish.run(`(inc 1)`)).toBe(2)
      expect(lispish.run(`(inc 0)`)).toBe(1)
      expect(lispish.run(`(inc -1)`)).toBe(0)
      expect(lispish.run(`(inc -2.5)`)).toBe(-1.5)
      expect(() => lispish.run(`(inc)`)).toThrow()
      expect(() => lispish.run(`(inc 1 1)`)).toThrow()
      expect(() => lispish.run(`(inc "1")`)).toThrow()
      expect(() => lispish.run(`(inc false)`)).toThrow()
      expect(() => lispish.run(`(inc true)`)).toThrow()
      expect(() => lispish.run(`(inc null)`)).toThrow()
      expect(() => lispish.run(`(inc boolean)`)).toThrow()
      expect(() => lispish.run(`(inc [])`)).toThrow()
      expect(() => lispish.run(`(inc (object))`)).toThrow()
    })
  })

  describe(`inc`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(dec 2.5)`)).toBe(1.5)
      expect(lispish.run(`(dec 1)`)).toBe(0)
      expect(lispish.run(`(dec 0)`)).toBe(-1)
      expect(lispish.run(`(dec -1)`)).toBe(-2)
      expect(lispish.run(`(dec -2.5)`)).toBe(-3.5)
      expect(() => lispish.run(`(dec)`)).toThrow()
      expect(() => lispish.run(`(dec 1 1)`)).toThrow()
      expect(() => lispish.run(`(dec "1")`)).toThrow()
      expect(() => lispish.run(`(dec false)`)).toThrow()
      expect(() => lispish.run(`(dec true)`)).toThrow()
      expect(() => lispish.run(`(dec null)`)).toThrow()
      expect(() => lispish.run(`(dec boolean)`)).toThrow()
      expect(() => lispish.run(`(dec [])`)).toThrow()
      expect(() => lispish.run(`(dec (object))`)).toThrow()
    })
  })

  describe(`+`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(+)`)).toBe(0)
      expect(lispish.run(`(+ 2)`)).toBe(2)
      expect(lispish.run(`(+ 2 2)`)).toBe(4)
      expect(lispish.run(`(+ -2 2)`)).toBe(0)
      expect(lispish.run(`(+ 1 2 3 4)`)).toBe(10)
      expect(() => lispish.run(`(+ "1" 2 3 4)`)).toThrow()
    })
  })

  describe(`*`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(*)`)).toBe(1)
      expect(lispish.run(`(* 2)`)).toBe(2)
      expect(lispish.run(`(* 2 2)`)).toBe(4)
      expect(lispish.run(`(* -2 2)`)).toBe(-4)
      expect(lispish.run(`(* 1 2 3 4)`)).toBe(24)
      expect(() => lispish.run(`(* "1" 2 3 4)`)).toThrow()
    })
  })

  describe(`/`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(/)`)).toBe(1)
      expect(lispish.run(`(/ 2)`)).toBe(1 / 2)
      expect(lispish.run(`(/ 2 2)`)).toBe(2 / 2)
      expect(lispish.run(`(/ -2 2)`)).toBe(-2 / 2)
      expect(lispish.run(`(/ 1 2 3 4)`)).toBe(1 / 2 / 3 / 4)
      expect(() => lispish.run(`(/ "1" 2 3 4)`)).toThrow()
    })
  })

  describe(`-`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(-)`)).toBe(0)
      expect(lispish.run(`(- 2)`)).toBe(-2)
      expect(lispish.run(`(- 2 2)`)).toBe(2 - 2)
      expect(lispish.run(`(- -2 2)`)).toBe(-2 - 2)
      expect(lispish.run(`(- 1 2 3 4)`)).toBe(1 - 2 - 3 - 4)
      expect(() => lispish.run(`(- "1" 2 3 4)`)).toThrow()
    })
  })

  describe(`mod`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(mod)`)).toThrow()
      expect(() => lispish.run(`(mod 3)`)).toThrow()
      expect(() => lispish.run(`(mod 3 4 5)`)).toThrow()
      expect(lispish.run(`(mod 2 1)`)).toBe(0)
      expect(lispish.run(`(mod 2 2)`)).toBe(0)
      expect(lispish.run(`(mod 3 2)`)).toBe(1)
      expect(lispish.run(`(mod 3 -2)`)).toBe(1)
      expect(lispish.run(`(mod -3 -2)`)).toBe(-1)
      expect(lispish.run(`(mod -3 2)`)).toBe(-1)
      expect(() => lispish.run(`(mod 4 0)`)).toThrow()
    })
  })

  describe(`sqrt`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(sqrt)`)).toThrow()
      expect(() => lispish.run(`(sqrt 3 4)`)).toThrow()
      expect(() => lispish.run(`(sqrt -3)`)).toThrow()
      expect(lispish.run(`(sqrt 0)`)).toBe(0)
      expect(lispish.run(`(sqrt 1)`)).toBe(1)
      expect(lispish.run(`(sqrt 4)`)).toBe(2)
    })
  })

  describe(`cbrt`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(cbrt)`)).toThrow()
      expect(() => lispish.run(`(cbrt 3 4)`)).toThrow()
      expect(() => lispish.run(`(cbrt -3)`)).toThrow()
      expect(lispish.run(`(cbrt 0)`)).toBe(0)
      expect(lispish.run(`(cbrt 1)`)).toBe(1)
      expect(lispish.run(`(cbrt 8)`)).toBe(2)
      expect(lispish.run(`(cbrt 12)`)).toBe(Math.cbrt(12))
    })
  })

  describe(`pow`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(pow)`)).toThrow()
      expect(() => lispish.run(`(pow 3)`)).toThrow()
      expect(() => lispish.run(`(pow 3 4 5)`)).toThrow()
      expect(lispish.run(`(pow 2 0)`)).toBe(1)
      expect(lispish.run(`(pow 2 1)`)).toBe(2)
      expect(lispish.run(`(pow 2 2)`)).toBe(4)
      expect(lispish.run(`(pow 2 3)`)).toBe(8)
      expect(lispish.run(`(pow 16 0.5)`)).toBe(4)
      expect(lispish.run(`(pow 10 -1)`)).toBe(0.1)
      expect(lispish.run(`(pow 10 -2)`)).toBe(0.01)
      expect(lispish.run(`(pow -2 -1)`)).toBe(-0.5)
      expect(lispish.run(`(pow -2 -2)`)).toBe(0.25)
    })
  })

  describe(`round`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(round)`)).toThrow()
      expect(() => lispish.run(`(round 3 4 5)`)).toThrow()
      expect(lispish.run(`(round 0)`)).toBe(0)
      expect(lispish.run(`(round 1)`)).toBe(1)
      expect(lispish.run(`(round 0.4)`)).toBe(0)
      expect(lispish.run(`(round 0.5)`)).toBe(1)
      expect(lispish.run(`(round 0.6)`)).toBe(1)
      expect(lispish.run(`(round -0.4)`)).toBe(-0)
      expect(lispish.run(`(round -0.5)`)).toBe(-0)
      expect(lispish.run(`(round -0.6)`)).toBe(-1)
      expect(lispish.run(`(round -0.125 1)`)).toBe(-0.1)
      expect(lispish.run(`(round 0.125 2)`)).toBe(0.13)
    })
  })

  describe(`floor`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(floor)`)).toThrow()
      expect(() => lispish.run(`(floor 3 4)`)).toThrow()
      expect(lispish.run(`(floor 0)`)).toBe(0)
      expect(lispish.run(`(floor 1)`)).toBe(1)
      expect(lispish.run(`(floor 0.4)`)).toBe(0)
      expect(lispish.run(`(floor 0.5)`)).toBe(0)
      expect(lispish.run(`(floor 0.6)`)).toBe(0)
      expect(lispish.run(`(floor -0.4)`)).toBe(-1)
      expect(lispish.run(`(floor -0.5)`)).toBe(-1)
      expect(lispish.run(`(floor -0.6)`)).toBe(-1)
    })
  })

  describe(`ceil`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(ceil)`)).toThrow()
      expect(() => lispish.run(`(ceil 3 4)`)).toThrow()
      expect(lispish.run(`(ceil 0)`)).toBe(0)
      expect(lispish.run(`(ceil 1)`)).toBe(1)
      expect(lispish.run(`(ceil 0.4)`)).toBe(1)
      expect(lispish.run(`(ceil 0.5)`)).toBe(1)
      expect(lispish.run(`(ceil 0.6)`)).toBe(1)
      expect(lispish.run(`(ceil -0.4)`)).toBe(-0)
      expect(lispish.run(`(ceil -0.5)`)).toBe(-0)
      expect(lispish.run(`(ceil -0.6)`)).toBe(-0)
    })
  })

  describe(`rand`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(rand)`)).toBeLessThan(1)
      expect(lispish.run(`(rand 0.1)`)).toBeLessThan(0.1)
      expect(lispish.run(`(rand 0.1)`)).toBeGreaterThanOrEqual(0)
      expect(() => lispish.run(`(rand "x")`)).toThrow()
      expect(() => lispish.run(`(rand 1 2)`)).toThrow()
    })
  })

  describe(`rand-int`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(rand-int 1)`)).toBe(0)
      expect(lispish.run(`(rand-int 2)`)).toBeLessThan(2)
      expect(lispish.run(`(rand-int 20)`)).toBeLessThan(20)
      expect(lispish.run(`(rand-int 10.1)`)).toBeLessThan(10.1)
      expect(() => lispish.run(`(rand-int "x")`)).toThrow()
      expect(() => lispish.run(`(rand-int 1 2)`)).toThrow()
    })
  })

  describe(`>`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(>)`)).toThrow()
      expect(lispish.run(`(> 1)`)).toBe(true)
      expect(lispish.run(`(> 1 2)`)).toBe(false)
      expect(lispish.run(`(> 1 1)`)).toBe(false)
      expect(lispish.run(`(> 2 1)`)).toBe(true)
      expect(lispish.run(`(> 2 1 2)`)).toBe(false)
      expect(lispish.run(`(> 2 1 0)`)).toBe(true)
      expect(() => lispish.run(`(> "1")`)).toThrow()
      expect(() => lispish.run(`(> "1" "3")`)).toThrow()
    })
  })

  describe(`<`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(<)`)).toThrow()
      expect(lispish.run(`(< 1)`)).toBe(true)
      expect(lispish.run(`(< 1 2)`)).toBe(true)
      expect(lispish.run(`(< 1 1)`)).toBe(false)
      expect(lispish.run(`(< 2 1)`)).toBe(false)
      expect(lispish.run(`(< 1 2 1)`)).toBe(false)
      expect(lispish.run(`(< 0 1 2)`)).toBe(true)
      expect(() => lispish.run(`(< "1")`)).toThrow()
      expect(() => lispish.run(`(< "1" "3")`)).toThrow()
    })
  })

  describe(`>=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(>=)`)).toThrow()
      expect(lispish.run(`(>= 1)`)).toBe(true)
      expect(lispish.run(`(>= 1 2)`)).toBe(false)
      expect(lispish.run(`(>= 1 1)`)).toBe(true)
      expect(lispish.run(`(>= 2 1)`)).toBe(true)
      expect(lispish.run(`(>= 2 1 2)`)).toBe(false)
      expect(lispish.run(`(>= 2 1 1)`)).toBe(true)
      expect(() => lispish.run(`(>= "1")`)).toThrow()
      expect(() => lispish.run(`(>= "1" "3")`)).toThrow()
    })
  })

  describe(`<=`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(<=)`)).toThrow()
      expect(lispish.run(`(<= 1)`)).toBe(true)
      expect(lispish.run(`(<= 1 2)`)).toBe(true)
      expect(lispish.run(`(<= 1 1)`)).toBe(true)
      expect(lispish.run(`(<= 2 1)`)).toBe(false)
      expect(lispish.run(`(<= 1 2 1)`)).toBe(false)
      expect(lispish.run(`(<= 1 2 2)`)).toBe(true)
      expect(() => lispish.run(`(<= "1")`)).toThrow()
      expect(() => lispish.run(`(<= "1" "3")`)).toThrow()
    })
  })

  describe(`min`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(min 1)`)).toBe(1)
      expect(lispish.run(`(min 1 -2)`)).toBe(-2)
      expect(lispish.run(`(min 3 1 2 )`)).toBe(1)
      expect(() => lispish.run(`(min)`)).toThrow()
      expect(() => lispish.run(`(min "1")`)).toThrow()
      expect(() => lispish.run(`(min "1" "3")`)).toThrow()
    })
  })

  describe(`max`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(max 1)`)).toBe(1)
      expect(lispish.run(`(max 1 -2)`)).toBe(1)
      expect(lispish.run(`(max 3 1 2 )`)).toBe(3)
      expect(() => lispish.run(`(max)`)).toThrow()
      expect(() => lispish.run(`(max "1")`)).toThrow()
      expect(() => lispish.run(`(max "1" "3")`)).toThrow()
    })
  })

  describe(`e`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(e)`)).toBe(Math.E)
      expect(() => lispish.run(`(e "1")`)).toThrow()
    })
  })

  describe(`pi`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(pi)`)).toBe(Math.PI)
      expect(() => lispish.run(`(pi 1)`)).toThrow()
    })
  })

  describe(`abs`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(abs 2)`)).toBe(2)
      expect(lispish.run(`(abs -2)`)).toBe(2)
      expect(lispish.run(`(abs -0)`)).toBe(0)
      expect(() => lispish.run(`(abs)`)).toThrow()
      expect(() => lispish.run(`(abs 1 2)`)).toThrow()
    })
  })

  describe(`sign`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(sign 2)`)).toBe(1)
      expect(lispish.run(`(sign -2)`)).toBe(-1)
      expect(lispish.run(`(sign -0)`)).toBe(-0)
      expect(lispish.run(`(sign 0)`)).toBe(0)
      expect(() => lispish.run(`(sign)`)).toThrow()
      expect(() => lispish.run(`(sign 1 2)`)).toThrow()
    })
  })

  describe(`exp`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(exp 1)`)).toBe(Math.exp(1))
      expect(lispish.run(`(exp -2)`)).toBe(Math.exp(-2))
      expect(lispish.run(`(exp -0)`)).toBe(Math.exp(-0))
      expect(lispish.run(`(exp 0)`)).toBe(Math.exp(0))
      expect(() => lispish.run(`(exp)`)).toThrow()
      expect(() => lispish.run(`(exp 1 2)`)).toThrow()
    })
  })

  describe(`log`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(log 0.1)`)).toBe(Math.log(0.1))
      expect(lispish.run(`(log 1)`)).toBe(Math.log(1))
      expect(lispish.run(`(log 100)`)).toBe(Math.log(100))
      expect(() => lispish.run(`(log -2)`)).toThrow()
      expect(() => lispish.run(`(log 0)`)).toThrow()
      expect(() => lispish.run(`(log -0)`)).toThrow()
      expect(() => lispish.run(`(log)`)).toThrow()
      expect(() => lispish.run(`(log 1 2)`)).toThrow()
    })
  })

  describe(`log2`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(log2 0.1)`)).toBe(Math.log2(0.1))
      expect(lispish.run(`(log2 1)`)).toBe(Math.log2(1))
      expect(lispish.run(`(log2 100)`)).toBe(Math.log2(100))
      expect(() => lispish.run(`(log2 -2)`)).toThrow()
      expect(() => lispish.run(`(log2 0)`)).toThrow()
      expect(() => lispish.run(`(log2 -0)`)).toThrow()
      expect(() => lispish.run(`(log2)`)).toThrow()
      expect(() => lispish.run(`(log2 1 2)`)).toThrow()
    })
  })

  describe(`log10`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(log10 0.1)`)).toBe(Math.log10(0.1))
      expect(lispish.run(`(log10 1)`)).toBe(Math.log10(1))
      expect(lispish.run(`(log10 100)`)).toBe(Math.log10(100))
      expect(() => lispish.run(`(log10 -2)`)).toThrow()
      expect(() => lispish.run(`(log10 0)`)).toThrow()
      expect(() => lispish.run(`(log10 -0)`)).toThrow()
      expect(() => lispish.run(`(log10)`)).toThrow()
      expect(() => lispish.run(`(log10 1 2)`)).toThrow()
    })
  })

  describe(`trunc`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(trunc 0)`)).toBe(0)
      expect(lispish.run(`(trunc 0.123)`)).toBe(0)
      expect(lispish.run(`(trunc 0.999)`)).toBe(0)
      expect(lispish.run(`(trunc -0.99)`)).toBe(-0)
      expect(lispish.run(`(trunc -0.1)`)).toBe(-0)
      expect(() => lispish.run(`(trunc)`)).toThrow()
      expect(() => lispish.run(`(trunc 100 200)`)).toThrow()
    })
  })

  describe(`sin`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(sin 0)`)).toBe(Math.sin(0))
      expect(lispish.run(`(sin 0.1)`)).toBe(Math.sin(0.1))
      expect(lispish.run(`(sin -0.1)`)).toBe(Math.sin(-0.1))
      expect(lispish.run(`(sin 1)`)).toBe(Math.sin(1))
      expect(lispish.run(`(sin 100)`)).toBe(Math.sin(100))
      expect(() => lispish.run(`(sin)`)).toThrow()
      expect(() => lispish.run(`(sin 1 2)`)).toThrow()
    })
  })
  describe(`cos`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(cos 0)`)).toBe(Math.cos(0))
      expect(lispish.run(`(cos 0.1)`)).toBe(Math.cos(0.1))
      expect(lispish.run(`(cos -0.1)`)).toBe(Math.cos(-0.1))
      expect(lispish.run(`(cos 1)`)).toBe(Math.cos(1))
      expect(lispish.run(`(cos 100)`)).toBe(Math.cos(100))
      expect(() => lispish.run(`(cos)`)).toThrow()
      expect(() => lispish.run(`(cos 1 2)`)).toThrow()
    })
  })
  describe(`tan`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(tan 0)`)).toBe(Math.tan(0))
      expect(lispish.run(`(tan 0.1)`)).toBe(Math.tan(0.1))
      expect(lispish.run(`(tan -0.1)`)).toBe(Math.tan(-0.1))
      expect(lispish.run(`(tan 1)`)).toBe(Math.tan(1))
      expect(lispish.run(`(tan 100)`)).toBe(Math.tan(100))
      expect(() => lispish.run(`(tan)`)).toThrow()
      expect(() => lispish.run(`(tan 1 2)`)).toThrow()
    })
  })

  describe(`sinh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(sinh 0)`)).toBe(Math.sinh(0))
      expect(lispish.run(`(sinh 0.1)`)).toBe(Math.sinh(0.1))
      expect(lispish.run(`(sinh -0.1)`)).toBe(Math.sinh(-0.1))
      expect(lispish.run(`(sinh 1)`)).toBe(Math.sinh(1))
      expect(lispish.run(`(sinh 100)`)).toBe(Math.sinh(100))
      expect(() => lispish.run(`(sinh)`)).toThrow()
      expect(() => lispish.run(`(sinh 1 2)`)).toThrow()
    })
  })
  describe(`cosh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(cosh 0)`)).toBe(Math.cosh(0))
      expect(lispish.run(`(cosh 0.1)`)).toBe(Math.cosh(0.1))
      expect(lispish.run(`(cosh -0.1)`)).toBe(Math.cosh(-0.1))
      expect(lispish.run(`(cosh 1)`)).toBe(Math.cosh(1))
      expect(lispish.run(`(cosh 100)`)).toBe(Math.cosh(100))
      expect(() => lispish.run(`(cosh)`)).toThrow()
      expect(() => lispish.run(`(cosh 1 2)`)).toThrow()
    })
  })
  describe(`tanh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(tanh 0)`)).toBe(Math.tanh(0))
      expect(lispish.run(`(tanh 0.1)`)).toBe(Math.tanh(0.1))
      expect(lispish.run(`(tanh -0.1)`)).toBe(Math.tanh(-0.1))
      expect(lispish.run(`(tanh 1)`)).toBe(Math.tanh(1))
      expect(lispish.run(`(tanh 100)`)).toBe(Math.tanh(100))
      expect(() => lispish.run(`(tanh)`)).toThrow()
      expect(() => lispish.run(`(tanh 1 2)`)).toThrow()
    })
  })

  describe(`asin`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(asin 0)`)).toBe(Math.asin(0))
      expect(lispish.run(`(asin 0.1)`)).toBe(Math.asin(0.1))
      expect(lispish.run(`(asin -0.1)`)).toBe(Math.asin(-0.1))
      expect(lispish.run(`(asin 1)`)).toBe(Math.asin(1))
      expect(() => lispish.run(`(asin 100)`)).toThrow()
      expect(() => lispish.run(`(asin)`)).toThrow()
      expect(() => lispish.run(`(asin 1 2)`)).toThrow()
    })
  })
  describe(`acos`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(acos 0)`)).toBe(Math.acos(0))
      expect(lispish.run(`(acos 0.1)`)).toBe(Math.acos(0.1))
      expect(lispish.run(`(acos -0.1)`)).toBe(Math.acos(-0.1))
      expect(lispish.run(`(acos 1)`)).toBe(Math.acos(1))
      expect(() => lispish.run(`(acos 100)`)).toThrow()
      expect(() => lispish.run(`(acos)`)).toThrow()
      expect(() => lispish.run(`(acos 1 2)`)).toThrow()
    })
  })
  describe(`atan`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(atan 0)`)).toBe(Math.atan(0))
      expect(lispish.run(`(atan 0.1)`)).toBe(Math.atan(0.1))
      expect(lispish.run(`(atan -0.1)`)).toBe(Math.atan(-0.1))
      expect(lispish.run(`(atan 1)`)).toBe(Math.atan(1))
      expect(lispish.run(`(atan 100)`)).toBe(Math.atan(100))
      expect(() => lispish.run(`(atan)`)).toThrow()
      expect(() => lispish.run(`(atan 1 2)`)).toThrow()
    })
  })

  describe(`asinh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(asinh 0)`)).toBe(Math.asinh(0))
      expect(lispish.run(`(asinh 0.1)`)).toBe(Math.asinh(0.1))
      expect(lispish.run(`(asinh -0.1)`)).toBe(Math.asinh(-0.1))
      expect(lispish.run(`(asinh 1)`)).toBe(Math.asinh(1))
      expect(lispish.run(`(asinh 100)`)).toBe(Math.asinh(100))
      expect(() => lispish.run(`(asinh)`)).toThrow()
      expect(() => lispish.run(`(asinh 1 2)`)).toThrow()
    })
  })
  describe(`acosh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(acosh 1)`)).toBe(Math.acosh(1))
      expect(lispish.run(`(acosh 100)`)).toBe(Math.acosh(100))
      expect(() => lispish.run(`(acosh 0.1)`)).toThrow()
      expect(() => lispish.run(`(acosh -0.1)`)).toThrow()
      expect(() => lispish.run(`(acosh 0)`)).toThrow()
      expect(() => lispish.run(`(acosh)`)).toThrow()
      expect(() => lispish.run(`(acosh 1 2)`)).toThrow()
    })
  })
  describe(`atanh`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(atanh 0)`)).toBe(Math.atanh(0))
      expect(lispish.run(`(atanh 0.1)`)).toBe(Math.atanh(0.1))
      expect(lispish.run(`(atanh -0.1)`)).toBe(Math.atanh(-0.1))
      expect(() => lispish.run(`(atanh 1)`)).toThrow()
      expect(() => lispish.run(`(atanh 100)`)).toThrow()
      expect(() => lispish.run(`(atanh)`)).toThrow()
      expect(() => lispish.run(`(atanh 1 2)`)).toThrow()
    })
  })
})
