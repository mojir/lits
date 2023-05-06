import { Lits } from '../../../src'

describe(`math functions`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe(`inc`, () => {
      test(`samples`, () => {
        expect(lits.run(`(inc 2.5)`)).toBe(3.5)
        expect(lits.run(`(inc 1)`)).toBe(2)
        expect(lits.run(`(inc 0)`)).toBe(1)
        expect(lits.run(`(inc -1)`)).toBe(0)
        expect(lits.run(`(inc -2.5)`)).toBe(-1.5)
        expect(() => lits.run(`(inc)`)).toThrow()
        expect(() => lits.run(`(inc 1 1)`)).toThrow()
        expect(() => lits.run(`(inc :1)`)).toThrow()
        expect(() => lits.run(`(inc false)`)).toThrow()
        expect(() => lits.run(`(inc true)`)).toThrow()
        expect(() => lits.run(`(inc nil)`)).toThrow()
        expect(() => lits.run(`(inc boolean)`)).toThrow()
        expect(() => lits.run(`(inc [])`)).toThrow()
        expect(() => lits.run(`(inc (object))`)).toThrow()
      })
    })

    describe(`inc`, () => {
      test(`samples`, () => {
        expect(lits.run(`(dec 2.5)`)).toBe(1.5)
        expect(lits.run(`(dec 1)`)).toBe(0)
        expect(lits.run(`(dec 0)`)).toBe(-1)
        expect(lits.run(`(dec -1)`)).toBe(-2)
        expect(lits.run(`(dec -2.5)`)).toBe(-3.5)
        expect(() => lits.run(`(dec)`)).toThrow()
        expect(() => lits.run(`(dec 1 1)`)).toThrow()
        expect(() => lits.run(`(dec :1)`)).toThrow()
        expect(() => lits.run(`(dec false)`)).toThrow()
        expect(() => lits.run(`(dec true)`)).toThrow()
        expect(() => lits.run(`(dec nil)`)).toThrow()
        expect(() => lits.run(`(dec boolean)`)).toThrow()
        expect(() => lits.run(`(dec [])`)).toThrow()
        expect(() => lits.run(`(dec (object))`)).toThrow()
      })
    })

    describe(`+`, () => {
      test(`samples`, () => {
        expect(lits.run(`(+)`)).toBe(0)
        expect(lits.run(`(+ 2)`)).toBe(2)
        expect(lits.run(`(+ 2 2)`)).toBe(4)
        expect(lits.run(`(+ -2 2)`)).toBe(0)
        expect(lits.run(`(+ 1 2 3 4)`)).toBe(10)
        expect(() => lits.run(`(+ :1 2 3 4)`)).toThrow()
      })
    })

    describe(`*`, () => {
      test(`samples`, () => {
        expect(lits.run(`(*)`)).toBe(1)
        expect(lits.run(`(* 2)`)).toBe(2)
        expect(lits.run(`(* 2 2)`)).toBe(4)
        expect(lits.run(`(* -2 2)`)).toBe(-4)
        expect(lits.run(`(* 1 2 3 4)`)).toBe(24)
        expect(() => lits.run(`(* :1 2 3 4)`)).toThrow()
      })
    })

    describe(`/`, () => {
      test(`samples`, () => {
        expect(lits.run(`(/)`)).toBe(1)
        expect(lits.run(`(/ 2)`)).toBe(1 / 2)
        expect(lits.run(`(/ 2 2)`)).toBe(2 / 2)
        expect(lits.run(`(/ -2 2)`)).toBe(-2 / 2)
        expect(lits.run(`(/ 1 2 3 4)`)).toBe(1 / 2 / 3 / 4)
        expect(() => lits.run(`(/ :1 2 3 4)`)).toThrow()
      })
    })

    describe(`-`, () => {
      test(`samples`, () => {
        expect(lits.run(`(-)`)).toBe(0)
        expect(lits.run(`(- 2)`)).toBe(-2)
        expect(lits.run(`(- 2 2)`)).toBe(2 - 2)
        expect(lits.run(`(- -2 2)`)).toBe(-2 - 2)
        expect(lits.run(`(- 1 2 3 4)`)).toBe(1 - 2 - 3 - 4)
        expect(() => lits.run(`(- :1 2 3 4)`)).toThrow()
      })
      test(`strange bug`, () => {
        expect(lits.run(`(def a 0) (def b 2) (- a b)`)).toBe(-2)
      })
    })

    describe(`sqrt`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(sqrt)`)).toThrow()
        expect(() => lits.run(`(sqrt 3 4)`)).toThrow()
        expect(lits.run(`(sqrt -3)`)).toBeNaN()
        expect(lits.run(`(sqrt 0)`)).toBe(0)
        expect(lits.run(`(sqrt 1)`)).toBe(1)
        expect(lits.run(`(sqrt 4)`)).toBe(2)
      })
    })

    describe(`cbrt`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(cbrt)`)).toThrow()
        expect(() => lits.run(`(cbrt 3 4)`)).toThrow()
        expect(lits.run(`(cbrt -8)`)).toBe(-2)
        expect(lits.run(`(cbrt 0)`)).toBe(0)
        expect(lits.run(`(cbrt 1)`)).toBe(1)
        expect(lits.run(`(cbrt 8)`)).toBe(2)
        expect(lits.run(`(cbrt 12)`)).toBe(Math.cbrt(12))
      })
    })

    describe(`pow`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(pow)`)).toThrow()
        expect(() => lits.run(`(pow 3)`)).toThrow()
        expect(() => lits.run(`(pow 3 4 5)`)).toThrow()
        expect(lits.run(`(pow 2 0)`)).toBe(1)
        expect(lits.run(`(pow 2 1)`)).toBe(2)
        expect(lits.run(`(pow 2 2)`)).toBe(4)
        expect(lits.run(`(pow 2 3)`)).toBe(8)
        expect(lits.run(`(pow 16 0.5)`)).toBe(4)
        expect(lits.run(`(pow 10 -1)`)).toBe(0.1)
        expect(lits.run(`(pow 10 -2)`)).toBe(0.01)
        expect(lits.run(`(pow -2 -1)`)).toBe(-0.5)
        expect(lits.run(`(pow -2 -2)`)).toBe(0.25)
      })
    })

    describe(`round`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(round)`)).toThrow()
        expect(() => lits.run(`(round 3 4 5)`)).toThrow()
        expect(lits.run(`(round 0)`)).toBe(0)
        expect(lits.run(`(round 1)`)).toBe(1)
        expect(lits.run(`(round 0.4)`)).toBe(0)
        expect(lits.run(`(round 0.5)`)).toBe(1)
        expect(lits.run(`(round 0.6)`)).toBe(1)
        expect(lits.run(`(round -0.4)`)).toBe(-0)
        expect(lits.run(`(round -0.5)`)).toBe(-0)
        expect(lits.run(`(round -0.6)`)).toBe(-1)
        expect(lits.run(`(round -0.125 1)`)).toBe(-0.1)
        expect(lits.run(`(round 0.125 2)`)).toBe(0.13)
      })
    })

    describe(`floor`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(floor)`)).toThrow()
        expect(() => lits.run(`(floor 3 4)`)).toThrow()
        expect(lits.run(`(floor 0)`)).toBe(0)
        expect(lits.run(`(floor 1)`)).toBe(1)
        expect(lits.run(`(floor 0.4)`)).toBe(0)
        expect(lits.run(`(floor 0.5)`)).toBe(0)
        expect(lits.run(`(floor 0.6)`)).toBe(0)
        expect(lits.run(`(floor -0.4)`)).toBe(-1)
        expect(lits.run(`(floor -0.5)`)).toBe(-1)
        expect(lits.run(`(floor -0.6)`)).toBe(-1)
      })
    })

    describe(`ceil`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(ceil)`)).toThrow()
        expect(() => lits.run(`(ceil 3 4)`)).toThrow()
        expect(lits.run(`(ceil 0)`)).toBe(0)
        expect(lits.run(`(ceil 1)`)).toBe(1)
        expect(lits.run(`(ceil 0.4)`)).toBe(1)
        expect(lits.run(`(ceil 0.5)`)).toBe(1)
        expect(lits.run(`(ceil 0.6)`)).toBe(1)
        expect(lits.run(`(ceil -0.4)`)).toBe(-0)
        expect(lits.run(`(ceil -0.5)`)).toBe(-0)
        expect(lits.run(`(ceil -0.6)`)).toBe(-0)
      })
    })

    describe(`rand!`, () => {
      test(`samples`, () => {
        expect(lits.run(`(rand!)`)).toBeLessThan(1)
        expect(lits.run(`(rand! 0.1)`)).toBeLessThan(0.1)
        expect(lits.run(`(rand! 0.1)`)).toBeGreaterThanOrEqual(0)
        expect(() => lits.run(`(rand! :x)`)).toThrow()
        expect(() => lits.run(`(rand! 1 2)`)).toThrow()
      })
    })

    describe(`rand-int!`, () => {
      test(`samples`, () => {
        expect(lits.run(`(rand-int! 1)`)).toBe(0)
        expect(lits.run(`(rand-int! 2)`)).toBeLessThan(2)
        expect(lits.run(`(rand-int! 20)`)).toBeLessThan(20)
        expect(lits.run(`(rand-int! 10.1)`)).toBeLessThan(10.1)
        expect(() => lits.run(`(rand-int! :x)`)).toThrow()
        expect(() => lits.run(`(rand-int! 1 2)`)).toThrow()
      })
    })

    describe(`min`, () => {
      test(`samples`, () => {
        expect(lits.run(`(min 1)`)).toBe(1)
        expect(lits.run(`(min 1 -2)`)).toBe(-2)
        expect(lits.run(`(min 3 1 2 )`)).toBe(1)
        expect(() => lits.run(`(min)`)).toThrow()
        expect(() => lits.run(`(min :1)`)).toThrow()
        expect(() => lits.run(`(min :1 :3)`)).toThrow()
      })
    })

    describe(`max`, () => {
      test(`samples`, () => {
        expect(lits.run(`(max 1)`)).toBe(1)
        expect(lits.run(`(max 1 -2)`)).toBe(1)
        expect(lits.run(`(max 3 1 2 )`)).toBe(3)
        expect(() => lits.run(`(max)`)).toThrow()
        expect(() => lits.run(`(max :1)`)).toThrow()
        expect(() => lits.run(`(max :1 :3)`)).toThrow()
      })
    })

    describe(`e`, () => {
      test(`samples`, () => {
        expect(lits.run(`(e)`)).toBe(Math.E)
        expect(() => lits.run(`(e :1)`)).toThrow()
      })
    })

    describe(`max-safe-integer`, () => {
      test(`samples`, () => {
        expect(lits.run(`(max-safe-integer)`)).toBe(Number.MAX_SAFE_INTEGER)
        expect(() => lits.run(`(max-safe-integer :1)`)).toThrow()
      })
    })

    describe(`min-safe-integer`, () => {
      test(`samples`, () => {
        expect(lits.run(`(min-safe-integer)`)).toBe(Number.MIN_SAFE_INTEGER)
        expect(() => lits.run(`(min-safe-integer :1)`)).toThrow()
      })
    })

    describe(`max-value`, () => {
      test(`samples`, () => {
        expect(lits.run(`(max-value)`)).toBe(Number.MAX_VALUE)
        expect(() => lits.run(`(max-value :1)`)).toThrow()
      })
    })

    describe(`min-value`, () => {
      test(`samples`, () => {
        expect(lits.run(`(min-value)`)).toBe(Number.MIN_VALUE)
        expect(() => lits.run(`(min-value :1)`)).toThrow()
      })
    })

    describe(`epsilon`, () => {
      test(`samples`, () => {
        expect(lits.run(`(epsilon)`)).toBe(Number.EPSILON)
        expect(() => lits.run(`(epsilon :1)`)).toThrow()
      })
    })

    describe(`nan`, () => {
      test(`samples`, () => {
        expect(lits.run(`(nan)`)).toBeNaN()
        expect(() => lits.run(`(nan :1)`)).toThrow()
      })
    })

    describe(`positive-infinity`, () => {
      test(`samples`, () => {
        expect(lits.run(`(positive-infinity)`)).toBe(Number.POSITIVE_INFINITY)
        expect(() => lits.run(`(positive-infinity :1)`)).toThrow()
      })
    })

    describe(`negative-infinity`, () => {
      test(`samples`, () => {
        expect(lits.run(`(negative-infinity)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(() => lits.run(`(negative-infinity :1)`)).toThrow()
      })
    })

    describe(`pi`, () => {
      test(`samples`, () => {
        expect(lits.run(`(pi)`)).toBe(Math.PI)
        expect(() => lits.run(`(pi 1)`)).toThrow()
      })
    })

    describe(`abs`, () => {
      test(`samples`, () => {
        expect(lits.run(`(abs 2)`)).toBe(2)
        expect(lits.run(`(abs -2)`)).toBe(2)
        expect(lits.run(`(abs -0)`)).toBe(0)
        expect(() => lits.run(`(abs)`)).toThrow()
        expect(() => lits.run(`(abs 1 2)`)).toThrow()
      })
    })

    describe(`sign`, () => {
      test(`samples`, () => {
        expect(lits.run(`(sign 2)`)).toBe(1)
        expect(lits.run(`(sign -2)`)).toBe(-1)
        expect(lits.run(`(sign -0)`)).toBe(-0)
        expect(lits.run(`(sign 0)`)).toBe(0)
        expect(() => lits.run(`(sign)`)).toThrow()
        expect(() => lits.run(`(sign 1 2)`)).toThrow()
      })
    })

    describe(`exp`, () => {
      test(`samples`, () => {
        expect(lits.run(`(exp 1)`)).toBe(Math.exp(1))
        expect(lits.run(`(exp -2)`)).toBe(Math.exp(-2))
        expect(lits.run(`(exp -0)`)).toBe(Math.exp(-0))
        expect(lits.run(`(exp 0)`)).toBe(Math.exp(0))
        expect(() => lits.run(`(exp)`)).toThrow()
        expect(() => lits.run(`(exp 1 2)`)).toThrow()
      })
    })

    describe(`log`, () => {
      test(`samples`, () => {
        expect(lits.run(`(log 0.1)`)).toBe(Math.log(0.1))
        expect(lits.run(`(log 1)`)).toBe(Math.log(1))
        expect(lits.run(`(log 100)`)).toBe(Math.log(100))
        expect(lits.run(`(log -2)`)).toBeNaN()
        expect(lits.run(`(log 0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(lits.run(`(log -0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(() => lits.run(`(log)`)).toThrow()
        expect(() => lits.run(`(log 1 2)`)).toThrow()
      })
    })

    describe(`log2`, () => {
      test(`samples`, () => {
        expect(lits.run(`(log2 0.1)`)).toBe(Math.log2(0.1))
        expect(lits.run(`(log2 1)`)).toBe(Math.log2(1))
        expect(lits.run(`(log2 100)`)).toBe(Math.log2(100))
        expect(lits.run(`(log2 -2)`)).toBeNaN()
        expect(lits.run(`(log2 0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(lits.run(`(log2 -0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(() => lits.run(`(log2)`)).toThrow()
        expect(() => lits.run(`(log2 1 2)`)).toThrow()
      })
    })

    describe(`log10`, () => {
      test(`samples`, () => {
        expect(lits.run(`(log10 0.1)`)).toBe(Math.log10(0.1))
        expect(lits.run(`(log10 1)`)).toBe(Math.log10(1))
        expect(lits.run(`(log10 100)`)).toBe(Math.log10(100))
        expect(lits.run(`(log10 -2)`)).toBeNaN()
        expect(lits.run(`(log10 0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(lits.run(`(log10 -0)`)).toBe(Number.NEGATIVE_INFINITY)
        expect(() => lits.run(`(log10)`)).toThrow()
        expect(() => lits.run(`(log10 1 2)`)).toThrow()
      })
    })

    describe(`trunc`, () => {
      test(`samples`, () => {
        expect(lits.run(`(trunc 0)`)).toBe(0)
        expect(lits.run(`(trunc 0.123)`)).toBe(0)
        expect(lits.run(`(trunc 0.999)`)).toBe(0)
        expect(lits.run(`(trunc -0.99)`)).toBe(-0)
        expect(lits.run(`(trunc -0.1)`)).toBe(-0)
        expect(() => lits.run(`(trunc)`)).toThrow()
        expect(() => lits.run(`(trunc 100 200)`)).toThrow()
      })
    })

    describe(`sin`, () => {
      test(`samples`, () => {
        expect(lits.run(`(sin 0)`)).toBe(Math.sin(0))
        expect(lits.run(`(sin 0.1)`)).toBe(Math.sin(0.1))
        expect(lits.run(`(sin -0.1)`)).toBe(Math.sin(-0.1))
        expect(lits.run(`(sin 1)`)).toBe(Math.sin(1))
        expect(lits.run(`(sin 100)`)).toBe(Math.sin(100))
        expect(() => lits.run(`(sin)`)).toThrow()
        expect(() => lits.run(`(sin 1 2)`)).toThrow()
      })
    })
    describe(`cos`, () => {
      test(`samples`, () => {
        expect(lits.run(`(cos 0)`)).toBe(Math.cos(0))
        expect(lits.run(`(cos 0.1)`)).toBe(Math.cos(0.1))
        expect(lits.run(`(cos -0.1)`)).toBe(Math.cos(-0.1))
        expect(lits.run(`(cos 1)`)).toBe(Math.cos(1))
        expect(lits.run(`(cos 100)`)).toBe(Math.cos(100))
        expect(() => lits.run(`(cos)`)).toThrow()
        expect(() => lits.run(`(cos 1 2)`)).toThrow()
      })
    })
    describe(`tan`, () => {
      test(`samples`, () => {
        expect(lits.run(`(tan 0)`)).toBe(Math.tan(0))
        expect(lits.run(`(tan 0.1)`)).toBe(Math.tan(0.1))
        expect(lits.run(`(tan -0.1)`)).toBe(Math.tan(-0.1))
        expect(lits.run(`(tan 1)`)).toBe(Math.tan(1))
        expect(lits.run(`(tan 100)`)).toBe(Math.tan(100))
        expect(() => lits.run(`(tan)`)).toThrow()
        expect(() => lits.run(`(tan 1 2)`)).toThrow()
      })
    })

    describe(`sinh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(sinh 0)`)).toBe(Math.sinh(0))
        expect(lits.run(`(sinh 0.1)`)).toBe(Math.sinh(0.1))
        expect(lits.run(`(sinh -0.1)`)).toBe(Math.sinh(-0.1))
        expect(lits.run(`(sinh 1)`)).toBe(Math.sinh(1))
        expect(lits.run(`(sinh 100)`)).toBe(Math.sinh(100))
        expect(() => lits.run(`(sinh)`)).toThrow()
        expect(() => lits.run(`(sinh 1 2)`)).toThrow()
      })
    })
    describe(`cosh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(cosh 0)`)).toBe(Math.cosh(0))
        expect(lits.run(`(cosh 0.1)`)).toBe(Math.cosh(0.1))
        expect(lits.run(`(cosh -0.1)`)).toBe(Math.cosh(-0.1))
        expect(lits.run(`(cosh 1)`)).toBe(Math.cosh(1))
        expect(lits.run(`(cosh 100)`)).toBe(Math.cosh(100))
        expect(() => lits.run(`(cosh)`)).toThrow()
        expect(() => lits.run(`(cosh 1 2)`)).toThrow()
      })
    })
    describe(`tanh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(tanh 0)`)).toBe(Math.tanh(0))
        expect(lits.run(`(tanh 0.1)`)).toBe(Math.tanh(0.1))
        expect(lits.run(`(tanh -0.1)`)).toBe(Math.tanh(-0.1))
        expect(lits.run(`(tanh 1)`)).toBe(Math.tanh(1))
        expect(lits.run(`(tanh 100)`)).toBe(Math.tanh(100))
        expect(() => lits.run(`(tanh)`)).toThrow()
        expect(() => lits.run(`(tanh 1 2)`)).toThrow()
      })
    })

    describe(`asin`, () => {
      test(`samples`, () => {
        expect(lits.run(`(asin 0)`)).toBe(Math.asin(0))
        expect(lits.run(`(asin 0.1)`)).toBe(Math.asin(0.1))
        expect(lits.run(`(asin -0.1)`)).toBe(Math.asin(-0.1))
        expect(lits.run(`(asin 1)`)).toBe(Math.asin(1))
        expect(lits.run(`(asin 100)`)).toBeNaN()
        expect(() => lits.run(`(asin)`)).toThrow()
        expect(() => lits.run(`(asin 1 2)`)).toThrow()
      })
    })
    describe(`acos`, () => {
      test(`samples`, () => {
        expect(lits.run(`(acos 0)`)).toBe(Math.acos(0))
        expect(lits.run(`(acos 0.1)`)).toBe(Math.acos(0.1))
        expect(lits.run(`(acos -0.1)`)).toBe(Math.acos(-0.1))
        expect(lits.run(`(acos 1)`)).toBe(Math.acos(1))
        expect(lits.run(`(acos 100)`)).toBeNaN()
        expect(() => lits.run(`(acos)`)).toThrow()
        expect(() => lits.run(`(acos 1 2)`)).toThrow()
      })
    })
    describe(`atan`, () => {
      test(`samples`, () => {
        expect(lits.run(`(atan 0)`)).toBe(Math.atan(0))
        expect(lits.run(`(atan 0.1)`)).toBe(Math.atan(0.1))
        expect(lits.run(`(atan -0.1)`)).toBe(Math.atan(-0.1))
        expect(lits.run(`(atan 1)`)).toBe(Math.atan(1))
        expect(lits.run(`(atan 100)`)).toBe(Math.atan(100))
        expect(() => lits.run(`(atan)`)).toThrow()
        expect(() => lits.run(`(atan 1 2)`)).toThrow()
      })
    })

    describe(`asinh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(asinh 0)`)).toBe(Math.asinh(0))
        expect(lits.run(`(asinh 0.1)`)).toBe(Math.asinh(0.1))
        expect(lits.run(`(asinh -0.1)`)).toBe(Math.asinh(-0.1))
        expect(lits.run(`(asinh 1)`)).toBe(Math.asinh(1))
        expect(lits.run(`(asinh 100)`)).toBe(Math.asinh(100))
        expect(() => lits.run(`(asinh)`)).toThrow()
        expect(() => lits.run(`(asinh 1 2)`)).toThrow()
      })
    })
    describe(`acosh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(acosh 1)`)).toBe(Math.acosh(1))
        expect(lits.run(`(acosh 100)`)).toBe(Math.acosh(100))
        expect(lits.run(`(acosh 0.1)`)).toBeNaN()
        expect(lits.run(`(acosh -0.1)`)).toBeNaN()
        expect(lits.run(`(acosh 0)`)).toBeNaN()
        expect(() => lits.run(`(acosh)`)).toThrow()
        expect(() => lits.run(`(acosh 1 2)`)).toThrow()
      })
    })
    describe(`atanh`, () => {
      test(`samples`, () => {
        expect(lits.run(`(atanh 0)`)).toBe(Math.atanh(0))
        expect(lits.run(`(atanh 0.1)`)).toBe(Math.atanh(0.1))
        expect(lits.run(`(atanh -0.1)`)).toBe(Math.atanh(-0.1))
        expect(lits.run(`(atanh 1)`)).toBe(Number.POSITIVE_INFINITY)
        expect(lits.run(`(atanh 100)`)).toBeNaN()
        expect(() => lits.run(`(atanh)`)).toThrow()
        expect(() => lits.run(`(atanh 1 2)`)).toThrow()
      })
    })

    describe(`quot`, () => {
      test(`samples`, () => {
        expect(lits.run(`(quot 13.75 3.25)`)).toBe(4)
        expect(lits.run(`(quot -13.75 3.25)`)).toBe(-4)
        expect(lits.run(`(quot 13.75 -3.25)`)).toBe(-4)
        expect(lits.run(`(quot -13.75 -3.25)`)).toBe(4)
        expect(() => lits.run(`(quot)`)).toThrow()
        expect(() => lits.run(`(quot 1)`)).toThrow()
        expect(() => lits.run(`(quot 1 2 3)`)).toThrow()
      })
    })

    describe(`mod`, () => {
      test(`samples`, () => {
        expect(() => lits.run(`(mod)`)).toThrow()
        expect(() => lits.run(`(mod 3)`)).toThrow()
        expect(() => lits.run(`(mod 3 4 5)`)).toThrow()
        expect(lits.run(`(mod 13.75 3.25)`)).toBe(0.75)
        expect(lits.run(`(mod -13.75 3.25)`)).toBe(2.5)
        expect(lits.run(`(mod 13.75 -3.25)`)).toBe(-2.5)
        expect(lits.run(`(mod -13.75 -3.25)`)).toBe(-0.75)
        expect(lits.run(`(mod 2 1)`)).toBe(0)
        expect(lits.run(`(mod 2 2)`)).toBe(0)
        expect(lits.run(`(mod 3 2)`)).toBe(1)
        expect(lits.run(`(mod 3 -2)`)).toBe(-1)
        expect(lits.run(`(mod -3 -2)`)).toBe(-1)
        expect(lits.run(`(mod -3 2)`)).toBe(1)
        expect(lits.run(`(mod 4 0)`)).toBeNaN()
        expect(() => lits.run(`(mod 4 0 3)`)).toThrow()
      })
    })

    describe(`rem`, () => {
      test(`samples`, () => {
        expect(lits.run(`(rem 13.75 3.25)`)).toBe(0.75)
        expect(lits.run(`(rem -13.75 3.25)`)).toBe(-0.75)
        expect(lits.run(`(rem 13.75 -3.25)`)).toBe(0.75)
        expect(lits.run(`(rem -13.75 -3.25)`)).toBe(-0.75)
        expect(() => lits.run(`(rem)`)).toThrow()
        expect(() => lits.run(`(rem 1)`)).toThrow()
        expect(() => lits.run(`(rem 1 2 3)`)).toThrow()
      })
    })
  }
})
