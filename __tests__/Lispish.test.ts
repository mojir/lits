import { Lispish } from '../src'

describe(`avaluate`, () => {
  const lispish = new Lispish()
  const tokens = lispish.tokenize(`(+ 1 2)`)
  const ast = lispish.parse(tokens)
  const result = lispish.evaluate(ast)
  expect(result).toBe(3)
})

describe(`import`, () => {
  let lispish: Lispish
  beforeEach(() => {
    lispish = new Lispish()
  })
  test(`import a function`, () => {
    lispish.import(`(defn tripple [x] (* x 3))`)
    expect(lispish.run(`(tripple 10)`)).toBe(30)
  })

  test(`import a variable`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`change imported variable`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`import a function twice`, () => {
    lispish.import(`(defn tripple [x] (* x 3))`)
    expect(() => lispish.import(`(defn tripple [x] (* x 3))`)).toThrow()
  })

  test(`import a function with a built in normal expression name`, () => {
    expect(() => lispish.import(`(defn 1+ (x) (+ x 1))`)).toThrow()
  })

  test(`import a function with a built in special expression name`, () => {
    expect(() => lispish.import(`(defn and (x y) (* x y))`)).toThrow()
  })

  test(`import a variable twice`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(() => lispish.import(`(def magicNumber 42)`)).toThrow()
  })

  test(`import more than once`, () => {
    lispish.import(`(defn tripple [x] (* x 3))`)
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`(tripple magicNumber)`)).toBe(126)
  })
})
