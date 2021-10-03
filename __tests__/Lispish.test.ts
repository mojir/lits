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
    lispish.import(`(defun tripple (x) (* x 3))`)
    expect(lispish.run(`(tripple 10)`)).toBe(30)
  })

  test(`import a variable`, () => {
    lispish.import(`(setq magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`change imported variable`, () => {
    lispish.import(`(setq magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`import a function twice`, () => {
    lispish.import(`(defun tripple (x) (* x 3))`)
    expect(() => lispish.import(`(defun tripple (x) (* x 3))`)).toThrow()
  })

  test(`import a function with a built in normal expression name`, () => {
    expect(() => lispish.import(`(defun 1+ (x) (+ x 1))`)).toThrow()
  })

  test(`import a function with a built in special expression name`, () => {
    expect(() => lispish.import(`(defun and (x y) (* x y))`)).toThrow()
  })

  test(`import a variable twice`, () => {
    lispish.import(`(setq magicNumber 42)`)
    expect(() => lispish.import(`(setq magicNumber 42)`)).toThrow()
  })

  test(`import more than once`, () => {
    lispish.import(`(defun tripple (x) (* x 3))`)
    lispish.import(`(setq magicNumber 42)`)
    expect(lispish.run(`(tripple magicNumber)`)).toBe(126)
  })
})
