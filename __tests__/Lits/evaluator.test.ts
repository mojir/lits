import { tokenize } from '../../src/tokenizer'
import { parse } from '../../src/parser'
import { evaluate, evaluateAstNode } from '../../src/evaluator'
import { Context, Lits } from '../../src'
import { ContextStack } from '../../src/ContextStack'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true })
})

const simpleProgram = `
(let
  [
    day (let [sec 1000] (* 24 60 60 sec))
  ]
  (* (get-in info [:days 1]) day)
)`

const formatPhoneNumber = `
(if (string? $data)
  (let [phoneNumber (if (= "+" (nth $data 0)) (subs $data 2) $data)]
    (cond
      (> (count phoneNumber) 6)
        (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3 6) "-" (subs phoneNumber 6))

      (> (count phoneNumber) 3)
        (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3))

      (> (count phoneNumber) 0)
        (str "(" (subs phoneNumber 0))

      true phoneNumber
    )
  )
  ""
)
`

const context: Context = {
  kalle: { value: 5 },
  info: {
    value: {
      days: [10, 13],
      gender: `male`,
    },
  },
}

describe(`Evaluator`, () => {
  test(`super simple program`, () => {
    const tokens = tokenize(`(+ 10 kalle)`, { debug: true })
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toBe(15)
  })
  test(`simple program`, () => {
    const tokens = tokenize(simpleProgram, { debug: true })
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toBe(13 * 24 * 60 * 60 * 1000)
  })
  test(`if statement (true)`, () => {
    const tokens = tokenize(
      `
      (if (= (get info "gender") "male") "It\\"s a boy" "It\\"s not a girl")
    `,
      { debug: true },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toBe(`It"s a boy`)
  })
  test(`if statement (false)`, () => {
    const tokens = tokenize(
      `
      (if (= (get info "gender") "female") "It\\"s a girl" "It\\"s not a girl")
    `,
      { debug: true },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toBe(`It"s not a girl`)
  })
  test(`> statement`, () => {
    const tokens = tokenize(
      `
      (> 0 -1)
    `,
      { debug: true },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toBe(true)
  })
  test(`not= statement 1`, () => {
    const tokens = tokenize(
      `
      [(not= 0 -1) (not= 1 1)]
    `,
      { debug: true },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, ContextStack.create([context]))
    expect(result).toEqual([true, false])
  })

  test(`normal expression with lambda`, () => {
    expect(lits.run(`((fn [x] (* x x)) 10)`)).toBe(100)
  })

  test(`litsFunction`, () => {
    expect(lits.run(`((fn [] 10))`)).toBe(10)
    expect(lits.run(`((fn [x] (x 10)) (fn [x] (* x x)))`)).toBe(100)
    expect(lits.run(`((fn [x] (x 10)) inc)`)).toBe(11)
    expect(() => lits.run(`((fn [x] (x 10)) inc+)`)).toThrow()
    expect(() => lits.run(`((fn [x] (* x x)) 10 20)`)).toThrow()
    expect(() => lits.run(`((fn [x] (* x x)))`)).toThrow()
  })

  test(`formatPhoneNumber`, () => {
    expect(lits.run(formatPhoneNumber, { globals: { $data: null } })).toBe(``)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `` } })).toBe(``)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+` } })).toBe(``)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+1` } })).toBe(``)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+12` } })).toBe(`(2`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+123` } })).toBe(`(23`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+1234` } })).toBe(`(234`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+12345` } })).toBe(`(234) 5`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+123456` } })).toBe(`(234) 56`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+1234567` } })).toBe(`(234) 567`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+12345678` } })).toBe(`(234) 567-8`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+123456789` } })).toBe(`(234) 567-89`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+1234567890` } })).toBe(`(234) 567-890`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+12345678901` } })).toBe(`(234) 567-8901`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `+123456789012` } })).toBe(`(234) 567-89012`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `2` } })).toBe(`(2`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `23` } })).toBe(`(23`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `234` } })).toBe(`(234`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `2345` } })).toBe(`(234) 5`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `23456` } })).toBe(`(234) 56`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `234567` } })).toBe(`(234) 567`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `2345678` } })).toBe(`(234) 567-8`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `23456789` } })).toBe(`(234) 567-89`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `234567890` } })).toBe(`(234) 567-890`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `2345678901` } })).toBe(`(234) 567-8901`)
    expect(lits.run(formatPhoneNumber, { globals: { $data: `23456789012` } })).toBe(`(234) 567-89012`)
  })
})

test(`evaluateAstNode`, () => {
  expect(() =>
    evaluateAstNode(
      {
        type: `Modifier`,
        value: `&`,
        token: { type: `name`, value: `X` },
      },
      ContextStack.create(),
    ),
  ).toThrow()
  expect(() =>
    evaluateAstNode(
      {
        type: `Modifier`,
        value: `&`,
      },
      ContextStack.create(),
    ),
  ).toThrow()
  expect(() =>
    evaluateAstNode(
      {
        type: `Modifier`,
        value: `&`,
        token: { type: `name`, debugInfo: { code: ``, column: 1, line: 1 }, value: `X` },
      },
      ContextStack.create(),
    ),
  ).toThrow()
})

test(`a test`, () => {
  expect(() =>
    lits.run(`(defn numberComparer [a b]
    (cond
      (< a b) -1
      (> a b) 1
      true 0
    )
  )
  `),
  ).not.toThrow()
})
