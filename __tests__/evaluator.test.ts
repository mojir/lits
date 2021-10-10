import { tokenize } from '../src/tokenizer'
import { parse } from '../src/parser'
import { evaluate } from '../src/evaluator'
import { Lispish } from '../src'
import { Context } from '../src/evaluator/interface'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

const simpleProgram = `
(let
  [
    day (let [sec 1000] (* 24 60 60 sec))
  ]
  (* (get-path info "days[1]") day)
)`

const formatPhoneNumber = `
(if (string? $data)
  (let [phoneNumber (if (= "+" (nth $data 0)) (subs $data 2) $data)]
    (cond
      ((> (count phoneNumber) 6)
        (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3 6) "-" (subs phoneNumber 6))
      )
      ((> (count phoneNumber) 3)
        (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3))
      )
      ((> (count phoneNumber) 0)
        (str "(" (subs phoneNumber 0))
      )
      (true
        phoneNumber
      )
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
    const tokens = tokenize(`(+ 10 kalle)`)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toBe(15)
  })
  test(`simple program`, () => {
    const tokens = tokenize(simpleProgram)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toBe(13 * 24 * 60 * 60 * 1000)
  })
  test(`if statement (true)`, () => {
    const tokens = tokenize(`
      (if (= (get info "gender") "male") "It's a boy" "It's not a girl")
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toBe(`It's a boy`)
  })
  test(`if statement (false)`, () => {
    const tokens = tokenize(`
      (if (= (get info "gender") "female") "It's a girl" "It's not a girl")
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toBe(`It's not a girl`)
  })
  test(`> statement`, () => {
    const tokens = tokenize(`
      (> 0 -1)
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toBe(true)
  })
  test(`not= statement 1`, () => {
    const tokens = tokenize(`
      [(not= 0 -1) (not= 1 1)]
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, {})
    expect(result).toEqual([true, false])
  })

  test(`expressionExpression`, () => {
    expect(lispish.run(`((fn [x] (* x x)) 10)`)).toBe(100)
  })

  test(`lispishFunction`, () => {
    expect(lispish.run(`((fn [] 10))`)).toBe(10)
    expect(lispish.run(`((fn [x] (x 10)) (fn [x] (* x x)))`)).toBe(100)
    expect(lispish.run(`((fn [x] (x 10)) inc)`)).toBe(11)
    expect(() => lispish.run(`((fn [x] (x 10)) inc+)`)).toThrow()
    expect(() => lispish.run(`((fn [x] (* x x)) 10 20)`)).toThrow()
    expect(() => lispish.run(`((fn [x] (* x x)))`)).toThrow()
  })

  test(`formatPhoneNumber`, () => {
    expect(lispish.run(formatPhoneNumber, { vars: { $data: null } })).toBe(``)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `` } })).toBe(``)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+` } })).toBe(``)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+1` } })).toBe(``)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+12` } })).toBe(`(2`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+123` } })).toBe(`(23`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+1234` } })).toBe(`(234`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+12345` } })).toBe(`(234) 5`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+123456` } })).toBe(`(234) 56`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+1234567` } })).toBe(`(234) 567`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+12345678` } })).toBe(`(234) 567-8`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+123456789` } })).toBe(`(234) 567-89`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+1234567890` } })).toBe(`(234) 567-890`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+12345678901` } })).toBe(`(234) 567-8901`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `+123456789012` } })).toBe(`(234) 567-89012`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `2` } })).toBe(`(2`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `23` } })).toBe(`(23`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `234` } })).toBe(`(234`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `2345` } })).toBe(`(234) 5`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `23456` } })).toBe(`(234) 56`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `234567` } })).toBe(`(234) 567`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `2345678` } })).toBe(`(234) 567-8`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `23456789` } })).toBe(`(234) 567-89`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `234567890` } })).toBe(`(234) 567-890`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `2345678901` } })).toBe(`(234) 567-8901`)
    expect(lispish.run(formatPhoneNumber, { vars: { $data: `23456789012` } })).toBe(`(234) 567-89012`)
  })
})
