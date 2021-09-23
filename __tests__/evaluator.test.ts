import { tokenize } from '../src/tokenizer'
import { parse } from '../src/parser'
import { evaluate } from '../src/evaluator'
import { Lispish } from '../src'

const lispish = new Lispish()

const simpleProgram = `
(let
  (
    (day
      (let ((sec 1000))
        (* 24 60 60 sec)
      )
    )
  )
  (* (get-path info "days[1]") day)
)`

const formatPhoneNumber = `
(if (string? $data)
  (let ((phoneNumber (if (= "+" (at $data 0)) (substring $data 2) $data)))
    (cond
      ((> (string-length phoneNumber) 6)
        (concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3 6) "-" (substring phoneNumber 6))
      )
      ((> (string-length phoneNumber) 3)
        (concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3))
      )
      ((> (string-length phoneNumber) 0)
        (concat "(" (substring phoneNumber 0))
      )
      (true
        phoneNumber
      )
    )
  )
  ""
)
`

const context = {
  kalle: 5,
  info: {
    days: [10, 13],
    gender: 'male',
  },
}

describe('Evaluator', () => {
  test('super simple program', () => {
    const tokens = tokenize(`(+ 10 kalle)`)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(15)
  })
  test('simple program', () => {
    const tokens = tokenize(simpleProgram)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(13 * 24 * 60 * 60 * 1000)
  })
  test('if statement (true)', () => {
    const tokens = tokenize(`
      (if (= (get-attr info "gender") "male") "It's a boy" "It's not a girl")
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(`It's a boy`)
  })
  test('if statement (false)', () => {
    const tokens = tokenize(`
      (if (= (get-attr info "gender") "female") "It's a girl" "It's not a girl")
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(`It's not a girl`)
  })
  test('> statement', () => {
    const tokens = tokenize(`
      (> 0 -1)
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(true)
  })
  test('!= statement 1', () => {
    const tokens = tokenize(`
      '((!= 0 -1) (!= 1 1))
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toEqual([true, false])
  })

  test('expressionExpression', () => {
    expect(lispish.run(`((lambda (x) (* x x)) 10)`)).toBe(100)
  })

  test('lispishFunction', () => {
    expect(lispish.run(`((lambda () 10))`)).toBe(10)
    expect(lispish.run(`((lambda (x) (x 10)) (lambda (x) (* x x)))`)).toBe(100)
    expect(lispish.run(`((lambda (x) (x 10)) (function 1+))`)).toBe(11)
    expect(() => lispish.run(`((lambda (x) (x 10)) (function 1++))`)).toThrow()
    expect(() => lispish.run(`((lambda (x) (* x x)) 10 20)`)).toThrow()
    expect(() => lispish.run(`((lambda (x) (* x x)))`)).toThrow()
  })

  test('formatPhoneNumber', () => {
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: null } })).toBe('')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '' } })).toBe('')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+' } })).toBe('')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+1' } })).toBe('')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+12' } })).toBe('(2')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+123' } })).toBe('(23')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+1234' } })).toBe('(234')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+12345' } })).toBe('(234) 5')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+123456' } })).toBe('(234) 56')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+1234567' } })).toBe('(234) 567')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+12345678' } })).toBe('(234) 567-8')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+123456789' } })).toBe('(234) 567-89')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+1234567890' } })).toBe('(234) 567-890')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+12345678901' } })).toBe('(234) 567-8901')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '+123456789012' } })).toBe('(234) 567-89012')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '2' } })).toBe('(2')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '23' } })).toBe('(23')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '234' } })).toBe('(234')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '2345' } })).toBe('(234) 5')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '23456' } })).toBe('(234) 56')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '234567' } })).toBe('(234) 567')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '2345678' } })).toBe('(234) 567-8')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '23456789' } })).toBe('(234) 567-89')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '234567890' } })).toBe('(234) 567-890')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '2345678901' } })).toBe('(234) 567-8901')
    expect(lispish.run(formatPhoneNumber, { globalVariables: { $data: '23456789012' } })).toBe('(234) 567-89012')
  })
})
