import { tokenize } from '../src/tokenizer/Tokenizer'
import { parseProgram } from '../src/parser/Parser'
import { evaluateAst } from '../src/evaluator/Evaluator'
import { executeProgram } from '../src'

const simpleProgram = `
(let
  (
    (day
      (let ((sec 1000))
        (* 24 60 60 sec)
      )
    )
  )
  (* info.days[1] day)
)`

const formatPhoneNumber = `
(if (stringp $data)
  (let ((phoneNumber (if (= "+" (aref $data 0)) (substring $data 2) $data)))
    (if (> (length phoneNumber) 6)
      (string-concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3 6) "-" (substring phoneNumber 6))
      (if (> (length phoneNumber) 3)
        (string-concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3))
        (if (> (length phoneNumber) 0)
          (string-concat "(" (substring phoneNumber 0))
          phoneNumber
        )
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
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toBe(15)
  })
  test('simple program', () => {
    const tokens = tokenize(simpleProgram)
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toBe(13 * 24 * 60 * 60 * 1000)
  })
  test('if statement (true)', () => {
    const tokens = tokenize(`
      (if (= info.gender "male") (write "It's a boy") (write "It's not a girl"))
    `)
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toBe(`It's a boy`)
  })
  test('if statement (false)', () => {
    const tokens = tokenize(`
      (if (write (= info.gender "female")) (write "It's a girl") (write "It's not a girl"))
    `)
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toBe(`It's not a girl`)
  })
  test('> statement', () => {
    const tokens = tokenize(`
      (> 0 -1)
    `)
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toBe(true)
  })
  test('!= statement 1', () => {
    const tokens = tokenize(`
      (array (!= 0 -1) (!= 1 1))
    `)
    const ast = parseProgram(tokens)
    const result = evaluateAst(ast, context)
    expect(result).toEqual([true, false])
  })

  test('formatPhoneNumber', () => {
    // expect(executeProgram(formatPhoneNumber, { $data: null })).toBe('')
    expect(executeProgram(formatPhoneNumber, { $data: '' })).toBe('')
    // expect(executeProgram(formatPhoneNumber, { $data: '+' })).toBe('')
    // expect(executeProgram(formatPhoneNumber, { $data: '+1' })).toBe('')
    // expect(executeProgram(formatPhoneNumber, { $data: '+12' })).toBe('(2')
    // expect(executeProgram(formatPhoneNumber, { $data: '+123' })).toBe('(23')
    // expect(executeProgram(formatPhoneNumber, { $data: '+1234' })).toBe('(234')
    // expect(executeProgram(formatPhoneNumber, { $data: '+12345' })).toBe('(234) 5')
    // expect(executeProgram(formatPhoneNumber, { $data: '+123456' })).toBe('(234) 56')
    // expect(executeProgram(formatPhoneNumber, { $data: '+1234567' })).toBe('(234) 567')
    // expect(executeProgram(formatPhoneNumber, { $data: '+12345678' })).toBe('(234) 567-8')
    // expect(executeProgram(formatPhoneNumber, { $data: '+123456789' })).toBe('(234) 567-89')
    // expect(executeProgram(formatPhoneNumber, { $data: '+1234567890' })).toBe('(234) 567-890')
    // expect(executeProgram(formatPhoneNumber, { $data: '+12345678901' })).toBe('(234) 567-8901')
    // expect(executeProgram(formatPhoneNumber, { $data: '+123456789012' })).toBe('(234) 567-89012')
    // expect(executeProgram(formatPhoneNumber, { $data: '2' })).toBe('(2')
    // expect(executeProgram(formatPhoneNumber, { $data: '23' })).toBe('(23')
    // expect(executeProgram(formatPhoneNumber, { $data: '234' })).toBe('(234')
    // expect(executeProgram(formatPhoneNumber, { $data: '2345' })).toBe('(234) 5')
    // expect(executeProgram(formatPhoneNumber, { $data: '23456' })).toBe('(234) 56')
    // expect(executeProgram(formatPhoneNumber, { $data: '234567' })).toBe('(234) 567')
    // expect(executeProgram(formatPhoneNumber, { $data: '2345678' })).toBe('(234) 567-8')
    // expect(executeProgram(formatPhoneNumber, { $data: '23456789' })).toBe('(234) 567-89')
    // expect(executeProgram(formatPhoneNumber, { $data: '234567890' })).toBe('(234) 567-890')
    // expect(executeProgram(formatPhoneNumber, { $data: '2345678901' })).toBe('(234) 567-8901')
    // expect(executeProgram(formatPhoneNumber, { $data: '23456789012' })).toBe('(234) 567-89012')
  })

  test('setq', () => {
    executeProgram(`
      (setq a 10)
      (write a)
    `)
  })

  describe('vector', () => {
    test('vector', () => {
      expect(executeProgram('(array 1 2 3)')).toEqual([1, 2, 3])
    })
    test('aref', () => {
      expect(executeProgram('(aref (array 1 2 3) 1)')).toEqual(2)
    })
  })
})
