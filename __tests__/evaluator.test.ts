import { tokenize } from '../src/tokenizer'
import { parse } from '../src/parser'
import { evaluate } from '../src/evaluator'
import { lispish } from '../src'

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
      (concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3 6) "-" (substring phoneNumber 6))
      (if (> (length phoneNumber) 3)
        (concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3))
        (if (> (length phoneNumber) 0)
          (concat "(" (substring phoneNumber 0))
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
      (if (= info.gender "male") "It's a boy" "It's not a girl")
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toBe(`It's a boy`)
  })
  test('if statement (false)', () => {
    const tokens = tokenize(`
      (if (= info.gender "female") "It's a girl" "It's not a girl")
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
      (array (!= 0 -1) (!= 1 1))
    `)
    const ast = parse(tokens)
    const result = evaluate(ast, context, { variables: {}, functions: {} })
    expect(result).toEqual([true, false])
  })

  test('formatPhoneNumber', () => {
    expect(lispish(formatPhoneNumber, { $data: null })).toBe('')
    expect(lispish(formatPhoneNumber, { $data: '' })).toBe('')
    expect(lispish(formatPhoneNumber, { $data: '+' })).toBe('')
    expect(lispish(formatPhoneNumber, { $data: '+1' })).toBe('')
    expect(lispish(formatPhoneNumber, { $data: '+12' })).toBe('(2')
    expect(lispish(formatPhoneNumber, { $data: '+123' })).toBe('(23')
    expect(lispish(formatPhoneNumber, { $data: '+1234' })).toBe('(234')
    expect(lispish(formatPhoneNumber, { $data: '+12345' })).toBe('(234) 5')
    expect(lispish(formatPhoneNumber, { $data: '+123456' })).toBe('(234) 56')
    expect(lispish(formatPhoneNumber, { $data: '+1234567' })).toBe('(234) 567')
    expect(lispish(formatPhoneNumber, { $data: '+12345678' })).toBe('(234) 567-8')
    expect(lispish(formatPhoneNumber, { $data: '+123456789' })).toBe('(234) 567-89')
    expect(lispish(formatPhoneNumber, { $data: '+1234567890' })).toBe('(234) 567-890')
    expect(lispish(formatPhoneNumber, { $data: '+12345678901' })).toBe('(234) 567-8901')
    expect(lispish(formatPhoneNumber, { $data: '+123456789012' })).toBe('(234) 567-89012')
    expect(lispish(formatPhoneNumber, { $data: '2' })).toBe('(2')
    expect(lispish(formatPhoneNumber, { $data: '23' })).toBe('(23')
    expect(lispish(formatPhoneNumber, { $data: '234' })).toBe('(234')
    expect(lispish(formatPhoneNumber, { $data: '2345' })).toBe('(234) 5')
    expect(lispish(formatPhoneNumber, { $data: '23456' })).toBe('(234) 56')
    expect(lispish(formatPhoneNumber, { $data: '234567' })).toBe('(234) 567')
    expect(lispish(formatPhoneNumber, { $data: '2345678' })).toBe('(234) 567-8')
    expect(lispish(formatPhoneNumber, { $data: '23456789' })).toBe('(234) 567-89')
    expect(lispish(formatPhoneNumber, { $data: '234567890' })).toBe('(234) 567-890')
    expect(lispish(formatPhoneNumber, { $data: '2345678901' })).toBe('(234) 567-8901')
    expect(lispish(formatPhoneNumber, { $data: '23456789012' })).toBe('(234) 567-89012')
  })

  test('reduce', () => {
    let program = `
      (defun countChars (stringArray)
        (reduce
          (lambda (sum str) (+ sum (length str)))
          stringArray
          0
        )
      )

      (countChars (array "First" "Second" "Third"))
      `
    expect(lispish(program)).toBe(16)

    program = `
      (defun longestLength (stringArray)
        (reduce
          (lambda (sum str)
            (if (> sum (length str))
              sum
              (length str)
            )
          )
          stringArray
          0
        )
      )

      (longestLength (array "First" "Second" "Third"))
      `
    expect(lispish(program)).toBe(6)
  })
})
