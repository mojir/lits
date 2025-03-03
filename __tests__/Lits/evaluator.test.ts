import { beforeEach, describe, expect, it } from 'vitest'
import { tokenize } from '../../src/tokenizer'
import { parse } from '../../src/parser'
import { evaluate, evaluateAstNode } from '../../src/evaluator'
import { Lits } from '../../src'
import type { Context } from '../../src/evaluator/interface'
import { AstNodeType } from '../../src/constants/constants'
import type { LazyValue } from '../../src/Lits/Lits'
import { createContextStack } from '../../src/evaluator/ContextStack'

let lits: Lits

beforeEach(() => {
  lits = new Lits({ debug: true, algebraic: false })
})

const simpleProgram = `
(let
  [
    day (do (let [sec 1000]) (* 24 60 60 sec))
  ])
(* (get_in info [:days 1]) day)
`

const formatPhoneNumber = `
(if (string? $data)
  (do
    (let [ phoneNumber
          (if (== "+" (nth $data 0)) (subs $data 2) $data)
        ])
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
      gender: 'male',
    },
  },
}

const lazyValues: Record<string, LazyValue> = {
  lisa: { read: () => 15 },
}

describe('evaluator', () => {
  it('super simple program', () => {
    const tokens = tokenize('(+ 10 kalle)', { debug: true, algebraic: false })
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toBe(15)
  })
  it('another super simple program', () => {
    const tokens = tokenize('(+ 10 lisa)', { debug: true, algebraic: false })
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ lazyValues }))
    expect(result).toBe(25)
  })
  it('simple program', () => {
    const tokens = tokenize(simpleProgram, { debug: true, algebraic: false })
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toBe(13 * 24 * 60 * 60 * 1000)
  })
  it('if statement (true)', () => {
    const tokens = tokenize(
      `
      (if (== (get info "gender") "male") "It\\"s a boy" "It\\"s not a girl")
    `,
      { debug: true, algebraic: false },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toBe('It"s a boy')
  })
  it('if statement (false)', () => {
    const tokens = tokenize(
      `
      (if (== (get info "gender") "female") "It\\"s a girl" "It\\"s not a girl")
    `,
      { debug: true, algebraic: false },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toBe('It"s not a girl')
  })
  it('> statement', () => {
    const tokens = tokenize(
      `
      (> 0 -1)
    `,
      { debug: true, algebraic: false },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toBe(true)
  })
  it('!= statement 1', () => {
    const tokens = tokenize(
      `
      [(!= 0 -1) (!= 1 1)]
    `,
      { debug: true, algebraic: false },
    )
    const ast = parse(tokens)
    const result = evaluate(ast, createContextStack({ contexts: [context] }))
    expect(result).toEqual([true, false])
  })

  it('normal expression with lambda', () => {
    expect(lits.run('((fn [x] (* x x)) 10)')).toBe(100)
  })

  it('litsFunction', () => {
    expect(lits.run('((fn [] 10))')).toBe(10)
    expect(lits.run('((fn [x] (x 10)) (fn [x] (* x x)))')).toBe(100)
    expect(lits.run('((fn [x] (x 10)) inc)')).toBe(11)
    expect(() => lits.run('((fn [x] (x 10)) inc+)')).toThrow()
    expect(() => lits.run('((fn [x] (* x x)) 10 20)')).toThrow()
    expect(() => lits.run('((fn [x] (* x x)))')).toThrow()
  })

  it('formatPhoneNumber', () => {
    expect(lits.run(formatPhoneNumber, { values: { $data: null } })).toBe('')
    expect(lits.run(formatPhoneNumber, { values: { $data: '' } })).toBe('')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+' } })).toBe('')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+1' } })).toBe('')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+12' } })).toBe('(2')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+123' } })).toBe('(23')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+1234' } })).toBe('(234')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+12345' } })).toBe('(234) 5')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+123456' } })).toBe('(234) 56')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+1234567' } })).toBe('(234) 567')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+12345678' } })).toBe('(234) 567-8')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+123456789' } })).toBe('(234) 567-89')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+1234567890' } })).toBe('(234) 567-890')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+12345678901' } })).toBe('(234) 567-8901')
    expect(lits.run(formatPhoneNumber, { values: { $data: '+123456789012' } })).toBe('(234) 567-89012')
    expect(lits.run(formatPhoneNumber, { values: { $data: '2' } })).toBe('(2')
    expect(lits.run(formatPhoneNumber, { values: { $data: '23' } })).toBe('(23')
    expect(lits.run(formatPhoneNumber, { values: { $data: '234' } })).toBe('(234')
    expect(lits.run(formatPhoneNumber, { values: { $data: '2345' } })).toBe('(234) 5')
    expect(lits.run(formatPhoneNumber, { values: { $data: '23456' } })).toBe('(234) 56')
    expect(lits.run(formatPhoneNumber, { values: { $data: '234567' } })).toBe('(234) 567')
    expect(lits.run(formatPhoneNumber, { values: { $data: '2345678' } })).toBe('(234) 567-8')
    expect(lits.run(formatPhoneNumber, { values: { $data: '23456789' } })).toBe('(234) 567-89')
    expect(lits.run(formatPhoneNumber, { values: { $data: '234567890' } })).toBe('(234) 567-890')
    expect(lits.run(formatPhoneNumber, { values: { $data: '2345678901' } })).toBe('(234) 567-8901')
    expect(lits.run(formatPhoneNumber, { values: { $data: '23456789012' } })).toBe('(234) 567-89012')
  })
})

it('evaluateAstNode', () => {
  expect(() =>
    evaluateAstNode(
      {
        t: AstNodeType.Modifier,
        v: '&rest',
        token: ['P_Symbol', 'X'],
        p: [],
        n: undefined,
      },
      createContextStack(),
    ),
  ).toThrow()
  expect(() =>
    evaluateAstNode(
      {
        t: AstNodeType.Modifier,
        v: '&rest',
        token: undefined,
        p: [],
        n: undefined,
      },
      createContextStack(),
    ),
  ).toThrow()
  expect(() =>
    evaluateAstNode(
      {
        t: AstNodeType.Modifier,
        v: '&rest',
        token: ['P_Symbol', 'X', {
          sourceCodeInfo: { code: '', position: { column: 1, line: 1 } },
        }],
        p: [],
        n: undefined,
      },
      createContextStack(),
    ),
  ).toThrow()
})

it('a test', () => {
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
