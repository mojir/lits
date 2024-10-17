import { describe, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import type { SpecialExpressionName } from '../../../builtin'
import { testFormatter } from '../testFormatter'

const lits = new Lits({ debug: true })

const expressionsWithParams: SpecialExpressionName[] = [
  'and',
  'comment',
  'declared?',
  'def',
  'defs',
  'if',
  'if-not',
  'or',
  '??',
  'recur',
  'time!',
  'throw',
]

describe('unparse expressions with params', () => {
  it('should unparse with zero params', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'or', 'recur'].includes(name))
    ) {
      const program = `(${expressionWithParams})`
      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should unparse with 1 param', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'declared?', 'or', '??', 'recur', 'time!', 'throw'].includes(name))
    ) {
      const program = `(${expressionWithParams} foo)`
      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should unparse with 2 params', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'def', 'defs', 'if', 'if-not', 'or', '??', 'recur'].includes(name))
    ) {
      const program = `(${expressionWithParams} foo bar)`
      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should unparse with 3 params', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'if', 'if-not', 'or', 'recur'].includes(name))
    ) {
      const program = `(${expressionWithParams} foo bar baz)`
      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should unparse with 4 params', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'or', 'recur'].includes(name))
    ) {
      const program = `(${expressionWithParams} foo bar baz bazz)`
      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should work zero params and comments', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'or', 'recur'].includes(name))
    ) {
      const program = `
;; Comment 1

;; Comment 2

;; Leading comment 1
;; Leading comment 2
(${expressionWithParams}) ;; Inline comment


(${expressionWithParams} ;; Inline comment

;; Comment
) ;; Inline comment

;; Comment
`.trim()

      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should work 1 param and comments', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'or', '??', 'recur'].includes(name))
    ) {
      const program = `
;; Comment

;; Leading comment 1
;; Leading comment 2
(${expressionWithParams} foo) ;; Inline comment


(${expressionWithParams} foo ;; Inline comment

;; Comment
) ;; Inline comment

;; Comment
`.trim()

      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should work 2 params and comments', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['??'].includes(name))
    // ['and', 'comment', 'def', 'defs', 'if', 'if-not', 'or', '??', 'recur'].includes(name))
    ) {
      const program = `
;; Comment

;; Leading comment 1
;; Leading comment 2
(${expressionWithParams} foo ;; Inline comment

 ${' '.repeat(expressionWithParams.length)} ;; Comment

 ${' '.repeat(expressionWithParams.length)} bar) ;; Inline comment


(${expressionWithParams} ;; Inline comment
 foo ;; Inline comment
 bar ;; Inline comment
) ;; Inline comment

;; Comment
`.trim()

      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should work 3 params and comments', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'if', 'if-not', 'or', 'recur'].includes(name))
    ) {
      const program = `
;; Comment

;; Leading comment 1
;; Leading comment 2
(${expressionWithParams} foo ;; Inline comment
 ${' '.repeat(expressionWithParams.length)} bar ;; Inline comment

 ${' '.repeat(expressionWithParams.length)} ;; Comment

 ${' '.repeat(expressionWithParams.length)} baz) ;; Inline comment


(${expressionWithParams} ;; Inline comment
 foo ;; Inline comment
 bar ;; Inline comment
 ;; Comment
 baz ;; Inline comment
) ;; Inline comment

;; Comment
`.trim()

      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })

  it('should work 4 params and comments', () => {
    for (const expressionWithParams of expressionsWithParams
      .filter(name =>
        ['and', 'comment', 'or', 'recur'].includes(name))
    ) {
      const program = `
;; Comment

;; Leading comment 1
;; Leading comment 2
(${expressionWithParams} foo ;; Inline comment
 ${' '.repeat(expressionWithParams.length)} bar ;; Inline comment
 ${' '.repeat(expressionWithParams.length)} baz ;; Inline comment
 ${' '.repeat(expressionWithParams.length)} bazz) ;; Inline comment


(${expressionWithParams} ;; Inline comment
 foo ;; Inline comment
 bar ;; Inline comment
 ;; Leading Comment
 baz ;; Inline comment
 ;; Comment
 bazz ;; Inline comment
) ;; Inline comment

;; Comment
`.trim()

      testFormatter(
        p => lits.format(p),
        program,
        program,
      )
    }
  })
})
