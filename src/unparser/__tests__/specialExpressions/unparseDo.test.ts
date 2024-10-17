import { describe, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { testFormatter } from '../testFormatter'

const lits = new Lits({ debug: true })

const sampleProgram = '(do foo (bar 1 2) baz)'
const sampleProgramWithComments = `
;; Leading Comment
(do ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1 2) ;; Inline comment
  baz) ;; Inline comment
`.trim()

describe('unparse expressions with params', () => {
  it('should unparse with zero params', () => {
    const program = '(do)'
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  it('should unparse with 1 param', () => {
    const program = '(do foo)'
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
    testFormatter(
      p => lits.format(p, { lineLength: 1 }),
      program,
      '(do\n  foo)',
    )
  })
  describe('unparse sampleProgram', () => {
    for (let lineLength = 0; lineLength <= sampleProgram.length + 1; lineLength += 1) {
      it(`should unparse with line length ${lineLength}`, () => {
        testFormatter(
          program => lits.format(program, { lineLength }),
          sampleProgram,
          formatSampleProgram(lineLength),
        )
      })
    }
  })

  describe('unparse sampleProgramWithComments', () => {
    for (let lineLength = 0; lineLength <= 30; lineLength += 1) {
      it(`should unparse with line length ${lineLength}`, () => {
        testFormatter(
          program => lits.format(program, { lineLength }),
          sampleProgramWithComments,
          formatSampleProgramWithComments(lineLength),
        )
      })
    }
  })
})

function formatSampleProgramWithComments(lineLength: number): string {
  if (lineLength >= 29 || lineLength === 0)
    return sampleProgramWithComments

  if (lineLength >= 27) {
    return `
;; Leading Comment
(do ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1
       2) ;; Inline comment
  baz) ;; Inline comment
`.trim()
  }

  return `
;; Leading Comment
(do ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar
   1
   2) ;; Inline comment
  baz) ;; Inline comment
`.trim()
}

function formatSampleProgram(lineLength: number): string {
  if (lineLength >= sampleProgram.length || lineLength === 0)
    return sampleProgram

  if (lineLength >= 13) {
    return `
(do foo
    (bar 1 2)
    baz)
`.trim()
  }

  if (lineLength >= 11) {
    return `
(do
  foo
  (bar 1 2)
  baz)
`.trim()
  }

  if (lineLength >= 9) {
    return `
(do
  foo
  (bar 1
       2)
  baz)
`.trim()
  }

  return `
(do
  foo
  (bar
   1
   2)
  baz)
`.trim()
}
