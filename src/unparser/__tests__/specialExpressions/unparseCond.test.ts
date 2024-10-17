import { describe, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { testFormatter } from '../testFormatter'

const lits = new Lits({ debug: true })

const sampleProgram = '(cond foo (bar 1 2), baz (qux 3 4))'
const sampleProgramWithComments = `
; This is a comment
(cond ;; This is a comment
  foo (bar 1 2) ;; This is a comment
  baz (qux 3 4))`.trim()

describe('unparse cond', () => {
  describe('unparse sampleProgram with comments', () => {
    for (let lineLength = 0; lineLength <= 80; lineLength += 1) {
      it(`should unparse with line length ${lineLength}`, () => {
        testFormatter(
          program => lits.format(program, { lineLength }),
          sampleProgramWithComments,
          formatSampleProgramWithComments(lineLength),
        )
      })
    }
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

  it('should work 1', () => {
    const program = `(cond
  foo (bar 1
           2)
  baz (qux 3
           4))
`.trim()
    testFormatter(
      p => lits.format(p, { lineLength: 14 }),
      program,
      program,
    )
  })
})

function formatSampleProgramWithComments(lineLength: number): string {
  if (lineLength === 0)
    return sampleProgramWithComments

  if (lineLength >= 36) {
    return `
; This is a comment
(cond ;; This is a comment
  foo (bar 1 2) ;; This is a comment
  baz (qux 3 4))
`.trim()
  }

  if (lineLength >= 34) {
    return `
; This is a comment
(cond ;; This is a comment
  foo (bar 1
           2) ;; This is a comment
  baz (qux 3 4))
`.trim()
  }

  if (lineLength >= 30) {
    return `
; This is a comment
(cond ;; This is a comment
  foo (bar
       1
       2) ;; This is a comment
  baz (qux 3 4))
`.trim()
  }

  if (lineLength >= 11) {
    return `
; This is a comment
(cond ;; This is a comment
  foo
  (bar
   1
   2) ;; This is a comment
  baz
  (qux 3 4))
`.trim()
  }

  if (lineLength >= 9) {
    return `
; This is a comment
(cond ;; This is a comment
  foo
  (bar
   1
   2) ;; This is a comment
  baz
  (qux 3
       4))
`.trim()
  }

  if (lineLength >= 1) {
    return `
; This is a comment
(cond ;; This is a comment
  foo
  (bar
   1
   2) ;; This is a comment
  baz
  (qux
   3
   4))
`.trim()
  }

  return sampleProgramWithComments
}

function formatSampleProgram(lineLength: number): string {
  if (lineLength >= sampleProgram.length || lineLength === 0)
    return sampleProgram

  if (lineLength >= 20) {
    return `
(cond foo (bar 1 2)
      baz (qux 3 4))
`.trim()
  }

  if (lineLength >= 16) {
    return `
(cond
  foo (bar 1 2)
  baz (qux 3 4))
`.trim()
  }

  if (lineLength >= 15) {
    return `
(cond
  foo
  (bar 1 2)
  baz
  (qux 3 4))
`.trim()
  }

  if (lineLength >= 14) {
    return `
(cond
  foo (bar 1
           2)
  baz (qux 3
           4))
`.trim()
  }

  if (lineLength >= 13) {
    return `
(cond
  foo
  (bar 1 2)
  baz
  (qux 3 4))
`.trim()
  }

  if (lineLength >= 10) {
    return `
(cond
  foo (bar
       1
       2)
  baz (qux
       3
       4))
`.trim()
  }

  if (lineLength >= 9) {
    return `
(cond
  foo
  (bar 1
       2)
  baz
  (qux 3
       4))
`.trim()
  }

  return `
(cond
  foo
  (bar
   1
   2)
  baz
  (qux
   3
   4))
`.trim()
}
