import { describe, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { testFormatter } from '../testFormatter'

const lits = new Lits({ debug: true })

const sampleProgram = '(if-let [foo 3] foo (bar 1 2))'
const sampleProgramWithComments = `
;; Leading Comment
(if-let ;; Inline comment
 [a ;; Inline comment
  10 ;; Inline comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1 2)) ;; Inline comment`.trim()

describe('unparse if-let', () => {
  it('should work 1', () => {
    testFormatter(
      program => lits.format(program, { lineLength: 0, debug: false }),
      sampleProgramWithComments,
      '(if-let [a 10] foo (bar 1 2))',
    )

    testFormatter(
      program => lits.format(program, { lineLength: 20, debug: false }),
      sampleProgramWithComments,
      `
(if-let [a 10]
  foo
  (bar 1 2))
`.trim(),
    )

    testFormatter(
      program => lits.format(program, { lineLength: 13, debug: false }),
      sampleProgramWithComments,
      `
(if-let [a
         10]
  foo
  (bar 1 2))
`.trim(),
    )

    testFormatter(
      program => lits.format(program, { lineLength: 1, debug: false }),
      sampleProgramWithComments,
      `
(if-let [a
         10]
  foo
  (bar
   1
   2))
`.trim(),
    )
  })

  it('should work 2', () => {
    const program = '(if-let [a 1] "Hi")'
    testFormatter(
      p => lits.format(p, { lineLength: 0 }),
      program,
      program,
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

  if (lineLength >= 20) {
    return `
;; Leading Comment
(if-let ;; Inline comment
 [a ;; Inline comment
  10 ;; Inline comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1 2)) ;; Inline comment`.trim()
  }

  return `
;; Leading Comment
(if-let ;; Inline comment
 [a ;; Inline comment
  10 ;; Inline comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar
   1
   2)) ;; Inline comment`.trim()
}

function formatSampleProgram(lineLength: number): string {
  if (lineLength >= sampleProgram.length || lineLength === 0)
    return `${sampleProgram}`

  if (lineLength >= 15) {
    return `
(if-let [foo 3]
  foo
  (bar 1 2))
`.trim()
  }

  if (lineLength >= 11) {
    return `
(if-let [foo
         3]
  foo
  (bar 1 2))
`.trim()
  }

  if (lineLength >= 9) {
    return `
(if-let [foo
         3]
  foo
  (bar 1
       2))
`.trim()
  }

  return `
(if-let [foo
         3]
  foo
  (bar
   1
   2))
`.trim()
}
