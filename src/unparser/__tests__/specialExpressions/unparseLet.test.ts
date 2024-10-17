import { describe, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { testFormatter } from '../testFormatter'

const lits = new Lits({ debug: true })

const sampleProgram = '(let [foo 3, bar 4] foo (bar 1 2) baz)'
const sampleProgramWithComments = `
;; Leading Comment
(let ;; Inline comment
 [a ;; Inline comment
  10
  b
  20 ;; Inline Comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1 2) ;; Inline comment
  baz) ;; Inline comment`.trim()

describe('unparse let', () => {
  it('should work 1', () => {
    testFormatter(
      program => lits.format(program, { lineLength: 0, debug: false }),
      sampleProgramWithComments,
      '(let [a 10, b 20] foo (bar 1 2) baz)',
    )

    testFormatter(
      program => lits.format(program, { lineLength: 30, debug: false }),
      sampleProgramWithComments,
      `
(let [a 10, b 20]
  foo
  (bar 1 2)
  baz)
`.trim(),
    )

    testFormatter(
      program => lits.format(program, { lineLength: 15, debug: false }),
      sampleProgramWithComments,
      `
(let [a 10
      b 20]
  foo
  (bar 1 2)
  baz)
`.trim(),
    )

    testFormatter(
      program => lits.format(program, { lineLength: 10, debug: false }),
      sampleProgramWithComments,
      `
(let [a
      10
      b
      20]
  foo
  (bar 1
       2)
  baz)
`.trim(),
    )

    testFormatter(
      program => lits.format(program, { lineLength: 8, debug: false }),
      sampleProgramWithComments,
      `
(let [a
      10
      b
      20]
  foo
  (bar
   1
   2)
  baz)
`.trim(),
    )
  })

  it('should work 2', () => {
    const program = '(let [] "Hi")'
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

  if (lineLength >= 27) {
    return `
;; Leading Comment
(let ;; Inline comment
 [a ;; Inline comment
  10
  b
  20 ;; Inline Comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar 1
       2) ;; Inline comment
  baz) ;; Inline comment`.trim()
  }

  return `
;; Leading Comment
(let ;; Inline comment
 [a ;; Inline comment
  10
  b
  20 ;; Inline Comment
 ] ;; Inline comment
  ;; Leading comment
  foo

  ;; Comment

  ;; Leading Comment
  (bar
   1
   2) ;; Inline comment
  baz) ;; Inline comment`.trim()
}

function formatSampleProgram(lineLength: number): string {
  if (lineLength >= sampleProgram.length || lineLength === 0)
    return `${sampleProgram}`

  if (lineLength >= 19) {
    return `
(let [foo 3, bar 4]
  foo
  (bar 1 2)
  baz)
`.trim()
  }

  if (lineLength >= 12) {
    return `
(let [foo 3
      bar 4]
  foo
  (bar 1 2)
  baz)
`.trim()
  }

  if (lineLength >= 11) {
    return `
(let [foo
      3
      bar
      4]
  foo
  (bar 1 2)
  baz)
`.trim()
  }

  if (lineLength >= 9) {
    return `
(let [foo
      3
      bar
      4]
  foo
  (bar 1
       2)
  baz)
`.trim()
  }

  return `
(let [foo
      3
      bar
      4]
  foo
  (bar
   1
   2)
  baz)
`.trim()
}
