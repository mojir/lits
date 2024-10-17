import { describe, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { testFormatter } from './testFormatter'

const lits = new Lits({ debug: true })

describe('unparseNormalExpression', () => {
  it('should work 1', () => {
    const program = '(rand!)'
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  it('should work 2', () => {
    const program = `
(flatten (range
          10))
`.trim()
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  it('should work 3', () => {
    const program = `
[(+ 1)]
`.trim()
    testFormatter(
      p => lits.format(p, { lineLength: 1 }),
      program,
  `
[(+
  1)]
`.trim(),
    )
  })

  it('should work 4', () => {
    const program = `
(+
 ;; comment

 1
)
`.trim()
    const expected = `
(+
 ;; comment

 1)
`.trim()

    testFormatter(
      p => lits.format(p),
      program,
      expected,
    )
  })

  it('should work 5', () => {
    const program = `
(+ 1

   ;; Comment

   2

   3
)
`.trim()
    const expected = `
(+ 1

   ;; Comment

   2

   3)
`.trim()
    testFormatter(
      p => lits.format(p),
      program,
      expected,
    )
  })

  it('should work 6', () => {
    const program = `
(+

 1

 2)
`.trim()

    const expected = `
(+

 1

 2)
`.trim()

    testFormatter(
      p => lits.format(p),
      program,
      expected,
    )
  })

  it('should work 7', () => {
    const program = `
( ;; Inline comment
 +
 1
 2
)
`.trim()
    const expected = `
( ;; Inline comment
 +
 1
 2)
`.trim()
    testFormatter(
      p => lits.format(p),
      program,
      expected,
    )
  })
})
