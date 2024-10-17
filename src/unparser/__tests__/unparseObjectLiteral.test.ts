import { describe, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { testFormatter } from './testFormatter'

const lits = new Lits({ debug: true })
const sampleProgram = '(merge {:a 1, :b 2} {:foo {:x 42, :y 144}, :foobar {}})'
const sampleProgramWithComments = `
(merge
 ;; Fist object
 {:a 1, :b 2} ;; Inline comment
 ;; Second object
 {:foo ;; Key
  {:x 42 ;; Inline comment
   :y 144}

  ;; Add more here

  ;; Last key-value pair
  :foobar
  {}})
`.trim()

describe('unparseObjectLitteral', () => {
  it('should unparse empty object', () => {
    const program = '{}'
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  it('should work 0.9', () => {
    const program = `
{:x 1
 :y 2
 :z 3}
`.trim()
    testFormatter(
      p => lits.format(p, { lineLength: 12 }),
      program,
      `
{:x 1
 :y 2
 :z 3}
`.trim(),
    )
  })

  it('should work 1', () => {
    const program = '{:x 1, :y 2, :z 3}'
    testFormatter(
      p => lits.format(p, { lineLength: 12 }),
      program,
      `
{:x 1
 :y 2
 :z 3}
`.trim(),
    )
  })

  it('should work 1.5', () => {
    const program = `
{:x 1
 :y 2
 :z 3}
`.trim()
    testFormatter(
      p => lits.format(p, { lineLength: 12 }),
      program,
      `
{:x 1
 :y 2
 :z 3}
`.trim(),
    )
  })

  it('should work 2', () => {
    const program = '{:a {:x 1, :y 2, :z 3}}'
    testFormatter(
      p => lits.format(p, { lineLength: 12 }),
      program,
      `
{:a {:x 1
     :y 2
     :z 3}}
`.trim(),
    )
  })

  it('should work 3', () => {
    const program = '{:a {:x 1, :y 2, :z 3}, :b {:x2 1, :y2 2, :z2 3}}'
    testFormatter(
      p => lits.format(p, { lineLength: 1 }),
      program,
      `
{:a
 {:x
  1
  :y
  2
  :z
  3}
 :b
 {:x2
  1
  :y2
  2
  :z2
  3}}
`.trim(),
    )

    testFormatter(
      p => lits.format(p, { lineLength: 23 }),
      program,
      `
{:a {:x 1, :y 2, :z 3}
 :b {:x2 1
     :y2 2
     :z2 3}}
`.trim(),
    )

    testFormatter(
      p => lits.format(p, { lineLength: 26 }),
      program,
      `
{:a {:x 1, :y 2, :z 3}
 :b {:x2 1, :y2 2, :z2 3}}
`.trim(),
    )
  })

  it('should work 4', () => {
    const program = `
{:foo ;; Key
 {:x 42 ;; Inline comment
  :y 144}}
`.trim()
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  it('should work 5', () => {
    const program = `
{ ;; Inline comment
 :foo 1
 :bar 2}
`.trim()
    testFormatter(
      p => lits.format(p),
      program,
      program,
    )
  })

  describe('unparse sample program.', () => {
    for (let lineLength = 0; lineLength <= sampleProgram.length + 1; lineLength += 1) {
      it(`should unparse with line length ${lineLength}`, () => {
        testFormatter(
          p => lits.format(p, { lineLength }),
          sampleProgram,
          formatSampleProgram(lineLength),
        )
      })
    }
  })

  describe('unparse sample program with comments.', () => {
    // for (let lineLength = 80; lineLength <= sampleProgramWithComments.length + 1; lineLength += 1) {
    for (let lineLength = 0; lineLength <= 80; lineLength += 1) {
      it(`should unparse with line length ${lineLength}`, () => {
        testFormatter(
          p => lits.format(p, { lineLength }),
          sampleProgramWithComments,
          formatSampleProgramWithComments(lineLength),
        )
      })
    }
  })
})

function formatSampleProgramWithComments(lineLength: number): string {
  if (lineLength >= 31 || lineLength === 0)
    return sampleProgramWithComments

  if (lineLength >= 26) {
    return `
(merge
 ;; Fist object
 {:a 1
  :b 2} ;; Inline comment
 ;; Second object
 {:foo ;; Key
  {:x 42 ;; Inline comment
   :y 144}

  ;; Add more here

  ;; Last key-value pair
  :foobar
  {}})
`.trim()
  }

  if (lineLength >= 25) {
    return `
(merge
 ;; Fist object
 {:a 1
  :b 2} ;; Inline comment
 ;; Second object
 {:foo ;; Key
  {:x
   42 ;; Inline comment
   :y
   144}

  ;; Add more here

  ;; Last key-value pair
  :foobar
  {}})
`.trim()
  }

  return `
(merge
 ;; Fist object
 {:a
  1
  :b
  2} ;; Inline comment
 ;; Second object
 {:foo ;; Key
  {:x
   42 ;; Inline comment
   :y
   144}

  ;; Add more here

  ;; Last key-value pair
  :foobar
  {}})
`.trim()
}

function formatSampleProgram(lineLength: number): string {
  if (lineLength >= sampleProgram.length || lineLength === 0)
    return sampleProgram

  if (lineLength >= 42) {
    return `
(merge {:a 1, :b 2}
       {:foo {:x 42, :y 144}, :foobar {}})
`.trim()
  }

  if (lineLength >= 35) {
    return `
(merge
 {:a 1, :b 2}
 {:foo {:x 42, :y 144}, :foobar {}})
`.trim()
  }

  if (lineLength >= 22) {
    return `
(merge
 {:a 1, :b 2}
 {:foo {:x 42, :y 144}
  :foobar {}})
`.trim()
  }

  if (lineLength >= 15) {
    return `
(merge
 {:a 1, :b 2}
 {:foo {:x 42
        :y 144}
  :foobar {}})
`.trim()
  }

  if (lineLength >= 13) {
    return `
(merge
 {:a 1, :b 2}
 {:foo {:x
        42
        :y
        144}
  :foobar {}})
`.trim()
  }

  if (lineLength >= 10) {
    return `
(merge
 {:a 1
  :b 2}
 {:foo
  {:x 42
   :y 144}
  :foobar
  {}})
`.trim()
  }

  if (lineLength >= 7) {
    return `
(merge
 {:a 1
  :b 2}
 {:foo
  {:x
   42
   :y
   144}
  :foobar
  {}})
`.trim()
  }

  return `
(merge
 {:a
  1
  :b
  2}
 {:foo
  {:x
   42
   :y
   144}
  :foobar
  {}})
`.trim()
}
