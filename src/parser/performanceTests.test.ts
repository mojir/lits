/* eslint-disable no-console */
import { describe, it } from 'vitest'
import { Lits } from '../Lits/Lits'

const lits = new Lits({ debug: false })

describe.skip('performance comparison', () => {
  const expressions = [
    '42', // a bit faster than eval
    '42 + 1', // a bit slower than eval
    '2 + 3 * 4',
    '5 ** 2 - 3 / 2',
    '[1, 2, 3][1]',
    '((x, y) => x + y)(2, 3)',
    '2 ** (3 + 1) - 5 / (1 + 1)',
    '2 ** (3 * 2) + 4 / (2 - 1) - 5 % 3',
    '((2 + 3) * 4 / 2 - 1) ** 2 % 5 + 6 - 7 * 8 / 9', // more than 20 times slower than eval
  ]
  const iterations = 10000

  it('compares performance of lits.run and eval', () => {
    type ReportEntry = {
      expression: string
      eval: number
      lits: number
    }
    const entries: ReportEntry[] = []

    for (const expression of expressions) {
      const report: ReportEntry = {
        expression,
        eval: 0,
        lits: 0,
      }
      entries.push(report)

      let startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        lits.run(expression)
      }
      report.lits = (performance.now() - startTime) * 1000 / iterations

      startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        // eslint-disable-next-line no-eval
        eval(expression)
      }
      report.eval = (performance.now() - startTime) * 1000 / iterations
    }

    console.log('lits.run is slower than eval by a factor', calculateFactor(entries))
  })

  it('compares performance of lits.parse + lits.evaluate and eval', () => {
    type ReportEntry = {
      expression: string
      eval: number
      lits: number
    }
    const entries: ReportEntry[] = []
    const expressionsWithTokenStreams = expressions.map((expression) => {
      const tokenStream = lits.tokenize(expression)

      return {
        expression,
        tokenStream,
      }
    })

    for (const expression of expressionsWithTokenStreams) {
      const report: ReportEntry = {
        expression: expression.expression,
        eval: 0,
        lits: 0,
      }
      entries.push(report)

      let startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        const ast = lits.parse(expression.tokenStream)
        lits.evaluate(ast, {})
      }
      report.lits = (performance.now() - startTime) * 1000 / iterations

      startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        // eslint-disable-next-line no-eval
        eval(expression.expression)
      }
      report.eval = (performance.now() - startTime) * 1000 / iterations
    }

    console.log('lits.parse + lits.evaluate is slower than eval by a factor', calculateFactor(entries))
  })

  it('compares performance of lits.evaluate and eval', () => {
    type ReportEntry = {
      expression: string
      eval: number
      lits: number
    }
    const entries: ReportEntry[] = []
    const expressionsWithAsts = expressions.map((expression) => {
      const tokenStream = lits.tokenize(expression)
      const ast = lits.parse(tokenStream)

      return {
        expression,
        ast,
      }
    })

    for (const expression of expressionsWithAsts) {
      const report: ReportEntry = {
        expression: expression.expression,
        eval: 0,
        lits: 0,
      }
      entries.push(report)

      let startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        lits.evaluate(expression.ast, {})
      }
      report.lits = (performance.now() - startTime) * 1000 / iterations

      startTime = performance.now()
      for (let i = 0; i < iterations; i++) {
        // eslint-disable-next-line no-eval
        eval(expression.expression)
      }
      report.eval = (performance.now() - startTime) * 1000 / iterations
    }

    console.log('lits.evaluate is slower than eval by a factor', calculateFactor(entries))
  })
})

function calculateFactor(entries: { eval: number, lits: number }[]) {
  return Math.round(100 * entries.reduce((acc, { eval: evalTime, lits: litsTime }) => acc + litsTime / evalTime, 0) / entries.length) / 100
}
