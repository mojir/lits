/* eslint-disable no-console */
import { describe, it } from 'vitest'
import { Lits } from '../src'
import { evaluate } from '../src/evaluator'
import { ContextStackImpl } from '../src/evaluator/ContextStack'
import type { Obj } from '../src/interface'
import { parse } from '../src/parser'
import { tokenize } from '../src/tokenizer'

const ITERATIONS = 25000
const program = '(+ (* (- x y) (- y x)) (* (/ x y) (/ y x)))'
const hostValues: Obj = { x: 20, y: 30 }
const contextStack = new ContextStackImpl({ contexts: [{}], values: hostValues })
const jsExpression = '((x - y) * (y - x)) + ((x / y) * (y / x))'

// Some baseline values for javascript eval to compare with
const startRefTime = Date.now()
for (let i = 0; i < ITERATIONS; i += 1) {
  const jsProgram = createJsProgram(jsExpression, hostValues)
  // eslint-disable-next-line no-eval
  eval(jsProgram)
}
const averageRefTime = Math.round((100000 * (Date.now() - startRefTime)) / ITERATIONS) / 100

function createJsProgram(expression: string, globalContext: Record<string, unknown>) {
  const vars = Object.entries(globalContext)
    .map(entry => `  var ${entry[0]} = ${JSON.stringify(entry[1])};`)
    .join('\n')
  return `
(function () {
${vars}
return ${expression}
})()
`
}

function logPerformace(label: string, time: number) {
  const averageTime = Math.round((100000 * time) / ITERATIONS) / 100
  console.log(`
${label}: ${averageTime} µs
Factor: ${Math.round((100 * averageTime) / averageRefTime) / 100} (${averageRefTime} µs)`)
}

describe.skip('performace', () => {
  it('tokenise', () => {
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1)
      tokenize(program, { debug: true, polish: true })

    logPerformace('Tokenise', Date.now() - startTime)
  })

  it('parse', () => {
    const tokens = tokenize(program, { debug: true, polish: true })
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1)
      parse(tokens)

    logPerformace('Parse tokens', Date.now() - startTime)
  })

  it('evaluate', () => {
    const tokens = tokenize(program, { debug: true, polish: true })
    const ast = parse(tokens)
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1)
      evaluate(ast, contextStack)

    logPerformace('Evaluate AST', Date.now() - startTime)
  })

  it('lits run - NOT cached.', () => {
    const lits = new Lits({ polish: true })
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1)
      lits.run(program, { values: hostValues })

    logPerformace('Run program', Date.now() - startTime)
  })

  it('lits run - cached', () => {
    const lits = new Lits({ astCacheSize: 100 })
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1)
      lits.run(program, { values: hostValues })

    console.log(lits.getRuntimeInfo())
    logPerformace('Run program (with astCache)', Date.now() - startTime)
  })
})
