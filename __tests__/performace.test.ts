/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'
import { tokenize } from '../src/tokenizer'
import { parse } from '../src/parser'
import { evaluate } from '../src/evaluator'
import { Context } from '../src/evaluator/interface'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

const ITERATIONS = 25000
const program = `(+ (* (- x y) (- y x)) (* (/ x y) (/ y x)))`
const globalContext: Context = {
  variables: { x: { value: 20, constant: false }, y: { value: 30, constant: false } },
  functions: {},
}
const jsExpression = `((x - y) * (y - x)) + ((x / y) * (y / x))`

// Some baseline values for javascript eval to compare with
const startRefTime = Date.now()
for (let i = 0; i < ITERATIONS; i += 1) {
  const jsProgram = createJsProgram(jsExpression, globalContext)
  eval(jsProgram)
}
const averageRefTime = Math.round((100000 * (Date.now() - startRefTime)) / ITERATIONS) / 100

function createJsProgram(expression: string, globalContext: any) {
  const vars = Object.entries(globalContext.variables)
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

xdescribe('performace', () => {
  test('tokenise', () => {
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      tokenize(program)
    }
    logPerformace('Tokenise', Date.now() - startTime)
  })

  test('parse', () => {
    const tokens = tokenize(program)
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      parse(tokens)
    }
    logPerformace('Parse tokens', Date.now() - startTime)
  })

  test('evaluate', () => {
    const tokens = tokenize(program)
    const ast = parse(tokens)
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      evaluate(ast, globalContext, { variables: {}, functions: {} })
    }
    logPerformace('Evaluate AST', Date.now() - startTime)
  })

  test('lispish tokenize - parse - evaluate', () => {
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      lispish.run(program, { globalContext })
    }
    logPerformace('Execute program (tokenize, parse and evaluate)', Date.now() - startTime)
  })
})
