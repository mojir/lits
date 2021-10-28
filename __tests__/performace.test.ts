/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lits } from '../src'
import { createContextStack, evaluate } from '../src/evaluator'
import { Context } from '../src/evaluator/interface'
import { Obj } from '../src/interface'
import { parse } from '../src/parser'
import { tokenize } from '../src/tokenizer'
import { createContextFromValues } from '../src/utils'

const ITERATIONS = 25000
const program = `(+ (* (- x y) (- y x)) (* (/ x y) (/ y x)))`
const globals: Obj = { x: 20, y: 30 }
const context = createContextFromValues(globals)
const contextStack = createContextStack([context])
const jsExpression = `((x - y) * (y - x)) + ((x / y) * (y / x))`

// Some baseline values for javascript eval to compare with
const startRefTime = Date.now()
for (let i = 0; i < ITERATIONS; i += 1) {
  const jsProgram = createJsProgram(jsExpression, context)
  eval(jsProgram)
}
const averageRefTime = Math.round((100000 * (Date.now() - startRefTime)) / ITERATIONS) / 100

function createJsProgram(expression: string, globalContext: Context) {
  const vars = Object.entries(globalContext)
    .map(entry => `  var ${entry[0]} = ${JSON.stringify(entry[1])};`)
    .join(`\n`)
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

xdescribe(`performace`, () => {
  test(`tokenise`, () => {
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      tokenize(program)
    }
    logPerformace(`Tokenise`, Date.now() - startTime)
  })

  test(`parse`, () => {
    const tokens = tokenize(program)
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      parse(tokens)
    }
    logPerformace(`Parse tokens`, Date.now() - startTime)
  })

  test(`evaluate`, () => {
    const tokens = tokenize(program)
    const ast = parse(tokens)
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      evaluate(ast, contextStack)
    }
    logPerformace(`Evaluate AST`, Date.now() - startTime)
  })

  test(`lits run - NOT chaced.`, () => {
    const lits = new Lits()
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      lits.run(program, { globals })
    }
    logPerformace(`Run program`, Date.now() - startTime)
  })

  test(`lits run - cached`, () => {
    const lits = new Lits({ astCacheSize: 100 })
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      lits.run(program, { globals })
    }
    logPerformace(`Run program (with astCache)`, Date.now() - startTime)
  })
})
