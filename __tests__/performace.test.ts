/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Lispish } from '../src'
import { evaluate } from '../src/evaluator'
import { Context } from '../src/evaluator/interface'
import { parse } from '../src/parser'
import { tokenize } from '../src/tokenizer'

const ITERATIONS = 25000
const program = `(+ (* (- x y) (- y x)) (* (/ x y) (/ y x)))`
const globalContext: Context = { x: { value: 20 }, y: { value: 30 } }
const jsExpression = `((x - y) * (y - x)) + ((x / y) * (y / x))`

// Some baseline values for javascript eval to compare with
const startRefTime = Date.now()
for (let i = 0; i < ITERATIONS; i += 1) {
  const jsProgram = createJsProgram(jsExpression, globalContext)
  eval(jsProgram)
}
const averageRefTime = Math.round((100000 * (Date.now() - startRefTime)) / ITERATIONS) / 100

function createJsProgram(expression: string, globalContext: any) {
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
      evaluate(ast, globalContext, {})
    }
    logPerformace(`Evaluate AST`, Date.now() - startTime)
  })

  test(`lispish run - NOT chaced.`, () => {
    const lispish = new Lispish()
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      lispish.run(program, { globalContext })
    }
    logPerformace(`Run program`, Date.now() - startTime)
  })

  test(`lispish run - cached`, () => {
    const lispish = new Lispish({ astCacheSize: 100 })
    const startTime = Date.now()
    for (let i = 0; i < ITERATIONS; i += 1) {
      lispish.run(program, { globalContext })
    }
    logPerformace(`Run program (with astCache)`, Date.now() - startTime)
  })
})
