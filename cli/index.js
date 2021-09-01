#!/usr/bin/env node
const readline = require('readline')
const { lispish } = require('../dist/lispish.js')

if (process.argv[2]) {
  console.log(lispish(process.argv[2]))
  process.exit(0)
}

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: 'LISPISH> ',
})

const globalContext = {}

console.log('Type "`help" for more information.')
rl.prompt()

rl.on('line', line => {
  switch (line) {
    case '`help':
      printHelp()
      break
    case '`context':
      printContext()
      break
    default:
      try {
        const result = lispish(line, {}, globalContext)
        console.log(result)
      } catch (error) {
        console.log(error.message)
      }
  }
  rl.prompt()
}).on('close', () => {
  console.log('Over and out!')
  process.exit(0)
})

function printHelp() {
  console.log(`.context  Print the global context
.help     Print this help message
`)
}

function printContext() {
  Object.keys(globalContext)
    .sort()
    .forEach(x => console.log(`${x} = ${JSON.stringify(globalContext[x], null, 2)}`))
  console.log()
}
