#!/usr/bin/env node
const fs = require('fs')
const readline = require('readline')
const { lispish } = require('../dist/lispish.js')

const config = processArguments(process.argv.slice(2))
if (config.expression) {
  execute(config.expression)
  process.exit(0)
} else if (config.filename) {
  const content = fs.readFileSync(config.filename, { encoding: 'utf-8' })
  execute(content)
  process.exit(0)
} else {
  runREPL()
}

function execute(expression, globalContext = {}) {
  try {
    console.log(lispish(expression, config.context, globalContext))
  } catch (error) {
    console.error(error.message)
  }
}

function processArguments(args) {
  const config = {
    filename: '',
    context: {},
    expression: '',
  }
  for (let i = 0; i < args.length; i += 2) {
    const option = args[i]
    const argument = args[i + 1]
    switch (option) {
      case '-f':
        if (!argument) {
          console.error('Missing filename after -f')
          process.exit(1)
        }
        config.filename = argument
        break
      case '-c':
        if (!argument) {
          console.error('Missing context after -c')
          process.exit(1)
        }
        try {
          config.context = JSON.parse(argument)
        } catch (e) {
          console.error(`Couldn't parse context: ${e?.message}`)
        }
        break
      case '-C':
        if (!argument) {
          console.error('Missing context filename after -C')
          process.exit(1)
        }
        try {
          const contextString = fs.readFileSync(argument, { encoding: 'utf-8' })
          config.context = JSON.parse(contextString)
        } catch (e) {
          console.error(`Couldn't parse context: ${e?.message}`)
        }
        break
      case '-e':
        if (!argument) {
          console.error('Missing lispish expression after -e')
          process.exit(1)
        }
        config.expression = argument
        break
      case '-h':
      case '--help':
        printUsage()
        process.exit(0)
      default:
        console.error(`Unknown argument "${argument}"`)
    }
  }
  if (config.filename && config.expression) {
    console.error('Cannot both specify -f and -e')
  }
  return config
}

function runREPL() {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: 'LISPISH> ',
  })

  let globalContext = {}

  console.log('Type "`help" for more information.')
  rl.prompt()

  rl.on('line', line => {
    switch (line) {
      case '`help':
        printHelp()
        break
      case '`context':
        printContext(false)
        break
      case '`Context':
        printContext(true)
        break
      case '`reset':
        globalContext = {}
        console.log('Context is now empty\n')
        break
      default:
        execute(line, globalContext)
    }
    rl.prompt()
  }).on('close', () => {
    console.log('Over and out!')
    process.exit(0)
  })
}

function printHelp() {
  console.log(`\`context  Print the global context
\`Context  Print the global context (JSON.stringify)
\`help     Print this help message
\`reset    Reset the context
`)
}

function printUsage() {
  console.log(`Usage: lispish [options]

Options:
  -c ...         Context as a JSON string
  -C ...         Context file (.json file)
  -f ...         Lispish file
  -e ...         Lispish expression
  -h, --help     Show this help
`)
}

function printContext(stringify) {
  Object.keys(globalContext)
    .sort()
    .forEach(x => {
      if (stringify) {
        console.log(`${x} = ${JSON.stringify(globalContext[x], null, 2)}`)
      } else {
        console.log(`${x} =`, globalContext[x])
      }
    })
  console.log()
}
