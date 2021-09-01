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

function execute(expression) {
  try {
    console.log(lispish(expression, config.globalContext, config.localScope))
  } catch (error) {
    console.error(error.message)
  }
}

function processArguments(args) {
  const config = {
    filename: '',
    globalContext: {},
    localScope: {},
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
          console.error('Missing global context after -s')
          process.exit(1)
        }
        try {
          config.globalContext = JSON.parse(argument)
        } catch (e) {
          console.error(`Couldn't parse global context: ${e?.message}`)
        }
        break
      case '-C':
        if (!argument) {
          console.error('Missing global context filename after -C')
          process.exit(1)
        }
        try {
          const contextString = fs.readFileSync(argument, { encoding: 'utf-8' })
          config.globalContext = JSON.parse(contextString)
        } catch (e) {
          console.error(`Couldn't parse global context: ${e?.message}`)
        }
        break
      case '-s':
        if (!argument) {
          console.error('Missing local scope after -s')
          process.exit(1)
        }
        try {
          config.localScope = JSON.parse(argument)
        } catch (e) {
          console.error(`Couldn't parse local scope: ${e?.message}`)
        }
        break
      case '-S':
        if (!argument) {
          console.error('Missing local scope filename after -C')
          process.exit(1)
        }
        try {
          const scopeString = fs.readFileSync(argument, { encoding: 'utf-8' })
          config.localScope = JSON.parse(scopeString)
        } catch (e) {
          console.error(`Couldn't parse local scope: ${e?.message}`)
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

  console.log('Type "`help" for more information.')
  rl.prompt()

  rl.on('line', line => {
    if (line.startsWith('`')) {
      switch (line) {
        case '`h':
        case '`help':
          printHelp()
          break
        case '`c':
        case '`context':
          printObj('Global context', config.globalContext, false)
          break
        case '`C':
        case '`Context':
          printObj('Global context', config.globalContext, true)
          break
        case '`s':
        case '`scope':
          printObj('Local scope', config.localScope, false)
          break
        case '`S':
        case '`Scope':
          printObj('Local scope', config.localScope, true)
          break
        case '`rc':
        case '`reset-context':
          config.globalContext = {}
          console.log('Global context is now empty\n')
          break
        case '`rs':
        case '`reset-scope':
          config.localScope = {}
          console.log('Local scope is now empty\n')
          break
        case '`q':
        case '`quit':
          rl.close()
          break
        default:
          console.error(`Unrecognized command "${line}", try "\`help"\n`)
      }
    } else if (line) {
      execute(line)
    }
    rl.prompt()
  }).on('close', () => {
    console.log('Over and out!')
    process.exit(0)
  })
}

function printHelp() {
  console.log(`\`c, \`context        Print the global context
\`C, \`Context         Print the global context (JSON.stringify)
\`rc, \`reset-context  Reset the global context
\`rs, \`reset-scope    Reset the local scope
\`s, \`scope           Print the local scope
\`S, \`Scope           Print the local scope (JSON.stringify)
\`h, \`help            Print this help message
\`q, \`quit            Quit
`)
}

function printUsage() {
  console.log(`Usage: lispish [options]

Options:
  -c ...         Global context as a JSON string
  -C ...         Global context file (.json file)
  -s ...         Local scope as a JSON string
  -S ...         Local scope file (.json file)
  -f ...         Lispish file
  -e ...         Lispish expression
  -h, --help     Show this help
`)
}

function printObj(label, obj, stringify) {
  console.log(`${label}:`)
  if (Object.keys(obj).length === 0) {
    console.log('[empty]\n')
    return
  } else {
    Object.keys(obj)
      .sort()
      .forEach(x => {
        if (stringify) {
          console.log(`${x} = ${JSON.stringify(obj[x], null, 2)}`)
        } else {
          console.log(`${x} =`, obj[x])
        }
      })
    console.log()
  }
}
