#!/usr/bin/env node
const readline = require('readline')
const path = require('path')
const fs = require('fs')
const homeDir = require('os').homedir()
const { lispish, normalExpressionKeys, specialExpressionKeys, reservedNames } = require('../dist/lispish.js')

const historyDir = path.join(homeDir, '.config')
const historyFile = path.join(historyDir, 'lispish_history.txt')

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
    console.log(lispish(expression, config.globalContext, config.replScope))
  } catch (error) {
    console.error(error.message)
  }
}

function processArguments(args) {
  const config = {
    filename: '',
    globalContext: {},
    replScope: {},
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
          process.exit(1)
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
          process.exit(1)
        }
        break
      case '-s':
        if (!argument) {
          console.error('Missing repl scope after -s')
          process.exit(1)
        }
        try {
          config.replScope = JSON.parse(argument)
        } catch (e) {
          console.error(`Couldn't parse repl scope: ${e?.message}`)
          process.exit(1)
        }
        break
      case '-S':
        if (!argument) {
          console.error('Missing repl scope filename after -C')
          process.exit(1)
        }
        try {
          const scopeString = fs.readFileSync(argument, { encoding: 'utf-8' })
          config.replScope = JSON.parse(scopeString)
        } catch (e) {
          console.error(`Couldn't parse repl scope: ${e?.message}`)
          process.exit(1)
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
  createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: 'LISPISH> ',
    completer,
    next: function (rl) {
      console.log('Type "`help" for more information.')
      rl.prompt()

      rl.on('line', line => {
        line = line.trim()
        if (line.startsWith('`')) {
          switch (line) {
            case '`help':
              printHelp()
              break
            case '`context':
              printObj('Global context', config.globalContext, false)
              break
            case '`Context':
              printObj('Global context', config.globalContext, true)
              break
            case '`scope':
              printObj('Repl scope', config.replScope, false)
              break
            case '`Scope':
              printObj('Repl scope', config.replScope, true)
              break
            case '`reset-context':
              config.globalContext = {}
              console.log('Global context is now empty\n')
              break
            case '`reset-scope':
              config.replScope = {}
              console.log('Repl scope is now empty\n')
              break
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
      })
        .on('close', () => {
          console.log('Over and out!')
          process.exit(0)
        })
        .on('history', history => {
          console.log('HISTORY', history)
        })
    },
  })
}

function printHelp() {
  console.log(`\`context        Print the global context
\`Context         Print the global context (JSON.stringify)
\`scope           Print the repl scope
\`Scope           Print the repl scope (JSON.stringify)
\`reset-context   Reset the global context
\`reset-scope     Reset the repl scope
\`help            Print this help message
\`quit            Quit
`)
}

function printUsage() {
  console.log(`Usage: lispish [options]

Options:
  -c ...         Global context as a JSON string
  -C ...         Global context file (.json file)
  -s ...         Repl scope as a JSON string
  -S ...         Repl scope file (.json file)
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

const commands = ['`help', '`quit', '`context', '`Context', '`scope', '`Scope', '`reset-context', '`reset-scope']
const expressionRegExp = /^(.*\(\s*)([0-9a-zA-Z_^?=!#$%&<>.+*/\-[\]]*)$/
const nameRegExp = /^(.*)([0-9a-zA-Z_^?=!#$%&<>.+*/\-[\]]*)$/
const expressions = [...normalExpressionKeys, ...specialExpressionKeys]

function completer(line) {
  if (line.startsWith('`')) {
    return [commands.filter(c => c.startsWith(line)).map(c => `${c} `), line]
  }

  const expressionMatch = expressionRegExp.exec(line)
  if (expressionMatch) {
    return [expressions.filter(c => c.startsWith(expressionMatch[2])).map(c => `${expressionMatch[1]}${c} `), line]
  }

  const names = Array.from(
    new Set([...reservedNames, ...Object.keys(config.replScope), ...Object.keys(config.globalContext)]),
  )
  const nameMatch = nameRegExp.exec(line)
  if (nameMatch) {
    return [names.filter(c => c.startsWith(nameMatch[2])).map(c => `${nameMatch[1]}${c} `), line]
  }

  return [[], line]
}

function isHistoryEnabled() {
  if (fs.existsSync(historyFile)) {
    return true
  }

  try {
    fs.openSync(historyFile)
  } catch {
    console.error(`No history for you!
If you would like to enable history persistence, make sure the directory "${path.resolve(
      historyDir,
    )}" exists and is writable.
`)
    return false
  }
}

function createInterface(options) {
  const historyEnabled = isHistoryEnabled()
  const history = historyEnabled
    ? fs.readFileSync(historyFile, 'utf8').toString().split('\n').slice(0, -1).reverse().slice(0, 100)
    : []

  readline.kHistorySize = Math.max(readline.kHistorySize, 100)

  const rl = readline.createInterface(options)

  if (historyEnabled) {
    const oldAddHistory = rl._addHistory

    rl._addHistory = function () {
      const last = rl.history[0]
      const line = oldAddHistory.call(rl)

      if (line.length > 0 && line != last) {
        fs.appendFileSync(historyFile, line + '\n')
      }
      return line
    }

    if (rl.history instanceof Array) {
      rl.history.push(...history)
    }
  }

  options.next(rl)
}
