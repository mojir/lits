#!/usr/bin/env node
const { version } = require('../package.json')
const readline = require('readline')
const path = require('path')
const fs = require('fs')
const homeDir = require('os').homedir()
const {
  lispish,
  normalExpressionKeys,
  specialExpressionKeys,
  reservedNames,
  isLispishFunction,
  LispishFunction,
} = require('../dist/lispish.js')

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
    const result = lispish(expression, config.globalVariables, config.topScope)
    console.log(isLispishFunction(result) ? functionToString(result) : result)
  } catch (error) {
    console.error(error.message)
  }
}

function processArguments(args) {
  const config = {
    filename: '',
    globalVariables: {},
    topScope: { variables: {}, functions: {} },
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
      case '-g':
        if (!argument) {
          console.error('Missing global variables after -g')
          process.exit(1)
        }
        try {
          config.globalVariables = JSON.parse(argument)
        } catch (e) {
          console.error(`Couldn't parse global variables: ${e?.message}`)
          process.exit(1)
        }
        break
      case '-G':
        if (!argument) {
          console.error('Missing global variables filename after -G')
          process.exit(1)
        }
        try {
          const contextString = fs.readFileSync(argument, { encoding: 'utf-8' })
          config.globalVariables = JSON.parse(contextString)
        } catch (e) {
          console.error(`Couldn't parse global variables: ${e?.message}`)
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
      case '-v':
      case '--version':
        console.log(version)
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
            case '`builtins':
              printBuiltins()
              break
            case '` (* = special expression)help':
              printHelp()
              break
            case '`globalVariables':
              printObj('Global variables', config.globalVariables, false)
              break
            case '`GlobalVariables':
              printObj('Global variables', config.globalVariables, true)
              break
            case '`topScope':
              printObj('Top scope variables', config.topScope.variables, false)
              printObj('Top scope functions', config.topScope.functions, false)
              break
            case '`TopScope':
              printObj('Top scope variables', config.topScope.variables, true)
              printObj('Top scope functions', config.topScope.functions, true)
              break
            case '`resetGlobalVariables':
              config.globalVariables = {}
              console.log('Global variables is now empty\n')
              break
            case '`resetTopScope':
              config.topScope = {}
              console.log('Top scope is now empty\n')
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

function printBuiltins() {
  console.log(
    `Builtins (* = special expression):\n${[
      ...normalExpressionKeys.map(name => ({ name, special: false })),
      ...specialExpressionKeys.map(name => ({ name, special: true })),
    ]
      .sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0))
      .map(entry => `${entry.special ? '* ' : '  '}${entry.name}`)
      .join('\n')}`,
  )
}

function printHelp() {
  console.log(`\`globalVariables        Print all global variables
\`GlobalVariables        Print all global variables (JSON.stringify)
\`topScope               Print top scope
\`TopScope               Print top scope (JSON.stringify)
\`resetGlobalVariables   Reset global variables
\`resetTopScope          Reset top scope
\`help                   Print this help message
\`quit                   Quit
`)
}

function printUsage() {
  console.log(`Usage: lispish [options]

Options:
  -g ...         Global variables as a JSON string
  -G ...         Global variables file (.json file)
  -f ...         .lispish file
  -e ...         Lispish expression
  -h, --help     Show this help
  -v, --version  Print lispish version
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
          console.log(`${x} =`, isLispishFunction(obj[x]) ? functionToString(obj[x]) : obj[x])
        }
      })
    console.log()
  }
}

function functionToString(fun) {
  if (fun.builtin) {
    return `<BUILTIN FUNCTION ${fun.builtin}>`
  } else {
    return `<FUNCTION ${fun.name ?? 'λ'} (${fun.arguments.join(' ')})>`
  }
}

const commands = [
  '`help',
  '`quit',
  '`builtins',
  '`globalVariables',
  '`GlobalVariables',
  '`topScope',
  '`TopScope',
  '`resetGlobalVariables',
  '`resetTopScope',
]
const expressionRegExp = /^(.*\(\s*)([0-9a-zA-Z_^?=!$%&<>.+*/\-[\]]*)$/
const nameRegExp = /^(.*)([0-9a-zA-Z_^?=!$%&<>.+*/\-[\]]*)$/
const expressions = [...normalExpressionKeys, ...specialExpressionKeys]

function completer(line) {
  if (line.startsWith('`')) {
    return [commands.filter(c => c.startsWith(line)).map(c => `${c} `), line]
  }

  const expressionMatch = expressionRegExp.exec(line)
  if (expressionMatch) {
    return [
      [...expressions, ...Object.keys(config.topScope.functions)]
        .filter(c => c.startsWith(expressionMatch[2]))
        .map(c => `${expressionMatch[1]}${c} `),
      line,
    ]
  }

  const names = Array.from(
    new Set([...reservedNames, ...Object.keys(config.topScope), ...Object.keys(config.globalVariables)]),
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
    fs.openSync(historyFile, 'w')
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
