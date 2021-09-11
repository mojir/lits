#!/usr/bin/env node
const colors = require('colors')
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
} = require('../dist/lispish.js')
const reference = require('./reference.js')

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
const helpRegExp = /^`help\s+([0-9a-zA-Z_^?=!$%&<>.+*/\-[\]]+)\s*$/
const expressions = [...normalExpressionKeys, ...specialExpressionKeys]

const historyDir = path.join(homeDir, '.config')
const historyFile = path.join(historyDir, 'lispish_history.txt')

const config = processArguments(process.argv.slice(2))

if (!config.colors) {
  colors.disable()
}

if (config.help) {
  console.log(getFullDocumentation(config.help))
  process.exit(0)
}

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
    console.log(formatValue(result))
  } catch (error) {
    console.log(error.message ? error.message.brightRed : 'ERROR!'.brightRed)
  }
}

function executeExample(expression) {
  const outputs = []
  const oldLog = console.log
  console.log = (...values) => outputs.push(values.map(value => formatValue(value, true)))
  try {
    const result = lispish(expression, config.globalVariables, config.topScope)
    const outputString = outputs.map(output => `console.log(${output.join(', ')})`).join('  ').gray
    return `${formatValue(result)}    ${outputString}`
  } catch (error) {
    return 'ERROR!'.brightRed
  } finally {
    console.log = oldLog
  }
}

function stringifyValue(value, indent) {
  return JSON.stringify(
    value,
    (k, v) => (v === undefined ? 'b234ca78-ccc4-5749-9384-1d3415d29423' : v),
    indent ? 2 : undefined,
  ).replace(/"b234ca78-ccc4-5749-9384-1d3415d29423"/g, 'undefined')
}

function formatValue(value, noColors) {
  if (noColors) {
    if (isLispishFunction(value)) {
      return functionToString(value)
    }

    if (
      typeof value === 'string' ||
      Array.isArray(value) ||
      (value !== null && typeof value === 'object' && !(value instanceof RegExp))
    ) {
      return `${stringifyValue(value)}`
    }

    return `${value}`
  } else {
    if (isLispishFunction(value)) {
      return functionToString(value).white
    }

    if (value === null || value === undefined) {
      return `${value}`.blue
    }
    if (value === true) {
      return `${value}`.green
    }
    if (value === false) {
      return `${value}`.red
    }
    if (typeof value === 'string') {
      return `${stringifyValue(value)}`.yellow
    }
    if (typeof value === 'number') {
      return `${value}`.brightMagenta
    }
    if (Array.isArray(value)) {
      return `${stringifyValue(value)}`.cyan
    }
    if (typeof value === 'object') {
      if (value instanceof RegExp) {
        return `${value}`.yellow
      } else {
        return `${stringifyValue(value)}`.brightGreen
      }
    }
  }
}

function processArguments(args) {
  const config = {
    colors: true,
    filename: '',
    globalVariables: {},
    topScope: { variables: {}, functions: {} },
    expression: '',
    help: undefined,
  }
  for (let i = 0; i < args.length; i += 2) {
    const option = args[i]
    const argument = args[i + 1]
    switch (option) {
      case '--no-colors':
        i -= 1 // no argument
        config.colors = false
        break
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
        if (argument) {
          config.help = argument
        } else {
          printUsage()
          process.exit(0)
        }
        break
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
    prompt: 'LISPISH> '.brightWhite.bold,
    completer,
    next: function (rl) {
      console.log('Type "`help" for more information.')
      rl.prompt()

      rl.on('line', line => {
        line = line.trim()

        const helpMatch = helpRegExp.exec(line)
        if (helpMatch) {
          const name = helpMatch[1]
          console.log(getFullDocumentation(name))
        } else if (line.startsWith('`')) {
          switch (line) {
            case '`builtins':
              printBuiltins()
              break
            case '`help':
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
  const all = [
    ...normalExpressionKeys.map(name => ({ name, special: false })),
    ...specialExpressionKeys.map(name => ({ name, special: true })),
  ].sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0))

  const maxLength = all.reduce((max, current) => {
    return Math.max(max, current.name.length)
  }, 0)

  console.log(
    `${'Builtins (* = special expression):'.underline}\n${all
      .map(entry => {
        const prefix = entry.special ? '* ' : '  '
        const name = entry.name.padEnd(maxLength + 2, ' ')
        return `${prefix}${name}  ${getDocString(entry.name)}`
      })
      .join('\n')}\n`,
  )
}

function getDocString(name) {
  const doc = reference[name]
  if (!doc) {
    return ''
  }

  return `${doc.syntax}   ${doc.shortDescription.gray.italic}`
}

function getFullDocumentation(name) {
  const doc = reference[name]
  if (!doc) {
    return `No documentation available for ${name.bold}`
  }

  const header = `${doc.specialExpression ? 'Special expression' : 'Function'} ${name.bold}`.underline.brightWhite

  return `${header}

${doc.longDescription.italic}

${'Syntax'.underline}
  ${doc.syntax.bold}

${'Arguments'.underline}
${doc.arguments.length === 0 ? '  None'.italic : doc.arguments.map(arg => `  ${arg.name.bold}: ${arg.type}`).join('\n')}

${'Side effects'.underline}
${doc.sideEffects.length === 0 ? '  None'.italic : doc.sideEffects.map(sideEffect => `  ${sideEffect}`).join('\n')}

${'Examples'.underline}
${
  doc.examples.length === 0
    ? '[no examples]'
    : doc.examples.map(example => `  ${example} ${'=>'.gray} ${executeExample(example)}`).join('\n')
}
`
}

function printHelp() {
  console.log(`\`builtins                 Print all builtin functions
\`globalVariables          Print all global variables
\`GlobalVariables          Print all global variables (JSON.stringify)
\`topScope                 Print top scope
\`TopScope                 Print top scope (JSON.stringify)
\`resetGlobalVariables     Reset global variables
\`resetTopScope            Reset top scope
\`help                     Print this help message
\`help [builtin function]  Print help for [builtin function]
\`quit                     Quit
`)
}

function printUsage() {
  console.log(`Usage: lispish [options]

Options:
  -g ...                          Global variables as a JSON string
  -G ...                          Global variables file (.json file)
  -f ...                          .lispish file
  -e ...                          Lispish expression
  -h, --help                      Show this help
  -h, --help <builtin function>   Show help for <builtin function>
  -v, --version                   Print lispish version
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
          console.log(`${x} = ${stringifyValue(obj[x], true)}`)
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

function completer(line) {
  const helpMatch = line.match(/`help\s+(.*)/)
  if (helpMatch) {
    return [expressions.filter(c => c.startsWith(helpMatch[1])).map(c => `\`help ${c} `), line]
  }

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
