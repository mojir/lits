#!/usr/bin/env node
/* eslint-disable node/prefer-global/process */
/* eslint-disable no-console */

import fs from 'node:fs'
import path from 'node:path'
import { version } from '../../package.json'
import { runTest } from '../../src/testFramework'
import type { Reference } from '../../reference'
import { apiReference, isFunctionReference } from '../../reference'
import { asAny } from '../../src/typeGuards/lits'
import type { UnknownRecord } from '../../src/interface'
import { stringifyValue } from '../../common/utils'
import { polishSymbolCharacterClass, polishSymbolFirstCharacterClass } from '../../src/symbolPatterns'
import type { Context } from '../../src/evaluator/interface'
import { Lits } from '../../src/Lits/Lits'
import { allBuiltinModules } from '../../src/allModules'
import '../../src/initReferenceData'
import { normalExpressionKeys, specialExpressionKeys } from '../../src/builtin'
import { bundle } from '../../src/bundler'
import { isLitsBundle } from '../../src/bundler/interface'
import type { LitsBundle } from '../../src/bundler/interface'
import { Colors, createColorizer } from './colorizer'
import { getCliFunctionSignature } from './cliDocumentation/getCliFunctionSignature'
import { getCliDocumentation } from './cliDocumentation/getCliDocumentation'
import { getInlineCodeFormatter } from './cliFormatterRules'
import { createReadlineInterface } from './createReadlineInterface'
import { getCliModules } from './js-interop/Cli'

const useColor = !process.env.NO_COLOR
const fmt = createColorizer(useColor)

const HIST_SIZE = 1000
const PROMPT = fmt.bright.gray('> ')

type Maybe<T> = T | null

// --- Option types shared across subcommands ---

interface ContextOptions {
  context: Context
}

interface PrintOptions {
  printResult: boolean
}

// --- Subcommand configs ---

interface ReplConfig {
  subcommand: 'repl'
  loadFilename: Maybe<string>
  context: Context
}

interface RunConfig {
  subcommand: 'run'
  filename: string
  context: Context
  printResult: boolean
}

interface RunBundleConfig {
  subcommand: 'run-bundle'
  filename: string
  context: Context
  printResult: boolean
}

interface EvalConfig {
  subcommand: 'eval'
  expression: string
  context: Context
  printResult: boolean
}

interface TestConfig {
  subcommand: 'test'
  filename: string
  testPattern: Maybe<string>
}

interface BundleConfig {
  subcommand: 'bundle'
  filename: string
  output: Maybe<string>
}

interface HelpConfig {
  subcommand: 'help'
}

interface VersionConfig {
  subcommand: 'version'
}

type Config = ReplConfig | RunConfig | RunBundleConfig | EvalConfig | TestConfig | BundleConfig | HelpConfig | VersionConfig

const historyResults: unknown[] = []
const formatValue = getInlineCodeFormatter(fmt)

const commands = ['`help', '`quit', '`builtins', '`context']
const expressionRegExp = new RegExp(`^(.*\\(\\s*)(${polishSymbolFirstCharacterClass}${polishSymbolCharacterClass}*)$`)
const nameRegExp = new RegExp(`^(.*?)(${polishSymbolFirstCharacterClass}${polishSymbolCharacterClass}*)$`)
const helpRegExp = new RegExp(`^\`help\\s+(${polishSymbolFirstCharacterClass}${polishSymbolCharacterClass}+)\\s*$`)
const expressions = [...normalExpressionKeys, ...specialExpressionKeys]

const config = processArguments(process.argv.slice(2))

const cliModules = getCliModules()

function createLits(context: Context) {
  const _lits = new Lits({ debug: true, modules: [...allBuiltinModules, ...cliModules] })
  return {
    run: (program: string | LitsBundle) =>
      _lits.run(program, {
        globalContext: context,
        globalModuleScope: true,
      }),
  }
}

switch (config.subcommand) {
  case 'run': {
    const lits = createLits(config.context)
    try {
      const content = fs.readFileSync(config.filename, { encoding: 'utf-8' })
      const result = lits.run(content)
      if (config.printResult) {
        console.log(result)
      }
      process.exit(0)
    }
    catch (error) {
      printErrorMessage(`${error}`)
      process.exit(1)
    }
    break
  }
  case 'run-bundle': {
    const lits = createLits(config.context)
    try {
      const content = fs.readFileSync(config.filename, { encoding: 'utf-8' })
      let parsed: unknown
      try {
        parsed = JSON.parse(content)
      }
      catch {
        printErrorMessage(`Invalid bundle: ${config.filename} is not valid JSON`)
        process.exit(1)
      }
      if (!isLitsBundle(parsed)) {
        printErrorMessage(`Invalid bundle: ${config.filename} is not a valid Lits bundle (expected "program" string and "fileModules" array)`)
        process.exit(1)
      }
      const result = lits.run(parsed)
      if (config.printResult) {
        console.log(result)
      }
      process.exit(0)
    }
    catch (error) {
      printErrorMessage(`${error}`)
      process.exit(1)
    }
    break
  }
  case 'eval': {
    const lits = createLits(config.context)
    try {
      const result = lits.run(config.expression)
      if (config.printResult) {
        console.log(result)
      }
      process.exit(0)
    }
    catch (error) {
      printErrorMessage(`${error}`)
      process.exit(1)
    }
    break
  }
  case 'bundle': {
    try {
      const absolutePath = path.resolve(config.filename)
      const result = bundle(absolutePath)
      const json = JSON.stringify(result, null, 2)
      if (config.output) {
        fs.writeFileSync(config.output, json, { encoding: 'utf-8' })
      }
      else {
        console.log(json)
      }
      process.exit(0)
    }
    catch (error) {
      printErrorMessage(`${error}`)
      process.exit(1)
    }
    break
  }
  case 'test': {
    runLitsTest(config.filename, config.testPattern)
    process.exit(0)
    break
  }
  case 'repl': {
    if (config.loadFilename) {
      const lits = createLits(config.context)
      const content = fs.readFileSync(config.loadFilename, { encoding: 'utf-8' })
      const result = lits.run(content)
      if (result !== null && typeof result === 'object' && !Array.isArray(result)) {
        for (const [key, value] of Object.entries(result as Record<string, unknown>)) {
          config.context[key] = { value: asAny(value) }
        }
      }
    }
    runREPL(config.context)
    break
  }
  case 'help': {
    printUsage()
    process.exit(0)
    break
  }
  case 'version': {
    console.log(version)
    process.exit(0)
    break
  }
}

function runLitsTest(testPath: string, testNamePattern: Maybe<string>) {
  if (!testPath.match(/\.test\.lits/)) {
    printErrorMessage('Test file must end with .test.lits')
    process.exit(1)
  }
  const { success, tap } = runTest({
    testPath,
    testNamePattern: testNamePattern !== null ? new RegExp(testNamePattern) : undefined,
  })

  console.log(`\n${tap}`)

  if (!success)
    process.exit(1)
}

function execute(expression: string, context: Context): boolean {
  const lits = createLits(context)
  try {
    const result = lits.run(expression)
    historyResults.unshift(result)
    if (historyResults.length > 9) {
      historyResults.length = 9
    }

    setReplHistoryVariables(context)
    console.log(stringifyValue(result, false))
    return true
  }
  catch (error) {
    printErrorMessage(`${error}`)
    context['*e*'] = { value: getErrorMessage(error) }
    return false
  }
}

function getErrorMessage(error: unknown) {
  if (error instanceof Error)
    return error.message

  return 'Unknown error'
}

function setReplHistoryVariables(context: Context) {
  delete context['*1*']
  delete context['*2*']
  delete context['*3*']
  delete context['*4*']
  delete context['*5*']
  delete context['*6*']
  delete context['*7*']
  delete context['*8*']
  delete context['*9*']
  historyResults.forEach((value, i) => {
    context[`*${i + 1}*`] = { value: asAny(value) }
  })
}

function parseOption(args: string[], i: number): { option: string, argument: Maybe<string>, count: number } | null {
  const option = args[i]!

  if (option === '-s') {
    return { option, argument: null, count: 1 }
  }
  if (/^-[a-z]$/i.test(option))
    return { option, argument: args[i + 1] ?? null, count: 2 }

  const match = /^(--[a-z-]+)(?:=(.*))?$/i.exec(option)
  if (match)
    return { option: match[1]!, argument: match[2] ?? null, count: 1 }

  return null
}

function parseContextOptions(args: string[], startIndex: number): { options: ContextOptions, nextIndex: number } {
  const options: ContextOptions = { context: {} }
  let i = startIndex
  while (i < args.length) {
    const parsed = parseOption(args, i)
    if (!parsed)
      break

    switch (parsed.option) {
      case '-c':
      case '--context':
        if (!parsed.argument) {
          printErrorMessage(`Missing context JSON after ${parsed.option}`)
          process.exit(1)
        }
        try {
          Object.entries(JSON.parse(parsed.argument) as UnknownRecord).forEach(([key, value]) => {
            options.context[key] = { value: asAny(value) }
          })
        }
        catch (e) {
          printErrorMessage(`Couldn\`t parse context: ${getErrorMessage(e)}`)
          process.exit(1)
        }
        i += parsed.count
        break
      case '-C':
      case '--context-file':
        if (!parsed.argument) {
          printErrorMessage(`Missing context filename after ${parsed.option}`)
          process.exit(1)
        }
        try {
          const contextString = fs.readFileSync(parsed.argument, { encoding: 'utf-8' })
          Object.entries(JSON.parse(contextString) as UnknownRecord).forEach(([key, value]) => {
            options.context[key] = { value: asAny(value) }
          })
        }
        catch (e) {
          printErrorMessage(`Couldn\`t parse context: ${getErrorMessage(e)}`)
          process.exit(1)
        }
        i += parsed.count
        break
      default:
        return { options, nextIndex: i }
    }
  }
  return { options, nextIndex: i }
}

function parsePrintOptions(args: string[], startIndex: number): { options: PrintOptions, nextIndex: number } {
  const options: PrintOptions = { printResult: true }
  let i = startIndex
  while (i < args.length) {
    const parsed = parseOption(args, i)
    if (!parsed)
      break

    switch (parsed.option) {
      case '-s':
      case '--silent':
        options.printResult = false
        i += parsed.count
        break
      default:
        return { options, nextIndex: i }
    }
  }
  return { options, nextIndex: i }
}

function parseRunEvalOptions(args: string[], startIndex: number): { context: Context, printResult: boolean, nextIndex: number } {
  let context: Context = {}
  let printResult = true
  let i = startIndex
  while (i < args.length) {
    const parsed = parseOption(args, i)
    if (!parsed)
      break

    switch (parsed.option) {
      case '-c':
      case '--context':
      case '-C':
      case '--context-file': {
        const result = parseContextOptions(args, i)
        context = { ...context, ...result.options.context }
        i = result.nextIndex
        break
      }
      case '-s':
      case '--silent': {
        const result = parsePrintOptions(args, i)
        printResult = result.options.printResult
        i = result.nextIndex
        break
      }
      default:
        printErrorMessage(`Unknown option "${parsed.option}"`)
        process.exit(1)
    }
  }
  if (i < args.length) {
    printErrorMessage(`Unknown argument "${args[i]}"`)
    process.exit(1)
  }
  return { context, printResult, nextIndex: i }
}

function processArguments(args: string[]): Config {
  // Global flags (no subcommand)
  if (args.length === 0) {
    return { subcommand: 'repl', loadFilename: null, context: {} }
  }

  const first = args[0]!

  if (first === '--help' || first === '-h') {
    return { subcommand: 'help' }
  }
  if (first === '--version') {
    return { subcommand: 'version' }
  }

  switch (first) {
    case 'run': {
      const filename = args[1]
      if (!filename || filename.startsWith('-')) {
        printErrorMessage('Missing filename after "run"')
        process.exit(1)
      }
      const { context, printResult } = parseRunEvalOptions(args, 2)
      return { subcommand: 'run', filename, context, printResult }
    }
    case 'run-bundle': {
      const filename = args[1]
      if (!filename || filename.startsWith('-')) {
        printErrorMessage('Missing filename after "run-bundle"')
        process.exit(1)
      }
      const { context, printResult } = parseRunEvalOptions(args, 2)
      return { subcommand: 'run-bundle', filename, context, printResult }
    }
    case 'eval': {
      const expression = args[1]
      if (!expression || expression.startsWith('-')) {
        printErrorMessage('Missing expression after "eval"')
        process.exit(1)
      }
      const { context, printResult } = parseRunEvalOptions(args, 2)
      return { subcommand: 'eval', expression, context, printResult }
    }
    case 'bundle': {
      const filename = args[1]
      if (!filename || filename.startsWith('-')) {
        printErrorMessage('Missing filename after "bundle"')
        process.exit(1)
      }
      let output: Maybe<string> = null
      let i = 2
      while (i < args.length) {
        const parsed = parseOption(args, i)
        if (!parsed) {
          printErrorMessage(`Unknown argument "${args[i]}"`)
          process.exit(1)
        }
        switch (parsed.option) {
          case '-o':
          case '--output':
            if (!parsed.argument) {
              printErrorMessage(`Missing output filename after ${parsed.option}`)
              process.exit(1)
            }
            output = parsed.argument
            i += parsed.count
            break
          default:
            printErrorMessage(`Unknown option "${parsed.option}" for "bundle"`)
            process.exit(1)
        }
      }
      return { subcommand: 'bundle', filename, output }
    }
    case 'test': {
      const filename = args[1]
      if (!filename || filename.startsWith('-')) {
        printErrorMessage('Missing filename after "test"')
        process.exit(1)
      }
      let testPattern: Maybe<string> = null
      let i = 2
      while (i < args.length) {
        const parsed = parseOption(args, i)
        if (!parsed) {
          printErrorMessage(`Unknown argument "${args[i]}"`)
          process.exit(1)
        }
        switch (parsed.option) {
          case '--pattern':
            if (!parsed.argument) {
              printErrorMessage(`Missing test name pattern after ${parsed.option}`)
              process.exit(1)
            }
            testPattern = parsed.argument
            i += parsed.count
            break
          default:
            printErrorMessage(`Unknown option "${parsed.option}" for "test"`)
            process.exit(1)
        }
      }
      return { subcommand: 'test', filename, testPattern }
    }
    case 'repl': {
      let loadFilename: Maybe<string> = null
      let context: Context = {}
      let i = 1
      while (i < args.length) {
        const parsed = parseOption(args, i)
        if (!parsed) {
          printErrorMessage(`Unknown argument "${args[i]}"`)
          process.exit(1)
        }
        switch (parsed.option) {
          case '-l':
          case '--load':
            if (!parsed.argument) {
              printErrorMessage(`Missing filename after ${parsed.option}`)
              process.exit(1)
            }
            loadFilename = parsed.argument
            i += parsed.count
            break
          case '-c':
          case '--context':
          case '-C':
          case '--context-file': {
            const result = parseContextOptions(args, i)
            context = { ...context, ...result.options.context }
            i = result.nextIndex
            break
          }
          default:
            printErrorMessage(`Unknown option "${parsed.option}" for "repl"`)
            process.exit(1)
        }
      }
      return { subcommand: 'repl', loadFilename, context }
    }
    case 'help': {
      return { subcommand: 'help' }
    }
    default: {
      printErrorMessage(`Unknown subcommand "${first}". Run "lits help" for usage.`)
      process.exit(1)
    }
  }
}

function runREPL(context: Context) {
  console.log(`Welcome to Lits v${version}.
Type ${fmt.italic('`help')} for more information.`)

  const rl = createReadlineInterface({
    completer,
    historySize: HIST_SIZE,
    prompt: PROMPT,
  })

  rl.prompt()

  rl.on('line', (line) => {
    line = line.trim()

    const helpMatch = helpRegExp.exec(line)
    if (helpMatch) {
      const name = helpMatch[1]!
      console.log(getCliDocumentation(fmt, name))
    }
    else if (line.startsWith('`')) {
      switch (line) {
        case '`builtins':
          printBuiltins()
          break
        case '`help':
          printHelp()
          break
        case '`context':
          printContext(context)
          break
        case '`quit':
          rl.close()
          break
        default:
          printErrorMessage(`Unrecognized command ${Colors.Italic}${line}${Colors.ResetItalic}, try ${Colors.Italic}\`help${Colors.ResetItalic}`)
      }
    }
    else if (line) {
      execute(line, context)
    }
    rl.prompt()
  }).on('close', () => {
    console.log('Over and out!')
    process.exit(0)
  })
}

function printBuiltins() {
  Object
    .values(apiReference)
    .sort((a, b) => a.title.localeCompare(b.title))
    .forEach((reference) => {
      console.log(`
${fmt.bright.blue(reference.title)} - ${fmt.gray(reference.category)}
${getDocString(reference)}`)
    })
}

function getDocString(reference: Reference) {
  if (isFunctionReference(reference))
    return `${getCliFunctionSignature(fmt, reference)}`
  return ''
}

function printHelp() {
  console.log(`
\`builtins                 Print all builtin functions
\`context                  Print context
\`help                     Print this help message
\`help [builtin function]  Print help for [builtin function]
\`quit                     Quit
`.trim())
}

function printUsage() {
  console.log(`
Usage: lits [subcommand] [options]

Subcommands:
  run <file> [options]            Run a .lits file
  run-bundle <file> [options]     Run a .json bundle
  eval <expression> [options]     Evaluate a Lits expression
  bundle <entry> [options]        Bundle a multi-file project
  test <file> [options]           Run a .test.lits test file
  repl [options]                  Start an interactive REPL
  help                            Show this help

Run/Run-bundle/Eval options:
  -c, --context=<json>            Context as a JSON string
  -C, --context-file=<file>       Context from a .json file
  -s, --silent                    Suppress printing the result

Bundle options:
  -o, --output=<file>             Write bundle to file (default: stdout)

Test options:
  --pattern=<regex>               Only run tests matching pattern

Repl options:
  -l, --load=<file>               Preload a .lits file into the REPL context
  -c, --context=<json>            Context as a JSON string
  -C, --context-file=<file>       Context from a .json file

Global options:
  -h, --help                      Show this help
  --version                       Print lits version

With no subcommand, starts an interactive REPL.
`.trim())
}

function printContext(context: Context) {
  const keys = Object.keys(context)

  if (keys.length === 0) {
    console.log('[empty]\n')
  }
  else {
    keys.sort().forEach((x) => {
      console.log(`${x} = ${formatValue(stringifyValue(context[x]!.value, false))}`)
    })
    console.log()
  }
}

function completer(line: string) {
  const helpMatch = line.match(/`help\s+(.*)/)
  if (helpMatch)
    return [expressions.filter(c => c.startsWith(helpMatch[1]!)).map(c => `\`help ${c} `), line]

  if (line.startsWith('`'))
    return [commands.filter(c => c.startsWith(line)).map(c => `${c} `), line]

  const expressionMatch = expressionRegExp.exec(line)

  if (expressionMatch)
    return [expressions.filter(c => c.startsWith(expressionMatch[2]!)).map(c => `${expressionMatch[1]}${c} `), line]

  // TODO, add reserved names
  const context = (config as ReplConfig).context ?? {}
  const names = Array.from(new Set([...Object.keys(context)]))
  const nameMatch = nameRegExp.exec(line)

  if (nameMatch)
    return [names.filter(c => c.startsWith(nameMatch[2]!)).map(c => `${nameMatch[1]}${c} `), line]

  return [[], line]
}

function printErrorMessage(message: string) {
  console.error(fmt.bright.red(message))
}
