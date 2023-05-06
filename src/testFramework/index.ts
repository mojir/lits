/* eslint-disable @typescript-eslint/no-var-requires */
import { AbstractLitsError } from '../errors'
import { Lits } from '../Lits/Lits'
import { getCodeMarker } from '../utils/helpers'

const fs = require(`fs`)
const path = require(`path`)

type TestChunk = {
  name: string
  program: string
  directive: `SKIP` | null
}

export type RunTestParams = {
  testPath: string
  testNamePattern?: RegExp
}

export type TestResult = {
  /**
   * Test report
   * http://testanything.org/
   */
  tap: string
  success: boolean
}

function getIncludesLocation(line: number, col: number, sourceMappign: IncludesResult[`sourceMapping`]): string {
  for (const fileInfo of sourceMappign) {
    if (line <= fileInfo.start + fileInfo.size) {
      return `${fileInfo.file}:${line - fileInfo.start + 1}:${col}`
    }
  }
  /* istanbul ignore next */
  throw Error(`Broken source code mapping`)
}

export function runTest({ testPath, testNamePattern }: RunTestParams): TestResult {
  const test = readLitsFile(testPath)
  const includes = getIncludes(testPath, test)
  const testResult: TestResult = {
    tap: `TAP version 13\n`,
    success: true,
  }
  try {
    const testChunks = getTestChunks(test)
    testResult.tap += `1..${testChunks.length}\n`
    testChunks.forEach((testChunkProgram, index) => {
      const testNumber = index + 1
      if (testNamePattern && !testNamePattern.test(testChunkProgram.name)) {
        testResult.tap += `ok ${testNumber} ${testChunkProgram.name} # skip - Not matching testNamePattern ${testNamePattern}\n`
      } else if (testChunkProgram.directive === `SKIP`) {
        testResult.tap += `ok ${testNumber} ${testChunkProgram.name} # skip\n`
      } else {
        try {
          const lits = new Lits({ debug: true })
          const context = lits.context(includes.code, {
            getLocation: (line, col) => getIncludesLocation(line, col, includes.sourceMapping),
          })
          lits.run(testChunkProgram.program, {
            contexts: [context],
            getLocation: (line, col) => `${testPath}:${line}:${col}`,
          })
          testResult.tap += `ok ${testNumber} ${testChunkProgram.name}\n`
        } catch (error) {
          testResult.success = false
          testResult.tap += `not ok ${testNumber} ${testChunkProgram.name}${getErrorYaml(error)}`
        }
      }
    })
  } catch (error: unknown) {
    testResult.tap += `Bail out! ${getErrorMessage(error)}\n`
    testResult.success = false
  }
  return testResult
}

function readLitsFile(litsPath: string): string {
  if (!litsPath.endsWith(`.lits`)) {
    throw Error(`Expected .lits file, got ${litsPath}`)
  }
  return fs.readFileSync(litsPath, { encoding: `utf-8` })
}

type IncludesResult = { code: string; sourceMapping: Array<{ file: string; start: number; size: number }> }
function getIncludes(testPath: string, test: string): IncludesResult {
  const dirname = path.dirname(testPath)
  let okToInclude = true
  let currentLine = 1
  return test.split(`\n`).reduce(
    (result: IncludesResult, line) => {
      const includeMatch = line.match(/^\s*;+\s*@include\s*(\S+)\s*$/)
      if (includeMatch) {
        if (!okToInclude) {
          throw Error(`@include must be in the beginning of file`)
        }
        const relativeFilePath = includeMatch[1] as string
        const filePath = path.resolve(dirname, relativeFilePath)
        const fileContent = readLitsFile(filePath)
        result.code += `${fileContent}\n`
        const size = result.code.split(`\n`).length
        result.sourceMapping.push({
          file: filePath,
          start: currentLine,
          size,
        })
        currentLine += size
      }
      if (!line.match(/^\s*(?:;.*)$/)) {
        okToInclude = false
      }
      return result
    },
    { code: ``, sourceMapping: [] },
  )
}

// Splitting test file based on @test annotations
function getTestChunks(testProgram: string): TestChunk[] {
  let currentTest: TestChunk | undefined
  let setupCode = ``
  return testProgram.split(`\n`).reduce((result: TestChunk[], line, index) => {
    const currentLineNbr = index + 1
    const testNameAnnotationMatch = line.match(/^\s*;+\s*@(?:(skip)-)?test\s*(.*)$/)
    if (testNameAnnotationMatch) {
      const directive = (testNameAnnotationMatch[1] ?? ``).toUpperCase()
      const testName = testNameAnnotationMatch[2]
      if (!testName) {
        throw Error(`Missing test name on line ${currentLineNbr}`)
      }
      if (result.find(chunk => chunk.name === testName)) {
        throw Error(`Duplicate test name ${testName}`)
      }
      currentTest = {
        directive: (directive || null) as TestChunk[`directive`],
        name: testName,
        // Adding new-lines to make lits debug information report correct rows
        program:
          setupCode + [...Array(currentLineNbr + 2 - setupCode.split(`\n`).length).keys()].map(() => ``).join(`\n`),
      }
      result.push(currentTest)
      return result
    }
    if (!currentTest) {
      setupCode += `${line}\n`
    } else {
      currentTest.program += `${line}\n`
    }
    return result
  }, [])
}

export function getErrorYaml(error: unknown): string {
  const message = getErrorMessage(error)
  // This is a fallback, should not happen (Lits should be throwing AbstractLitsErrors)
  /* istanbul ignore next */
  if (!isAbstractLitsError(error)) {
    return `
  ---
  message: ${JSON.stringify(message)}
  ...
`
  }

  const debugInfo = error.debugInfo
  /* istanbul ignore next */
  if (!debugInfo || typeof debugInfo === `string`) {
    return `
  ---
  message: ${JSON.stringify(message)}
  error: ${JSON.stringify(error.name)}
  ...
`
  }

  const getLocation = debugInfo.getLocation ?? ((line: number, column: number) => `(${line}:${column})`)
  const location = getLocation(debugInfo.line, debugInfo.column)
  const formattedMessage = message.includes(`\n`)
    ? `|\n    ${message.split(/\r?\n/).join(`\n    `)}`
    : JSON.stringify(message)
  return `
  ---
  error: ${JSON.stringify(error.name)}
  message: ${formattedMessage}
  location: ${JSON.stringify(location)}
  code:
    - "${debugInfo.code}"
    - "${getCodeMarker(debugInfo)}"
  ...
`
}

function getErrorMessage(error: unknown): string {
  if (!isAbstractLitsError(error)) {
    // error should always be an Error (other cases is just for kicks)
    /* istanbul ignore next */
    return typeof error === `string` ? error : error instanceof Error ? error.message : `Unknown error`
  }
  return error.shortMessage
}

function isAbstractLitsError(error: unknown): error is AbstractLitsError {
  return error instanceof AbstractLitsError
}
