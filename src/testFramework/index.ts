import fs from 'node:fs'
import path from 'node:path'
import { LitsError } from '../errors'
import type { Context } from '../evaluator/interface'
import { Lits } from '../Lits/Lits'
import type { SourceCodeInfo } from '../tokenizer/token'
import { getCodeMarker } from '../utils/debug/getCodeMarker'

interface TestChunk {
  name: string
  program: string
  directive: 'SKIP' | null
}

export interface RunTestParams {
  testPath: string
  testNamePattern?: RegExp
}

export interface TestResult {
  /**
   * Test report
   * http://testanything.org/
   */
  tap: string
  success: boolean
}

export function runTest({ testPath: filePath, testNamePattern }: RunTestParams): TestResult {
  const includedFilePaths = getIncludedFilePaths(filePath)
  const testResult: TestResult = {
    tap: 'TAP version 13\n',
    success: true,
  }
  try {
    const testChunks = getTestChunks(filePath)
    testResult.tap += `1..${testChunks.length}\n`
    testChunks.forEach((testChunkProgram, index) => {
      const testNumber = index + 1
      if (testNamePattern && !testNamePattern.test(testChunkProgram.name)) {
        testResult.tap += `ok ${testNumber} ${testChunkProgram.name} # skip - Not matching testNamePattern ${testNamePattern}\n`
      }
      else if (testChunkProgram.directive === 'SKIP') {
        testResult.tap += `ok ${testNumber} ${testChunkProgram.name} # skip\n`
      }
      else {
        try {
          const lits = new Lits({ debug: true })
          const contexts = getContexts(includedFilePaths, lits)
          lits.run(testChunkProgram.program, {
            contexts,
            filePath,
          })
          testResult.tap += `ok ${testNumber} ${testChunkProgram.name}\n`
        }
        catch (error) {
          testResult.success = false
          testResult.tap += `not ok ${testNumber} ${testChunkProgram.name}${getErrorYaml(error)}`
        }
      }
    })
  }
  catch (error: unknown) {
    testResult.tap += `Bail out! ${getErrorMessage(error)}\n`
    testResult.success = false
  }
  return testResult
}

function readLitsFile(litsPath: string): string {
  if (!litsPath.endsWith('.lits'))
    throw new Error(`Expected .lits file, got ${litsPath}`)

  return fs.readFileSync(litsPath, { encoding: 'utf-8' })
}

function getContexts(includedFilePaths: string[], lits: Lits): Context[] {
  return includedFilePaths.reduce((acc: Context[], filePath) => {
    const fileContent = readLitsFile(filePath)
    acc.push(lits.context(fileContent, { filePath, contexts: acc }))
    return acc
  }, [])
}

function getIncludedFilePaths(absoluteFilePath: string): string[] {
  const result: string[] = []
  getIncludesRecursively(absoluteFilePath, result)
  return result.reduce((acc: string[], entry: string) => {
    if (!acc.includes(entry))
      acc.push(entry)

    return acc
  }, [])

  function getIncludesRecursively(filePath: string, includedFilePaths: string[]): void {
    const includeFilePaths = readIncludeDirectives(filePath)
    includeFilePaths.forEach((includeFilePath) => {
      getIncludesRecursively(includeFilePath, includedFilePaths)
      includedFilePaths.push(includeFilePath)
    })
  }
}

function readIncludeDirectives(filePath: string): string[] {
  const fileContent = readLitsFile(filePath)
  const dirname = path.dirname(filePath)
  let okToInclude = true
  return fileContent.split('\n').reduce((acc: string[], line) => {
    const includeMatch = line.match(/^\s*\/{2}\s*@include\s*(\S+)\s*$/)
    if (includeMatch) {
      if (!okToInclude)
        throw new Error(`@include must be in the beginning of file: ${filePath}:${line + 1}`)

      const relativeFilePath = includeMatch[1] as string
      acc.push(path.resolve(dirname, relativeFilePath))
    }
    if (!line.match(/^\s*\/.*$/))
      okToInclude = false

    return acc
  }, [])
}

// Splitting test file based on @test annotations
function getTestChunks(testPath: string): TestChunk[] {
  const testProgram = readLitsFile(testPath)
  let currentTest: TestChunk | undefined
  let setupCode = ''
  return testProgram.split('\n').reduce((result: TestChunk[], line, index) => {
    const currentLineNbr = index + 1
    // eslint-disable-next-line regexp/no-super-linear-backtracking
    const testNameAnnotationMatch = line.match(/^\s*\/{2}\s*@(?:(skip)-)?test\s*(.*)$/)
    if (testNameAnnotationMatch) {
      const directive = (testNameAnnotationMatch[1] ?? '').toUpperCase()
      const testName = testNameAnnotationMatch[2]
      if (!testName)
        throw new Error(`Missing test name on line ${currentLineNbr}`)

      if (result.find(chunk => chunk.name === testName))
        throw new Error(`Duplicate test name ${testName}`)

      currentTest = {
        directive: (directive || null) as TestChunk['directive'],
        name: testName,
        // Adding new-lines to make lits debug information report correct rows
        program:
          setupCode + [...Array(currentLineNbr + 2 - setupCode.split('\n').length).keys()].map(() => '').join('\n'),
      }
      result.push(currentTest)
      return result
    }
    if (!currentTest)
      setupCode += `${line}\n`
    else
      currentTest.program += `${line}\n`

    return result
  }, [])
}

export function getErrorYaml(error: unknown): string {
  const message = getErrorMessage(error)
  /* v8 ignore next 7 */
  if (!isAbstractLitsError(error)) {
    return `
  ---
  message: ${JSON.stringify(message)}
  ...
`
  }

  const sourceCodeInfo = error.sourceCodeInfo
  /* v8 ignore next 8 */
  if (!sourceCodeInfo || typeof sourceCodeInfo === 'string') {
    return `
  ---
  message: ${JSON.stringify(message)}
  error: ${JSON.stringify(error.name)}
  ...
`
  }

  const formattedMessage = message.includes('\n')
    ? `|\n    ${message.split(/\r?\n/).join('\n    ')}`
    : JSON.stringify(message)
  return `
  ---
  error: ${JSON.stringify(error.name)}
  message: ${formattedMessage}
  location: ${JSON.stringify(getLocation(sourceCodeInfo))}
  code:
    - "${sourceCodeInfo.code}"
    - "${getCodeMarker(sourceCodeInfo)}"
  ...
`
}

function getLocation(sourceCodeInfo: SourceCodeInfo): string {
  const terms: string[] = []
  if (sourceCodeInfo.filePath)
    terms.push(sourceCodeInfo.filePath)

  if (sourceCodeInfo.position) {
    terms.push(`${sourceCodeInfo.position.line}`)
    terms.push(`${sourceCodeInfo.position.column}`)
  }

  return terms.join(':')
}

function getErrorMessage(error: unknown): string {
  if (!isAbstractLitsError(error)) {
    // error should always be an Error (other cases is just for kicks)
    /* v8 ignore next 1 */
    return typeof error === 'string' ? error : error instanceof Error ? error.message : 'Unknown error'
  }
  return error.shortMessage
}

function isAbstractLitsError(error: unknown): error is LitsError {
  return error instanceof LitsError
}
