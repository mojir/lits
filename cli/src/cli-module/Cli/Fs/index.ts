/* eslint-disable ts/no-unsafe-member-access */
import fs from 'node:fs'
import type { GetFsModule } from '../../utils'
import type { JsFunction } from '../../../../../src'

const readFile: JsFunction = {
  fn: (filePath: string): string => {
    return fs.readFileSync(filePath, { encoding: 'utf8' })
  },
  docString: 'Reads a file and returns its content as a string.',
  arity: { min: 1, max: 1 },
}

const writeFile: JsFunction = {
  fn: (content: string, filePath: string): void => {
    fs.writeFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: 'Writes content to a file. If the file does not exist, it will be created.',
  arity: { min: 2, max: 2 },
}

const readJson: JsFunction = {
  fn: (filePath: string): unknown => {
    const content = fs.readFileSync(filePath, { encoding: 'utf8' })
    try {
      return JSON.parse(content) as unknown
    }
    catch (error: any) {
      throw new Error(`Failed to parse JSON from file ${filePath}: ${error.message}`)
    }
  },
  docString: 'Reads a JSON file and returns its parsed content.',
  arity: { min: 1, max: 1 },
}

const writeJson: JsFunction = {
  fn: (data: unknown, filePath: string): void => {
    const content = JSON.stringify(data, null, 2)
    fs.writeFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: 'Writes data to a JSON file. If the file does not exist, it will be created.',
  arity: { min: 2, max: 2 },
}

const appendFile: JsFunction = {
  fn: (content: string, filePath: string): void => {
    fs.appendFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: 'Appends content to a file. If the file does not exist, it will be created.',
  arity: { min: 2, max: 2 },
}

const exists: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath)
  },
  docString: 'Checks if a file or directory exists at the specified path.',
  arity: { min: 1, max: 1 },
}

const isFile: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath) && fs.statSync(filePath).isFile()
  },
  docString: 'Checks if the specified path is a file.',
  arity: { min: 1, max: 1 },
}

const isDirectory: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath) && fs.statSync(filePath).isDirectory()
  },
  docString: 'Checks if the specified path is a directory.',
  arity: { min: 1, max: 1 },
}

const list: JsFunction = {
  fn: (directoryPath: string): string[] => {
    return fs.readdirSync(directoryPath, { encoding: 'utf8' })
  },
  docString: 'Lists the contents of a directory.',
  arity: { min: 1, max: 1 },
}

const mkdir: JsFunction = {
  fn: (directoryPath: string): void => {
    fs.mkdirSync(directoryPath, { recursive: true })
  },
  docString: 'Creates a directory and all its parent directories if they do not exist.',
  arity: { min: 1, max: 1 },
}

const remove = {
  fn: (filePath: string): void => {
    if (!fs.existsSync(filePath)) {
      throw new Error(`File or directory at path "${filePath}" does not exist.`)
    }
    fs.rmSync(filePath, { recursive: true, force: true })
  },
  docString: 'Removes a file or directory at the specified path.',
  arity: { min: 1, max: 1 },
}

export const getFsModule: GetFsModule = (modulePath) => {
  const prefix = modulePath.join('.')
  return {
    [`${prefix}.read-file`]: readFile,
    [`${prefix}.read-json`]: readJson,
    [`${prefix}.write-file`]: writeFile,
    [`${prefix}.write-json`]: writeJson,
    [`${prefix}.append-file`]: appendFile,
    [`${prefix}.exists?`]: exists,
    [`${prefix}.file?`]: isFile,
    [`${prefix}.directory?`]: isDirectory,
    [`${prefix}.list`]: list,
    [`${prefix}.mkdir`]: mkdir,
    [`${prefix}.remove`]: remove,
  }
}
