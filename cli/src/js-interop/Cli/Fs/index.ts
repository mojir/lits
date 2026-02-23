/* eslint-disable ts/no-unsafe-member-access */
import fs from 'node:fs'
import process from 'node:process'
import os from 'node:os'
import path from 'node:path'
import type { JsFunction } from '../../../../../src'
import type { Any } from '../../../../../src/interface'
import type { LitsModule } from '../../../../../src/builtin/modules/interface'
import type { BuiltinNormalExpressions } from '../../../../../src/builtin/interface'

let initiated = false
const tempFiles: string[] = []
const tempDirs: string[] = []
function init() {
  if (initiated) {
    return
  }
  initiated = true

  process.on('exit', () => {
    // Clean up temporary files and directories on process exit
    tempFiles.forEach((file) => {
      try {
        fs.unlinkSync(file)
      }
      catch (error) {
        console.error(`Failed to delete temporary file ${file}:`, error)
      }
    })
    tempDirs.forEach((dir) => {
      try {
        fs.rmSync(dir, { recursive: true, force: true })
      }
      catch (error) {
        console.error(`Failed to delete temporary directory ${dir}:`, error)
      }
    })
  })
}

const readFile: JsFunction = {
  fn: (filePath: string): string => {
    return fs.readFileSync(filePath, { encoding: 'utf8' })
  },
  docString: `
Reads a file and returns its content as a string.

Parameters:
- filePath: The path to the file to read.
`,
  arity: { min: 1, max: 1 },
}

const writeFile: JsFunction = {
  fn: (content: string, filePath: string): void => {
    fs.writeFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: `
Writes content to a file. If the file does not exist, it will be created.

Parameters:
- content: The content to write to the file.
- filePath: The path to the file where the content will be written.

Description:
If the file already exists, its content will be overwritten.
`,
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
  docString: `
Reads a JSON file and returns its parsed content.

Parameters:
- filePath: The path to the JSON file to read.

Description:
If the file does not exist or is not valid JSON, an error will be thrown.
`,
  arity: { min: 1, max: 1 },
}

const writeJson: JsFunction = {
  fn: (data: unknown, filePath: string, indentation: number | null = 2): void => {
    const content = indentation ? JSON.stringify(data, null, indentation) : JSON.stringify(data)
    fs.writeFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: `
Writes data to a JSON file. If the file does not exist, it will be created.

Parameters:
- data: The data to write to the JSON file.
- filePath: The path to the JSON file where the data will be written.
- indentation: Optional. The number of spaces to use for indentation in the JSON file. Defaults to 2. 0 or null will write without indentation.

Description:
If the file already exists, its content will be overwritten. The data will be formatted with 2 spaces for readability.
`,
  arity: { min: 2, max: 3 },
}

const appendFile: JsFunction = {
  fn: (content: string, filePath: string): void => {
    fs.appendFileSync(filePath, content, { encoding: 'utf8' })
  },
  docString: `
Appends content to a file. If the file does not exist, it will be created.

Parameters:
- content: The content to append to the file.
- filePath: The path to the file where the content will be appended.

Description:
If the file already exists, the content will be added to the end of the file without overwriting existing content.
`,
  arity: { min: 2, max: 2 },
}

const exists: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath)
  },
  docString: `
Checks if a file or directory exists at the specified path.

Parameters:
- filePath: The path to the file or directory to check.
`,
  arity: { min: 1, max: 1 },
}

const isFile: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath) && fs.statSync(filePath).isFile()
  },
  docString: `
Checks if the specified path is a file.

Parameters:
- filePath: The path to the file to check.
`,
  arity: { min: 1, max: 1 },
}

const isDirectory: JsFunction = {
  fn: (filePath: string): boolean => {
    return fs.existsSync(filePath) && fs.statSync(filePath).isDirectory()
  },
  docString: `
Checks if the specified path is a directory.

Parameters:
- filePath: The path to the directory to check.
`,
  arity: { min: 1, max: 1 },
}

const listDir: JsFunction = {
  fn: (directoryPath: string): string[] => {
    return fs.readdirSync(directoryPath, { encoding: 'utf8' })
  },
  docString: `
Lists the contents of a directory.

Parameters:
- directoryPath: The path to the directory to list.

Description:
The returned array contains the names of files and directories in the specified directory.
`,
  arity: { min: 1, max: 1 },
}

const mkdir: JsFunction = {
  fn: (directoryPath: string): void => {
    fs.mkdirSync(directoryPath, { recursive: true })
  },
  docString: `
Creates a directory and all its parent directories if they do not exist.

Parameters:
- directoryPath: The path to the directory to create.

Description:
If the directory already exists, this function does nothing. If the parent directories do not exist, they will be created as well.
`,
  arity: { min: 1, max: 1 },
}

const remove = {
  fn: (filePath: string): void => {
    if (!fs.existsSync(filePath)) {
      throw new Error(`File or directory at path "${filePath}" does not exist.`)
    }
    fs.rmSync(filePath, { recursive: true, force: true })
  },
  docString: `
Removes a file or directory at the specified path.

Parameters:
- filePath: The path to the file or directory to remove.

Description:
If the path is a directory, it will be removed recursively. If the path is a file, it will be deleted. If the file or directory does not exist, an error will be thrown.
`,
  arity: { min: 1, max: 1 },
}

const copy = {
  fn: (sourcePath: string, destinationPath: string): void => {
    if (!fs.existsSync(sourcePath)) {
      throw new Error(`Source file or directory at path "${sourcePath}" does not exist.`)
    }
    fs.cpSync(sourcePath, destinationPath, { recursive: true, force: true })
  },
  docString: `
Copies a file or directory from source to destination.

Parameters:
- sourcePath: The path to the source file or directory.
- destinationPath: The path to the destination file or directory.

Description:
If the destination already exists, it will be overwritten. If the source is a directory, its contents will be copied recursively.
If the source does not exist, an error will be thrown.
`,
  arity: { min: 2, max: 2 },
}

const move = {
  fn: (sourcePath: string, destinationPath: string): void => {
    if (!fs.existsSync(sourcePath)) {
      throw new Error(`Source file or directory at path "${sourcePath}" does not exist.`)
    }
    fs.renameSync(sourcePath, destinationPath)
  },
  docString: `
Moves a file or directory from source to destination.

Parameters:
- sourcePath: The path to the source file or directory.
- destinationPath: The path to the destination file or directory.

Description:
If the destination already exists, it will be overwritten. If the source is a directory, its contents will be moved recursively.
If the source does not exist, an error will be thrown.
`,
  arity: { min: 2, max: 2 },
}

const stats = {
  fn: (filePath: string): unknown => {
    if (!fs.existsSync(filePath)) {
      throw new Error(`File or directory at path "${filePath}" does not exist.`)
    }
    const statsInfo = fs.statSync(filePath)
    return {
      'dev': statsInfo.dev,
      'ino': statsInfo.ino,
      'mode': statsInfo.mode,
      'nlink': statsInfo.nlink,
      'uid': statsInfo.uid,
      'gid': statsInfo.gid,
      'rdev': statsInfo.rdev,
      'size': statsInfo.size,
      'blksize': statsInfo.blksize,
      'blocks': statsInfo.blocks,
      'atime': statsInfo.atimeMs,
      'mtime': statsInfo.mtimeMs,
      'ctime': statsInfo.ctimeMs,
      'birthtime': statsInfo.birthtimeMs,
      'is-file': statsInfo.isFile(),
      'is-directory': statsInfo.isDirectory(),
      'is-block-device': statsInfo.isBlockDevice(),
      'is-character-device': statsInfo.isCharacterDevice(),
      'is-symbolic-link': statsInfo.isSymbolicLink(),
      'is-fifo': statsInfo.isFIFO(),
      'is-socket': statsInfo.isSocket(),
    }
  },
  docString: `
Returns the stats of a file or directory at the specified path.

Parameters:
- filePath: The path to the file or directory.

Description:
The returned object contains the following properites:
- dev: Device ID
- ino: Inode number
- mode: File type and mode
- nlink: Number of hard links
- uid: User ID of owner
- gid: Group ID of owner
- rdev: Device ID (if special file)
- size: Size in bytes
- blksize: Block size for filesystem I/O
- blocks: Number of blocks allocated
- atime: Last access time in milliseconds
- mtime: Last modification time in milliseconds
- ctime: Last status change time in milliseconds
- birthtime: Creation time in milliseconds
- is-file: Boolean indicating if it is a file
- is-directory: Boolean indicating if it is a directory
- is-block-device: Boolean indicating if it is a block device
- is-character-device: Boolean indicating if it is a character device
- is-symbolic-link: Boolean indicating if it is a symbolic link
- is-fifo: Boolean indicating if it is a FIFO (named pipe)
- is-socket: Boolean indicating if it is a socket
`,
  arity: { min: 1, max: 1 },
}

const chmod = {
  fn: (filePath: string, mode: string | number): void => {
    if (!fs.existsSync(filePath)) {
      throw new Error(`File or directory at path "${filePath}" does not exist.`)
    }
    fs.chmodSync(filePath, mode)
  },
  docString: 'Changes the permissions of a file or directory at the specified path.',
  arity: { min: 2, max: 2 },
}

const touch = {
  fn: (filePath: string): void => {
    fs.closeSync(fs.openSync(filePath, 'a'))
  },
  docString: 'Updates the access and modification times of a file. If the file does not exist, it will be created.',
  arity: { min: 1, max: 1 },
}

type TempOptions = {
  prefix?: string
  suffix?: string
  dir?: string
}

function validateTempOptions(options: TempOptions): void {
  const { dir, suffix, prefix } = options
  if (prefix && prefix.includes(path.sep)) {
    throw new Error(`Prefix "${prefix}" cannot contain path separators.`)
  }
  if (suffix && suffix.includes(path.sep)) {
    throw new Error(`Suffix "${suffix}" cannot contain path separators.`)
  }
  if (options.dir && fs.existsSync(options.dir) && !fs.statSync(options.dir).isDirectory()) {
    throw new Error(`Specified directory "${dir}" is not a valid directory.`)
  }
}

const createTempFile = {
  fn: (options: TempOptions = {}): string => {
    validateTempOptions(options)
    const { prefix = '', suffix = '', dir = os.tmpdir() } = options
    const timestamp = Date.now()
    const random = Math.random().toString(36).substring(2, 8)
    const filename = `${prefix}-temp-${timestamp}-${random}${suffix}`
    const tempPath = path.join(dir, filename)
    fs.writeFileSync(tempPath, '', { encoding: 'utf8' }) // Create an empty file
    tempFiles.push(tempPath) // Track the temporary file for cleanup
    return tempPath
  },
  docString: `
Creates a temporary file with a unique name.

Parameters:
- options: An object with optional properties:
  - prefix: A string to prepend to the filename (default: '').
  - suffix: A string to append to the filename (default: '').
  - dir: The directory where the temporary file will be created (default: system's temp directory).

Returns:
- The path to the created temporary file.
`,
  arity: { min: 0, max: 1 },
}

const createTempDir = {
  fn: (options: TempOptions = {}): string => {
    validateTempOptions(options)
    const { prefix = '', suffix = '', dir = os.tmpdir() } = options
    const timestamp = Date.now()
    const random = Math.random().toString(36).substring(2, 8)
    const dirname = `${prefix}-temp-${timestamp}-${random}${suffix}`
    const tempPath = path.join(dir, dirname)
    fs.mkdirSync(tempPath, { recursive: true }) // Create the directory
    tempDirs.push(tempPath) // Track the temporary directory for cleanup
    return tempPath
  },
  docString: `
Creates a temporary directory with a unique name.

Parameters:
- options: An object with optional properties:
  - prefix: A string to prepend to the directory name (default: '').
  - suffix: A string to append to the directory name (default: '').
  - dir: The directory where the temporary directory will be created (default: system's temp directory).

Returns:
- The path to the created temporary directory.
`,
  arity: { min: 0, max: 1 },
}

function jsFnToExpression(jsFn: JsFunction): BuiltinNormalExpressions[string] {
  return {
    evaluate: params => jsFn.fn(...params) as Any,
    arity: jsFn.arity ?? {},
  }
}

export function getFsModule(): LitsModule {
  init()
  return {
    name: 'cli-fs',
    functions: {
      'read-file': jsFnToExpression(readFile),
      'read-json': jsFnToExpression(readJson),
      'write-file': jsFnToExpression(writeFile),
      'write-json': jsFnToExpression(writeJson),
      'append-file': jsFnToExpression(appendFile),
      'exists?': jsFnToExpression(exists),
      'file?': jsFnToExpression(isFile),
      'directory?': jsFnToExpression(isDirectory),
      'list-dir': jsFnToExpression(listDir),
      'mkdir': jsFnToExpression(mkdir),
      'remove': jsFnToExpression(remove),
      'copy': jsFnToExpression(copy),
      'move': jsFnToExpression(move),
      'get-stats': jsFnToExpression(stats),
      'chmod': jsFnToExpression(chmod),
      'touch': jsFnToExpression(touch),
      'create-temp-file': jsFnToExpression(createTempFile),
      'create-temp-dir': jsFnToExpression(createTempDir),
    },
  }
}
