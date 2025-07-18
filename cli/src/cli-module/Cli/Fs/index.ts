/* eslint-disable ts/no-unsafe-member-access */
import fs from 'node:fs'
import type { GetFsModule } from '../../utils'
import type { JsFunction } from '../../../../../src'

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

const list: JsFunction = {
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
    [`${prefix}.copy`]: copy,
    [`${prefix}.move`]: move,
    [`${prefix}.stats`]: stats,
    [`${prefix}.chmod`]: chmod,
    [`${prefix}.touch`]: touch,
  }
}
