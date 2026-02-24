/* eslint-disable ts/no-unsafe-assignment */
/* eslint-disable ts/no-unsafe-member-access */
import { execSync } from 'node:child_process'
import * as fs from 'node:fs'
import * as path from 'node:path'
import { afterEach, beforeAll, beforeEach, describe, expect, test } from 'vitest'

describe('the cli-fs Integration Tests', () => {
  const testDir = path.join(__dirname, 'test_temp')
  const litsCliPath = path.join(__dirname, '../../dist/cli/cli.js') // Adjust path as needed

  beforeAll(() => {
    // Build the CLI first
    if (!fs.existsSync(litsCliPath)) {
      try {
        execSync('npm run build', {
          cwd: path.join(__dirname, '../..'), // Adjust to project root
          stdio: 'pipe',
        })
      }
      catch (error: any) {
        throw new Error(`Failed to build CLI: ${error.message}`)
      }
    }
  })
  beforeEach(() => {
    // Clean up and create fresh test directory
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true })
    }
    fs.mkdirSync(testDir, { recursive: true })

    // Create test files and directories
    setupTestFiles()
  })

  afterEach(() => {
    // Clean up test directory
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true })
    }
  })

  function setupTestFiles() {
    // Create test files
    fs.writeFileSync(path.join(testDir, 'test.txt'), 'Hello, World!')
    fs.writeFileSync(path.join(testDir, 'config.json'), JSON.stringify({ name: 'test', version: 1 }, null, 2))
    fs.writeFileSync(path.join(testDir, 'data.csv'), 'name,age\nAlice,30\nBob,25')

    // Create subdirectories
    fs.mkdirSync(path.join(testDir, 'subdir'))
    fs.mkdirSync(path.join(testDir, 'nested', 'deep'), { recursive: true })

    // Create files in subdirectories
    fs.writeFileSync(path.join(testDir, 'subdir', 'nested.txt'), 'Nested content')
    fs.writeFileSync(path.join(testDir, 'nested', 'deep', 'buried.txt'), 'Deep content')
  }

  function runLits(expression: string): string {
    try {
      const result = execSync(`node '${litsCliPath}' eval '${expression}' -p`, {
        cwd: testDir,
        encoding: 'utf8',
        stdio: 'pipe',
        env: { ...process.env, NO_COLOR: '1', FORCE_COLOR: '0' },
      })
      return result.trim()
    }
    catch (error: any) {
      throw new Error(`Lits CLI failed: ${error.message}\nStdout: ${error.stdout}\nStderr: ${error.stderr}`)
    }
  }

  describe('file Reading Operations', () => {
    test('should read file content', () => {
      const result = runLits('let f = import(cli-fs); f.read-file("test.txt")')
      expect(result).toBe('Hello, World!')
    })

    test('should read JSON file', () => {
      const result = runLits('let f = import(cli-fs); f.read-json("config.json") |> "name"')
      expect(result).toBe('test')
    })

    test('should check if file exists', () => {
      const existsResult = runLits('let f = import(cli-fs); f.exists?("test.txt")')
      expect(existsResult).toBe('true')

      const notExistsResult = runLits('let f = import(cli-fs); f.exists?("nonexistent.txt")')
      expect(notExistsResult).toBe('false')
    })

    test('should check if path is file or directory', () => {
      const isFileResult = runLits('let f = import(cli-fs); f.file?("test.txt")')
      expect(isFileResult).toBe('true')

      const isDirResult = runLits('let f = import(cli-fs); f.directory?("subdir")')
      expect(isDirResult).toBe('true')

      const fileIsNotDirResult = runLits('let f = import(cli-fs); f.directory?("test.txt")')
      expect(fileIsNotDirResult).toBe('false')
    })
  })

  describe('directory Operations', () => {
    test('should list directory contents', () => {
      const result = runLits('let f = import(cli-fs); f.list-dir(".") |> json-stringify')
      const files = JSON.parse(result)

      expect(files).toEqual(expect.arrayContaining([
        'test.txt',
        'config.json',
        'data.csv',
        'subdir',
        'nested',
      ]))
    })

    test('should create new directory', () => {
      runLits('let f = import(cli-fs); f.mkdir("new-dir")')

      const exists = fs.existsSync(path.join(testDir, 'new-dir'))
      expect(exists).toBe(true)

      const isDir = fs.statSync(path.join(testDir, 'new-dir')).isDirectory()
      expect(isDir).toBe(true)
    })

    test('should create nested directories', () => {
      runLits('let f = import(cli-fs); f.mkdir("very/deep/nested/dir")')

      const exists = fs.existsSync(path.join(testDir, 'very', 'deep', 'nested', 'dir'))
      expect(exists).toBe(true)
    })
  })

  describe('file Writing Operations', () => {
    test('should write file content', () => {
      runLits('let f = import(cli-fs); f.write-file("New content", "output.txt")')

      const content = fs.readFileSync(path.join(testDir, 'output.txt'), 'utf8')
      expect(content).toBe('New content')
    })

    test('should append to file', () => {
      runLits('let f = import(cli-fs); f.append-file( " Additional text", "test.txt")')

      const content = fs.readFileSync(path.join(testDir, 'test.txt'), 'utf8')
      expect(content).toBe('Hello, World! Additional text')
    })

    test('should write JSON file', () => {
      runLits('let f = import(cli-fs); f.write-json({name: "new-test", value: 42}, "new-config.json")')

      const content = fs.readFileSync(path.join(testDir, 'new-config.json'), 'utf8')
      const parsed = JSON.parse(content)
      expect(parsed).toEqual({ name: 'new-test', value: 42 })
    })
  })

  describe('file Operations', () => {
    test('should remove file', () => {
      runLits('let f = import(cli-fs); f.remove("test.txt")')

      const exists = fs.existsSync(path.join(testDir, 'test.txt'))
      expect(exists).toBe(false)
    })

    test('should remove directory recursively', () => {
      runLits('let f = import(cli-fs); f.remove("nested")')

      const exists = fs.existsSync(path.join(testDir, 'nested'))
      expect(exists).toBe(false)
    })
    test('should copy file', () => {
      runLits('let f = import(cli-fs); f.copy("test.txt", "test_copy.txt")')

      const exists = fs.existsSync(path.join(testDir, 'test_copy.txt'))
      expect(exists).toBe(true)

      const content = fs.readFileSync(path.join(testDir, 'test_copy.txt'), 'utf8')
      expect(content).toBe('Hello, World!')
    })
    test('should copy directory recursively', () => {
      runLits('let f = import(cli-fs); f.copy("subdir", "subdir_copy")')

      const exists = fs.existsSync(path.join(testDir, 'subdir_copy', 'nested.txt'))
      expect(exists).toBe(true)
      const content = fs.readFileSync(path.join(testDir, 'subdir_copy', 'nested.txt'), 'utf8')
      expect(content).toBe('Nested content')
    })
    test('should move file', () => {
      runLits('let f = import(cli-fs); f.move("test.txt", "test_moved.txt")')

      const exists = fs.existsSync(path.join(testDir, 'test_moved.txt'))
      expect(exists).toBe(true)

      const content = fs.readFileSync(path.join(testDir, 'test_moved.txt'), 'utf8')
      expect(content).toBe('Hello, World!')

      // Ensure original file no longer exists
      const originalExists = fs.existsSync(path.join(testDir, 'test.txt'))
      expect(originalExists).toBe(false)
    })
    test('should move directory recursively', () => {
      runLits('let f = import(cli-fs); f.move("subdir", "subdir_moved")')

      const exists = fs.existsSync(path.join(testDir, 'subdir_moved', 'nested.txt'))
      expect(exists).toBe(true)
      const content = fs.readFileSync(path.join(testDir, 'subdir_moved', 'nested.txt'), 'utf8')
      expect(content).toBe('Nested content')
      // Ensure original directory no longer exists
      const originalExists = fs.existsSync(path.join(testDir, 'subdir'))
      expect(originalExists).toBe(false)
    })
    test('should get stats', () => {
      const result = runLits('let f = import(cli-fs); f.get-stats("test.txt") |> json-stringify(_, 2)')
      const stats = JSON.parse(result)

      expect(stats).toHaveProperty('size')
      expect(stats.size).toBeGreaterThan(0)
      expect(stats).toHaveProperty('mtime')
      expect(stats).toHaveProperty('ctime')
      expect(stats).toHaveProperty('atime')
    })
  })

  // describe('complex File Operations', () => {
  //   test('should process multiple files with Lits pipeline', () => {
  //     const litsScript = `
  //       let files = let f = import(cli-fs); f.listDir(".")
  //         |> filter (f -> Cli.path.extname(f) == ".txt")
  //         |> map (f -> {
  //             name: f,
  //             content: let f = import(cli-fs); f.readFile(f),
  //             size: let f = import(cli-fs); f.size(f)
  //           });
  //
  //       files |> count
  //     `
  //
  //     const result = runLits(litsScript)
  //     expect(Number.parseInt(result)).toBeGreaterThan(0)
  //   })
  //
  //   test('should backup and modify file', () => {
  //     const litsScript = `
  //       let original = "test.txt";
  //       let backup = original ++ ".backup";
  //
  //       // Create backup
  //       let f = import(cli-fs); f.copy(original, backup);
  //
  //       // Modify original
  //       let content = let f = import(cli-fs); f.readFile(original);
  //       let modified = content ++ " - Modified";
  //       let f = import(cli-fs); f.writeFile(original, modified);
  //
  //       // Verify both exist
  //       [let f = import(cli-fs); f.exists(original), let f = import(cli-fs); f.exists(backup)]
  //     `
  //
  //     const result = runLits(litsScript)
  //     expect(result).toBe('[true,true]')
  //
  //     // Verify file contents
  //     const originalContent = fs.readFileSync(path.join(testDir, 'test.txt'), 'utf8')
  //     const backupContent = fs.readFileSync(path.join(testDir, 'test.txt.backup'), 'utf8')
  //
  //     expect(originalContent).toBe('Hello, World! - Modified')
  //     expect(backupContent).toBe('Hello, World!')
  //   })
  // })

  describe('error Handling', () => {
    test('should handle reading non-existent file', () => {
      expect(() => {
        runLits('let f = import(cli-fs); f.read-file("does-not-exist.txt")')
      }).toThrow()
    })

    test('should handle removing non-existent file', () => {
      expect(() => {
        runLits('let f = import(cli-fs); f.remove("does-not-exist.txt")')
      }).toThrow()
    })
  })
})
