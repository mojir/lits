/* eslint-disable ts/no-unsafe-member-access */
import { execSync } from 'node:child_process'
import path from 'node:path'
import { describe, expect, test } from 'vitest'

describe('proc Integration Tests', () => {
  const litsCliPath = path.join(__dirname, '../../dist/cli/cli.js') // Adjust path as needed

  function runLits(expression: string): string {
    try {
      const result = execSync(`node '${litsCliPath}' -p -e '${expression}'`, {
        encoding: 'utf8',
        stdio: 'pipe',
        cwd: __dirname, // Ensure we run in the correct directory
        env: { ...process.env, NO_COLOR: '1', FORCE_COLOR: '0' },
      })
      return result.trim()
    }
    catch (error: any) {
      throw new Error(`Lits Proc failed: ${error.message}\nStdout: ${error.stdout}\nStderr: ${error.stderr}`)
    }
  }

  describe('process operations', () => {
    test('shuld return current working directory', () => {
      const result = runLits('Cli.Proc.get-cwd()')
      expect(result).toBe(__dirname)
    })
  })
})
