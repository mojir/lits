import process from 'node:process'
import fs from 'node:fs'

export function sys_cwd(): string {
  return process.cwd()
}

export function sys_readFile(filePath: string): string {
  return fs.readFileSync(filePath, { encoding: 'utf8' })
}

export function sys_writeFile(filePath: string, content: string): void {
  fs.writeFileSync(content, filePath, { encoding: 'utf8' })
}
