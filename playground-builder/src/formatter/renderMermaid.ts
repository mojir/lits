import { execSync } from 'node:child_process'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import process from 'node:process'

export function renderMermaidToSvg(code: string): string {
  const tmpDir = os.tmpdir()
  const timestamp = Date.now()
  const inputFile = path.join(tmpDir, `lits-mermaid-${timestamp}.mmd`)
  const outputFile = path.join(tmpDir, `lits-mermaid-${timestamp}.svg`)
  const mmdc = path.resolve(process.cwd(), 'node_modules/.bin/mmdc')

  try {
    fs.writeFileSync(inputFile, code)
    execSync(`"${mmdc}" -i "${inputFile}" -o "${outputFile}" -b transparent -t dark`, {
      stdio: 'pipe',
      timeout: 30000,
    })
    return fs.readFileSync(outputFile, 'utf-8')
  }
  finally {
    try {
      fs.unlinkSync(inputFile)
    }
    catch {
      /* ignore */
    }
    try {
      fs.unlinkSync(outputFile)
    }
    catch {
      /* ignore */
    }
  }
}
