import process from 'node:process'
import type { JsFunction } from '../../../../../src'
import type { Any } from '../../../../../src/interface'
import type { LitsModule } from '../../../../../src/builtin/modules/interface'
import type { BuiltinNormalExpressions } from '../../../../../src/builtin/interface'

export function sys_cwd(): string {
  return process.cwd()
}

const getCwd: JsFunction = {
  fn: (): string => process.cwd(),
  docString: 'Returns the current working directory of the process',
  arity: { min: 0, max: 0 },
}

const exit: JsFunction = {
  fn: (code: number = 0): never => {
    process.exit(code)
  },
  docString: `
Exits the process with the given exit code

Parameters:
- code (number): The exit code to use. Defaults to 0 (success).
`,
  arity: { min: 0, max: 1 },
}

const getPid: JsFunction = {
  fn: (): number => process.pid,
  docString: 'Returns the current process ID',
  arity: { min: 0, max: 0 },
}

const setUmask: JsFunction = {
  fn: (mask: number): null => {
    if (typeof mask !== 'number' || !Number.isInteger(mask)) {
      throw new TypeError('Umask must be an integer')
    }

    if (mask < 0 || mask > 0o777) {
      throw new Error(`Umask out of range: ${mask.toString(8)} (must be 0-0o777)`)
    }

    process.umask(mask)
    return null
  },
  docString: `
Sets the file creation mask. Use octal literals like 0o755, 0o644, etc.',

Parameters:
- mask (number): The new file creation mask to set. Must be an integer between 0 and 0o777 (octal).
`,
  arity: { min: 1, max: 1 },
}

const getUmask: JsFunction = {
  fn: (): number => {
    // Use most restrictive mask as temporary value to be safe
    const tempMask = 0o077 // Very restrictive
    const currentMask = process.umask(tempMask)
    process.umask(currentMask) // Restore immediately
    return currentMask
  },
  docString: 'Returns the current file creation mask as a number.',
  arity: { min: 0, max: 0 },
}

function jsFnToExpression(jsFn: JsFunction): BuiltinNormalExpressions[string] {
  return {
    evaluate: params => jsFn.fn(...params) as Any,
    arity: jsFn.arity ?? {},
  }
}

export function getProcModule(): LitsModule {
  return {
    name: 'cli-proc',
    functions: {
      'get-cwd': jsFnToExpression(getCwd),
      'exit': jsFnToExpression(exit),
      'get-pid': jsFnToExpression(getPid),
      'set-umask': jsFnToExpression(setUmask),
      'get-umask': jsFnToExpression(getUmask),
    },
  }
}
