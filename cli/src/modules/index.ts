import type { JsFunction } from '../../../src/Lits/Lits'
import { sys_cwd, sys_readFile, sys_writeFile } from './sys'

const sysFunctions: Record<string, JsFunction> = {
  'Sys.cwd': {
    fn: sys_cwd,
    docString: 'Returns the current working directory of the process.',
    arity: { min: 0, max: 0 },
  },
  'Sys.read-file': {
    fn: sys_readFile,
    docString: 'Reads the content of a file at the specified path.',
    arity: { min: 1, max: 1 },
  },
  'Sys.write-file': {
    fn: sys_writeFile,
    docString: 'Writes content to a file at the specified path.',
    arity: { min: 2, max: 2 },
  },
}

export const jsFunctions = {
  ...sysFunctions,
}
