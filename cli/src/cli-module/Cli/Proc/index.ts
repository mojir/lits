import process from 'node:process'
import type { GetFsModule } from '../../utils'
import type { JsFunction } from '../../../../../src'

export function sys_cwd(): string {
  return process.cwd()
}

const cwd: JsFunction = {
  fn: (): string => process.cwd(),
}

export const getProcModule: GetFsModule = (modulePath) => {
  const prefix = modulePath.join('.')
  return {
    [`${prefix}.cwd`]: cwd,
  }
}
