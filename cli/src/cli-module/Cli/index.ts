import { getFsModule } from './Fs/index.js'
import { getProcModule } from './Proc/index.js'

export function getCliModule() {
  return {
    ...getFsModule(['Cli', 'Fs']),
    ...getProcModule(['Cli', 'Fs']),
  }
}
