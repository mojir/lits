/* eslint-disable no-console */
import { stringifyValue } from '../../common/utils'
import { allBuiltinModules } from '../../src/allModules'
import { Lits } from '../../src/Lits/Lits'
import { formatLitsExpression } from './formatter/rules'
import { styles } from './styles'

const lits = new Lits({ debug: false, modules: allBuiltinModules })

export interface RenderExampleOptions {
  noRun?: boolean
  noResult?: boolean
}

export function renderExample(example: string | string[], name: string, options?: RenderExampleOptions): string {
  const code = Array.isArray(example) ? example.join('\n') : example
  const { noRun = false, noResult = false } = options ?? {}
  const encodedUriExample = btoa(encodeURIComponent(code))
  const formattedExample = formatLitsExpression(code)

  if (noRun) {
    return `
            <div ${styles('text-sm', 'font-mono', 'flex', 'flex-col')}>
              <div ${styles('flex', 'gap-3')} >
                <span ${styles('font-bold', 'select-none')}>&gt;</span>
                <span ${styles('cursor-pointer', 'whitespace-pre-wrap')} onclick="Playground.addToPlayground('${name}', '${encodedUriExample}')">${formattedExample}</span>
              </div>
            </div>
              `
  }

  const oldLog = console.log
  console.log = function () {}
  try {
    const result = lits.run(`try ${code} catch(e) e end`)
    const stringifiedResult = stringifyValue(result, true)

    return `
            <div ${styles('text-sm', 'font-mono', 'flex', 'flex-col')}>
              <div ${styles('flex', 'gap-3')} >
                <span ${styles('font-bold', 'select-none')}>&gt;</span>
                <span ${styles('cursor-pointer', 'whitespace-pre-wrap')} onclick="Playground.addToPlayground('${name}', '${encodedUriExample}')">${formattedExample}</span>
              </div>
              ${noResult ? '' : `<div ${styles('whitespace-pre-wrap', 'text-color-gray-400', 'mt-1')}>${stringifiedResult}</div>`}
            </div>
              `
  }
  catch (e) {
    console.error(`Error in example: ${name}`, example)
    throw e
  }
  finally {
    console.log = oldLog
  }
}
