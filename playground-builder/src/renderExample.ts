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

  const codeSection = `<div ${styles('p-4', 'text-sm', 'font-mono', 'whitespace-pre-wrap')}>${formattedExample}</div>`

  if (noRun) {
    return `<div class="example-code" ${styles('flex', 'flex-col', 'mb-4', 'cursor-pointer')} style="overflow-x: auto;" onclick="Playground.addToPlayground('${name}', '${encodedUriExample}')">${codeSection}</div>`
  }

  const oldLog = console.log
  console.log = function () {}
  try {
    const result = lits.run(`try\n${code}\ncatch(e) e end`)
    const stringifiedResult = stringifyValue(result, true)
    const resultSection = noResult ? '' : `<div class="example-result" ${styles('px-4', 'py-2', 'text-sm', 'font-mono', 'whitespace-pre-wrap', 'text-color-gray-400')}>${stringifiedResult}</div>`

    return `<div class="example-code" ${styles('flex', 'flex-col', 'mb-4', 'cursor-pointer')} style="overflow-x: auto;" onclick="Playground.addToPlayground('${name}', '${encodedUriExample}')">${codeSection}${resultSection}</div>`
  }
  catch (e) {
    console.error(`Error in example: ${name}`, example)
    throw e
  }
  finally {
    console.log = oldLog
  }
}
