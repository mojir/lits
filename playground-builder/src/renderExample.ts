/* eslint-disable no-console */
import { stringifyValue } from '../../common/utils'
import { allBuiltinModules } from '../../src/allModules'
import { Lits } from '../../src/Lits/Lits'
import { formatLitsExpression } from './formatter/rules'
import { copyIcon, playIcon } from './icons'
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

  const playButton = `<div class="example-action-btn" ${styles('p-2', 'text-lg', 'cursor-pointer')} onclick="event.stopPropagation(); Playground.addToPlayground('${name}', '${encodedUriExample}')">${playIcon}</div>`
  const copyButton = `<div class="example-action-btn" ${styles('p-2', 'text-lg', 'cursor-pointer')} onclick="event.stopPropagation(); Playground.copyExample('${encodedUriExample}')">${copyIcon}</div>`
  const actionBar = `<div class="example-action-bar" ${styles('absolute', 'top-0', 'right-0', 'flex-row', 'margin-top: 2px;')}>${playButton}${copyButton}</div>`

  const codeSection = `<div ${styles('py-3', 'px-4', 'text-sm', 'font-mono', 'whitespace-pre-wrap')}>${formattedExample}</div>`

  if (noRun) {
    return `<div class="example-code" ${styles('relative', 'flex', 'flex-col', 'mb-5')} style="overflow-x: auto;">${actionBar}${codeSection}</div>`
  }

  const oldLog = console.log
  console.log = function () {}
  try {
    const result = lits.run(`try\n${code}\ncatch(e) e end`)
    const stringifiedResult = stringifyValue(result, true)
    const resultSection = noResult
      ? ''
      : `
      <div class="example-result" ${styles('px-4', 'py-2', 'text-sm', 'font-mono', 'whitespace-pre-wrap', 'text-color-gray-400', 'flex', 'flex-row', 'gap-2', 'items-start')}>
        <div ${styles('text-2xl', 'line-height: 1.25rem;')}>⇒</div>
        <div ${styles('padding-top: 3px;')}>${stringifiedResult}</div>
      </div>`

    return `<div class="example-code" ${styles('relative', 'flex', 'flex-col', 'mb-5')} style="overflow-x: auto;">${actionBar}${codeSection}${resultSection}</div>`
  }
  catch (e) {
    console.error(`Error in example: ${name}`, example)
    throw e
  }
  finally {
    console.log = oldLog
  }
}
