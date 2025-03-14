/* eslint-disable no-console */
import { Lits } from '../../../../src/Lits/Lits'
import { styles } from '../../styles/index'
import type { Reference } from '../../../../reference'
import { formatLitsExpression } from '../../formatter/rules'
import { stringifyValue } from '../../../../common/utils'

const lits = new Lits({ debug: false })

export function getFunctionExamples(reference: Reference) {
  const { examples, title: name } = reference
  return `
    <div ${styles('flex', 'flex-col', 'gap-4')}>
      ${examples
        .map(example => example.trim())
        .map((example) => {
          const oldLog = console.log
          console.log = function () {}
          const oldWarn = console.warn
          console.warn = function () {}
          const encodedUriExample = btoa(encodeURIComponent(example))
          try {
            const result = lits.run(`try ${example} catch(e) e end`)
            const stringifiedResult = stringifyValue(result, true)

            const formattedExample = formatLitsExpression(example)

            return `
            <div ${styles('text-sm', 'font-mono', 'flex', 'flex-col')}>
              <div ${styles('flex', 'gap-3')} >
                <span ${styles('font-bold', 'select-none')}>&gt;</span>
                <span ${styles('cursor-pointer', 'whitespace-pre-wrap')} onclick="Playground.addToPlayground('${name}', '${encodedUriExample}')">${formattedExample}</span>
              </div>
              <div ${styles('whitespace-pre-wrap', 'text-color-gray-400', 'mt-1')}>${stringifiedResult}</div>
            </div>
              `
          }
          catch (e) {
            console.error(`Error in ${name} example: `, example)
            throw e
          }
          finally {
            console.log = oldLog
            console.warn = oldWarn
          }
        })
        .join('\n')}
    </div>`
}
