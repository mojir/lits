/* eslint-disable no-console */
import { Lits } from '../../../../src/Lits/Lits'
import { allBuiltinNamespaces } from '../../../../src/allNamespaces'
import '../../../../src/initReferenceData'
import { styles } from '../../styles/index'
import type { Reference } from '../../../../reference'
import { formatLitsExpression } from '../../formatter/rules'
import { stringifyValue } from '../../../../common/utils'

const lits = new Lits({ debug: false, namespaces: allBuiltinNamespaces })

// Function names starting with a digit need special handling: ns.1-norm(...) -> (get ns "1-norm")(...)
function fixNumericFunctionNames(example: string): string {
  // Match patterns like: varName.1-norm( and replace with (get varName "1-norm")(
  return example.replace(
    /(\w+)\.(\d[a-z0-9-]*)\(/gi,
    '(get $1 "$2")(',
  )
}

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
            // Fix numeric function names (e.g., mat.1-norm -> (get mat "1-norm"))
            const executableExample = fixNumericFunctionNames(example)
            // Examples are now self-contained with imports
            const result = lits.run(`try ${executableExample} catch(e) e end`)
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
