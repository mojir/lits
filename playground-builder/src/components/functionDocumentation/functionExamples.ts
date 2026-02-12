/* eslint-disable no-console */
import { Lits } from '../../../../src/Lits/Lits'
import { styles } from '../../styles/index'
import type { Reference } from '../../../../reference'
import { formatLitsExpression } from '../../formatter/rules'
import { stringifyValue } from '../../../../common/utils'
import { categoryToNamespace, namespaceCategories, type Category } from '../../../../reference/api'

const lits = new Lits({ debug: false })

function isNamespaceCategory(category: string): category is Category {
  return namespaceCategories.includes(category as Category)
}

// Map from namespace name to a safe variable name (some namespace names clash with built-ins)
const safeVarName: Record<string, string> = {
  nth: 'nt', // 'nth' is a built-in function
}

function wrapNamespaceExample(example: string, category: string): string {
  if (!isNamespaceCategory(category)) {
    return example
  }
  const nsName = categoryToNamespace[category as keyof typeof categoryToNamespace]
  if (!nsName) {
    return example
  }
  
  // Use a safe variable name that doesn't clash with built-ins
  const varName = safeVarName[nsName] || nsName
  
  // Replace function calls with the safe variable name
  let modifiedExample = example.replace(
    new RegExp(`${nsName}\\.`, 'g'),
    `${varName}.`,
  )
  
  // Replace function calls like "ns.func-name(...)" with "(get ns \"func-name\")(...)"
  // for functions that start with a number (like mat.1-norm)
  modifiedExample = modifiedExample.replace(
    new RegExp(`${varName}\\.(\\d[a-zA-Z0-9-]*)\\(`, 'g'),
    `(get ${varName} "$1")(`,
  )
  
  // Wrap the example with the import statement using statement syntax
  return `let ${varName} = import("${nsName}"); ${modifiedExample}`
}

export function getFunctionExamples(reference: Reference) {
  const { examples, title: name, category } = reference
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
            // For namespace functions, wrap with import
            const executableExample = wrapNamespaceExample(example, category)
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
