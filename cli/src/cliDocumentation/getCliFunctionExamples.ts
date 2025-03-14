/* eslint-disable no-console */

import { stringifyValue } from '../../../common/utils'
import type { Reference } from '../../../reference'
import { Lits } from '../../../src/Lits/Lits'
import type { Colorizer } from '../colorizer'
import { getLitsFormatter } from '../cliFormatterRules'

const lits = new Lits({ debug: false })

export function getCliFunctionExamples(fmt: Colorizer, reference: Reference) {
  const { examples } = reference
  return examples
    .map(example => example.trim())
    .map((example) => {
      const oldLog = console.log
      console.log = function () {}
      const oldWarn = console.warn
      console.warn = function () {}
      let result
      try {
        result = lits.run(`(try (do ${example}) (catch e e))`)
        const stringifiedResult = stringifyValue(result, false)

        const formattedExample = getLitsFormatter(fmt)(example)

        return `${formattedExample}
${fmt.gray(stringifiedResult)}`
      }
      finally {
        console.log = oldLog
        console.warn = oldWarn
      }
    })
    .join('\n\n')
}
