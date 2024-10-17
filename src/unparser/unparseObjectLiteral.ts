import type { NormalExpressionNode } from '../parser/interface'
import type { UnparseOptions } from './UnparseOptions'
import { unparseMultilinePairwise, unparseMultilineParams, unparseSingleLinePairs } from './unparseParams'
import { applyMetaTokens } from './utils'

export function unparseObjectLiteral(node: NormalExpressionNode, options: UnparseOptions): string {
  const startBracket = applyMetaTokens('{', node.debugData?.token.debugData?.metaTokens, options)
  const endBracket = applyMetaTokens('}', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())

  // If no parameters, return empty object literal
  if (node.p.length === 0)
    return `${startBracket}${endBracket}`

  const params = node.p

  if (!startBracket.endsWith('\n')) {
  // 1. Try to unparse the parameters as one line
    try {
      const unparsedParams = unparseSingleLinePairs(params, options.inline().lock())

      if (!unparsedParams.includes('\n')) {
        const result = `${startBracket}${unparsedParams}${endBracket}`
        return options.assertNotOverflown(result)
      }
    }
    catch (error) {
    // If locked, we do not try anything else
      if (options.locked)
        throw error
    }
  }

  // 2. Try to unparse the parameters pairwise on multiple lines
  // e.g. {:a 1 :b 2 :c 3 :d 4 :e 5}
  // ==>  {:a 1
  //       :b 2
  //       :c 3
  //       :d 4
  //       :e 5}
  const multilineOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc()
  try {
    const paiwise = unparseMultilinePairwise(params, multilineOptions)
    const result = `${startBracket}${paiwise}${endBracket}`
    return options.assertNotOverflown(result)
  }
  catch {
    // Continue to the next step
  }

  // 3. Try to unparse the parameters in multiple lines
  return startBracket + unparseMultilineParams(node.p, multilineOptions) + endBracket
}
