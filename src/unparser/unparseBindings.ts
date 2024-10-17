import type { NormalExpressionNode } from '../parser/interface'
import type { UnparseOptions } from './UnparseOptions'
import { unparseMultilinePairwise, unparseMultilineParams, unparseSingleLinePairs } from './unparseParams'
import { applyMetaTokens } from './utils'

export function unparseBindings(node: NormalExpressionNode, options: UnparseOptions): string {
  const startBracket = applyMetaTokens('[', node.debugData?.token.debugData?.metaTokens, options)

  // If no parameters, return empty array literal
  if (node.p.length === 0) {
    const endBracket = applyMetaTokens(']', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())
    return `${startBracket}${endBracket}`
  }

  const params = node.p

  // 1. Try to unparse the bindings as one line
  try {
    const unparsedParams = unparseSingleLinePairs(params, options.inline().lock())
    const endBracket = applyMetaTokens(']', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())
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

  // 2. Try to unparse the bindings pairwise on multiple lines
  // e.g. [a 1 b 2]
  // ==>  [a 1
  //       b 2]
  try {
    const endBracket = applyMetaTokens(']', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())
    const result = startBracket + unparseMultilinePairwise(params, options.inline().inc()) + endBracket
    return options.assertNotOverflown(result)
  }
  catch {
    // 2. Unparse the bindings on multiple lines
    // e.g. [a 1 b 2]
    // ==>  [a
    //       1,
    //       b
    //       2]
    const unparsedParams = unparseMultilineParams(params, options.inline().inc())
    const endBracket = unparsedParams.endsWith('\n')
      ? applyMetaTokens(']', node.debugData?.lastToken?.debugData?.metaTokens, options.noInline())
      : applyMetaTokens(']', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())

    return startBracket + unparseMultilineParams(params, options.inline().inc()) + endBracket
  }
}
