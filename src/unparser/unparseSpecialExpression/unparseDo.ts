import type { DoNode } from '../../builtin/specialExpressions/do'
import type { UnparseOptions } from '../UnparseOptions'
import { unparseParams } from '../unparseParams'
import { applyMetaTokens } from '../utils'

export function unparseDo(node: DoNode, options: UnparseOptions) {
  const startBracket = applyMetaTokens('(', node.debugData?.token.debugData?.metaTokens, options)
  const endBracket = applyMetaTokens(')', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())
  const name = applyMetaTokens(node.n, node.debugData?.nameToken?.debugData?.metaTokens, options.inline())
  const prefix = startBracket + name
  const inline = !name.includes('\n')

  return unparseParams({
    params: node.p,
    options,
    prefix,
    inline,
    name,
    endBracket,
    body: true,
  })
}
