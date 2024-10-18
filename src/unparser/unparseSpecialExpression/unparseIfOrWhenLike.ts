import type { IfNode } from '../../builtin/specialExpressions/if'
import type { IfNotNode } from '../../builtin/specialExpressions/if-not'
import type { WhenNode } from '../../builtin/specialExpressions/when'
import type { WhenNotNode } from '../../builtin/specialExpressions/when-not'
import type { UnparseOptions } from '../UnparseOptions'
import { unparseParams } from '../unparseParams'
import { applyMetaTokens } from '../utils'

export function unparseIfOrWhenLike(node: WhenNode | WhenNotNode | IfNode | IfNotNode, options: UnparseOptions): string {
  const startBracket = applyMetaTokens('(', node.debugData?.token.debugData?.metaTokens, options)
  const endBracket = applyMetaTokens(')', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())

  const nameOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc()
  const name = applyMetaTokens(node.n, node.debugData?.nameToken?.debugData?.metaTokens, nameOptions)

  const inc = name.includes('\n') ? 1 : name.length + 2
  const unparsedCondition = options.unparse(node.p[0]!, options.inc(inc).inline())

  const prefix = `${startBracket + name} ${unparsedCondition}`
  const params = node.p.slice(1)
  // const inline = !(name + unparsedCondition).includes('\n')
  const inline = false

  return unparseParams({ params, options, prefix, inline, name, endBracket, body: node.n === 'when' || node.n === 'when-not' })
}
