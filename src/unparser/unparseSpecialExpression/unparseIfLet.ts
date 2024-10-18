import type { IfLetNode } from '../../builtin/specialExpressions/if-let'
import { AstNodeType } from '../../constants/constants'
import type { AstNode, BindingNode, NormalExpressionNode } from '../../parser/interface'
import { unparseBindings } from '../unparseBindings'
import type { UnparseOptions } from '../UnparseOptions'
import { unparseParams } from '../unparseParams'
import { applyMetaTokens } from '../utils'

export function unparseIfLet(node: IfLetNode, options: UnparseOptions) {
  const startBracket = applyMetaTokens('(', node.debugData?.token.debugData?.metaTokens, options)
  const endBracket = applyMetaTokens(')', node.debugData?.lastToken?.debugData?.metaTokens, options.inline())
  const name = applyMetaTokens(node.n, node.debugData?.nameToken?.debugData?.metaTokens, options.inline())

  const letArray = node.debugData ? node.debugData.bindingArray : createLetArray(node.b)

  const inc = name.includes('\n') ? 1 : name.length + 2
  const unparsedLetArray = unparseBindings(letArray, options.inc(inc).inline())

  const prefix = `${startBracket + name} ${unparsedLetArray}`
  const inline = !(name + unparsedLetArray).includes('\n')
  return unparseParams({
    params: node.p,
    options,
    prefix,
    inline,
    endBracket,
    body: true,
    noMultilineInline: true,
  })
}

function createLetArray(bindingNode: BindingNode) {
  const params: AstNode[] = [
    {
      t: AstNodeType.Name,
      n: undefined,
      p: [],
      debugData: undefined,
      v: bindingNode.n,
    },
    bindingNode.v,
  ]

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'array',
    p: params,
    debugData: undefined,
  }
  return node
}
