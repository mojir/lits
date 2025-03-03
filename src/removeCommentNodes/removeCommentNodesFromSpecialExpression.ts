import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import type { AndNode } from '../builtin/specialExpressions/and'
import type { CondNode } from '../builtin/specialExpressions/cond'
import type { DeclaredNode } from '../builtin/specialExpressions/declared'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { DefnNode, DefnsNode, FnNode } from '../builtin/specialExpressions/functions'
import type { LetNode } from '../builtin/specialExpressions/let'
import type { LoopNode } from '../builtin/specialExpressions/loop'
import type { DoSeqNode, ForNode } from '../builtin/specialExpressions/loops'
import type { ThrowNode } from '../builtin/specialExpressions/throw'
import type { TryNode } from '../builtin/specialExpressions/try'
import type { SwitchNode } from '../builtin/specialExpressions/switch'
import type { RemoveOptions } from '.'

const specialExpressionCommentRemovers = {
  '&&': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'comment': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'cond': (node: CondNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'switch': (node: SwitchNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'declared?': (node: DeclaredNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'defn': (_node: DefnNode, _removeOptions: RemoveOptions) => {},
  'def': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'defns': (_node: DefnsNode, _removeOptions: RemoveOptions) => {},
  'defs': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'do': (node: DoNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'doseq': (_node: DoSeqNode, _removeOptions: RemoveOptions) => {},
  'fn': (_node: FnNode, _removeOptions: RemoveOptions) => {},
  'for': (_node: ForNode, _removeOptions: RemoveOptions) => {},
  'if': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'unless': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'let': (node: LetNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
    node.bs.forEach((bindingNode) => {
      removeOptions.recursivelyRemoveCommentNodes(bindingNode.v)
    })
  },
  'loop': (_node: LoopNode, _removeOptions: RemoveOptions) => {},
  '||': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  '??': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'recur': (node: AndNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'throw': (node: ThrowNode, removeOptions: RemoveOptions) => {
    removeOptions.removeCommenNodesFromArray(node.p)
    node.p.forEach(removeOptions.recursivelyRemoveCommentNodes)
  },
  'try': (_node: TryNode, _removeOptions: RemoveOptions) => {},
} satisfies Record<SpecialExpressionName, (astNode: any, removeOptions: RemoveOptions) => void>

export function removeCommentNodesFromSpecialExpression(
  node: SpecialExpressionNode,
  removeOptions: RemoveOptions,
) {
  const uncommenter = specialExpressionCommentRemovers[node.n as '&&']
  return uncommenter?.(node as AndNode, removeOptions)
}
