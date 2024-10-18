import type { SpecialExpressionNode } from '../../builtin'
import type { AndNode } from '../../builtin/specialExpressions/and'
// import type { AstNode, NormalExpressionNode } from '../../parser/interface'
import type { UnparseOptions } from '../UnparseOptions'
import { unparseNormalExpressionNode } from '../unparseNormalExpression'
import { unparseCond } from './unparseCond'
import { unparseDo } from './unparseDo'
import { unparseIfLet } from './unparseIfLet'
import { unparseLet } from './unparseLet'

// type ExpressionWithSingleParamNode = Pick<NormalExpressionNode, 'debug' | 'n'> & { p: AstNode }

// function expressionWithSingleParamUnparser(astNode: ExpressionWithSingleParamNode, options: UnparseOptions) {
//   return unparseNormalExpressionNode({ ...astNode, p: [astNode.p] }, options)
// }

const specialExpressionUnparser = {
  'and': unparseNormalExpressionNode,
  'comment': unparseNormalExpressionNode,
  'cond': unparseCond,
  'declared?': unparseNormalExpressionNode,
  // 'defn': (astNode: DefnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'def': unparseNormalExpressionNode,
  // 'defns': (astNode: DefnsNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'defs': unparseNormalExpressionNode,
  'do': unparseDo,
  // 'doseq': (astNode: DoSeqNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'fn': (astNode: FnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'for': (astNode: ForNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'if-let': unparseIfLet,
  'if': unparseNormalExpressionNode,
  'if-not': unparseNormalExpressionNode,
  'let': unparseLet,
  // 'loop': (astNode: LoopNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'or': unparseNormalExpressionNode,
  '??': unparseNormalExpressionNode,
  'recur': unparseNormalExpressionNode,
  'time!': unparseNormalExpressionNode,
  'throw': unparseNormalExpressionNode,
  // 'try': (astNode: TryNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when-first': (astNode: WhenFirstNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when-let': (astNode: WhenLetNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when': (astNode: WhenNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when-not': (astNode: WhenNotNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),

} satisfies Record<string /* TODO: SpecialExpressionName */, (astNode: any, options: UnparseOptions) => string>

export function unparseSpecialExpression(node: SpecialExpressionNode, options: UnparseOptions): string {
  const unparser = specialExpressionUnparser[node.n as 'and']
  return unparser(node as AndNode, options)
}
