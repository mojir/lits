import type { SpecialExpressionNode } from '../../builtin'
import type { AndNode } from '../../builtin/specialExpressions/and'
// import type { AstNode, NormalExpressionNode } from '../../parser/interface'
import type { UnparseOptions } from '../UnparseOptions'
import { unparseNormalExpression } from '../unparseNormalExpression'
import { unparseCond } from './unparseCond'
import { unparseDo } from './unparseDo'
import { unparseIfLet } from './unparseIfLet'
import { unparseLet } from './unparseLet'
import { unparseIfOrWhenLike } from './unparseIfOrWhenLike'

// type ExpressionWithSingleParamNode = Pick<NormalExpressionNode, 'debug' | 'n'> & { p: AstNode }

// function expressionWithSingleParamUnparser(astNode: ExpressionWithSingleParamNode, options: UnparseOptions) {
//   return unparseNormalExpressionNode({ ...astNode, p: [astNode.p] }, options)
// }

const specialExpressionUnparser = {
  'and': unparseNormalExpression,
  'comment': unparseNormalExpression,
  'cond': unparseCond,
  'declared?': unparseNormalExpression,
  // 'defn': (astNode: DefnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'def': unparseNormalExpression,
  // 'defns': (astNode: DefnsNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'defs': unparseNormalExpression,
  'do': unparseDo,
  // 'doseq': (astNode: DoSeqNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'fn': (astNode: FnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'for': (astNode: ForNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'if-let': unparseIfLet,
  'if': unparseIfOrWhenLike,
  'if-not': unparseIfOrWhenLike,
  'let': unparseLet,
  // 'loop': (astNode: LoopNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'or': unparseNormalExpression,
  '??': unparseNormalExpression,
  'recur': unparseNormalExpression,
  'time!': unparseNormalExpression,
  'throw': unparseNormalExpression,
  // 'try': (astNode: TryNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when-first': (astNode: WhenFirstNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  // 'when-let': (astNode: WhenLetNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
  'when': unparseIfOrWhenLike,
  'when-not': unparseIfOrWhenLike,

} satisfies Record<string /* TODO: SpecialExpressionName */, (astNode: any, options: UnparseOptions) => string>

export function unparseSpecialExpression(node: SpecialExpressionNode, options: UnparseOptions): string {
  const unparser = specialExpressionUnparser[node.n as 'and']
  return unparser(node as AndNode, options)
}
