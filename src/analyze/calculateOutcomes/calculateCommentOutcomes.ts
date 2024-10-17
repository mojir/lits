import type { CommentExpressionNode } from '../../builtin/specialExpressions/comment'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateCommentOutcomes: CalculatePossibleAstNodesHelper<CommentExpressionNode> = ({
  nilNode,
}) => {
  return [nilNode]
}
