import { Context, EvaluateAstNode } from '../evaluator/interface'
import { ParseExpression, SpecialExpressionNode } from '../parser/interface'
import { Token } from '../tokenizer/interface'

export type SpecialExpression = {
  parse: (tokens: Token[], position: number, parseExpression: ParseExpression) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: Context[], evaluateAstNode: EvaluateAstNode) => unknown
  validate: (node: SpecialExpressionNode) => void
}
