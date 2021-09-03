import { Context, EvaluateAstNode } from '../evaluator/interface'
import { ParseExpression, ParseParams, SpecialExpressionNode } from '../parser/interface'
import { Token } from '../tokenizer/interface'

type Parsers = {
  parseExpression: ParseExpression
  parseParams: ParseParams
}

export type SpecialExpression = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: Context[], evaluateAstNode: EvaluateAstNode) => unknown
  validate: (node: SpecialExpressionNode) => void
}
