import { Context } from '../evaluator/interface'
import { SpecialExpressionNode } from '../parser/interface'
import { Token } from '../tokenizer/interface'

type SpecialExpressionParser = (tokens: Token[], position: number) => [number, SpecialExpressionNode]
export type SpecialExpression = {
  parse: SpecialExpressionParser
  evaluate: (node: SpecialExpressionNode, contextStack: Context[]) => unknown
  validate: (node: SpecialExpressionNode) => void
}
