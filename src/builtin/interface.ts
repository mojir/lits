import { Context, EvaluateAstNode } from '../evaluator/interface'
import {
  ParseArgument,
  ParseBinding,
  ParseExpression,
  ParseParams,
  ParseToken,
  SpecialExpressionNode,
} from '../parser/interface'
import { Token } from '../tokenizer/interface'

type Parsers = {
  parseExpression: ParseExpression
  parseParams: ParseParams
  parseToken: ParseToken
  parseBinding: ParseBinding
  parseArgument: ParseArgument
}

export type SpecialExpression = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: Context[], evaluateAstNode: EvaluateAstNode) => unknown
  validate?: (node: SpecialExpressionNode) => void
}
