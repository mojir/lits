import { Context, EvaluateAstNode, EvaluateLispishFunction } from '../evaluator/interface'
import {
  ParseArgument,
  ParseBinding,
  ParseExpression,
  ParseParams,
  ParseToken,
  SpecialExpressionNode,
} from '../parser/interface'
import { Token } from '../tokenizer/interface'
import { NormalExpressionNode } from '../parser/interface'

type Evaluate = (
  params: unknown[],
  contextStack: Context[],
  { evaluateLispishFunction }: { evaluateLispishFunction: EvaluateLispishFunction },
) => unknown
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression = {
  evaluate: Evaluate
  validate?: ValidateNode
}

type Parsers = {
  parseExpression: ParseExpression
  parseParams: ParseParams
  parseToken: ParseToken
  parseBinding: ParseBinding
  parseArgument: ParseArgument
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression>

export type SpecialExpression = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: Context[], evaluateAstNode: EvaluateAstNode) => unknown
  validate?: (node: SpecialExpressionNode) => void
}

export type SpecialExpressionName =
  | 'and'
  | 'block'
  | 'cond'
  | 'defun'
  | 'dolist'
  | 'dotimes'
  | 'function'
  | 'if'
  | 'lambda'
  | 'let'
  | 'loop'
  | 'or'
  | 'return-from'
  | 'return'
  | 'setq'
  | 'setq-constant'
  | 'throw'
  | 'try'
  | 'unless'
  | 'when'
  | 'while'
