import { Context, EvaluateLispishFunction } from '../../evaluator/interface'
import { NormalExpressionNode } from '../../parser/interface'

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

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression>
