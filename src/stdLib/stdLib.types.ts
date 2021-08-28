import { BasicExpressionNode } from '../parser/Parser.types'

type Evaluate = (params: unknown[]) => unknown
type ValidateNode = (node: BasicExpressionNode) => void

export type StdLib = Record<
  string,
  {
    evaluate: Evaluate
    validate?: ValidateNode
  }
>

export type StdLibEvaluators = Record<string, Evaluate>
export type StdLibValidators = Record<string, ValidateNode>
