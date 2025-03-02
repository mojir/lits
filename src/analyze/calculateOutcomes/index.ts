import type { Outcomes } from '..'
import { builtin } from '../../builtin'
import { AstNodeType } from '../../constants/constants'
import { evaluate } from '../../evaluator'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context } from '../../evaluator/interface'
import type { Ast, AstNode } from '../../parser/interface'
import { findUnresolvedIdentifiers } from '../findUnresolvedIdentifiers'
import { combinate } from '../utils'
import { specialExpressionCalculator } from './specialExpressionCalculators'

export type CalculatePossibleAstNodes = (astNode: AstNode, indentifiers?: string[]) => AstNode[]
export type CombinateAstNodes = (astNodes: AstNode[], indentifiers?: string[]) => AstNode[][]
export type IsAstComputable = (node: AstNode | AstNode[] | AstNode[][]) => boolean
export type AddGlobaleIdentifier = (name: string) => void

export interface CalculatePossibleAstNodesHelperOptions<T extends AstNode> {
  astNode: T
  nilNode: AstNode
  calculatePossibleAstNodes: CalculatePossibleAstNodes
  combinateAstNodes: CombinateAstNodes
  isAstComputable: IsAstComputable
  addGlobalIdentifier: AddGlobaleIdentifier
}

export type CalculatePossibleAstNodesHelper<T extends AstNode> = (
  options: CalculatePossibleAstNodesHelperOptions<T>,
) => AstNode[]

export function calculateOutcomes(contextStack: ContextStack, astNodes: AstNode[]): Outcomes | null {
  // First, we try to calculate outcomes for the whole astNodes array.
  // If that fails, we try to calculate outcomes for the array without the first element.
  // If that fails, we try to calculate outcomes for the array without the first two elements.
  // And so on.

  // This makes it possible to calculate outcomes for e.g.
  // (write! x) x

  // Problems occur for e.g.
  // (def x 1) (write! x) x
  // This should output [1], but since (write! x) fails to calculate outcomes, we get null.

  // Ok, but not optimal
  // The contract is that when an array is returned, it must be correct.
  // But returning null (indicating that the calculation failed) is always a way out.
  for (let i = 0; i < astNodes.length; i++) {
    const usingAstNode = astNodes.slice(i)
    const outcomes = calculateOutcomesInner(contextStack, usingAstNode)
    if (outcomes !== null)
      return outcomes
  }
  return null
}

export function calculateOutcomesInner(contextStack: ContextStack, astNodes: AstNode[]): Outcomes | null {
  const possibleAsts = calculatePossibleAsts(contextStack.clone(), astNodes)

  if (possibleAsts === null)
    return null

  const outcomes: Outcomes = []

  for (const possibleAst of possibleAsts) {
    const unresolvedIdentifiers = findUnresolvedIdentifiers(possibleAst, contextStack.clone(), builtin)
    if (unresolvedIdentifiers.size !== 0)
      return null

    const ast: Ast = {
      b: possibleAst,
      hasDebugData: true,
    }
    try {
      outcomes.push(evaluate(ast, contextStack.clone()))
    }
    catch (e) {
      outcomes.push(e)
    }
  }

  return outcomes
}

function calculatePossibleAsts(contextStack: ContextStack, astNodes: AstNode[]): AstNode[][] | null {
  let possibleAsts: AstNode[][]

  try {
    possibleAsts = combinate(
      astNodes.map(
        astNode => calculatePossibleAstNodes(contextStack, astNode),
      ),
    )
  }
  catch (e) {
    return null
  }
  return possibleAsts
}

const nilNode: AstNode = { t: AstNodeType.ReservedSymbol, v: 'nil', token: undefined, p: [], n: undefined }

function calculatePossibleAstNodes(contextStack: ContextStack, astNode: AstNode, newIndentifiers?: string[]): AstNode[] {
  const newContext = newIndentifiers
    ? newIndentifiers.reduce((acc: Context, identity) => {
      acc[identity] = { value: null }
      return acc
    }, {})
    : undefined
  const newContextStack = newContext ? contextStack.create(newContext) : contextStack

  if (astNode.t === AstNodeType.NormalExpression) {
    return combinate(astNode.p.map(n => calculatePossibleAstNodes(newContextStack, n)))
      .map(p => ({ ...astNode, p }))
  }
  else if (astNode.t === AstNodeType.SpecialExpression) {
    const helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'> = {
      nilNode,
      calculatePossibleAstNodes: (node: AstNode, identifiers?: string[]) => calculatePossibleAstNodes(newContextStack.clone(), node, identifiers),
      combinateAstNodes: (nodes: AstNode[], identifiers?: string[]) =>
        combinate(nodes.map(node => calculatePossibleAstNodes(newContextStack.clone(), node, identifiers))),
      isAstComputable: (node: AstNode | AstNode[] | AstNode[][]) =>
        calculateOutcomesInner(newContextStack, Array.isArray(node) ? node.flat() : [node]) !== null,
      addGlobalIdentifier: (name: string) => newContextStack.globalContext[name] = { value: null },
    }

    // eslint-disable-next-line ts/no-unsafe-argument
    return specialExpressionCalculator[astNode.n](astNode as any, helperOptions)
  }
  return [astNode]
}
