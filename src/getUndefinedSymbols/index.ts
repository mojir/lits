import type { Builtin } from '../builtin/interface'
import type { DoNode } from '../builtin/specialExpressions/do'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateNode } from '../evaluator/interface'
import type { Ast, Node, NormalExpressionNode, SpecialExpressionNode, SpreadNode, UserDefinedSymbolNode } from '../parser/types'
import { isNormalExpressionNodeWithName, isUserDefinedSymbolNode } from '../typeGuards/astNode'

export type UndefinedSymbols = Set<string>

export const getUndefinedSymbols: GetUndefinedSymbols = (ast, contextStack, builtin, evaluateNode) => {
  const nodes: Node[] = Array.isArray(ast)
    ? ast
    : [[NodeTypes.SpecialExpression, [specialExpressionTypes.do, ast.body]] satisfies DoNode]

  const unresolvedSymbols = new Set<string>()

  for (const subNode of nodes) {
    findUnresolvedSymbolsInNode(subNode, contextStack, builtin, evaluateNode)
      ?.forEach(symbol => unresolvedSymbols.add(symbol))
  }
  return unresolvedSymbols
}

export type GetUndefinedSymbols = (ast: Ast | Node[], contextStack: ContextStack, builtin: Builtin, evaluateNode: EvaluateNode) => UndefinedSymbols

function findUnresolvedSymbolsInNode(node: Node, contextStack: ContextStack, builtin: Builtin, evaluateNode: EvaluateNode): UndefinedSymbols | null {
  const nodeType = node[0]
  switch (nodeType) {
    case NodeTypes.UserDefinedSymbol: {
      const symbolNode = node as UserDefinedSymbolNode
      const lookUpResult = contextStack.lookUp(symbolNode)
      if (lookUpResult === null)
        return new Set([symbolNode[1]])

      return null
    }
    case NodeTypes.NormalBuiltinSymbol:
    case NodeTypes.SpecialBuiltinSymbol:
    case NodeTypes.String:
    case NodeTypes.Number:
    case NodeTypes.ReservedSymbol:
    case NodeTypes.Binding:
      return null
    case NodeTypes.NormalExpression: {
      const normalExpressionNode = node as NormalExpressionNode
      const unresolvedSymbols = new Set<string>()
      if (isNormalExpressionNodeWithName(normalExpressionNode)) {
        const [, [symbolNode]] = normalExpressionNode
        if (isUserDefinedSymbolNode(symbolNode)) {
          const lookUpResult = contextStack.lookUp(symbolNode)
          if (lookUpResult === null)
            unresolvedSymbols.add(symbolNode[1])
        }
      }
      else {
        const [, [expressionNode]] = normalExpressionNode
        findUnresolvedSymbolsInNode(expressionNode, contextStack, builtin, evaluateNode)?.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      for (const subNode of normalExpressionNode[1][1]) {
        findUnresolvedSymbolsInNode(subNode, contextStack, builtin, evaluateNode)?.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      return unresolvedSymbols
    }
    case NodeTypes.SpecialExpression: {
      const specialExpressionNode = node as SpecialExpressionNode
      const specialExpressionType = specialExpressionNode[1][0]
      const specialExpression = builtin.specialExpressions[specialExpressionType]

      const castedGetUndefinedSymbols = specialExpression.getUndefinedSymbols as Function

      return castedGetUndefinedSymbols(specialExpressionNode, contextStack, {
        getUndefinedSymbols,
        builtin,
        evaluateNode,
      }) as UndefinedSymbols
    }
    case NodeTypes.Spread:
      return findUnresolvedSymbolsInNode((node as SpreadNode)[1], contextStack, builtin, evaluateNode)

    /* v8 ignore next 2 */
    default:
      throw new Error(`Unhandled node type: ${nodeType satisfies never}`)
  }
}
