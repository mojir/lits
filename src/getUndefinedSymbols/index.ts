import type { Builtin } from '../builtin/interface'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateAstNode } from '../evaluator/interface'
import type { Ast, AstNode } from '../parser/types'
import { asNonUndefined } from '../typeGuards'

export type UndefinedSymbols = Set<string>

export const getUndefinedSymbols: GetUndefinedSymbols = (ast, contextStack, builtin, evaluateAstNode) => {
  const astNodes = Array.isArray(ast)
    ? ast
    : [{
      type: 'SpecialExpression',
      name: 'do',
      params: ast.body,
      sourceCodeInfo: undefined,
    } satisfies DoNode]

  const unresolvedSymbols = new Set<string>()

  for (const subNode of astNodes) {
    findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin, evaluateAstNode)
      ?.forEach(symbol => unresolvedSymbols.add(symbol))
  }
  return unresolvedSymbols
}

export type GetUndefinedSymbols = (ast: Ast | AstNode[], contextStack: ContextStack, builtin: Builtin, evaluateAstNode: EvaluateAstNode) => UndefinedSymbols

function findUnresolvedSymbolsInAstNode(astNode: AstNode, contextStack: ContextStack, builtin: Builtin, evaluateAstNode: EvaluateAstNode): UndefinedSymbols | null {
  switch (astNode.type) {
    case 'Symbol': {
      const lookUpResult = contextStack.lookUp(astNode)
      if (lookUpResult === null)
        return new Set([astNode.value])

      return null
    }
    case 'String':
    case 'Number':
    case 'Modifier':
    case 'ReservedSymbol':
    case 'Comment':
      return null
    case 'NormalExpression': {
      const unresolvedSymbols = new Set<string>()
      const { name, sourceCodeInfo } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ type: 'Symbol', value: name, sourceCodeInfo })
        if (lookUpResult === null)
          unresolvedSymbols.add(name)
      }
      for (const subNode of astNode.params) {
        findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin, evaluateAstNode)?.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      return unresolvedSymbols
    }
    case 'SpecialExpression': {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.name], astNode.sourceCodeInfo)

      // eslint-disable-next-line ts/no-unsafe-argument
      return specialExpression.getUndefinedSymbols(astNode as any, contextStack, {
        getUndefinedSymbols,
        builtin,
        evaluateAstNode,
      })
    }
    case 'Spread':
      return findUnresolvedSymbolsInAstNode(astNode.value, contextStack, builtin, evaluateAstNode)
  }
}
