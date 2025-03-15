import type { Builtin } from '../builtin/interface'
import type { DoNode } from '../builtin/specialExpressions/do'
import { evaluateAstNode } from '../evaluator'
import type { ContextStack } from '../evaluator/ContextStack'
import type { Ast, AstNode } from '../parser/types'
import { tokenSourceCodeInfo } from '../tokenizer/token'
import { asNonUndefined } from '../typeGuards'

export type UndefinedSymbols = Set<string>

export const getUndefinedSymbols: GetUndefinedSymbols = (ast, contextStack, builtin: Builtin) => {
  const astNodes = Array.isArray(ast)
    ? ast
    : [{
      type: 'SpecialExpression',
      name: 'do',
      params: ast.body,
      token: undefined,
    } satisfies DoNode]

  const unresolvedSymbols = new Set<string>()

  for (const subNode of astNodes) {
    findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)
      ?.forEach(symbol => unresolvedSymbols.add(symbol))
  }
  return unresolvedSymbols
}

export type GetUndefinedSymbols = (ast: Ast | AstNode[], contextStack: ContextStack, builtin: Builtin) => UndefinedSymbols

function findUnresolvedSymbolsInAstNode(astNode: AstNode, contextStack: ContextStack, builtin: Builtin): UndefinedSymbols | null {
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
      const { name, token: debug } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ type: 'Symbol', value: name, token: debug, params: [], name: undefined })
        if (lookUpResult === null)
          unresolvedSymbols.add(name)
      }
      for (const subNode of astNode.params) {
        findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)?.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      return unresolvedSymbols
    }
    case 'SpecialExpression': {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.name], tokenSourceCodeInfo(astNode.token))

      // eslint-disable-next-line ts/no-unsafe-argument
      return specialExpression.getUndefinedSymbols(astNode as any, contextStack, {
        getUndefinedSymbols,
        builtin,
        evaluateAstNode,
      })
    }
  }
}
