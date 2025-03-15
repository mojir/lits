import type { Builtin } from '../builtin/interface'
import type { DoNode } from '../builtin/specialExpressions/do'
import { AstNodeType } from '../constants/constants'
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
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: ast.b,
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
  switch (astNode.t) {
    case AstNodeType.Symbol: {
      const lookUpResult = contextStack.lookUp(astNode)
      if (lookUpResult === null)
        return new Set([astNode.v])

      return null
    }
    case AstNodeType.String:
    case AstNodeType.Number:
    case AstNodeType.Modifier:
    case AstNodeType.ReservedSymbol:
    case AstNodeType.Comment:
      return null
    case AstNodeType.NormalExpression: {
      const unresolvedSymbols = new Set<string>()
      const { n: name, token: debug } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ t: AstNodeType.Symbol, v: name, token: debug, p: [], n: undefined })
        if (lookUpResult === null)
          unresolvedSymbols.add(name)
      }
      for (const subNode of astNode.p) {
        findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)?.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      return unresolvedSymbols
    }
    case AstNodeType.SpecialExpression: {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], tokenSourceCodeInfo(astNode.token))

      // eslint-disable-next-line ts/no-unsafe-argument
      return specialExpression.getUndefinedSymbols(astNode as any, contextStack, {
        getUndefinedSymbols,
        builtin,
        evaluateAstNode,
      })
    }
  }
}
