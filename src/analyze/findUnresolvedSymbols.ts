import type { Builtin } from '../builtin/interface'
import { AstNodeType } from '../constants/constants'
import type { ContextStack } from '../evaluator/ContextStack'
import type { AstNode } from '../parser/interface'
import { asNonUndefined } from '../typeGuards'
import { evaluateAstNode } from '../evaluator'
import { getTokenDebugData } from '../tokenizer/utils'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { FindUnresolvedSymbols, UnresolvedSymbol, UnresolvedSymbols } from '.'

export const findUnresolvedSymbols: FindUnresolvedSymbols = (ast, contextStack, builtin: Builtin) => {
  const astNodes = Array.isArray(ast)
    ? ast
    : [{
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: ast.b,
      token: undefined,
    } satisfies DoNode]

  const unresolvedSymbols = new Set<UnresolvedSymbol>()

  for (const subNode of astNodes) {
    findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)
      .forEach(symbol => unresolvedSymbols.add(symbol))
  }

  return unresolvedSymbols
}

function findUnresolvedSymbolsInAstNode(astNode: AstNode, contextStack: ContextStack, builtin: Builtin): UnresolvedSymbols {
  const emptySet = new Set<UnresolvedSymbol>()
  switch (astNode.t) {
    case AstNodeType.Symbol: {
      const lookUpResult = contextStack.lookUp(astNode)
      if (lookUpResult === null)
        return new Set([{ symbol: astNode.v, token: astNode.token }])

      return emptySet
    }
    case AstNodeType.String:
    case AstNodeType.Number:
    case AstNodeType.Modifier:
    case AstNodeType.ReservedSymbol:
    case AstNodeType.Comment:
      return emptySet
    case AstNodeType.NormalExpression: {
      const unresolvedSymbols = new Set<UnresolvedSymbol>()
      const { n: name, token: debug } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ t: AstNodeType.Symbol, v: name, token: debug, p: [], n: undefined })
        if (lookUpResult === null)
          unresolvedSymbols.add({ symbol: name, token: astNode.token })
      }
      for (const subNode of astNode.p) {
        const innerUnresolvedSymbols = findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)
        innerUnresolvedSymbols.forEach(symbol => unresolvedSymbols.add(symbol))
      }
      return unresolvedSymbols
    }
    case AstNodeType.SpecialExpression: {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], getTokenDebugData(astNode.token)?.sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-argument
      const unresolvedSymbols = specialExpression.findUnresolvedSymbols(astNode as any, contextStack, {
        findUnresolvedSymbols,
        builtin,
        evaluateAstNode,
      })
      return unresolvedSymbols
    }
  }
}
