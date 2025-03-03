import type { Builtin } from '../builtin/interface'
import { AstNodeType } from '../constants/constants'
import type { ContextStack } from '../evaluator/ContextStack'
import type { AstNode } from '../parser/interface'
import { asNonUndefined } from '../typeGuards'
import { evaluateAstNode } from '../evaluator'
import { getTokenDebugData } from '../tokenizer/utils'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { FindUnresolvedIdentifiers, UnresolvedIdentifier, UnresolvedIdentifiers } from '.'

export const findUnresolvedIdentifiers: FindUnresolvedIdentifiers = (ast, contextStack, builtin: Builtin) => {
  const astNodes = Array.isArray(ast)
    ? ast
    : [{
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: ast.b,
      token: undefined,
    } satisfies DoNode]

  const unresolvedIdentifiers = new Set<UnresolvedIdentifier>()

  for (const subNode of astNodes) {
    findUnresolvedIdentifiersInAstNode(subNode, contextStack, builtin)
      .forEach(symbol => unresolvedIdentifiers.add(symbol))
  }

  return unresolvedIdentifiers
}

function findUnresolvedIdentifiersInAstNode(astNode: AstNode, contextStack: ContextStack, builtin: Builtin): UnresolvedIdentifiers {
  const emptySet = new Set<UnresolvedIdentifier>()
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
      const unresolvedIdentifiers = new Set<UnresolvedIdentifier>()
      const { n: name, token: debug } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ t: AstNodeType.Symbol, v: name, token: debug, p: [], n: undefined })
        if (lookUpResult === null)
          unresolvedIdentifiers.add({ symbol: name, token: astNode.token })
      }
      for (const subNode of astNode.p) {
        const innerUnresolvedIdentifiers = findUnresolvedIdentifiersInAstNode(subNode, contextStack, builtin)
        innerUnresolvedIdentifiers.forEach(symbol => unresolvedIdentifiers.add(symbol))
      }
      return unresolvedIdentifiers
    }
    case AstNodeType.SpecialExpression: {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], getTokenDebugData(astNode.token)?.sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-argument
      const unresolvedIdentifiers = specialExpression.findUnresolvedIdentifiers(astNode as any, contextStack, {
        findUnresolvedIdentifiers,
        builtin,
        evaluateAstNode,
      })
      return unresolvedIdentifiers
    }
  }
}
