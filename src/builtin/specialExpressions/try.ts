import { joinUndefinedSymbols } from '../../analyze/undefinedSymbols/utils'
import { LitsError } from '../../errors'
import { Context } from '../../ContextStack/interface'
import { Any } from '../../interface'
import { AstNode, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { any, nameNode, token } from '../../utils/assertion'
import { getDebugInfo } from '../../utils/helpers'
import { BuiltinSpecialExpression } from '../interface'

type TryNode = SpecialExpressionNode & {
  tryExpression: AstNode
  error: NameNode
  catchExpression: AstNode
}

export const trySpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let tryExpression: AstNode
    ;[position, tryExpression] = parseToken(tokens, position)

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `(` })
    position += 1

    let catchNode: AstNode
    ;[position, catchNode] = parseToken(tokens, position)
    nameNode.assert(catchNode, catchNode.token?.debugInfo)
    if (catchNode.value !== `catch`) {
      throw new LitsError(
        `Expected 'catch', got '${catchNode.value}'.`,
        getDebugInfo(catchNode, catchNode.token?.debugInfo),
      )
    }

    let error: AstNode
    ;[position, error] = parseToken(tokens, position)
    nameNode.assert(error, error.token?.debugInfo)

    let catchExpression: AstNode
    ;[position, catchExpression] = parseToken(tokens, position)

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })
    position += 1

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })
    position += 1

    const node: TryNode = {
      type: `SpecialExpression`,
      name: `try`,
      params: [],
      tryExpression,
      catchExpression,
      error,
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [position, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { tryExpression, catchExpression, error: errorNode } = node as TryNode
    try {
      return evaluateAstNode(tryExpression, contextStack)
    } catch (error) {
      const newContext: Context = {
        [errorNode.value]: { value: any.as(error, node.token?.debugInfo) },
      } as Context
      return evaluateAstNode(catchExpression, contextStack.withContext(newContext))
    }
  },
  validateArity: () => undefined,
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) => {
    const { tryExpression, catchExpression, error: errorNode } = node as TryNode
    const tryResult = findUndefinedSymbols(tryExpression, contextStack, builtin)
    const newContext: Context = {
      [errorNode.value]: { value: true },
    }
    const catchResult = findUndefinedSymbols(catchExpression, contextStack.withContext(newContext), builtin)
    return joinUndefinedSymbols(tryResult, catchResult)
  },
}
