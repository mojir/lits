import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType, TokenType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode, NameNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { assertNameNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import { asToken, assertToken } from '../../typeGuards/token'
import { getSourceCodeInfo } from '../../utils/debug/getSourceCodeInfo'
import type { BuiltinSpecialExpression } from '../interface'

export interface TryNode extends CommonSpecialExpressionNode<'try'> {
  e: NameNode
  ce: AstNode
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  parse: (tokenStream, position, firstToken, { parseToken }) => {
    let tryExpression: AstNode
    ;[position, tryExpression] = parseToken(tokenStream, position)

    assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '(' })
    position += 1

    let catchNode: AstNode
    ;[position, catchNode] = parseToken(tokenStream, position)
    assertNameNode(catchNode, catchNode.debugData?.token.debugData?.sourceCodeInfo)
    if (catchNode.v !== 'catch') {
      throw new LitsError(
        `Expected 'catch', got '${catchNode.v}'.`,
        getSourceCodeInfo(catchNode, catchNode.debugData?.token.debugData?.sourceCodeInfo),
      )
    }

    let error: AstNode
    ;[position, error] = parseToken(tokenStream, position)
    assertNameNode(error, error.debugData?.token.debugData?.sourceCodeInfo)

    let catchExpression: AstNode
    ;[position, catchExpression] = parseToken(tokenStream, position)

    assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })
    position += 1

    const lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: TryNode = {
      t: AstNodeType.SpecialExpression,
      n: 'try',
      p: [tryExpression],
      ce: catchExpression,
      e: error,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams(1, node)

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    try {
      return evaluateAstNode(tryExpressions[0]!, contextStack)
    }
    catch (error) {
      const newContext: Context = {
        [errorNode.v]: { value: asAny(error, node.debugData?.token.debugData?.sourceCodeInfo) },
      }
      return evaluateAstNode(catchExpression, contextStack.create(newContext))
    }
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    const tryResult = findUnresolvedIdentifiers(tryExpressions, contextStack, builtin)
    const newContext: Context = {
      [errorNode.v]: { value: true },
    }
    const catchResult = findUnresolvedIdentifiers([catchExpression], contextStack.create(newContext), builtin)
    return joinAnalyzeResults(tryResult, catchResult)
  },
}
