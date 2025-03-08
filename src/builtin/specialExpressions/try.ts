import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { assertLParenToken, assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertSymbolNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import { getSourceCodeInfo } from '../../utils/debug/getSourceCodeInfo'
import type { BuiltinSpecialExpression } from '../interface'

export interface TryNode extends CommonSpecialExpressionNode<'try'> {
  e: SymbolNode | undefined
  ce: AstNode
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseToken }) => {
    const tryExpression = parseToken(tokenStream, parseState)

    assertLParenToken(tokenStream.tokens[parseState.position++])

    const catchNode = parseToken(tokenStream, parseState)
    assertSymbolNode(catchNode, getTokenDebugData(catchNode.token)?.sourceCodeInfo)
    if (catchNode.v !== 'catch') {
      throw new LitsError(
        `Expected 'catch', got '${catchNode.v}'.`,
        getSourceCodeInfo(catchNode, getTokenDebugData(catchNode.token)?.sourceCodeInfo),
      )
    }

    const error = parseToken(tokenStream, parseState)
    assertSymbolNode(error, getTokenDebugData(error.token)?.sourceCodeInfo)

    const catchExpression = parseToken(tokenStream, parseState)

    assertRParenToken(tokenStream.tokens[parseState.position++])

    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: TryNode = {
      t: AstNodeType.SpecialExpression,
      n: 'try',
      p: [tryExpression],
      ce: catchExpression,
      e: error,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  paramCount: 1,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    try {
      return evaluateAstNode(tryExpressions[0]!, contextStack)
    }
    catch (error) {
      const newContext: Context = errorNode
        ? {
            [errorNode.v]: { value: asAny(error, getTokenDebugData(node.token)?.sourceCodeInfo) },
          }
        : {}
      return evaluateAstNode(catchExpression, contextStack.create(newContext))
    }
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    const tryResult = findUnresolvedSymbols(tryExpressions, contextStack, builtin)
    const newContext: Context = errorNode
      ? {
          [errorNode.v]: { value: true },
        }
      : {}
    const catchResult = findUnresolvedSymbols([catchExpression], contextStack.create(newContext), builtin)
    return joinAnalyzeResults(tryResult, catchResult)
  },
}
