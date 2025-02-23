import { joinAnalyzeResults } from '../../analyze/utils'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { assertLParenToken, assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { assertNameNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import { getSourceCodeInfo } from '../../utils/debug/getSourceCodeInfo'
import type { BuiltinSpecialExpression } from '../interface'

export interface TryNode extends CommonSpecialExpressionNode<'try'> {
  e: SymbolNode
  ce: AstNode
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  parse: (tokenStream, parseState, firstToken, { parseToken }) => {
    const tryExpression = parseToken(tokenStream, parseState)

    assertLParenToken(tokenStream.tokens[parseState.position++])

    const catchNode = parseToken(tokenStream, parseState)
    assertNameNode(catchNode, getTokenDebugData(catchNode.debugData?.token)?.sourceCodeInfo)
    if (catchNode.v !== 'catch') {
      throw new LitsError(
        `Expected 'catch', got '${catchNode.v}'.`,
        getSourceCodeInfo(catchNode, getTokenDebugData(catchNode.debugData?.token)?.sourceCodeInfo),
      )
    }

    const error = parseToken(tokenStream, parseState)
    assertNameNode(error, getTokenDebugData(error.debugData?.token)?.sourceCodeInfo)

    const catchExpression = parseToken(tokenStream, parseState)

    assertRParenToken(tokenStream.tokens[parseState.position++])

    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: TryNode = {
      t: AstNodeType.SpecialExpression,
      n: 'try',
      p: [tryExpression],
      ce: catchExpression,
      e: error,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
      },
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { p: tryExpressions, ce: catchExpression, e: errorNode } = node
    try {
      return evaluateAstNode(tryExpressions[0]!, contextStack)
    }
    catch (error) {
      const newContext: Context = {
        [errorNode.v]: { value: asAny(error, getTokenDebugData(node.debugData?.token)?.sourceCodeInfo) },
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
