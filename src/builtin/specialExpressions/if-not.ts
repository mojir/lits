import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export const ifNotSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    const [newPosition, params] = parseTokens(tokenStream, position)
    return [
      newPosition + 1,
      {
        t: AstNodeType.SpecialExpression,
        n: 'if-not',
        p: params,
        tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.tkn?.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.p
    if (!evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack)
    }
    else {
      if (node.p.length === 3)
        return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack)
      else
        return null
    }
  },
  validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.p, contextStack, builtin),
}
