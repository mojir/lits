import { AstNodeType } from '../../constants/constants'
import type { SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import { assertNameNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean> = {
  parse: (tokenStream, position, { parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    const [newPosition, params] = parseTokens(tokenStream, position)
    const node: SpecialExpressionNode = {
      t: AstNodeType.SpecialExpression,
      n: 'declared?',
      p: params,
      tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack) => {
    const [astNode] = node.p
    assertNameNode(astNode, node.tkn?.sourceCodeInfo)

    const lookUpResult = contextStack.lookUp(astNode)
    return lookUpResult !== null
  },
  validate: node => assertNumberOfParams(1, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.p, contextStack, builtin),
}
