import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import { asAstNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'
import { assertString } from '../../typeGuards/string'
import { assertNumberOfParams } from '../../typeGuards'

export const defsSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    const [newPosition, params] = parseTokens(tokenStream, position)
    return [
      newPosition + 1,
      {
        t: AstNodeType.SpecialExpression,
        n: 'defs',
        p: params,
        tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = node.tkn?.sourceCodeInfo
    const name = evaluateAstNode(asAstNode(node.p[0], sourceCodeInfo), contextStack)
    assertString(name, sourceCodeInfo)

    assertNameNotDefined(name, contextStack, builtin, node.tkn?.sourceCodeInfo)

    const value = evaluateAstNode(asAstNode(node.p[1], sourceCodeInfo), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertNumberOfParams(2, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const subNode = asAstNode(node.p[1], node.tkn?.sourceCodeInfo)
    return analyzeAst(subNode, contextStack, builtin)
  },
}
