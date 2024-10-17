import type { Any } from '../../interface'
import { AstNodeType } from '../../constants/constants'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode, asNameNode, assertNameNode } from '../../typeGuards/astNode'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export const defSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    const [newPosition, params] = parseTokens(tokenStream, position)
    assertNameNode(params[0], firstToken.sourceCodeInfo)
    return [
      newPosition + 1,
      {
        t: AstNodeType.SpecialExpression,
        n: 'def',
        p: params,
        tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = node.tkn?.sourceCodeInfo
    const name = asNameNode(node.p[0], sourceCodeInfo).v

    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)

    const value = evaluateAstNode(asAstNode(node.p[1], sourceCodeInfo), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertNumberOfParams(2, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const sourceCodeInfo = node.tkn?.sourceCodeInfo
    const subNode = asAstNode(node.p[1], sourceCodeInfo)
    const result = analyzeAst(subNode, contextStack, builtin)
    const name = asNameNode(node.p[0], sourceCodeInfo).v
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    return result
  },
}
