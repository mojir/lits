import type { Any } from '../../interface'
import { AstNodeType, TokenType } from '../../constants/constants'
import type { AstNode, ParseToken, SpecialExpressionNode } from '../../parser/interface'
import type { TokenStream } from '../../tokenizer/interface'
import { asToken, isToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface Condition {
  t: AstNode // test
  f: AstNode // form
}

type CondNode = SpecialExpressionNode & {
  c: Condition[]
}

function parseConditions(tokenStream: TokenStream, position: number, parseToken: ParseToken): [number, Condition[]] {
  const conditions: Condition[] = []

  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
    let test: AstNode
    ;[position, test] = parseToken(tokenStream, position)

    let form: AstNode
    ;[position, form] = parseToken(tokenStream, position)

    conditions.push({ t: test, f: form })

    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }
  return [position, conditions]
}

export const condSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokenStream, position, { parseToken }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    let conditions: Condition[]
    ;[position, conditions] = parseConditions(tokenStream, position, parseToken)

    return [
      position + 1,
      {
        t: AstNodeType.SpecialExpression,
        n: 'cond',
        c: conditions,
        p: [],
        tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const condition of (node as CondNode).c) {
      const value = evaluateAstNode(condition.t, contextStack)
      if (!value)
        continue

      return evaluateAstNode(condition.f, contextStack)
    }
    return null
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const astNodes = (node as CondNode).c.flatMap(condition => [condition.t, condition.f])
    return analyzeAst(astNodes, contextStack, builtin)
  },
}
