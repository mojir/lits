import { ReturnSignal } from '../../errors'
import { Context } from '../../evaluator/interface'
import { AstNode, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertInteger, assertNameNode, assertNonNegativeNumber } from '../../utils'
import { SpecialExpression } from '../interface'

interface DotimesSpecialExpressionNode extends SpecialExpressionNode {
  name: 'dotimes'
  varName: NameNode
  result: AstNode | undefined
  count: AstNode
}

export const dotimesSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw Error(`Expected "(", got ${token.type}: ${token.value}`)
    }
    position += 1

    let varName: AstNode
    ;[position, varName] = parseToken(tokens, position)
    assertNameNode(varName)

    let count: AstNode
    ;[position, count] = parseToken(tokens, position)

    token = asNotUndefined(tokens[position])
    let result = undefined
    if (!(token.type === 'paren' && token.value === ')')) {
      ;[position, result] = parseToken(tokens, position)
    }

    const node: DotimesSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'dotimes',
      params: [],
      varName,
      result,
      count,
    }

    position += 1
    token = asNotUndefined(tokens[position])
    while (!(token.type === 'paren' && token.value === ')')) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      token = asNotUndefined(tokens[position])
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castDotimesExpressionNode(node)
    const count = evaluateAstNode(node.count, contextStack)
    assertInteger(count)
    assertNonNegativeNumber(count)

    const varName = node.varName.value

    const newContext: Context = {
      functions: {},
      variables: { [varName]: { value: undefined, constant: false } },
    }

    const newContextStack = [newContext, ...contextStack]

    try {
      let i
      for (i = 0; i < count; i += 1) {
        newContext.variables[varName] = { value: i, constant: false }
        for (const form of node.params) {
          evaluateAstNode(form, newContextStack)
        }
      }
      if (!node.result) {
        return undefined
      }
      newContext.variables[varName] = { value: i, constant: false }
      return evaluateAstNode(node.result, newContextStack)
    } catch (error) {
      if (error instanceof ReturnSignal) {
        return error.value
      }
      throw error
    }
  },
}

function castDotimesExpressionNode(_node: SpecialExpressionNode): asserts _node is DotimesSpecialExpressionNode {
  return
}
