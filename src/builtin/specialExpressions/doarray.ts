import { ReturnSignal, UnexpectedTokenError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { AstNode, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertArray, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

interface DoarraySpecialExpressionNode extends SpecialExpressionNode {
  name: `doarray`
  varName: NameNode
  result: AstNode | undefined
  array: AstNode
}

export const doarraySpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (!(token.type === `paren` && token.value === `(`)) {
      throw new UnexpectedTokenError(`(`, token)
    }
    position += 1

    let varName: AstNode
    ;[position, varName] = parseToken(tokens, position)
    assertNameNode(varName)

    let array: AstNode
    ;[position, array] = parseToken(tokens, position)

    token = asNotUndefined(tokens[position])
    let result = undefined
    if (!(token.type === `paren` && token.value === `)`)) {
      ;[position, result] = parseToken(tokens, position)
    }

    const node: DoarraySpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `doarray`,
      params: [],
      varName,
      result,
      array,
    }

    position += 1
    token = asNotUndefined(tokens[position])
    while (!(token.type === `paren` && token.value === `)`)) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      token = asNotUndefined(tokens[position])
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castDoarrayExpressionNode(node)
    const array = evaluateAstNode(node.array, contextStack)
    assertArray(array)

    const varName = node.varName.value

    const newContext: Context = { [varName]: { value: undefined } }

    const newContextStack = [newContext, ...contextStack]

    try {
      while (array.length > 0) {
        const variable = array.shift()
        newContext[varName] = { value: variable }
        for (const form of node.params) {
          evaluateAstNode(form, newContextStack)
        }
      }
    } catch (error) {
      if (error instanceof ReturnSignal) {
        return error.value
      }
      throw error
    }
    if (!node.result) {
      return undefined
    }
    newContext[varName] = { value: undefined }
    return evaluateAstNode(node.result, newContextStack)
  },
}

function castDoarrayExpressionNode(_node: SpecialExpressionNode): asserts _node is DoarraySpecialExpressionNode {
  return
}
