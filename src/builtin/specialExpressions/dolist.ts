import { ReturnSignal, UnexpectedTokenError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { AstNode, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertArray, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

interface DolistSpecialExpressionNode extends SpecialExpressionNode {
  name: 'dolist'
  varName: NameNode
  result: AstNode | undefined
  list: AstNode
}

export const dolistSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw new UnexpectedTokenError('(', token)
    }
    position += 1

    let varName: AstNode
    ;[position, varName] = parseToken(tokens, position)
    assertNameNode(varName)

    let list: AstNode
    ;[position, list] = parseToken(tokens, position)

    token = asNotUndefined(tokens[position])
    let result = undefined
    if (!(token.type === 'paren' && token.value === ')')) {
      ;[position, result] = parseToken(tokens, position)
    }

    const node: DolistSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'dolist',
      params: [],
      varName,
      result,
      list,
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
    castDolistExpressionNode(node)
    const list = evaluateAstNode(node.list, contextStack)
    assertArray(list)

    const varName = node.varName.value

    const newContext: Context = {
      functions: {},
      variables: { [varName]: { value: undefined, constant: false } },
    }

    const newContextStack = [newContext, ...contextStack]

    try {
      while (list.length > 0) {
        const variable = list.shift()
        newContext.variables[varName] = { value: variable, constant: false }
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
    newContext.variables[varName] = { value: undefined, constant: false }
    return evaluateAstNode(node.result, newContextStack)
  },
}

function castDolistExpressionNode(_node: SpecialExpressionNode): asserts _node is DolistSpecialExpressionNode {
  return
}
