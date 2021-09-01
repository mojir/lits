import { evaluateAstNode } from '../../evaluator'
import { Context } from '../../evaluator/interface'
import { NormalExpressionNode, SpecialExpressionNode } from '../../parser/interface'
import { parseExpression } from '../../parser/parsers'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface LetSpecialExpressionNode extends SpecialExpressionNode {
  name: 'let'
  bindings: NormalExpressionNode[]
}

export const letSpecialExpression: SpecialExpression = {
  parse: (tokens: Token[], position: number) => {
    const node: LetSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'let',
      params: [],
      bindings: [],
    }
    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected list of bindings`)
    }
    position += 1
    while (!(token.type === 'paren' && token.value === ')')) {
      if (!(token.type === 'paren' && token.value === '(')) {
        throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected an expression`)
      }
      const [newPosition, param] = parseExpression(tokens, position)
      if (param.type === 'SpecialExpression') {
        throw Error('Expected a binding expression')
      }
      position = newPosition
      node.bindings.push(param)
      token = asNotUndefined(tokens[position])
    }
    position += 1 // skip right parenthesis - end of let bindings
    return [position, node]
  },
  evaluate: (node: SpecialExpressionNode, contextStack: Context[]) => {
    assertLetExpressionNode(node)
    const locals: Context = {}
    for (const binding of node.bindings) {
      const bindingNode = binding.params[0]
      if (bindingNode === undefined) {
        throw Error(`binding node undefined`)
      }
      locals[binding.name] = evaluateAstNode(bindingNode, contextStack)
    }
    const newContextStack = [locals, ...contextStack]

    let result: unknown
    for (const astNode of node.params) {
      result = evaluateAstNode(astNode, newContextStack)
    }
    return result
  },
  validate: (node: SpecialExpressionNode) => {
    assertLetExpressionNode(node)
  },
}

function assertLetExpressionNode(node: SpecialExpressionNode): asserts node is LetSpecialExpressionNode {
  if (node.name !== 'let') {
    throw Error('Expected let special expression node')
  }
}
