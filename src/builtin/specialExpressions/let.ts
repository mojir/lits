import { Context } from '../../evaluator/interface'
import { BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, isLispishFunction } from '../../utils'
import { SpecialExpression } from '../interface'

interface LetSpecialExpressionNode extends SpecialExpressionNode {
  name: 'let'
  bindings: BindingNode[]
}

export const letSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseBinding, parseParams }) => {
    const { inputPosition } = asNotUndefined(tokens[position])
    const node: LetSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'let',
      params: [],
      bindings: [],
      inputPosition,
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
      const [newPosition, binding] = parseBinding(tokens, position)
      position = newPosition
      node.bindings.push(binding)
      token = asNotUndefined(tokens[position])
    }
    position += 1 // skip right parenthesis - end of let bindings
    const [newPosition, params] = parseParams(tokens, position)
    node.params = params
    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castLetExpressionNode(node)
    const locals: Context = { variables: {}, functions: {} }
    for (const binding of node.bindings) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      if (isLispishFunction(bindingValue)) {
        throw Error('Cannot bind function in let expression')
      }
      locals.variables[binding.name] = bindingValue
    }
    const newContextStack = [locals, ...contextStack]

    let result: unknown
    for (const astNode of node.params) {
      result = evaluateAstNode(astNode, newContextStack)
    }
    return result
  },
}

function castLetExpressionNode(_node: SpecialExpressionNode): asserts _node is LetSpecialExpressionNode {
  return
}
