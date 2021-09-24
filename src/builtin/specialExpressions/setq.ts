import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

type SetqName = 'setq' | 'setq-constant' | 'setq-local' | 'setq-local-constant'

interface SetqSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq'
}
interface SetqConstantSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq-constant'
}
interface SetqLocalSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq-local'
}
interface SetqLocalConstantSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq-local-constant'
}

function createParser(name: SetqName): SpecialExpression['parse'] {
  const parseSetq: SpecialExpression['parse'] = (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name,
        params,
      },
    ]
  }
  return parseSetq
}

function findContext(name: string, contextStack: Context[], local: boolean): [Context, boolean] {
  // The second last stack entry is the "global" scope
  let exisitingVariable = false
  let context: Context | undefined = undefined
  if (local) {
    context = asNotUndefined(contextStack[0])
    exisitingVariable = !!context.variables[name]
  } else {
    for (let i = 0; i < contextStack.length - 1; i += 1) {
      if (asNotUndefined(contextStack[i]).variables[name]) {
        exisitingVariable = true
        context = contextStack[i]
        break
      }
    }

    if (!context) {
      // The second last stack entry is the "global" scope
      context = asNotUndefined(contextStack[contextStack.length - 2], 'This cannot be')
    }
  }
  return [context, exisitingVariable]
}

function createEvaluator(name: SetqName): SpecialExpression['evaluate'] {
  const constant = name === 'setq-constant' || name === 'setq-local-constant'
  const local = name === 'setq-local' || name === 'setq-local-constant'
  const evaluateSetq: SpecialExpression['evaluate'] = (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const [context, exisitingVariable] = findContext(name, contextStack, local)

    if (context.variables[name]?.constant) {
      throw Error(`Cannot change constant variable "${name}"`)
    }

    if (constant && exisitingVariable) {
      throw Error(`Cannot change a non constant variable to constant: "${name}"`)
    }

    context.variables[name] = { value, constant }

    return value
  }
  return evaluateSetq
}

export const setqSpecialExpression: SpecialExpression = {
  parse: createParser('setq'),
  evaluate: createEvaluator('setq'),
  validate: node => assertLength(2, node),
}

export const setqConstantSpecialExpression: SpecialExpression = {
  parse: createParser('setq-constant'),
  evaluate: createEvaluator('setq-constant'),
  validate: node => assertLength(2, node),
}

export const setqLocalSpecialExpression: SpecialExpression = {
  parse: createParser('setq-local'),
  evaluate: createEvaluator('setq-local'),
  validate: node => assertLength(2, node),
}

export const setqLocalConstantSpecialExpression: SpecialExpression = {
  parse: createParser('setq-local-constant'),
  evaluate: createEvaluator('setq-local-constant'),
  validate: node => assertLength(2, node),
}

function castSetqExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is
  | SetqSpecialExpressionNode
  | SetqConstantSpecialExpressionNode
  | SetqLocalSpecialExpressionNode
  | SetqLocalConstantSpecialExpressionNode {
  return
}
