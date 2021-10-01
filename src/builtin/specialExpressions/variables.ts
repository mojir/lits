import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode, assertString } from '../../utils'
import { SpecialExpression, SpecialExpressionName } from '../interface'

type SetqName = SpecialExpressionName & ('setq' | 'setq-constant' | 'setq-local' | 'setq-local-constant')
type CreateVariableName = 'create-variable' | 'create-constant-variable'

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

interface CreateVariableSpecialExpressionNode extends SpecialExpressionNode {
  name: 'create-variable'
}

interface CreateConstantVariableSpecialExpressionNode extends SpecialExpressionNode {
  name: 'create-constant-variable'
}

function createSetqParser(name: SetqName): SpecialExpression['parse'] {
  const parser: SpecialExpression['parse'] = (tokens, position, { parseParams }) => {
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
  return parser
}

function createVariableParser(name: CreateVariableName): SpecialExpression['parse'] {
  const parser: SpecialExpression['parse'] = (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name,
        params,
      },
    ]
  }
  return parser
}

function createSetqEvaluator(name: SetqName): SpecialExpression['evaluate'] {
  const constant = name === 'setq-constant' || name === 'setq-local-constant'
  const findContextType = name === 'setq-local' || name === 'setq-local-constant' ? 'local' : 'default'
  const evaluate: SpecialExpression['evaluate'] = (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const [context, exisitingVariable] = findContext(name, contextStack, findContextType)

    if (context.variables[name]?.constant) {
      throw Error(`Cannot change constant variable "${name}"`)
    }

    if (constant && exisitingVariable) {
      throw Error(`Cannot change a non constant variable to constant: "${name}"`)
    }

    context.variables[name] = { value, constant }

    return value
  }
  return evaluate
}

function createVariableEvaluator(name: CreateVariableName): SpecialExpression['evaluate'] {
  const constant = name === 'create-constant-variable'
  const evaluator: SpecialExpression['evaluate'] = (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = evaluateAstNode(asAstNode(node.params[0]), contextStack)
    assertString(name)
    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const [context, exisitingVariable] = findContext(name, contextStack, 'global')

    if (context.variables[name]?.constant) {
      throw Error(`Cannot change constant variable "${name}"`)
    }

    if (constant && exisitingVariable) {
      throw Error(`Cannot change a non constant variable to constant: "${name}"`)
    }

    context.variables[name] = { value, constant }

    return value
  }
  return evaluator
}

function findContext(name: string, contextStack: Context[], scope: 'default' | 'local' | 'global'): [Context, boolean] {
  // The second last stack entry is the "global" scope
  let exisitingVariable = false
  let context: Context | undefined = undefined
  if (scope === 'local') {
    context = asNotUndefined(contextStack[0])
    exisitingVariable = !!context.variables[name]
  } else if (scope === 'global') {
    context = asNotUndefined(contextStack[contextStack.length - 2])
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

export const setqSpecialExpression: SpecialExpression = {
  parse: createSetqParser('setq'),
  evaluate: createSetqEvaluator('setq'),
  validate: node => assertLength(2, node),
}

export const setqConstantSpecialExpression: SpecialExpression = {
  parse: createSetqParser('setq-constant'),
  evaluate: createSetqEvaluator('setq-constant'),
  validate: node => assertLength(2, node),
}

export const setqLocalSpecialExpression: SpecialExpression = {
  parse: createSetqParser('setq-local'),
  evaluate: createSetqEvaluator('setq-local'),
  validate: node => assertLength(2, node),
}

export const setqLocalConstantSpecialExpression: SpecialExpression = {
  parse: createSetqParser('setq-local-constant'),
  evaluate: createSetqEvaluator('setq-local-constant'),
  validate: node => assertLength(2, node),
}

export const createVariableSpecialExpression: SpecialExpression = {
  parse: createVariableParser('create-variable'),
  evaluate: createVariableEvaluator('create-variable'),
  validate: node => assertLength(2, node),
}

export const createConstantVariableSpecialExpression: SpecialExpression = {
  parse: createVariableParser('create-constant-variable'),
  evaluate: createVariableEvaluator('create-constant-variable'),
  validate: node => assertLength(2, node),
}

function castSetqExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is
  | SetqSpecialExpressionNode
  | SetqConstantSpecialExpressionNode
  | SetqLocalSpecialExpressionNode
  | SetqLocalConstantSpecialExpressionNode
  | CreateVariableSpecialExpressionNode
  | CreateConstantVariableSpecialExpressionNode {
  return
}
