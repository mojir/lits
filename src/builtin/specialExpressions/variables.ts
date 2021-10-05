import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode, assertString } from '../../utils'
import { SpecialExpression } from '../interface'

type ScopeName = `local` | `global`
type CreatorName = `create-variable` | `create-global-variable`

interface DefSpecialExpressionNode extends SpecialExpressionNode {
  name: `def`
}

interface CreateVariableSpecialExpressionNode extends SpecialExpressionNode {
  name: `create-variable`
}

interface CreateGlobalVariableSpecialExpressionNode extends SpecialExpressionNode {
  name: `create-global-variable`
}

function createDefParser(): SpecialExpression[`parse`] {
  const parser: SpecialExpression[`parse`] = (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `def`,
        params,
      },
    ]
  }
  return parser
}

function createCreatorParser(name: CreatorName): SpecialExpression[`parse`] {
  const parser: SpecialExpression[`parse`] = (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name,
        params,
      },
    ]
  }
  return parser
}

function createDefEvaluator(): SpecialExpression[`evaluate`] {
  const evaluate: SpecialExpression[`evaluate`] = (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const context = asNotUndefined(contextStack[0])

    if (context[name]) {
      throw Error(`Variable already exists "${name}"`)
    }

    context[name] = { value }

    return value
  }
  return evaluate
}

function createCreatorEvaluator(name: CreatorName): SpecialExpression[`evaluate`] {
  const scopeName: ScopeName = name === `create-global-variable` ? `global` : `local`

  const evaluator: SpecialExpression[`evaluate`] = (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = evaluateAstNode(asAstNode(node.params[0]), contextStack)
    assertString(name)
    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const context = findContext(contextStack, scopeName)

    if (context[name]) {
      throw Error(`Variable already exists "${name}"`)
    }

    context[name] = { value }

    return value
  }
  return evaluator
}

function findContext(contextStack: Context[], scope: ScopeName): Context {
  if (scope === `local`) {
    return asNotUndefined(contextStack[0])
  }
  return asNotUndefined(contextStack[contextStack.length - 2])
}

export const defSpecialExpression: SpecialExpression = {
  parse: createDefParser(),
  evaluate: createDefEvaluator(),
  validate: node => assertLength(2, node),
}

export const createVariableSpecialExpression: SpecialExpression = {
  parse: createCreatorParser(`create-variable`),
  evaluate: createCreatorEvaluator(`create-variable`),
  validate: node => assertLength(2, node),
}

export const createGlobalVariableSpecialExpression: SpecialExpression = {
  parse: createCreatorParser(`create-global-variable`),
  evaluate: createCreatorEvaluator(`create-global-variable`),
  validate: node => assertLength(2, node),
}

function castSetqExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is
  | DefSpecialExpressionNode
  | CreateVariableSpecialExpressionNode
  | CreateGlobalVariableSpecialExpressionNode {
  return
}
