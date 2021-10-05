import { builtin } from '..'
import { UnexpectedNodeTypeError, UnexpectedTokenError } from '../../errors'
import { Context, EvaluateAstNode } from '../../evaluator/interface'
import {
  AstNode,
  EvaluatedFunctionArguments,
  functionSymbol,
  LispishFunction,
  NameNode,
  SpecialExpressionNode,
} from '../../parser/interface'
import { asNotUndefined, assertString } from '../../utils'
import { SpecialExpression } from '../interface'
import { FunctionArguments, parseFunctionArguments } from '../utils'

interface DefnSpecialExpressionNode extends SpecialExpressionNode {
  name: `defn`
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

interface DefnsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defns`
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

interface FnSpecialExpressionNode extends SpecialExpressionNode {
  name: `fn`
  arguments: FunctionArguments
  body: AstNode[]
}

type ExpressionNode = DefnSpecialExpressionNode | DefnsSpecialExpressionNode | FnSpecialExpressionNode
type ExpressionsName = `defn` | `defns` | `fn`

function createParser(expressionName: ExpressionsName): SpecialExpression[`parse`] {
  return (tokens, position, { parseToken, parseArgument, parseBinding }) => {
    let functionName = undefined
    if (expressionName === `defn` || expressionName === `defns`) {
      ;[position, functionName] = parseToken(tokens, position)
      if (expressionName === `defn` && functionName.type !== `Name`) {
        throw new UnexpectedNodeTypeError(`Name`, functionName)
      }
    }

    let token = asNotUndefined(tokens[position])
    if (!(token.type === `paren` && token.value === `(`)) {
      throw new UnexpectedTokenError(`)`, token)
    }

    position += 1

    const [nextPosition, functionArguments] = parseFunctionArguments(tokens, position, parseArgument, parseBinding)
    position = nextPosition

    token = asNotUndefined(tokens[position])
    const body: AstNode[] = []
    while (!(token.type === `paren` && token.value === `)`)) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      body.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    if (body.length === 0) {
      throw Error(`Missing body in special expression "defn"`)
    }

    position += 1

    if (expressionName === `defn` || expressionName === `defns`) {
      return [
        position,
        {
          type: `SpecialExpression`,
          name: expressionName,
          functionName,
          params: [],
          arguments: functionArguments,
          body,
        },
      ]
    }

    return [
      position,
      {
        type: `SpecialExpression`,
        name: `fn`,
        params: [],
        arguments: functionArguments,
        body,
      },
    ]
  }
}

function getFunctionName(
  expressionName: ExpressionsName,
  node: ExpressionNode,
  contextStack: Context[],
  evaluateAstNode: EvaluateAstNode,
): string | undefined {
  if (expressionName === `defn`) {
    const name = ((node as DefnSpecialExpressionNode).functionName as NameNode).value
    assertString(name)
    return name
  }
  if (expressionName === `defns`) {
    const name = evaluateAstNode((node as DefnsSpecialExpressionNode).functionName, contextStack)
    assertString(name)
    return name
  }
  return undefined
}

function createEvaluator(expressionName: ExpressionsName): SpecialExpression[`evaluate`] {
  return (node, contextStack, evaluateAstNode): LispishFunction | undefined => {
    castExpressionNode(node)
    const name = getFunctionName(expressionName, node, contextStack, evaluateAstNode)

    if (name && builtin.specialExpressions[name]) {
      throw Error(`Cannot define function ${name}, it's a special expression`)
    }

    if (name && builtin.normalExpressions[name]) {
      throw Error(`Cannot define function ${name}, it's a builtin function`)
    }

    const functionContext: Context = {}
    for (const binding of node.arguments.bindings) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      functionContext[binding.name] = { value: bindingValue }
    }

    const optionalArguments: EvaluatedFunctionArguments[`optionalArguments`] = node.arguments.optionalArguments.map(
      optArg => {
        const name = optArg.name
        const defaultValue = optArg.defaultValue
        if (defaultValue) {
          return {
            name,
            defaultValue: evaluateAstNode(defaultValue, contextStack),
          }
        }
        return { name }
      },
    )

    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name,
      arguments: {
        mandatoryArguments: node.arguments.mandatoryArguments,
        restArgument: node.arguments.restArgument,
        optionalArguments,
      },
      body: node.body,
      functionContext,
    }

    if (expressionName === `fn`) {
      return lispishFunction
    }

    // The second last stack entry is the "global" scope
    const context = asNotUndefined(contextStack[contextStack.length - 2], `Could not find global scope`)

    context[name as string] = { value: lispishFunction }
    return undefined
  }
}

export const defnSpecialExpression: SpecialExpression = {
  parse: createParser(`defn`),
  evaluate: createEvaluator(`defn`),
}

export const defnsSpecialExpression: SpecialExpression = {
  parse: createParser(`defns`),
  evaluate: createEvaluator(`defns`),
}

export const fnSpecialExpression: SpecialExpression = {
  parse: createParser(`fn`),
  evaluate: createEvaluator(`fn`),
}

function castExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is DefnSpecialExpressionNode | DefnsSpecialExpressionNode | FnSpecialExpressionNode {
  return
}
