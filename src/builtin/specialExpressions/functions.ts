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

interface DefunSpecialExpressionNode extends SpecialExpressionNode {
  name: 'defun'
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

interface CreateFunctionSpecialExpressionNode extends SpecialExpressionNode {
  name: 'create-function'
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

interface LambdaSpecialExpressionNode extends SpecialExpressionNode {
  name: 'lambda'
  arguments: FunctionArguments
  body: AstNode[]
}

type ExpressionNode = DefunSpecialExpressionNode | CreateFunctionSpecialExpressionNode | LambdaSpecialExpressionNode
type ExpressionsName = 'defun' | 'create-function' | 'lambda'

function createParser(expressionName: ExpressionsName): SpecialExpression['parse'] {
  return (tokens, position, { parseToken, parseArgument, parseBinding }) => {
    let functionName = undefined
    if (expressionName === 'defun' || expressionName === 'create-function') {
      ;[position, functionName] = parseToken(tokens, position)
      if (expressionName === 'defun' && functionName.type !== 'Name') {
        throw new UnexpectedNodeTypeError('Name', functionName)
      }
    }

    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw new UnexpectedTokenError(')', token)
    }

    position += 1

    const [nextPosition, functionArguments] = parseFunctionArguments(tokens, position, parseArgument, parseBinding)
    position = nextPosition

    token = asNotUndefined(tokens[position])
    const body: AstNode[] = []
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      body.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    if (body.length === 0) {
      throw Error('Missing body in special expression "defun"')
    }

    position += 1

    if (expressionName === 'defun' || expressionName === 'create-function') {
      return [
        position,
        {
          type: 'SpecialExpression',
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
        type: 'SpecialExpression',
        name: expressionName,
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
  if (expressionName === 'defun') {
    const name = ((node as DefunSpecialExpressionNode).functionName as NameNode).value
    assertString(name)
    return name
  }
  if (expressionName === 'create-function') {
    const name = evaluateAstNode((node as CreateFunctionSpecialExpressionNode).functionName, contextStack)
    assertString(name)
    return name
  }
  return undefined
}

function createEvaluator(expressionName: ExpressionsName): SpecialExpression['evaluate'] {
  return (node, contextStack, evaluateAstNode): LispishFunction | undefined => {
    castExpressionNode(node)
    const name = getFunctionName(expressionName, node, contextStack, evaluateAstNode)

    const functionContext: Context = { variables: {}, functions: {} }
    for (const binding of node.arguments.bindings) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      functionContext.variables[binding.name] = { value: bindingValue, constant: false }
    }

    const optionalArguments: EvaluatedFunctionArguments['optionalArguments'] = node.arguments.optionalArguments.map(
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

    if (expressionName === 'lambda') {
      return lispishFunction
    }

    // The second last stack entry is the "global" scope
    const context = asNotUndefined(contextStack[contextStack.length - 2], 'Could not find global scope')

    context.functions[name as string] = { fun: lispishFunction, constant: false }
    return undefined
  }
}

export const defunSpecialExpression: SpecialExpression = {
  parse: createParser('defun'),
  evaluate: createEvaluator('defun'),
}

export const createFunctionSpecialExpression: SpecialExpression = {
  parse: createParser('create-function'),
  evaluate: createEvaluator('create-function'),
}

export const lambdaSpecialExpression: SpecialExpression = {
  parse: createParser('lambda'),
  evaluate: createEvaluator('lambda'),
}

function castExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is DefunSpecialExpressionNode | CreateFunctionSpecialExpressionNode | LambdaSpecialExpressionNode {
  return
}
