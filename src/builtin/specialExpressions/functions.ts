import { UnexpectedNodeTypeError, UnexpectedTokenError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import {
  AstNode,
  BindingNode,
  EvaluatedFunctionArguments,
  FUNCTION_SYMBOL,
  LitsFunction,
  NameNode,
  ParseArgument,
  ParseBindings,
  SpecialExpressionNode,
} from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined, assertString } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined, FunctionArguments } from '../utils'

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

export interface FnSpecialExpressionNode extends SpecialExpressionNode {
  name: `fn`
  arguments: FunctionArguments
  body: AstNode[]
}

type FunctionNode = DefnSpecialExpressionNode | DefnsSpecialExpressionNode | FnSpecialExpressionNode

type ExpressionsName = `defn` | `defns` | `fn`

function createParser(expressionName: ExpressionsName): BuiltinSpecialExpression<FunctionNode>[`parse`] {
  return (tokens, position, { parseToken, parseArgument, parseBindings }) => {
    let functionName = undefined
    if (expressionName === `defn` || expressionName === `defns`) {
      ;[position, functionName] = parseToken(tokens, position)
      if (expressionName === `defn` && functionName.type !== `Name`) {
        throw new UnexpectedNodeTypeError(`Name`, functionName)
      }
    }

    const [nextPosition, functionArguments] = parseFunctionArguments(tokens, position, parseArgument, parseBindings)
    position = nextPosition

    let token = asNotUndefined(tokens[position])
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
          functionName: functionName as AstNode,
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
  node: FunctionNode,
  contextStack: ContextStack,
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

function createEvaluator(expressionName: ExpressionsName): BuiltinSpecialExpression<LitsFunction | null>[`evaluate`] {
  return (node, contextStack, { evaluateAstNode, builtin }) => {
    castExpressionNode(node)
    const name = getFunctionName(expressionName, node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin)

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

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
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
      return litsFunction
    }

    contextStack.globalContext[name as string] = { value: litsFunction }
    return null
  }
}

export const defnSpecialExpression: BuiltinSpecialExpression<LitsFunction | null> = {
  parse: createParser(`defn`),
  evaluate: createEvaluator(`defn`),
}

export const defnsSpecialExpression: BuiltinSpecialExpression<LitsFunction | null> = {
  parse: createParser(`defns`),
  evaluate: createEvaluator(`defns`),
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction | null> = {
  parse: createParser(`fn`),
  evaluate: createEvaluator(`fn`),
}

function castExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is DefnSpecialExpressionNode | DefnsSpecialExpressionNode | FnSpecialExpressionNode {
  return
}

function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parseArgument: ParseArgument,
  parseBindings: ParseBindings,
): [number, FunctionArguments] {
  let bindings: BindingNode[] = []
  let restArgument: string | undefined = undefined
  const mandatoryArguments: string[] = []
  const optionalArguments: Array<{
    name: string
    defaultValue?: AstNode
  }> = []
  const argNames: Record<string, true> = {}
  let state: `mandatory` | `optional` | `rest` | `let` = `mandatory`
  let token = asNotUndefined(tokens[position])
  if (!(token.type === `paren` && token.value === `[`)) {
    throw new UnexpectedTokenError(`[`, token)
  }

  position += 1
  token = asNotUndefined(tokens[position])
  while (!(token.type === `paren` && token.value === `]`)) {
    if (state === `let`) {
      ;[position, bindings] = parseBindings(tokens, position)
      break
    } else {
      const [newPosition, node] = parseArgument(tokens, position)
      position = newPosition
      token = asNotUndefined(tokens[position])

      if (node.type === `Modifier`) {
        switch (node.value) {
          case `&opt`:
            if (state === `rest`) {
              throw Error(`&opt cannot appear after &rest`)
            }
            if (state === `optional`) {
              throw Error(`&opt can only appear once`)
            }
            state = `optional`
            break
          case `&rest`:
            if (state === `rest`) {
              throw Error(`&rest can only appear once`)
            }
            if (state === `optional` && optionalArguments.length === 0) {
              throw Error(`No optional arguments where spcified`)
            }
            state = `rest`
            break
          case `&let`:
            if (state === `optional` && optionalArguments.length === 0) {
              throw Error(`No optional arguments where spcified`)
            }
            if (state === `rest` && !restArgument) {
              throw Error(`No rest argument was spcified`)
            }
            state = `let`
            break
          default:
            throw Error(`Illegal modifier: ${node.value}`)
        }
      } else {
        if (argNames[node.name]) {
          throw Error(`Duplicate argument "${node.name}"`)
        } else {
          argNames[node.name] = true
        }

        if (Object.getOwnPropertyDescriptor(node, `defaultValue`)) {
          if (state !== `optional`) {
            throw Error(`Cannot specify default value if not an optional argument`)
          }
          optionalArguments.push({
            name: node.name,
            defaultValue: node.defaultValue,
          })
        } else {
          switch (state) {
            case `mandatory`:
              mandatoryArguments.push(node.name)
              break
            case `optional`:
              optionalArguments.push({
                name: node.name,
                defaultValue: undefined,
              })
              break
            case `rest`:
              if (restArgument !== undefined) {
                throw Error(`Can only specify one rest argument`)
              }
              restArgument = node.name
              break
          }
        }
      }
    }
  }

  if (state === `rest` && restArgument === undefined) {
    throw Error(`Missing rest argument name`)
  }
  if (state === `optional` && optionalArguments.length === 0) {
    throw Error(`No optional arguments where spcified`)
  }

  position += 1

  const args: FunctionArguments = {
    mandatoryArguments,
    optionalArguments,
    restArgument,
    bindings,
  }

  return [position, args]
}
