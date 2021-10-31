import { UnexpectedNodeTypeError, UnexpectedTokenError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import {
  AstNode,
  BindingNode,
  EvaluatedFunctionOverload,
  FUNCTION_SYMBOL,
  LitsFunction,
  NameNode,
  SpecialExpressionNode,
} from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined, assertString } from '../../utils'
import { BuiltinSpecialExpression, Parsers } from '../interface'
import { Arity, assertNameNotDefined, FunctionArguments, FunctionOverload } from '../utils'

interface DefnSpecialExpressionNode extends SpecialExpressionNode {
  name: `defn`
  functionName: AstNode
  overloads: FunctionOverload[]
}

interface DefnsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defns`
  functionName: AstNode
  overloads: FunctionOverload[]
}

export interface FnSpecialExpressionNode extends SpecialExpressionNode {
  name: `fn`
  overloads: FunctionOverload[]
}

type FunctionNode = DefnSpecialExpressionNode | DefnsSpecialExpressionNode | FnSpecialExpressionNode

type ExpressionsName = `defn` | `defns` | `fn`

function createParser(expressionName: ExpressionsName): BuiltinSpecialExpression<FunctionNode>[`parse`] {
  return (tokens, position, parsers) => {
    const firstToken = asNotUndefined(tokens[position])
    const { parseToken } = parsers
    let functionName = undefined
    if (expressionName === `defn` || expressionName === `defns`) {
      ;[position, functionName] = parseToken(tokens, position)
      if (expressionName === `defn` && functionName.type !== `Name`) {
        throw new UnexpectedNodeTypeError(`Name`, functionName)
      }
    }

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokens, position, parsers)

    if (expressionName === `defn` || expressionName === `defns`) {
      return [
        position,
        {
          type: `SpecialExpression`,
          name: expressionName,
          functionName: functionName as AstNode,
          params: [],
          overloads: functionOverloades,
          token: firstToken,
        },
      ]
    }

    return [
      position,
      {
        type: `SpecialExpression`,
        name: expressionName,
        params: [],
        overloads: functionOverloades,
        token: firstToken,
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

    const evaluatedFunctionOverloades: EvaluatedFunctionOverload[] = []
    for (const functionOverload of node.overloads) {
      const functionContext: Context = {}
      for (const binding of functionOverload.arguments.bindings) {
        const bindingValueNode = binding.value
        const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
        functionContext[binding.name] = { value: bindingValue }
      }

      const evaluatedFunctionOverload: EvaluatedFunctionOverload = {
        arguments: {
          mandatoryArguments: functionOverload.arguments.mandatoryArguments,
          restArgument: functionOverload.arguments.restArgument,
        },
        arity: functionOverload.arity,
        body: functionOverload.body,
        functionContext,
      }

      evaluatedFunctionOverloades.push(evaluatedFunctionOverload)
    }

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
      name,
      overloads: evaluatedFunctionOverloades,
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

function arityOk(overloadedFunctions: FunctionOverload[], arity: Arity) {
  if (typeof arity === `number`) {
    return overloadedFunctions.every(fun => {
      if (typeof fun.arity === `number`) {
        return fun.arity !== arity
      }
      return fun.arity.min > arity
    })
  }
  return overloadedFunctions.every(fun => {
    if (typeof fun.arity === `number`) {
      return fun.arity < arity.min
    }
    return false
  })
}

function parseFunctionBody(tokens: Token[], position: number, { parseToken }: Parsers): [number, AstNode[]] {
  let token = asNotUndefined(tokens[position])
  const body: AstNode[] = []
  while (!(token.type === `paren` && (token.value === `)` || token.value === `]`))) {
    const [newPosition, bodyNode] = parseToken(tokens, position)
    body.push(bodyNode)
    position = newPosition
    token = asNotUndefined(tokens[position])
  }
  if (body.length === 0) {
    throw Error(`Missing body in function`)
  }
  position += 1
  return [position, body]
}

function parseFunctionOverloades(tokens: Token[], position: number, parsers: Parsers): [number, FunctionOverload[]] {
  let token = asNotUndefined(tokens[position])
  if (token.type === `paren` && token.value === `(`) {
    const functionOverloades: FunctionOverload[] = []
    while (!(token.type === `paren` && token.value === `)`)) {
      position += 1
      token = asNotUndefined(tokens[position])
      let functionArguments: FunctionArguments
      ;[position, functionArguments] = parseFunctionArguments(tokens, position, parsers)
      const arity: Arity = functionArguments.restArgument
        ? { min: functionArguments.mandatoryArguments.length }
        : functionArguments.mandatoryArguments.length

      if (!arityOk(functionOverloades, arity)) {
        throw Error(`All overloaded functions must have different arity`)
      }

      let functionBody: AstNode[]
      ;[position, functionBody] = parseFunctionBody(tokens, position, parsers)
      functionOverloades.push({
        arguments: functionArguments,
        body: functionBody,
        arity,
      })
      token = asNotUndefined(tokens[position])
    }

    return [position + 1, functionOverloades]
  } else if (token.type === `paren` && token.value === `[`) {
    let functionArguments: FunctionArguments
    ;[position, functionArguments] = parseFunctionArguments(tokens, position, parsers)
    const arity: Arity = functionArguments.restArgument
      ? { min: functionArguments.mandatoryArguments.length }
      : functionArguments.mandatoryArguments.length
    let functionBody: AstNode[]
    ;[position, functionBody] = parseFunctionBody(tokens, position, parsers)
    return [
      position,
      [
        {
          arguments: functionArguments,
          body: functionBody,
          arity,
        },
      ],
    ]
  } else {
    throw new UnexpectedTokenError(`[ or (`, token)
  }
}

function parseFunctionArguments(tokens: Token[], position: number, parsers: Parsers): [number, FunctionArguments] {
  const { parseArgument, parseBindings } = parsers

  let bindings: BindingNode[] = []
  let restArgument: string | undefined = undefined
  const mandatoryArguments: string[] = []
  const argNames: Record<string, true> = {}
  let state: `mandatory` | `rest` | `let` = `mandatory`
  let token = asNotUndefined(tokens[position])

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
          case `&`:
            if (state === `rest`) {
              throw Error(`& can only appear once`)
            }
            state = `rest`
            break
          case `&let`:
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
        switch (state) {
          case `mandatory`:
            mandatoryArguments.push(node.name)
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

  if (state === `rest` && restArgument === undefined) {
    throw Error(`Missing rest argument name`)
  }

  position += 1

  const args: FunctionArguments = {
    mandatoryArguments,
    restArgument,
    bindings,
  }

  return [position, args]
}
