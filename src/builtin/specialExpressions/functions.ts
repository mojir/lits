import { AnalyzeAst, AnalyzeResult } from '../../analyze/interface'
import { addAnalyzeResults } from '../../analyze/utils'
import { LitsError } from '../../errors'
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
import { nameNode, string, token } from '../../utils/assertion'
import { valueToString } from '../../utils/helpers'
import { Builtin, BuiltinSpecialExpression, ParserHelpers } from '../interface'
import { Arity, assertNameNotDefined, FunctionArguments, FunctionOverload } from '../utils'

type DefnNode = SpecialExpressionNode & {
  functionName: NameNode
  overloads: FunctionOverload[]
}

type DefnsNode = SpecialExpressionNode & {
  functionName: AstNode
  overloads: FunctionOverload[]
}

export type FnNode = SpecialExpressionNode & {
  overloads: FunctionOverload[]
}

export const defnSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, parsers) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const { parseToken } = parsers
    let functionName = undefined
    ;[position, functionName] = parseToken(tokens, position)
    nameNode.assert(functionName, functionName.token?.debugInfo)

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokens, position, parsers)

    return [
      position,
      {
        type: `SpecialExpression`,
        name: `defn`,
        functionName,
        params: [],
        overloads: functionOverloades,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName(`defn`, node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, node.token?.debugInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: node.token?.debugInfo,
      type: `user-defined`,
      name,
      overloads: evaluatedFunctionOverloades,
    }

    contextStack.globalContext[name as string] = { value: litsFunction }
    return null
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    contextStack.globalContext[(node as DefnNode).functionName.value] = { value: true }
    const newContext: Context = { [(node as DefnNode).functionName.value]: { value: true } }
    return addOverloadsUndefinedSymbols((node as DefnNode).overloads, contextStack, analyzeAst, builtin, newContext)
  },
}

export const defnsSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, parsers) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const { parseToken } = parsers
    let functionName: AstNode
    ;[position, functionName] = parseToken(tokens, position)

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokens, position, parsers)

    return [
      position,
      {
        type: `SpecialExpression`,
        name: `defns`,
        functionName,
        params: [],
        overloads: functionOverloades,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName(`defns`, node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, node.token?.debugInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: node.token?.debugInfo,
      type: `user-defined`,
      name,
      overloads: evaluatedFunctionOverloades,
    }

    contextStack.globalContext[name as string] = { value: litsFunction }
    return null
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) =>
    addOverloadsUndefinedSymbols((node as DefnsNode).overloads, contextStack, analyzeAst, builtin),
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction> = {
  parse: (tokens, position, parsers) => {
    const firstToken = token.as(tokens[position], `EOF`)

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokens, position, parsers)

    return [
      position,
      {
        type: `SpecialExpression`,
        name: `fn`,
        params: [],
        overloads: functionOverloades,
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: node.token?.debugInfo,
      type: `user-defined`,
      name: undefined,
      overloads: evaluatedFunctionOverloades,
    }

    return litsFunction
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) =>
    addOverloadsUndefinedSymbols((node as FnNode).overloads, contextStack, analyzeAst, builtin),
}

function getFunctionName(
  expressionName: `defn` | `defns`,
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): string {
  const debugInfo = node.token?.debugInfo
  if (expressionName === `defn`) {
    return ((node as DefnNode).functionName as NameNode).value
  }

  const name = evaluateAstNode((node as DefnsNode).functionName, contextStack)
  string.assert(name, debugInfo)
  return name
}

function evaluateFunctionOverloades(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunctionOverload[] {
  const evaluatedFunctionOverloades: EvaluatedFunctionOverload[] = []
  for (const functionOverload of (node as DefnNode | DefnsNode | FnNode).overloads) {
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
  return evaluatedFunctionOverloades
}

function addOverloadsUndefinedSymbols(
  overloads: FunctionOverload[],
  contextStack: ContextStack,
  analyzeAst: AnalyzeAst,
  builtin: Builtin,
  functionNameContext?: Context,
): AnalyzeResult {
  const result: AnalyzeResult = { undefinedSymbols: new Set() }
  const contextStackWithFunctionName = functionNameContext
    ? contextStack.withContext(functionNameContext)
    : contextStack
  for (const overload of overloads) {
    const newContext: Context = {}
    overload.arguments.bindings.forEach(binding => {
      const bindingResult = analyzeAst(binding.value, contextStack, builtin)
      addAnalyzeResults(result, bindingResult)
      newContext[binding.name] = { value: true }
    })
    overload.arguments.mandatoryArguments.forEach(arg => {
      newContext[arg] = { value: true }
    })
    if (typeof overload.arguments.restArgument === `string`) {
      newContext[overload.arguments.restArgument] = { value: true }
    }
    const newContextStack = contextStackWithFunctionName.withContext(newContext)
    const overloadResult = analyzeAst(overload.body, newContextStack, builtin)
    addAnalyzeResults(result, overloadResult)
  }
  return result
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

function parseFunctionBody(tokens: Token[], position: number, { parseToken }: ParserHelpers): [number, AstNode[]] {
  let tkn = token.as(tokens[position], `EOF`)
  const body: AstNode[] = []
  while (!(tkn.type === `paren` && tkn.value === `)`)) {
    let bodyNode: AstNode
    ;[position, bodyNode] = parseToken(tokens, position)
    body.push(bodyNode)
    tkn = token.as(tokens[position], `EOF`)
  }
  if (body.length === 0) {
    throw new LitsError(`Missing body in function`, tkn.debugInfo)
  }
  return [position + 1, body]
}

function parseFunctionOverloades(
  tokens: Token[],
  position: number,
  parsers: ParserHelpers,
): [number, FunctionOverload[]] {
  let tkn = token.as(tokens[position], `EOF`, { type: `paren` })
  if (tkn.value === `(`) {
    const functionOverloades: FunctionOverload[] = []
    while (!(tkn.type === `paren` && tkn.value === `)`)) {
      position += 1
      tkn = token.as(tokens[position], `EOF`)
      let functionArguments: FunctionArguments
      ;[position, functionArguments] = parseFunctionArguments(tokens, position, parsers)
      const arity: Arity = functionArguments.restArgument
        ? { min: functionArguments.mandatoryArguments.length }
        : functionArguments.mandatoryArguments.length

      if (!arityOk(functionOverloades, arity)) {
        throw new LitsError(`All overloaded functions must have different arity`, tkn.debugInfo)
      }

      let functionBody: AstNode[]
      ;[position, functionBody] = parseFunctionBody(tokens, position, parsers)
      functionOverloades.push({
        arguments: functionArguments,
        body: functionBody,
        arity,
      })

      tkn = token.as(tokens[position], `EOF`, { type: `paren` })
      if (tkn.value !== `)` && tkn.value !== `(`) {
        throw new LitsError(`Expected ( or ) token, got ${valueToString(tkn)}.`, tkn.debugInfo)
      }
    }

    return [position + 1, functionOverloades]
  } else if (tkn.value === `[`) {
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
    throw new LitsError(`Expected [ or ( token, got ${valueToString(tkn)}`, tkn.debugInfo)
  }
}

function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parsers: ParserHelpers,
): [number, FunctionArguments] {
  const { parseArgument, parseBindings } = parsers

  let bindings: BindingNode[] = []
  let restArgument: string | undefined = undefined
  const mandatoryArguments: string[] = []
  let state: `mandatory` | `rest` | `let` = `mandatory`
  let tkn = token.as(tokens[position], `EOF`)

  position += 1
  tkn = token.as(tokens[position], `EOF`)
  while (!(tkn.type === `paren` && tkn.value === `]`)) {
    if (state === `let`) {
      ;[position, bindings] = parseBindings(tokens, position)
      break
    } else {
      const [newPosition, node] = parseArgument(tokens, position)
      position = newPosition
      tkn = token.as(tokens[position], `EOF`)

      if (node.type === `Modifier`) {
        switch (node.value) {
          case `&`:
            if (state === `rest`) {
              throw new LitsError(`& can only appear once`, tkn.debugInfo)
            }
            state = `rest`
            break
          case `&let`:
            if (state === `rest` && !restArgument) {
              throw new LitsError(`No rest argument was specified`, tkn.debugInfo)
            }
            state = `let`
            break
          default:
            throw new LitsError(`Illegal modifier: ${node.value}`, tkn.debugInfo)
        }
      } else {
        switch (state) {
          case `mandatory`:
            mandatoryArguments.push(node.name)
            break
          case `rest`:
            if (restArgument !== undefined) {
              throw new LitsError(`Can only specify one rest argument`, tkn.debugInfo)
            }
            restArgument = node.name
            break
        }
      }
    }
  }

  if (state === `rest` && restArgument === undefined) {
    throw new LitsError(`Missing rest argument name`, tkn.debugInfo)
  }

  position += 1

  const args: FunctionArguments = {
    mandatoryArguments,
    restArgument,
    bindings,
  }

  return [position, args]
}
