import type { SpecialExpressionNode } from '..'
import type { FindUnresolvedIdentifiers, UnresolvedIdentifier, UnresolvedIdentifiers } from '../../analyze'
import { addAnalyzeResults } from '../../analyze/utils'
import { AstNodeType, FunctionType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type {
  AstNode,
  BindingNode,
  CommonSpecialExpressionNode,
  EvaluatedFunctionOverload,
  LitsFunction,
  ParseState,
  SymbolNode,
} from '../../parser/interface'
import { asLBracketToken, assertRParenToken, isLBracketToken, isLParenToken, isRBracketToken, isRParenToken } from '../../tokenizer/common/commonTokens'
import type { TokenStream } from '../../tokenizer/interface'
import { asToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertSymbolNode } from '../../typeGuards/astNode'
import { valueToString } from '../../utils/debug/debugTools'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { Builtin, BuiltinSpecialExpression, ParserHelpers } from '../interface'
import type { Arity, FunctionArguments, FunctionOverload } from '../utils'
import { assertNameNotDefined } from '../utils'

export interface DefnNode extends CommonSpecialExpressionNode<'defn'> {
  f: SymbolNode
  o: FunctionOverload[]
}

export interface FnNode extends CommonSpecialExpressionNode<'fn'> {
  p: AstNode[]
  o: FunctionOverload[]
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  polishParse: (tokenStream, parseState, firstToken, parsers) => {
    const { parseToken } = parsers
    const functionName = parseToken(tokenStream, parseState)
    assertSymbolNode(functionName, getTokenDebugData(functionName.token)?.sourceCodeInfo)

    const functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: DefnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defn',
      f: functionName,
      p: [],
      o: functionOverloades,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = node.f.v

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.token)?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.exportValue(name, litsFunction)
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    contextStack.exportValue(node.f.v, true)
    const newContext: Context = { [node.f.v]: { value: true } }
    return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  polishParse: (tokenStream, parseState, firstToken, parsers) => {
    const functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: FnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'fn',
      p: [],
      o: functionOverloades,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  validateParameterCount: () => undefined,
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: undefined,
      o: evaluatedFunctionOverloades,
    }

    return litsFunction
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) =>
    addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin),
}

function evaluateFunctionOverloades(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunctionOverload[] {
  const evaluatedFunctionOverloades: EvaluatedFunctionOverload[] = []
  for (const functionOverload of (node as DefnNode | FnNode).o) {
    const functionContext: Context = {}
    for (const binding of functionOverload.as.b) {
      const bindingValueNode = binding.v
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      functionContext[binding.n] = { value: bindingValue }
    }

    const evaluatedFunctionOverload: EvaluatedFunctionOverload = {
      as: {
        mandatoryArguments: functionOverload.as.m,
        restArgument: functionOverload.as.r,
      },
      a: functionOverload.a,
      b: functionOverload.b,
      f: functionContext,
    }

    evaluatedFunctionOverloades.push(evaluatedFunctionOverload)
  }
  return evaluatedFunctionOverloades
}

function addOverloadsUnresolvedIdentifiers(
  overloads: FunctionOverload[],
  contextStack: ContextStack,
  findUnresolvedIdentifiers: FindUnresolvedIdentifiers,
  builtin: Builtin,
  functionNameContext?: Context,
): UnresolvedIdentifiers {
  const result = new Set<UnresolvedIdentifier>()
  const contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack
  for (const overload of overloads) {
    const newContext: Context = {}
    overload.as.b.forEach((binding) => {
      const bindingResult = findUnresolvedIdentifiers([binding.v], contextStack, builtin)
      addAnalyzeResults(result, bindingResult)
      newContext[binding.n] = { value: true }
    })
    overload.as.m.forEach((arg) => {
      newContext[arg] = { value: true }
    })
    if (typeof overload.as.r === 'string')
      newContext[overload.as.r] = { value: true }

    const newContextStack = contextStackWithFunctionName.create(newContext)
    const overloadResult = findUnresolvedIdentifiers(overload.b, newContextStack, builtin)
    addAnalyzeResults(result, overloadResult)
  }
  return result
}

function arityOk(overloadedFunctions: FunctionOverload[], arity: Arity) {
  if (typeof arity === 'number') {
    return overloadedFunctions.every((fun) => {
      if (typeof fun.a === 'number')
        return fun.a !== arity

      return fun.a.min > arity
    })
  }
  return overloadedFunctions.every((fun) => {
    if (typeof fun.a === 'number')
      return fun.a < arity.min

    return false
  })
}

function parseFunctionBody(
  tokenStream: TokenStream,
  parseState: ParseState,
  { parseToken }: ParserHelpers,
): AstNode[] {
  let tkn = asToken(tokenStream.tokens[parseState.position])
  const body: AstNode[] = []
  while (!isRParenToken(tkn)) {
    body.push(parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }
  if (body.length === 0)
    throw new LitsError('Missing body in function', getTokenDebugData(tkn)?.sourceCodeInfo)

  return body
}

function parseFunctionOverloades(
  tokenStream: TokenStream,
  parseState: ParseState,
  parsers: ParserHelpers,
): FunctionOverload[] {
  let tkn = asToken(tokenStream.tokens[parseState.position])
  if (isLParenToken(tkn)) {
    const functionOverloades: FunctionOverload[] = []
    while (!isRParenToken(tkn)) {
      parseState.position++
      tkn = asLBracketToken(tokenStream.tokens[parseState.position])
      const functionArguments = parseFunctionArguments(tokenStream, parseState, parsers)
      const arity: Arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length

      if (!arityOk(functionOverloades, arity))
        throw new LitsError('All overloaded functions must have different arity', getTokenDebugData(tkn)?.sourceCodeInfo)

      const functionBody = parseFunctionBody(tokenStream, parseState, parsers)
      functionOverloades.push({
        as: functionArguments,
        b: functionBody,
        a: arity,
      })

      tkn = asToken(tokenStream.tokens[++parseState.position])
      if (!isRParenToken(tkn) && !isLParenToken(tkn))
        throw new LitsError(`Expected ( or ) token, got ${valueToString(tkn)}.`, getTokenDebugData(tkn)?.sourceCodeInfo)
    }

    return functionOverloades
  }
  else if (isLBracketToken(tkn)) {
    const functionArguments = parseFunctionArguments(tokenStream, parseState, parsers)
    const arity: Arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length
    const functionBody = parseFunctionBody(tokenStream, parseState, parsers)
    return [
      {
        as: functionArguments,
        b: functionBody,
        a: arity,
      },
    ]
  }
  else {
    throw new LitsError(`Expected [ or ( token, got ${valueToString(tkn)}`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }
}

function parseFunctionArguments(
  tokenStream: TokenStream,
  parseState: ParseState,
  parsers: ParserHelpers,
): FunctionArguments {
  const { parseArgument, parseBindings } = parsers

  let bindings: BindingNode[] = []
  let restArgument: string | undefined
  const mandatoryArguments: string[] = []
  let state: 'mandatory' | 'rest' | 'let' = 'mandatory'
  let tkn = asToken(tokenStream.tokens[++parseState.position])

  // let tkn = asToken(tokenStream.tokens[parseState.position])
  while (!isRBracketToken(tkn)) {
    if (state === 'let') {
      bindings = parseBindings(tokenStream, parseState)
      break
    }
    else {
      const node = parseArgument(tokenStream, parseState)
      tkn = asToken(tokenStream.tokens[parseState.position])

      if (node.t === AstNodeType.Modifier) {
        switch (node.v) {
          case '&rest':
            if (state === 'rest')
              throw new LitsError('& can only appear once', getTokenDebugData(tkn)?.sourceCodeInfo)

            state = 'rest'
            break
          case '&let':
            if (state === 'rest' && !restArgument)
              throw new LitsError('No rest argument was specified', getTokenDebugData(tkn)?.sourceCodeInfo)

            state = 'let'
            break
          default:
            throw new LitsError(`Illegal modifier: ${node.v}`, getTokenDebugData(tkn)?.sourceCodeInfo)
        }
      }
      else {
        switch (state) {
          case 'mandatory':
            mandatoryArguments.push(node.n)
            break
          case 'rest':
            if (restArgument !== undefined)
              throw new LitsError('Can only specify one rest argument', getTokenDebugData(tkn)?.sourceCodeInfo)

            restArgument = node.n
            break
        }
      }
    }
  }
  parseState.position += 1

  if (state === 'rest' && restArgument === undefined)
    throw new LitsError('Missing rest argument name', getTokenDebugData(tkn)?.sourceCodeInfo)

  const args: FunctionArguments = {
    m: mandatoryArguments,
    r: restArgument,
    b: bindings,
  }

  return args
}
