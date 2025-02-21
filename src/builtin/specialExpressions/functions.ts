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
  NameNode,
  ParseState,
} from '../../parser/interface'
import { asLBracketToken, asRParenToken, isLBracketToken, isLParenToken, isRBracketToken, isRParenToken } from '../../tokenizer/common/commonTokens'
import type { TokenStream } from '../../tokenizer/interface'
import { asToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asAstNode, assertNameNode } from '../../typeGuards/astNode'
import { asString, assertString } from '../../typeGuards/string'
import { valueToString } from '../../utils/debug/debugTools'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { Builtin, BuiltinSpecialExpression, ParserHelpers } from '../interface'
import type { Arity, FunctionArguments, FunctionOverload } from '../utils'
import { assertNameNotDefined } from '../utils'

export interface DefnNode extends CommonSpecialExpressionNode<'defn'> {
  f: NameNode
  o: FunctionOverload[]
}

export interface DefnsNode extends CommonSpecialExpressionNode<'defns'> {
  f: AstNode
  o: FunctionOverload[]
}

export interface FnNode extends CommonSpecialExpressionNode<'fn'> {
  p: AstNode[]
  o: FunctionOverload[]
}

export const defnSpecialExpression: BuiltinSpecialExpression<null, DefnNode> = {
  parse: (tokenStream, parseState, firstToken, parsers) => {
    const { parseToken } = parsers
    const functionName = parseToken(tokenStream, parseState)
    assertNameNode(functionName, getTokenDebugData(functionName.debugData?.token)?.sourceCodeInfo)

    const functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: DefnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defn',
      f: functionName,
      p: [],
      o: functionOverloades,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    return node
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName('defn', node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.debugData?.token)?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.debugData?.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.globalContext[name] = { value: litsFunction }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => {
    contextStack.globalContext[node.f.v] = { value: true }
    const newContext: Context = { [node.f.v]: { value: true } }
    return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext)
  },
}

export const defnsSpecialExpression: BuiltinSpecialExpression<null, DefnsNode> = {
  parse: (tokenStream, parseState, firstToken, parsers) => {
    const { parseToken } = parsers
    const functionName = parseToken(tokenStream, parseState)

    const functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: DefnsNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defns',
      p: [],
      f: functionName,
      o: functionOverloades,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    return node
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName('defns', node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, getTokenDebugData(node.debugData?.token)?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.debugData?.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.globalContext[name] = { value: litsFunction }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin, evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.debugData?.token)?.sourceCodeInfo
    const name = evaluateAstNode(asAstNode(node.f, sourceCodeInfo), contextStack)
    assertString(name, sourceCodeInfo)
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    const newContext = { [name]: { value: true } }

    return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  parse: (tokenStream, parseState, firstToken, parsers) => {
    const functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: FnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'fn',
      p: [],
      o: functionOverloades,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: getTokenDebugData(node.debugData?.token)?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: undefined,
      o: evaluatedFunctionOverloades,
    }

    return litsFunction
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) =>
    addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin),
}

function getFunctionName(
  expressionName: 'defn' | 'defns',
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): string {
  const sourceCodeInfo = getTokenDebugData(node.debugData?.token)?.sourceCodeInfo
  if (expressionName === 'defn')
    return ((node as DefnNode).f).v

  const name = evaluateAstNode((node as DefnsNode).f, contextStack)
  return asString(name, sourceCodeInfo)
}

function evaluateFunctionOverloades(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
): EvaluatedFunctionOverload[] {
  const evaluatedFunctionOverloades: EvaluatedFunctionOverload[] = []
  for (const functionOverload of (node as DefnNode | DefnsNode | FnNode).o) {
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
          case '&':
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
