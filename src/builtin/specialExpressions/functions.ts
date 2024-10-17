import type { SpecialExpressionNode } from '..'
import type { FindUnresolvedIdentifiers, UnresolvedIdentifier, UnresolvedIdentifiers } from '../../analyze'
import { addAnalyzeResults } from '../../analyze/utils'
import { AstNodeType, FunctionType, TokenType } from '../../constants/constants'
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
} from '../../parser/interface'
import type { TokenStream } from '../../tokenizer/interface'
import { asAstNode, assertNameNode } from '../../typeGuards/astNode'
import { asString, assertString } from '../../typeGuards/string'
import { asToken } from '../../typeGuards/token'
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
  parse: (tokenStream, position, firstToken, parsers) => {
    const { parseToken } = parsers
    let functionName
    ;[position, functionName] = parseToken(tokenStream, position)
    assertNameNode(functionName, functionName.debugData?.token.debugData?.sourceCodeInfo)

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokenStream, position, parsers)
    const lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: DefnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defn',
      f: functionName,
      p: [],
      o: functionOverloades,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    return [
      position + 1,
      node,
    ]
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName('defn', node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, node.debugData?.token.debugData?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.debugData?.token.debugData?.sourceCodeInfo,
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
  parse: (tokenStream, position, firstToken, parsers) => {
    const { parseToken } = parsers
    let functionName: AstNode
    ;[position, functionName] = parseToken(tokenStream, position)

    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokenStream, position, parsers)
    const lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: DefnsNode = {
      t: AstNodeType.SpecialExpression,
      n: 'defns',
      p: [],
      f: functionName,
      o: functionOverloades,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    return [
      position + 1,
      node,
    ]
  },
  evaluate: (node, contextStack, { builtin, evaluateAstNode }) => {
    const name = getFunctionName('defns', node, contextStack, evaluateAstNode)

    assertNameNotDefined(name, contextStack, builtin, node.debugData?.token.debugData?.sourceCodeInfo)

    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.debugData?.token.debugData?.sourceCodeInfo,
      t: FunctionType.UserDefined,
      n: name,
      o: evaluatedFunctionOverloades,
    }

    contextStack.globalContext[name] = { value: litsFunction }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin, evaluateAstNode }) => {
    const sourceCodeInfo = node.debugData?.token.debugData?.sourceCodeInfo
    const name = evaluateAstNode(asAstNode(node.f, sourceCodeInfo), contextStack)
    assertString(name, sourceCodeInfo)
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.globalContext[name] = { value: true }
    const newContext = { [name]: { value: true } }

    return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext)
  },
}

export const fnSpecialExpression: BuiltinSpecialExpression<LitsFunction, FnNode> = {
  parse: (tokenStream, position, firstToken, parsers) => {
    let functionOverloades: FunctionOverload[]
    ;[position, functionOverloades] = parseFunctionOverloades(tokenStream, position, parsers)
    const lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: FnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'fn',
      p: [],
      o: functionOverloades,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    return [
      position + 1,
      node,
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode)

    const litsFunction: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      sourceCodeInfo: node.debugData?.token.debugData?.sourceCodeInfo,
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
  const sourceCodeInfo = node.debugData?.token.debugData?.sourceCodeInfo
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
  position: number,
  { parseToken }: ParserHelpers,
): [number, AstNode[]] {
  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const body: AstNode[] = []
  while (!(tkn.t === TokenType.Bracket && tkn.v === ')')) {
    let bodyNode: AstNode
    ;[position, bodyNode] = parseToken(tokenStream, position)
    body.push(bodyNode)
    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }
  if (body.length === 0)
    throw new LitsError('Missing body in function', tkn.debugData?.sourceCodeInfo)

  return [position, body]
}

function parseFunctionOverloades(
  tokenStream: TokenStream,
  position: number,
  parsers: ParserHelpers,
): [number, FunctionOverload[]] {
  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket })
  if (tkn.v === '(') {
    const functionOverloades: FunctionOverload[] = []
    while (!(tkn.t === TokenType.Bracket && tkn.v === ')')) {
      position += 1
      tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
      let functionArguments: FunctionArguments
      ;[position, functionArguments] = parseFunctionArguments(tokenStream, position, parsers)
      const arity: Arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length

      if (!arityOk(functionOverloades, arity))
        throw new LitsError('All overloaded functions must have different arity', tkn.debugData?.sourceCodeInfo)

      let functionBody: AstNode[]
      ;[position, functionBody] = parseFunctionBody(tokenStream, position, parsers)
      functionOverloades.push({
        as: functionArguments,
        b: functionBody,
        a: arity,
      })

      tkn = asToken(tokenStream.tokens[position + 1], tokenStream.filePath, { type: TokenType.Bracket })
      if (tkn.v !== ')' && tkn.v !== '(')
        throw new LitsError(`Expected ( or ) token, got ${valueToString(tkn)}.`, tkn.debugData?.sourceCodeInfo)
      position += 1
    }

    return [position, functionOverloades]
  }
  else if (tkn.v === '[') {
    let functionArguments: FunctionArguments
    ;[position, functionArguments] = parseFunctionArguments(tokenStream, position, parsers)
    const arity: Arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length
    let functionBody: AstNode[]
    ;[position, functionBody] = parseFunctionBody(tokenStream, position, parsers)
    return [
      position,
      [
        {
          as: functionArguments,
          b: functionBody,
          a: arity,
        },
      ],
    ]
  }
  else {
    throw new LitsError(`Expected [ or ( token, got ${valueToString(tkn)}`, tkn.debugData?.sourceCodeInfo)
  }
}

function parseFunctionArguments(
  tokenStream: TokenStream,
  position: number,
  parsers: ParserHelpers,
): [number, FunctionArguments] {
  const { parseArgument, parseBindings } = parsers

  let bindings: BindingNode[] = []
  let restArgument: string | undefined
  const mandatoryArguments: string[] = []
  let state: 'mandatory' | 'rest' | 'let' = 'mandatory'
  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)

  position += 1
  tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
    if (state === 'let') {
      ;[position, bindings] = parseBindings(tokenStream, position)
      break
    }
    else {
      const [newPosition, node] = parseArgument(tokenStream, position)
      position = newPosition
      tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)

      if (node.t === AstNodeType.Modifier) {
        switch (node.v) {
          case '&':
            if (state === 'rest')
              throw new LitsError('& can only appear once', tkn.debugData?.sourceCodeInfo)

            state = 'rest'
            break
          case '&let':
            if (state === 'rest' && !restArgument)
              throw new LitsError('No rest argument was specified', tkn.debugData?.sourceCodeInfo)

            state = 'let'
            break
          default:
            throw new LitsError(`Illegal modifier: ${node.v}`, tkn.debugData?.sourceCodeInfo)
        }
      }
      else {
        switch (state) {
          case 'mandatory':
            mandatoryArguments.push(node.n)
            break
          case 'rest':
            if (restArgument !== undefined)
              throw new LitsError('Can only specify one rest argument', tkn.debugData?.sourceCodeInfo)

            restArgument = node.n
            break
        }
      }
    }
  }

  if (state === 'rest' && restArgument === undefined)
    throw new LitsError('Missing rest argument name', tkn.debugData?.sourceCodeInfo)

  position += 1

  const args: FunctionArguments = {
    m: mandatoryArguments,
    r: restArgument,
    b: bindings,
  }

  return [position, args]
}
