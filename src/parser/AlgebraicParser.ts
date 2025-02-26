import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import { builtin, specialExpressionKeys } from '../builtin'
import type { FnNode } from '../builtin/specialExpressions/functions'
import type { Arity, FunctionArguments } from '../builtin/utils'
import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { withoutCommentNodes } from '../removeCommentNodes'
import type { A_OperatorToken, AlgebraicTokenType } from '../tokenizer/algebraic/algebraicTokens'
import { assertA_OperatorToken, isA_OperatorToken, isA_SymbolToken } from '../tokenizer/algebraic/algebraicTokens'
import { asLBraceToken, asLBracketToken, assertEndNotationToken, assertRBraceToken, assertRBracketToken, isEndNotationToken, isLBraceToken, isLBracketToken, isLParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { TokenStream } from '../tokenizer/interface'
import type { Token } from '../tokenizer/tokens'
import { getTokenDebugData, hasTokenDebugData } from '../tokenizer/utils'
import { asSymbolNode, isSymbolNode } from '../typeGuards/astNode'
import { arrayToPairs } from '../utils'
import { parseNumber, parseReservedSymbol, parseString, parseSymbol } from './commonTokenParsers'
import type { AstNode, NormalExpressionNodeWithName, ParseState, StringNode, SymbolNode } from './interface'

const exponentiationPrecedence = 9
const placeholderRegexp = /^\$([1-9]\d?)?$/

function getPrecedence(operator: A_OperatorToken): number {
  const operatorSign = operator[1]
  switch (operatorSign) {
    case '.': // accessor
      return 10

    case '**': // exponentiation
      return exponentiationPrecedence

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 8

    case '+': // addition
    case '-': // subtraction
      return 7

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
      return 6

    case '++': // string concatenation
      return 5

    case '<': // less than
    case '<=': // less than or equal
    case '>': // greater than
    case '>=': // greater than or equal
      return 4

    case '==': // equal
    case '!=': // not equal
      return 3

    case '&': // bitwise AND
    case '^': // bitwise XOR
    case '|': // bitwise OR
      return 2

    case '&&': // logical AND
    case '||': // logical OR
    case '??': // nullish coalescing
      return 1

    /* v8 ignore next 8 */
    case '!': // logical NOT
    case '~': // bitwise NOT
    case '=': // property assignemnt operator
    case ',': // element delimiter
    case '=>': // Only used in lamda function parser
    case '...': // Only used in lamda function parser
      throw new Error(`Unknown binary operator: ${operatorSign}`)

    default:
      throw new Error(`Unknown binary operator: ${operatorSign satisfies never}`)
  }
}

function createNamedNormalExpressionNode(name: string, params: AstNode[], token: Token | undefined): NormalExpressionNodeWithName {
  return {
    t: AstNodeType.NormalExpression,
    n: name,
    p: params,
    token: getTokenDebugData(token) && token,
  }
}

function fromSymbolToStringNode(symbol: SymbolNode): StringNode {
  return {
    t: AstNodeType.String,
    v: symbol.v,
    token: getTokenDebugData(symbol.token) && symbol.token,
    p: [],
    n: undefined,
  }
}

function createAccessorNode(left: AstNode, right: AstNode, token: Token | undefined): AstNode {
  // Unnamed normal expression
  return {
    t: AstNodeType.NormalExpression,
    p: [left, right],
    n: undefined,
    token: getTokenDebugData(token) && token,
  }
}

function fromUnaryAlgebraicToAstNode(operator: A_OperatorToken, operand: AstNode): AstNode {
  const token: Token | undefined = hasTokenDebugData(operator) ? operand.token : undefined

  const operatorName = operator[1]

  switch (operatorName) {
    case '+':
      return createNamedNormalExpressionNode('+', [operand], token)
    case '-':
      return createNamedNormalExpressionNode('-', [operand], token)
    case '!':
      return createNamedNormalExpressionNode('not', [operand], token)
    case '~':
      return createNamedNormalExpressionNode('bit-not', [operand], token)
    /* v8 ignore next 2 */
    default:
      throw new Error(`Unknown operator: ${operatorName}`)
  }
}

function fromBinaryAlgebraicToAstNode(operator: A_OperatorToken, left: AstNode, right: AstNode): AstNode {
  const token: Token | undefined = hasTokenDebugData(operator) ? operator : undefined

  const operatorName = operator[1]

  switch (operatorName) {
    case '.':
      return createAccessorNode(left, fromSymbolToStringNode(asSymbolNode(right, getTokenDebugData(token)?.sourceCodeInfo)), token)
    case '**': // exponentiation
      return createNamedNormalExpressionNode('pow', [left, right], token)
    case '*':
      return createNamedNormalExpressionNode('*', [left, right], token)
    case '/':
      return createNamedNormalExpressionNode('/', [left, right], token)
    case '%':
      return createNamedNormalExpressionNode('rem', [left, right], token)
    case '+':
      return createNamedNormalExpressionNode('+', [left, right], token)
    case '-':
      return createNamedNormalExpressionNode('-', [left, right], token)
    case '<<':
      return createNamedNormalExpressionNode('bit-shift-left', [left, right], token)
    case '>>':
      return createNamedNormalExpressionNode('bit-shift-right', [left, right], token)
    case '>>>':
      return createNamedNormalExpressionNode('unsigned-bit-shift-right', [left, right], token)
    case '++': {
      const leftString = createNamedNormalExpressionNode('str', [left], token)
      const rightString = createNamedNormalExpressionNode('str', [right], token)
      return createNamedNormalExpressionNode('str', [leftString, rightString], token)
    }
    case '<':
      return createNamedNormalExpressionNode('<', [left, right], token)
    case '<=':
      return createNamedNormalExpressionNode('<=', [left, right], token)
    case '>':
      return createNamedNormalExpressionNode('>', [left, right], token)
    case '>=':
      return createNamedNormalExpressionNode('>=', [left, right], token)
    case '==':
      return createNamedNormalExpressionNode('=', [left, right], token)
    case '!=':
      return createNamedNormalExpressionNode('not=', [left, right], token)
    case '&':
      return createNamedNormalExpressionNode('bit-and', [left, right], token)
    case '^':
      return createNamedNormalExpressionNode('bit-xor', [left, right], token)
    case '|':
      return createNamedNormalExpressionNode('bit-or', [left, right], token)
    case '&&':
      return {
        t: AstNodeType.SpecialExpression,
        n: 'and',
        p: [left, right],
        token: getTokenDebugData(token) && token,
      }
    case '||':
      return {
        t: AstNodeType.SpecialExpression,
        n: 'or',
        p: [left, right],
        token: getTokenDebugData(token) && token,
      }
    case '??':
      return {
        t: AstNodeType.SpecialExpression,
        n: '??',
        p: [left, right],
        token: getTokenDebugData(token) && token,
      }
    /* v8 ignore next 8 */
    case '!':
    case '~':
    case '=':
    case ',':
    case '=>':
    case '...':
      throw new LitsError(`Unknown binary operator: ${operatorName}`, getTokenDebugData(token)?.sourceCodeInfo)

    default:
      throw new LitsError(`Unknown binary operator: ${operatorName satisfies never}`, getTokenDebugData(token)?.sourceCodeInfo)
  }
}

export class AlgebraicParser {
  constructor(
    private readonly tokenStream: TokenStream,
    private parseState: ParseState,
  ) {}

  private advance(): void {
    this.parseState.position += 1
  }

  public parse(): AstNode {
    return this.parseExpression()
  }

  private parseExpression(precedence = 0): AstNode {
    let left = this.parseOperand()

    while (!this.isAtEnd() && !isA_OperatorToken(this.peek(), ',') && !isRBracketToken(this.peek()) && !isRParenToken(this.peek())) {
      const operator = this.peek()

      // Handle index accessor
      if (isLParenToken(operator)) {
        if (precedence >= 9) {
          break
        }
        left = this.parseFunctionCall(left)
      }
      // Handle index accessor
      else if (isLBracketToken(operator)) {
        if (precedence >= 9) {
          break
        }
        this.advance()
        const right = this.parseExpression(1)
        if (!isRBracketToken(this.peek())) {
          throw new LitsError('Expected closing bracket', getTokenDebugData(this.peek())?.sourceCodeInfo)
        }
        this.advance()
        left = createAccessorNode(left, right, operator)
      }
      else {
        if (!isA_OperatorToken(operator)) {
          break
        }
        const newPrecedece = getPrecedence(operator)
        if (
          newPrecedece <= precedence
          // ** (exponentiation) is right associative
          && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
          break
        }
        this.advance()
        const right = this.parseExpression(newPrecedece)
        left = fromBinaryAlgebraicToAstNode(operator, left, right)
      }
    }

    return left
  }

  private parseOperand(): AstNode {
    const token = this.peek()

    // Parentheses
    if (isLParenToken(token)) {
      const positionBefore = this.parseState.position
      const lamdaFunction = this.parseLambdaFunction()
      if (lamdaFunction) {
        return lamdaFunction
      }
      this.parseState.position = positionBefore
      this.advance()
      const expression = this.parseExpression()
      if (!isRParenToken(this.peek())) {
        throw new Error('Expected closing parenthesis')
      }
      this.advance()
      return expression
    }

    // Unary operators
    if (isA_OperatorToken(token)) {
      const operatorName = token[1]
      if (['-', '+', '!', '~'].includes(operatorName)) {
        this.advance()
        const operand = this.parseOperand()
        return fromUnaryAlgebraicToAstNode(token, operand)
      }
      else if (operatorName === '=>') {
        return this.parseShorthandLamdaFunction()
      }
      else {
        throw new Error(`Unknown unary operator: ${operatorName}`)
      }
    }

    // Object litteral, e.g. {a=1, b=2}
    if (isLBraceToken(token)) {
      return this.parseObject()
    }

    // Array litteral, e.g. [1, 2]
    if (isLBracketToken(token)) {
      return this.parseArray()
    }

    const tokenType = token[0] as Exclude<
      AlgebraicTokenType,
      | 'A_Operator' // Handled above
      | 'LParen' // Handled above
      | 'LBrace' // Handled above
      | 'LBracket' // Handled above

      | 'RParen' // Illegal token
      | 'RBrace' // Illegal token
      | 'RBracket' // Illegal token

      | 'A_MultiLineComment' // Should have been removed
      | 'A_SingleLineComment' // Should have been removed
      | 'A_Whitespace' // Should have been removed
    >
    switch (tokenType) {
      case 'A_Number':
        return parseNumber(this.tokenStream, this.parseState)
      case 'String':
        return parseString(this.tokenStream, this.parseState)
      case 'A_Symbol': {
        return parseSymbol(this.tokenStream, this.parseState)
      }
      case 'A_ReservedSymbol':
        return parseReservedSymbol(this.tokenStream, this.parseState)
      case 'PolNotation': {
        this.parseState.algebraic = false
        const astNodes: AstNode[] = []
        this.advance()
        do {
          astNodes.push(this.parseState.parseToken(this.tokenStream, this.parseState))
        } while (!isEndNotationToken(this.peek()))
        this.advance()
        this.parseState.algebraic = true
        if (astNodes.length === 1) {
          return astNodes[0]!
        }
        return {
          t: AstNodeType.SpecialExpression,
          n: 'do',
          p: astNodes,
          token: getTokenDebugData(token) && token,
        }
      }
      case 'AlgNotation': {
        this.advance()
        const node = this.parseOperand()
        assertEndNotationToken(this.peek())
        this.advance()
        return node
      }

      default:
        throw new LitsError(`Unknown token type: ${tokenType}`, getTokenDebugData(token)?.sourceCodeInfo)
    }
  }

  private parseObject(): AstNode {
    const firstToken = asLBraceToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      const key = this.parseOperand()
      if (key.t !== AstNodeType.Symbol && key.t !== AstNodeType.String) {
        throw new LitsError('Expected key to be a symbol or a string', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }

      params.push({
        t: AstNodeType.String,
        v: key.v,
        token: getTokenDebugData(key.token) && key.token,
        p: [],
        n: undefined,
      })

      assertA_OperatorToken(this.peek(), '=')
      this.advance()

      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isA_OperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
        throw new LitsError('Expected comma or closing brace', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }

      if (isA_OperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBraceToken(this.peek())
    this.advance()

    return {
      t: AstNodeType.NormalExpression,
      n: 'object',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }
  }

  private parseArray(): AstNode {
    const firstToken = asLBracketToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isA_OperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }
      if (isA_OperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBracketToken(this.peek())
    this.advance()

    return {
      t: AstNodeType.NormalExpression,
      n: 'array',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }
  }

  private parseFunctionCall(symbol: AstNode): AstNode {
    const params: AstNode[] = []
    this.advance()
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isA_OperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }
      if (isA_OperatorToken(nextToken, ',')) {
        this.advance()
      }
    }
    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', getTokenDebugData(this.peek())?.sourceCodeInfo)
    }
    this.advance()
    if (isSymbolNode(symbol)) {
      if (specialExpressionKeys.includes(symbol.v)) {
        const name: SpecialExpressionName = symbol.v as SpecialExpressionName
        switch (name) {
          case '??':
          case 'and':
          case 'comment':
          case 'cond':
          case 'declared?':
          case 'if':
          case 'if-not':
          case 'or':
          case 'when':
          case 'when-not':
          case 'do':
          case 'time!':
          case 'throw': {
            const node: SpecialExpressionNode = {
              t: AstNodeType.SpecialExpression,
              n: name,
              p: params,
              token: getTokenDebugData(symbol.token) && symbol.token,
            }
            builtin.specialExpressions[node.n].validateParameterCount(node)
            return node
          }
          case 'let':
            return this.parseLet(symbol, params)
          case 'def':
          case 'defs':
          case 'if-let':
          case 'when-let':
          case 'when-first':
          case 'fn':
          case 'defn':
          case 'defns':
          case 'try':
          case 'recur':
          case 'loop':
          case 'doseq':
          case 'for':
            throw new Error(`Special expression ${name} is not available in algebraic notation`)
          default:
            throw new Error(`Unknown special expression: ${name satisfies never}`)
        }
      }
      const node = createNamedNormalExpressionNode(symbol.v, params, symbol.token)

      const builtinExpression = builtin.normalExpressions[node.n]

      if (builtinExpression) {
        builtinExpression.validate?.({
          ...node,
          p: withoutCommentNodes(node.p),
        })
      }

      return node
    }
    else {
      return {
        t: AstNodeType.NormalExpression,
        n: undefined,
        p: [symbol, ...params],
        token: getTokenDebugData(symbol.token) && symbol.token,
      }
    }
  }

  parseLambdaFunction(): AstNode | null {
    const firstToken = this.peek()
    this.advance()
    let rest = false
    let letBindingObject: AstNode | undefined
    const args: string[] = []
    let restArg: string | undefined
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      if (letBindingObject) {
        throw new LitsError('Expected right parentheses', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }
      if (isLBraceToken(this.peek())) {
        letBindingObject = this.parseObject()
      }
      else {
        if (isA_OperatorToken(this.peek(), '...')) {
          if (rest) {
            throw new LitsError('Multiple spread operators in lambda function', getTokenDebugData(this.peek())?.sourceCodeInfo)
          }
          this.advance()
          rest = true
        }
        const symbolToken = this.peek()
        if (!isA_SymbolToken(symbolToken)) {
          return null
        }
        if (rest) {
          restArg = symbolToken[1]
        }
        else {
          args.push(symbolToken[1])
        }
        this.advance()
      }
      if (!isA_OperatorToken(this.peek(), ',') && !isRParenToken(this.peek())) {
        return null
      }
      if (isA_OperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }
    if (!isRParenToken(this.peek())) {
      return null
    }
    this.advance()
    if (!isA_OperatorToken(this.peek(), '=>')) {
      return null
    }
    this.advance()
    const letBindings = letBindingObject ? arrayToPairs(letBindingObject.p) : []
    const body = this.parseExpression()
    const arity: Arity = restArg !== undefined ? { min: args.length } : args.length

    return {
      t: AstNodeType.SpecialExpression,
      n: 'fn',
      p: [],
      o: [{
        as: {
          m: args,
          r: restArg,
          b: letBindings.map((pair) => {
            const key = pair[0] as StringNode
            const value = pair[1] as AstNode
            return {
              t: AstNodeType.Binding,
              n: key.v,
              v: value,
              p: [],
              token: getTokenDebugData(key.token) && key.token,
            }
          }),
        },
        b: [body],
        a: arity,
      }],
      token: getTokenDebugData(firstToken) && firstToken,
    }
  }

  private parseShorthandLamdaFunction(): FnNode {
    const firstToken = this.peek()
    this.advance()
    const startPos = this.parseState.position
    const exprNode = this.parseExpression()
    const endPos = this.parseState.position - 1

    let arity = 0
    let percent1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET' // referring to argument bindings. % = NAKED, %1, %2, %3, etc = WITH_1
    for (let pos = startPos; pos <= endPos; pos += 1) {
      const tkn = this.tokenStream.tokens[pos]!
      if (isA_SymbolToken(tkn)) {
        const match = placeholderRegexp.exec(tkn[1])
        if (match) {
          const number = match[1] ?? '1'
          if (number === '1') {
            const mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED')
            if (mixedPercent1)
              throw new LitsError('Please make up your mind, either use $ or $1', getTokenDebugData(firstToken)?.sourceCodeInfo)

            percent1 = match[1] ? 'WITH_1' : 'NAKED'
          }

          arity = Math.max(arity, Number(number))
          if (arity > 20)
            throw new LitsError('Can\'t specify more than 20 arguments', getTokenDebugData(firstToken)?.sourceCodeInfo)
        }
      }
      if (isA_OperatorToken(tkn, '=>')) {
        throw new LitsError('Nested shortcut functions are not allowed', getTokenDebugData(firstToken)?.sourceCodeInfo)
      }
    }

    const mandatoryArguments: string[] = []

    for (let i = 1; i <= arity; i += 1) {
      if (i === 1 && percent1 === 'NAKED')
        mandatoryArguments.push('$')
      else
        mandatoryArguments.push(`$${i}`)
    }

    const args: FunctionArguments = {
      b: [],
      m: mandatoryArguments,
    }

    const node: FnNode = {
      t: AstNodeType.SpecialExpression,
      n: 'fn',
      p: [],
      o: [
        {
          as: args,
          b: [exprNode],
          a: args.m.length,
        },
      ],
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  }

  private parseLet(nameSymbol: SymbolNode, params: AstNode[]): AstNode {
    if (params.length !== 2) {
      throw new LitsError('let expects two arguments', getTokenDebugData(nameSymbol.token)?.sourceCodeInfo)
    }

    const letObject = params[0]!
    if (letObject.t !== AstNodeType.NormalExpression || letObject.n !== 'object') {
      throw new LitsError('let expects an object as first argument', getTokenDebugData(letObject.token)?.sourceCodeInfo)
    }

    const letBindings = arrayToPairs(letObject.p)
    const expression = params[1]!

    return {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: [expression],
      token: getTokenDebugData(nameSymbol.token) && nameSymbol.token,
      bs: letBindings.map((pair) => {
        const key = pair[0] as StringNode
        const value = pair[1] as AstNode

        return {
          t: AstNodeType.Binding,
          n: key.v,
          v: value,
          p: [],
          token: getTokenDebugData(key.token) && key.token,
        }
      }),
    }
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }
}
