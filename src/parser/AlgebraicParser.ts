import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { asLBraceToken, asLBracketToken, assertEndNotationToken, assertRBraceToken, assertRBracketToken, isEndNotationToken, isLBraceToken, isLBracketToken, isLParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { A_OperatorToken, AlgebraicTokenType } from '../tokenizer/algebraic/algebraicTokens'
import { assertA_OperatorToken, isA_OperatorToken, isA_SymbolToken } from '../tokenizer/algebraic/algebraicTokens'
import type { TokenStream } from '../tokenizer/interface'
import type { Token } from '../tokenizer/tokens'
import { getTokenDebugData, hasTokenDebugData } from '../tokenizer/utils'
import { asSymbolNode, isSymbolNode } from '../typeGuards/astNode'
import { specialExpressionKeys } from '../builtin'
import type { SpecialExpressionName } from '../builtin'
import type { Arity } from '../builtin/utils'
import { parseNumber, parseReservedSymbol, parseString, parseSymbol } from './commonTokenParsers'
import type { AstNode, NormalExpressionNodeWithName, ParseState, StringNode, SymbolNode } from './interface'

const exponentiationPrecedence = 8
function getPrecedence(operator: A_OperatorToken): number {
  const operatorSign = operator[1]
  switch (operatorSign) {
    case '.': // accessor
      return 9

    case '**': // exponentiation
      return exponentiationPrecedence

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 7

    case '+': // addition
    case '-': // subtraction
      return 6

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
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
      const lamdaFunction = this.parseLamdaFunction()
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
        let astNode: AstNode
        this.advance()
        do {
          astNode = this.parseState.parseToken(this.tokenStream, this.parseState)
        } while (!isEndNotationToken(this.peek()))
        this.advance()
        this.parseState.algebraic = true
        return astNode
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
            return {
              t: AstNodeType.SpecialExpression,
              n: name,
              p: params,
              token: getTokenDebugData(symbol.token) && symbol.token,
            }
          case 'def':
          case 'defs':
          case 'let':
          case 'if-let':
          case 'when-let':
          case 'when-first':
          case 'fn':
          case 'defn':
          case 'defns':
          case 'try':
          case 'throw':
          case 'recur':
          case 'loop':
          case 'time!':
          case 'doseq':
          case 'for':
          case 'do':
            throw new Error(`Special expression ${name} is not available in algebraic notation`)
          default:
            throw new Error(`Unknown special expression: ${name satisfies never}`)
        }
      }
      return createNamedNormalExpressionNode(symbol.v, params, symbol.token)
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

  parseLamdaFunction(): AstNode | null {
    const firstToken = this.peek()
    this.advance()
    let spread = false
    const args: string[] = []
    let restArg: string | undefined
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      if (isA_OperatorToken(this.peek(), '...')) {
        if (spread) {
          throw new LitsError('Multiple spread operators in lambda function', getTokenDebugData(this.peek())?.sourceCodeInfo)
        }
        this.advance()
        spread = true
      }
      const symbolToken = this.peek()
      if (!isA_SymbolToken(symbolToken)) {
        return null
      }
      if (spread) {
        restArg = symbolToken[1]
      }
      else {
        args.push(symbolToken[1])
      }
      this.advance()
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
          b: [],
        },
        b: [body],
        a: arity,
      }],
      token: getTokenDebugData(firstToken) && firstToken,
    }
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }
}
