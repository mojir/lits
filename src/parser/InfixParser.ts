import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { asLBraceToken, asLBracketToken, assertRBraceToken, assertRBracketToken, isLBraceToken, isLBracketToken, isLParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { IF_OperatorToken, InfixTokenType } from '../tokenizer/infix/infixTokens'
import { assertIF_OperatorToken, isIF_OperatorToken } from '../tokenizer/infix/infixTokens'
import type { TokenStream } from '../tokenizer/interface'
import type { Token } from '../tokenizer/tokens'
import { hasTokenDebugData } from '../tokenizer/utils'
import { parseNumber, parseReservedSymbol, parseString, parseSymbol } from './commonTokenParsers'
import type { AstNode, NormalExpressionNodeWithName, ParseState } from './interface'

function getPrecedence(operator: IF_OperatorToken): number {
  const operatorSign = operator[1]
  switch (operatorSign) {
    case '**': // exponentiation
      return 1

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 2

    case '+': // addition
    case '-': // subtraction
      return 3

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
      return 4

    case '<': // less than
    case '<=': // less than or equal
    case '>': // greater than
    case '>=': // greater than or equal
      return 5

    case '==': // equal
    case '!=': // not equal
      return 6

    case '&': // bitwise AND
    case '^': // bitwise XOR
    case '|': // bitwise OR
      return 7

    case '&&': // logical AND
    case '||': // logical OR
    case '??': // nullish coalescing
      return 8

    /* v8 ignore next 8 */
    case '!': // logical NOT
    case '~': // bitwise NOT
    case '=': // property assignemnt operator
    case ',': // element delimiter
      throw new Error(`Unknown binary operator: ${operatorSign}`)

    default:
      throw new Error(`Unknown binary operator: ${operatorSign satisfies never}`)
  }
}

function createNormalExpressionNode(name: string, params: AstNode[], token: Token | undefined): NormalExpressionNodeWithName {
  return {
    t: AstNodeType.NormalExpression,
    n: name,
    p: params,
    token,
  }
}

function fromUnaryInfixToAstNode(operator: IF_OperatorToken, operand: AstNode): AstNode {
  const token: Token | undefined = hasTokenDebugData(operator) ? operand.token : undefined

  const operatorName = operator[1]

  switch (operatorName) {
    case '+':
      return createNormalExpressionNode('+', [operand], token)
    case '-':
      return createNormalExpressionNode('-', [operand], token)
    case '!':
      return createNormalExpressionNode('not', [operand], token)
    case '~':
      return createNormalExpressionNode('bit-not', [operand], token)
    /* v8 ignore next 2 */
    default:
      throw new Error(`Unknown operator: ${operatorName}`)
  }
}

function fromBinaryInfixToAstNode(operator: IF_OperatorToken, left: AstNode, right: AstNode): AstNode {
  const token: Token | undefined = hasTokenDebugData(operator) ? operator : undefined

  const operatorName = operator[1]

  switch (operatorName) {
    case '**': // exponentiation
      return createNormalExpressionNode('pow', [left, right], token)
    case '*':
      return createNormalExpressionNode('*', [left, right], token)
    case '/':
      return createNormalExpressionNode('/', [left, right], token)
    case '%':
      return createNormalExpressionNode('rem', [left, right], token)
    case '+':
      return createNormalExpressionNode('+', [left, right], token)
    case '-':
      return createNormalExpressionNode('-', [left, right], token)
    case '<<':
      return createNormalExpressionNode('bit-shift-left', [left, right], token)
    case '>>':
      return createNormalExpressionNode('bit-shift-right', [left, right], token)
    case '>>>':
      return createNormalExpressionNode('unsigned-bit-shift-right', [left, right], token)
    case '<':
      return createNormalExpressionNode('<', [left, right], token)
    case '<=':
      return createNormalExpressionNode('<=', [left, right], token)
    case '>':
      return createNormalExpressionNode('>', [left, right], token)
    case '>=':
      return createNormalExpressionNode('>=', [left, right], token)
    case '==':
      return createNormalExpressionNode('=', [left, right], token)
    case '!=':
      return createNormalExpressionNode('not=', [left, right], token)
    case '&':
      return createNormalExpressionNode('bit-and', [left, right], token)
    case '^':
      return createNormalExpressionNode('bit-xor', [left, right], token)
    case '|':
      return createNormalExpressionNode('bit-or', [left, right], token)
    case '&&':
      return {
        t: AstNodeType.SpecialExpression,
        n: 'and',
        p: [left, right],
        token,
      }
    case '||':
      return {
        t: AstNodeType.SpecialExpression,
        n: 'or',
        p: [left, right],
        token,
      }
    case '??':
      return {
        t: AstNodeType.SpecialExpression,
        n: '??',
        p: [left, right],
        token,
      }
    /* v8 ignore next 8 */
    case '!':
    case '~':
    case '=':
    case ',':
      throw new Error(`Unknown binary operator: ${operatorName}`)

    default:
      throw new Error(`Unknown binary operator: ${operatorName satisfies never}`)
  }
}

export class InfixParser {
  constructor(
    private readonly tokenStream: TokenStream,
    private parseState: ParseState,
  ) {}

  private advance(): void {
    this.parseState.position++
  }

  public parse(): AstNode {
    return this.parseExpression()
  }

  private parseExpression(precedence = 0): AstNode {
    let left = this.parseOperand()

    while (!this.isAtEnd() && !isIF_OperatorToken(this.peek(), ',')) {
      const operator = this.peek()
      if (!isIF_OperatorToken(operator)) {
        break
      }
      const newPrecedece = getPrecedence(operator)
      if (
        newPrecedece <= precedence
        // ** (exponentiation) is right associative
        && !(newPrecedece === 1 && precedence === 1)) {
        break
      }
      this.advance()
      const right = this.parseExpression(newPrecedece)
      left = fromBinaryInfixToAstNode(operator, left, right)
    }

    return left
  }

  private parseOperand(): AstNode {
    const token = this.peek()

    // Parentheses
    if (isLParenToken(token)) {
      this.advance()
      const expression = this.parseExpression()
      if (!isRParenToken(this.peek())) {
        throw new Error('Expected closing parenthesis')
      }
      this.advance()
      return expression
    }

    // Unary operators
    if (isIF_OperatorToken(token)) {
      const operatorName = token[1]
      if (['-', '+', '!', '~'].includes(operatorName)) {
        this.advance()
        const operand = this.parseOperand()
        return fromUnaryInfixToAstNode(token, operand)
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
      InfixTokenType,
      | 'IF_Operator' // Handled above
      | 'LParen' // Handled above
      | 'LBrace' // Handled above
      | 'LBracket' // Handled above

      | 'RParen' // Illegal token
      | 'RBrace' // Illegal token
      | 'RBracket' // Illegal token

      | 'IF_MultiLineComment' // Should have been removed
      | 'IF_SingleLineComment' // Should have been removed
      | 'IF_Whitespace' // Should have been removed
    >
    switch (tokenType) {
      case 'IF_Number':
        return parseNumber(this.tokenStream, this.parseState)
      case 'String':
        return parseString(this.tokenStream, this.parseState)
      case 'IF_Symbol':
        return parseSymbol(this.tokenStream, this.parseState)
      case 'IF_ReservedSymbol':
        return parseReservedSymbol(this.tokenStream, this.parseState)
      case 'IF_Postfix': {
        // this.parseState.position++
        // this.parseState.infix = false
        // const astNode = this.parseState.parseToken(this.tokenStream, this.parseState)
        // this.parseState.infix = true
        // return astNode
        throw new LitsError('Not implemented')
      }

      default:
        throw new LitsError(`Unknown token type: ${tokenType}`)
    }
  }

  private parseObject(): AstNode {
    const firstToken = asLBraceToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      const key = this.parseOperand()
      if (key.t !== AstNodeType.Symbol && key.t !== AstNodeType.String) {
        throw new LitsError('Expected key to be a symbol or a string')
      }

      params.push({
        t: AstNodeType.String,
        v: key.v,
        token: key.token,
        p: [],
        n: undefined,
      })

      assertIF_OperatorToken(this.peek(), '=')
      this.advance()

      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isIF_OperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
        throw new LitsError('Expected comma or closing brace')
      }

      if (isIF_OperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBraceToken(this.peek())
    this.advance()

    return {
      t: AstNodeType.NormalExpression,
      n: 'object',
      p: params,
      token: firstToken,
    }
  }

  private parseArray(): AstNode {
    const firstToken = asLBracketToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isIF_OperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis')
      }
      if (isIF_OperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBracketToken(this.peek())
    this.advance()

    return {
      t: AstNodeType.NormalExpression,
      n: 'array',
      p: params,
      token: firstToken,
    }
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }
}
