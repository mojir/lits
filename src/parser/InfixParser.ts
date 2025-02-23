import { LitsError } from '../errors'
import { AstNodeType } from '../constants/constants'
import { isLParenToken, isRBraceToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { IF_OperatorToken, InfixTokenType } from '../tokenizer/infix/infixTokens'
import { isIF_OperatorToken } from '../tokenizer/infix/infixTokens'
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

    case '!': // logical NOT
    case '~': // bitwise NOT
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
    case '!':
    case '~':
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

    while (!this.isAtEnd()) {
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
    if (isLParenToken(token)) {
      this.advance()
      const expression = this.parseExpression()
      if (!isRParenToken(this.peek())) {
        throw new Error('Expected closing brace')
      }
      this.advance()
      return expression
    }

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

    if (isLParenToken(token)) {
      this.advance()
      const expression = this.parseExpression()
      if (!isRParenToken(this.peek())) {
        throw new LitsError('Expected closing parenthesis')
      }
      this.advance()
      return expression
    }

    const tokenType = token[0] as InfixTokenType
    switch (tokenType) {
      case 'Number':
        return parseNumber(this.tokenStream, this.parseState)
      case 'String':
        return parseString(this.tokenStream, this.parseState)
      case 'IF_Symbol':
        return parseSymbol(this.tokenStream, this.parseState)
      case 'IF_ReservedSymbol':
        return parseReservedSymbol(this.tokenStream, this.parseState)
    }

    return this.parseState.parseToken(this.tokenStream, this.parseState)
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length || isRBraceToken(this.peek())
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }
}
