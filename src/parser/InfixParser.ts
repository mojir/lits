import type { TokenStream } from '../tokenizer/interface'
import type { IF_OperatorToken, Token } from '../tokenizer/Token'
import { isIF_OperatorToken, isLParenToken, isRBraceToken, isRParenToken } from '../tokenizer/Token'
import { parseNumber } from './commonTokenParsers'
import type { AstNode, ParseState, ParseToken } from './interface'

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
    default:
      throw new Error(`Unknown operator: ${operatorSign satisfies never}`)
  }
}

function fromBinaryInfixToAstNode(operator: IF_OperatorToken, left: AstNode, right: AstNode): AstNode {

}

export class InfixParser {
  constructor(
    private readonly tokenStream: TokenStream,
    private parseState: ParseState,
    private readonly parseToken: ParseToken,
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
      if (newPrecedece <= precedence) {
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

    const tokenType = token[0]
    switch (tokenType) {
      case 'Number':
        return parseNumber(this.tokenStream, this.parseState)
      case 'String':
        return parseString(this.tokenStream, this.parseState)
    }

    return this.parseToken(this.tokenStream, this.parseState)
  }

  private isAtEnd(): boolean {
    return this.position >= this.tokenStream.tokens.length || isRBraceToken(this.peek())
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.position]!
  }
}
