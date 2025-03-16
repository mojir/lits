import type { SpecialExpressionName } from '../builtin'
import { builtin, specialExpressionKeys } from '../builtin'
import type { CondNode } from '../builtin/specialExpressions/cond'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { DefnNode, FnNode, FunctionNode } from '../builtin/specialExpressions/functions'
import type { IfNode } from '../builtin/specialExpressions/if'
import type { LetNode } from '../builtin/specialExpressions/let'
import type { LoopNode } from '../builtin/specialExpressions/loop'
import type { DoSeqNode, ForNode, LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { SwitchNode } from '../builtin/specialExpressions/switch'
import type { TryNode } from '../builtin/specialExpressions/try'
import type { UnlessNode } from '../builtin/specialExpressions/unless'
import type { FunctionArgument } from '../builtin/utils'
import { LitsError } from '../errors'
import type { TokenStream } from '../tokenizer/tokenize'
import { type SymbolicBinaryOperator, isBinaryOperator, isFunctionOperator } from '../tokenizer/operators'
import type { OperatorToken, ReservedSymbolToken, SymbolToken, Token, TokenType } from '../tokenizer/token'
import {
  asLBraceToken,
  asLBracketToken,
  asSymbolToken,
  assertLParenToken,
  assertOperatorToken,
  assertRBraceToken,
  assertRBracketToken,
  assertRParenToken,
  assertReservedSymbolToken,
  assertSymbolToken,
  hasTokenSourceCodeInfo,
  isA_BinaryOperatorToken,
  isLBraceToken,
  isLBracketToken,
  isLParenToken,
  isOperatorToken,
  isRBraceToken,
  isRBracketToken,
  isRParenToken,
  isReservedSymbolToken,
  isSymbolToken,
  tokenSourceCodeInfo,
} from '../tokenizer/token'
import { assertNumberOfParams } from '../typeGuards'
import type { QqNode } from '../builtin/specialExpressions/qq'
import type { AndNode } from '../builtin/specialExpressions/and'
import type { DeclaredNode } from '../builtin/specialExpressions/declared'
import type { OrNode } from '../builtin/specialExpressions/or'
import type { RecurNode } from '../builtin/specialExpressions/recur'
import type { ThrowNode } from '../builtin/specialExpressions/throw'
import { isNumberReservedSymbol, numberReservedSymbolRecord } from '../tokenizer/reservedNames'
import type { AstNode, BindingNode, NormalExpressionNode, NormalExpressionNodeWithName, NumberNode, ParseState, ReservedSymbolNode, StringNode, SymbolNode } from './types'

const exponentiationPrecedence = 10
const binaryFunctionalOperatorPrecedence = 1
const placeholderRegexp = /^\$([1-9]\d?)?$/

function getPrecedence(operatorSign: SymbolicBinaryOperator): number {
  switch (operatorSign) {
    case '**': // exponentiation
      return exponentiationPrecedence

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 9

    case '+': // addition
    case '-': // subtraction
      return 8

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
      return 7

    case '++': // string concatenation
      return 6

    case '<': // less than
    case '<=': // less than or equal
    case '≤': // less than or equal
    case '>': // greater than
    case '>=': // greater than or equal
    case '≥': // greater than or equal
      return 5

    case '=': // equal
    case '!=': // not equal
    case '≠': // not equal
      return 4

    case '&': // bitwise AND
    case '^': // bitwise XOR
    case '|': // bitwise OR
      return 3

    case '&&': // logical AND
    case '||': // logical OR
    case '??': // nullish coalescing
      return 2

    // leave room for binaryFunctionalOperatorPrecedence = 1
    /* v8 ignore next 2 */
    default:
      throw new Error(`Unknown binary operator: ${operatorSign satisfies never}`)
  }
}

function createNamedNormalExpressionNode(name: string, params: AstNode[], token: Token | undefined): NormalExpressionNodeWithName {
  const node: NormalExpressionNodeWithName = {
    type: 'NormalExpression',
    name,
    params,
    token: tokenSourceCodeInfo(token) && token,
  }
  const builtinExpression = builtin.normalExpressions[node.name]

  if (builtinExpression) {
    assertNumberOfParams(builtinExpression.paramCount, node)
  }

  return node
}

function createAccessorNode(left: AstNode, right: AstNode, token: Token | undefined): AstNode {
  // Unnamed normal expression
  return {
    type: 'NormalExpression',
    params: [left, right],
    name: undefined,
    token: tokenSourceCodeInfo(token) && token,
  }
}

function fromBinaryOperatorToAstNode(operator: OperatorToken | SymbolToken<'+'>, left: AstNode, right: AstNode, token: Token | undefined): AstNode {
  const operatorName = operator[1]

  switch (operatorName) {
    case '**': // exponentiation
    case '*':
    case '/':
    case '%':
    case '+':
    case '-':
    case '<<':
    case '>>':
    case '>>>':
    case '++':
    case '<':
    case '<=':
    case '≤':
    case '>':
    case '>=':
    case '≥':
    case '=':
    case '!=':
    case '≠':
    case '&':
    case '^':
    case '|':
      return createNamedNormalExpressionNode(operatorName, [left, right], token)
    case '&&':
    case '||':
    case '??':
      return {
        type: 'SpecialExpression',
        name: operatorName,
        params: [left, right],
        token: tokenSourceCodeInfo(token) && token,
      }
    /* v8 ignore next 10 */
    case '.':
    case ';':
    case ':=':
    case ',':
    case '->':
    case '...':
      throw new LitsError(`Unknown binary operator: ${operatorName}`, tokenSourceCodeInfo(token))

    default:
      throw new LitsError(`Unknown binary operator: ${operatorName satisfies never}`, tokenSourceCodeInfo(token))
  }
}

export class Parser {
  constructor(
    private readonly tokenStream: TokenStream,
    private parseState: ParseState,
  ) {}

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }

  private peekAhead(count: number): Token {
    return this.tokenStream.tokens[this.parseState.position + count]!
  }

  private advance(): void {
    this.parseState.position += 1
  }

  public parse(): AstNode[] {
    const nodes: AstNode[] = []
    while (!this.isAtEnd()) {
      nodes.push(this.parseExpression(0, true))
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else {
        if (!this.isAtEnd()) {
          throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
        }
      }
    }
    return nodes
  }

  private parseExpression(precedence = 0, moduleScope = false): AstNode {
    const firstToken = this.peek()

    let left: AstNode

    if (isSymbolToken(firstToken)) {
      switch (firstToken[1]) {
        case 'let':
          return this.parseLet(firstToken)
        case 'if':
        case 'unless':
          left = this.parseIfOrUnless(firstToken)
          break
        case 'cond':
          left = this.parseCond(firstToken)
          break
        case 'switch':
          left = this.parseSwitch(firstToken)
          break
        case 'for':
        case 'doseq':
          left = this.parseForOrDoseq(firstToken)
          break
        case 'do':
          left = this.parseDo(firstToken)
          break
        case 'loop':
          left = this.parseLoop(firstToken)
          break
        case 'try':
          left = this.parseTry(firstToken)
          break
      }
    }
    else if (isReservedSymbolToken(firstToken, 'function')) {
      return this.parseFunction(firstToken)
    }
    else if (isReservedSymbolToken(firstToken, 'export')) {
      if (!moduleScope) {
        throw new LitsError('export is only allowed in module scope', tokenSourceCodeInfo(firstToken))
      }
      return this.parseExport(firstToken)
    }

    left ||= this.parseOperand()
    let operator = this.peek()

    while (!this.isAtExpressionEnd()) {
      if (isA_BinaryOperatorToken(operator)) {
        const name = operator[1]
        const newPrecedece = getPrecedence(name)
        if (
          newPrecedece <= precedence
          // ** (exponentiation) is right associative
          && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
          break
        }
        this.advance()
        const right = this.parseExpression(newPrecedece)
        const token: Token | undefined = hasTokenSourceCodeInfo(operator) ? operator : undefined
        left = fromBinaryOperatorToAstNode(operator, left, right, token)
      }
      else if (isSymbolToken(operator)) {
        if (!isFunctionOperator(operator[1])) {
          break
        }
        const newPrecedece = binaryFunctionalOperatorPrecedence
        if (newPrecedece <= precedence) {
          break
        }
        this.advance()
        const right = this.parseExpression(newPrecedece)
        const token: Token | undefined = hasTokenSourceCodeInfo(operator) ? operator : undefined

        left = createNamedNormalExpressionNode(operator[1], [left, right], token)
      }
      else {
        break
      }

      operator = this.peek()
    }

    return left
  }

  private parseOperand(): AstNode {
    let operand = this.parseOperandPart()
    let token = this.peek()
    while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
      if (token[1] === '.') {
        this.advance()
        const symbolToken = this.peek()
        if (!isSymbolToken(symbolToken)) {
          throw new LitsError('Expected symbol', tokenSourceCodeInfo(this.peek()))
        }
        const stringNode: StringNode = {
          type: 'String',
          value: symbolToken[1],
          token: tokenSourceCodeInfo(symbolToken) && symbolToken,
          params: [],
          name: undefined,
        }
        operand = createAccessorNode(operand, stringNode, token)
        this.advance()
        token = this.peek()
      }
      else if (isLBracketToken(token)) {
        this.advance()
        const expression = this.parseExpression()
        if (!isRBracketToken(this.peek())) {
          throw new LitsError('Expected closing bracket', tokenSourceCodeInfo(this.peek()))
        }
        operand = createAccessorNode(operand, expression, token)
        this.advance()
        token = this.peek()
      }
      else if (isLParenToken(token)) {
        operand = this.parseFunctionCall(operand)
        token = this.peek()
      }
    }
    return operand
  }

  private parseOperandPart(): AstNode {
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
    else if (isOperatorToken(token)) {
      const operatorName = token[1]
      if (isBinaryOperator(operatorName)) {
        this.advance()
        return {
          type: 'Symbol',
          value: operatorName,
          token: tokenSourceCodeInfo(token) && token,
          params: [],
          name: undefined,
        } satisfies SymbolNode
      }

      if (operatorName === '->') {
        return this.parseShorthandLamdaFunction()
      }
      else {
        throw new LitsError(`Illegal operator: ${operatorName}`, tokenSourceCodeInfo(token))
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
      TokenType,
      | 'Operator' // Handled above
      | 'LParen' // Handled above
      | 'LBrace' // Handled above
      | 'LBracket' // Handled above

      | 'RParen' // Illegal token
      | 'RBrace' // Illegal token
      | 'RBracket' // Illegal token

      | 'MultiLineComment' // Should have been removed
      | 'SingleLineComment' // Should have been removed
      | 'Whitespace' // Should have been removed
    >
    switch (tokenType) {
      case 'Number':
      case 'BasePrefixedNumber':
        return this.parseNumber()
      case 'String':
        return this.parseString()
      case 'Symbol': {
        const positionBefore = this.parseState.position
        const lamdaFunction = this.parseLambdaFunction()
        if (lamdaFunction) {
          return lamdaFunction
        }
        this.parseState.position = positionBefore
        return this.parseSymbol()
      }
      case 'ReservedSymbol':
        return this.parseReservedSymbol()
      case 'RegexpShorthand':
        return this.parseRegexpShorthand()

      default:
        throw new LitsError(`Unknown token type: ${tokenType}`, tokenSourceCodeInfo(token))
    }
  }

  private parseObject(): AstNode {
    const firstToken = asLBraceToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      const key = this.parseOperand()
      if (key.type !== 'Symbol' && key.type !== 'String') {
        throw new LitsError('Expected key to be a symbol or a string', tokenSourceCodeInfo(this.peek()))
      }

      params.push({
        type: 'String',
        value: key.value,
        token: tokenSourceCodeInfo(key.token) && key.token,
        params: [],
        name: undefined,
      })

      assertOperatorToken(this.peek(), ':=')
      this.advance()

      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
        throw new LitsError('Expected comma or closing brace', tokenSourceCodeInfo(this.peek()))
      }

      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBraceToken(this.peek())
    this.advance()

    return {
      type: 'NormalExpression',
      name: 'object',
      params,
      token: tokenSourceCodeInfo(firstToken) && firstToken,
    }
  }

  private parseArray(): AstNode {
    const firstToken = asLBracketToken(this.peek())
    this.advance()
    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', tokenSourceCodeInfo(this.peek()))
      }
      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBracketToken(this.peek())
    this.advance()

    return {
      type: 'NormalExpression',
      name: 'array',
      params,
      token: tokenSourceCodeInfo(firstToken) && firstToken,
    }
  }

  private parseFunctionCall(symbol: AstNode): AstNode {
    const isNamedFunction = symbol.type === 'Symbol'
    this.advance()

    const params: AstNode[] = []
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      params.push(this.parseExpression())
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', tokenSourceCodeInfo(this.peek()))
      }
      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }
    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', tokenSourceCodeInfo(this.peek()))
    }
    this.advance()
    if (isNamedFunction) {
      if (specialExpressionKeys.includes(symbol.value)) {
        const name: SpecialExpressionName = symbol.value as Exclude<SpecialExpressionName, 'for' | 'if' | 'unless' | 'cond' | 'switch' | 'let' | 'do' | 'loop' | 'try' | 'doseq' | 'function'>
        switch (name) {
          case '??':
          case '&&':
          case 'defined?':
          case '||':
          case 'recur':
          case 'throw': {
            const node: QqNode | AndNode | DeclaredNode | OrNode | RecurNode | ThrowNode = {
              type: 'SpecialExpression',
              name,
              params,
              token: tokenSourceCodeInfo(symbol.token) && symbol.token,
            }
            assertNumberOfParams(builtin.specialExpressions[node.name].paramCount, node)
            return node
          }
          case 'fn':
          case 'def':
          case 'defn':
            throw new Error(`${name} is not allowed`)
          /* v8 ignore next 2 */
          default:
            throw new Error(`Unknown special expression: ${name satisfies never}`)
        }
      }
      return createNamedNormalExpressionNode(symbol.value, params, symbol.token)
    }
    else {
      return {
        type: 'NormalExpression',
        name: undefined,
        params: [symbol, ...params],
        token: tokenSourceCodeInfo(symbol.token) && symbol.token,
      }
    }
  }

  parseLambdaFunction(): AstNode | null {
    const firstToken = this.peek()

    if (isLParenToken(firstToken)
      && isSymbolToken(this.peekAhead(1))
      && isOperatorToken(this.peekAhead(2), '->')) {
      return null
    }

    try {
      const { functionArguments, bindingNodes } = this.parseFunctionArguments()

      if (!isOperatorToken(this.peek(), '->')) {
        return null
      }
      this.advance()

      const body = this.parseExpression()

      return {
        type: 'SpecialExpression',
        name: 'fn',
        params: [],
        function: {
          arguments: functionArguments,
          bindingNodes,
          body: [body],
        },
        token: tokenSourceCodeInfo(firstToken) && firstToken,
      }
    }
    catch {
      return null
    }
  }

  parseFunctionArguments(): { functionArguments: FunctionArgument[], bindingNodes: BindingNode[] } {
    const firstToken = this.peek()
    if (isSymbolToken(firstToken)) {
      this.advance()
      return {
        functionArguments: [{
          name: firstToken[1],
        } satisfies FunctionArgument],
        bindingNodes: [],
      }
    }

    assertLParenToken(firstToken)
    this.advance()

    // let bindings, to be able to pass on values in the context down to the body
    // This is needed since lits is dynamically scoped
    // E.g.
    // x => y => x + y // would not work, x is not available in the second lambda
    // x => (y, let x = x) => x + y // would work, x is available in the second lambda
    const bindingNodes: BindingNode[] = []
    let token = this.peek()
    while (isSymbolToken(token, 'let')) {
      const letNode = this.parseLet(token, true)
      bindingNodes.push(letNode.bindingNodes[0]!)
      if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek())) {
        throw new LitsError('Expected comma or closing parenthesis', tokenSourceCodeInfo(this.peek()))
      }
      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
      token = this.peek()
    }

    let rest = false
    let defaults = false
    const functionArguments: FunctionArgument[] = []
    while (!this.isAtEnd() && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
      if (rest) {
        throw new LitsError('Rest argument must be last', tokenSourceCodeInfo(this.peek()))
      }
      if (isOperatorToken(this.peek(), '...')) {
        this.advance()
        rest = true
      }
      const symbolToken = this.peek()
      if (!isSymbolToken(symbolToken)) {
        throw new LitsError('Expected symbol', tokenSourceCodeInfo(this.peek()))
      }
      const name = symbolToken[1]
      this.advance()

      let isDefault: AstNode | undefined
      if (isOperatorToken(this.peek(), ':=')) {
        if (rest) {
          throw new LitsError('Optional arguments must be last', tokenSourceCodeInfo(this.peek()))
        }
        defaults = true
        this.advance()
        isDefault = this.parseExpression()
      }
      else if (defaults) {
        throw new LitsError('Optional arguments must be last', tokenSourceCodeInfo(this.peek()))
      }

      functionArguments.push({
        name,
        default: isDefault,
        rest: rest ? true : undefined,
      })

      if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
        throw new LitsError('Expected comma or closing parenthesis', tokenSourceCodeInfo(this.peek()))
      }
      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }

    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', tokenSourceCodeInfo(this.peek()))
    }

    this.advance()

    return {
      functionArguments,
      bindingNodes,
    }
  }

  private parseShorthandLamdaFunction(): FnNode {
    const firstToken = this.peek()
    this.advance()
    const startPos = this.parseState.position
    const exprNode = this.parseExpression()
    const endPos = this.parseState.position - 1

    let arity = 0
    let dollar1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET' // referring to argument bindings. $ = NAKED, $1, $2, $3, etc = WITH_1
    for (let pos = startPos; pos <= endPos; pos += 1) {
      const token = this.tokenStream.tokens[pos]!
      if (isSymbolToken(token)) {
        const match = placeholderRegexp.exec(token[1])
        if (match) {
          const number = match[1] ?? '1'
          if (number === '1') {
            const mixedPercent1 = (!match[1] && dollar1 === 'WITH_1') || (match[1] && dollar1 === 'NAKED')
            if (mixedPercent1)
              throw new LitsError('Please make up your mind, either use $ or $1', tokenSourceCodeInfo(firstToken))

            dollar1 = match[1] ? 'WITH_1' : 'NAKED'
          }

          arity = Math.max(arity, Number(number))
          if (arity > 20)
            throw new LitsError('Can\'t specify more than 20 arguments', tokenSourceCodeInfo(firstToken))
        }
      }
    }

    const functionArguments: FunctionArgument[] = []

    for (let i = 1; i <= arity; i += 1) {
      if (i === 1 && dollar1 === 'NAKED')
        functionArguments.push({ name: '$' })
      else
        functionArguments.push({ name: `$${i}` })
    }

    const node: FnNode = {
      type: 'SpecialExpression',
      name: 'fn',
      params: [],
      function: {
        arguments: functionArguments,
        bindingNodes: [],
        body: [exprNode],
      },

      token: tokenSourceCodeInfo(firstToken) && firstToken,
    }

    return node
  }

  private parseLet(token: SymbolToken, optionalSemicolon = false): LetNode {
    this.advance()

    const letSymbol = this.parseSymbol()

    assertOperatorToken(this.peek(), ':=')
    this.advance()

    const value = this.parseExpression()

    if (!optionalSemicolon) {
      assertOperatorToken(this.peek(), ';')
    }

    return {
      type: 'SpecialExpression',
      name: 'let',
      params: [],
      bindingNodes: [{
        type: 'Binding',
        name: letSymbol.value,
        value,
        params: [],
        token: tokenSourceCodeInfo(token) && token,
      }],
      token: tokenSourceCodeInfo(letSymbol.token) && letSymbol.token,
    }
  }

  private parseDo(token: SymbolToken): DoNode {
    this.advance()
    const expressions: AstNode[] = []
    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      expressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }
    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()
    return {
      type: 'SpecialExpression',
      name: 'do',
      params: expressions,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  private parseLoop(firstToken: SymbolToken): LoopNode {
    this.advance()

    const bindingNodes: BindingNode[] = []
    let token = this.peek()
    while (!this.isAtEnd() && !isSymbolToken(token, 'do')) {
      assertSymbolToken(token, 'let')
      this.advance()

      const symbol = this.parseSymbol()
      assertOperatorToken(this.peek(), ':=')
      this.advance()
      const value = this.parseExpression()
      bindingNodes.push({
        type: 'Binding',
        name: symbol.value,
        value,
        params: [],
        token: tokenSourceCodeInfo(symbol.token) && symbol.token,
      } satisfies BindingNode)

      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
      token = this.peek()
    }
    if (bindingNodes.length === 0) {
      throw new LitsError('Expected binding', tokenSourceCodeInfo(this.peek()))
    }

    assertSymbolToken(token, 'do')
    this.advance()

    const params: AstNode[] = []
    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      params.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }
    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      type: 'SpecialExpression',
      name: 'loop',
      params,
      bindingNodes,
      token: tokenSourceCodeInfo(firstToken) && firstToken,
    }
  }

  private parseTry(token: SymbolToken): TryNode {
    this.advance()
    const tryExpressions: AstNode[] = []
    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'catch')) {
      tryExpressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'catch')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }

    const tryExpression = tryExpressions.length === 1
      ? tryExpressions[0]!
      : {
        type: 'SpecialExpression',
        name: 'do',
        params: tryExpressions,
        token: tokenSourceCodeInfo(token) && token,
      } satisfies DoNode

    assertReservedSymbolToken(this.peek(), 'catch')
    this.advance()

    let errorSymbol: SymbolNode | undefined
    if (isLParenToken(this.peek())) {
      this.advance()
      errorSymbol = this.parseSymbol()
      assertRParenToken(this.peek())
      this.advance()
    }

    const catchExpressions: AstNode[] = []
    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      catchExpressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    const catchExpression = catchExpressions.length === 1
      ? catchExpressions[0]!
      : {
        type: 'SpecialExpression',
        name: 'do',
        params: catchExpressions,
        token: tokenSourceCodeInfo(token) && token,
      } satisfies DoNode

    return {
      type: 'SpecialExpression',
      name: 'try',
      params: [tryExpression],
      ce: catchExpression,
      e: errorSymbol,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  private parseForOrDoseq(firstToken: SymbolToken): ForNode | DoSeqNode {
    const isDoseq = firstToken[1] === 'doseq'
    this.advance()

    const forLoopBindings: LoopBindingNode[] = []

    while (!this.isAtEnd() && !isSymbolToken(this.peek(), 'do')) {
      const loopBinding = this.parseForLoopBinding()
      if (forLoopBindings.some(b => b.b.name === loopBinding.b.name)) {
        throw new LitsError('Duplicate binding', tokenSourceCodeInfo(loopBinding.b.token))
      }
      forLoopBindings.push(loopBinding)
    }

    assertSymbolToken(this.peek(), 'do')
    this.advance()

    const expressions: AstNode[] = []

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      expressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      type: 'SpecialExpression',
      name: isDoseq ? 'doseq' : 'for',
      params: expressions,
      token: tokenSourceCodeInfo(firstToken) && firstToken,
      l: forLoopBindings,
    }
  }

  private parseForLoopBinding(): LoopBindingNode {
    assertReservedSymbolToken(this.peek(), 'each')

    this.advance()

    const bindingNode = this.parseBinding()

    const modifiers: Array<'&let' | '&when' | '&while'> = []
    let token = this.peek()

    if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
      throw new LitsError('Expected do, each or comma', tokenSourceCodeInfo(token))
    }
    if (isOperatorToken(token, ',')) {
      this.advance()
      token = this.peek()
    }

    if (!isSymbolToken(token, 'let')
      && !isReservedSymbolToken(token, 'when')
      && !isReservedSymbolToken(token, 'while')
      && !isSymbolToken(token, 'do')
      && !isReservedSymbolToken(token, 'each')
    ) {
      throw new LitsError('Expected symbol each, do, let, when or while', tokenSourceCodeInfo(token))
    }

    let letBindings: BindingNode[] | undefined
    if (token[1] === 'let') {
      modifiers.push('&let')
      letBindings = []
      while (isSymbolToken(token, 'let')) {
        const letNode = this.parseLet(token, true)
        if (letBindings.some(b => b.name === letNode.bindingNodes[0]!.name)) {
          throw new LitsError('Duplicate binding', tokenSourceCodeInfo(letNode.bindingNodes[0]!.token))
        }
        letBindings.push(letNode.bindingNodes[0]!)
        token = this.peek()
        if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
          throw new LitsError('Expected do, each or comma', tokenSourceCodeInfo(token))
        }
        if (isOperatorToken(token, ',')) {
          this.advance()
        }
        token = this.peek()
      }
    }

    let whenNode: AstNode | undefined
    let whileNode: AstNode | undefined
    while (
      isReservedSymbolToken(token, 'when')
      || isReservedSymbolToken(token, 'while')
    ) {
      this.advance()

      if (token[1] === 'when') {
        if (modifiers.includes('&when')) {
          throw new LitsError('Multiple when modifiers in for loop', tokenSourceCodeInfo(token))
        }
        modifiers.push('&when')
        whenNode = this.parseExpression()
      }
      else {
        if (modifiers.includes('&while')) {
          throw new LitsError('Multiple while modifiers in for loop', tokenSourceCodeInfo(token))
        }
        modifiers.push('&while')
        whileNode = this.parseExpression()
      }
      token = this.peek()
      if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
        throw new LitsError('Expected do or comma', tokenSourceCodeInfo(token))
      }
      if (isOperatorToken(token, ',')) {
        this.advance()
      }
      token = this.peek()
    }

    if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each')) {
      throw new LitsError('Expected do or each', tokenSourceCodeInfo(token))
    }

    return {
      b: bindingNode,
      m: modifiers,
      l: letBindings,
      wn: whenNode,
      we: whileNode,
    }
  }

  private parseBinding(): BindingNode {
    const firstToken = asSymbolToken(this.peek())
    const name = firstToken[1]
    this.advance()

    assertReservedSymbolToken(this.peek(), 'in')
    this.advance()

    const value = this.parseExpression()

    const node: BindingNode = {
      type: 'Binding',
      name,
      value,
      params: [],
      token: tokenSourceCodeInfo(firstToken) && firstToken,
    }
    return node
  }

  parseIfOrUnless(token: SymbolToken): IfNode | UnlessNode {
    const isUnless = token[1] === 'unless'
    this.advance()
    const condition = this.parseExpression()
    assertReservedSymbolToken(this.peek(), 'then')
    this.advance()
    const thenExpressions: AstNode[] = []
    while (
      !this.isAtEnd()
      && !isReservedSymbolToken(this.peek(), 'else')
      && !isReservedSymbolToken(this.peek(), 'end')
    ) {
      thenExpressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'else') && !isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }

    const thenExpression = thenExpressions.length === 1
      ? thenExpressions[0]!
      : {
        type: 'SpecialExpression',
        name: 'do',
        params: thenExpressions,
        token: tokenSourceCodeInfo(token) && token,
      } satisfies DoNode

    let elseExpression: AstNode | undefined
    if (isReservedSymbolToken(this.peek(), 'else')) {
      this.advance()
      const elseExpressions: AstNode[] = []
      while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
        elseExpressions.push(this.parseExpression())
        if (isOperatorToken(this.peek(), ';')) {
          this.advance()
        }
        else if (!isReservedSymbolToken(this.peek(), 'end')) {
          throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
        }
      }

      elseExpression = elseExpressions.length === 1
        ? elseExpressions[0]
        : {
          type: 'SpecialExpression',
          name: 'do',
          params: elseExpressions,
          token: tokenSourceCodeInfo(token) && token,
        } satisfies DoNode
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    const params = [condition, thenExpression]
    if (elseExpression) {
      params.push(elseExpression)
    }

    return {
      type: 'SpecialExpression',
      name: isUnless ? 'unless' : 'if',
      params,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  parseCond(token: SymbolToken): CondNode {
    this.advance()
    const params: AstNode[] = []

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      assertReservedSymbolToken(this.peek(), 'case')
      this.advance()
      params.push(this.parseExpression())
      assertReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const expressions: AstNode[] = []
      while (
        !this.isAtEnd()
        && !isReservedSymbolToken(this.peek(), 'case')
        && !isReservedSymbolToken(this.peek(), 'end')) {
        expressions.push(this.parseExpression())
        if (isOperatorToken(this.peek(), ';')) {
          this.advance()
        }
        else if (!isReservedSymbolToken(this.peek(), 'case') && !isReservedSymbolToken(this.peek(), 'end')) {
          throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
        }
      }

      params.push(
        expressions.length === 1
          ? expressions[0]!
          : {
            type: 'SpecialExpression',
            name: 'do',
            params: expressions,
            token: tokenSourceCodeInfo(token) && token,
          } satisfies DoNode,
      )
      if (isReservedSymbolToken(this.peek(), 'end')) {
        break
      }
      assertReservedSymbolToken(this.peek(), 'case')
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      type: 'SpecialExpression',
      name: 'cond',
      params,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  parseSwitch(token: SymbolToken): SwitchNode {
    this.advance()
    const params: AstNode[] = [this.parseExpression()]

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      assertReservedSymbolToken(this.peek(), 'case')
      this.advance()
      params.push(this.parseExpression())
      assertReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const expressions: AstNode[] = []
      while (
        !this.isAtEnd()
        && !isReservedSymbolToken(this.peek(), 'case')
        && !isReservedSymbolToken(this.peek(), 'end')) {
        expressions.push(this.parseExpression())
        if (isOperatorToken(this.peek(), ';')) {
          this.advance()
        }
        else if (!isReservedSymbolToken(this.peek(), 'case') && !isReservedSymbolToken(this.peek(), 'end')) {
          throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
        }
      }

      params.push(
        expressions.length === 1
          ? expressions[0]!
          : {
            type: 'SpecialExpression',
            name: 'do',
            params: expressions,
            token: tokenSourceCodeInfo(token) && token,
          } satisfies DoNode,
      )
      if (isReservedSymbolToken(this.peek(), 'end')) {
        break
      }
      assertReservedSymbolToken(this.peek(), 'case')
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      type: 'SpecialExpression',
      name: 'switch',
      params,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  parseFunction(token: ReservedSymbolToken<'function'>): FunctionNode {
    this.advance()
    const symbol = this.parseSymbol()
    const { functionArguments, bindingNodes } = this.parseFunctionArguments()

    const body: AstNode[] = []

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      body.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isReservedSymbolToken(this.peek(), 'end')) {
        throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
      }
    }
    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()
    assertOperatorToken(this.peek(), ';')

    return {
      type: 'SpecialExpression',
      name: 'function',
      functionName: symbol,
      params: [],
      function: {
        arguments: functionArguments,
        bindingNodes,
        body,
      },
      token: tokenSourceCodeInfo(token) && token,
    } satisfies FunctionNode
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length
  }

  private isAtExpressionEnd(): boolean {
    if (this.isAtEnd()) {
      return true
    }
    const token = this.peek()
    if (isOperatorToken(token)) {
      return [';', ','].includes(token[1])
    }
    if (isReservedSymbolToken(token)) {
      return ['else', 'when', 'while', 'then', 'end', 'case', 'catch'].includes(token[1])
    }
    return false
  }

  private parseExport(token: ReservedSymbolToken<'export'>): DefNode | DefnNode {
    this.advance()
    if (isSymbolToken(this.peek(), 'let')) {
      this.advance()
      const symbol = this.parseSymbol()

      assertOperatorToken(this.peek(), ':=')
      this.advance()

      const value = this.parseExpression()

      assertOperatorToken(this.peek(), ';')
      return {
        type: 'SpecialExpression',
        name: 'def',
        params: [symbol, value],
        token: tokenSourceCodeInfo(symbol.token) && symbol.token,
      }
    }
    else if (isReservedSymbolToken(this.peek(), 'function')) {
      this.advance()
      const symbol = this.parseSymbol()

      const { functionArguments, bindingNodes } = this.parseFunctionArguments()

      const body: AstNode[] = []

      while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
        body.push(this.parseExpression())
        if (isOperatorToken(this.peek(), ';')) {
          this.advance()
        }
        else if (!isReservedSymbolToken(this.peek(), 'end')) {
          throw new LitsError('Expected ;', tokenSourceCodeInfo(this.peek()))
        }
      }
      assertReservedSymbolToken(this.peek(), 'end')
      this.advance()
      return {
        type: 'SpecialExpression',
        name: 'defn',
        functionName: symbol,
        params: [],
        function: {
          arguments: functionArguments,
          bindingNodes,
          body,
        },
        token: tokenSourceCodeInfo(token) && token,
      }
    }
    else {
      throw new LitsError('Expected let or function', tokenSourceCodeInfo(this.peek()))
    }
  }

  private parseSymbol(): SymbolNode {
    const token = this.peek()
    this.advance()
    if (!isSymbolToken(token)) {
      throw new LitsError(`Expected symbol token, got ${token[0]}`, tokenSourceCodeInfo(token))
    }
    if (token[1][0] !== '\'') {
      return {
        type: 'Symbol',
        value: token[1],
        params: [],
        name: undefined,
        token: tokenSourceCodeInfo(token) && token,
      }
    }
    else {
      const value = token[1].substring(1, token[1].length - 1)
        .replace(
          /(\\{2})|(\\')|\\(.)/g,
          (
            _,
            backslash: string,
            singleQuote: string,
            normalChar: string,
          ) => {
            if (backslash) {
              return '\\'
            }
            if (singleQuote) {
              return '\''
            }
            return `\\${normalChar}`
          },
        )
      return {
        type: 'Symbol',
        value,
        params: [],
        name: undefined,
        token: tokenSourceCodeInfo(token) && token,
      }
    }
  }

  private parseReservedSymbol(): ReservedSymbolNode | NumberNode {
    const token = this.peek()
    this.advance()

    if (isReservedSymbolToken(token)) {
      const symbol = token[1]
      if (isNumberReservedSymbol(symbol)) {
        return {
          type: 'Number',
          value: numberReservedSymbolRecord[symbol],
          params: [],
          name: undefined,
          token: tokenSourceCodeInfo(token) && token,
        }
      }
    }
    return {
      type: 'ReservedSymbol',
      value: token[1],
      params: [],
      name: undefined,
      token: tokenSourceCodeInfo(token) && token,
    } satisfies ReservedSymbolNode
  }

  private parseNumber(): NumberNode {
    const token = this.peek()
    this.advance()

    const value = token[1]
    const negative = value[0] === '-'
    const numberString = (negative ? value.substring(1) : value).replace(/_/g, '')
    return {
      type: 'Number',
      value: negative ? -Number(numberString) : Number(numberString),
      params: [],
      name: undefined,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  private parseString(): StringNode {
    const token = this.peek()
    this.advance()
    const value = token[1].substring(1, token[1].length - 1)
      .replace(
        /(\\{2})|(\\")|(\\n)|(\\t)|(\\r)|(\\b)|(\\f)|\\(.)/g,
        (
          _,
          backslash: string,
          doubleQuote: string,
          newline: string,
          tab: string,
          carriageReturn: string,
          backspace: string,
          formFeed: string,
          normalChar: string,
        ) => {
          // If it's a double escape (\\x), return \x
          if (backslash) {
            return '\\'
          }
          // If it's a special character (\n, \t, \r, \b, \f), return the special character
          else if (newline) {
            return '\n'
          }
          else if (tab) {
            return '\t'
          }
          else if (carriageReturn) {
            return '\r'
          }
          else if (backspace) {
            return '\b'
          }
          else if (formFeed) {
            return '\f'
          }
          else if (doubleQuote) {
            return '"'
          }
          return normalChar
        },
      )

    return {
      type: 'String',
      value,
      params: [],
      name: undefined,
      token: tokenSourceCodeInfo(token) && token,
    }
  }

  private parseRegexpShorthand(): NormalExpressionNodeWithName {
    const token = this.peek()
    this.advance()

    const endStringPosition = token[1].lastIndexOf('"')
    const regexpString = token[1].substring(2, endStringPosition)
    const optionsString = token[1].substring(endStringPosition + 1)
    const stringNode: StringNode = {
      type: 'String',
      value: regexpString,
      params: [],
      name: undefined,
      token: tokenSourceCodeInfo(token) && token,
    }

    const optionsNode: StringNode = {
      type: 'String',
      value: optionsString,
      params: [],
      name: undefined,
      token: tokenSourceCodeInfo(token) && token,
    }

    const node: NormalExpressionNode = {
      type: 'NormalExpression',
      name: 'regexp',
      params: [stringNode, optionsNode],
      token: tokenSourceCodeInfo(token) && token,
    }

    return node
  }
}
