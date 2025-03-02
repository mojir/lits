import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import { builtin, specialExpressionKeys } from '../builtin'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { DefnNode, FnNode } from '../builtin/specialExpressions/functions'
import type { LetNode } from '../builtin/specialExpressions/let'
import type { ForNode, LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { Arity, FunctionArguments } from '../builtin/utils'
import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { withoutCommentNodes } from '../removeCommentNodes'
import type { A_OperatorToken, A_SymbolToken, AlgebraicTokenType, SymbolicBinaryOperator } from '../tokenizer/algebraic/algebraicTokens'
import { asA_SymbolToken, assertA_OperatorToken, isA_BinaryOperatorToken, isA_OperatorToken, isA_SymbolToken, isFunctionOperator, isSymbolicUnaryOperator } from '../tokenizer/algebraic/algebraicTokens'
import { asLBraceToken, asLBracketToken, assertEndNotationToken, assertLBraceToken, assertRBraceToken, assertRBracketToken, assertRParenToken, isEndNotationToken, isLBraceToken, isLBracketToken, isLParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { TokenStream } from '../tokenizer/interface'
import type { Token } from '../tokenizer/tokens'
import { getTokenDebugData, hasTokenDebugData } from '../tokenizer/utils'
import { asSymbolNode } from '../typeGuards/astNode'
import { arrayToPairs } from '../utils'
import { parseNumber, parseReservedSymbol, parseString, parseSymbol } from './commonTokenParsers'
import type { AstNode, BindingNode, NormalExpressionNodeWithName, ParseState, StringNode, SymbolNode } from './interface'

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
    case '>': // greater than
    case '>=': // greater than or equal
      return 5

    case '==': // equal
    case '!=': // not equal
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
    default:
      throw new Error(`Unknown binary operator: ${operatorSign satisfies never}`)
  }
}

function createNamedNormalExpressionNode(name: string, params: AstNode[], token: Token | undefined): NormalExpressionNodeWithName {
  const node: NormalExpressionNodeWithName = {
    t: AstNodeType.NormalExpression,
    n: name,
    p: params,
    token: getTokenDebugData(token) && token,
  }
  const builtinExpression = builtin.normalExpressions[node.n]

  if (builtinExpression) {
    builtinExpression.validate?.({
      ...node,
      p: withoutCommentNodes(node.p),
    })
  }

  return node
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
      return createNamedNormalExpressionNode('~', [operand], token)
    /* v8 ignore next 2 */
    default:
      throw new Error(`Unknown operator: ${operatorName}`)
  }
}

function fromBinaryOperatorToAstNode(operator: A_OperatorToken, left: AstNode, right: AstNode, token: Token | undefined): AstNode {
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
      return createNamedNormalExpressionNode('<<', [left, right], token)
    case '>>':
      return createNamedNormalExpressionNode('>>', [left, right], token)
    case '>>>':
      return createNamedNormalExpressionNode('>>>', [left, right], token)
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
      return createNamedNormalExpressionNode('!=', [left, right], token)
    case '&':
      return createNamedNormalExpressionNode('&', [left, right], token)
    case '^':
      return createNamedNormalExpressionNode('^', [left, right], token)
    case '|':
      return createNamedNormalExpressionNode('|', [left, right], token)
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
    case ';':
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

  public parse(): AstNode[] {
    const nodes: AstNode[] = []
    while (!this.isAtEnd()) {
      nodes.push(this.parseExpression())
      if (!isA_OperatorToken(this.peek(), ';')) {
        break
      }
      this.advance()
    }
    return nodes
  }

  private parseExpression(precedence = 0): AstNode {
    const firstToken = this.peek()
    if (isA_SymbolToken(firstToken) && firstToken[1] === 'def') {
      return this.parseDef(firstToken)
    }
    if (isA_SymbolToken(firstToken) && firstToken[1] === 'defn') {
      return this.parseDefn(firstToken)
    }

    let left = this.parseOperand()
    let operator = this.peek()

    while (!this.isAtEnd()
      && !isA_OperatorToken(operator, ',')
      && !isA_OperatorToken(operator, ';')
      && !isRBracketToken(operator)
      && !isRParenToken(operator)) {
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
        const token: Token | undefined = hasTokenDebugData(operator) ? operator : undefined
        left = fromBinaryOperatorToAstNode(operator, left, right, token)
      }
      else if (isA_SymbolToken(operator)) {
        if (!isFunctionOperator(operator[1])) {
          break
        }
        const newPrecedece = binaryFunctionalOperatorPrecedence
        if (newPrecedece <= precedence) {
          break
        }
        this.advance()
        const right = this.parseExpression(newPrecedece)
        const token: Token | undefined = hasTokenDebugData(operator) ? operator : undefined

        left = createNamedNormalExpressionNode(operator[1], [left, right], token)
      }
      else {
        break
      }

      operator = this.peek()
    }

    if (!left) {
      throw new LitsError('Expected operand', getTokenDebugData(this.peek())?.sourceCodeInfo)
    }

    return left
  }

  private parseOperand(): AstNode {
    let operand = this.parseOperandPart()
    let token = this.peek()
    while (isA_OperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
      if (token[1] === '.') {
        this.advance()
        const symbolToken = this.peek()
        if (!isA_SymbolToken(symbolToken)) {
          throw new LitsError('Expected symbol', getTokenDebugData(this.peek())?.sourceCodeInfo)
        }
        const stringNode: StringNode = {
          t: AstNodeType.String,
          v: symbolToken[1],
          token: getTokenDebugData(symbolToken) && symbolToken,
          p: [],
          n: undefined,
        }
        operand = createAccessorNode(operand, stringNode, token)
        this.advance()
        token = this.peek()
      }
      else if (isLBracketToken(token)) {
        this.advance()
        const expression = this.parseExpression()
        if (!isRBracketToken(this.peek())) {
          throw new LitsError('Expected closing bracket', getTokenDebugData(this.peek())?.sourceCodeInfo)
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
    else if (isA_OperatorToken(token)) {
      const operatorName = token[1]
      if (isSymbolicUnaryOperator(operatorName)) {
        this.advance()
        const operand = this.parseOperand()
        if (operand === null) {
          throw new LitsError('Expected operand', getTokenDebugData(token)?.sourceCodeInfo)
        }
        return fromUnaryAlgebraicToAstNode(token, operand)
      }

      if (operatorName === '=>') {
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
      case 'A_BasePrefixedNumber':
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
      if (key === null) {
        throw new LitsError('Expected key', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }
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
    const isNamedFunction = symbol.t === AstNodeType.Symbol
    this.advance()
    if (isNamedFunction && symbol.v === 'for') {
      return this.parseFor(symbol)
    }

    const params: AstNode[] = []
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
    if (isNamedFunction) {
      if (specialExpressionKeys.includes(symbol.v)) {
        const name: SpecialExpressionName = symbol.v as Exclude<SpecialExpressionName, 'for' | 'def' | 'defn'>
        switch (name) {
          case '??':
          case 'and':
          case 'comment':
          case 'cond':
          case 'declared?':
          case 'if':
          case 'if_not':
          case 'or':
          case 'when':
          case 'when_not':
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
          case 'defs':
          case 'if_let':
          case 'when_let':
          case 'when_first':
          case 'fn':
          case 'defns':
          case 'try':
          case 'recur':
          case 'loop':
          case 'doseq':
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

  parseLambdaFunction(): AstNode | null {
    const firstToken = this.peek()
    try {
      const { functionArguments, arity } = this.parseFunctionArguments()

      if (!isA_OperatorToken(this.peek(), '=>')) {
        return null
      }
      this.advance()

      const body = this.parseExpression()

      return {
        t: AstNodeType.SpecialExpression,
        n: 'fn',
        p: [],
        o: [{
          as: functionArguments,
          b: [body],
          a: arity,
        }],
        token: getTokenDebugData(firstToken) && firstToken,
      }
    }
    catch {
      return null
    }
  }

  parseFunctionArguments(): { functionArguments: FunctionArguments, arity: Arity } {
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
          throw new LitsError('Expected symbol', getTokenDebugData(this.peek())?.sourceCodeInfo)
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
        throw new LitsError('Expected comma or closing parenthesis', getTokenDebugData(this.peek())?.sourceCodeInfo)
      }
      if (isA_OperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }
    const arity: Arity = restArg !== undefined ? { min: args.length } : args.length
    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', getTokenDebugData(this.peek())?.sourceCodeInfo)
    }
    const letBindings = letBindingObject ? arrayToPairs(letBindingObject.p) : []
    const functionArguments: FunctionArguments = {
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
    }

    this.advance()

    return {
      functionArguments,
      arity,
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

  private parseLet(letSymbol: SymbolNode, params: AstNode[]): LetNode {
    if (params.length !== 2) {
      throw new LitsError('let expects two arguments', getTokenDebugData(letSymbol.token)?.sourceCodeInfo)
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
      token: getTokenDebugData(letSymbol.token) && letSymbol.token,
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

  private parseFor(forSymbol: SymbolNode): ForNode {
    const forLoopBindings: LoopBindingNode[] = [
      this.parseForLoopBinding(),
    ]
    let nextToken = this.peekAhead()
    while (isA_SymbolToken(nextToken) && nextToken[1] === 'of') {
      forLoopBindings.push(this.parseForLoopBinding())
      nextToken = this.peekAhead()
    }

    const expression = this.parseExpression()

    assertRParenToken(this.peek())
    this.advance()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'for',
      p: [expression],
      token: getTokenDebugData(forSymbol.token) && forSymbol.token,
      l: forLoopBindings,
    }
  }

  // export interface LoopBindingNode {
  //   b: BindingNode // Binding
  //   m: Array<'&let' | '&when' | '&while'> // Modifiers
  //   l?: BindingNode[] // Let-Bindings
  //   wn?: AstNode // When Node
  //   we?: AstNode // While Node
  // }
  private parseForLoopBinding(): LoopBindingNode {
    const bindingNode = this.parseBinding()

    if (isA_OperatorToken(this.peek(), ',')) {
      this.advance()
      return {
        b: bindingNode,
        m: [],
      }
    }

    const modifiers: Array<'&let' | '&when' | '&while'> = []
    let token = this.peek()

    if (!isA_SymbolToken(token)) {
      throw new LitsError('Expected symbol let, when or while', getTokenDebugData(token)?.sourceCodeInfo)
    }

    let letBindings: BindingNode[] | undefined
    if (token[1] === 'let') {
      modifiers.push('&let')
      letBindings = []
      this.advance()
      const letObject = this.parseObject()
      letBindings = arrayToPairs(letObject.p).map((pair) => {
        const key = pair[0] as StringNode
        const value = pair[1] as AstNode

        return {
          t: AstNodeType.Binding,
          n: key.v,
          v: value,
          p: [],
          token: getTokenDebugData(key.token) && key.token,
        }
      })
    }

    token = this.peek()
    let whenNode: AstNode | undefined
    let whileNode: AstNode | undefined
    while (
      isA_SymbolToken(token)
      && (
        (token[1] === 'when' && !modifiers.includes('&when'))
        || (token[1] === 'while' && !modifiers.includes('&while'))
      )
    ) {
      this.advance()

      if (token[1] === 'when') {
        modifiers.push('&when')
        whenNode = this.parseExpression()
      }
      else {
        modifiers.push('&while')
        whileNode = this.parseExpression()
      }
      token = this.peek()
    }

    assertA_OperatorToken(token, ',')
    this.advance()

    return {
      b: bindingNode,
      m: modifiers,
      l: letBindings,
      wn: whenNode,
      we: whileNode,
    }
  }

  private parseBinding(): BindingNode {
    const firstToken = asA_SymbolToken(this.peek())
    const name = firstToken[1]
    this.advance()

    const ofSymbol = asA_SymbolToken(this.peek())
    if (ofSymbol[1] !== 'of') {
      throw new LitsError('Expected "of"', getTokenDebugData(this.peek())?.sourceCodeInfo)
    }
    this.advance()

    const value = this.parseExpression()

    const node: BindingNode = {
      t: AstNodeType.Binding,
      n: name,
      v: value,
      p: [],
      token: getTokenDebugData(firstToken) && firstToken,
    }
    return node
  }

  parseDef(token: A_SymbolToken): DefNode {
    this.advance()
    const symbol = parseSymbol(this.tokenStream, this.parseState)
    assertA_OperatorToken(this.peek(), '=')
    this.advance()
    const value = this.parseExpression()
    return {
      t: AstNodeType.SpecialExpression,
      n: 'def',
      p: [symbol, value],
      token: getTokenDebugData(token) && token,
    }
  }

  parseDefn(token: A_SymbolToken): DefnNode {
    this.advance()
    const symbol = parseSymbol(this.tokenStream, this.parseState)
    const { functionArguments, arity } = this.parseFunctionArguments()
    assertLBraceToken(this.peek())
    this.advance()

    const body: AstNode[] = []

    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      body.push(this.parseExpression())
      if (isA_OperatorToken(this.peek(), ';')) {
        this.advance()
      }
    }
    if (!isRBraceToken(this.peek())) {
      throw new LitsError('Expected closing brace', getTokenDebugData(this.peek())?.sourceCodeInfo)
    }
    this.advance()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'defn',
      f: symbol,
      p: [],
      o: [{
        as: functionArguments,
        b: body,
        a: arity,
      }],
      token: getTokenDebugData(token) && token,
    }
  }

  private isAtEnd(): boolean {
    return this.parseState.position >= this.tokenStream.tokens.length
  }

  private peek(): Token {
    return this.tokenStream.tokens[this.parseState.position]!
  }

  private peekAhead(): Token {
    return this.tokenStream.tokens[this.parseState.position + 1]!
  }
}
