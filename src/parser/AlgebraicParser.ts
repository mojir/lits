import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import { builtin, specialExpressionKeys } from '../builtin'
import type { CondNode } from '../builtin/specialExpressions/cond'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { DoNode } from '../builtin/specialExpressions/do'
import type { DefnNode, FnNode } from '../builtin/specialExpressions/functions'
import type { IfNode } from '../builtin/specialExpressions/if'
import type { LetNode } from '../builtin/specialExpressions/let'
import type { ForNode, LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { SwitchNode } from '../builtin/specialExpressions/switch'
import type { Arity, FunctionArguments } from '../builtin/utils'
import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { withoutCommentNodes } from '../removeCommentNodes'
import type { A_OperatorToken, A_SymbolToken, AlgebraicTokenType, SymbolicBinaryOperator } from '../tokenizer/algebraic/algebraicTokens'
import { asA_SymbolToken, assertA_OperatorToken, assertA_ReservedSymbolToken, isA_BinaryOperatorToken, isA_OperatorToken, isA_ReservedSymbolToken, isA_SymbolToken, isFunctionOperator, isSymbolicUnaryOperator } from '../tokenizer/algebraic/algebraicTokens'
import { asLBraceToken, asLBracketToken, assertEndNotationToken, assertLParenToken, assertRBraceToken, assertRBracketToken, assertRParenToken, isEndNotationToken, isLBraceToken, isLBracketToken, isLParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
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
    case '-':
    case '!':
    case '~':
      return createNamedNormalExpressionNode(operatorName, [operand], token)
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
    case '>':
    case '>=':
    case '==':
    case '!=':
    case '&':
    case '^':
    case '|':
      return createNamedNormalExpressionNode(operatorName, [left, right], token)
    case '&&':
    case '||':
    case '??':
      return {
        t: AstNodeType.SpecialExpression,
        n: operatorName,
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
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'defn') {
      return this.parseDefn(firstToken)
    }
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'let') {
      return this.parseLet(firstToken)
    }

    let left: AstNode
    if (isA_SymbolToken(firstToken) && firstToken[1] === 'if') {
      left = this.parseIf(firstToken)
    }
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'cond') {
      left = this.parseCond(firstToken)
    }
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'switch') {
      left = this.parseSwitch(firstToken)
    }
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'for') {
      left = this.parseFor(firstToken)
    }
    else if (isA_SymbolToken(firstToken) && firstToken[1] === 'do') {
      left = this.parseDo(firstToken)
    }

    else {
      left = this.parseOperand()
    }
    let operator = this.peek()

    while (!this.isAtEnd()
      && !isA_OperatorToken(operator, ',')
      && !isA_OperatorToken(operator, ';')
      && !isRBracketToken(operator)
      && !isA_ReservedSymbolToken(operator, 'else')
      && !isA_ReservedSymbolToken(operator, 'when')
      && !isA_ReservedSymbolToken(operator, 'while')
      && !isA_ReservedSymbolToken(operator, 'then')
      && !isA_ReservedSymbolToken(operator, 'end')
      && !isA_ReservedSymbolToken(operator, 'case')
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
        const name: SpecialExpressionName = symbol.v as Exclude<SpecialExpressionName, 'for' | 'def' | 'defn' | 'if' | 'cond' | 'switch' | 'let' | 'do'>
        switch (name) {
          case '??':
          case '&&':
          case 'comment':
          case 'declared?':
          case 'unless':
          case '||':
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
          case 'defs':
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

  private parseLet(token: A_SymbolToken): LetNode {
    this.advance()

    const letSymbol = parseSymbol(this.tokenStream, this.parseState)

    assertA_OperatorToken(this.peek(), '=')
    this.advance()

    const value = this.parseExpression()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      p: [],
      token: getTokenDebugData(letSymbol.token) && letSymbol.token,
      bs: [{
        t: AstNodeType.Binding,
        n: letSymbol.v,
        v: value,
        p: [],
        token: getTokenDebugData(token) && token,
      }],
    }
  }

  private parseDo(token: A_SymbolToken): DoNode {
    this.advance()
    const expressions: AstNode[] = []
    while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
      expressions.push(this.parseExpression())
      if (isA_OperatorToken(this.peek(), ';')) {
        this.advance()
      }
    }
    assertA_ReservedSymbolToken(this.peek(), 'end')
    this.advance()
    return {
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: expressions,
      token: getTokenDebugData(token) && token,
    }
  }

  private parseFor(token: A_SymbolToken): ForNode {
    this.advance()
    assertLParenToken(this.peek())
    this.advance()
    const forLoopBindings: LoopBindingNode[] = [
      this.parseForLoopBinding(),
    ]
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      forLoopBindings.push(this.parseForLoopBinding())
      if (isA_OperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }
    assertRParenToken(this.peek())
    this.advance()

    const expression = this.parseExpression()

    assertA_ReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'for',
      p: [expression],
      token: getTokenDebugData(token) && token,
      l: forLoopBindings,
    }
  }

  private parseForLoopBinding(): LoopBindingNode {
    const bindingNode = this.parseBinding()

    if (isRParenToken(this.peek()) || isA_OperatorToken(this.peek(), ',')) {
      if (isA_OperatorToken(this.peek(), ',')) {
        this.advance()
      }
      return {
        b: bindingNode,
        m: [],
      }
    }

    const modifiers: Array<'&let' | '&when' | '&while'> = []
    let token = this.peek()

    if (!(
      (isA_SymbolToken(token) && token[1] === 'let')
      || isA_ReservedSymbolToken(token, 'when')
      || isA_ReservedSymbolToken(token, 'while')
    )) {
      throw new LitsError('Expected symbol let, when or while', getTokenDebugData(token)?.sourceCodeInfo)
    }

    let letBindings: BindingNode[] | undefined
    if (token[1] === 'let') {
      modifiers.push('&let')
      letBindings = []
      while (isA_SymbolToken(token) && token[1] === 'let') {
        const letNode = this.parseLet(token)
        letBindings.push(letNode.bs[0]!)
        token = this.peek()
      }
    }

    token = this.peek()
    let whenNode: AstNode | undefined
    let whileNode: AstNode | undefined
    while (
      isA_ReservedSymbolToken(token, 'when')
      || isA_ReservedSymbolToken(token, 'while')
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

    if (isA_OperatorToken(token, ',')) {
      this.advance()
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

  parseIf(token: A_SymbolToken): IfNode {
    this.advance()
    const condition = this.parseExpression()
    assertA_ReservedSymbolToken(this.peek(), 'then')
    this.advance()
    const thenExpressions: AstNode[] = []
    while (
      !this.isAtEnd()
      && !isA_ReservedSymbolToken(this.peek(), 'else')
      && !isA_ReservedSymbolToken(this.peek(), 'end')
    ) {
      thenExpressions.push(this.parseExpression())
      if (isA_OperatorToken(this.peek(), ';')) {
        this.advance()
      }
    }

    const thenExpression = thenExpressions.length === 1
      ? thenExpressions[0]!
      : {
        t: AstNodeType.SpecialExpression,
        n: 'do',
        p: thenExpressions,
        token: getTokenDebugData(token) && token,
      } satisfies DoNode

    let elseExpression: AstNode | undefined
    if (isA_ReservedSymbolToken(this.peek(), 'else')) {
      this.advance()
      const elseExpressions: AstNode[] = []
      while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
        elseExpressions.push(this.parseExpression())
        if (isA_OperatorToken(this.peek(), ';')) {
          this.advance()
        }
      }

      elseExpression = elseExpressions.length === 1
        ? elseExpressions[0]
        : {
          t: AstNodeType.SpecialExpression,
          n: 'do',
          p: elseExpressions,
          token: getTokenDebugData(token) && token,
        } satisfies DoNode
    }

    assertA_ReservedSymbolToken(this.peek(), 'end')
    this.advance()

    const params = [condition, thenExpression]
    if (elseExpression) {
      params.push(elseExpression)
    }

    return {
      t: AstNodeType.SpecialExpression,
      n: 'if',
      p: params,
      token: getTokenDebugData(token) && token,
    }
  }

  parseCond(token: A_SymbolToken): CondNode {
    this.advance()
    const params: AstNode[] = []

    while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
      assertA_ReservedSymbolToken(this.peek(), 'case')
      this.advance()
      params.push(this.parseExpression())
      assertA_ReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const expressions: AstNode[] = []
      while (
        !this.isAtEnd()
        && !isA_ReservedSymbolToken(this.peek(), 'case')
        && !isA_ReservedSymbolToken(this.peek(), 'end')) {
        expressions.push(this.parseExpression())
        if (isA_OperatorToken(this.peek(), ';')) {
          this.advance()
        }
      }

      params.push(
        expressions.length === 1
          ? expressions[0]!
          : {
            t: AstNodeType.SpecialExpression,
            n: 'do',
            p: expressions,
            token: getTokenDebugData(token) && token,
          } satisfies DoNode,
      )
      if (isA_ReservedSymbolToken(this.peek(), 'end')) {
        break
      }
      assertA_ReservedSymbolToken(this.peek(), 'case')
    }

    assertA_ReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'cond',
      p: params,
      token: getTokenDebugData(token) && token,
    }
  }

  parseSwitch(token: A_SymbolToken): SwitchNode {
    this.advance()
    const params: AstNode[] = [this.parseExpression()]

    while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
      assertA_ReservedSymbolToken(this.peek(), 'case')
      this.advance()
      params.push(this.parseExpression())
      assertA_ReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const expressions: AstNode[] = []
      while (
        !this.isAtEnd()
        && !isA_ReservedSymbolToken(this.peek(), 'case')
        && !isA_ReservedSymbolToken(this.peek(), 'end')) {
        expressions.push(this.parseExpression())
        if (isA_OperatorToken(this.peek(), ';')) {
          this.advance()
        }
      }

      params.push(
        expressions.length === 1
          ? expressions[0]!
          : {
            t: AstNodeType.SpecialExpression,
            n: 'do',
            p: expressions,
            token: getTokenDebugData(token) && token,
          } satisfies DoNode,
      )
      if (isA_ReservedSymbolToken(this.peek(), 'end')) {
        break
      }
      assertA_ReservedSymbolToken(this.peek(), 'case')
    }

    assertA_ReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return {
      t: AstNodeType.SpecialExpression,
      n: 'switch',
      p: params,
      token: getTokenDebugData(token) && token,
    }
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

    const body: AstNode[] = []

    while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
      body.push(this.parseExpression())
      if (isA_OperatorToken(this.peek(), ';')) {
        this.advance()
      }
    }
    assertA_ReservedSymbolToken(this.peek(), 'end')
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
}
