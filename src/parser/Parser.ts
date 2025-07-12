import type { NormalExpressionName } from '../../reference/api'
import { type SpecialExpression, type SpecialExpressionName, type SpecialExpressionType, builtin } from '../builtin'
import { getAllBindingTargetNames } from '../builtin/bindingNode'
import { allNormalExpressions, normalExpressionTypes } from '../builtin/normalExpressions'
import type { AndNode } from '../builtin/specialExpressions/and'
import type { ArrayNode } from '../builtin/specialExpressions/array'
import type { CondNode } from '../builtin/specialExpressions/cond'
import type { DefinedNode } from '../builtin/specialExpressions/defined'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { DoNode } from '../builtin/specialExpressions/block'
import type { LambdaNode } from '../builtin/specialExpressions/functions'
import type { IfNode } from '../builtin/specialExpressions/if'
import type { LetNode } from '../builtin/specialExpressions/let'
import type { LoopNode } from '../builtin/specialExpressions/loop'
import type { DoSeqNode, ForNode, LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { ObjectNode } from '../builtin/specialExpressions/object'
import type { OrNode } from '../builtin/specialExpressions/or'
import type { QqNode } from '../builtin/specialExpressions/qq'
import type { RecurNode } from '../builtin/specialExpressions/recur'
import type { SwitchNode } from '../builtin/specialExpressions/switch'
import type { ThrowNode } from '../builtin/specialExpressions/throw'
import type { TryNode } from '../builtin/specialExpressions/try'
import type { UnlessNode } from '../builtin/specialExpressions/unless'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import { LitsError } from '../errors'
import { type SymbolicBinaryOperator, isBinaryOperator, isFunctionOperator } from '../tokenizer/operators'
import { isNumberReservedSymbol, numberReservedSymbolRecord } from '../tokenizer/reservedNames'
import type { OperatorToken, ReservedSymbolToken, SourceCodeInfo, StringToken, SymbolToken, Token, TokenType } from '../tokenizer/token'
import {
  asLBraceToken,
  asLBracketToken,
  asReservedSymbolToken,
  asSymbolToken,
  assertLParenToken,
  assertOperatorToken,
  assertRBraceToken,
  assertRBracketToken,
  assertRParenToken,
  assertReservedSymbolToken,
  isA_BinaryOperatorToken,
  isDocStringToken,
  isLBraceToken,
  isLBracketToken,
  isLParenToken,
  isOperatorToken,
  isRBraceToken,
  isRBracketToken,
  isRParenToken,
  isReservedSymbolToken,
  isStringToken,
  isSymbolToken,
} from '../tokenizer/token'
import type { TokenStream } from '../tokenizer/tokenize'
import { asUserDefinedSymbolNode, isNormalBuiltinSymbolNode, isSpecialBuiltinSymbolNode, isUserDefinedSymbolNode } from '../typeGuards/astNode'
import { smartTrim } from '../utils'
import { assertNumberOfParams } from '../utils/arity'
import { type BindingNode, type BindingTarget, type Node, type NormalBuiltinSymbolNode, type NormalExpressionNodeExpression, type NormalExpressionNodeWithName, type NumberNode, type ParseState, type ReservedSymbolNode, type SpecialBuiltinSymbolNode, type StringNode, type SymbolNode, type UserDefinedSymbolNode, bindingTargetTypes } from './types'

type InternalLoopBindingDelimiter = 'let' | 'when' | 'while'
type ImplicitBlockEnd = 'end' | 'else' | 'catch' | 'case'

const exponentiationPrecedence = 12
const binaryFunctionalOperatorPrecedence = 3
const conditionalOperatorPrecedence = 1
const placeholderRegexp = /^\$([1-9]\d?)?$/

function withSourceCodeInfo<T extends Node | BindingTarget>(node: T, sourceCodeInfo: SourceCodeInfo | undefined): T {
  if (sourceCodeInfo) {
    node[2] = sourceCodeInfo
  }
  return node
}

function getPrecedence(operatorSign: SymbolicBinaryOperator, sourceCodeInfo: SourceCodeInfo | undefined): number {
  switch (operatorSign) {
    case '^': // exponentiation
      return exponentiationPrecedence

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 11

    case '+': // addition
    case '-': // subtraction
      return 10

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
      return 9

    case '++': // string concatenation
      return 8

    case '<': // less than
    case '<=': // less than or equal
    case '≤': // less than or equal
    case '>': // greater than
    case '>=': // greater than or equal
    case '≥': // greater than or equal
      return 7

    case '==': // equal
    case '!=': // not equal
    case '≠': // not equal
      return 6

    case '&': // bitwise AND
    case 'xor': // bitwise XOR
    case '|': // bitwise OR
      return 5

    case '&&': // logical AND
    case '||': // logical OR
    case '??': // nullish coalescing
      return 4

      // leave room for binaryFunctionalOperatorPrecedence = 3

    case '|>': // pipe
      return 2

      // leave room for conditionalOperatorPrecedence = 1

    /* v8 ignore next 2 */
    default:
      throw new LitsError(`Unknown binary operator: ${operatorSign satisfies never}`, sourceCodeInfo)
  }
}

function createNamedNormalExpressionNode(symbolNode: NormalBuiltinSymbolNode | UserDefinedSymbolNode, params: Node[], sourceCodeInfo: SourceCodeInfo | undefined): NormalExpressionNodeWithName {
  const node: NormalExpressionNodeWithName = withSourceCodeInfo([NodeTypes.NormalExpression, [symbolNode, params]], sourceCodeInfo)

  if (isNormalBuiltinSymbolNode(symbolNode)) {
    assertNumberOfParams(allNormalExpressions[symbolNode[1]]!.arity, node[1][1].length, sourceCodeInfo)
  }

  return node
}

function createAccessorNode(left: Node, right: Node, sourceCodeInfo: SourceCodeInfo | undefined): NormalExpressionNodeExpression {
  return withSourceCodeInfo([NodeTypes.NormalExpression, [[NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.get], [left, right]]], sourceCodeInfo)
}

function fromBinaryOperatorToNode(operator: OperatorToken, symbolNode: SymbolNode, left: Node, right: Node, sourceCodeInfo: SourceCodeInfo | undefined): Node {
  const operatorName = operator[1]

  switch (operatorName) {
    case '^': // exponentiation
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
    case '==':
    case '!=':
    case '≠':
    case '&':
    case 'xor':
    case '|':
    case '|>':
      return createNamedNormalExpressionNode(symbolNode as NormalBuiltinSymbolNode, [left, right], sourceCodeInfo)
    case '&&':
    case '||':
    case '??':
      return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes[operatorName], [left, right]]] as AndNode, sourceCodeInfo)
    /* v8 ignore next 11 */
    case '.':
    case ';':
    case ':':
    case '=':
    case ',':
    case '->':
    case '...':
    case '?':
      throw new LitsError(`Unknown binary operator: ${operatorName}`, sourceCodeInfo)
    default:
      throw new LitsError(`Unknown binary operator: ${operatorName satisfies never}`, sourceCodeInfo)
  }
}

export class Parser {
  constructor(
    private readonly tokenStream: TokenStream,
    private parseState: ParseState,
  ) {}

  private peek(): Token | undefined {
    return this.tokenStream.tokens[this.parseState.position]
  }

  private peekSourceCodeInfo(): SourceCodeInfo | undefined {
    const currentToken = this.peek()
    return currentToken ? currentToken[2] : this.tokenStream.tokens.at(-1)?.[2]
  }

  private peekAhead(count: number): Token {
    return this.tokenStream.tokens[this.parseState.position + count]!
  }

  private advance(): void {
    this.parseState.position += 1
  }

  public parse(): Node[] {
    this.tokenStream.tokens.forEach((token) => {
      if (token[0] === 'Error') {
        throw new LitsError(token[3], token[2])
      }
    })

    const nodes: Node[] = []
    while (!this.isAtEnd()) {
      nodes.push(this.parseExpression(0, true))
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else {
        if (!this.isAtEnd()) {
          throw new LitsError('Expected ;', this.peekSourceCodeInfo())
        }
      }
    }
    return nodes
  }

  private parseExpression(precedence = 0, moduleScope = false): Node {
    const firstToken = this.peek()

    let left: Node

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
        // cas:
        //   left = this.parseDo(firstToken)
        //   break
        case 'loop':
          left = this.parseLoop(firstToken)
          break
        case 'try':
          left = this.parseTry(firstToken)
          break
      }
    }
    else if (isReservedSymbolToken(firstToken, 'export')) {
      if (!moduleScope) {
        throw new LitsError('export is only allowed in module scope', firstToken[2])
      }
      return this.parseExport(firstToken)
    }

    left ||= this.parseOperand()
    let operator = this.peek()

    while (!this.isAtExpressionEnd()) {
      if (isA_BinaryOperatorToken(operator)) {
        const name = operator[1]
        const newPrecedece = getPrecedence(name, operator[2])
        if (
          newPrecedece <= precedence
          // ^ (exponentiation) is right associative
          && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
          break
        }
        const symbol: SymbolNode = specialExpressionTypes[name as SpecialExpressionName]
          ? withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[name as SpecialExpressionName]], operator[2])
          : withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[name]!], operator[2])
        this.advance()
        const right = this.parseExpression(newPrecedece)
        left = fromBinaryOperatorToNode(operator, symbol, left, right, operator[2])
      }
      else if (isSymbolToken(operator)) {
        if (!isFunctionOperator(operator[1])) {
          break
        }
        const newPrecedece = binaryFunctionalOperatorPrecedence
        if (newPrecedece <= precedence) {
          break
        }
        const operatorSymbol = this.parseSymbol()
        const right = this.parseExpression(newPrecedece)
        if (isSpecialBuiltinSymbolNode(operatorSymbol)) {
          throw new LitsError('Special expressions are not allowed in binary functional operators', operatorSymbol[2])
        }
        left = createNamedNormalExpressionNode(operatorSymbol, [left, right], operator[2])
      }
      else if (operator?.[1] === '?') {
        if (conditionalOperatorPrecedence <= precedence) {
          break
        }
        this.advance()
        const trueNode = this.parseExpression()
        if (!isOperatorToken(this.peek(), ':')) {
          throw new LitsError('Expected :', this.peekSourceCodeInfo())
        }
        this.advance()
        const falseNode = this.parseExpression()
        left = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [left, trueNode, falseNode]]], left[2]) satisfies IfNode
      }
      else {
        break
      }

      operator = this.peek()
    }

    return left
  }

  asToken(token: Token | undefined): Token {
    if (!token) {
      throw new LitsError('Unexpected end of input', this.peekSourceCodeInfo())
    }
    return token
  }

  private parseOperand(): Node {
    let operand = this.parseOperandPart()
    let token = this.peek()

    while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
      if (token[1] === '.') {
        this.advance()
        const symbolToken = this.asToken(this.peek())
        if (!isSymbolToken(symbolToken)) {
          throw new LitsError('Expected symbol', this.peekSourceCodeInfo())
        }
        const stringNode: StringNode = withSourceCodeInfo([NodeTypes.String, symbolToken[1]], symbolToken[2])
        operand = createAccessorNode(operand, stringNode, token[2])
        this.advance()
        token = this.peek()
      }
      else if (isLBracketToken(token)) {
        this.advance()
        const expression = this.parseExpression()
        if (!isRBracketToken(this.peek())) {
          throw new LitsError('Expected closing bracket', this.peekSourceCodeInfo())
        }
        operand = createAccessorNode(operand, expression, token[2])
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

  private parseOperandPart(): Node {
    const token = this.asToken(this.peek())

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
        throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo())
      }
      this.advance()
      return expression
    }

    else if (isOperatorToken(token)) {
      const operatorName = token[1]
      if (isBinaryOperator(operatorName)) {
        this.advance()
        if (specialExpressionTypes[operatorName as SpecialExpressionName] !== undefined) {
          return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[operatorName as SpecialExpressionName]], token[2]) satisfies SpecialBuiltinSymbolNode
        }
        return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[operatorName as NormalExpressionName] as number], token[2]) satisfies NormalBuiltinSymbolNode
      }

      if (operatorName === '->') {
        return this.parseShorthandLamdaFunction()
      }
      else {
        throw new LitsError(`Illegal operator: ${operatorName}`, token[2])
      }
    }

    // Object litteral, e.g. {a: 1, b: 2}
    // Or block.
    if (isLBraceToken(token)) {
      const positionBefore = this.parseState.position
      try {
        return this.parseObject()
      }
      catch {
        this.parseState.position = positionBefore
        return this.parseBlock()[0]
      }
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
        return this.parseString(token as StringToken)
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
        throw new LitsError(`Unknown token type: ${tokenType}`, token[2])
    }
  }

  private parseObject(): ObjectNode {
    const
      firstToken = asLBraceToken(this.peek())
    this.advance()
    const params: Node[] = []
    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      if (isOperatorToken(this.peek(), '...')) {
        this.advance()
        params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()))
      }
      else {
        const token = this.peek()
        if (isStringToken(token)) {
          const stringNode = this.parseString(token)
          params.push(withSourceCodeInfo([NodeTypes.String, stringNode[1]], token[2]))
        }
        else if (isSymbolToken(token)) {
          const value = token[1].startsWith('\'')
            ? this.stringFromQuotedSymbol(token[1])
            : token[1]
          params.push(withSourceCodeInfo([NodeTypes.String, value], token[2]))
          this.advance()
        }
        else {
          throw new LitsError('Expected key to be a symbol or a string', this.peekSourceCodeInfo())
        }

        assertOperatorToken(this.peek(), ':')
        this.advance()

        params.push(this.parseExpression())
      }
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
        throw new LitsError('Expected comma or closing brace', this.peekSourceCodeInfo())
      }

      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBraceToken(this.peek())
    this.advance()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.object, params]], firstToken[2])
  }

  private parseArray(): ArrayNode {
    const firstToken = asLBracketToken(this.peek())
    this.advance()
    const params: Node[] = []
    while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
      if (isOperatorToken(this.peek(), '...')) {
        this.advance()
        params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()))
      }
      else {
        params.push(this.parseExpression())
      }
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', this.peekSourceCodeInfo())
      }
      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }

    assertRBracketToken(this.peek())
    this.advance()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.array, params]], firstToken[2])
  }

  private parseFunctionCall(symbol: Node): Node {
    this.advance()

    const params: Node[] = []
    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      if (isOperatorToken(this.peek(), '...')) {
        this.advance()
        params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()))
      }
      else {
        params.push(this.parseExpression())
      }
      const nextToken = this.peek()
      if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
        throw new LitsError('Expected comma or closing parenthesis', this.peek()?.[2])
      }
      if (isOperatorToken(nextToken, ',')) {
        this.advance()
      }
    }
    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo())
    }
    this.advance()

    if (isSpecialBuiltinSymbolNode(symbol)) { // Named function
      const specialExpressionType = symbol[1]

      const type = specialExpressionType as Exclude<
        SpecialExpressionType,
        | typeof specialExpressionTypes.for
        | typeof specialExpressionTypes.if
        | typeof specialExpressionTypes.unless
        | typeof specialExpressionTypes.cond
        | typeof specialExpressionTypes.switch
        | typeof specialExpressionTypes.let
        | typeof specialExpressionTypes.block
        | typeof specialExpressionTypes.loop
        | typeof specialExpressionTypes.try
        | typeof specialExpressionTypes.doseq
      >
      const specialExpression: SpecialExpression = builtin.specialExpressions[type]
      assertNumberOfParams(specialExpression.arity, params.length, symbol[2])
      switch (type) {
        case specialExpressionTypes['||']:
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies OrNode
        case specialExpressionTypes['&&']:
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies AndNode
        case specialExpressionTypes.recur:
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies RecurNode
        case specialExpressionTypes.array:
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies ArrayNode
        case specialExpressionTypes.object:
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies ObjectNode
        case specialExpressionTypes['??']: {
          if (params.length === 1) {
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, [params[0]!, undefined]]], symbol[2]) satisfies QqNode
          }
          else {
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, [params[0]!, params[1]!]]], symbol[2]) satisfies QqNode
          }
        }
        case specialExpressionTypes['defined?']: {
          const [param] = params
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param as SymbolNode]], symbol[2]) satisfies DefinedNode
        }
        case specialExpressionTypes.throw: {
          const [param] = params
          return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param!]], symbol[2]) satisfies ThrowNode
        }
        case specialExpressionTypes['0_lambda']:
        case specialExpressionTypes['0_def']:
          throw new LitsError(`${type} is not allowed`, symbol[2])
          /* v8 ignore next 2 */
        default:
          throw new LitsError(`Unknown special expression: ${type satisfies never}`, symbol[2])
      }
    }
    else if (isNormalBuiltinSymbolNode(symbol) || isNormalBuiltinSymbolNode(symbol)) {
      return createNamedNormalExpressionNode(symbol, params, symbol[2])
    }

    else {
      return withSourceCodeInfo([NodeTypes.NormalExpression, [symbol, params]], symbol[2]) satisfies NormalExpressionNodeExpression
    }
  }

  parseLambdaFunction(): LambdaNode | null {
    const firstToken = this.asToken(this.peek())

    if (isLParenToken(firstToken)
      && isSymbolToken(this.peekAhead(1))
      && isOperatorToken(this.peekAhead(2), '->')) {
      return null
    }

    try {
      const functionArguments = this.parseFunctionArguments()

      if (!isOperatorToken(this.peek(), '->')) {
        return null
      }
      this.advance()
      let nodes: Node[] | undefined
      let docString = ''
      if (isLBraceToken(this.peek())) {
        const parsedBlock = this.parseBlock(true)
        docString = parsedBlock[1]
        nodes = parsedBlock[0][1][1]
      }
      else {
        nodes = [this.parseExpression()]
      }

      return withSourceCodeInfo([
        NodeTypes.SpecialExpression,
        [
          specialExpressionTypes['0_lambda'],
          [
            functionArguments,
            nodes,
          ],
          docString,
        ],
      ], firstToken[2]) satisfies LambdaNode
    }
    catch {
      return null
    }
  }

  parseFunctionArguments(): BindingTarget[] {
    const firstToken = this.peek()
    if (isSymbolToken(firstToken)) {
      return [withSourceCodeInfo([bindingTargetTypes.symbol, [this.parseSymbol(), undefined]], firstToken[2])]
    }

    assertLParenToken(firstToken)
    this.advance()

    let rest = false
    let defaults = false
    const functionArguments: BindingTarget[] = []
    while (!this.isAtEnd() && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
      if (rest) {
        throw new LitsError('Rest argument must be last', this.peekSourceCodeInfo())
      }
      const bindingTarget = this.parseBindingTarget()
      if (bindingTarget[1][1] !== undefined) {
        defaults = true
      }
      if (bindingTarget[0] === bindingTargetTypes.rest) {
        rest = true
      }
      if (defaults && !bindingTarget[1][1]) {
        throw new LitsError('Default arguments must be last', this.peekSourceCodeInfo())
      }
      functionArguments.push(bindingTarget)

      if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
        throw new LitsError('Expected comma or closing parenthesis', this.peekSourceCodeInfo())
      }
      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }

    if (!isRParenToken(this.peek())) {
      throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo())
    }

    this.advance()

    return functionArguments
  }

  private parseShorthandLamdaFunction(): LambdaNode {
    const firstToken = this.asToken(this.peek())
    this.advance()
    const startPos = this.parseState.position

    let nodes: Node[] | undefined
    let docString = ''
    if (isLBraceToken(this.peek())) {
      const parsedBlock = this.parseBlock(true)
      docString = parsedBlock[1]
      nodes = parsedBlock[0][1][1]
    }
    else {
      nodes = [this.parseExpression()]
    }

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
              throw new LitsError('Please make up your mind, either use $ or $1', firstToken[2])

            dollar1 = match[1] ? 'WITH_1' : 'NAKED'
          }

          arity = Math.max(arity, Number(number))
          if (arity > 20)
            throw new LitsError('Can\'t specify more than 20 arguments', firstToken[2])
        }
      }
    }

    const functionArguments: BindingTarget[] = []

    for (let i = 1; i <= arity; i += 1) {
      if (i === 1 && dollar1 === 'NAKED') {
        functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, '$'], undefined]], firstToken[2]))
      }
      else {
        functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, `$${i}`], undefined]], firstToken[2]))
      }
    }

    const node: LambdaNode = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_lambda'], [
      functionArguments,
      nodes,
    ], docString]], firstToken[2])

    return node
  }

  private parseOptionalDefaulValue(): Node | undefined {
    if (isOperatorToken(this.peek(), '=')) {
      this.advance()
      return this.parseExpression()
    }
    return undefined
  }

  private parseBindingTarget({ requireDefaultValue, noRest }: { requireDefaultValue?: true, noRest?: true } = {}): BindingTarget {
    const firstToken = this.peek()

    // Symbol
    if (isSymbolToken(firstToken)) {
      const symbol = this.parseSymbol()
      if (!isUserDefinedSymbolNode(symbol)) {
        throw new LitsError('Expected user defined symbol', firstToken[2])
      }

      const defaultValue = this.parseOptionalDefaulValue()
      if (requireDefaultValue && !defaultValue) {
        throw new LitsError('Expected assignment', this.peekSourceCodeInfo())
      }

      return withSourceCodeInfo([bindingTargetTypes.symbol, [symbol, defaultValue]], firstToken[2])
    }

    // Rest
    if (isOperatorToken(firstToken, '...')) {
      if (noRest) {
        throw new LitsError('Rest element not allowed', firstToken[2])
      }
      this.advance()
      const symbol = asUserDefinedSymbolNode(this.parseSymbol())
      if (isOperatorToken(this.peek(), '=')) {
        throw new LitsError('Rest argument can not have default value', this.peekSourceCodeInfo())
      }
      return withSourceCodeInfo([bindingTargetTypes.rest, [symbol[1], undefined]], firstToken[2])
    }

    // Array
    if (isLBracketToken(firstToken)) {
      this.advance()
      const elements: (BindingTarget | null)[] = []
      let token = this.asToken(this.peek())

      let rest = false
      while (!isRBracketToken(token)) {
        if (rest) {
          throw new LitsError('Rest argument must be last', token[2])
        }
        if (isOperatorToken(token, ',')) {
          elements.push(null)
          this.advance()
          token = this.asToken(this.peek())
          continue
        }

        const target = this.parseBindingTarget()

        if (target[0] === bindingTargetTypes.rest) {
          rest = true
        }

        elements.push(target)
        token = this.asToken(this.peek())

        if (!isRBracketToken(token)) {
          assertOperatorToken(token, ',')
          this.advance()
        }
        token = this.asToken(this.peek())
      }
      this.advance()

      const defaultValue = this.parseOptionalDefaulValue()
      if (requireDefaultValue && !defaultValue) {
        throw new LitsError('Expected assignment', this.peekSourceCodeInfo())
      }

      return withSourceCodeInfo([bindingTargetTypes.array, [elements, defaultValue]], firstToken[2])
    }

    // Object
    if (isLBraceToken(firstToken)) {
      this.advance()
      const elements: Record<string, BindingTarget> = {}
      let token = this.asToken(this.peek())
      let rest = false
      while (!isRBraceToken(token)) {
        if (rest) {
          throw new LitsError('Rest argument must be last', token[2])
        }
        if (isOperatorToken(token, '...')) {
          rest = true
          this.advance()
        }
        const key = asUserDefinedSymbolNode(this.parseSymbol())
        token = this.asToken(this.peek())
        if (isReservedSymbolToken(token, 'as')) {
          if (rest) {
            throw new LitsError('Rest argument can not have alias', token[2])
          }
          this.advance()
          const name = asUserDefinedSymbolNode(this.parseSymbol())
          if (elements[name[1]]) {
            throw new LitsError(`Duplicate binding name: ${name}`, token[2])
          }
          elements[key[1]] = withSourceCodeInfo([bindingTargetTypes.symbol, [name, this.parseOptionalDefaulValue()]], firstToken[2])
        }
        else if (isRBraceToken(token) || isOperatorToken(token, ',') || isOperatorToken(token, '=')) {
          if (elements[key[1]]) {
            throw new LitsError(`Duplicate binding name: ${key}`, token[2])
          }
          if (rest && isOperatorToken(this.peek(), '=')) {
            throw new LitsError('Rest argument can not have default value', this.peekSourceCodeInfo())
          }

          elements[key[1]] = rest
            ? withSourceCodeInfo([bindingTargetTypes.rest, [key[1], this.parseOptionalDefaulValue()]], firstToken[2])
            : withSourceCodeInfo([bindingTargetTypes.symbol, [key, this.parseOptionalDefaulValue()]], firstToken[2])
        }
        else if (isOperatorToken(token, ':')) {
          this.advance()
          token = this.asToken(this.peek())
          if (!isLBraceToken(token) && !isLBracketToken(token)) {
            throw new LitsError('Expected object or array', token[2])
          }
          elements[key[1]] = this.parseBindingTarget()
        }

        if (!isRBraceToken(this.peek())) {
          assertOperatorToken(this.peek(), ',')
          this.advance()
        }
        token = this.asToken(this.peek())
      }
      this.advance()
      token = this.asToken(this.peek())

      const defaultValue = this.parseOptionalDefaulValue()
      if (requireDefaultValue && !defaultValue) {
        throw new LitsError('Expected assignment', token[2])
      }

      return withSourceCodeInfo([bindingTargetTypes.object, [elements, defaultValue]], firstToken[2])
    }

    throw new LitsError('Expected symbol', this.peekSourceCodeInfo())
  }

  private parseLet(token: SymbolToken): LetNode {
    this.advance()

    const target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true })

    const value = target[1][1]!
    target[1][1] = undefined

    const bindingTarget: BindingNode = withSourceCodeInfo([NodeTypes.Binding, [target, value]], token[2])
    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.let, bindingTarget]], token[2]) satisfies LetNode
  }

  private parseBlock(allowDocString = false): [DoNode, string] {
    const token = asLBraceToken(this.peek())
    this.advance()
    let docString: string = ''
    if (allowDocString && isDocStringToken(this.peek())) {
      docString = this.parseDocString()
    }

    const expressions: Node[] = []
    while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
      expressions.push(this.parseExpression())
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else if (!isRBraceToken(this.peek())) {
        throw new LitsError('Expected }', this.peekSourceCodeInfo())
      }
    }
    if (expressions.length === 0) {
      expressions.push(withSourceCodeInfo([NodeTypes.ReservedSymbol, 'null'], token[2]))
    }
    assertRBraceToken(this.peek())
    this.advance()
    return [
      withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.block, expressions]], token[2]) satisfies DoNode,
      docString,
    ]
  }

  private parseImplicitBlock(ends: ImplicitBlockEnd[]): Node {
    const nodes: Node[] = []
    while (!this.isAtEnd() && !this.isImplicitBlockEnd(ends)) {
      if (isOperatorToken(this.peek(), ';')) {
        this.advance()
      }
      else {
        nodes.push(this.parseExpression())
      }
    }
    this.assertImplicitBlockEnd(ends)

    if (nodes.length === 0) {
      throw new LitsError('Expected expression', this.peekSourceCodeInfo())
    }

    return nodes.length === 1
      ? nodes[0]!
      : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.block, nodes]], this.peekSourceCodeInfo()) satisfies DoNode
  }

  private assertImplicitBlockEnd(ends: ImplicitBlockEnd[]): void {
    if (!this.isImplicitBlockEnd(ends)) {
      throw new LitsError(`Expected ${ends.map(e => e[1]).join(' or ')}`, this.peekSourceCodeInfo())
    }
  }

  private isImplicitBlockEnd(ends: ImplicitBlockEnd[]): boolean {
    for (const end of ends) {
      if (isReservedSymbolToken(this.peek(), end)) {
        return true
      }
    }
    return false
  }

  private parseLoop(firstToken: SymbolToken): LoopNode {
    this.advance()

    assertLParenToken(this.peek())
    this.advance()

    const bindingNodes: BindingNode[] = []
    let token = this.peek()
    while (!this.isAtEnd() && !isRParenToken(token)) {
      const target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true })
      const value = target[1][1]!
      target[1][1] = undefined

      bindingNodes.push(withSourceCodeInfo([NodeTypes.Binding, [target, value]], target[2]) satisfies BindingNode)

      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
      token = this.peek()
    }
    if (bindingNodes.length === 0) {
      throw new LitsError('Expected binding', this.peekSourceCodeInfo())
    }

    assertRParenToken(token)
    this.advance()

    assertOperatorToken(this.peek(), '->')
    this.advance()

    const expression = this.parseExpression()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.loop, bindingNodes, expression]], firstToken[2]) satisfies LoopNode
  }

  private parseTry(token: SymbolToken): TryNode {
    this.advance()
    const tryExpression = this.parseImplicitBlock(['catch'])

    this.advance()

    let errorSymbol: SymbolNode | undefined
    if (isLParenToken(this.peek())) {
      this.advance()
      errorSymbol = this.parseSymbol()
      assertRParenToken(this.peek())
      this.advance()
    }

    const catchExpression = this.parseImplicitBlock(['end'])

    this.advance()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.try, tryExpression, errorSymbol, catchExpression]], token[2]) satisfies TryNode
  }

  private parseForOrDoseq(firstToken: SymbolToken): ForNode | DoSeqNode {
    const isDoseq = firstToken[1] === 'doseq'
    this.advance()

    assertLParenToken(this.peek())
    this.advance()

    const forLoopBindings: LoopBindingNode[] = []

    while (!this.isAtEnd() && !isRParenToken(this.peek())) {
      const loopBinding = this.parseForLoopBinding()
      const existingBoundNames = forLoopBindings.flatMap(b => Object.keys(getAllBindingTargetNames(b[0][1][0])))
      const newBoundNames = getAllBindingTargetNames(loopBinding[0][1][0])
      if (Object.keys(newBoundNames).some(n => existingBoundNames.includes(n))) {
        throw new LitsError('Duplicate binding', loopBinding[0][2])
      }
      forLoopBindings.push(loopBinding)
      if (isOperatorToken(this.peek(), ',')) {
        this.advance()
      }
    }

    assertRParenToken(this.peek())
    this.advance()

    assertOperatorToken(this.peek(), '->')
    this.advance()

    const expression = this.parseExpression()

    return isDoseq
      ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.doseq, forLoopBindings, expression]], firstToken[2]) satisfies DoSeqNode
      : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.for, forLoopBindings, expression]], firstToken[2]) satisfies ForNode
  }

  private parseForLoopBinding(): LoopBindingNode {
    const bindingNode = this.parseBinding()

    const modifiers: Array<'&let' | '&when' | '&while'> = []
    let token = this.asToken(this.peek())

    this.assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while'])

    const letBindings: BindingNode[] = []
    if (token[1] === 'let') {
      modifiers.push('&let')
      while (isSymbolToken(token, 'let')) {
        const letNode = this.parseLet(token)
        const existingBoundNames = letBindings.flatMap(b => Object.keys(getAllBindingTargetNames(b[1][0])))
        const newBoundNames = Object.keys(getAllBindingTargetNames(letNode[1][1][1][0]))
        if (newBoundNames.some(n => existingBoundNames.includes(n))) {
          throw new LitsError('Duplicate binding', letNode[1][1][2])
        }

        letBindings.push(letNode[1][1])
        token = this.asToken(this.peek())
        this.assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while'])
        token = this.asToken(this.peek())
      }
    }

    let whenNode: Node | undefined
    let whileNode: Node | undefined
    while (
      isReservedSymbolToken(token, 'when')
      || isReservedSymbolToken(token, 'while')
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
      token = this.asToken(this.peek())

      const symbols: ('when' | 'while')[] = modifiers.includes('&when') && modifiers.includes('&while')
        ? []
        : modifiers.includes('&when')
          ? ['while']
          : ['when']

      this.assertInternalLoopBindingDelimiter(token, symbols)
      token = this.asToken(this.peek())
    }

    this.assertInternalLoopBindingDelimiter(token, [])

    return [bindingNode, letBindings, whenNode, whileNode] satisfies LoopBindingNode
  }

  assertInternalLoopBindingDelimiter(token: Token, symbols: InternalLoopBindingDelimiter[]): void {
    if (!this.isInternalLoopBindingDelimiter(token, symbols)) {
      const symbolsString = `${[...symbols, ','].map(symbol => `"${symbol}"`).join(', ')} or ")"`
      throw new LitsError(`Expected symbol ${symbolsString}`, token[2])
    }
  }

  isInternalLoopBindingDelimiter(token: Token, symbols: InternalLoopBindingDelimiter[]): boolean {
    // end of loop binding
    if (isOperatorToken(token, ',') || isRParenToken(token)) {
      return true
    }
    for (const symbol of symbols) {
      if (symbol === 'let' && isSymbolToken(token, 'let')) {
        return true
      }
      if (['when', 'while'].includes(symbol) && isReservedSymbolToken(token, symbol as 'when' | 'while')) {
        return true
      }
    }
    return false
  }

  private parseBinding(): BindingNode {
    const firstToken = asSymbolToken(this.peek())
    const name = asUserDefinedSymbolNode(this.parseSymbol())

    assertReservedSymbolToken(this.peek(), 'in')
    this.advance()

    const value = this.parseExpression()

    const node: BindingNode = withSourceCodeInfo(
      [
        NodeTypes.Binding,
        [
          withSourceCodeInfo([bindingTargetTypes.symbol, [name, undefined]], firstToken[2]),
          value,
        ],
      ],
      firstToken[2],
    )
    return node
  }

  parseIfOrUnless(token: SymbolToken): IfNode | UnlessNode {
    const isUnless = token[1] === 'unless'
    this.advance()
    const condition = this.parseExpression()
    assertReservedSymbolToken(this.peek(), 'then')
    this.advance()
    const thenExpression = this.parseImplicitBlock(['else', 'end'])

    let elseExpression: Node | undefined
    if (isReservedSymbolToken(this.peek(), 'else')) {
      this.advance()
      elseExpression = this.parseImplicitBlock(['end'])
    }

    this.advance()

    return isUnless
      ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.unless, [condition, thenExpression, elseExpression]]], token[2]) satisfies UnlessNode
      : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [condition, thenExpression, elseExpression]]], token[2]) satisfies IfNode
  }

  parseCond(token: SymbolToken): CondNode {
    this.advance()

    const params: [Node, Node][] = []

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      assertReservedSymbolToken(this.peek(), 'case')
      this.advance()
      const caseExpression = this.parseExpression()
      assertReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const thenExpression = this.parseImplicitBlock(['case', 'end'])

      params.push([caseExpression, thenExpression])
      if (isReservedSymbolToken(this.peek(), 'end')) {
        break
      }
    }

    assertReservedSymbolToken(this.peek())
    this.advance()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.cond, params]], token[2]) satisfies CondNode
  }

  parseSwitch(token: SymbolToken): SwitchNode {
    this.advance()
    const valueExpression = this.parseExpression()
    const params: [Node, Node][] = []

    while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
      assertReservedSymbolToken(this.peek(), 'case')
      this.advance()
      const caseExpression = this.parseExpression()
      assertReservedSymbolToken(this.peek(), 'then')
      this.advance()
      const thenExpression = this.parseImplicitBlock(['case', 'end'])

      params.push([caseExpression, thenExpression])
      if (isReservedSymbolToken(this.peek(), 'end')) {
        break
      }
    }

    assertReservedSymbolToken(this.peek(), 'end')
    this.advance()

    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.switch, valueExpression, params]], token[2]) satisfies SwitchNode
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
      return [';', ',', ':'].includes(token[1])
    }
    if (isReservedSymbolToken(token)) {
      return ['else', 'when', 'while', 'case', 'catch', 'let', 'then'].includes(token[1])
    }
    return false
  }

  private parseExport(exportToken: ReservedSymbolToken<'export'>): DefNode {
    this.advance()
    const token = this.peek()
    if (isSymbolToken(token, 'let')) {
      const letNode = this.parseLet(asSymbolToken(token))
      return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_def'], letNode[1][1]]], exportToken[2]) satisfies DefNode
    }
    else {
      throw new LitsError('Expected let', this.peekSourceCodeInfo())
    }
  }

  private stringToSymbolNode(value: string, sourceCodeInfo: SourceCodeInfo | undefined): SymbolNode {
    if (specialExpressionTypes[value as SpecialExpressionName] !== undefined && value !== 'fn' && value !== 'def' && value !== 'defn') {
      return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[value as SpecialExpressionName]], sourceCodeInfo) satisfies SymbolNode
    }
    if (normalExpressionTypes[value as NormalExpressionName] !== undefined) {
      return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[value as NormalExpressionName] as number], sourceCodeInfo) satisfies SymbolNode
    }
    return withSourceCodeInfo([NodeTypes.UserDefinedSymbol, value], sourceCodeInfo) satisfies SymbolNode
  }

  stringFromQuotedSymbol(value: string): string {
    return value.substring(1, value.length - 1)
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
  }

  private parseSymbol(): SymbolNode | NormalBuiltinSymbolNode | SpecialBuiltinSymbolNode {
    const token = this.asToken(this.peek())
    this.advance()
    if (!isSymbolToken(token)) {
      throw new LitsError(`Expected symbol token, got ${token[0]}`, token[2])
    }
    if (token[1][0] === '\'') {
      return this.stringToSymbolNode(this.stringFromQuotedSymbol(token[1]), token[2])
    }
    else {
      return this.stringToSymbolNode(token[1], token[2])
    }
  }

  private parseReservedSymbol(): ReservedSymbolNode | NumberNode {
    const token = asReservedSymbolToken(this.peek())
    this.advance()

    const symbol = token[1]
    if (isNumberReservedSymbol(symbol)) {
      return withSourceCodeInfo([NodeTypes.Number, numberReservedSymbolRecord[symbol]], token[2]) satisfies NumberNode
    }
    return withSourceCodeInfo([NodeTypes.ReservedSymbol, token[1]], token[2]) satisfies ReservedSymbolNode
  }

  private parseNumber(): NumberNode {
    const token = this.asToken(this.peek())
    this.advance()

    const value = token[1]
    const negative = value[0] === '-'
    const numberString = (negative ? value.substring(1) : value).replace(/_/g, '')
    return withSourceCodeInfo([NodeTypes.Number, negative ? -Number(numberString) : Number(numberString)], token[2]) satisfies NumberNode
  }

  private parseDocString(): string {
    const token = this.asToken(this.peek())
    const stringToken: StringToken = token[2] ? ['String', token[1].slice(2, -2), token[2]] : ['String', token[1].slice(2, -2)]
    const stringNode = this.parseString(stringToken)
    return smartTrim(stringNode[1]) // Extract the string value from the StringNode
  }

  private parseString(token: StringToken): StringNode {
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

    return withSourceCodeInfo([NodeTypes.String, value], token[2]) satisfies StringNode
  }

  private parseRegexpShorthand(): NormalExpressionNodeWithName {
    const token = this.asToken(this.peek())
    this.advance()

    const endStringPosition = token[1].lastIndexOf('"')
    const regexpString = token[1].substring(2, endStringPosition)
    const optionsString = token[1].substring(endStringPosition + 1)
    const stringNode: StringNode = withSourceCodeInfo([NodeTypes.String, regexpString], token[2]) satisfies StringNode

    const optionsNode: StringNode = withSourceCodeInfo([NodeTypes.String, optionsString], token[2]) satisfies StringNode

    const node: NormalExpressionNodeWithName = withSourceCodeInfo([
      NodeTypes.NormalExpression,
      [
        withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.regexp as number], token[2]),
        [stringNode, optionsNode],
      ],
    ], token[2])

    return node
  }
}
