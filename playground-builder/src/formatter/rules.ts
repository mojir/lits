import { styles } from '../styles'
import type { TextFormatter } from '../../../common/createFormatter'
import { polishSymbolCharacterClass, polishSymbolFirstCharacterClass } from '../../../src/symbolPatterns'
import { Lits } from '../../../src/Lits/Lits'
import { allBuiltinModules } from '../../../src/allModules'
import type { Token } from '../../../src/tokenizer/token'
import { normalExpressionKeys, specialExpressionKeys } from '../../../src/builtin'

export type FormatterRule = (text: string, index: number, formatter: TextFormatter) => {
  count: number
  formattedText: string
}

const variableRegExp = new RegExp(`^\\$${polishSymbolFirstCharacterClass}${polishSymbolCharacterClass}*`)

const noMatch = { count: 0, formattedText: '' }

export function createVariableRule(
  formatVariableName: TextFormatter,
  variableNamePredicate: (variableName: string) => boolean,
): FormatterRule {
  return (text, index) => {
    const startMatch = variableRegExp.exec(text.slice(index))
    if (startMatch) {
      const count = startMatch[0].length
      const variableName = startMatch[0].slice(1)
      if (!variableNamePredicate(variableName))
        return noMatch

      const formattedText = formatVariableName(variableName)
      return { count, formattedText }
    }
    return { count: 0, formattedText: '' }
  }
}

const numberRegExp = /^\d+(?:\.\d+)?/
export const numberRule: FormatterRule = (text, index) => {
  const startMatch = numberRegExp.exec(text.slice(index))
  if (startMatch) {
    const count = startMatch[0].length
    const characterBefor = text[index - 1]
    const characterAfter = text[index + count]
    if (characterBefor && new RegExp(polishSymbolCharacterClass).test(characterBefor))
      return noMatch
    if (characterBefor && numberRegExp.test(characterBefor))
      return noMatch
    if (characterAfter && new RegExp(polishSymbolCharacterClass).test(characterAfter))
      return noMatch
    if (characterAfter && numberRegExp.test(characterAfter))
      return noMatch

    const number = startMatch[0]
    const formattedText = `<span ${styles('text-color-Beige')}>${number}</span>`
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

const inlineCodeRule: FormatterRule = (text, index) => {
  if (text[index] === '`') {
    let count = 1
    let body = ''

    while (index + count < text.length && text[index + count] !== '`') {
      body += text[index + count]
      count += 1
    }
    if (text[index + count] !== '`')
      throw new Error(`No end \` found for rule inlineCodeRule: ${text}`)

    count += 1
    const formattedText = formatLitsExpression(body)
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

const lits = new Lits({ debug: false, modules: allBuiltinModules })

export type StyleOverride = {
  values: string[]
  style: string
}

const normalExpressionSet = new Set(normalExpressionKeys)
const specialExpressionSet = new Set(specialExpressionKeys)

export function formatLitsExpression(program: string, styleOverride?: StyleOverride): string {
  try {
    const tokens = lits.tokenize(program).tokens
    const spans = tokens.map((token) => {
      const style = styleOverride?.values.includes(token[1]) ? styleOverride.style : getStylesFromToken(token)
      return `<span ${style}>${token[1]}</span>`
    })

    return `<span ${styles('text-color-gray-200', 'font-mono')}>${
      spans.join('')
    }</span>`
  }
  catch (error) {
    return `<span ${styles('text-color-Crimson')}>${program}</span>`
  }
}

function getStylesFromToken(token: Token): string {
  const tokenType = token[0]
  switch (tokenType) {
    case 'String':
      return styles('text-color-Pink')
    case 'DocString':
      return styles('text-color-Pink')
    case 'RegexpShorthand':
      return styles('text-color-Pink')
    case 'Symbol':
      return specialExpressionSet.has(token[1])
        ? styles('text-color-BrightYellow')
        : normalExpressionSet.has(token[1])
          ? styles('text-color-Beige')
          : styles('text-color-Mint')
    case 'BasePrefixedNumber':
    case 'Number':
      return styles('text-color-Viola')
    case 'Shebang':
    case 'SingleLineComment':
    case 'MultiLineComment':
      return styles('text-color-gray-500', 'italic')
    case 'Operator':
      return styles('text-color-gray-300')
    case 'ReservedSymbol':
      return styles('text-color-BrightYellow')
    case 'Whitespace':
      return ''
    case 'Error':
      return styles('text-color-Crimson')
    case 'LBrace':
    case 'RBrace':
    case 'LBracket':
    case 'RBracket':
    case 'LParen':
    case 'RParen':
      return styles('text-color-gray-300')

    default:
      throw new Error(`Unexpected token: ${token satisfies never}`)
  }
}

const inlineLitsExpressionRule: FormatterRule = (text, index) => {
  if (text.slice(index, index + 2) === '``') {
    let count = 2
    let body = ''

    while (index + count < text.length && text.slice(index + count, index + count + 2) !== '``') {
      body += text[index + count]
      count += 1
    }
    if (text.slice(index + count, index + count + 2) !== '``')
      throw new Error(`No end \` found for rule inlineLitsCodeRule: ${text}`)

    count += 2
    const formattedText = formatLitsExpression(body)
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

const italicRule = createRule({
  name: 'italic',
  startPattern: /^\*\*\*/,
  endPattern: /^\*\*\*/,
  startTag: `<span ${styles('italic')}>`,
  endTag: '</span>',
})

const boldRule = createRule({
  name: 'bold',
  startPattern: /^\*\*/,
  endPattern: /^\*\*/,
  startTag: `<span ${styles('text-color-gray-300')}>`,
  endTag: '</span>',
})

const newLineRule = createRule({
  name: 'new-line',
  startPattern: /^ {2}\n/,
  startTag: '',
  endTag: '<br>',
})

const newParagraphRule = createRule({
  name: 'new-line',
  startPattern: /^\n{2}/,
  startTag: '',
  endTag: '<p>',
})

const paragraphRule = createRule({
  name: 'paragraph',
  startPattern: /^\n{2}/,
  startTag: `<div ${styles('mb-2')}>`,
  endTag: '</div>',
})

export const mdRules: FormatterRule[] = [
  inlineLitsExpressionRule,
  inlineCodeRule,
  italicRule,
  boldRule,
  newLineRule,
  newParagraphRule,
  paragraphRule,
  numberRule,
]

function createRule({
  name,
  startPattern,
  endPattern,
  startTag,
  endTag,
  keepPatterns,
  formatPatterns,
  stopRecursion,
}: {
  name: string
  startPattern: RegExp
  endPattern?: RegExp
  startTag: string
  endTag: string
  keepPatterns?: boolean
  formatPatterns?: boolean
  stopRecursion?: boolean
}): FormatterRule {
  return (text, index, formatter) => {
    const startMatch = startPattern.exec(text.slice(index))
    if (startMatch) {
      let count = startMatch[0].length
      let body = keepPatterns && formatPatterns ? startMatch[0] : ''
      let endMatch: RegExpExecArray | null = null

      if (endPattern) {
        while (index + count < text.length && !endPattern.test(text.slice(index + count))) {
          body += text[index + count]
          count += 1
        }
        endMatch = endPattern.exec(text.slice(index + count))
        if (!endMatch)
          throw new Error(`No end pattern found for rule ${name},  ${endPattern}`)

        count += endMatch[0].length
        body += keepPatterns && formatPatterns ? endMatch[0] : ''
      }
      const formattedText = `${
        keepPatterns && !formatPatterns ? startMatch[0] : ''
      }${
        startTag
      }${
        body ? (stopRecursion ? body : formatter(body)) : ''
      }${
        endTag
      }${
        endMatch && keepPatterns && !formatPatterns ? endMatch[0] : ''
      }`
      return { count, formattedText }
    }
    return { count: 0, formattedText: '' }
  }
}
