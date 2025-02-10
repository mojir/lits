import { styles } from '../styles'
import { type TextFormatter, createFormatter } from '../../../common/createFormatter'
import { postfixIdentifierCharacterClass, postfixIdentifierFirstCharacterClass } from '../../../src/identifier'

export type FormatterRule = (text: string, index: number, formatter: TextFormatter) => {
  count: number
  formattedText: string
}

const variableRegExp = new RegExp(`^\\$${postfixIdentifierFirstCharacterClass}${postfixIdentifierCharacterClass}*`)

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
    if (characterBefor && new RegExp(postfixIdentifierCharacterClass).test(characterBefor))
      return noMatch
    if (characterBefor && numberRegExp.test(characterBefor))
      return noMatch
    if (characterAfter && new RegExp(postfixIdentifierCharacterClass).test(characterAfter))
      return noMatch
    if (characterAfter && numberRegExp.test(characterAfter))
      return noMatch

    const number = startMatch[0]
    const formattedText = `<span ${styles('text-color-Beige')}>${number}</span>`
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

const operatorRule = createRule({
  name: 'string',
  startPattern: /^[<>\-+/*=?.,():]+/,
  startTag: `<span ${styles('text-color-gray-200')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const stringRule = createRule({
  name: 'string',
  startPattern: /^"/,
  endPattern: /^"/,
  startTag: `<span ${styles('text-color-Pink')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const shortcutStringRule = createRule({
  name: 'string',
  startPattern: new RegExp(`^:${postfixIdentifierCharacterClass}+`),
  startTag: `<span ${styles('text-color-Pink')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const functionNameRule = createRule({
  name: 'functionName',
  startPattern: new RegExp(`^\\((?=${postfixIdentifierCharacterClass}+)`),
  endPattern: /^[) \n]/,
  startTag: `<span ${styles('text-color-Blue')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: false,
  stopRecursion: true,
})

const nameRule = createRule({
  name: 'functionName',
  startPattern: new RegExp(`^${postfixIdentifierCharacterClass}+`),
  startTag: `<span ${styles('text-color-Mint')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const commentRule = createRule({
  name: 'comment',
  startPattern: /^;.*/,
  startTag: `<span ${styles('text-color-gray-500', 'italic')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const litsKeywordRule = createRule({
  name: 'functionName',
  startPattern: /^(nil|true|false)\b/,
  startTag: `<span ${styles('text-color-gray-200')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const inlineCodeKeywordRule = createRule({
  name: 'inlineCodeKeywordRule',
  startPattern: /^(null|true|false|nil|falsy|truthy)\b/,
  startTag: `<span ${styles('text-color-gray-200')}>`,
  endTag: '</span>',
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const formatInlineCode = createFormatter([
  operatorRule,
  stringRule,
  shortcutStringRule,
  numberRule,
  inlineCodeKeywordRule,
  nameRule,
], {
  prefix: `<span ${styles('text-color-gray-200')}>`,
  suffix: '</span>',
})

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
    const formattedText = formatInlineCode(body)
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

export const litsExpressionRules: FormatterRule[] = [
  commentRule,
  stringRule,
  shortcutStringRule,
  functionNameRule,
  numberRule,
  litsKeywordRule,
  nameRule,
]

export const formatLitsExpression = createFormatter(litsExpressionRules, {
  prefix: `<span ${styles('text-color-gray-200', 'font-mono')}>`,
  suffix: '</span>',
})

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
  startTag: `<span ${styles('font-bold')}>`,
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
