import { type TextFormatter, createFormatter } from '../../common/createFormatter'
import { polishSymbolCharacterClass, polishSymbolFirstCharacterClass } from '../../src/symbolPatterns'
import { ColorEnum, type Colorizer } from './colorizer'

export type FormatterRule = (text: string, index: number, formatter: TextFormatter) => {
  count: number
  formattedText: string
}

export const variableRegExp = new RegExp(`^\\$${polishSymbolFirstCharacterClass}${polishSymbolCharacterClass}*`)

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
export const getNumberRule: (cli: Colorizer) => FormatterRule = fmt => (text, index) => {
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
    const formattedText = fmt.yellow.bright(number)
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

const operatorRule = createRule({
  name: 'string',
  startPattern: /^[<>\-+/*=?.,():]+/,
  startTag: `${ColorEnum.Bright}${ColorEnum.FgWhite}`,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const stringRule = createRule({
  name: 'string',
  startPattern: /^"/,
  endPattern: /^"/,
  startTag: ColorEnum.FgRed,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const shortcutStringRule = createRule({
  name: 'string',
  startPattern: new RegExp(`^:${polishSymbolCharacterClass}+`),
  startTag: ColorEnum.FgRed,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const functionNameRule = createRule({
  name: 'functionName',
  startPattern: new RegExp(`^\\((?=${polishSymbolCharacterClass}+)`),
  endPattern: /^[) \n]/,
  startTag: ColorEnum.FgBlue,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: false,
  stopRecursion: true,
})

const nameRule = createRule({
  name: 'functionName',
  startPattern: new RegExp(`^${polishSymbolCharacterClass}+`),
  startTag: `${ColorEnum.Bright}${ColorEnum.FgBlue}`,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const commentRule = createRule({
  name: 'comment',
  startPattern: /^;.*/,
  startTag: `${ColorEnum.FgGray}${ColorEnum.Italic}`,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const litsKeywordRule = createRule({
  name: 'functionName',
  startPattern: /^(null|true|false)\b/,
  startTag: `${ColorEnum.Bright}${ColorEnum.FgGray}`,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

const inlineCodeKeywordRule = createRule({
  name: 'inlineCodeKeywordRule',
  startPattern: /^(null|true|false|falsy|truthy)\b/,
  startTag: `${ColorEnum.Bright}${ColorEnum.FgGray}`,
  endTag: ColorEnum.Reset,
  keepPatterns: true,
  formatPatterns: true,
  stopRecursion: true,
})

export function getInlineCodeFormatter(cli: Colorizer) {
  return createFormatter([
    operatorRule,
    stringRule,
    shortcutStringRule,
    getNumberRule(cli),
    inlineCodeKeywordRule,
    nameRule,
  ])
}

const getInlineCodeRule: (fmt: Colorizer) => FormatterRule = fmt => (text, index) => {
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
    const formattedText = getInlineCodeFormatter(fmt)(body)
    return { count, formattedText }
  }
  return { count: 0, formattedText: '' }
}

export function getLitsExpressionRules(cli: Colorizer): FormatterRule[] {
  return [
    commentRule,
    stringRule,
    shortcutStringRule,
    functionNameRule,
    getNumberRule(cli),
    litsKeywordRule,
    nameRule,
  ]
}

export function getLitsFormatter(fmt: Colorizer) {
  return createFormatter(getLitsExpressionRules(fmt))
}

function getInlineLitsExpressionRule(fmt: Colorizer): FormatterRule {
  return (text, index) => {
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
      const formattedText = getLitsFormatter(fmt)(body)
      return { count, formattedText }
    }
    return { count: 0, formattedText: '' }
  }
}

const italicRule = createRule({
  name: 'italic',
  startPattern: /^\*\*\*/,
  endPattern: /^\*\*\*/,
  startTag: ColorEnum.Italic,
  endTag: ColorEnum.Reset,
})

const boldRule = createRule({
  name: 'bold',
  startPattern: /^\*\*/,
  endPattern: /^\*\*/,
  startTag: ColorEnum.Bright,
  endTag: ColorEnum.Reset,
})

export function getMdRules(fmt: Colorizer): FormatterRule[] {
  return [
    getInlineLitsExpressionRule(fmt),
    getInlineCodeRule(fmt),
    italicRule,
    boldRule,
    getNumberRule(fmt),
  ]
}

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
