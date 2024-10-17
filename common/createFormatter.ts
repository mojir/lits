import type { FormatterRule } from '../cli/src/cliFormatterRules'

export type TextFormatter = (text: string) => string

export function createFormatter(rules: FormatterRule[], options?: { prefix?: string, suffix?: string }): TextFormatter {
  const prefix = options?.prefix ?? ''
  const suffix = options?.suffix ?? ''
  function format(text: string): string {
    let newText = text
    let index = 0

    whileLoop:
    while (index < newText.length) {
      for (const rule of rules) {
        const { count, formattedText } = rule(newText, index, format)
        if (!count)
          continue
        newText = `${newText.slice(0, index)}${formattedText}${newText.slice(index + count)}`
        index += formattedText.length
        continue whileLoop
      }
      index += 1
    }
    return `${prefix}${newText}${suffix}`
  }

  return format
}
