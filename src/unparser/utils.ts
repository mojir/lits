import { isNewLineToken } from '../tokenizer'
import type { MetaToken, MetaTokens } from '../tokenizer/interface'
import type { UnparseOptions } from './UnparseOptions'

export function ensureNewlineSeparator(a: string, b: string): string {
  return !a || !b || a.endsWith('\n') || b.startsWith('\n') ? `${a}${b}` : `${a}\n${b}`
}

export function ensureSpaceSeparator(a: string, b: string): string {
  return !a || !b || a.endsWith(' ') || b.startsWith(' ') || b.startsWith('\n')
    ? `${a}${b}`
    : `${a} ${b}`
}

export function applyMetaTokens(
  value: string | number,
  metaTokens: MetaTokens | undefined,
  options: UnparseOptions,
): string {
  if (!metaTokens) {
    return `${options.inlined ? '' : options.indent}${value}`
  }
  else {
    const result = `${
        metaTokensToString(metaTokens.leadingMetaTokens, value)
      }${
        value
      }${
        metaTokens?.inlineCommentToken ? ` ${metaTokens.inlineCommentToken.v}\n` : ''
      }`
    return result.split('\n').map((line, index) =>
      `${line && (!options.inlined || index > 0) ? options.indent : ''}${line}`,
    ).join('\n')
  }
}

function metaTokensToString(metaTokens: MetaToken[], tokenValue: string | number): string {
  const isEndBracket = tokenValue === ')' || tokenValue === ']' || tokenValue === '}'
  const isOnlyNewLine = metaTokens.length === 1 && isNewLineToken(metaTokens[0])
  if (isEndBracket && isOnlyNewLine)
    return ''

  return metaTokens.map(metaToken =>
    isNewLineToken(metaToken) ? metaToken.v : `${metaToken.v}\n`,
  ).join('')
}
