import type { SourceCodeInfo } from '../../tokenizer/token'

export function getCodeMarker(sourceCodeInfo: SourceCodeInfo): string {
  if (!sourceCodeInfo.position || !sourceCodeInfo.code)
    return ''

  const leftPadding = sourceCodeInfo.position.column - 1
  const rightPadding = sourceCodeInfo.code.length - leftPadding - 1
  return `${' '.repeat(Math.max(leftPadding, 0))}^${' '.repeat(Math.max(rightPadding, 0))}`
}
