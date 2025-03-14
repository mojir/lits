import type { SourceCodeInfo } from '../../tokenizer/token'

export function getSourceCodeInfo(anyValue: any, sourceCodeInfo?: SourceCodeInfo): SourceCodeInfo | undefined {
  // eslint-disable-next-line ts/no-unsafe-return, ts/no-unsafe-member-access
  return anyValue?.sourceCodeInfo ?? sourceCodeInfo
}
