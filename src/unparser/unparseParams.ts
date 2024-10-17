import type { AstNode } from '../parser/interface'
import type { UnparseOptions } from './UnparseOptions'
import { ensureNewlineSeparator, ensureSpaceSeparator } from './utils'

export function unparseParams({
  params,
  options,
  prefix,
  inline,
  endBracket,
  body = false,
  name = '',
  noMultilineInline = false,
  pairs = false,
}: {
  params: AstNode[]
  prefix: string
  inline: boolean
  endBracket: string
  options: UnparseOptions
  body?: boolean
  name?: string
  noMultilineInline?: boolean
  pairs?: boolean
}): string {
  // If no parameters, return the expression with brackets
  if (params.length === 0)
    return `${prefix}${endBracket}`

  // 1. Try to unparse the parameters as a single line
  try {
    const unparsedParams = pairs
      ? unparseSingleLinePairs(params, options.inline().lock())
      : unparseSingleLineParams(params, options.inline().lock())
    if (!unparsedParams.includes('\n'))
      return options.assertNotOverflown(`${prefix} ${unparsedParams}${endBracket}`)
  }
  catch (error) {
    // If locked, we do not try anything else
    if (options.locked)
      throw error
  }

  if (pairs) {
    if (name && !name.includes('\n')) {
      try {
        const unparsedParams = unparseMultilinePairwise(params, options.inline().inc(name.length + 2).lock())
        const result = options.assertNotOverflown(`${prefix} ${unparsedParams}${endBracket}`)
        return result
      }
      catch {
      }
    }
    try {
      const unparsedParams = unparseMultilinePairwise(params, options.noInline().inc(body ? 2 : 1))
      return options.assertNotOverflown(ensureNewlineSeparator(prefix, `${unparsedParams}${endBracket}`))
    }
    catch {
    }
  }

  // 2. Try to unparse the parameters in multiple lines, first parameter on the same line
  // e.g. (round 1 2 3 4 5)
  // ==>  (round 1
  //             2
  //             3
  //             4
  //             5)
  else if (inline) {
    const newOptions = options.inc(name.length + 2).lock()
    try {
      const firstParam = options.unparse(params[0]!, noMultilineInline ? newOptions.noInline() : newOptions.inline())
      // If the first parameter is multiline, fallback to option 3
      if (!firstParam.startsWith('\n')) {
        const indentedParams = unparseMultilineParams(params.slice(1), newOptions.noInline())
        return noMultilineInline
          ? options.assertNotOverflown(
            `${ensureNewlineSeparator(prefix, ensureNewlineSeparator(firstParam, indentedParams))}${endBracket}`,
          )
          : options.assertNotOverflown(
            `${prefix} ${ensureNewlineSeparator(firstParam, indentedParams)}${endBracket}`,
          )
      }
    }
    catch {
    // Try option 3
    }
  }

  // 3. Try to unparse the parameters in multiple lines
  // e.g. (round 1 2 3 4 5)
  // ==>  (round
  //       1
  //       2
  //       3
  //       4
  //       5)
  return `${ensureNewlineSeparator(
    prefix,
    unparseMultilineParams(params, options.noInline().inc(body ? 2 : 1)),
  )}${endBracket}`
}

export function unparseSingleLineParams(params: AstNode[], options: UnparseOptions): string {
  return params.reduce<string>((acc, param) =>
    ensureSpaceSeparator(acc, options.unparse(param, options.inline())), '')
}

export function unparseSingleLinePairs(params: AstNode[], options: UnparseOptions): string {
  return params.reduce<string>((acc, param, index) => {
    if (index > 0 && index % 2 === 0)
      acc += ','

    return ensureSpaceSeparator(acc, options.unparse(param, options.inline()))
  }, '')
}

export function unparseMultilineParams(params: AstNode[], options: UnparseOptions): string {
  return params.reduce<string>((acc, param, index) => ensureNewlineSeparator(acc, options.unparse(
    param,
    index === 0 && options.inlined ? options.inline() : options.noInline(),
  )), '')
}

export function unparseMultilinePairwise(params: AstNode[], options: UnparseOptions): string {
  let keyLength: number
  return params.reduce<string>((acc, param, index) => {
    if (index % 2 === 0) {
      let key = options.unparse(
        param,
        options.inline(),
      )
      const leadingMetaTokens = param.debugData?.token.debugData?.metaTokens.leadingMetaTokens
      if (index === 0 && options.inlined && leadingMetaTokens && leadingMetaTokens.length > 0)
        throw new Error('First key with leading meta tokens is not allowed.')

      key = key.trimStart()
      keyLength = key.length
      key = index === 0 && options.inlined ? key : `${options.indent}${key}`
      if (key.includes('\n'))
        throw new Error('Key with new line character is not allowed.')

      return index > 0 ? ensureNewlineSeparator(acc, key) : `${acc}${key}`
    }
    else {
      const value = options.unparse(
        param,
        options.inline().inc(keyLength + 1),
      )
      return ensureSpaceSeparator(acc, value)
    }
  }, '')
}
