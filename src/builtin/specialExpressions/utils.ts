import { AstNode, ParseArgument } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'

export type FunctionArguments = {
  mandatoryArguments: string[]
  optionalArguments: Array<{
    name: string
    defaultValue?: AstNode
  }>
  restArgument?: string
}
export function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parseArgument: ParseArgument,
): [number, FunctionArguments] {
  const args: FunctionArguments = {
    mandatoryArguments: [],
    optionalArguments: [],
  }
  const argNames: Record<string, true> = {}
  let state: 'mandatory' | 'optional' | 'rest' = 'mandatory'
  let token = asNotUndefined(tokens[position])
  while (!(token.type === 'paren' && token.value === ')')) {
    const [newPosition, node] = parseArgument(tokens, position)
    position = newPosition
    token = asNotUndefined(tokens[position])

    if (node.type === 'Modifier') {
      switch (node.value) {
        case '&optional':
          if (state === 'rest') {
            throw Error('&optional cannot appear after &rest')
          }
          if (state === 'optional') {
            throw Error('&optional can only appear once')
          }
          state = 'optional'
          break
        case '&rest':
          if (state === 'rest') {
            throw Error('&rest can only appear once')
          }
          if (state === 'optional' && args.optionalArguments.length === 0) {
            throw Error('No optional arguments where spcified')
          }
          state = 'rest'
          break
      }
    } else {
      if (argNames[node.name]) {
        throw Error(`Duplicate argument "${node.name}"`)
      } else {
        argNames[node.name] = true
      }

      if (Object.getOwnPropertyDescriptor(node, 'defaultValue')) {
        if (state !== 'optional') {
          throw Error('Cannot specify default value if not an optional argument')
        }
        args.optionalArguments.push({
          name: node.name,
          defaultValue: node.defaultValue,
        })
      } else {
        switch (state) {
          case 'mandatory':
            args.mandatoryArguments.push(node.name)
            break
          case 'optional':
            args.optionalArguments.push({
              name: node.name,
              defaultValue: undefined,
            })
            break
          case 'rest':
            if (args.restArgument) {
              throw Error('Can only specify one rest argument')
            }
            args.restArgument = node.name
            break
        }
      }
    }
  }

  if (state === 'rest' && !Object.getOwnPropertyDescriptor(args, 'restArgument')) {
    throw Error('Missing rest argument name')
  }
  if (state === 'optional' && args.optionalArguments.length === 0) {
    throw Error('No optional arguments where spcified')
  }

  position += 1
  return [position, args]
}
