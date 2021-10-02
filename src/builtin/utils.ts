import { AstNode, BindingNode, ParseArgument, ParseBinding } from '../parser/interface'
import { Token } from '../tokenizer/interface'
import { asNotUndefined } from '../utils'

export type FunctionArguments = {
  mandatoryArguments: string[]
  optionalArguments: Array<{
    name: string
    defaultValue?: AstNode
  }>
  restArgument?: string
  bindings: BindingNode[]
}

export function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parseArgument: ParseArgument,
  parseBinding: ParseBinding,
): [number, FunctionArguments] {
  let bindings: BindingNode[] = []
  let restArgument: string | undefined = undefined
  const mandatoryArguments: string[] = []
  const optionalArguments: Array<{
    name: string
    defaultValue?: AstNode
  }> = []
  const argNames: Record<string, true> = {}
  let state: 'mandatory' | 'optional' | 'rest' | 'bind' = 'mandatory'
  let token = asNotUndefined(tokens[position])
  while (!(token.type === 'paren' && token.value === ')')) {
    if (state === 'bind') {
      ;[position, bindings] = parseBindings(tokens, position, parseBinding)
      break
    } else {
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
            if (state === 'optional' && optionalArguments.length === 0) {
              throw Error('No optional arguments where spcified')
            }
            state = 'rest'
            break
          case '&bind':
            if (state === 'optional' && optionalArguments.length === 0) {
              throw Error('No optional arguments where spcified')
            }
            if (state === 'rest' && !restArgument) {
              throw Error('No rest argument was spcified')
            }
            state = 'bind'
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
          optionalArguments.push({
            name: node.name,
            defaultValue: node.defaultValue,
          })
        } else {
          switch (state) {
            case 'mandatory':
              mandatoryArguments.push(node.name)
              break
            case 'optional':
              optionalArguments.push({
                name: node.name,
                defaultValue: undefined,
              })
              break
            case 'rest':
              if (restArgument !== undefined) {
                throw Error('Can only specify one rest argument')
              }
              restArgument = node.name
              break
          }
        }
      }
    }
  }

  if (state === 'rest' && restArgument === undefined) {
    throw Error('Missing rest argument name')
  }
  if (state === 'optional' && optionalArguments.length === 0) {
    throw Error('No optional arguments where spcified')
  }

  position += 1

  const args: FunctionArguments = {
    mandatoryArguments,
    optionalArguments,
    restArgument,
    bindings,
  }

  return [position, args]
}

function parseBindings(tokens: Token[], position: number, parseBinding: ParseBinding): [number, BindingNode[]] {
  const bindings: BindingNode[] = []
  position += 1
  let token = asNotUndefined(tokens[position])
  while (!(token.type === 'paren' && token.value === ')')) {
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected an expression`)
    }
    const [newPosition, binding] = parseBinding(tokens, position)
    position = newPosition
    bindings.push(binding)
    token = asNotUndefined(tokens[position])
  }
  position += 1 // skip right parenthesis - end of bindings
  return [position, bindings]
}
