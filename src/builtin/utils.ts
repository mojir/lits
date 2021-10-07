import { UnexpectedTokenError } from '../errors'
import { Context } from '../evaluator/interface'
import { AstNode, BindingNode, ParseArgument, ParseBindings } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { Token } from '../tokenizer/interface'
import { asNotUndefined } from '../utils'
import { Builtin } from './interface'

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
  parseBindings: ParseBindings,
): [number, FunctionArguments] {
  let bindings: BindingNode[] = []
  let restArgument: string | undefined = undefined
  const mandatoryArguments: string[] = []
  const optionalArguments: Array<{
    name: string
    defaultValue?: AstNode
  }> = []
  const argNames: Record<string, true> = {}
  let state: `mandatory` | `optional` | `rest` | `bind` = `mandatory`
  let token = asNotUndefined(tokens[position])
  if (!(token.type === `paren` && token.value === `[`)) {
    throw new UnexpectedTokenError(`[`, token)
  }

  position += 1
  token = asNotUndefined(tokens[position])
  while (!(token.type === `paren` && token.value === `]`)) {
    if (state === `bind`) {
      ;[position, bindings] = parseBindings(tokens, position)
      break
    } else {
      const [newPosition, node] = parseArgument(tokens, position)
      position = newPosition
      token = asNotUndefined(tokens[position])

      if (node.type === `Modifier`) {
        switch (node.value) {
          case `&opt`:
            if (state === `rest`) {
              throw Error(`&opt cannot appear after &rest`)
            }
            if (state === `optional`) {
              throw Error(`&opt can only appear once`)
            }
            state = `optional`
            break
          case `&rest`:
            if (state === `rest`) {
              throw Error(`&rest can only appear once`)
            }
            if (state === `optional` && optionalArguments.length === 0) {
              throw Error(`No optional arguments where spcified`)
            }
            state = `rest`
            break
          case `&bind`:
            if (state === `optional` && optionalArguments.length === 0) {
              throw Error(`No optional arguments where spcified`)
            }
            if (state === `rest` && !restArgument) {
              throw Error(`No rest argument was spcified`)
            }
            state = `bind`
            break
        }
      } else {
        if (argNames[node.name]) {
          throw Error(`Duplicate argument "${node.name}"`)
        } else {
          argNames[node.name] = true
        }

        if (Object.getOwnPropertyDescriptor(node, `defaultValue`)) {
          if (state !== `optional`) {
            throw Error(`Cannot specify default value if not an optional argument`)
          }
          optionalArguments.push({
            name: node.name,
            defaultValue: node.defaultValue,
          })
        } else {
          switch (state) {
            case `mandatory`:
              mandatoryArguments.push(node.name)
              break
            case `optional`:
              optionalArguments.push({
                name: node.name,
                defaultValue: undefined,
              })
              break
            case `rest`:
              if (restArgument !== undefined) {
                throw Error(`Can only specify one rest argument`)
              }
              restArgument = node.name
              break
          }
        }
      }
    }
  }

  if (state === `rest` && restArgument === undefined) {
    throw Error(`Missing rest argument name`)
  }
  if (state === `optional` && optionalArguments.length === 0) {
    throw Error(`No optional arguments where spcified`)
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

export function assertNameNotDefined<T>(name: T, contextStack: Context[], builtin: Builtin): asserts name is T {
  if (typeof name !== `string`) {
    return
  }
  if (builtin.specialExpressions[name]) {
    throw Error(`Cannot define variable ${name}, it's a special expression`)
  }

  if (builtin.normalExpressions[name]) {
    throw Error(`Cannot define variable ${name}, it's a builtin function`)
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  if ((reservedNamesRecord as any)[name]) {
    throw Error(`Cannot define variable ${name}, it's a reserved name`)
  }

  const globalContext = asNotUndefined(contextStack[contextStack.length - 2])

  if (globalContext[name]) {
    throw Error(`Name already defined "${name}"`)
  }
}
