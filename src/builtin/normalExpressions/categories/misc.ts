import { AssertionError } from '../../../errors'
import { Context, ContextEntry } from '../../../evaluator/interface'
import { Arr } from '../../../interface'
import { assertLength, assertObjectOrArray, assertString, compare, deepEqual, isLispishFunction } from '../../../utils'
import { getPath } from '../../getPath'
import { BuiltinNormalExpressions } from '../../interface'
import { version } from '../../../version'

export const miscNormalExpression: BuiltinNormalExpressions = {
  'not=': {
    evaluate: (params: Arr): boolean => {
      for (let i = 0; i < params.length - 1; i += 1) {
        for (let j = i + 1; j < params.length; j += 1) {
          if (params[i] === params[j]) {
            return false
          }
        }
      }

      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  '=': {
    evaluate: ([first, ...rest]: Arr): boolean => {
      for (const param of rest) {
        if (param !== first) {
          return false
        }
      }

      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'equal?': {
    evaluate: ([a, b]: Arr): boolean => {
      return deepEqual(a, b)
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  '>': {
    evaluate: ([first, ...rest]: Arr): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) <= 0) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  '<': {
    evaluate: ([first, ...rest]: Arr): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) >= 0) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  '>=': {
    evaluate: ([first, ...rest]: Arr): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) < 0) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  '<=': {
    evaluate: ([first, ...rest]: Arr): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) > 0) {
          return false
        }
        currentValue = param
      }
      return true
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  'get-path': {
    evaluate: ([first, second]: Arr): unknown => {
      assertObjectOrArray(first)
      assertString(second)
      return getPath(first, second)
    },
    validate: node => assertLength(2, node),
  },
  not: {
    evaluate: ([first]: Arr): boolean => !first,
    validate: node => assertLength(1, node),
  },
  'inst-ms': {
    evaluate: (): number => {
      return Date.now()
    },
    validate: node => assertLength(0, node),
  },
  'write!': {
    evaluate: (params: Arr): unknown => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0) {
        return params[params.length - 1]
      }

      return undefined
    },
  },
  'debug!': {
    evaluate: (_, contextStack): undefined => {
      // eslint-disable-next-line no-console
      console.log(`*** LISPISH DEBUG ***\n\n${contextstackToString(contextStack)}`)
      return undefined
    },
    validate: node => assertLength(0, node),
  },
  boolean: {
    evaluate: ([value]): boolean => {
      return !!value
    },
    validate: node => assertLength(1, node),
  },
  compare: {
    evaluate: ([a, b]): number => {
      return compare(a, b)
    },
    validate: node => assertLength(2, node),
  },
  assert: {
    evaluate: (params): unknown => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      assertString(message)
      if (!value) {
        throw new AssertionError(message)
      }
      return value
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  'lispish-version': {
    evaluate: (): unknown => {
      return version
    },
    validate: node => assertLength(0, node),
  },
}

function contextstackToString(contextStack: Context[]): string {
  return [...contextStack].reverse().reduce((result, context, index) => {
    return `${result}Context ${index}${
      index === 0 ? ` - Import context` : index === 1 ? ` - Global context` : ``
    }\n${contextToString(context)}\n`
  }, ``)
}

function contextToString(context: Context) {
  if (Object.keys(context).length === 0) {
    return `  <empty>\n`
  }
  const maxKeyLength = Math.max(...Object.keys(context).map(key => key.length))
  return Object.entries(context).reduce((result, entry) => {
    const key = `${entry[0]}`.padEnd(maxKeyLength + 2, ` `)
    return `${result}  ${key}${valueToString(entry[1])}\n`
  }, ``)
}

function valueToString(contextEntry: ContextEntry): string {
  const { value } = contextEntry
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const name: string | undefined = (value as any).name
  if (isLispishFunction(value)) {
    if (name) {
      return `<${value.type} function ${name}>`
    } else {
      return `<${value.type} function λ>`
    }
  }
  return JSON.stringify(contextEntry.value)
}
