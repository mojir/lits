import { AssertionError } from '../../../errors'
import { Context, ContextEntry, ContextStack } from '../../../evaluator/interface'
import { Any } from '../../../interface'
import { assertLength, assertString, compare, deepEqual } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { version } from '../../../version'
import { any, litsFunction } from '../../../utils/assertion'

export const miscNormalExpression: BuiltinNormalExpressions = {
  'not=': {
    evaluate: (params): boolean => {
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
    evaluate: ([first, ...rest]): boolean => {
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
    evaluate: ([a, b], meta): boolean => {
      return deepEqual(any.as(a, meta), any.as(b, meta), meta)
    },
    validate: node => assertLength({ min: 1 }, node),
  },
  '>': {
    evaluate: ([first, ...rest]): boolean => {
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
    evaluate: ([first, ...rest]): boolean => {
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
    evaluate: ([first, ...rest]): boolean => {
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
    evaluate: ([first, ...rest]): boolean => {
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
  not: {
    evaluate: ([first]): boolean => !first,
    validate: node => assertLength(1, node),
  },
  'inst-ms': {
    evaluate: (): number => {
      return Date.now()
    },
    validate: node => assertLength(0, node),
  },
  'write!': {
    evaluate: (params, meta): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0) {
        return any.as(params[params.length - 1], meta)
      }

      return null
    },
  },
  'debug!': {
    evaluate: (params, meta, contextStack): Any => {
      if (params.length === 0) {
        // eslint-disable-next-line no-console
        console.warn(`*** LITS DEBUG ***\n${contextStackToString(contextStack)}\n`)
        return null
      }
      // eslint-disable-next-line no-console
      console.warn(`*** LITS DEBUG ***\n${JSON.stringify(params[0], null, 2)}\n`)
      return any.as(params[0], meta)
    },
    validate: node => assertLength({ max: 1 }, node),
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
    evaluate: (params, meta): Any => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      assertString(message, meta)
      if (!value) {
        throw new AssertionError(message, meta)
      }
      return any.as(value, meta)
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  'lits-version': {
    evaluate: (): Any => {
      return version
    },
    validate: node => assertLength(0, node),
  },
}

function contextStackToString(contextStack: ContextStack): string {
  return contextStack.stack.reduce((result, context, index) => {
    return `${result}Context ${index}${
      context === contextStack.globalContext ? ` - Global context` : ``
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
  if (litsFunction.is(value)) {
    if (name) {
      return `<${value.type} function ${name}>`
    } else {
      return `<${value.type} function λ>`
    }
  }
  return JSON.stringify(contextEntry.value)
}
