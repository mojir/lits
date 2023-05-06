import { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { version } from '../../../version'
import { any, assertNumberOfParams, litsFunction, number, string } from '../../../utils/assertion'
import { ContextStack } from '../../../ContextStack'
import { Context, ContextEntry } from '../../../ContextStack/interface'
const uuidTemplate = `xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx`
const xyRegexp = /[xy]/g

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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `not=`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `=`, debugInfo),
  },
  'equal?': {
    evaluate: ([a, b], debugInfo): boolean => {
      return deepEqual(any.as(a, debugInfo), any.as(b, debugInfo), debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `equal?`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `>`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `<`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `>=`, debugInfo),
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
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `<=`, debugInfo),
  },
  not: {
    evaluate: ([first]): boolean => !first,
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `not`, debugInfo),
  },
  'inst-ms!': {
    evaluate: (): number => {
      return Date.now()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `inst-ms!`, debugInfo),
  },
  'inst-ms->iso-date-time': {
    evaluate: ([ms], debugInfo): string => {
      number.assert(ms, debugInfo)
      return new Date(ms).toISOString()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `inst-ms->iso-date-time`, debugInfo),
  },
  'iso-date-time->inst-ms': {
    evaluate: ([dateTime], debugInfo): number => {
      string.assert(dateTime, debugInfo)
      const ms = new Date(dateTime).valueOf()
      number.assert(ms, debugInfo, { finite: true })
      return ms
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `iso-date-time->inst-ms`, debugInfo),
  },
  'write!': {
    evaluate: (params, debugInfo): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0) {
        return any.as(params[params.length - 1], debugInfo)
      }

      return null
    },
    validateArity: () => undefined,
  },
  'debug!': {
    evaluate: (params, debugInfo, contextStack): Any => {
      if (params.length === 0) {
        // eslint-disable-next-line no-console
        console.warn(`*** LITS DEBUG ***\n${contextStackToString(contextStack)}\n`)
        return null
      }
      // eslint-disable-next-line no-console
      console.warn(`*** LITS DEBUG ***\n${JSON.stringify(params[0], null, 2)}\n`)
      return any.as(params[0], debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ max: 1 }, arity, `debug!`, debugInfo),
  },
  boolean: {
    evaluate: ([value]): boolean => {
      return !!value
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `boolean`, debugInfo),
  },
  compare: {
    evaluate: ([a, b]): number => {
      return compare(a, b)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `compare`, debugInfo),
  },
  'uuid!': {
    evaluate: (): string => {
      return uuidTemplate.replace(xyRegexp, character => {
        const randomNbr = Math.floor(Math.random() * 16)
        const newValue = character === `x` ? randomNbr : (randomNbr & 0x3) | 0x8
        return newValue.toString(16)
      })
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `uuid!`, debugInfo),
  },
  'lits-version!': {
    evaluate: (): Any => {
      return version
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `lits-version!`, debugInfo),
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
