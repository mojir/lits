import type { Any, Arr, Obj } from '../../interface'
import { assertArray, assertStringArray } from '../../typeGuards/array'
import { assertFunctionLike, assertObj } from '../../typeGuards/lits'
import { asString, assertString } from '../../typeGuards/string'
import { collHasKey, toAny } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

export const objectNormalExpression: BuiltinNormalExpressions = {
  'keys': {
    evaluate: ([obj], sourceCodeInfo): string[] => {
      assertObj(obj, sourceCodeInfo)
      return Object.keys(obj)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Object',
      returns: { type: 'any', array: true },
      args: { obj: { type: 'object' } },
      variants: [{ argumentNames: ['obj'] }],
      description: 'Returns array of all keys in $obj.',
      seeAlso: ['vals', 'entries', 'zipmap', 'select-keys'],
      examples: [
        'keys({})',
        'keys({ x: 10, y: true, z: "A string" })',
        'keys(object("x", 10, "y", true, "z", "A string"))',
      ],
    },
  },

  'vals': {
    evaluate: ([obj], sourceCodeInfo): Arr => {
      assertObj(obj, sourceCodeInfo)
      return Object.values(obj)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Object',
      returns: { type: 'any', array: true },
      args: { obj: { type: 'object' } },
      variants: [{ argumentNames: ['obj'] }],
      description: 'Returns array of all values in $obj.',
      seeAlso: ['keys', 'entries', 'zipmap'],
      examples: [
        'vals({})',
        'vals({ x: 10, y: true, z: "A string" })',
        'vals(object("x", 10, "y", true, "z", "A string"))',
      ],
    },
  },

  'entries': {
    evaluate: ([obj], sourceCodeInfo): Array<[string, unknown]> => {
      assertObj(obj, sourceCodeInfo)
      return Object.entries(obj)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Object',
      returns: { type: 'array' },
      args: { obj: { type: 'object' } },
      variants: [{ argumentNames: ['obj'] }],
      description: 'Returns nested array of all key - value pairs in $obj.',
      seeAlso: ['keys', 'vals', 'zipmap', 'find'],
      examples: [
        'entries({})',
        'entries({ x: 10, y: true, z: "A string" })',
        'entries(object("x", 10, "y", true, "z", "A string"))',
      ],
    },
  },

  'find': {
    evaluate: ([obj, key], sourceCodeInfo): [string, unknown] | null => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      if (collHasKey(obj, key))
        return [key, obj[key]]

      return null
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Object',
      returns: { type: ['array', 'null'] },
      args: {
        a: { type: 'object' },
        b: { type: 'string' },
        obj: { type: 'object' },
        key: { type: 'string' },
      },
      variants: [{ argumentNames: ['obj', 'key'] }],
      description: 'Returns entry (key-value pair) for $key, or `null` if $key not present in $obj.',
      seeAlso: ['get', 'contains?', 'entries', 'Sequence-Utils.position', 'some'],
      examples: [
        '{ a: 1, "b": 2 } find "a"',
        'find(object("a", 1, "b", 2), "b")',
        'find(object("a", 1, "b", 2), "c")',
      ],
    },
  },

  'dissoc': {
    evaluate: ([obj, key], sourceCodeInfo): Any => {
      assertObj(obj, sourceCodeInfo)
      assertString(key, sourceCodeInfo)
      const newObj = { ...obj }
      delete newObj[key]
      return newObj
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Object',
      returns: { type: 'object' },
      args: {
        a: { type: 'object' },
        b: { type: 'string' },
        obj: { type: 'object' },
        key: { type: 'string' },
      },
      variants: [{ argumentNames: ['obj', 'key'] }],
      description: 'Return shallow copy of $obj with $key deleted.',
      seeAlso: ['assoc', 'select-keys'],
      examples: [
        '{ x: 10, y: 20 } dissoc "y"',
        'dissoc({ x: 10, y: 20 }, "x")',
        'dissoc({ x: 10 }, "y")',
        `
let o = { a: 5 };
dissoc(o, "a");
o`,
      ],
    },
  },

  'merge': {
    evaluate: (params, sourceCodeInfo): Any => {
      if (params.length === 0)
        return null

      const [first, ...rest] = params
      assertObj(first, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, sourceCodeInfo)
          return { ...result, ...obj }
        },
        { ...first },
      )
    },
    arity: { min: 0 },
    docs: {
      category: 'Object',
      returns: { type: 'object' },
      args: {
        a: { type: 'object' },
        b: { type: 'object' },
        objs: { type: 'object', rest: true },
      },
      variants: [{ argumentNames: ['objs'] }],
      description: `Returns a new object created by merging together all arguments.

If two keys appears in more than one object the value from the last object is used.
If no arguments are provided \`null\` is returned.`,
      seeAlso: ['merge-with', 'assoc'],
      examples: [
        '{ x: 10 } merge { y: 20 }',
        'merge(object("x", 10), object("y", 20))',
        'merge(object("x", 10), object("x", 15, "y", 20))',
      ],
    },
  },

  'merge-with': {
    evaluate: (params: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      const first = params[0]
      const fn = params.at(-1)
      const rest = params.slice(1, -1)

      assertObj(first, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      return rest.reduce(
        (result: Obj, obj) => {
          assertObj(obj, sourceCodeInfo)
          Object.entries(obj).forEach((entry) => {
            const key = asString(entry[0], sourceCodeInfo)
            const val = toAny(entry[1])
            if (collHasKey(result, key))
              result[key] = executeFunction(fn, [result[key], val], contextStack, sourceCodeInfo)
            else
              result[key] = val
          })
          return result
        },
        { ...first },
      )
    },
    arity: { min: 2 },
    docs: {
      category: 'Object',
      returns: { type: 'object' },
      args: {
        objs: { type: 'object', rest: true },
        fun: { type: 'function' },
      },
      variants: [{ argumentNames: ['objs', 'fun'] }],
      description: `
Returns a new object created by merging together all arguments.
If two keys appears in more than one object $fun is used to calculate the new value.

If no arguments are provided \`null\` is returned.`,
      seeAlso: ['merge'],
      examples: [
        'merge-with(object("x", 10), object("y", 20), +)',
        'merge-with(object("x", 10), object("x", 15, "y", 20), +)',
        'merge-with({ x: 10 }, { x: 20 }, { x: 30 }, { x: 40 }, -)',
      ],
      hideOperatorForm: true,
    },
  },

  'zipmap': {
    evaluate: ([keys, values], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      assertArray(values, sourceCodeInfo)

      const length = Math.min(keys.length, values.length)

      const result: Obj = {}

      for (let i = 0; i < length; i += 1) {
        const key = asString(keys[i], sourceCodeInfo)
        result[key] = toAny(values[i])
      }
      return result
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Object',
      returns: { type: 'object' },
      args: {
        a: { type: 'array' },
        b: { type: 'array' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns a new object created by mapping $a to $b.',
      seeAlso: ['entries', 'keys', 'vals', 'Sequence-Utils.interleave'],
      examples: [
        '["a", "b", "c"] zipmap [1, 2, 3]',
        'zipmap(["a", "b", "c"], [10, null, [1, 2, 3]])',
        'zipmap(["a", "b", "c"], [1])',
        'zipmap([], [10, null, [1, 2, 3]])',
      ],
    },
  },

  'select-keys': {
    evaluate: ([obj, keys], sourceCodeInfo): Any => {
      assertStringArray(keys, sourceCodeInfo)
      assertObj(obj, sourceCodeInfo)

      return keys.reduce((result: Obj, key) => {
        if (collHasKey(obj, key))
          result[key] = toAny(obj[key])

        return result
      }, {})
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Object',
      returns: { type: 'object' },
      args: {
        a: { type: 'object' },
        b: { type: 'array' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Returns an object containing only those entries in $a whose key is in $b.',
      seeAlso: ['dissoc', 'keys'],
      examples: [
        '{ a: 1, b: 2, c: 3 } select-keys ["a", "b"]',
        'select-keys({ a: 1, b: 2, c: 3 }, ["a", "b"])',
        'select-keys({ a: 1 }, ["a", "b"])',
      ],
    },
  },
}
