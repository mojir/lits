import type { FunctionReference } from '..'
import type { ObjectApiName } from '../api'

export const objectReference: Record<ObjectApiName, FunctionReference<'Object'>> = {
  'dissoc': {
    title: 'dissoc',
    category: 'Object',
    linkName: 'dissoc',
    returns: {
      type: 'object',
    },
    args: {
      obj: {
        type: 'object',
      },
      key: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['obj', 'key'] },
    ],
    description: 'Return shallow copy of $obj with $key deleted.',
    examples: [
      '(dissoc (object :x 10 :y 20) :x)',
      '(dissoc { :x 10 } :y)',
      `
(def o { :a 5 }) (dissoc o :a)
o`,
    ],
  },
  'object': {
    title: 'object',
    category: 'Object',
    linkName: 'object',
    clojureDocs: null,
    returns: {
      type: 'object',
    },
    args: {
      kvps: {
        type: 'any',
        rest: true,
        description: 'key - value pairs, where key is a string',
      },
    },
    variants: [
      { argumentNames: ['kvps'] },
    ],
    description: 'Constructs a new object. Object members are created from the $kvps key-value pairs. Requires an even number of arguments.',
    examples: [
      '(object)',
      '(object :x 10 :y true :z "A string")',
      '{}',
      '{:a 1 :b 2}',
    ],
  },
  'keys': {
    title: 'keys',
    category: 'Object',
    linkName: 'keys',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      obj: {
        type: 'object',
      },
    },
    variants: [
      { argumentNames: ['obj'] },
    ],
    description: 'Returns array of all keys in $obj.',
    examples: [
      '(keys (object))',
      '(keys (object :x 10 :y true :z "A string"))',
    ],
  },
  'vals': {
    title: 'vals',
    category: 'Object',
    linkName: 'vals',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      obj: {
        type: 'object',
      },
    },
    variants: [
      { argumentNames: ['obj'] },
    ],
    description: 'Returns array of all values in $obj.',
    examples: [
      '(vals (object))',
      '(vals (object :x 10 :y true :z "A string"))',
    ],
  },
  'entries': {
    title: 'entries',
    category: 'Object',
    linkName: 'entries',
    clojureDocs: null,
    returns: {
      type: 'array',
    },
    args: {
      obj: {
        type: 'object',
      },
    },
    variants: [
      { argumentNames: ['obj'] },
    ],
    description: 'Returns nested array of all key - value pairs in $obj.',
    examples: [
      '(entries (object))',
      '(entries (object :x 10 :y true :z "A string"))',
    ],
  },
  'find': {
    title: 'find',
    category: 'Object',
    linkName: 'find',
    returns: {
      type: ['array', 'null'],
    },
    args: {
      obj: {
        type: 'object',
      },
      key: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['obj', 'key'] },
    ],
    description: 'Returns entry (key-value pair) for $key, or `null` if $key not present in $obj.',
    examples: [
      '(find (object :a 1 :b 2) :b)',
      '(find (object :a 1 :b 2) :c)',
    ],
  },
  'merge': {
    title: 'merge',
    category: 'Object',
    linkName: 'merge',
    returns: {
      type: 'object',
    },
    args: {
      objs: {
        type: 'object',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['objs'] },
    ],
    description: `Returns a new object created by merging together all arguments.

If two keys appears in more than one object the value from the last object is used.  
If no arguments are provided \`null\` is returned.`,
    examples: [
      '(merge (object :x 10) (object :y 20))',
      '(merge (object :x 10) (object :x 15 :y 20))',
    ],
  },
  'merge-with': {
    title: 'merge-with',
    category: 'Object',
    linkName: 'merge-with',
    returns: {
      type: 'object',
    },
    args: {
      objs: {
        type: 'object',
        rest: true,
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['objs', 'fn'] },
    ],
    description: `
Returns a new object created by merging together all arguments.
If two keys appears in more than one object $fn is used to calculate the new value.

If no arguments are provided \`null\` is returned.`,
    examples: [
      '(merge-with (object :x 10) (object :y 20) +)',
      '(merge-with (object :x 10) (object :x 15 :y 20) +)',
      '(merge-with (object :x 10) (object :x 20) (object :x 30) (object :x 40) -)',
    ],
  },
  'zipmap': {
    title: 'zipmap',
    category: 'Object',
    linkName: 'zipmap',
    returns: {
      type: 'object',
    },
    args: {
      keys: {
        type: 'string',
        array: true,
      },
      values: {
        type: 'any',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['keys', 'values'] },
    ],
    description: 'Returns a new object created by mapping $keys to $values.',
    examples: [
      '(zipmap [:a :b :c] [10 null [1 2 3]])',
      '(zipmap [:a :b :c] [1])',
      '(zipmap [] [10 null [1 2 3]])',
    ],
  },
  'select-keys': {
    title: 'select-keys',
    category: 'Object',
    linkName: 'select-keys',
    returns: {
      type: 'object',
    },
    args: {
      obj: {
        type: 'object',
      },
      keys: {
        type: 'string',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['obj', 'keys'] },
    ],
    description: 'Returns an object containing only those entries in $obj whose key is in $keys.',
    examples: [
      '(select-keys {:a 1 :b 2 :c 3} [:a :b])',
      '(select-keys {:a 1} [:a :b])',
    ],
  },
}
