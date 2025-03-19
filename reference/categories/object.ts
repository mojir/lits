import type { FunctionReference } from '..'
import { type ObjectApiName, getOperatorArgs } from '../api'

export const objectReference: Record<ObjectApiName, FunctionReference<'Object'>> = {
  'dissoc': {
    title: 'dissoc',
    category: 'Object',
    linkName: 'dissoc',
    returns: {
      type: 'object',
    },
    args: {
      ...getOperatorArgs('object', 'string'),
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
      '{ x := 10, y := 20 } dissoc "y"',
      'dissoc({ x := 10, y := 20 }, "x")',
      'dissoc({ x := 10 }, "y")',
      `
let o := { a := 5 };
dissoc(o, "a");
o`,
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
      'keys({})',
      'keys({ x := 10, y := true, z := "A string" })',
      'keys(object("x", 10, "y", true, "z", "A string"))',
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
      'vals({})',
      'vals({ x := 10, y := true, z := "A string" })',
      'vals(object("x", 10, "y", true, "z", "A string"))',
    ],
  },
  'entries': {
    title: 'entries',
    category: 'Object',
    linkName: 'entries',
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
      'entries({})',
      'entries({ x := 10, y := true, z := "A string" })',
      'entries(object("x", 10, "y", true, "z", "A string"))',
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
      ...getOperatorArgs('object', 'string'),
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
      '{ a := 1, "b" := 2 } find "a"',
      'find(object("a", 1, "b", 2), "b")',
      'find(object("a", 1, "b", 2), "c")',
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
      ...getOperatorArgs('object', 'object'),
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
      '{ x := 10 } merge { y := 20 }',
      'merge(object("x", 10), object("y", 20))',
      'merge(object("x", 10), object("x", 15, "y", 20))',
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
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['objs', 'fun'] },
    ],
    description: `
Returns a new object created by merging together all arguments.
If two keys appears in more than one object $fun is used to calculate the new value.

If no arguments are provided \`null\` is returned.`,
    examples: [
      'merge-with(object("x", 10), object("y", 20), +)',
      'merge-with(object("x", 10), object("x", 15, "y", 20), +)',
      'merge-with({ x := 10 }, { x := 20 }, { x := 30 }, { x := 40 }, -)',
    ],
    noOperatorDocumentation: true,
  },
  'zipmap': {
    title: 'zipmap',
    category: 'Object',
    linkName: 'zipmap',
    returns: {
      type: 'object',
    },
    args: {
      ...getOperatorArgs('array', 'array'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a new object created by mapping $a to $b.',
    examples: [
      '["a", "b", "c"] zipmap [1, 2, 3]',
      'zipmap(["a", "b", "c"], [10, null, [1, 2, 3]])',
      'zipmap(["a", "b", "c"], [1])',
      'zipmap([], [10, null, [1, 2, 3]])',
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
      ...getOperatorArgs('object', 'array'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns an object containing only those entries in $a whose key is in $b.',
    examples: [
      '{ a := 1, b := 2, c := 3 } select-keys ["a", "b"]',
      'select-keys({ a := 1, b := 2, c := 3 }, ["a", "b"])',
      'select-keys({ a := 1 }, ["a", "b"])',
    ],
  },
}
