module.exports = {
  'has-attr': {
    name: 'has-attr',
    category: 'Object',
    linkName: 'has-attr',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `true` if `object` has an attribute named `attr`, otherwise returns `false`.',
    longDescription: 'Returns `true` if `object` has an attribute named `attr`, otherwise returns `false`.',
    examples: [
      '(has-attr (object "a" 10 "b" 20) "a")',
      '(has-attr (object "a" 10 "b" undefined) "b")',
      '(has-attr (object "a" 10 "b" undefined) "c")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'get-attr': {
    name: 'get-attr',
    category: 'Object',
    linkName: 'get-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription:
      "Returns the value of `object`'s attribute `attr`. Returns `undefined` if `object` has no attribute named `attr`.",
    longDescription:
      "Returns the value of `object`'s attribute `attr`. Returns `undefined` if `object` has no attribute named `attr`.",
    examples: [
      '(get-attr (object "a" 10 "b" 20) "a")',
      '(get-attr (object "a" 10 "b" undefined) "b")',
      '(get-attr (object "a" 10 "b" undefined) "c")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'del-attr': {
    name: 'del-attr',
    category: 'Object',
    linkName: 'del-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription: 'Deletes the attribute `attr` from `object`.',
    longDescription: 'Deletes the attribute `attr` from `object`.',
    examples: [
      '(del-attr (object "x" 10) "x")',
      '(del-attr (object "x" 10) "y")',
      '(setq o (object "a" 5)) (del-attr o "a") o',
      '(setq o (object "a" 5)) (del-attr o "b") o',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'set-attr': {
    name: 'set-attr',
    category: 'Object',
    linkName: 'set-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Sets an attribute on `object`. Returns `value`.',
    longDescription: 'Sets an attribute on `object`. Returns `value`.',
    examples: [
      '(set-attr (object "x" 10) "a" 10)',
      '(setq o (object)) (set-attr o "a" 10) o',
      '(setq o (object "a" 5)) (set-attr o "a" 10) o',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  object: {
    name: 'object',
    category: 'Object',
    linkName: 'object',
    returns: {
      type: 'object',
    },
    arguments: [
      {
        name: '[key value]',
        type: '[string any]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Constructs a new object. Object members are created from `key` - `value` pairs.',
    longDescription:
      'Constructs a new object. Object members are created from `key` - `value` pairs. Requires an even number of arguments.',
    examples: [`(object)`, `(object "x" 10 "y" true "z" "A string")`],
    specialExpression: false,
    sideEffects: [],
  },
  keys: {
    name: 'keys',
    category: 'Object',
    linkName: 'keys',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns list of all keys in `object`.',
    longDescription: 'Returns list of all keys in `object`.',
    examples: [`(keys (object))`, `(keys (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  values: {
    name: 'values',
    category: 'Object',
    linkName: 'values',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns list of all values in `object`.',
    longDescription: 'Returns list of all values in `object`.',
    examples: [`(values (object))`, `(values (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  entries: {
    name: 'entries',
    category: 'Object',
    linkName: 'entries',
    returns: {
      type: 'list of [key value] - paris',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns nested list of all key - value pairs in `object`.',
    longDescription: 'Returns nested list of all key - value pairs in `object`.',
    examples: [`(entries (object))`, `(entries (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  merge: {
    name: 'merge',
    category: 'Object',
    linkName: 'merge',
    returns: {
      type: 'object',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
        description: 'one or many',
      },
    ],
    shortDescription: 'Returns a new object created by merging together all arguments.',
    longDescription: 'Returns a new object created by merging together all arguments.',
    examples: [`(merge (object "x" 10) (object "y" 20))`, `(merge (object "x" 10) (object "x" 15 "y" 20))`],
    specialExpression: false,
    sideEffects: [],
  },
}
