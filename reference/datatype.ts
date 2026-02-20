import type { DatatypeName } from './api'
import type { DatatypeReference } from '.'

export const datatype: Record<DatatypeName, DatatypeReference> = {
  '-type-number': {
    datatype: true,
    title: 'number',
    category: 'datatype',
    description: 'A `number`',
    examples: [
      '42',
      '3.14',
    ],
  },
  '-type-string': {
    datatype: true,
    title: 'string',
    category: 'datatype',
    description: 'A `string`',
    examples: [
      '"hello"',
      '""',
    ],
  },
  '-type-object': {
    datatype: true,
    title: 'object',
    category: 'datatype',
    description: 'An `object`, a collection of key-value pairs where keys are `strings`',
    examples: [
      '{}',
      '{ a: 1, b: 2}',
    ],
  },
  '-type-array': {
    datatype: true,
    title: 'array',
    category: 'datatype',
    description: 'An `array`',
    examples: [
      '[]',
      '[1, 2, 3]',
      '["a", null, true]',
    ],
  },
  '-type-vector': {
    datatype: true,
    title: 'vector',
    category: 'datatype',
    description: 'An `array` of `numbers`',
    examples: [
      '[]',
      '[1, 2, 3]',
    ],
  },
  '-type-matrix': {
    datatype: true,
    title: 'matrix',
    category: 'datatype',
    description: 'A `matrix`, a two-dimensional `array` with `numbers` where each row has the same number of columns. A `matrix` is also a `grid`.',
    examples: [
      '[[]]',
      '[[1, 2], [3, 4]]',
      '[[1, 2], [3, 4], [5, 6]]',
    ],
  },
  '-type-grid': {
    datatype: true,
    title: 'grid',
    category: 'datatype',
    description: 'A `grid`, a two-dimensional `array` where each row has the same number of columns',
    examples: [
      '[[]]',
      '[[1, 2], [3, 4]]',
      '[["a", "b"], [3, 4], [5, 6]]',
    ],
  },
  '-type-boolean': {
    datatype: true,
    title: 'boolean',
    category: 'datatype',
    description: 'A `boolean`',
    examples: [
      'true',
      'false',
    ],
  },
  '-type-function': {
    datatype: true,
    title: 'function',
    category: 'datatype',
    description: 'A `function`',
    examples: [
      'x -> x + 1',
      '(a, b, c) -> (a + b) * c',
      '() -> 42',
      '-> $1 + $2',
    ],
  },
  '-type-integer': {
    datatype: true,
    title: 'integer',
    category: 'datatype',
    description: 'An `integer`',
    examples: [
      '42',
      '-42',
    ],
  },
  '-type-any': {
    datatype: true,
    title: 'any',
    category: 'datatype',
    description: '`Any` value',
    examples: [
      '42',
      '"hello"',
      'true',
      'null',
    ],
  },
  '-type-null': {
    datatype: true,
    title: 'null',
    category: 'datatype',
    description: 'The value `null`',
    examples: [
      'null',
    ],
  },
  '-type-collection': {
    datatype: true,
    title: 'collection',
    category: 'datatype',
    description: 'A collection, an `object`, an `array` or a `string`',
    examples: [
      '{ foo: 42 }',
      '[1, 2, 3]',
      '"hello"',
    ],
  },
  '-type-sequence': {
    datatype: true,
    title: 'sequence',
    category: 'datatype',
    description: 'A sequence, an `array` or a `string`',
    examples: [
      '[1, 2, 3]',
      '"hello"',
    ],
  },
  '-type-regexp': {
    datatype: true,
    title: 'regexp',
    category: 'datatype',
    description: 'A regular expression',
    examples: [
      'regexp("^\\\\s*(.*)$")',
      '#"albert"ig',
    ],
  },
  '-type-never': {
    datatype: true,
    title: 'never',
    category: 'datatype',
    description: 'A value that can never be created',
    examples: [`
// throw("error") will never return a value
try throw("error") catch "never" end`,
    ],
  },
}
