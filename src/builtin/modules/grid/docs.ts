import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'every?': {
    category: 'Grid',
    description: 'Checks if all elements in a grid satisfy a predicate. Returns true only if the predicate returns true for every element in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "every?" shadows a builtin function\nlet { every? as grid-every? } = import("Grid");\ngrid-every?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], string?)',
      '// Using "as" alias because "every?" shadows a builtin function\nlet { every? as grid-every? } = import("Grid");\ngrid-every?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], string?)',
      '// Using "as" alias because "every?" shadows a builtin function\nlet { every? as grid-every? } = import("Grid");\ngrid-every?([\n  [1, 2],\n  [3, 4],\n], string?)',
    ],
    seeAlso: ['Collection-Utils.every?', 'Grid.some?', 'Grid.every-row?', 'Grid.every-col?'],
  },
  'some?': {
    category: 'Grid',
    description: 'Checks if any element in a grid satisfies a predicate. Returns true if the predicate returns true for at least one element in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { some? } = import("Grid");\nsome?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], string?)',
      'let { some? } = import("Grid");\nsome?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], string?)',
      'let { some? } = import("Grid");\nsome?([\n  [1, 2],\n  [3, 4],\n], string?)',
    ],
    seeAlso: ['Collection-Utils.any?', 'Grid.every?', 'Grid.some-row?', 'Grid.some-col?'],
  },
  'every-row?': {
    category: 'Grid',
    description: 'Checks if all rows in a grid satisfy a predicate. Returns true only if the predicate returns true for every row in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { every-row? } = import("Grid");\nevery-row?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> string?($[0]))',
      'let { every-row? } = import("Grid");\nevery-row?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> string?($[0]))',
      'let { every-row? } = import("Grid");\nevery-row?([\n  [1, 2],\n  [3, 4],\n], -> string?($[0]))',
    ],
    seeAlso: ['Grid.some-row?', 'Grid.every-col?', 'Grid.every?'],
  },
  'some-row?': {
    category: 'Grid',
    description: 'Checks if any row in a grid satisfies a predicate. Returns true if the predicate returns true for at least one row in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { some-row? } = import("Grid");\nsome-row?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $ contains? "Albert")',
      'let { some-row? } = import("Grid");\nsome-row?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> $ contains? "Albert")',
      'let { some-row? } = import("Grid");\nsome-row?([\n  [1, 2],\n  [3, 4],\n], -> $ contains? "Albert")',
    ],
    seeAlso: ['Grid.every-row?', 'Grid.some-col?', 'Grid.some?'],
  },
  'every-col?': {
    category: 'Grid',
    description: 'Checks if all columns in a grid satisfy a predicate. Returns true only if the predicate returns true for every column in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { every-col? } = import("Grid");\nevery-col?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> string?($[0]))',
      'let { every-col? } = import("Grid");\nevery-col?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> string?($[0]))',
      'let { every-col? } = import("Grid");\nevery-col?([\n  [1, 2],\n  [3, 4],\n], -> string?($[0]))',
    ],
    seeAlso: ['Grid.some-col?', 'Grid.every-row?', 'Grid.every?'],
  },
  'some-col?': {
    category: 'Grid',
    description: 'Checks if any column in a grid satisfies a predicate. Returns true if the predicate returns true for at least one column in the grid.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { some-col? } = import("Grid");\nsome-col?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $ contains? "Albert")',
      'let { some-col? } = import("Grid");\nsome-col?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> $ contains? "Albert")',
      'let { some-col? } = import("Grid");\nsome-col?([\n  [1, 2],\n  [3, 4],\n], -> $ contains? "Albert")',
    ],
    seeAlso: ['Grid.every-col?', 'Grid.some-row?', 'Grid.some?'],
  },
  'row': {
    category: 'Grid',
    description: 'Returns the row at index $a in the grid $b.',
    returns: {
      type: 'any',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'number',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { row } = import("Grid");\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 0)',
      'let { row } = import("Grid");\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
      'let { row } = import("Grid");\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 2)',
    ],
    seeAlso: ['Grid.col', 'Grid.shape'],
  },
  'col': {
    category: 'Grid',
    description: 'Returns the column at index $a in the grid $b.',
    returns: {
      type: 'any',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'number',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { col } = import("Grid");\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 0)',
      'let { col } = import("Grid");\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
      'let { col } = import("Grid");\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 2)',
    ],
    seeAlso: ['Grid.row', 'Grid.shape'],
  },
  'shape': {
    category: 'Grid',
    description: 'Returns the shape of the grid `g` as a `vector` of two numbers, where the first number is the number of rows and the second number is the number of columns.',
    returns: {
      type: 'vector',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to get the shape of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { shape } = import("Grid");\nshape([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { shape } = import("Grid");\nshape([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { shape } = import("Grid");\nshape([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.row', 'Grid.col', 'Grid.reshape'],
  },
  'fill': {
    category: 'Grid',
    description: 'Creates a grid of the specified size, filled with the specified value.',
    returns: {
      type: 'grid',
    },
    args: {
      rows: {
        type: 'integer',
        description: 'The number of rows in the grid.',
      },
      cols: {
        type: 'integer',
        description: 'The number of columns in the grid.',
      },
      value: {
        type: 'any',
        description: 'The value to fill the grid with.',
      },
    },
    variants: [
      {
        argumentNames: [
          'rows',
          'cols',
          'value',
        ],
      },
    ],
    examples: [
      'let { fill } = import("Grid");\nfill(2, 3, 0)',
      'let { fill } = import("Grid");\nfill(2, 3, "x")',
    ],
    seeAlso: ['Grid.generate', 'Grid.from-array', 'Vector.fill'],
  },
  'generate': {
    category: 'Grid',
    description: 'Generates a grid of the specified size, where each element is generated by the provided function.',
    returns: {
      type: 'grid',
    },
    args: {
      rows: {
        type: 'number',
        description: 'The number of rows in the grid.',
      },
      cols: {
        type: 'number',
        description: 'The number of columns in the grid.',
      },
      fn: {
        type: 'function',
        description: 'The function to generate the grid. It takes two arguments: the row index and the column index.',
      },
    },
    variants: [
      {
        argumentNames: [
          'rows',
          'cols',
          'fn',
        ],
      },
    ],
    examples: [
      'let { generate } = import("Grid");\ngenerate(3, 3, (i, j) -> i + j)',
    ],
    seeAlso: ['Grid.fill', 'Grid.from-array', 'Vector.generate'],
  },
  'reshape': {
    category: 'Grid',
    description: 'Reshapes the grid `a` into a new grid with the specified number of rows `b`. The number of columns is automatically calculated based on the total number of elements in the grid.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'number',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { reshape } = import("Grid");\nreshape([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], 2)',
    ],
    seeAlso: ['Grid.shape', 'Grid.from-array'],
  },
  'transpose': {
    category: 'Grid',
    description: 'Transposes the grid `g`, swapping its rows and columns.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to transpose.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { transpose } = import("Grid");\ntranspose([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { transpose } = import("Grid");\ntranspose([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { transpose } = import("Grid");\ntranspose([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.flip-h', 'Grid.flip-v', 'Grid.rotate'],
  },
  'flip-h': {
    category: 'Grid',
    description: 'Flips the grid `g` horizontally.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to flip horizontally.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { flip-h } = import("Grid");\nflip-h([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { flip-h } = import("Grid");\nflip-h([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { flip-h } = import("Grid");\nflip-h([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.flip-v', 'Grid.transpose', 'Grid.rotate', 'Grid.reverse-cols'],
  },
  'flip-v': {
    category: 'Grid',
    description: 'Flips the grid `g` vertically.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to flip vertically.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { flip-v } = import("Grid");\nflip-v([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { flip-v } = import("Grid");\nflip-v([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { flip-v } = import("Grid");\nflip-v([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.flip-h', 'Grid.transpose', 'Grid.rotate', 'Grid.reverse-rows'],
  },
  'rotate': {
    category: 'Grid',
    description: 'Rotates the grid `g` by the specified angle. The angle is given in terms of 90-degree rotations. Positive values rotate the grid clockwise, while negative values rotate it counterclockwise.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], 1)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], 2)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], 3)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], 4)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], -1)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], -2)',
      'let { rotate } = import("Grid");\nrotate([\n  [1, 2],\n  [3, 4],\n], -3)',
    ],
    seeAlso: ['Grid.transpose', 'Grid.flip-h', 'Grid.flip-v'],
  },
  'reverse-rows': {
    category: 'Grid',
    description: 'Reverses the order of rows in the grid `g`.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to reverse rows.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { reverse-rows } = import("Grid");\nreverse-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { reverse-rows } = import("Grid");\nreverse-rows([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { reverse-rows } = import("Grid");\nreverse-rows([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.reverse-cols', 'Grid.flip-v'],
  },
  'reverse-cols': {
    category: 'Grid',
    description: 'Reverses the order of columns in the grid `g`.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to reverse columns.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { reverse-cols } = import("Grid");\nreverse-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { reverse-cols } = import("Grid");\nreverse-cols([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { reverse-cols } = import("Grid");\nreverse-cols([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.reverse-rows', 'Grid.flip-h'],
  },
  'slice': {
    category: 'Grid',
    description: 'Slices the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to slice.',
      },
      begin: {
        type: 'vector',
        description: 'The starting index of the slice as a vector of two numbers: `[row, col]`.',
      },
      stop: {
        type: 'vector',
        description: 'Optional ending index of the slice as a vector of two numbers: `[row, col]`.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'begin',
        ],
      },
      {
        argumentNames: [
          'g',
          'begin',
          'stop',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "slice" shadows a builtin function\nlet { slice as grid-slice } = import("Grid");\ngrid-slice([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [1, 1], [2, 2])',
      '// Using "as" alias because "slice" shadows a builtin function\nlet { slice as grid-slice } = import("Grid");\ngrid-slice([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [1, 1])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.slice-rows', 'Grid.slice-cols'],
  },
  'slice-rows': {
    category: 'Grid',
    description: 'Slices rows of the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to slice.',
      },
      begin: {
        type: 'number',
        description: 'The starting index of the slice.',
      },
      stop: {
        type: 'number',
        description: 'Optional ending index of the slice.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'begin',
        ],
      },
      {
        argumentNames: [
          'g',
          'begin',
          'stop',
        ],
      },
    ],
    examples: [
      'let { slice-rows } = import("Grid");\nslice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { slice-rows } = import("Grid");\nslice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.slice', 'Grid.slice-cols', 'Grid.splice-rows'],
  },
  'slice-cols': {
    category: 'Grid',
    description: 'Slices columns of the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to slice.',
      },
      begin: {
        type: 'number',
        description: 'The starting index of the slice.',
      },
      stop: {
        type: 'number',
        description: 'Optional ending index of the slice.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'begin',
        ],
      },
      {
        argumentNames: [
          'g',
          'begin',
          'stop',
        ],
      },
    ],
    examples: [
      'let { slice-cols } = import("Grid");\nslice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { slice-cols } = import("Grid");\nslice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.slice', 'Grid.slice-rows', 'Grid.splice-cols'],
  },
  'splice-rows': {
    category: 'Grid',
    description: 'Splices rows of the grid `g` starting from the index `begin`. Deletes `deleteCount` rows and inserts the specified `items` at that position.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to splice.',
      },
      begin: {
        type: 'number',
        description: 'The starting index of the splice.',
      },
      deleteCount: {
        type: 'number',
        description: 'The number of rows to delete.',
      },
      items: {
        type: 'array',
        rest: true,
        description: 'The rows to insert.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'begin',
          'deleteCount',
        ],
      },
      {
        argumentNames: [
          'g',
          'begin',
          'deleteCount',
          'items',
        ],
      },
    ],
    examples: [
      'let { splice-rows } = import("Grid");\nsplice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { splice-rows } = import("Grid");\nsplice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 1, ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.splice-cols', 'Grid.slice-rows'],
  },
  'splice-cols': {
    category: 'Grid',
    description: 'Splices columns of the grid `g` starting from the index `begin`. Deletes `deleteCount` columns and inserts the specified `items` at that position.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to splice.',
      },
      begin: {
        type: 'number',
        description: 'The starting index of the splice.',
      },
      deleteCount: {
        type: 'number',
        description: 'The number of columns to delete.',
      },
      items: {
        type: 'array',
        rest: true,
        description: 'The columns to insert.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'begin',
          'deleteCount',
        ],
      },
      {
        argumentNames: [
          'g',
          'begin',
          'deleteCount',
          'items',
        ],
      },
    ],
    examples: [
      'let { splice-cols } = import("Grid");\nsplice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { splice-cols } = import("Grid");\nsplice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 1, ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.splice-rows', 'Grid.slice-cols'],
  },
  'concat-rows': {
    category: 'Grid',
    description: 'Concatenates two grids `a` and `b` by rows. The number of columns in both grids must be the same.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'grid',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { concat-rows } = import("Grid");\nconcat-rows([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], [\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['Grid.concat-cols', 'Grid.push-rows'],
  },
  'concat-cols': {
    category: 'Grid',
    description: 'Concatenates two grids `a` and `b` by columns. The number of rows in both grids must be the same.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'grid',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { concat-cols } = import("Grid");\nconcat-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
    ],
    seeAlso: ['Grid.concat-rows', 'Grid.push-cols'],
  },
  'map': {
    category: 'Grid',
    description: 'Maps a function `a` over each element of the grid `b`, returning a new grid with the results.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "map" shadows a builtin function\nlet { map as grid-map } = import("Grid");\ngrid-map([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], str)',
    ],
    seeAlso: ['map', 'Grid.mapi', 'Grid.reduce'],
  },
  'mapi': {
    category: 'Grid',
    description: 'Maps a function `a` over each element of the grid `b`, passing the row and column index as additional arguments to the function.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'grid',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "mapi" shadows a builtin function\nlet { mapi as grid-mapi } = import("Grid");\ngrid-mapi([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")',
    ],
    seeAlso: ['Grid.map', 'Grid.reducei', 'map'],
  },
  'reduce': {
    category: 'Grid',
    description: 'Reduces the grid `a` using the function `b`, returning a single value.',
    returns: {
      type: 'any',
    },
    args: {
      'g': {
        type: 'grid',
        description: 'The grid to reduce.',
      },
      'fn': {
        type: 'function',
        description: 'The function to reduce the grid. It takes two arguments: the accumulator and the current element.',
      },
      'initial-value': {
        type: 'any',
        description: 'The initial value for the accumulator.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'fn',
          'initial-value',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "reduce" shadows a builtin function\nlet { reduce as grid-reduce } = import("Grid");\ngrid-reduce([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ++, "")',
    ],
    seeAlso: ['reduce', 'Grid.reducei', 'Grid.map'],
  },
  'reducei': {
    category: 'Grid',
    description: 'Reduces the grid `a` using the function `b`, passing the row and column indices as additional arguments to the function.',
    returns: {
      type: 'any',
    },
    args: {
      'g': {
        type: 'grid',
        description: 'The grid to reduce.',
      },
      'fn': {
        type: 'function',
        description: 'The function to reduce the grid. It takes four arguments: the accumulator, the current element, the row index, and the column index.',
      },
      'initial-value': {
        type: 'any',
        description: 'The initial value for the accumulator.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'fn',
          'initial-value',
        ],
      },
    ],
    examples: [
      '// Using "as" alias because "reducei" shadows a builtin function\nlet { reducei as grid-reducei } = import("Grid");\ngrid-reducei([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ++, "")',
    ],
    seeAlso: ['Grid.reduce', 'Grid.mapi', 'reduce'],
  },
  'push-rows': {
    category: 'Grid',
    description: 'Pushes the specified rows into the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to push rows into.',
      },
      rows: {
        type: 'array',
        rest: true,
        description: 'The rows to push into the grid.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'rows',
        ],
      },
    ],
    examples: [
      'let { push-rows } = import("Grid");\npush-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.unshift-rows', 'Grid.pop-row', 'Grid.shift-row', 'Grid.concat-rows'],
  },
  'unshift-rows': {
    category: 'Grid',
    description: 'Unshifts the specified rows into the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to unshift rows into.',
      },
      rows: {
        type: 'array',
        rest: true,
        description: 'The rows to unshift into the grid.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'rows',
        ],
      },
    ],
    examples: [
      'let { unshift-rows } = import("Grid");\nunshift-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.push-rows', 'Grid.shift-row', 'Grid.pop-row'],
  },
  'pop-row': {
    category: 'Grid',
    description: 'Pops the last row from the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to pop a row from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { pop-row } = import("Grid");\npop-row([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['Grid.shift-row', 'Grid.push-rows', 'Grid.unshift-rows'],
  },
  'shift-row': {
    category: 'Grid',
    description: 'Shifts the first row from the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to shift a row from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { shift-row } = import("Grid");\nshift-row([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['Grid.pop-row', 'Grid.push-rows', 'Grid.unshift-rows'],
  },
  'push-cols': {
    category: 'Grid',
    description: 'Pushes the specified columns into the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to push columns into.',
      },
      cols: {
        type: 'array',
        rest: true,
        description: 'The columns to push into the grid.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'cols',
        ],
      },
    ],
    examples: [
      'let { push-cols } = import("Grid");\npush-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.unshift-cols', 'Grid.pop-col', 'Grid.shift-col', 'Grid.concat-cols'],
  },
  'unshift-cols': {
    category: 'Grid',
    description: 'Unshifts the specified columns into the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to unshift columns into.',
      },
      cols: {
        type: 'array',
        rest: true,
        description: 'The columns to unshift into the grid.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
          'cols',
        ],
      },
    ],
    examples: [
      'let { unshift-cols } = import("Grid");\nunshift-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['Grid.push-cols', 'Grid.shift-col', 'Grid.pop-col'],
  },
  'pop-col': {
    category: 'Grid',
    description: 'Pops the last column from the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to pop a column from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { pop-col } = import("Grid");\npop-col([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['Grid.shift-col', 'Grid.push-cols', 'Grid.unshift-cols'],
  },
  'shift-col': {
    category: 'Grid',
    description: 'Shifts the first column from the grid `g` and returns the new grid.',
    returns: {
      type: 'grid',
    },
    args: {
      g: {
        type: 'grid',
        description: 'The grid to shift a column from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'g',
        ],
      },
    ],
    examples: [
      'let { shift-col } = import("Grid");\nshift-col([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['Grid.pop-col', 'Grid.push-cols', 'Grid.unshift-cols'],
  },
  'from-array': {
    category: 'Grid',
    description: 'Creates a grid from a flat array with specified dimensions. The array is reshaped into the specified number of rows, and the number of columns is automatically calculated based on the total number of elements in the array.',
    returns: {
      type: 'grid',
    },
    args: {
      a: {
        type: 'array',
      },
      b: {
        type: 'number',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { from-array } = import("Grid");\nfrom-array([1, 2, 3, 4], 2)',
      'let { from-array } = import("Grid");\nfrom-array([1, 2, 3, 4], 4)',
    ],
    seeAlso: ['Grid.fill', 'Grid.generate', 'Grid.reshape'],
  },
}
