import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'cell-every?': {
    category: 'grid',
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
      'let { cell-every? } = import(grid);\ncell-every?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], string?)',
      'let { cell-every? } = import(grid);\ncell-every?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], string?)',
      'let { cell-every? } = import(grid);\ncell-every?([\n  [1, 2],\n  [3, 4],\n], string?)',
    ],
    seeAlso: ['collection.every?', 'grid.some?', 'grid.every-row?', 'grid.every-col?'],
  },
  'some?': {
    category: 'grid',
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
      'let { some? } = import(grid);\nsome?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], string?)',
      'let { some? } = import(grid);\nsome?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], string?)',
      'let { some? } = import(grid);\nsome?([\n  [1, 2],\n  [3, 4],\n], string?)',
    ],
    seeAlso: ['collection.any?', 'grid.cell-every?', 'grid.some-row?', 'grid.some-col?'],
  },
  'every-row?': {
    category: 'grid',
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
      'let { every-row? } = import(grid);\nevery-row?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> string?($[0]))',
      'let { every-row? } = import(grid);\nevery-row?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> string?($[0]))',
      'let { every-row? } = import(grid);\nevery-row?([\n  [1, 2],\n  [3, 4],\n], -> string?($[0]))',
    ],
    seeAlso: ['grid.some-row?', 'grid.every-col?', 'grid.cell-every?'],
  },
  'some-row?': {
    category: 'grid',
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
      'let { some-row? } = import(grid);\nsome-row?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $ contains? "Albert")',
      'let { some-row? } = import(grid);\nsome-row?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> $ contains? "Albert")',
      'let { some-row? } = import(grid);\nsome-row?([\n  [1, 2],\n  [3, 4],\n], -> $ contains? "Albert")',
    ],
    seeAlso: ['grid.every-row?', 'grid.some-col?', 'grid.some?'],
  },
  'every-col?': {
    category: 'grid',
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
      'let { every-col? } = import(grid);\nevery-col?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> string?($[0]))',
      'let { every-col? } = import(grid);\nevery-col?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> string?($[0]))',
      'let { every-col? } = import(grid);\nevery-col?([\n  [1, 2],\n  [3, 4],\n], -> string?($[0]))',
    ],
    seeAlso: ['grid.some-col?', 'grid.every-row?', 'grid.cell-every?'],
  },
  'some-col?': {
    category: 'grid',
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
      'let { some-col? } = import(grid);\nsome-col?([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $ contains? "Albert")',
      'let { some-col? } = import(grid);\nsome-col?([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], -> $ contains? "Albert")',
      'let { some-col? } = import(grid);\nsome-col?([\n  [1, 2],\n  [3, 4],\n], -> $ contains? "Albert")',
    ],
    seeAlso: ['grid.every-col?', 'grid.some-row?', 'grid.some?'],
  },
  'row': {
    category: 'grid',
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
      'let { row } = import(grid);\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 0)',
      'let { row } = import(grid);\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
      'let { row } = import(grid);\nrow([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 2)',
    ],
    seeAlso: ['grid.col', 'grid.shape'],
  },
  'col': {
    category: 'grid',
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
      'let { col } = import(grid);\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 0)',
      'let { col } = import(grid);\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
      'let { col } = import(grid);\ncol([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 2)',
    ],
    seeAlso: ['grid.row', 'grid.shape'],
  },
  'shape': {
    category: 'grid',
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
      'let { shape } = import(grid);\nshape([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { shape } = import(grid);\nshape([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { shape } = import(grid);\nshape([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['grid.row', 'grid.col', 'grid.reshape'],
  },
  'fill': {
    category: 'grid',
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
      'let { fill } = import(grid);\nfill(2, 3, 0)',
      'let { fill } = import(grid);\nfill(2, 3, "x")',
    ],
    seeAlso: ['grid.generate', 'grid.from-array'],
  },
  'generate': {
    category: 'grid',
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
      'let { generate } = import(grid);\ngenerate(3, 3, (i, j) -> i + j)',
    ],
    seeAlso: ['grid.fill', 'grid.from-array'],
  },
  'reshape': {
    category: 'grid',
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
      'let { reshape } = import(grid);\nreshape([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], 2)',
    ],
    seeAlso: ['grid.shape', 'grid.from-array'],
  },
  'transpose': {
    category: 'grid',
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
      'let { transpose } = import(grid);\ntranspose([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { transpose } = import(grid);\ntranspose([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { transpose } = import(grid);\ntranspose([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['grid.flip-h', 'grid.flip-v', 'grid.rotate'],
  },
  'flip-h': {
    category: 'grid',
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
      'let { flip-h } = import(grid);\nflip-h([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { flip-h } = import(grid);\nflip-h([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { flip-h } = import(grid);\nflip-h([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['grid.flip-v', 'grid.transpose', 'grid.rotate'],
  },
  'flip-v': {
    category: 'grid',
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
      'let { flip-v } = import(grid);\nflip-v([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
      'let { flip-v } = import(grid);\nflip-v([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
      'let { flip-v } = import(grid);\nflip-v([\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['grid.flip-h', 'grid.transpose', 'grid.rotate'],
  },
  'rotate': {
    category: 'grid',
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
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], 1)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], 2)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], 3)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], 4)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], -1)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], -2)',
      'let { rotate } = import(grid);\nrotate([\n  [1, 2],\n  [3, 4],\n], -3)',
    ],
    seeAlso: ['grid.transpose', 'grid.flip-h', 'grid.flip-v'],
  },
  'crop': {
    category: 'grid',
    description: 'Crops the grid `g` from the starting index `begin` to the optional ending index `stop`. The crop is inclusive of the starting index and exclusive of the ending index.',
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
      'let { crop } = import(grid);\ncrop([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [1, 1], [2, 2])',
      'let { crop } = import(grid);\ncrop([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [1, 1])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.slice-rows', 'grid.slice-cols'],
  },
  'slice-rows': {
    category: 'grid',
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
      'let { slice-rows } = import(grid);\nslice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { slice-rows } = import(grid);\nslice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.crop', 'grid.slice-cols', 'grid.splice-rows'],
  },
  'slice-cols': {
    category: 'grid',
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
      'let { slice-cols } = import(grid);\nslice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { slice-cols } = import(grid);\nslice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1)',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.crop', 'grid.slice-rows', 'grid.splice-cols'],
  },
  'splice-rows': {
    category: 'grid',
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
      'let { splice-rows } = import(grid);\nsplice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { splice-rows } = import(grid);\nsplice-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 1, ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.splice-cols', 'grid.slice-rows'],
  },
  'splice-cols': {
    category: 'grid',
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
      'let { splice-cols } = import(grid);\nsplice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 2)',
      'let { splice-cols } = import(grid);\nsplice-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], 1, 1, ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.splice-rows', 'grid.slice-cols'],
  },
  'concat-rows': {
    category: 'grid',
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
      'let { concat-rows } = import(grid);\nconcat-rows([\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n], [\n  [1, 2],\n  [3, 4],\n])',
    ],
    seeAlso: ['grid.concat-cols', 'grid.push-rows'],
  },
  'concat-cols': {
    category: 'grid',
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
      'let { concat-cols } = import(grid);\nconcat-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], [\n  ["Albert", "father"],\n  ["Nina", "mother"],\n  ["Kian", "son"],\n])',
    ],
    seeAlso: ['grid.concat-rows', 'grid.push-cols'],
  },
  'cell-map': {
    category: 'grid',
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
      'let { cell-map } = import(grid);\ncell-map([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], str)',
    ],
    seeAlso: ['map', 'grid.cell-mapi', 'grid.cell-reduce'],
  },
  'cell-mapi': {
    category: 'grid',
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
      'let { cell-mapi } = import(grid);\ncell-mapi([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")',
    ],
    seeAlso: ['grid.cell-map', 'grid.cell-reducei', 'map'],
  },
  'cell-reduce': {
    category: 'grid',
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
      '// Using "as" alias because "reduce" shadows a builtin function\nlet { cell-reduce } = import(grid);\ncell-reduce([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ++, "")',
    ],
    seeAlso: ['reduce', 'grid.cell-reducei', 'grid.cell-map'],
  },
  'cell-reducei': {
    category: 'grid',
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
      '// Using "as" alias because "reducei" shadows a builtin function\nlet { cell-reducei } = import(grid);\ncell-reducei([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ++, "")',
    ],
    seeAlso: ['grid.cell-reduce', 'grid.cell-mapi', 'reduce'],
  },
  'push-rows': {
    category: 'grid',
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
      'let { push-rows } = import(grid);\npush-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.unshift-rows', 'grid.pop-row', 'grid.shift-row', 'grid.concat-rows'],
  },
  'unshift-rows': {
    category: 'grid',
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
      'let { unshift-rows } = import(grid);\nunshift-rows([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["Nazanin", "mother", 40])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.push-rows', 'grid.shift-row', 'grid.pop-row'],
  },
  'pop-row': {
    category: 'grid',
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
      'let { pop-row } = import(grid);\npop-row([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['grid.shift-row', 'grid.push-rows', 'grid.unshift-rows'],
  },
  'shift-row': {
    category: 'grid',
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
      'let { shift-row } = import(grid);\nshift-row([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['grid.pop-row', 'grid.push-rows', 'grid.unshift-rows'],
  },
  'push-cols': {
    category: 'grid',
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
      'let { push-cols } = import(grid);\npush-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.unshift-cols', 'grid.pop-col', 'grid.shift-col', 'grid.concat-cols'],
  },
  'unshift-cols': {
    category: 'grid',
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
      'let { unshift-cols } = import(grid);\nunshift-cols([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n], ["f", "m", "s"])',
    ],
    hideOperatorForm: true,
    seeAlso: ['grid.push-cols', 'grid.shift-col', 'grid.pop-col'],
  },
  'pop-col': {
    category: 'grid',
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
      'let { pop-col } = import(grid);\npop-col([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['grid.shift-col', 'grid.push-cols', 'grid.unshift-cols'],
  },
  'shift-col': {
    category: 'grid',
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
      'let { shift-col } = import(grid);\nshift-col([\n  ["Albert", "father", 10],\n  ["Nina", "mother", 20],\n  ["Kian", "son", 30],\n])',
    ],
    seeAlso: ['grid.pop-col', 'grid.push-cols', 'grid.unshift-cols'],
  },
  'from-array': {
    category: 'grid',
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
      'let { from-array } = import(grid);\nfrom-array([1, 2, 3, 4], 2)',
      'let { from-array } = import(grid);\nfrom-array([1, 2, 3, 4], 4)',
    ],
    seeAlso: ['grid.fill', 'grid.generate', 'grid.reshape'],
  },
}
