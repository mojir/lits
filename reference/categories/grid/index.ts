import type { FunctionReference } from '../..'
import { type GridApiName, getOperatorArgs } from '../../api'

const exampleGrid1 = `[
  ["Albert", "father", 10],
  ["Nina", "mother", 20],
  ["Kian", "son", 30],
]`

const exampleGrid2 = `[
  ["Albert", "father"],
  ["Nina", "mother"],
  ["Kian", "son"],
]`

const exampleGrid3 = `[
  [1, 2],
  [3, 4],
]`

export const gridReference: Record<GridApiName, FunctionReference<'Grid'>> = {
  'Grid.every?': {
    title: 'Grid.every?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if all elements in a grid satisfy a predicate. Returns true only if the predicate returns true for every element in the grid.',
    examples: [
      `// Using "as" alias because "every?" shadows a builtin function
let { every? as grid-every? } = import("Grid");
grid-every?(${exampleGrid1}, string?)`,
      `// Using "as" alias because "every?" shadows a builtin function
let { every? as grid-every? } = import("Grid");
grid-every?(${exampleGrid2}, string?)`,
      `// Using "as" alias because "every?" shadows a builtin function
let { every? as grid-every? } = import("Grid");
grid-every?(${exampleGrid3}, string?)`,
    ],
  },
  'Grid.some?': {
    title: 'Grid.some?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if any element in a grid satisfies a predicate. Returns true if the predicate returns true for at least one element in the grid.',
    examples: [
      `let { some? } = import("Grid");
some?(${exampleGrid1}, string?)`,
      `let { some? } = import("Grid");
some?(${exampleGrid2}, string?)`,
      `let { some? } = import("Grid");
some?(${exampleGrid3}, string?)`,
    ],
  },
  'Grid.every-row?': {
    title: 'Grid.every-row?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if all rows in a grid satisfy a predicate. Returns true only if the predicate returns true for every row in the grid.',
    examples: [
      `let { every-row? } = import("Grid");
every-row?(${exampleGrid1}, -> string?($[0]))`,
      `let { every-row? } = import("Grid");
every-row?(${exampleGrid2}, -> string?($[0]))`,
      `let { every-row? } = import("Grid");
every-row?(${exampleGrid3}, -> string?($[0]))`,
    ],
  },

  'Grid.some-row?': {
    title: 'Grid.some-row?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if any row in a grid satisfies a predicate. Returns true if the predicate returns true for at least one row in the grid.',
    examples: [
      `let { some-row? } = import("Grid");
some-row?(${exampleGrid1}, -> $ contains? "Albert")`,
      `let { some-row? } = import("Grid");
some-row?(${exampleGrid2}, -> $ contains? "Albert")`,
      `let { some-row? } = import("Grid");
some-row?(${exampleGrid3}, -> $ contains? "Albert")`,
    ],
  },
  'Grid.every-col?': {
    title: 'Grid.every-col?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if all columns in a grid satisfy a predicate. Returns true only if the predicate returns true for every column in the grid.',
    examples: [
      `let { every-col? } = import("Grid");
every-col?(${exampleGrid1}, -> string?($[0]))`,
      `let { every-col? } = import("Grid");
every-col?(${exampleGrid2}, -> string?($[0]))`,
      `let { every-col? } = import("Grid");
every-col?(${exampleGrid3}, -> string?($[0]))`,
    ],
  },
  'Grid.some-col?': {
    title: 'Grid.some-col?',
    category: 'Grid',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if any column in a grid satisfies a predicate. Returns true if the predicate returns true for at least one column in the grid.',
    examples: [
      `let { some-col? } = import("Grid");
some-col?(${exampleGrid1}, -> $ contains? "Albert")`,
      `let { some-col? } = import("Grid");
some-col?(${exampleGrid2}, -> $ contains? "Albert")`,
      `let { some-col? } = import("Grid");
some-col?(${exampleGrid3}, -> $ contains? "Albert")`,
    ],
  },
  'Grid.row': {
    title: 'Grid.row',
    category: 'Grid',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('grid', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns the row at index $a in the grid $b.',
    examples: [
      `let { row } = import("Grid");
row(${exampleGrid1}, 0)`,
      `let { row } = import("Grid");
row(${exampleGrid1}, 1)`,
      `let { row } = import("Grid");
row(${exampleGrid1}, 2)`,
    ],
  },
  'Grid.col': {
    title: 'Grid.col',
    category: 'Grid',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('grid', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns the column at index $a in the grid $b.',
    examples: [
      `let { col } = import("Grid");
col(${exampleGrid1}, 0)`,
      `let { col } = import("Grid");
col(${exampleGrid1}, 1)`,
      `let { col } = import("Grid");
col(${exampleGrid1}, 2)`,
    ],
  },
  'Grid.shape': {
    title: 'Grid.shape',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Returns the shape of the grid `g` as a `vector` of two numbers, where the first number is the number of rows and the second number is the number of columns.',
    examples: [
      `let { shape } = import("Grid");
shape(${exampleGrid1})`,
      `let { shape } = import("Grid");
shape(${exampleGrid2})`,
      `let { shape } = import("Grid");
shape(${exampleGrid3})`,
    ],
  },
  'Grid.fill': {
    title: 'Grid.fill',
    category: 'Grid',
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
      { argumentNames: ['rows', 'cols', 'value'] },
    ],
    description: 'Creates a grid of the specified size, filled with the specified value.',
    examples: [
      'let { fill } = import("Grid");\nfill(2, 3, 0)',
      'let { fill } = import("Grid");\nfill(2, 3, "x")',
    ],
  },
  'Grid.generate': {
    title: 'Grid.generate',
    category: 'Grid',
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
      { argumentNames: ['rows', 'cols', 'fn'] },
    ],
    description: 'Generates a grid of the specified size, where each element is generated by the provided function.',
    examples: [
      'let { generate } = import("Grid");\ngenerate(3, 3, (i, j) -> i + j)',
    ],
  },
  'Grid.reshape': {
    title: 'Grid.reshape',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Reshapes the grid `a` into a new grid with the specified number of rows `b`. The number of columns is automatically calculated based on the total number of elements in the grid.',
    examples: [
      `let { reshape } = import("Grid");
reshape(${exampleGrid2}, 2)`,
    ],
  },
  'Grid.transpose': {
    title: 'Grid.transpose',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Transposes the grid `g`, swapping its rows and columns.',
    examples: [
      `let { transpose } = import("Grid");
transpose(${exampleGrid1})`,
      `let { transpose } = import("Grid");
transpose(${exampleGrid2})`,
      `let { transpose } = import("Grid");
transpose(${exampleGrid3})`,
    ],
    aliases: ['Grid.tr'],
  },
  'Grid.flip-h': {
    title: 'Grid.flip-h',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Flips the grid `g` horizontally.',
    examples: [
      `let { flip-h } = import("Grid");
flip-h(${exampleGrid1})`,
      `let { flip-h } = import("Grid");
flip-h(${exampleGrid2})`,
      `let { flip-h } = import("Grid");
flip-h(${exampleGrid3})`,
    ],
  },
  'Grid.flip-v': {
    title: 'Grid.flip-v',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Flips the grid `g` vertically.',
    examples: [
      `let { flip-v } = import("Grid");
flip-v(${exampleGrid1})`,
      `let { flip-v } = import("Grid");
flip-v(${exampleGrid2})`,
      `let { flip-v } = import("Grid");
flip-v(${exampleGrid3})`,
    ],
  },
  'Grid.rotate': {
    title: 'Grid.rotate',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Rotates the grid `g` by the specified angle. The angle is given in terms of 90-degree rotations. Positive values rotate the grid clockwise, while negative values rotate it counterclockwise.',
    examples: [
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, 1)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, 2)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, 3)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, 4)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, -1)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, -2)`,
      `let { rotate } = import("Grid");
rotate(${exampleGrid3}, -3)`,
    ],
  },
  'Grid.reverse-rows': {
    title: 'Grid.reverse-rows',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Reverses the order of rows in the grid `g`.',
    examples: [
      `let { reverse-rows } = import("Grid");
reverse-rows(${exampleGrid1})`,
      `let { reverse-rows } = import("Grid");
reverse-rows(${exampleGrid2})`,
      `let { reverse-rows } = import("Grid");
reverse-rows(${exampleGrid3})`,
    ],
  },
  'Grid.reverse-cols': {
    title: 'Grid.reverse-cols',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Reverses the order of columns in the grid `g`.',
    examples: [
      `let { reverse-cols } = import("Grid");
reverse-cols(${exampleGrid1})`,
      `let { reverse-cols } = import("Grid");
reverse-cols(${exampleGrid2})`,
      `let { reverse-cols } = import("Grid");
reverse-cols(${exampleGrid3})`,
    ],
  },
  'Grid.slice': {
    title: 'Grid.slice',
    category: 'Grid',
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
      { argumentNames: ['g', 'begin'] },
      { argumentNames: ['g', 'begin', 'stop'] },
    ],
    description: 'Slices the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    examples: [
      `// Using "as" alias because "slice" shadows a builtin function
let { slice as grid-slice } = import("Grid");
grid-slice(${exampleGrid1}, [1, 1], [2, 2])`,
      `// Using "as" alias because "slice" shadows a builtin function
let { slice as grid-slice } = import("Grid");
grid-slice(${exampleGrid1}, [1, 1])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.slice-rows': {
    title: 'Grid.slice-rows',
    category: 'Grid',
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
      { argumentNames: ['g', 'begin'] },
      { argumentNames: ['g', 'begin', 'stop'] },
    ],
    description: 'Slices rows of the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    examples: [
      `let { slice-rows } = import("Grid");
slice-rows(${exampleGrid1}, 1, 2)`,
      `let { slice-rows } = import("Grid");
slice-rows(${exampleGrid1}, 1)`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.slice-cols': {
    title: 'Grid.slice-cols',
    category: 'Grid',
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
      { argumentNames: ['g', 'begin'] },
      { argumentNames: ['g', 'begin', 'stop'] },
    ],
    description: 'Slices columns of the grid `g` from the starting index `begin` to the optional ending index `stop`. The slice is inclusive of the starting index and exclusive of the ending index.',
    examples: [
      `let { slice-cols } = import("Grid");
slice-cols(${exampleGrid1}, 1, 2)`,
      `let { slice-cols } = import("Grid");
slice-cols(${exampleGrid1}, 1)`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.splice-rows': {
    title: 'Grid.splice-rows',
    category: 'Grid',
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
      { argumentNames: ['g', 'begin', 'deleteCount'] },
      { argumentNames: ['g', 'begin', 'deleteCount', 'items'] },
    ],
    description: 'Splices rows of the grid `g` starting from the index `begin`. Deletes `deleteCount` rows and inserts the specified `items` at that position.',
    examples: [
      `let { splice-rows } = import("Grid");
splice-rows(${exampleGrid1}, 1, 2)`,
      `let { splice-rows } = import("Grid");
splice-rows(${exampleGrid1}, 1, 1, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.splice-cols': {
    title: 'Grid.splice-cols',
    category: 'Grid',
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
      { argumentNames: ['g', 'begin', 'deleteCount'] },
      { argumentNames: ['g', 'begin', 'deleteCount', 'items'] },
    ],
    description: 'Splices columns of the grid `g` starting from the index `begin`. Deletes `deleteCount` columns and inserts the specified `items` at that position.',
    examples: [
      `let { splice-cols } = import("Grid");
splice-cols(${exampleGrid1}, 1, 2)`,
      `let { splice-cols } = import("Grid");
splice-cols(${exampleGrid1}, 1, 1, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.concat-rows': {
    title: 'Grid.concat-rows',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'grid'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Concatenates two grids `a` and `b` by rows. The number of columns in both grids must be the same.',
    examples: [
      `let { concat-rows } = import("Grid");
concat-rows(${exampleGrid2}, ${exampleGrid3})`,
    ],
  },
  'Grid.concat-cols': {
    title: 'Grid.concat-cols',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'grid'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Concatenates two grids `a` and `b` by columns. The number of rows in both grids must be the same.',
    examples: [
      `let { concat-cols } = import("Grid");
concat-cols(${exampleGrid1}, ${exampleGrid2})`,
    ],
  },
  'Grid.TEMP-map': {
    title: 'Grid.TEMP-map',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Maps a function `a` over each element of the grid `b`, returning a new grid with the results.',
    examples: [
      `let { TEMP-map } = import("Grid");
TEMP-map(${exampleGrid1}, str)`,
    ],
  },
  'Grid.mapi': {
    title: 'Grid.mapi',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('grid', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Maps a function `a` over each element of the grid `b`, passing the row and column index as additional arguments to the function.',
    examples: [
      `// Using "as" alias because "mapi" shadows a builtin function
let { mapi as grid-mapi } = import("Grid");
grid-mapi(${exampleGrid1}, -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")`,
    ],
  },
  'Grid.TEMP-reduce': {
    title: 'Grid.TEMP-reduce',
    category: 'Grid',
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
      { argumentNames: ['g', 'fn', 'initial-value'] },
    ],
    description: 'Reduces the grid `a` using the function `b`, returning a single value.',
    examples: [
      `let { TEMP-reduce } = import("Grid");
TEMP-reduce(${exampleGrid1}, ++, "")`,
    ],
  },
  'Grid.reducei': {
    title: 'Grid.reducei',
    category: 'Grid',
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
      { argumentNames: ['g', 'fn', 'initial-value'] },
    ],
    description: 'Reduces the grid `a` using the function `b`, passing the row and column indices as additional arguments to the function.',
    examples: [
      `// Using "as" alias because "reducei" shadows a builtin function
let { reducei as grid-reducei } = import("Grid");
grid-reducei(${exampleGrid1}, ++, "")`,
    ],
  },
  'Grid.push-rows': {
    title: 'Grid.push-rows',
    category: 'Grid',
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
      { argumentNames: ['g', 'rows'] },
    ],
    description: 'Pushes the specified rows into the grid `g` and returns the new grid.',
    examples: [
      `let { push-rows } = import("Grid");
push-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.unshift-rows': {
    title: 'Grid.unshift-rows',
    category: 'Grid',
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
      { argumentNames: ['g', 'rows'] },
    ],
    description: 'Unshifts the specified rows into the grid `g` and returns the new grid.',
    examples: [
      `let { unshift-rows } = import("Grid");
unshift-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.pop-row': {
    title: 'Grid.pop-row',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Pops the last row from the grid `g` and returns the new grid.',
    examples: [
      `let { pop-row } = import("Grid");
pop-row(${exampleGrid1})`,
    ],
  },
  'Grid.shift-row': {
    title: 'Grid.shift-row',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Shifts the first row from the grid `g` and returns the new grid.',
    examples: [
      `let { shift-row } = import("Grid");
shift-row(${exampleGrid1})`,
    ],
  },
  'Grid.push-cols': {
    title: 'Grid.push-cols',
    category: 'Grid',
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
      { argumentNames: ['g', 'cols'] },
    ],
    description: 'Pushes the specified columns into the grid `g` and returns the new grid.',
    examples: [
      `let { push-cols } = import("Grid");
push-cols(${exampleGrid1}, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.unshift-cols': {
    title: 'Grid.unshift-cols',
    category: 'Grid',
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
      { argumentNames: ['g', 'cols'] },
    ],
    description: 'Unshifts the specified columns into the grid `g` and returns the new grid.',
    examples: [
      `let { unshift-cols } = import("Grid");
unshift-cols(${exampleGrid1}, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'Grid.pop-col': {
    title: 'Grid.pop-col',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Pops the last column from the grid `g` and returns the new grid.',
    examples: [
      `let { pop-col } = import("Grid");
pop-col(${exampleGrid1})`,
    ],
  },
  'Grid.shift-col': {
    title: 'Grid.shift-col',
    category: 'Grid',
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
      { argumentNames: ['g'] },
    ],
    description: 'Shifts the first column from the grid `g` and returns the new grid.',
    examples: [
      `let { shift-col } = import("Grid");
shift-col(${exampleGrid1})`,
    ],
  },
  'Grid.from-array': {
    title: 'Grid.from-array',
    category: 'Grid',
    returns: {
      type: 'grid',
    },
    args: {
      ...getOperatorArgs('array', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Creates a grid from a flat array with specified dimensions. The array is reshaped into the specified number of rows, and the number of columns is automatically calculated based on the total number of elements in the array.',
    examples: [
      'let { from-array } = import("Grid");\nfrom-array([1, 2, 3, 4], 2)',
      'let { from-array } = import("Grid");\nfrom-array([1, 2, 3, 4], 4)',
    ],
  },
}
