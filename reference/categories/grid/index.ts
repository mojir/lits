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
  'grid:every?': {
    title: 'grid:every?',
    category: 'Grid',
    linkName: 'grid-colon-every-question',
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
      `grid:every?(${exampleGrid1}, string?)`,
      `grid:every?(${exampleGrid2}, string?)`,
      `grid:every?(${exampleGrid3}, string?)`,
    ],
  },
  'grid:some?': {
    title: 'grid:some?',
    category: 'Grid',
    linkName: 'grid-colon-some-question',
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
      `grid:some?(${exampleGrid1}, string?)`,
      `grid:some?(${exampleGrid2}, string?)`,
      `grid:some?(${exampleGrid3}, string?)`,
    ],
  },
  'grid:every-row?': {
    title: 'grid:every-row?',
    category: 'Grid',
    linkName: 'grid-colon-every-row-question',
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
      `grid:every-row?(${exampleGrid1}, -> string?($[0]))`,
      `grid:every-row?(${exampleGrid2}, -> string?($[0]))`,
      `grid:every-row?(${exampleGrid3}, -> string?($[0]))`,
    ],
  },

  'grid:some-row?': {
    title: 'grid:some-row?',
    category: 'Grid',
    linkName: 'grid-colon-some-row-question',
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
      `grid:some-row?(${exampleGrid1}, -> $ contains? "Albert")`,
      `grid:some-row?(${exampleGrid2}, -> $ contains? "Albert")`,
      `grid:some-row?(${exampleGrid3}, -> $ contains? "Albert")`,
    ],
  },
  'grid:every-col?': {
    title: 'grid:every-col?',
    category: 'Grid',
    linkName: 'grid-colon-every-col-question',
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
      `grid:every-col?(${exampleGrid1}, -> string?($[0]))`,
      `grid:every-col?(${exampleGrid2}, -> string?($[0]))`,
      `grid:every-col?(${exampleGrid3}, -> string?($[0]))`,
    ],
  },
  'grid:some-col?': {
    title: 'grid:some-col?',
    category: 'Grid',
    linkName: 'grid-colon-some-col-question',
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
      `grid:some-col?(${exampleGrid1}, -> $ contains? "Albert")`,
      `grid:some-col?(${exampleGrid2}, -> $ contains? "Albert")`,
      `grid:some-col?(${exampleGrid3}, -> $ contains? "Albert")`,
    ],
  },
  'grid:row': {
    title: 'grid:row',
    category: 'Grid',
    linkName: 'grid-colon-row',
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
      `grid:row(${exampleGrid1}, 0)`,
      `grid:row(${exampleGrid1}, 1)`,
      `grid:row(${exampleGrid1}, 2)`,
    ],
  },
  'grid:col': {
    title: 'grid:col',
    category: 'Grid',
    linkName: 'grid-colon-col',
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
      `grid:col(${exampleGrid1}, 0)`,
      `grid:col(${exampleGrid1}, 1)`,
      `grid:col(${exampleGrid1}, 2)`,
    ],
  },
  'grid:shape': {
    title: 'grid:shape',
    category: 'Grid',
    linkName: 'grid-colon-shape',
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
      `grid:shape(${exampleGrid1})`,
      `grid:shape(${exampleGrid2})`,
      `grid:shape(${exampleGrid3})`,
    ],
  },
  'grid:fill': {
    title: 'grid:fill',
    category: 'Grid',
    linkName: 'grid-colon-fill',
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
      'grid:fill(2, 3, 0)',
      'grid:fill(2, 3, "x")',
    ],
  },
  'grid:generate': {
    title: 'grid:generate',
    category: 'Grid',
    linkName: 'grid-colon-generate',
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
      'grid:generate(3, 3, (i, j) -> i + j)',
    ],
  },
  'grid:reshape': {
    title: 'grid:reshape',
    category: 'Grid',
    linkName: 'grid-colon-reshape',
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
      `grid:reshape(${exampleGrid2}, 2)`,
    ],
  },
  'grid:transpose': {
    title: 'grid:transpose',
    category: 'Grid',
    linkName: 'grid-colon-transpose',
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
      `grid:transpose(${exampleGrid1})`,
      `grid:transpose(${exampleGrid2})`,
      `grid:transpose(${exampleGrid3})`,
    ],
    aliases: ['grid:tr'],
  },
  'grid:flip-h': {
    title: 'grid:flip-h',
    category: 'Grid',
    linkName: 'grid-colon-flip-h',
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
      `grid:flip-h(${exampleGrid1})`,
      `grid:flip-h(${exampleGrid2})`,
      `grid:flip-h(${exampleGrid3})`,
    ],
  },
  'grid:flip-v': {
    title: 'grid:flip-v',
    category: 'Grid',
    linkName: 'grid-colon-flip-v',
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
      `grid:flip-v(${exampleGrid1})`,
      `grid:flip-v(${exampleGrid2})`,
      `grid:flip-v(${exampleGrid3})`,
    ],
  },
  'grid:rotate': {
    title: 'grid:rotate',
    category: 'Grid',
    linkName: 'grid-colon-rotate',
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
      `grid:rotate(${exampleGrid3}, 1)`,
      `grid:rotate(${exampleGrid3}, 2)`,
      `grid:rotate(${exampleGrid3}, 3)`,
      `grid:rotate(${exampleGrid3}, 4)`,
      `grid:rotate(${exampleGrid3}, -1)`,
      `grid:rotate(${exampleGrid3}, -2)`,
      `grid:rotate(${exampleGrid3}, -3)`,
    ],
  },
  'grid:reverse-rows': {
    title: 'grid:reverse-rows',
    category: 'Grid',
    linkName: 'grid-colon-reverse-rows',
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
      `grid:reverse-rows(${exampleGrid1})`,
      `grid:reverse-rows(${exampleGrid2})`,
      `grid:reverse-rows(${exampleGrid3})`,
    ],
  },
  'grid:reverse-cols': {
    title: 'grid:reverse-cols',
    category: 'Grid',
    linkName: 'grid-colon-reverse-cols',
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
      `grid:reverse-cols(${exampleGrid1})`,
      `grid:reverse-cols(${exampleGrid2})`,
      `grid:reverse-cols(${exampleGrid3})`,
    ],
  },
  'grid:slice': {
    title: 'grid:slice',
    category: 'Grid',
    linkName: 'grid-colon-slice',
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
      `grid:slice(${exampleGrid1}, [1, 1], [2, 2])`,
      `grid:slice(${exampleGrid1}, [1, 1])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:slice-rows': {
    title: 'grid:slice-rows',
    category: 'Grid',
    linkName: 'grid-colon-slice-rows',
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
      `grid:slice-rows(${exampleGrid1}, 1, 2)`,
      `grid:slice-rows(${exampleGrid1}, 1)`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:slice-cols': {
    title: 'grid:slice-cols',
    category: 'Grid',
    linkName: 'grid-colon-slice-cols',
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
      `grid:slice-cols(${exampleGrid1}, 1, 2)`,
      `grid:slice-cols(${exampleGrid1}, 1)`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:splice-rows': {
    title: 'grid:splice-rows',
    category: 'Grid',
    linkName: 'grid-colon-splice-rows',
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
      `grid:splice-rows(${exampleGrid1}, 1, 2)`,
      `grid:splice-rows(${exampleGrid1}, 1, 1, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:splice-cols': {
    title: 'grid:splice-cols',
    category: 'Grid',
    linkName: 'grid-colon-splice-cols',
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
      `grid:splice-cols(${exampleGrid1}, 1, 2)`,
      `grid:splice-cols(${exampleGrid1}, 1, 1, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:concat-rows': {
    title: 'grid:concat-rows',
    category: 'Grid',
    linkName: 'grid-colon-concat-rows',
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
      `grid:concat-rows(${exampleGrid2}, ${exampleGrid3})`,
    ],
  },
  'grid:concat-cols': {
    title: 'grid:concat-cols',
    category: 'Grid',
    linkName: 'grid-colon-concat-cols',
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
      `grid:concat-cols(${exampleGrid1}, ${exampleGrid2})`,
    ],
  },
  'grid:map': {
    title: 'grid:map',
    category: 'Grid',
    linkName: 'grid-colon-map',
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
      `grid:map(${exampleGrid1}, str)`,
    ],
  },
  'grid:mapi': {
    title: 'grid:mapi',
    category: 'Grid',
    linkName: 'grid-colon-mapi',
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
      `grid:mapi(${exampleGrid1}, -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")`,
    ],
  },
  'grid:reduce': {
    title: 'grid:reduce',
    category: 'Grid',
    linkName: 'grid-colon-reduce',
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
      `grid:reduce(${exampleGrid1}, ++, "")`,
    ],
  },
  'grid:reducei': {
    title: 'grid:reducei',
    category: 'Grid',
    linkName: 'grid-colon-reducei',
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
      `grid:reducei(${exampleGrid1}, ++, "")`,
    ],
  },
  'grid:push-rows': {
    title: 'grid:push-rows',
    category: 'Grid',
    linkName: 'grid-colon-push-rows',
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
      `grid:push-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:unshift-rows': {
    title: 'grid:unshift-rows',
    category: 'Grid',
    linkName: 'grid-colon-unshift-rows',
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
      `grid:unshift-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:pop-row': {
    title: 'grid:pop-row',
    category: 'Grid',
    linkName: 'grid-colon-pop-row',
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
      `grid:pop-row(${exampleGrid1})`,
    ],
  },
  'grid:shift-row': {
    title: 'grid:shift-row',
    category: 'Grid',
    linkName: 'grid-colon-shift-row',
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
      `grid:shift-row(${exampleGrid1})`,
    ],
  },
  'grid:push-cols': {
    title: 'grid:push-cols',
    category: 'Grid',
    linkName: 'grid-colon-push-cols',
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
      `grid:push-cols(${exampleGrid1}, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:unshift-cols': {
    title: 'grid:unshift-cols',
    category: 'Grid',
    linkName: 'grid-colon-unshift-cols',
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
      `grid:unshift-cols(${exampleGrid1}, ["f", "m", "s"])`,
    ],
    noOperatorDocumentation: true,
  },
  'grid:pop-col': {
    title: 'grid:pop-col',
    category: 'Grid',
    linkName: 'grid-colon-pop-col',
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
      `grid:pop-col(${exampleGrid1})`,
    ],
  },
  'grid:shift-col': {
    title: 'grid:shift-col',
    category: 'Grid',
    linkName: 'grid-colon-shift-col',
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
      `grid:shift-col(${exampleGrid1})`,
    ],
  },
  'grid:from-array': {
    title: 'grid:from-array',
    category: 'Grid',
    linkName: 'grid-colon-from-array',
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
      'grid:from-array([1, 2, 3, 4], 2)',
      'grid:from-array([1, 2, 3, 4], 4)',
    ],
  },
}
