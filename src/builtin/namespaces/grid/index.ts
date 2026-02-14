import { LitsError } from '../../../errors'
import type { Any } from '../../../interface'
import { assertGrid, assertVector } from '../../../typeGuards/annotatedArrays'
import { assertArray } from '../../../typeGuards/array'
import { asAny, asFunctionLike, assertAny, assertFunctionLike } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../../builtin/interface'
import type { LitsNamespace } from '../interface'
import { fromArray } from './fromArray'
import { transpose } from './transpose'

const gridFunctions: BuiltinNormalExpressions = {
  'every?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      for (const row of grid) {
        for (const cell of row) {
          if (!executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
            return false
          }
        }
      }
      return true
    },
    arity: toFixedArity(2),
  },
  'some?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      for (const row of grid) {
        for (const cell of row) {
          if (executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
            return true
          }
        }
      }
      return false
    },
    arity: toFixedArity(2),
  },
  'every-row?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      for (const row of grid) {
        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return false
        }
      }
      return true
    },
    arity: toFixedArity(2),
  },
  'some-row?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      for (const row of grid) {
        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return true
        }
      }
      return false
    },
    arity: toFixedArity(2),
  },
  'every-col?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      const transposed = transpose(grid)
      for (const row of transposed) {
        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return false
        }
      }
      return true
    },
    arity: toFixedArity(2),
  },
  'some-col?': {
    evaluate: ([grid, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(predicate, sourceCodeInfo)

      const transposed = transpose(grid)
      for (const row of transposed) {
        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return true
        }
      }
      return false
    },
    arity: toFixedArity(2),
  },
  'row': {
    evaluate: ([grid, row], sourceCodeInfo): Any[] => {
      assertGrid(grid, sourceCodeInfo)
      assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid.length })
      return grid[row]!
    },
    arity: toFixedArity(2),
  },
  'col': {
    evaluate: ([grid, col], sourceCodeInfo): Any[] => {
      assertGrid(grid, sourceCodeInfo)
      assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid[0]!.length })
      return grid.map(row => row[col]!)
    },
    arity: toFixedArity(2),
  },
  'shape': {
    evaluate: ([grid], sourceCodeInfo): Any[] => {
      assertGrid(grid, sourceCodeInfo)
      return [grid.length, grid[0]!.length]
    },
    arity: toFixedArity(1),
  },
  'fill': {
    evaluate: ([rows, cols, value], sourceCodeInfo): Any[][] => {
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(cols, sourceCodeInfo, { integer: true, positive: true })
      assertAny(value, sourceCodeInfo)
      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          row.push(value)
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(3),
  },
  'generate': {
    evaluate: ([rows, cols, generator], sourceCodeInfo, contextStack, { executeFunction }): Any[][] => {
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(cols, sourceCodeInfo, { integer: true, positive: true })
      assertFunctionLike(generator, sourceCodeInfo)

      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          const value = executeFunction(generator, [i, j], contextStack, sourceCodeInfo)
          assertAny(value, sourceCodeInfo)
          row.push(value)
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(3),
  },
  'reshape': {
    evaluate: ([grid, rows], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })

      const flatTable = grid.flat()
      if (flatTable.length % rows !== 0) {
        throw new LitsError(`The number of elements in the grid must be divisible by rows, but got ${flatTable.length} and ${rows}`, sourceCodeInfo)
      }
      const cols = flatTable.length / rows

      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          row.push(flatTable[i * cols + j]!)
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(2),
  },
  'transpose': {
    evaluate: ([grid], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      return transpose(grid)
    },
    arity: toFixedArity(1),
  },
  'flip-h': {
    evaluate: ([grid], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      return grid.map(row => row.reverse())
    },
    arity: toFixedArity(1),
  },
  'flip-v': {
    evaluate: ([grid], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      return grid.reverse()
    },
    arity: toFixedArity(1),
  },
  'rotate': {
    evaluate: ([grid, times], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertNumber(times, sourceCodeInfo, { integer: true })
      // Normalize times to be between 0 and 3
      times = ((times % 4) + 4) % 4

      // If times is 0, return the original grid
      if (times === 0 || grid.length === 0) {
        return grid.map(row => [...row])
      }

      const height = grid.length
      const width = grid[0]!.length

      let result: Any[][]

      switch (times) {
        case 1: // 90 degrees clockwise
          result = Array<Any>(width).fill(null).map(() => Array<Any>(height).fill(null))
          for (let y = 0; y < height; y++) {
            for (let x = 0; x < width; x++) {
              result[x]![height - 1 - y] = grid[y]![x]!
            }
          }
          break

        case 2: // 180 degrees
          result = Array<Any>(height).fill(null).map(() => Array<Any>(width).fill(null))
          for (let y = 0; y < height; y++) {
            for (let x = 0; x < width; x++) {
              result[height - 1 - y]![width - 1 - x] = grid[y]![x]!
            }
          }
          break

        case 3: // 270 degrees clockwise (or 90 degrees counter-clockwise)
          result = Array<Any>(width).fill(null).map(() => Array<Any>(height).fill(null))
          for (let y = 0; y < height; y++) {
            for (let x = 0; x < width; x++) {
              result[width - 1 - x]![y] = grid[y]![x]!
            }
          }
          break
      }

      return result!
    },
    arity: toFixedArity(2),
  },
  'reverse-rows': {
    evaluate: ([grid], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      return grid.reverse()
    },
    arity: toFixedArity(1),
  },
  'reverse-cols': {
    evaluate: ([grid], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      return grid.map(row => row.reverse())
    },
    arity: toFixedArity(1),
  },
  'slice': {
    evaluate: ([grid, start, end], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertVector(start, sourceCodeInfo)
      if (start.length !== 2) {
        throw new LitsError(`The start vector must have 2 elements, but got ${start.length}`, sourceCodeInfo)
      }
      const [rowStart, colStart] = start
      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid.length })
      assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid[0]!.length })

      end ??= [grid.length, grid[0]!.length]
      assertVector(end, sourceCodeInfo)
      if (end.length !== 2) {
        throw new LitsError(`The end vector must have 2 elements, but got ${end.length}`, sourceCodeInfo)
      }
      const [rowEnd, colEnd] = end
      assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: grid.length })
      assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: grid[0]!.length })

      const result: Any[][] = []
      for (let i = rowStart; i < rowEnd; i += 1) {
        const row: Any[] = []
        for (let j = colStart; j < colEnd; j += 1) {
          row.push(grid[i]![j]!)
        }
        result.push(row)
      }
      return result
    },
    arity: { min: 2, max: 3 },
  },
  'slice-rows': {
    evaluate: ([grid, rowStart, rowEnd], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)

      if (typeof rowEnd === 'undefined') {
        assertNumber(rowStart, sourceCodeInfo, { integer: true, lte: grid.length, gte: -grid.length })
        if (rowStart < 0) {
          return grid.slice(grid.length + rowStart)
        }
        return grid.slice(rowStart)
      }

      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: grid.length })
      assertNumber(rowEnd, sourceCodeInfo, { integer: true })
      rowEnd = rowEnd < 0 ? grid.length + rowEnd : rowEnd
      assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: grid.length })

      return grid.slice(rowStart, rowEnd)
    },
    arity: { min: 2, max: 3 },
  },
  'slice-cols': {
    evaluate: ([grid, colStart, colEnd], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      const trMatrix = transpose(grid)

      if (typeof colEnd === 'undefined') {
        assertNumber(colStart, sourceCodeInfo, { integer: true, lte: trMatrix.length, gte: -trMatrix.length })
        if (colStart < 0) {
          return transpose(trMatrix.slice(trMatrix.length + colStart))
        }
        return transpose(trMatrix.slice(colStart))
      }

      assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length })
      assertNumber(colEnd, sourceCodeInfo, { integer: true })
      colEnd = colEnd < 0 ? trMatrix.length + colEnd : colEnd
      assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: trMatrix.length })

      return transpose(trMatrix.slice(colStart, colEnd))
    },
    arity: { min: 2, max: 3 },
  },
  'splice-rows': {
    evaluate: ([grid, rowStart, rowDeleteCount, ...rows], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: grid.length })
      assertNumber(rowDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true })
      if (rows.length !== 0) {
        assertGrid(rows, sourceCodeInfo)
        rows.every((row) => {
          assertArray(row, sourceCodeInfo)
          if (grid[0]!.length !== row.length) {
            throw new LitsError(`All rows must have the same length as the number of columns in grid, but got ${row.length}`, sourceCodeInfo)
          }
          return true
        })
      }

      const result: Any[][] = []
      for (let i = 0; i < rowStart; i += 1) {
        result.push(grid[i]!)
      }
      if (rows.length > 0) {
        result.push(...(rows as Any[][]))
      }
      for (let i = rowStart + rowDeleteCount; i < grid.length; i += 1) {
        result.push(grid[i]!)
      }
      return result
    },
    arity: { min: 3 },
  },
  'splice-cols': {
    evaluate: ([grid, colStart, colDeleteCount, ...cols], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      const trMatrix = transpose(grid)
      assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length })
      assertNumber(colDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true })

      if (cols.length !== 0) {
        assertGrid(cols, sourceCodeInfo)
        cols.every((row) => {
          assertArray(row, sourceCodeInfo)
          if (trMatrix[0]!.length !== row.length) {
            throw new LitsError(`All rows must have the same length as the number of rows in grid, but got ${row.length}`, sourceCodeInfo)
          }
          return true
        })
      }

      const result: Any[][] = []
      for (let i = 0; i < colStart; i += 1) {
        result.push(trMatrix[i]!)
      }
      result.push(...(cols as Any[][]))
      for (let i = colStart + colDeleteCount; i < trMatrix.length; i += 1) {
        result.push(trMatrix[i]!)
      }
      return transpose(result)
    },
    arity: { min: 3 },
  },
  'concat-rows': {
    evaluate: (params, sourceCodeInfo): Any[][] => {
      assertArray(params, sourceCodeInfo)
      params.every(grid => assertGrid(grid, sourceCodeInfo))
      const cols = (params[0] as Any[][])[0]!.length
      ;(params as Any[][][]).slice(1).every((grid) => {
        if (grid[0]!.length !== cols) {
          throw new LitsError(`All matrices must have the same number of columns, but got ${cols} and ${grid[0]!.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Any[][] = []
      ;(params as Any[][][]).forEach((grid) => {
        grid.forEach((row) => {
          result.push(row)
        })
      })
      return result
    },
    arity: { min: 1 },
  },
  'concat-cols': {
    evaluate: (params, sourceCodeInfo): Any[][] => {
      assertArray(params, sourceCodeInfo)
      params.every(grid => assertGrid(grid, sourceCodeInfo))
      const rows = (params[0] as Any[][]).length
      ;(params as Any[][][]).slice(1).every((grid) => {
        if (grid.length !== rows) {
          throw new LitsError(`All matrices must have the same number of rows, but got ${rows} and ${grid.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        ;(params as Any[][][]).forEach((grid) => {
          row.push(...grid[i]!)
        })
        result.push(row)
      }
      return result
    },
    arity: { min: 1 },
  },
  'map': {
    evaluate: (params, sourceCodeInfo, contextStack, { executeFunction }): Any[][] => {
      const fn = asFunctionLike(params.at(-1), sourceCodeInfo)
      const grids = params.slice(0, -1)
      assertGrid(grids[0], sourceCodeInfo)
      const rows = grids[0].length
      const cols = grids[0][0]!.length
      grids.slice(1).forEach((grid) => {
        assertGrid(grid, sourceCodeInfo)
        if (grid.length !== rows) {
          throw new LitsError(`All matrices must have the same number of rows, but got ${rows} and ${grid.length}`, sourceCodeInfo)
        }
        if (grid[0]!.length !== cols) {
          throw new LitsError(`All matrices must have the same number of columns, but got ${cols} and ${grid[0]!.length}`, sourceCodeInfo)
        }
      })

      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          const args = grids.map(grid => (grid as Any[][])[i]![j])
          row.push(asAny(executeFunction(fn, args, contextStack, sourceCodeInfo)))
        }
        result.push(row)
      }
      return result
    },
    arity: { min: 2 },
  },
  'mapi': {
    evaluate: ([grid, fn], sourceCodeInfo, contextStack, { executeFunction }): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      const rows = grid.length
      const cols = grid[0]!.length

      const result: Any[][] = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          row.push(asAny(executeFunction(fn, [grid[i]![j], i, j], contextStack, sourceCodeInfo)))
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(2),
  },
  'reduce': {
    evaluate: ([grid, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      let accumulator = asAny(initialValue)
      for (const row of grid) {
        for (const cell of row) {
          accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo)
        }
      }
      return accumulator
    },
    arity: toFixedArity(3),
  },
  'reducei': {
    evaluate: ([grid, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertGrid(grid, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      let accumulator = asAny(initialValue)
      for (let i = 0; i < grid.length; i += 1) {
        for (let j = 0; j < grid[i]!.length; j += 1) {
          accumulator = executeFunction(fn, [accumulator, grid[i]![j], i, j], contextStack, sourceCodeInfo)
        }
      }
      return accumulator
    },
    arity: toFixedArity(3),
  },
  'push-rows': {
    evaluate: ([grid, ...rows], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertGrid(rows, sourceCodeInfo)
      if (grid[0]!.length !== rows[0]!.length) {
        throw new LitsError(`All rows must have the same length as the number of columns in grid, but got ${grid[0]!.length} and ${rows[0]!.length}`, sourceCodeInfo)
      }
      return [...grid, ...rows]
    },
    arity: { min: 2 },
  },
  'unshift-rows': {
    evaluate: ([grid, ...rows], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertGrid(rows, sourceCodeInfo)
      if (grid[0]!.length !== rows[0]!.length) {
        throw new LitsError(`All rows must have the same length as the number of columns in grid, but got ${grid[0]!.length} and ${rows[0]!.length}`, sourceCodeInfo)
      }
      return [...rows, ...grid]
    },
    arity: { min: 2 },
  },
  'pop-row': {
    evaluate: ([grid], sourceCodeInfo): Any[][] | null => {
      assertGrid(grid, sourceCodeInfo)
      if (grid.length === 1) {
        return null
      }
      return grid.slice(0, -1)
    },
    arity: toFixedArity(1),

  },
  'shift-row': {
    evaluate: ([grid], sourceCodeInfo): Any[][] | null => {
      assertGrid(grid, sourceCodeInfo)
      if (grid.length === 1) {
        return null
      }
      return grid.slice(1)
    },
    arity: toFixedArity(1),
  },
  'push-cols': {
    evaluate: ([grid, ...cols], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertGrid(cols, sourceCodeInfo)
      if (grid.length !== cols[0]!.length) {
        throw new LitsError(`All columns must have the same length as the number of rows in grid, but got ${cols.length}`, sourceCodeInfo)
      }

      const result: Any[][] = []

      for (let i = 0; i < grid.length; i += 1) {
        const row: Any[] = []
        row.push(...grid[i]!)
        cols.forEach((col) => {
          row.push(col[i]!)
        })
        result.push(row)
      }
      return result
    },
    arity: { min: 2 },
  },
  'unshift-cols': {
    evaluate: ([grid, ...cols], sourceCodeInfo): Any[][] => {
      assertGrid(grid, sourceCodeInfo)
      assertGrid(cols, sourceCodeInfo)
      if (grid.length !== cols[0]!.length) {
        throw new LitsError(`All columns must have the same length as the number of rows in grid, but got ${cols.length}`, sourceCodeInfo)
      }

      const result: Any[][] = []

      for (let i = 0; i < grid.length; i += 1) {
        const row: Any[] = []
        cols.forEach((col) => {
          row.push(col[i]!)
        })
        row.push(...grid[i]!)
        result.push(row)
      }
      return result
    },
    arity: { min: 2 },
  },
  'pop-col': {
    evaluate: ([grid], sourceCodeInfo): Any[][] | null => {
      assertGrid(grid, sourceCodeInfo)
      if (grid[0]!.length === 1) {
        return null
      }
      return grid.map(row => row.slice(0, -1))
    },
    arity: toFixedArity(1),
  },
  'shift-col': {
    evaluate: ([grid], sourceCodeInfo): Any[][] | null => {
      assertGrid(grid, sourceCodeInfo)
      if (grid[0]!.length === 1) {
        return null
      }
      return grid.map(row => row.slice(1))
    },
    arity: toFixedArity(1),
  },
  'from-array': {
    evaluate: ([array, rows], sourceCodeInfo): unknown[][] => {
      assertArray(array, sourceCodeInfo)
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      if (array.length % rows !== 0) {
        throw new LitsError(`The number of elements in the array must be divisible by rows, but got ${array.length} and ${rows}`, sourceCodeInfo)
      }
      return fromArray(array, rows)
    },
    arity: toFixedArity(2),
  },
}

/**
 * The grid namespace containing 2D array manipulation functions.
 */
export const gridNamespace: LitsNamespace = {
  name: 'Grid',
  functions: gridFunctions,
}
