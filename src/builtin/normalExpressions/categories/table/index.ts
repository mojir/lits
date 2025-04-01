import { LitsError } from '../../../../errors'
import type { Any } from '../../../../interface'
import type { SourceCodeInfo } from '../../../../tokenizer/token'
import { assertArray } from '../../../../typeGuards/array'
import { asAny, assertAny, isAny } from '../../../../typeGuards/lits'
import { asLitsFunction, assertLitsFunction } from '../../../../typeGuards/litsFunction'
import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { fromArray } from './fromArray'
import { tableEqual } from './tableEqual'
import { transpose } from './transpose'

export type Table = Any[][]

function isTable(table: unknown): table is Table {
  if (!Array.isArray(table)) {
    return false
  }
  if (table.length === 0) {
    return false
  }
  if (!Array.isArray(table[0])) {
    return false
  }
  const nbrOfCols = table[0].length
  for (const row of table.slice(1)) {
    if (!Array.isArray(row)) {
      return false
    }
    if (row.length !== nbrOfCols) {
      return false
    }
    if (row.some(cell => isAny(cell))) {
      return false
    }
  }
  return true
}

function assertTable(table: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts table is Table {
  if (!isTable(table)) {
    throw new LitsError(`Expected a table, but got ${table}`, sourceCodeInfo)
  }
}

function assertAnyArray(value: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts value is Any[] {
  if (!Array.isArray(value)) {
    throw new LitsError(`Expected an array, but got ${value}`, sourceCodeInfo)
  }
  if (value.some(cell => !isAny(cell))) {
    throw new LitsError(`Expected an array of Any, but got ${value}`, sourceCodeInfo)
  }
}

export const tableNormalExpression: BuiltinNormalExpressions = {
  't:table?': {
    evaluate: ([table]): boolean => isTable(table),
    paramCount: 1,
  },
  't:=': {
    evaluate: (params, sourceCodeInfo): boolean => {
      assertArray(params, sourceCodeInfo)
      params.every(table => assertTable(table, sourceCodeInfo))
      if (params.length <= 1) {
        return true
      }
      return tableEqual(params as Table[])
    },
    paramCount: { min: 1 },
  },
  't:!=': {
    evaluate: (params, sourceCodeInfo): boolean => {
      assertArray(params, sourceCodeInfo)
      params.every(table => assertTable(table, sourceCodeInfo))
      if (params.length <= 1) {
        return false
      }
      return !tableEqual(params as Table[])
    },
    paramCount: { min: 1 },
  },
  'table-every?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      for (const row of table) {
        for (const cell of row) {
          if (!executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
            return false
          }
        }
      }
      return true
    },
    paramCount: 2,
  },
  't:some?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      for (const row of table) {
        for (const cell of row) {
          if (executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
            return true
          }
        }
      }
      return false
    },
    paramCount: 2,
  },
  't:every-row?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      for (const row of table) {
        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return false
        }
      }
      return true
    },
    paramCount: 2,
  },
  't:some-row?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      for (const row of table) {
        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return true
        }
      }
      return false
    },
    paramCount: 2,
  },
  't:every-col?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      const transposed = transpose(table)
      for (const row of transposed) {
        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return false
        }
      }
      return true
    },
    paramCount: 2,
  },
  't:some-col?': {
    evaluate: ([table, predicate], sourceCodeInfo, contextStack, { executeFunction }): boolean => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(predicate, sourceCodeInfo)

      const transposed = transpose(table)
      for (const row of transposed) {
        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
          return true
        }
      }
      return false
    },
    paramCount: 2,
  },
  't:row': {
    evaluate: ([table, row], sourceCodeInfo): Any[] => {
      assertTable(table, sourceCodeInfo)
      assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length })
      return table[row]!
    },
    paramCount: 2,
  },
  't:col': {
    evaluate: ([table, col], sourceCodeInfo): Any[] => {
      assertTable(table, sourceCodeInfo)
      assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lte: table[0]!.length })
      return table.map(row => row[col]!)
    },
    paramCount: 2,
  },
  't:shape': {
    evaluate: ([table], sourceCodeInfo): Any[] => {
      assertTable(table, sourceCodeInfo)
      return [table.length, table[0]!.length]
    },
    paramCount: 1,
  },
  't:generate': {
    evaluate: ([rows, cols, generator], sourceCodeInfo, contextStack, { executeFunction }): Table => {
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(cols, sourceCodeInfo, { integer: true, positive: true })
      assertLitsFunction(generator, sourceCodeInfo)

      const result: Table = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          const value = executeFunction(generator, [i, j], contextStack, sourceCodeInfo)
          if (!isAny(value)) {
            throw new LitsError(`The generator function must return Any, but got ${value}`, sourceCodeInfo)
          }
          row.push(value)
        }
        result.push(row)
      }
      return result
    },
    paramCount: 3,
  },
  't:reshape': {
    evaluate: ([table, rows, cols], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(cols, sourceCodeInfo, { integer: true, positive: true })

      const flatTable = table.flat()
      if (flatTable.length !== rows * cols) {
        throw new LitsError(`The number of elements in the table must be equal to rows * cols, but got ${flatTable.length} and ${rows} * ${cols}`, sourceCodeInfo)
      }

      const result: Table = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          row.push(flatTable[i * cols + j]!)
        }
        result.push(row)
      }
      return result
    },
    paramCount: 3,
  },
  't:transpose': {
    evaluate: ([table], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      return transpose(table)
    },
    paramCount: 1,
  },
  't:slice': {
    evaluate: ([table, rowStart, rowEnd, colStart, colEnd], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length })
      assertNumber(rowEnd, sourceCodeInfo, { integer: true })
      rowEnd = rowEnd < 0 ? table.length + rowEnd : rowEnd
      assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: table.length })
      assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table[0]!.length })
      assertNumber(colEnd, sourceCodeInfo, { integer: true })
      colEnd = colEnd < 0 ? table[0]!.length + colEnd : colEnd
      assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: table[0]!.length })

      const result: Table = []
      for (let i = rowStart; i < rowEnd; i += 1) {
        const row: Any[] = []
        for (let j = colStart; j < colEnd; j += 1) {
          row.push(table[i]![j]!)
        }
        result.push(row)
      }
      return result
    },
    paramCount: 5,
  },
  't:slice-rows': {
    evaluate: ([table, rowStart, rowEnd], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)

      if (typeof rowEnd === 'undefined') {
        assertNumber(rowStart, sourceCodeInfo, { integer: true, lte: table.length, gte: -table.length })
        if (rowStart < 0) {
          return table.slice(table.length + rowStart)
        }
        return table.slice(rowStart)
      }

      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length })
      assertNumber(rowEnd, sourceCodeInfo, { integer: true })
      rowEnd = rowEnd < 0 ? table.length + rowEnd : rowEnd
      assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: table.length })

      return table.slice(rowStart, rowEnd)
    },
    paramCount: 3,
  },
  't:slice-cols': {
    evaluate: ([table, colStart, colEnd], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      const trMatrix = transpose(table)

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
    paramCount: 3,
  },
  't:splice-rows': {
    evaluate: ([table, rowStart, rowDeleteCount, ...rows], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length })
      assertNumber(rowDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true })
      assertTable(rows, sourceCodeInfo)
      rows.every((row) => {
        assertArray(row, sourceCodeInfo)
        if (table[0]!.length !== row.length) {
          throw new LitsError(`All rows must have the same length as the number of columns in table, but got ${row.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Table = []
      for (let i = 0; i < rowStart; i += 1) {
        result.push(table[i]!)
      }
      result.push(...rows)
      for (let i = rowStart + rowDeleteCount; i < table.length; i += 1) {
        result.push(table[i]!)
      }
      return result
    },
    paramCount: { min: 3 },
  },
  't:splice-cols': {
    evaluate: ([table, colStart, colDeleteCount, ...cols], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      const trMatrix = transpose(table)
      assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length })
      assertNumber(colDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true })
      assertTable(cols, sourceCodeInfo)
      cols.every((row) => {
        assertArray(row, sourceCodeInfo)
        if (trMatrix[0]!.length !== row.length) {
          throw new LitsError(`All rows must have the same length as the number of rows in table, but got ${row.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Table = []
      for (let i = 0; i < colStart; i += 1) {
        result.push(trMatrix[i]!)
      }
      result.push(...cols)
      for (let i = colStart + colDeleteCount; i < trMatrix.length; i += 1) {
        result.push(trMatrix[i]!)
      }
      return transpose(result)
    },
    paramCount: { min: 3 },
  },
  't:concat-rows': {
    evaluate: (params, sourceCodeInfo): Table => {
      assertArray(params, sourceCodeInfo)
      params.every(table => assertTable(table, sourceCodeInfo))
      const cols = (params[0] as Table)[0]!.length
      ;(params as Table[]).slice(1).every((table) => {
        if (table[0]!.length !== cols) {
          throw new LitsError(`All matrices must have the same number of columns, but got ${cols} and ${table[0]!.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Table = []
      ;(params as Table[]).forEach((table) => {
        table.forEach((row) => {
          result.push(row)
        })
      })
      return result
    },
    paramCount: { min: 1 },
  },
  't:concat-cols': {
    evaluate: (params, sourceCodeInfo): Table => {
      assertArray(params, sourceCodeInfo)
      params.every(table => assertTable(table, sourceCodeInfo))
      const rows = (params[0] as Table).length
      ;(params as Table[]).slice(1).every((table) => {
        if (table.length !== rows) {
          throw new LitsError(`All matrices must have the same number of rows, but got ${rows} and ${table.length}`, sourceCodeInfo)
        }
        return true
      })

      const result: Table = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        ;(params as Table[]).forEach((table) => {
          row.push(...table[i]!)
        })
        result.push(row)
      }
      return result
    },
    paramCount: { min: 1 },
  },
  't:map': {
    evaluate: (params, sourceCodeInfo, contextStack, { executeFunction }): Table => {
      const fn = asLitsFunction(params.at(-1))
      const matrices = params.slice(0, -1)
      assertTable(matrices[0], sourceCodeInfo)
      const rows = matrices[0].length
      const cols = matrices[0][0]!.length
      matrices.slice(1).forEach((table) => {
        assertTable(table, sourceCodeInfo)
        if (table.length !== rows) {
          throw new LitsError(`All matrices must have the same number of rows, but got ${rows} and ${table.length}`, sourceCodeInfo)
        }
        if (table[0]!.length !== cols) {
          throw new LitsError(`All matrices must have the same number of columns, but got ${cols} and ${table[0]!.length}`, sourceCodeInfo)
        }
      })

      const result: Table = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          const args = matrices.map(table => (table as Table)[i]![j])
          const value = executeFunction(fn, args, contextStack, sourceCodeInfo)
          if (!isAny(value)) {
            throw new LitsError(`The function must return a number, but got ${value}`, sourceCodeInfo)
          }
          row.push(value)
        }
        result.push(row)
      }
      return result
    },
    paramCount: { min: 2 },
  },
  't:map-with-indices': {
    evaluate: ([table, fn], sourceCodeInfo, contextStack, { executeFunction }): Table => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      const rows = table.length
      const cols = table[0]!.length

      const result: Table = []
      for (let i = 0; i < rows; i += 1) {
        const row: Any[] = []
        for (let j = 0; j < cols; j += 1) {
          const value = executeFunction(fn, [table[i]![j], i, j], contextStack, sourceCodeInfo)
          if (!isAny(value)) {
            throw new LitsError(`The function must return a number, but got ${value}`, sourceCodeInfo)
          }
          row.push(value)
        }
        result.push(row)
      }
      return result
    },
    paramCount: 2,
  },
  't:reduce': {
    evaluate: ([table, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      let accumulator = asAny(initialValue)
      for (const row of table) {
        for (const cell of row) {
          accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo)
        }
      }
      return accumulator
    },
    paramCount: 3,
  },
  't:reduce-with-indices': {
    evaluate: ([table, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)

      let accumulator = asAny(initialValue)
      for (let i = 0; i < table.length; i += 1) {
        for (let j = 0; j < table[i]!.length; j += 1) {
          accumulator = executeFunction(fn, [accumulator, table[i]![j], i, j], contextStack, sourceCodeInfo)
        }
      }
      return accumulator
    },
    paramCount: 3,
  },
  't:reduce-rows': {
    evaluate: ([table, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any[] => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)
      assertAny(initialValue)

      const result: Any[] = []
      for (const row of table) {
        let accumulator = asAny(initialValue)
        for (const cell of row) {
          accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo)
        }
        result.push(accumulator)
      }
      return result
    },
    paramCount: 3,
  },
  't:reduce-cols': {
    evaluate: ([table, fn, initialValue], sourceCodeInfo, contextStack, { executeFunction }): Any[] => {
      assertTable(table, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)
      assertAny(initialValue)

      const result: Any[] = []
      for (let j = 0; j < table[0]!.length; j += 1) {
        let accumulator = asAny(initialValue)
        for (let i = 0; i < table.length; i += 1) {
          accumulator = executeFunction(fn, [accumulator, table[i]![j]], contextStack, sourceCodeInfo)
        }
        result.push(accumulator)
      }
      return result
    },
    paramCount: 3,
  },
  't:push-rows': {
    evaluate: ([table, ...rows], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertTable(rows, sourceCodeInfo)
      if (table[0]!.length !== rows[0]!.length) {
        throw new LitsError(`All rows must have the same length as the number of columns in table, but got ${table[0]!.length} and ${rows[0]!.length}`, sourceCodeInfo)
      }
      return [...table, ...rows]
    },
    paramCount: { min: 2 },
  },
  't:unshift-rows': {
    evaluate: ([table, ...rows], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertTable(rows, sourceCodeInfo)
      if (table[0]!.length !== rows[0]!.length) {
        throw new LitsError(`All rows must have the same length as the number of columns in table, but got ${table[0]!.length} and ${rows[0]!.length}`, sourceCodeInfo)
      }
      return [...rows, ...table]
    },
    paramCount: { min: 2 },
  },
  't:pop-row': {
    evaluate: ([table], sourceCodeInfo): Table | null => {
      assertTable(table, sourceCodeInfo)
      if (table.length === 1) {
        return null
      }
      return table.slice(0, -1)
    },
    paramCount: 1,

  },
  't:shift-row': {
    evaluate: ([table], sourceCodeInfo): Table | null => {
      assertTable(table, sourceCodeInfo)
      if (table.length === 1) {
        return null
      }
      return table.slice(1)
    },
    paramCount: 1,
  },
  't:push-cols': {
    evaluate: ([table, ...cols], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertTable(cols, sourceCodeInfo)
      if (table.length !== cols[0]!.length) {
        throw new LitsError(`All columns must have the same length as the number of rows in table, but got ${cols.length}`, sourceCodeInfo)
      }

      const result: Table = []

      for (let i = 0; i < table.length; i += 1) {
        const row: Any[] = []
        row.push(...table[i]!)
        cols.forEach((col) => {
          row.push(col[i]!)
        })
        result.push(row)
      }
      return result
    },
    paramCount: { min: 2 },
  },
  't:unshift-cols': {
    evaluate: ([table, ...cols], sourceCodeInfo): Table => {
      assertTable(table, sourceCodeInfo)
      assertTable(cols, sourceCodeInfo)
      if (table.length !== cols[0]!.length) {
        throw new LitsError(`All columns must have the same length as the number of rows in table, but got ${cols.length}`, sourceCodeInfo)
      }

      const result: Table = []

      for (let i = 0; i < table.length; i += 1) {
        const row: Any[] = []
        cols.forEach((col) => {
          row.push(col[i]!)
        })
        row.push(...table[i]!)
        result.push(row)
      }
      return result
    },
    paramCount: { min: 2 },
  },
  't:pop-col': {
    evaluate: ([table], sourceCodeInfo): Table | null => {
      assertTable(table, sourceCodeInfo)
      if (table[0]!.length === 1) {
        return null
      }
      return table.map(row => row.slice(0, -1))
    },
    paramCount: 1,
  },
  't:shift-col': {
    evaluate: ([table], sourceCodeInfo): Table | null => {
      assertTable(table, sourceCodeInfo)
      if (table[0]!.length === 1) {
        return null
      }
      return table.map(row => row.slice(1))
    },
    paramCount: 1,
  },
  't:from-array': {
    evaluate: ([array, rows, cols], sourceCodeInfo): Table => {
      assertAnyArray(array, sourceCodeInfo)
      assertNumber(rows, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(cols, sourceCodeInfo, { integer: true, positive: true })
      if (array.length !== rows * cols) {
        throw new LitsError(`The number of elements in the array must be equal to rows * cols, but got ${array.length} and ${rows} * ${cols}`, sourceCodeInfo)
      }
      return fromArray(array, rows, cols)
    },
    paramCount: 3,
  },
}
