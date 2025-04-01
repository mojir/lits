import type { Table } from '.'

export function tableEqual(tables: Table[]): boolean {
  const firstTable = tables[0]!
  return tables.slice(1).every((table) => {
    if (table.length !== firstTable.length) {
      return false
    }
    if (table[0]!.length !== firstTable[0]!.length) {
      return false
    }
    return table.every((row, i) => {
      return row.every((cell, j) => cell === firstTable[i]![j])
    })
  })
}
