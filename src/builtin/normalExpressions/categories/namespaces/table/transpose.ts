import type { Any } from '../../../../../interface'

export function transpose<T extends Any>(table: T[][]): T[][] {
  const result: T[][] = []

  for (let i = 0; i < table[0]!.length; i += 1) {
    const row: T[] = []
    for (let j = 0; j < table.length; j += 1) {
      row.push(table[j]![i]!)
    }
    result.push(row)
  }
  return result
}
