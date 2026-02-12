import type { Any } from '../../interface'

export function transpose<T extends Any>(grid: T[][]): T[][] {
  const result: T[][] = []

  for (let i = 0; i < grid[0]!.length; i += 1) {
    const row: T[] = []
    for (let j = 0; j < grid.length; j += 1) {
      row.push(grid[j]![i]!)
    }
    result.push(row)
  }
  return result
}
