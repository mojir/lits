import { LitsError } from '../errors'
import type { Any } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/token'
import { isNumber } from './number'

const annotatedArrays = new WeakSet<unknown[]>()
const vectors = new WeakSet<unknown[]>()
const matrices = new WeakSet<unknown[]>()
const grids = new WeakSet<unknown[]>()

export function annotate<T>(value: T): T {
  if (!Array.isArray(value)) {
    return value
  }
  if (annotatedArrays.has(value)) {
    return value
  }
  isVector(value)
  if (!isMatrix(value)) {
    isGrid(value)
  }

  return value
}
export function isVector(vector: unknown): vector is number[] {
  if (!Array.isArray(vector)) {
    return false
  }

  if (vectors.has(vector)) {
    return true
  }

  if (vector.every(elem => isNumber(elem, { finite: true }))) {
    annotatedArrays.add(vector)
    vectors.add(vector)
    return true
  }
  return false
}

export function assertVector(vector: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts vector is number[] {
  if (!isVector(vector)) {
    throw new LitsError(`Expected a vector, but got ${vector}`, sourceCodeInfo)
  }
}

export function assertNonEmptyVector(vector: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts vector is number[] {
  assertVector(vector, sourceCodeInfo)
  if (vector.length === 0) {
    throw new LitsError(`Expected a non empty vector, but got ${vector}`, sourceCodeInfo)
  }
}

export function isGrid(grid: unknown): grid is unknown[][] {
  if (!Array.isArray(grid)) {
    return false
  }
  if (grids.has(grid)) {
    return true
  }
  if (grid.length === 0) {
    return false
  }
  if (!Array.isArray(grid[0])) {
    return false
  }
  const nbrOfCols = grid[0].length
  for (const row of grid.slice(1)) {
    if (!Array.isArray(row)) {
      return false
    }
    if (row.length !== nbrOfCols) {
      return false
    }
  }
  annotatedArrays.add(grid)
  grids.add(grid)
  return true
}

export function assertGrid(grid: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts grid is Any[][] {
  if (!isGrid(grid)) {
    throw new LitsError(`Expected a grid, but got ${grid}`, sourceCodeInfo)
  }
}

export function isMatrix(matrix: unknown): matrix is number[][] {
  if (!Array.isArray(matrix)) {
    return false
  }
  if (matrices.has(matrix)) {
    return true
  }
  if (matrix.length === 0) {
    return false
  }
  if (!Array.isArray(matrix[0])) {
    return false
  }
  const nbrOfCols = matrix[0].length
  for (const row of matrix.slice(1)) {
    if (!Array.isArray(row)) {
      return false
    }
    if (row.length !== nbrOfCols) {
      return false
    }
    if (row.some(cell => !isNumber(cell, { finite: true }))) {
      return false
    }
  }
  annotatedArrays.add(matrix)
  grids.add(matrix)
  matrices.add(matrix)
  return true
}

export function assertMatrix(matrix: unknown, sourceCodeInfo: SourceCodeInfo | undefined): asserts matrix is number[][] {
  if (!isMatrix(matrix)) {
    throw new LitsError(`Expected a matrix, but got ${matrix}`, sourceCodeInfo)
  }
}
