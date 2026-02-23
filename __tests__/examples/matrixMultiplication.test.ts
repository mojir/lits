import { describe, expect, it } from 'vitest'
import { Lits } from '../../src/Lits/Lits'
import { collectionUtilsModule } from '../../src/builtin/modules/collection'

const lits = new Lits({ debug: true, modules: [collectionUtilsModule] })
describe('determinant.', () => {
  it('should compile', () => {
    expect(lits.run(`
let { every? } = import(collection);
// Matrix multiplication with correct syntax
let matrixMultiply = (matrixA, matrixB) -> do
  // Check if inputs are arrays
  unless array?(matrixA) then throw("First input must be an array") end;
  unless array?(matrixB) then throw("Second input must be an array") end;

  // Check if matrices are not empty
  if empty?(matrixA) || empty?(matrixB) then throw("Matrices cannot be empty") end;

  // Check if matrices are 2D arrays
  unless array?(first(matrixA)) then throw("First input must be a 2D array") end;
  unless array?(first(matrixB)) then throw("Second input must be a 2D array") end;

  // Get dimensions
  let rowsA = count(matrixA);
  let colsA = count(first(matrixA));
  let rowsB = count(matrixB);
  let colsB = count(first(matrixB));

  // Check if all rows have consistent length
  unless every?(matrixA, row -> array?(row) && count(row) == colsA) then throw("First matrix has inconsistent row lengths") end;
  unless every?(matrixB, row -> array?(row) && count(row) == colsB) then throw("Second matrix has inconsistent row lengths") end;

  // Check if matrices can be multiplied
  unless colsA == rowsB then throw("Matrix dimensions mismatch: first matrix columns must equal second matrix rows") end;

  // Create a row of the result matrix
  let createRow = (rowIndex) -> do
    for (j in range(colsB)) -> do
      reduce(
        range(colsA),
        (acc, k) -> do
          let aValue = matrixA[rowIndex][k];
          let bValue = matrixB[k][j];
          acc + (aValue * bValue);
        end,
        0
      )
    end
  end;

  // Create the result matrix row by row
  for (i in range(rowsA)) -> createRow(i);
end;

let matrixA = [
  [1, 2, 3],
  [4, 5, 6]
];

let matrixB = [
  [7, 8],
  [9, 10],
  [11, 12]
];

matrixMultiply(matrixA, matrixB);
`)).toEqual([
      [58, 64],
      [139, 154],
    ])
  })
})
