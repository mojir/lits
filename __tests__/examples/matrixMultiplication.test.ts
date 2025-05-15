import { describe, expect, it } from 'vitest'
import { Lits } from '../../src/Lits/Lits'

const lits = new Lits({ debug: true })
describe('determinant.', () => {
  it('should compile', () => {
    expect(lits.run(`

// Matrix multiplication with correct syntax
function matrixMultiply(matrixA, matrixB) {
  // Check if inputs are arrays
  unless (array?(matrixA)) throw("First input must be an array");
  unless (array?(matrixB)) throw("Second input must be an array");

  // Check if matrices are not empty
  if (empty?(matrixA) || empty?(matrixB)) throw("Matrices cannot be empty");

  // Check if matrices are 2D arrays
  unless (array?(first(matrixA))) throw("First input must be a 2D array");
  unless (array?(first(matrixB))) throw("Second input must be a 2D array");

  // Get dimensions
  let rowsA = count(matrixA);
  let colsA = count(first(matrixA));
  let rowsB = count(matrixB);
  let colsB = count(first(matrixB));

  // Check if all rows have consistent length
  unless (every?(matrixA, row -> array?(row) && count(row) == colsA)) throw("First matrix has inconsistent row lengths");
  unless (every?(matrixB, row -> array?(row) && count(row) == colsB)) throw("Second matrix has inconsistent row lengths");

  // Check if matrices can be multiplied
  unless (colsA == rowsB) throw("Matrix dimensions mismatch: first matrix columns must equal second matrix rows");

  // Create a row of the result matrix
  function createRow(rowIndex) {
    for (j in range(colsB)) {
      reduce(
        range(colsA),
        (sum, k) -> {
          let aValue = matrixA[rowIndex][k];
          let bValue = matrixB[k][j];
          sum + (aValue * bValue);
        },
        0
      )
    }
  };

  // Create the result matrix row by row
  for (i in range(rowsA)) {
    createRow(i);
  }
};

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
