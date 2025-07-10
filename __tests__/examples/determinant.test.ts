import { describe, expect, it } from 'vitest'
import { Lits } from '../../src/Lits/Lits'

const lits = new Lits({ debug: true })
describe('determinant.', () => {
  it('should compile', () => {
    expect(lits.run(`
// Determinant function for square matrices
let determinant = (matrix) -> {
  // Check if input is an array
  if !(array?(matrix)) then {
    throw("Input must be an array");
  };

  // Check if matrix is empty
  if empty?(matrix) then {
    throw("Matrix cannot be empty");
  };

  let rows = count(matrix);
  
  // Get first row to check column count
  let firstRow = first(matrix);
  
  // Check if first row is an array
  if !(array?(firstRow)) then {
    throw("Input must be a 2D array");
  };
  
  let cols = count(firstRow);
  
  // Ensure matrix is square
  if rows ≠ cols then {
    throw("Matrix must be square");
  };
  
  // Base case: 1x1 matrix
  if rows == 1 then {
    get(get(matrix, 0), 0);
  } else {
    // Base case: 2x2 matrix
    if rows == 2 then {
      let a = matrix[0][0];
      let b = matrix[0][1];
      let c = matrix[1][0];
      let d = matrix[1][1];
      
      a * d - b * c;
    } else {
      // For larger matrices, use cofactor expansion along first row
      // Use reduce to calculate the determinant without mutating variables
      reduce(
        range(cols),
        (acc, j) -> {
          let minor = getMinor(matrix, 0, j);
          let cofactor = determinant(minor);
          let signFactor = if even?(j) then {
            1;
          } else {
            -1;
          };
          let term = signFactor * get(get(matrix, 0), j) * cofactor;
          
          acc + term;
        },
        0,
      );
    }
  }
};

// Helper function to get minor (submatrix) by removing specific row and column
let getMinor = (matrix, rowToRemove, colToRemove) -> {
  // Use map with filter to create the new matrix without mutating
  map(
    range(count(matrix)),
    i -> {
      if i == rowToRemove then {
        null; // This will be filtered out
      } else {
        let row = get(matrix, i);
        // Filter out the column to remove
        map(
          range(count(row)),
          j -> {
            if j == colToRemove then {
              null; // This will be filtered out
            } else {
              get(row, j);
            };
          },
        ) filter (item -> item ≠ null);
      };
    },
  ) filter (row -> row ≠ null);
};
  
// 3x3 invertible matrix
let matrix4 = [[2, 3, 4], [1, 2, 3], [3, 4, 1]];
determinant(matrix4);
   `)).toBe(-4)
  })
})
