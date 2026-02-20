import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'
import { gridModule } from './'

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

const lits = new Lits({ modules: [gridModule] })

// Helper to run grid module functions with the new import syntax
function runGrid(code: string): unknown {
  // Replace 'grid:functionName(' with 'let g = import("grid"); g.functionName('
  const modifiedCode = code.replace(/grid:(\S+?)\(/g, 'let g = import("grid"); g.$1(')
  return lits.run(modifiedCode)
}

describe('grid', () => {
  describe('grid:every?', () => {
    it('should check if every element in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:every?(${exampleGrid1}, string?)`)).toBe(false)
      expect(runGrid(`grid:every?(${exampleGrid2}, string?)`)).toBe(true)
      expect(runGrid(`grid:every?(${exampleGrid3}, string?)`)).toBe(false)
    })
  })
  describe('grid:some?', () => {
    it('should check if some element in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:some?(${exampleGrid1}, string?)`)).toBe(true)
      expect(runGrid(`grid:some?(${exampleGrid2}, string?)`)).toBe(true)
      expect(runGrid(`grid:some?(${exampleGrid3}, string?)`)).toBe(false)
    })
  })
  describe('grid:every-row?', () => {
    it('should check if every row in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:every-row?(${exampleGrid1}, -> string?($[0]))`)).toBe(true)
      expect(runGrid(`grid:every-row?(${exampleGrid2}, -> string?($[0]))`)).toBe(true)
      expect(runGrid(`grid:every-row?(${exampleGrid3}, -> string?($[0]))`)).toBe(false)
    })
  })
  describe('grid:some-row?', () => {
    it('should check if some row in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:some-row?(${exampleGrid1}, -> $ contains? "Albert")`)).toBe(true)
      expect(runGrid(`grid:some-row?(${exampleGrid2}, -> $ contains? "Albert")`)).toBe(true)
      expect(runGrid(`grid:some-row?(${exampleGrid3}, -> $ contains? "Albert")`)).toBe(false)
    })
  })
  describe('grid:every-col?', () => {
    it('should check if every column in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:every-col?(${exampleGrid1}, -> string?($[0]))`)).toBe(false)
      expect(runGrid(`grid:every-col?(${exampleGrid2}, -> string?($[0]))`)).toBe(true)
      expect(runGrid(`grid:every-col?(${exampleGrid3}, -> string?($[0]))`)).toBe(false)
    })
  })
  describe('grid:some-col?', () => {
    it('should check if some column in the grid satisfies the predicate', () => {
      expect(runGrid(`grid:some-col?(${exampleGrid1}, -> $ contains? "Albert")`)).toBe(true)
      expect(runGrid(`grid:some-col?(${exampleGrid2}, -> $ contains? "Albert")`)).toBe(true)
      expect(runGrid(`grid:some-col?(${exampleGrid3}, -> $ contains? "Albert")`)).toBe(false)
    })
  })
  describe('grid:row', () => {
    it('should return the row at the given index', () => {
      expect(runGrid(`grid:row(${exampleGrid1}, 0)`)).toEqual(['Albert', 'father', 10])
      expect(runGrid(`grid:row(${exampleGrid1}, 1)`)).toEqual(['Nina', 'mother', 20])
      expect(runGrid(`grid:row(${exampleGrid1}, 2)`)).toEqual(['Kian', 'son', 30])
    })
    it('should throw an error if the index is out of bounds', () => {
      expect(() => runGrid(`grid:row(${exampleGrid1}, 3)`)).toThrow(LitsError)
      expect(() => runGrid(`grid:row(${exampleGrid1}, -1)`)).toThrow(LitsError)
    })
  })
  describe('grid:col', () => {
    it('should return the column at the given index', () => {
      expect(runGrid(`grid:col(${exampleGrid1}, 0)`)).toEqual(['Albert', 'Nina', 'Kian'])
      expect(runGrid(`grid:col(${exampleGrid1}, 1)`)).toEqual(['father', 'mother', 'son'])
      expect(runGrid(`grid:col(${exampleGrid1}, 2)`)).toEqual([10, 20, 30])
    })
    it('should throw an error if the index is out of bounds', () => {
      expect(() => runGrid(`grid:col(${exampleGrid1}, 3)`)).toThrow(LitsError)
      expect(() => runGrid(`grid:col(${exampleGrid1}, -1)`)).toThrow(LitsError)
    })
  })
  describe('grid:shape', () => {
    it('should return the shape of the grid', () => {
      expect(runGrid(`grid:shape(${exampleGrid1})`)).toEqual([3, 3])
      expect(runGrid(`grid:shape(${exampleGrid2})`)).toEqual([3, 2])
      expect(runGrid(`grid:shape(${exampleGrid3})`)).toEqual([2, 2])
    })
  })
  describe('grid:fill', () => {
    it('should fill the grid with the given value', () => {
      expect(runGrid('grid:fill(3, 3, 0)')).toEqual([
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
      ])
    })
  })
  describe('grid:generate', () => {
    it('should generate a grid of the given shape', () => {
      expect(runGrid('grid:generate(3, 3, -> 0)')).toEqual([
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
      ])
      expect(runGrid('grid:generate(2, 4, -> $1 + $2)')).toEqual([
        [0, 1, 2, 3],
        [1, 2, 3, 4],
      ])
    })
  })
  describe('grid:reshape', () => {
    it('should reshape the grid to the given shape', () => {
      expect(runGrid(`grid:reshape(${exampleGrid2}, 2)`)).toEqual([
        ['Albert', 'father', 'Nina'],
        ['mother', 'Kian', 'son'],
      ])
      expect(() => runGrid(`grid:reshape(${exampleGrid2}, 5)`)).toThrow(LitsError)
    })
  })
  describe('grid:transpose', () => {
    it('should transpose the grid', () => {
      expect(runGrid(`grid:transpose(${exampleGrid1})`)).toEqual([
        ['Albert', 'Nina', 'Kian'],
        ['father', 'mother', 'son'],
        [10, 20, 30],
      ])
      expect(runGrid(`grid:transpose(${exampleGrid2})`)).toEqual([
        ['Albert', 'Nina', 'Kian'],
        ['father', 'mother', 'son'],
      ])
    })
  })
  describe('grid:slice', () => {
    it('should slice the grid', () => {
      expect(runGrid(`grid:slice(${exampleGrid1}, [1, 1], [2, 2])`)).toEqual([
        ['mother'],
      ])
      expect(runGrid(`grid:slice(${exampleGrid1}, [1, 1])`)).toEqual([
        ['mother', 20],
        ['son', 30],
      ])
      expect(() => runGrid(`grid:slice(${exampleGrid1}, [1, 1, 1], [2, 2])`)).toThrow(LitsError)
      expect(() => runGrid(`grid:slice(${exampleGrid1}, [1, 1], [2, 2, 2])`)).toThrow(LitsError)
    })
  })
  describe('grid:slice-rows', () => {
    it('should slice the rows of the grid', () => {
      expect(runGrid(`grid:slice-rows(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Nina', 'mother', 20],
      ])
      expect(runGrid(`grid:slice-rows(${exampleGrid1}, 1)`)).toEqual([
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
      expect(runGrid(`grid:slice-rows(${exampleGrid1}, 1, -1)`)).toEqual([
        ['Nina', 'mother', 20],
      ])
      expect(runGrid(`grid:slice-rows(${exampleGrid1}, -2)`)).toEqual([
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
    })
  })
  describe('grid:slice-cols', () => {
    it('should slice the columns of the grid', () => {
      expect(runGrid(`grid:slice-cols(${exampleGrid1}, 1, 2)`)).toEqual([
        ['father'],
        ['mother'],
        ['son'],
      ])
      expect(runGrid(`grid:slice-cols(${exampleGrid1}, 1)`)).toEqual([
        ['father', 10],
        ['mother', 20],
        ['son', 30],
      ])
      expect(runGrid(`grid:slice-cols(${exampleGrid1}, 1, -1)`)).toEqual([
        ['father'],
        ['mother'],
        ['son'],
      ])
      expect(runGrid(`grid:slice-cols(${exampleGrid1}, -1)`)).toEqual([
        [10],
        [20],
        [30],
      ])
    })
  })
  describe('grid:splice-rows', () => {
    it('should splice the rows of the grid', () => {
      expect(runGrid(`grid:splice-rows(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Albert', 'father', 10],
      ])
      expect(runGrid(`grid:splice-rows(${exampleGrid1}, 1, 1, ["Nazanin", "mother", 40])`)).toEqual([
        ['Albert', 'father', 10],
        ['Nazanin', 'mother', 40],
        ['Kian', 'son', 30],
      ])
      expect(() => runGrid(`grid:splice-rows(${exampleGrid1}, 1, 1, ["Nazanin", "mother"])`)).toThrow(LitsError)
    })
  })
  describe('grid:splice-cols', () => {
    it('should splice the columns of the grid', () => {
      expect(runGrid(`grid:splice-cols(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Albert'],
        ['Nina'],
        ['Kian'],
      ])
      expect(runGrid(`grid:splice-cols(${exampleGrid1}, 1, 1, ["f", "m", "s"])`)).toEqual([
        ['Albert', 'f', 10],
        ['Nina', 'm', 20],
        ['Kian', 's', 30],
      ])
      expect(() => runGrid(`grid:splice-cols(${exampleGrid1}, 1, 1, ["f", "m"])`)).toThrow(LitsError)
    })
  })
  describe('grid:concat-rows', () => {
    it('should concatenate the rows of the grid', () => {
      expect(runGrid(`grid:concat-rows(${exampleGrid2}, ${exampleGrid3})`)).toEqual([
        ['Albert', 'father'],
        ['Nina', 'mother'],
        ['Kian', 'son'],
        [1, 2],
        [3, 4],
      ])
      expect(() => runGrid(`grid:concat-rows(${exampleGrid1}, ${exampleGrid2})`)).toThrow(LitsError)
    })
  })
  describe('grid:concat-cols', () => {
    it('should concatenate the columns of the grid', () => {
      expect(runGrid(`grid:concat-cols(${exampleGrid1}, ${exampleGrid2})`)).toEqual([
        ['Albert', 'father', 10, 'Albert', 'father'],
        ['Nina', 'mother', 20, 'Nina', 'mother'],
        ['Kian', 'son', 30, 'Kian', 'son'],
      ])
      expect(() => runGrid(`grid:concat-cols(${exampleGrid2}, ${exampleGrid3})`)).toThrow(LitsError)
    })
  })
  describe('grid:map', () => {
    it('should map the grid', () => {
      expect(runGrid(`grid:map(${exampleGrid1}, str)`)).toEqual([
        ['Albert', 'father', '10'],
        ['Nina', 'mother', '20'],
        ['Kian', 'son', '30'],
      ])
    })
    it('should map multiple grids', () => {
      expect(runGrid(`grid:map(${exampleGrid3}, ${exampleGrid3}, +)`)).toEqual([[2, 4], [6, 8]])
    })
    it('should throw on different dimensions', () => {
      expect(() => runGrid(`grid:map(${exampleGrid3}, [[1], [2]], +)`)).toThrow(LitsError)
      expect(() => runGrid(`grid:map(${exampleGrid3}, [[1, 2]], +)`)).toThrow(LitsError)
    })
  })
  describe('grid:mapi', () => {
    it('should map the grid with index', () => {
      expect(runGrid(`grid:mapi(${exampleGrid1}, -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")`)).toEqual([
        ['Albert(0, 0)', 'father(0, 1)', '10(0, 2)'],
        ['Nina(1, 0)', 'mother(1, 1)', '20(1, 2)'],
        ['Kian(2, 0)', 'son(2, 1)', '30(2, 2)'],
      ])
    })
  })
  describe('grid:reduce', () => {
    it('should reduce the grid', () => {
      expect(runGrid(`grid:reduce(${exampleGrid1}, ++, "")`)).toEqual('Albertfather10Ninamother20Kianson30')
    })
  })
  describe('grid:reducei', () => {
    it('should reduce the grid with index', () => {
      expect(runGrid(`grid:reducei(${exampleGrid1}, -> $ + $3, 0)`)).toBe(9)
    })
  })
  describe('grid:push-rows', () => {
    it('should push rows to the grid', () => {
      expect(runGrid(`grid:push-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`)).toEqual([
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
        ['Nazanin', 'mother', 40],
      ])
      expect(() => runGrid(`grid:push-rows(${exampleGrid1}, ["Nazanin", 40])`)).toThrowError(LitsError)
    })
  })
  describe('grid:push-cols', () => {
    it('should push columns to the grid', () => {
      expect(runGrid(`grid:push-cols(${exampleGrid1}, ["f", "m", "s"])`)).toEqual([
        ['Albert', 'father', 10, 'f'],
        ['Nina', 'mother', 20, 'm'],
        ['Kian', 'son', 30, 's'],
      ])
    })
    it('should throw an error if the number of rows does not match', () => {
      expect(() => runGrid(`grid:push-cols(${exampleGrid1}, ["f", "m"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:unshift-rows', () => {
    it('should unshift rows to the grid', () => {
      expect(runGrid(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`)).toEqual([
        ['Nazanin', 'mother', 40],
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
      expect(() => runGrid(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", 40])`)).toThrowError(LitsError)
    })
    it('should throw an error if the number of columns does not match', () => {
      expect(() => runGrid(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", "mother"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:unshift-cols', () => {
    it('should unshift columns to the grid', () => {
      expect(runGrid(`grid:unshift-cols(${exampleGrid1}, ["f", "m", "s"])`)).toEqual([
        ['f', 'Albert', 'father', 10],
        ['m', 'Nina', 'mother', 20],
        ['s', 'Kian', 'son', 30],
      ])
    })
    it('should throw an error if the number of rows does not match', () => {
      expect(() => runGrid(`grid:unshift-cols(${exampleGrid1}, ["f", "m"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:pop-row', () => {
    it('should pop rows from the grid', () => {
      expect(runGrid(`grid:pop-row(${exampleGrid1})`)).toEqual([
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
      ])
      expect(runGrid('grid:pop-row([[1, 2]])')).toEqual(null)
    })
  })
  describe('grid:pop-col', () => {
    it('should pop columns from the grid', () => {
      expect(runGrid(`grid:pop-col(${exampleGrid1})`)).toEqual([
        ['Albert', 'father'],
        ['Nina', 'mother'],
        ['Kian', 'son'],
      ])
      expect(runGrid('grid:pop-col([[1], [2]])')).toEqual(null)
    })
  })
  describe('grid:shift-row', () => {
    it('should shift rows from the grid', () => {
      expect(runGrid(`grid:shift-row(${exampleGrid1})`)).toEqual([
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
    })
    it('should return null for single row grid', () => {
      expect(runGrid('grid:shift-row([[1, 2]])')).toEqual(null)
    })
  })
  describe('grid:shift-col', () => {
    it('should shift columns from the grid', () => {
      expect(runGrid(`grid:shift-col(${exampleGrid1})`)).toEqual([
        ['father', 10],
        ['mother', 20],
        ['son', 30],
      ])
    })
    it('should return null for single column grid', () => {
      expect(runGrid('grid:shift-col([[1], [2]])')).toEqual(null)
    })
  })
  describe('grid:from-array', () => {
    it('should convert an array to a grid', () => {
      expect(runGrid('grid:from-array([1, 2, 3, 4], 2)')).toEqual([
        [1, 2],
        [3, 4],
      ])
      expect(() => runGrid('grid:from-array([1, 2, 3], 2)')).toThrowError(LitsError)
    })
  })
  describe('grid:rotate', () => {
    it('should rotate the grid', () => {
      expect(runGrid(`grid:rotate(${exampleGrid1}, 1)`)).toEqual([
        ['Kian', 'Nina', 'Albert'],
        ['son', 'mother', 'father'],
        [30, 20, 10],
      ])
      expect(runGrid(`grid:rotate(${exampleGrid1}, 2)`)).toEqual([
        [30, 'son', 'Kian'],
        [20, 'mother', 'Nina'],
        [10, 'father', 'Albert'],
      ])
      expect(runGrid(`grid:rotate(${exampleGrid1}, 3)`)).toEqual([
        [10, 20, 30],
        ['father', 'mother', 'son'],
        ['Albert', 'Nina', 'Kian'],
      ])
      expect(runGrid(`grid:rotate(${exampleGrid1}, 4)`)).toEqual([
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
    })
  })
  describe('grid:flip-h', () => {
    it('should flip the grid horizontally', () => {
      expect(runGrid(`grid:flip-h(${exampleGrid1})`)).toEqual([
        [10, 'father', 'Albert'],
        [20, 'mother', 'Nina'],
        [30, 'son', 'Kian'],
      ])
    })
  })
  describe('grid:flip-v', () => {
    it('should flip the grid vertically', () => {
      expect(runGrid(`grid:flip-v(${exampleGrid1})`)).toEqual([
        ['Kian', 'son', 30],
        ['Nina', 'mother', 20],
        ['Albert', 'father', 10],
      ])
    })
  })
  describe('grid:reverse-rows', () => {
    it('should reverse the rows of the grid', () => {
      expect(runGrid(`grid:reverse-rows(${exampleGrid1})`)).toEqual([
        ['Kian', 'son', 30],
        ['Nina', 'mother', 20],
        ['Albert', 'father', 10],
      ])
    })
  })
  describe('grid:reverse-cols', () => {
    it('should reverse the columns of the grid', () => {
      expect(runGrid(`grid:reverse-cols(${exampleGrid1})`)).toEqual([
        [10, 'father', 'Albert'],
        [20, 'mother', 'Nina'],
        [30, 'son', 'Kian'],
      ])
    })
  })
})

describe('import with dot notation', () => {
  it('should import a single function directly', () => {
    expect(lits.run('let row = import("grid.row"); row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
  })

  it('should throw for unknown function (former alias)', () => {
    expect(() => lits.run('let tp = import("grid.tr"); tp([[1, 2], [3, 4]])')).toThrow(LitsError)
  })

  it('should throw for unknown function', () => {
    expect(() => lits.run('import("grid.unknown")')).toThrow(LitsError)
  })

  it('should throw for unknown module', () => {
    expect(() => lits.run('import("unknown.row")')).toThrow(LitsError)
  })

  it('should work with function composition', () => {
    expect(lits.run(`
      let transpose = import("grid.transpose");
      let row = import("grid.row");
      row(transpose([[1, 2], [3, 4]]), 1)
    `)).toEqual([2, 4])
  })
})
