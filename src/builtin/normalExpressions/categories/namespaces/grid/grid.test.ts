import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

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

const lits = new Lits()

describe('grid', () => {
  describe('grid:every?', () => {
    it('should check if every element in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:every?(${exampleGrid1}, string?)`)).toBe(false)
      expect(lits.run(`grid:every?(${exampleGrid2}, string?)`)).toBe(true)
      expect(lits.run(`grid:every?(${exampleGrid3}, string?)`)).toBe(false)
    })
  })
  describe('grid:some?', () => {
    it('should check if some element in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:some?(${exampleGrid1}, string?)`)).toBe(true)
      expect(lits.run(`grid:some?(${exampleGrid2}, string?)`)).toBe(true)
      expect(lits.run(`grid:some?(${exampleGrid3}, string?)`)).toBe(false)
    })
  })
  describe('grid:every-row?', () => {
    it('should check if every row in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:every-row?(${exampleGrid1}, -> string?($[0]))`)).toBe(true)
      expect(lits.run(`grid:every-row?(${exampleGrid2}, -> string?($[0]))`)).toBe(true)
      expect(lits.run(`grid:every-row?(${exampleGrid3}, -> string?($[0]))`)).toBe(false)
    })
  })
  describe('grid:some-row?', () => {
    it('should check if some row in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:some-row?(${exampleGrid1}, -> $ contains? "Albert")`)).toBe(true)
      expect(lits.run(`grid:some-row?(${exampleGrid2}, -> $ contains? "Albert")`)).toBe(true)
      expect(lits.run(`grid:some-row?(${exampleGrid3}, -> $ contains? "Albert")`)).toBe(false)
    })
  })
  describe('grid:every-col?', () => {
    it('should check if every column in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:every-col?(${exampleGrid1}, -> string?($[0]))`)).toBe(false)
      expect(lits.run(`grid:every-col?(${exampleGrid2}, -> string?($[0]))`)).toBe(true)
      expect(lits.run(`grid:every-col?(${exampleGrid3}, -> string?($[0]))`)).toBe(false)
    })
  })
  describe('grid:some-col?', () => {
    it('should check if some column in the grid satisfies the predicate', () => {
      expect(lits.run(`grid:some-col?(${exampleGrid1}, -> $ contains? "Albert")`)).toBe(true)
      expect(lits.run(`grid:some-col?(${exampleGrid2}, -> $ contains? "Albert")`)).toBe(true)
      expect(lits.run(`grid:some-col?(${exampleGrid3}, -> $ contains? "Albert")`)).toBe(false)
    })
  })
  describe('grid:row', () => {
    it('should return the row at the given index', () => {
      expect(lits.run(`grid:row(${exampleGrid1}, 0)`)).toEqual(['Albert', 'father', 10])
      expect(lits.run(`grid:row(${exampleGrid1}, 1)`)).toEqual(['Nina', 'mother', 20])
      expect(lits.run(`grid:row(${exampleGrid1}, 2)`)).toEqual(['Kian', 'son', 30])
    })
    it('should throw an error if the index is out of bounds', () => {
      expect(() => lits.run(`grid:row(${exampleGrid1}, 3)`)).toThrow(LitsError)
      expect(() => lits.run(`grid:row(${exampleGrid1}, -1)`)).toThrow(LitsError)
    })
  })
  describe('grid:col', () => {
    it('should return the column at the given index', () => {
      expect(lits.run(`grid:col(${exampleGrid1}, 0)`)).toEqual(['Albert', 'Nina', 'Kian'])
      expect(lits.run(`grid:col(${exampleGrid1}, 1)`)).toEqual(['father', 'mother', 'son'])
      expect(lits.run(`grid:col(${exampleGrid1}, 2)`)).toEqual([10, 20, 30])
    })
    it('should throw an error if the index is out of bounds', () => {
      expect(() => lits.run(`grid:col(${exampleGrid1}, 3)`)).toThrow(LitsError)
      expect(() => lits.run(`grid:col(${exampleGrid1}, -1)`)).toThrow(LitsError)
    })
  })
  describe('grid:shape', () => {
    it('should return the shape of the grid', () => {
      expect(lits.run(`grid:shape(${exampleGrid1})`)).toEqual([3, 3])
      expect(lits.run(`grid:shape(${exampleGrid2})`)).toEqual([3, 2])
      expect(lits.run(`grid:shape(${exampleGrid3})`)).toEqual([2, 2])
    })
  })
  describe('grid:generate', () => {
    it('should generate a grid of the given shape', () => {
      expect(lits.run('grid:generate(3, 3, -> 0)')).toEqual([
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
      ])
      expect(lits.run('grid:generate(2, 4, -> $1 + $2)')).toEqual([
        [0, 1, 2, 3],
        [1, 2, 3, 4],
      ])
    })
  })
  describe('grid:reshape', () => {
    it('should reshape the grid to the given shape', () => {
      expect(lits.run(`grid:reshape(${exampleGrid2}, 2)`)).toEqual([
        ['Albert', 'father', 'Nina'],
        ['mother', 'Kian', 'son'],
      ])
      expect(() => lits.run(`grid:reshape(${exampleGrid2}, 5)`)).toThrow(LitsError)
    })
  })
  describe('grid:transpose', () => {
    it('should transpose the grid', () => {
      expect(lits.run(`grid:transpose(${exampleGrid1})`)).toEqual([
        ['Albert', 'Nina', 'Kian'],
        ['father', 'mother', 'son'],
        [10, 20, 30],
      ])
      expect(lits.run(`grid:transpose(${exampleGrid2})`)).toEqual([
        ['Albert', 'Nina', 'Kian'],
        ['father', 'mother', 'son'],
      ])
    })
  })
  describe('grid:slice', () => {
    it('should slice the grid', () => {
      expect(lits.run(`grid:slice(${exampleGrid1}, [1, 1], [2, 2])`)).toEqual([
        ['mother'],
      ])
      expect(lits.run(`grid:slice(${exampleGrid1}, [1, 1])`)).toEqual([
        ['mother', 20],
        ['son', 30],
      ])
    })
  })
  describe('grid:slice-rows', () => {
    it('should slice the rows of the grid', () => {
      expect(lits.run(`grid:slice-rows(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Nina', 'mother', 20],
      ])
      expect(lits.run(`grid:slice-rows(${exampleGrid1}, 1)`)).toEqual([
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
    })
  })
  describe('grid:slice-cols', () => {
    it('should slice the columns of the grid', () => {
      expect(lits.run(`grid:slice-cols(${exampleGrid1}, 1, 2)`)).toEqual([
        ['father'],
        ['mother'],
        ['son'],
      ])
      expect(lits.run(`grid:slice-cols(${exampleGrid1}, 1)`)).toEqual([
        ['father', 10],
        ['mother', 20],
        ['son', 30],
      ])
    })
  })
  describe('grid:splice-rows', () => {
    it('should splice the rows of the grid', () => {
      expect(lits.run(`grid:splice-rows(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Albert', 'father', 10],
      ])
      expect(lits.run(`grid:splice-rows(${exampleGrid1}, 1, 1, ["Nazanin", "mother", 40])`)).toEqual([
        ['Albert', 'father', 10],
        ['Nazanin', 'mother', 40],
        ['Kian', 'son', 30],
      ])
    })
  })
  describe('grid:splice-cols', () => {
    it('should splice the columns of the grid', () => {
      expect(lits.run(`grid:splice-cols(${exampleGrid1}, 1, 2)`)).toEqual([
        ['Albert'],
        ['Nina'],
        ['Kian'],
      ])
      expect(lits.run(`grid:splice-cols(${exampleGrid1}, 1, 1, ["f", "m", "s"])`)).toEqual([
        ['Albert', 'f', 10],
        ['Nina', 'm', 20],
        ['Kian', 's', 30],
      ])
    })
  })
  describe('grid:concat-rows', () => {
    it('should concatenate the rows of the grid', () => {
      expect(lits.run(`grid:concat-rows(${exampleGrid2}, ${exampleGrid3})`)).toEqual([
        ['Albert', 'father'],
        ['Nina', 'mother'],
        ['Kian', 'son'],
        [1, 2],
        [3, 4],
      ])
      expect(() => lits.run(`grid:concat-rows(${exampleGrid1}, ${exampleGrid2})`)).toThrow(LitsError)
    })
  })
  describe('grid:concat-cols', () => {
    it('should concatenate the columns of the grid', () => {
      expect(lits.run(`grid:concat-cols(${exampleGrid1}, ${exampleGrid2})`)).toEqual([
        ['Albert', 'father', 10, 'Albert', 'father'],
        ['Nina', 'mother', 20, 'Nina', 'mother'],
        ['Kian', 'son', 30, 'Kian', 'son'],
      ])
      expect(() => lits.run(`grid:concat-cols(${exampleGrid2}, ${exampleGrid3})`)).toThrow(LitsError)
    })
  })
  describe('grid:map', () => {
    it('should map the grid', () => {
      expect(lits.run(`grid:map(${exampleGrid1}, str)`)).toEqual([
        ['Albert', 'father', '10'],
        ['Nina', 'mother', '20'],
        ['Kian', 'son', '30'],
      ])
    })
  })
  describe('grid:mapi', () => {
    it('should map the grid with index', () => {
      expect(lits.run(`grid:mapi(${exampleGrid1}, -> $1 ++ "(" ++ $2 ++ ", " ++ $3 ++ ")")`)).toEqual([
        ['Albert(0, 0)', 'father(0, 1)', '10(0, 2)'],
        ['Nina(1, 0)', 'mother(1, 1)', '20(1, 2)'],
        ['Kian(2, 0)', 'son(2, 1)', '30(2, 2)'],
      ])
    })
  })
  describe('grid:reduce', () => {
    it('should reduce the grid', () => {
      expect(lits.run(`grid:reduce(${exampleGrid1}, ++, "")`)).toEqual('Albertfather10Ninamother20Kianson30')
    })
  })
  describe('grid:reducei', () => {
    it('should reduce the grid with index', () => {
      expect(lits.run(`grid:reducei(${exampleGrid1}, -> $ + $3, 0)`)).toBe(9)
    })
  })
  describe('grid:push-rows', () => {
    it('should push rows to the grid', () => {
      expect(lits.run(`grid:push-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`)).toEqual([
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
        ['Nazanin', 'mother', 40],
      ])
      expect(() => lits.run(`grid:push-rows(${exampleGrid1}, ["Nazanin", 40])`)).toThrowError(LitsError)
    })
  })
  describe('grid:push-cols', () => {
    it('should push columns to the grid', () => {
      expect(lits.run(`grid:push-cols(${exampleGrid1}, ["f", "m", "s"])`)).toEqual([
        ['Albert', 'father', 10, 'f'],
        ['Nina', 'mother', 20, 'm'],
        ['Kian', 'son', 30, 's'],
      ])
    })
    it('should throw an error if the number of rows does not match', () => {
      expect(() => lits.run(`grid:push-cols(${exampleGrid1}, ["f", "m"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:unshift-rows', () => {
    it('should unshift rows to the grid', () => {
      expect(lits.run(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", "mother", 40])`)).toEqual([
        ['Nazanin', 'mother', 40],
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
      expect(() => lits.run(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", 40])`)).toThrowError(LitsError)
    })
    it('should throw an error if the number of columns does not match', () => {
      expect(() => lits.run(`grid:unshift-rows(${exampleGrid1}, ["Nazanin", "mother"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:unshift-cols', () => {
    it('should unshift columns to the grid', () => {
      expect(lits.run(`grid:unshift-cols(${exampleGrid1}, ["f", "m", "s"])`)).toEqual([
        ['f', 'Albert', 'father', 10],
        ['m', 'Nina', 'mother', 20],
        ['s', 'Kian', 'son', 30],
      ])
    })
    it('should throw an error if the number of rows does not match', () => {
      expect(() => lits.run(`grid:unshift-cols(${exampleGrid1}, ["f", "m"])`)).toThrowError(LitsError)
    })
  })
  describe('grid:pop-row', () => {
    it('should pop rows from the grid', () => {
      expect(lits.run(`grid:pop-row(${exampleGrid1})`)).toEqual([
        ['Albert', 'father', 10],
        ['Nina', 'mother', 20],
      ])
    })
  })
  describe('grid:pop-col', () => {
    it('should pop columns from the grid', () => {
      expect(lits.run(`grid:pop-col(${exampleGrid1})`)).toEqual([
        ['Albert', 'father'],
        ['Nina', 'mother'],
        ['Kian', 'son'],
      ])
    })
  })
  describe('grid:shift-row', () => {
    it('should shift rows from the grid', () => {
      expect(lits.run(`grid:shift-row(${exampleGrid1})`)).toEqual([
        ['Nina', 'mother', 20],
        ['Kian', 'son', 30],
      ])
    })
  })
  describe('grid:shift-col', () => {
    it('should shift columns from the grid', () => {
      expect(lits.run(`grid:shift-col(${exampleGrid1})`)).toEqual([
        ['father', 10],
        ['mother', 20],
        ['son', 30],
      ])
    })
  })
  describe('grid:from-array', () => {
    it('should convert an array to a grid', () => {
      expect(lits.run('grid:from-array([1, 2, 3, 4], 2)')).toEqual([
        [1, 2],
        [3, 4],
      ])
      expect(() => lits.run('grid:from-array([1, 2, 3], 2)')).toThrowError(LitsError)
    })
  })
})
