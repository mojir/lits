import { destructPath, parseBracketNotation, getPath } from '../src/builtin/getPath'

describe('getPath', () => {
  describe('parseBracketNotation', () => {
    test('numbers', () => {
      expect(parseBracketNotation('[123].x[2].y.z')).toEqual([5, 123])
      expect(parseBracketNotation('[123]')).toEqual([5, 123])
      expect(parseBracketNotation('[ 0 ].x[2].y.z')).toEqual([5, 0])
    })
    test('strings', () => {
      expect(parseBracketNotation(`['foo']`)).toEqual([7, 'foo'])
      expect(parseBracketNotation(`['foo'].bar`)).toEqual([7, 'foo'])
      expect(parseBracketNotation(`[ 'foo' ].bar`)).toEqual([9, 'foo'])
      expect(parseBracketNotation(`["foo"]`)).toEqual([7, 'foo'])
      expect(parseBracketNotation(`["foo"].bar`)).toEqual([7, 'foo'])
      expect(parseBracketNotation(`[ "foo" ].bar`)).toEqual([9, 'foo'])
    })
    test('invalids', () => {
      expect(() => parseBracketNotation(`['foo'`)).toThrow()
      expect(() => parseBracketNotation(`['foo"]`)).toThrow()
      expect(() => parseBracketNotation(`[foo]`)).toThrow()
      expect(() => parseBracketNotation(`[0.1]`)).toThrow()
    })
  })

  describe('destructPath', () => {
    test('samples', () => {
      expect(destructPath('foo.bar')).toEqual(['foo', 'bar'])
      expect(destructPath('.foo.bar')).toEqual(['foo', 'bar'])
      expect(destructPath('foo[2].bar')).toEqual(['foo', 2, 'bar'])
      expect(destructPath('foo[2]. bar')).toEqual(['foo', 2, ' bar'])
      expect(destructPath(`['x']["y"][2].x.y[1]`)).toEqual(['x', 'y', 2, 'x', 'y', 1])
      expect(() => destructPath('foo.bar.')).toThrow()
      expect(() => destructPath('foo[2]bar')).toThrow()
    })
  })
  describe('getPath', () => {
    test('samples', () => {
      expect(getPath({ a: 10 }, 'a')).toBe(10)
      expect(getPath({ a: [1, 2, 3] }, 'a[1]')).toBe(2)
      expect(getPath({ a: [1, 2, 3] }, 'a.1')).toBe(2)
      expect(getPath({ a: [1, 2, 3] }, 'a.["1"]')).toBe(2)
      expect(getPath([1, 2, 3], '.2')).toBe(3)
      expect(getPath([1, 2, 3], '[2]')).toBe(3)
      expect(getPath([1, 2, 3], 'a')).toBe(undefined)
    })
  })
})
