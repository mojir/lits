import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('lookAndSay', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:look-and-say-seq(1)')).toEqual(['1'])
    expect(lits.run('c:look-and-say-seq(2)')).toEqual(['1', '11'])
    expect(lits.run('c:look-and-say-seq(3)')).toEqual(['1', '11', '21'])
    expect(lits.run('c:look-and-say-seq(4)')).toEqual(['1', '11', '21', '1211'])
    expect(lits.run('c:look-and-say-seq(5)')).toEqual(['1', '11', '21', '1211', '111221'])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:look-and-say-nth(1)')).toEqual('1')
    expect(lits.run('c:look-and-say-nth(2)')).toEqual('11')
    expect(lits.run('c:look-and-say-nth(3)')).toEqual('21')
    expect(lits.run('c:look-and-say-nth(4)')).toEqual('1211')
    expect(lits.run('c:look-and-say-nth(5)')).toEqual('111221')
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:look-and-say-take-while(-> $2 < 5)')).toEqual(['1', '11', '21', '1211', '111221'])
    expect(lits.run('c:look-and-say-take-while(-> false)')).toEqual([])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:look-and-say?("1")')).toEqual(true)
    expect(lits.run('c:look-and-say?("11")')).toEqual(true)
    expect(lits.run('c:look-and-say?("21")')).toEqual(true)
    expect(lits.run('c:look-and-say?("1211")')).toEqual(true)
    expect(lits.run('c:look-and-say?("111221")')).toEqual(true)
    expect(lits.run('c:look-and-say?("12345")')).toEqual(false)
  })
})
