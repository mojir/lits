import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('lucas', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:lucas-seq(1)')).toEqual([2])
    expect(lits.run('nth:lucas-seq(2)')).toEqual([2, 1])
    expect(lits.run('nth:lucas-seq(3)')).toEqual([2, 1, 3])
    expect(lits.run('nth:lucas-seq(4)')).toEqual([2, 1, 3, 4])
    expect(lits.run('nth:lucas-seq(77)')).toEqual([
      2,
      1,
      3,
      4,
      7,
      11,
      18,
      29,
      47,
      76,
      123,
      199,
      322,
      521,
      843,
      1364,
      2207,
      3571,
      5778,
      9349,
      15127,
      24476,
      39603,
      64079,
      103682,
      167761,
      271443,
      439204,
      710647,
      1149851,
      1860498,
      3010349,
      4870847,
      7881196,
      12752043,
      20633239,
      33385282,
      54018521,
      87403803,
      141422324,
      228826127,
      370248451,
      599074578,
      969323029,
      1568397607,
      2537720636,
      4106118243,
      6643838879,
      10749957122,
      17393796001,
      28143753123,
      45537549124,
      73681302247,
      119218851371,
      192900153618,
      312119004989,
      505019158607,
      817138163596,
      1322157322203,
      2139295485799,
      3461452808002,
      5600748293801,
      9062201101803,
      14662949395604,
      23725150497407,
      38388099893011,
      62113250390418,
      100501350283429,
      162614600673847,
      263115950957276,
      425730551631123,
      688846502588399,
      1114577054219522,
      1803423556807921,
      2918000611027443,
      4721424167835364,
      7639424778862807,
    ])
    expect(() => lits.run('nth:lucas-seq(0)')).toThrow(LitsError)
    expect(() => lits.run('nth:lucas-seq(78)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:lucas-nth(1)')).toEqual(2)
    expect(lits.run('nth:lucas-nth(2)')).toEqual(1)
    expect(lits.run('nth:lucas-nth(3)')).toEqual(3)
    expect(lits.run('nth:lucas-nth(4)')).toEqual(4)
    expect(lits.run('nth:lucas-nth(5)')).toEqual(7)
    expect(lits.run('nth:lucas-nth(6)')).toEqual(11)
    expect(lits.run('nth:lucas-nth(7)')).toEqual(18)
    expect(lits.run('nth:lucas-nth(8)')).toEqual(29)
    expect(lits.run('nth:lucas-nth(77)')).toEqual(7639424778862807)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:lucas-take-while(-> $ < 50)')).toEqual([2, 1, 3, 4, 7, 11, 18, 29, 47])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:lucas?(0)')).toEqual(false)
    expect(lits.run('nth:lucas?(1)')).toEqual(true)
    expect(lits.run('nth:lucas?(2)')).toEqual(true)
    expect(lits.run('nth:lucas?(3)')).toEqual(true)
    expect(lits.run('nth:lucas?(4)')).toEqual(true)
    expect(lits.run('nth:lucas?(5)')).toEqual(false)
    expect(lits.run('nth:lucas?(6)')).toEqual(false)
    expect(lits.run('nth:lucas?(7)')).toEqual(true)
    expect(lits.run('nth:lucas?(8)')).toEqual(false)
    expect(lits.run('nth:lucas?(9349)')).toEqual(true)
    expect(lits.run('nth:lucas?(9350)')).toEqual(false)
  })
})
