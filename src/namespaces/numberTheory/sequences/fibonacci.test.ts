import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("nth"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('fibonacci', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:fibonacci-seq(1)')).toEqual([0])
    expect(runNth('nth:fibonacci-seq(2)')).toEqual([0, 1])
    expect(runNth('nth:fibonacci-seq(3)')).toEqual([0, 1, 1])
    expect(runNth('nth:fibonacci-seq(4)')).toEqual([0, 1, 1, 2])
    expect(runNth('nth:fibonacci-seq(79)')).toEqual([
      0,
      1,
      1,
      2,
      3,
      5,
      8,
      13,
      21,
      34,
      55,
      89,
      144,
      233,
      377,
      610,
      987,
      1597,
      2584,
      4181,
      6765,
      10946,
      17711,
      28657,
      46368,
      75025,
      121393,
      196418,
      317811,
      514229,
      832040,
      1346269,
      2178309,
      3524578,
      5702887,
      9227465,
      14930352,
      24157817,
      39088169,
      63245986,
      102334155,
      165580141,
      267914296,
      433494437,
      701408733,
      1134903170,
      1836311903,
      2971215073,
      4807526976,
      7778742049,
      12586269025,
      20365011074,
      32951280099,
      53316291173,
      86267571272,
      139583862445,
      225851433717,
      365435296162,
      591286729879,
      956722026041,
      1548008755920,
      2504730781961,
      4052739537881,
      6557470319842,
      10610209857723,
      17167680177565,
      27777890035288,
      44945570212853,
      72723460248141,
      117669030460994,
      190392490709135,
      308061521170129,
      498454011879264,
      806515533049393,
      1304969544928657,
      2111485077978050,
      3416454622906707,
      5527939700884757,
      8944394323791464,
    ])
    expect(() => runNth('nth:fibonacci-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:fibonacci-seq(80)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:fibonacci-nth(1)')).toEqual(0)
    expect(runNth('nth:fibonacci-nth(2)')).toEqual(1)
    expect(runNth('nth:fibonacci-nth(3)')).toEqual(1)
    expect(runNth('nth:fibonacci-nth(4)')).toEqual(2)
    expect(runNth('nth:fibonacci-nth(11)')).toEqual(55)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:fibonacci-take-while(-> $ < 100)')).toEqual([0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:fibonacci?(0)')).toEqual(true)
    expect(runNth('nth:fibonacci?(1)')).toEqual(true)
    expect(runNth('nth:fibonacci?(2)')).toEqual(true)
    expect(runNth('nth:fibonacci?(3)')).toEqual(true)
    expect(runNth('nth:fibonacci?(4)')).toEqual(false)
    expect(runNth('nth:fibonacci?(5)')).toEqual(true)
    expect(runNth('nth:fibonacci?(6)')).toEqual(false)
    expect(runNth('nth:fibonacci?(7)')).toEqual(false)
    expect(runNth('nth:fibonacci?(8)')).toEqual(true)
    expect(runNth('nth:fibonacci?(9)')).toEqual(false)
  })
})
