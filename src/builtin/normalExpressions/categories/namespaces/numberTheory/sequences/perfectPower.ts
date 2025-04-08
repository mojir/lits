import type { SequenceDefinition } from '.'

/**
 * Checks if a number is a perfect power and returns the base and exponent if it is.
 * A perfect power is a number that can be expressed as an integer power of another integer.
 *
 * @param n - The number to check
 * @returns [base, exponent] if n is a perfect power, null otherwise
 */
export function perfectPower(n: number): [number, number] | null {
  // Handle edge cases
  if (n < 2) {
    if (n === 1) {
      // 1 is 1^k for any k, we return [1, 2] as the simplest representation
      return [1, 2]
    }
    return null // Non positive numbers are not perfect powers
  }

  // For each possible exponent k, try to find base b such that b^k = n
  const maxK = Math.floor(Math.log2(n)) + 1

  for (let k = 2; k <= maxK; k++) {
    // Calculate the potential base as n^(1/k)
    const b = n ** (1 / k)
    const roundedB = Math.round(b)

    // Check if roundedB^k is equal to n (within a small epsilon to account for floating point errors)
    const epsilon = 1e-10
    if (Math.abs(roundedB ** k - n) < epsilon) {
      return [roundedB, k]
    }
  }

  return null // Not a perfect power
}

export const perfectPowerSequence: SequenceDefinition<'perfect-power'> = {
  'nth:perfect-power-seq': (length) => {
    const perfectPowers: number[] = []
    for (let i = 1; perfectPowers.length < length; i++) {
      if (perfectPower(i)) {
        perfectPowers.push(i)
      }
    }
    return perfectPowers
  },
  'nth:perfect-power?': n => perfectPower(n) !== null,
  'nth:perfect-power-take-while': (takeWhile) => {
    const perfectPowers: number[] = []
    for (let i = 1; ; i++) {
      if (perfectPower(i)) {
        if (!takeWhile(i, perfectPowers.length)) {
          break
        }
        perfectPowers.push(i)
      }
    }
    return perfectPowers
  },
}
