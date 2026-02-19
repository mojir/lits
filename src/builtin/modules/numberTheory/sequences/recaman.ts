import type { SequenceDefinition } from '.'

/**
 * Generates the first 'n' terms of the Recamán sequence.
 *
 * @param n - Number of terms to generate
 * @returns Array containing the first n terms of the Recamán sequence
 */
export function generateRecamanSequence(n: number): number[] {
  if (n === 1)
    return [0]

  const sequence: number[] = [0]
  const seen = new Set<number>([0])

  for (let i = 1; i < n; i++) {
    // Try to go backward
    let next = sequence[i - 1]! - i

    // If that's not positive or already seen, go forward
    if (next <= 0 || seen.has(next)) {
      next = sequence[i - 1]! + i
    }

    sequence.push(next)
    seen.add(next)
  }

  return sequence
}

export const recamanSequence: SequenceDefinition<'recaman'> = {
  'recaman-seq': length => generateRecamanSequence(length),
  'recaman-take-while': (takeWhile) => {
    if (!takeWhile(0, 0))
      return []

    const sequence: number[] = [0]
    const seen = new Set<number>([0])

    for (let i = 1; ; i++) {
      // Try to go backward
      let next = sequence[i - 1]! - i

      // If that's not positive or already seen, go forward
      if (next <= 0 || seen.has(next)) {
        next = sequence[i - 1]! + i
      }

      if (!takeWhile(next, i))
        break

      sequence.push(next)
      seen.add(next)
    }

    return sequence
  },
  'recaman?': () => true,
}
