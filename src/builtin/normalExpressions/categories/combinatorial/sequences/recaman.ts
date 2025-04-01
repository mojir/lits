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

/**
 * Checks if a number is in the Recamán sequence.
 *
 * The Recamán sequence is defined as:
 * a(0) = 0
 * For n > 0, a(n) = a(n-1) - n if a(n-1) - n > 0 and not already in the sequence
 * Otherwise, a(n) = a(n-1) + n
 *
 * @param target - The number to check
 * @param limit - Maximum number of terms to generate (default: 10000)
 * @returns Whether the target number is in the Recamán sequence
 */
export function isInRecamanSequence(target: number, limit: number = 10000): boolean {
  // Input validation
  if (!Number.isInteger(target))
    return false
  if (target < 0)
    return false

  // Special case: 0 is always the first element
  if (target === 0)
    return true

  // Generate the sequence until we find the target or reach the limit
  const seen = new Set<number>([0])
  let current = 0

  for (let n = 1; n <= limit; n++) {
    // Try to go backward first (current - n)
    let next = current - n

    // If that's not positive or already seen, go forward (current + n)
    if (next <= 0 || seen.has(next)) {
      next = current + n
    }

    // Check if we found our target
    if (next === target) {
      return true
    }

    seen.add(next)
    current = next

    // Optimization: If we've gone well past the target, it's unlikely to appear
    // This works because the sequence grows roughly at rate n
    if (current > target * 3 && n > target * 3) {
      return false
    }
  }

  // If we've reached the limit without finding the target, return false
  return false
}

export const recamanSequence: SequenceDefinition<'recaman'> = {
  'c:recaman-seq': length => generateRecamanSequence(length),
  'c:recaman-take-while': (takeWhile) => {
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
  'c:recaman-nth': n => generateRecamanSequence(n)[n - 1] ?? 0,
  'c:recaman?': () => true,
}
