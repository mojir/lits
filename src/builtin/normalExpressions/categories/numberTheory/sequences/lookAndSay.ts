import type { SequenceDefinition } from '.'

/**
 * Checks if a number is part of the Look-and-Say sequence.
 *
 * The Look-and-Say sequence starts with "1" and each subsequent term describes
 * the previous term by counting consecutive digits. For example:
 * 1, 11, 21, 1211, 111221, 312211, 13112221, ...
 *
 * @param {string|number} target - The number to check (can be a string or number)
 * @returns {boolean} - Whether the target is in the sequence
 */
function isLookAndSay(target: string): boolean {
  // The first term of the sequence
  let current = '1'

  // Check if the first term matches
  if (current === target) {
    return true
  }

  // Generate terms and check against the target
  while (true) {
    current = getNextLookAndSayTerm(current)

    if (current === target) {
      return true
    }

    // Optimization: if the current term is longer than the target, and
    // the sequence is strictly increasing in length, the target won't be found
    if (current.length > target.length) {
      return false
    }
  }
}

/**
 * Generates the next term in the Look-and-Say sequence
 *
 * @param {string} term - The current term
 * @returns {string} - The next term in the sequence
 */
function getNextLookAndSayTerm(term: string): string {
  let result = ''
  let count = 1

  for (let i = 0; i < term.length; i++) {
    // If the current digit is the same as the next one, increment count
    if (i + 1 < term.length && term[i] === term[i + 1]) {
      count++
    }
    else {
      // Otherwise, append count and the digit to the result
      result += count.toString() + term[i]
      count = 1
    }
  }

  return result
}

export const lookAndSaySequence: SequenceDefinition<'look-and-say', string> = {
  'string': true,
  'n:look-and-say-seq': (length) => {
    const lookAndSay = ['1']
    for (let i = 1; i < length; i += 1) {
      const prev = lookAndSay[i - 1]!
      const next = prev.replace(/(\d)\1*/g, match => `${match.length}${match[0]}`)
      lookAndSay[i] = next
    }
    return lookAndSay
  },
  'n:look-and-say-take-while': (takeWhile) => {
    if (!takeWhile('1', 0)) {
      return []
    }
    const lookAndSay = ['1']
    for (let i = 1; ; i += 1) {
      const prev = lookAndSay[i - 1]!
      const next = prev.replace(/(\d)\1*/g, match => `${match.length}${match[0]}`)
      if (!takeWhile(next, i)) {
        break
      }
      lookAndSay[i] = next
    }
    return lookAndSay
  },
  'n:look-and-say-nth': (n) => {
    let lookAndSay = '1'
    for (let i = 1; i < n; i += 1) {
      lookAndSay = lookAndSay.replace(/(\d)\1*/g, match => `${match.length}${match[0]}`)
    }
    return lookAndSay
  },
  'n:look-and-say?': n => isLookAndSay(n),
}
