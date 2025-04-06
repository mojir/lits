import { isPrime } from './prime'
import type { SequenceDefinition } from '.'

export function isComposite(num: number): boolean {
  if (num <= 1) {
    return false
  }
  return !isPrime(num)
}

function nthComposite(n: number): number {
  if (n === 1)
    return 4 // First composite is 4
  if (n === 2)
    return 6 // Second composite is 6
  let count = 2 // We've already counted 4 and 6
  let candidate = 8 // Start checking from 8
  while (count < n) {
    if (isComposite(candidate)) {
      count++
      if (count === n) {
        break
      }
    }
    candidate += 1
  }
  return candidate
}

export const compositeSequence: SequenceDefinition<'composite'> = {
  'c:composite-seq': (length) => {
    const composites = []
    let num = 2
    while (composites.length < length) {
      if (isComposite(num)) {
        composites.push(num)
      }
      num += 1
    }
    return composites
  },
  'c:composite-nth': n => nthComposite(n),
  'c:composite?': n => isComposite(n),
  'c:composite-take-while': (takeWhile) => {
    const composites = []
    for (let i = 4; ; i += 1) {
      if (!isComposite(i)) {
        continue
      }
      if (!takeWhile(i, composites.length)) {
        break
      }
      composites.push(i)
    }
    return composites
  },
}
