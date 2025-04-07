import type { SequenceDefinition } from '.'

export function isPrime(num: number): boolean {
  if (num <= 1) {
    return false
  }
  if (num <= 3) {
    return true
  }
  if (num % 2 === 0 || num % 3 === 0) {
    return false
  }

  for (let i = 5; i * i <= num; i += 6) {
    if (num % i === 0 || num % (i + 2) === 0) {
      return false
    }
  }
  return true
}

function nthPrime(n: number): number {
  if (n === 1)
    return 2 // First prime is 2
  if (n === 2)
    return 3 // Second prime is 3

  let count = 2 // We've already counted 2 and 3
  let candidate = 5 // Start checking from 5

  while (count < n) {
    if (isPrime(candidate)) {
      count++
      if (count === n) {
        break
      }
    }

    // We can skip even numbers and use the 6k±1 optimization
    candidate += 2
  }
  return candidate
}

export const primeSequence: SequenceDefinition<'prime'> = {
  'n:prime-seq': (length) => {
    const primes = []
    let num = 2
    while (primes.length < length) {
      if (isPrime(num)) {
        primes.push(num)
      }
      num += 1
    }
    return primes
  },
  'n:prime-nth': n => nthPrime(n),
  'n:prime?': n => isPrime(n),
  'n:prime-take-while': (takeWhile) => {
    const primes = []
    for (let i = 2; ; i += 1) {
      if (!isPrime(i)) {
        continue
      }
      if (!takeWhile(i, primes.length)) {
        break
      }
      primes.push(i)
    }
    return primes
  },
}
