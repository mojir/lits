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

export const primeSequence: SequenceDefinition<'prime'> = {
  'prime-seq': (length) => {
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
  'prime?': n => isPrime(n),
  'prime-take-while': (takeWhile) => {
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
