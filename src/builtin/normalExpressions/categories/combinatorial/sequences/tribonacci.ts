import type { SequenceDefinition } from '.'

export const tribonacciSequence: SequenceDefinition<'tribonacci'> = {
  'c:tribonacci-seq': (length) => {
    const tribonacci = [0, 1, 1]
    for (let i = 3; i < length; i += 1) {
      tribonacci[i] = tribonacci[i - 1]! + tribonacci[i - 2]! + tribonacci[i - 3]!
    }
    return tribonacci.slice(0, length)
  },
  'c:tribonacci-nth': (n) => {
    if (n === 1) {
      return 0
    }
    if (n === 2) {
      return 1
    }
    if (n === 3) {
      return 1
    }

    let a = 0
    let b = 1
    let c = 1
    for (let i = 4; i <= n; i += 1) {
      const temp = a + b + c
      a = b
      b = c
      c = temp
    }
    return c
  },
  'c:tribonacci?': (n) => {
    if (n < 0) {
      return false
    }
    if (n === 0 || n === 1 || n === 2) {
      return true
    }

    // Initialize the first three numbers of the sequence
    let a = 0
    let b = 0
    let c = 1

    // Generate the sequence until we reach or exceed the input number
    while (c < n) {
      const next = a + b + c
      a = b
      b = c
      c = next
    }

    // If c equals the input number, it's in the sequence
    return c === n
  },

  'c:tribonacci-take-while': (takeWhile) => {
    const tribonacci: number[] = []
    if (!takeWhile(0, 0)) {
      return tribonacci
    }
    tribonacci.push(0)
    if (!takeWhile(1, 1)) {
      return tribonacci
    }
    tribonacci.push(1)
    if (!takeWhile(1, 2)) {
      return tribonacci
    }
    tribonacci.push(1)

    for (let i = 3; ; i += 1) {
      const value = tribonacci[i - 1]! + tribonacci[i - 2]! + tribonacci[i - 3]!
      if (!takeWhile(value, i)) {
        break
      }
      tribonacci.push(value)
    }
    return tribonacci
  },
}
