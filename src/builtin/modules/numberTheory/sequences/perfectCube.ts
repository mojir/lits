import type { SequenceDefinition } from '.'

export const perfectCubeSequence: SequenceDefinition<'perfect-cube'> = {
  'perfect-cube-seq': (length) => {
    const perfectcubes = []
    for (let i = 1; i <= length; i++) {
      perfectcubes.push(i ** 3)
    }
    return perfectcubes
  },
  'perfect-cube?': n => n > 0 && Number.isInteger(Math.cbrt(n)),
  'perfect-cube-take-while': (takeWhile) => {
    const perfectcubes = []
    for (let i = 1; ; i++) {
      const value = i ** 3
      if (!takeWhile(value, i)) {
        break
      }
      perfectcubes.push(value)
    }
    return perfectcubes
  },
}
