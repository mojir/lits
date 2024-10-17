import { expect } from 'vitest'

export function testFormatter(formatter: (program: string) => string, program: string, expected: string) {
  const formattedProgram = formatter(program)
  expect(formattedProgram, 'Format program error').toEqual(expected)
  expect(formatter(formattedProgram), 'Format twice mismatch').toEqual(formattedProgram)
}
