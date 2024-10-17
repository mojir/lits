export function isNotNull<T>(value: T | null | undefined): value is T {
  return value !== null && value !== undefined
}

export function assertNotNull<T>(value: T | null | undefined): asserts value is T {
  if (!isNotNull(value))
    throw new Error('Value is null or undefined')
}

export function asNotNull<T>(value: T | null | undefined): T {
  assertNotNull(value)
  return value
}
