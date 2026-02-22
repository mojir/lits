/**
 * MaybePromise utilities for transparent async support.
 *
 * The sync path stays zero-overhead — when no async JS functions are involved,
 * everything runs synchronously with only `instanceof Promise` checks as overhead.
 * When an async value is detected, the evaluation chain switches to Promise-based
 * execution for the remainder.
 */

export type MaybePromise<T> = T | Promise<T>

/**
 * Chain a value that might be a Promise. If the value is sync, calls fn synchronously.
 * If it's a Promise, chains with .then().
 */
export function chain<T, U>(value: MaybePromise<T>, fn: (v: T) => MaybePromise<U>): MaybePromise<U> {
  if (value instanceof Promise) {
    return value.then(fn)
  }
  return fn(value)
}

/**
 * Resolve an array of MaybePromise values. If all are sync, returns the array as-is.
 * If any is a Promise, returns Promise.all().
 */
export function all<T>(values: MaybePromise<T>[]): MaybePromise<T[]> {
  if (values.some(v => v instanceof Promise)) {
    return Promise.all(values)
  }
  return values as T[]
}

/**
 * Like Array.map but handles MaybePromise callbacks sequentially.
 * In the sync case, runs as a simple loop. Switches to async only when needed.
 */
export function mapSequential<T, U>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<U>,
): MaybePromise<U[]> {
  const results: U[] = []
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingMap(result, results, arr, fn, i)
    }
    results.push(result)
  }
  return results
}

async function chainRemainingMap<T, U>(
  currentPromise: Promise<U>,
  results: U[],
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<U>,
  startIndex: number,
): Promise<U[]> {
  results.push(await currentPromise)
  for (let i = startIndex + 1; i < arr.length; i++) {
    results.push(await fn(arr[i]!, i))
  }
  return results
}

/**
 * Like Array.reduce but handles MaybePromise callbacks sequentially.
 * In the sync case, runs as a simple loop. Switches to async only when needed.
 */
export function reduceSequential<T, U>(
  arr: readonly T[],
  fn: (acc: U, elem: T, index: number) => MaybePromise<U>,
  initial: U,
): MaybePromise<U> {
  let result: U = initial
  for (let i = 0; i < arr.length; i++) {
    const next = fn(result, arr[i]!, i)
    if (next instanceof Promise) {
      return chainRemainingReduce(next, arr, fn, i)
    }
    result = next
  }
  return result
}

async function chainRemainingReduce<T, U>(
  currentPromise: Promise<U>,
  arr: readonly T[],
  fn: (acc: U, elem: T, index: number) => MaybePromise<U>,
  startIndex: number,
): Promise<U> {
  let result = await currentPromise
  for (let i = startIndex + 1; i < arr.length; i++) {
    result = await fn(result, arr[i]!, i)
  }
  return result
}

/**
 * Like Array.forEach but handles MaybePromise callbacks sequentially.
 * In the sync case, runs as a simple loop. Switches to async only when needed.
 */
export function forEachSequential<T>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<void>,
): MaybePromise<void> {
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingForEach(result, arr, fn, i)
    }
  }
}

async function chainRemainingForEach<T>(
  currentPromise: Promise<void>,
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<void>,
  startIndex: number,
): Promise<void> {
  await currentPromise
  for (let i = startIndex + 1; i < arr.length; i++) {
    await fn(arr[i]!, i)
  }
}

/**
 * Try/catch that handles MaybePromise values correctly.
 * If tryFn returns a Promise, catches both sync throws and Promise rejections.
 */
export function tryCatch<T>(
  tryFn: () => MaybePromise<T>,
  catchFn: (error: unknown) => MaybePromise<T>,
): MaybePromise<T> {
  try {
    const result = tryFn()
    if (result instanceof Promise) {
      return result.catch(catchFn)
    }
    return result
  }
  catch (error) {
    return catchFn(error)
  }
}

/**
 * Like Array.some but handles MaybePromise callbacks sequentially.
 * Returns the first truthy MaybePromise result, or false.
 */
export function someSequential<T>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
): MaybePromise<boolean> {
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingSome(result, arr, fn, i)
    }
    if (result)
      return true
  }
  return false
}

async function chainRemainingSome<T>(
  currentPromise: Promise<unknown>,
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
  startIndex: number,
): Promise<boolean> {
  if (await currentPromise)
    return true
  for (let i = startIndex + 1; i < arr.length; i++) {
    if (await fn(arr[i]!, i))
      return true
  }
  return false
}

/**
 * Like Array.every but handles MaybePromise callbacks sequentially.
 * Returns false as soon as a falsy result is found.
 */
export function everySequential<T>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
): MaybePromise<boolean> {
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingEvery(result, arr, fn, i)
    }
    if (!result)
      return false
  }
  return true
}

async function chainRemainingEvery<T>(
  currentPromise: Promise<unknown>,
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
  startIndex: number,
): Promise<boolean> {
  if (!await currentPromise)
    return false
  for (let i = startIndex + 1; i < arr.length; i++) {
    if (!await fn(arr[i]!, i))
      return false
  }
  return true
}

/**
 * Like Array.filter but handles MaybePromise callbacks sequentially.
 * Returns the filtered array.
 */
export function filterSequential<T>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
): MaybePromise<T[]> {
  const results: T[] = []
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingFilter(result, results, arr, fn, i)
    }
    if (result)
      results.push(arr[i]!)
  }
  return results
}

async function chainRemainingFilter<T>(
  currentPromise: Promise<unknown>,
  results: T[],
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
  startIndex: number,
): Promise<T[]> {
  if (await currentPromise)
    results.push(arr[startIndex]!)
  for (let i = startIndex + 1; i < arr.length; i++) {
    if (await fn(arr[i]!, i))
      results.push(arr[i]!)
  }
  return results
}

/**
 * Like Array.findIndex but handles MaybePromise callbacks sequentially.
 * Returns -1 if no element matches.
 */
export function findIndexSequential<T>(
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
): MaybePromise<number> {
  for (let i = 0; i < arr.length; i++) {
    const result = fn(arr[i]!, i)
    if (result instanceof Promise) {
      return chainRemainingFindIndex(result, arr, fn, i)
    }
    if (result)
      return i
  }
  return -1
}

async function chainRemainingFindIndex<T>(
  currentPromise: Promise<unknown>,
  arr: readonly T[],
  fn: (elem: T, index: number) => MaybePromise<unknown>,
  startIndex: number,
): Promise<number> {
  if (await currentPromise)
    return startIndex
  for (let i = startIndex + 1; i < arr.length; i++) {
    if (await fn(arr[i]!, i))
      return i
  }
  return -1
}
