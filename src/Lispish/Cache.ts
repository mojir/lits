import { asNotUndefined, toNonNegativeInteger } from '../utils'

type CacheEntry<T> = {
  key: string
  value: T
  nextEntry: CacheEntry<T> | undefined
}

export class Cache<T> {
  private cache: Record<string, CacheEntry<T>> = {}
  private firstEntry: CacheEntry<T> | undefined = undefined
  private lastEntry: CacheEntry<T> | undefined = undefined
  private _size = 0
  private maxSize: number
  constructor(maxSize: number) {
    this.maxSize = toNonNegativeInteger(maxSize)
    if (this.maxSize < 1) {
      throw Error(`1 is the minimum maxSize, got ${maxSize}`)
    }
  }

  public get size(): number {
    return this._size
  }

  public get(key: string): T | undefined {
    return this.cache[key]?.value
  }

  public clear(): void {
    this.cache = {}
    this.firstEntry = undefined
    this.lastEntry = undefined
    this._size = 0
  }

  public has(key: string): boolean {
    return !!this.cache[key]
  }

  public set(key: string, value: T): void {
    if (this.has(key)) {
      throw Error(`AstCache - key already present: ${key}`)
    }
    const newEntry: CacheEntry<T> = { value, nextEntry: undefined, key }

    this.cache[key] = newEntry
    this._size += 1

    if (this.lastEntry) {
      this.lastEntry.nextEntry = newEntry
    }
    this.lastEntry = newEntry

    if (!this.firstEntry) {
      this.firstEntry = this.lastEntry
    }

    while (this.size > this.maxSize) {
      this.dropFirstEntry()
    }
  }

  private dropFirstEntry(): void {
    const firstEntry = asNotUndefined(this.firstEntry)
    delete this.cache[firstEntry.key]
    this._size -= 1
    this.firstEntry = firstEntry.nextEntry
  }
}
