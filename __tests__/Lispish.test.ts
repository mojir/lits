import { Lispish } from '../src'
import { Cache } from '../src/Lispish/Cache'

describe(`import`, () => {
  let lispish: Lispish
  beforeEach(() => {
    lispish = new Lispish()
  })
  test(`import a function.`, () => {
    lispish = new Lispish({ astCacheSize: 10 })
    lispish.import(`(defn tripple [x] (* x 3))`)
    expect(lispish.run(`(tripple 10)`)).toBe(30)
    expect(lispish.run(`(tripple 10)`)).toBe(30)
  })

  test(`import a function - no cache`, () => {
    lispish = new Lispish()
    lispish.import(`(defn tripple [x] (* x 3))`, {})
    expect(lispish.run(`(tripple 10)`)).toBe(30)
    expect(lispish.run(`(tripple 10)`)).toBe(30)
  })

  test(`import a variable.`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`import a variable - again.`, () => {
    lispish.import(`
    (defn zip? [string] (boolean (match (regexp "^\\d{5}$") string)))
    (defn isoDateString? [string] (boolean (match (regexp "^\\d{4}-\\d{2}-\\d{2}$") string)))
    (def NAME_LENGTH 100)
    `)
    expect(lispish.run(`NAME_LENGTH`)).toBe(100)
  })

  test(`change imported variable`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`magicNumber`)).toBe(42)
  })

  test(`import a function twice`, () => {
    lispish.import(`(defn tripple [x] (* x 3))`)
    expect(() => lispish.import(`(defn tripple [x] (* x 3))`)).toThrow()
  })

  test(`import a function with a built in normal expression name`, () => {
    expect(() => lispish.import(`(defn inc (x) (+ x 1))`)).toThrow()
  })

  test(`import a function with a built in special expression name`, () => {
    expect(() => lispish.import(`(defn and (x y) (* x y))`)).toThrow()
  })

  test(`import a variable twice`, () => {
    lispish.import(`(def magicNumber 42)`)
    expect(() => lispish.import(`(def magicNumber 42)`)).toThrow()
  })

  test(`import more than once`, () => {
    lispish.import(`(defn tripple [x] (* x 3))`)
    lispish.import(`(def magicNumber 42)`)
    expect(lispish.run(`(tripple magicNumber)`)).toBe(126)
  })
})

describe(`Cache`, () => {
  test(`cannot set same key twice`, () => {
    const cache = new Cache<number>(10)
    cache.set(`a`, 1)
    expect(() => cache.set(`a`, 1)).toThrow()
  })

  test(`max cache zize must be at least 1`, () => {
    expect(() => new Cache(-1)).toThrow()
    expect(() => new Cache(0)).toThrow()
    expect(() => new Cache(0.1)).not.toThrow()
    expect(() => new Cache(1)).not.toThrow()
  })

  test(`Add an entry.`, () => {
    const cache = new Cache<number>(10)
    expect(cache.size).toBe(0)
    cache.set(`a`, 1)
    expect(cache.size).toBe(1)
    expect(cache.get(`a`)).toBe(1)
    expect(cache.has(`a`)).toBe(true)
  })

  test(`Clear cache.`, () => {
    const cache = new Cache<number>(10)
    cache.set(`a`, 1)
    cache.set(`b`, 2)
    cache.set(`c`, 3)
    expect(cache.size).toBe(3)
    cache.clear()
    expect(cache.size).toBe(0)
  })

  test(`Add an entry - cacheSize = 1`, () => {
    const cache = new Cache<number>(1)
    expect(cache.size).toBe(0)
    cache.set(`a`, 1)
    expect(cache.size).toBe(1)
    expect(cache.get(`a`)).toBe(1)
  })
  test(`maxSize.`, () => {
    const cache = new Cache<number>(1)
    cache.set(`a`, 1)
    expect(cache.get(`a`)).toBe(1)
    cache.set(`b`, 2)
    expect(cache.size).toBe(1)
    expect(cache.get(`a`)).toBeUndefined()
    expect(cache.has(`a`)).toBe(false)
    expect(cache.get(`b`)).toBe(2)
    expect(cache.has(`b`)).toBe(true)
  })
})
