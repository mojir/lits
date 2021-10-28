import { Lispish } from '../src'
import { Cache } from '../src/Lispish/Cache'

describe(`context`, () => {
  let lispish: Lispish
  beforeEach(() => {
    lispish = new Lispish()
  })
  test(`a function.`, () => {
    lispish = new Lispish({ astCacheSize: 10 })
    const contexts = [lispish.context(`(defn tripple [x] (* x 3))`)]
    expect(lispish.run(`(tripple 10)`, { contexts })).toBe(30)
    expect(lispish.run(`(tripple 10)`, { contexts })).toBe(30)
  })

  test(`a function - no cache`, () => {
    lispish = new Lispish()
    const contexts = [lispish.context(`(defn tripple [x] (* x 3))`, {})]
    expect(lispish.run(`(tripple 10)`, { contexts })).toBe(30)
    expect(lispish.run(`(tripple 10)`, { contexts })).toBe(30)
  })

  test(`a variable.`, () => {
    const contexts = [lispish.context(`(def magicNumber 42)`)]
    expect(lispish.run(`magicNumber`, { contexts })).toBe(42)
  })

  test(`a variable - again.`, () => {
    const contexts = [
      lispish.context(`
    (defn zip? [string] (boolean (match (regexp "^\\d{5}$") string)))
    (defn isoDateString? [string] (boolean (match (regexp "^\\d{4}-\\d{2}-\\d{2}$") string)))
    (def NAME_LENGTH 100)
    `),
    ]
    expect(lispish.run(`NAME_LENGTH`, { contexts })).toBe(100)
  })

  test(`change imported variable`, () => {
    const contexts = [lispish.context(`(def magicNumber 42)`)]
    expect(lispish.run(`magicNumber`, { contexts })).toBe(42)
  })

  test(`a function with a built in normal expression name`, () => {
    expect(() => lispish.context(`(defn inc (x) (+ x 1))`)).toThrow()
    expect(() => lispish.context(`(defn inc (x) (+ x 1))`, {})).toThrow()
    expect(() => lispish.context(`(defn inc (x) (+ x 1))`, { values: {} })).toThrow()
  })

  test(`a function with a built in special expression name`, () => {
    expect(() => lispish.context(`(defn and (x y) (* x y))`)).toThrow()
  })

  test(`a variable twice`, () => {
    const contexts = [lispish.context(`(def magicNumber 42) (defn getMagic [] 42)`)]
    expect(() => lispish.context(`(def magicNumber 42) (defn getMagic [] 42)`, { contexts })).not.toThrow()
  })

  test(`more than one`, () => {
    const contexts = [lispish.context(`(defn tripple [x] (* x 3))`), lispish.context(`(def magicNumber 42)`)]
    expect(lispish.run(`(tripple magicNumber)`, { contexts })).toBe(126)
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
