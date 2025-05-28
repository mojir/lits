/* eslint-disable no-console */
import { afterEach, beforeEach, describe, expect, it, vitest } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'

describe('misc functions', () => {
  let oldLog: () => void
  let oldWarn: () => void

  let logSpy: (...args: unknown[]) => void
  beforeEach(() => {
    oldLog = console.log
    oldWarn = console.warn
    logSpy = vitest.fn()
    console.log = (...args) => {
      logSpy(...args)
    }
    console.warn = (..._args) => {
    }
  })
  afterEach(() => {
    console.log = oldLog
    console.warn = oldWarn
  })
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('epoch->iso-date', () => {
      it('samples', () => {
        expect(lits.run('epoch->iso-date(1649756230899)')).toBe('2022-04-12T09:37:10.899Z')
        expect(lits.run('epoch->iso-date(-1649756230899)')).toBe('1917-09-21T14:22:49.101Z')
        expect(lits.run('epoch->iso-date(0)')).toBe('1970-01-01T00:00:00.000Z')
        expect(lits.run('epoch->iso-date(0.999)')).toBe('1970-01-01T00:00:00.000Z')
        expect(lits.run('epoch->iso-date(0.999)')).toBe('1970-01-01T00:00:00.000Z')
        expect(() => lits.run('epoch->iso-date(1649756230899 1649756230899)')).toThrow(LitsError)
        expect(() => lits.run('epoch->iso-date()')).toThrow(LitsError)
        expect(() => lits.run('epoch->iso-date("1649756230899")')).toThrow(LitsError)
        expect(() => lits.run('epoch->iso-date(null)')).toThrow(LitsError)
        expect(() => lits.run('epoch->iso-date(true)')).toThrow(LitsError)
      })
    })

    describe('iso-date->epoch', () => {
      it('samples', () => {
        expect(lits.run('iso-date->epoch("2022-04-12T09:37:10.899Z")')).toBe(1649756230899)
        expect(lits.run('iso-date->epoch("2022-04-12")')).toBeGreaterThan(1649548800000)
        expect(() =>
          lits.run('iso-date->epoch("2022-04-12T09:37:10.899Z", "2022-04-12T09:37:10.899Z")'),
        ).toThrow()
        expect(() => lits.run('iso-date->epoch()')).toThrow(LitsError)
        expect(() => lits.run('iso-date->epoch(1649756230899)')).toThrow(LitsError)
        expect(() => lits.run('iso-date->epoch(null)')).toThrow(LitsError)
        expect(() => lits.run('iso-date->epoch(true)')).toThrow(LitsError)
        expect(() => lits.run('iso-date->epoch("2022-04-1X")')).toThrow(LitsError)
        expect(() => lits.run('iso-date->epoch("")')).toThrow(LitsError)
      })
    })

    describe('!=', () => {
      it('samples', () => {
        expect(lits.run('1 != 1')).toBe(!lits.run('1 == 1'))
        expect(lits.run('1 != 2')).toBe(!lits.run('1 == 2'))

        expect(lits.run('!=(1)')).toBe(!lits.run('==(1)'))
        expect(lits.run('!=(1, 1)')).toBe(!lits.run('==(1, 1)'))
        expect(lits.run('!=(1, 2)')).toBe(!lits.run('==(1, 2)'))
        expect(lits.run('!=(1, 2, 1)')).toBe(!lits.run('==(1, 2, 1)'))
        expect(lits.run('!=(1, 2, 3)')).toBe(!lits.run('==(1, 2, 3)'))
        expect(lits.run('!=("1")')).toBe(!lits.run('==("1")'))
        expect(lits.run('!=("1", "1")')).toBe(!lits.run('==("1", "1")'))
        expect(lits.run('!=("1", "2")')).toBe(!lits.run('==("1", "2")'))
        expect(lits.run('!=("1", "2", "1")')).toBe(!lits.run('==("1", "2", "1")'))
        expect(lits.run('!=("1", "2", 3)')).toBe(!lits.run('==("1", "2", 3)'))
        expect(lits.run('!=(null, 0)')).toBe(!lits.run('==(null, 0)'))
        expect(lits.run('!=(1, true, 3)')).toBe(!lits.run('==(1, true, 3)'))
        expect(lits.run('!=(1, false, 3)')).toBe(!lits.run('==(1, false, 3)'))
        expect(() => lits.run('!=()')).toThrow(LitsError)
        expect(lits.run('!=([1, 2, { a: 10, b: [null]}], [1, 2, { b: [null], a: 10}])')).toBe(!lits.run('==([1, 2, { a: 10, b: [null]}], [1, 2, { b: [null], a: 10}])'))
        expect(lits.run('!=([1, 2, { a: 10, b: [null]}], [1, 2, { b: [0], a: 10}])')).toBe(!lits.run('==([1, 2, { a: 10, b: [null]}], [1, 2, { b: [0], a: 10}])'))
        expect(lits.run('!=({ a: 10, b: 20}, { b: 20, a: 10})')).toBe(!lits.run('==({ a: 10, b: 20}, { b: 20, a: 10})'))
        expect(lits.run('!=([1, true, null], [1, true, null])')).toBe(!lits.run('==([1, true, null], [1, true, null])'))
        expect(lits.run('!=({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 20}], a: 10})')).toBe(!lits.run('==({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 20}], a: 10})'))
        expect(lits.run('!=({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 21}], a: 10})')).toBe(!lits.run('==({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 21}], a: 10})'))
        expect(lits.run('!=([1, 2, 3], [1, 2, 3, 4])')).toBe(!lits.run('==([1, 2, 3], [1, 2, 3, 4])'))
        expect(lits.run('!=({ a: 10}, { a: 10, b: 20})')).toBe(!lits.run('==({ a: 10}, { a: 10, b: 20})'))
      })
    })

    describe('identical?', () => {
      it('samples', () => {
        expect(lits.run('1 identical? 1')).toBe(true)
        expect(lits.run('1 identical? 2')).toBe(false)

        expect(lits.run('identical?(1, 1)')).toBe(true)
        expect(lits.run('identical?(1, 2)')).toBe(false)
        expect(lits.run('identical?("1", "1")')).toBe(true)
        expect(lits.run('identical?("1", "2")')).toBe(false)
        expect(lits.run('identical?(null, 0)')).toBe(false)
        expect(lits.run('identical?([1], [1])')).toBe(false)
        expect(lits.run('identical?({}, {})')).toBe(false)
        expect(() => lits.run('identical?()')).toThrow(LitsError)
      })
    })

    describe('==', () => {
      it('samples', () => {
        expect(lits.run('1 == 1')).toBe(true)
        expect(lits.run('1 == 2')).toBe(false)

        expect(lits.run('==(1)')).toBe(true)
        expect(lits.run('==(1, 1)')).toBe(true)
        expect(lits.run('==(1, 2)')).toBe(false)
        expect(lits.run('==(1, 2, 1)')).toBe(false)
        expect(lits.run('==(1, 2, 3)')).toBe(false)
        expect(lits.run('==("1")')).toBe(true)
        expect(lits.run('==("1", "1")')).toBe(true)
        expect(lits.run('==("1", "2")')).toBe(false)
        expect(lits.run('==("1", "2", "1")')).toBe(false)
        expect(lits.run('==("1", "2", "3")')).toBe(false)
        expect(lits.run('==("2", "2", "2")')).toBe(true)
        expect(lits.run('==(1, "2", 3)')).toBe(false)
        expect(lits.run('==(1, null, 3)')).toBe(false)
        expect(lits.run('==(1, true, 3)')).toBe(false)
        expect(lits.run('==(1, false, 3)')).toBe(false)
        expect(lits.run('==(null, null)')).toBe(true)
        expect(lits.run('==(true, true)')).toBe(true)
        expect(lits.run('==(false, false)')).toBe(true)
        expect(lits.run('==([1, 2, { a: 10, b: [null]}], [1, 2, { b: [null], a: 10}])')).toBe(true)
        expect(lits.run('==([1, 2, { a: 10, b: [null]}], [1, 2, { b: [0], a: 10}])')).toBe(false)
        expect(lits.run('==({ a: 10, b: 20}, { b: 20, a: 10})')).toBe(true)
        expect(lits.run('==([1, true, null], [1, true, null])')).toBe(true)
        expect(lits.run('==({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 20}], a: 10})')).toBe(true)
        expect(lits.run('==({ a: 10, b: [1, 2, { b: 20}]}, { b: [1, 2, { b: 21}], a: 10})')).toBe(false)
        expect(lits.run('==([1, 2, 3], [1, 2, 3, 4])')).toBe(false)
        expect(lits.run('==({ a: 10}, { a: 10, b: 20})')).toBe(false)
        expect(() => lits.run('==()')).toThrow(LitsError)
      })
    })

    describe('>', () => {
      it('samples', () => {
        expect(lits.run('1 > 2')).toBe(false)
        expect(lits.run('1 > 1')).toBe(false)
        expect(lits.run('2 > 1')).toBe(true)

        expect(lits.run('>(1)')).toBe(true)
        expect(lits.run('>(1, 2)')).toBe(false)
        expect(lits.run('>(1, 1)')).toBe(false)
        expect(lits.run('>(2, 1)')).toBe(true)
        expect(lits.run('>(2, 1, 2)')).toBe(false)
        expect(lits.run('>(2, 1, 0)')).toBe(true)
        expect(lits.run('>("albert", "ALBERT")')).toBe(true)
        expect(lits.run('>("ALBERT", "albert")')).toBe(false)
        expect(lits.run('>("albert", "alber")')).toBe(true)
        expect(lits.run('>("albert", "albert")')).toBe(false)
        expect(lits.run('>("alber", "albert")')).toBe(false)

        expect(lits.run('>("1")')).toBe(true)
        expect(lits.run('>("1", "2")')).toBe(false)
        expect(lits.run('>("1", "1")')).toBe(false)
        expect(lits.run('>("2", "1")')).toBe(true)
        expect(lits.run('>("2", "1", "2")')).toBe(false)

        expect(() => lits.run('1 > "a"')).toThrow(LitsError)
        expect(() => lits.run('>()')).toThrow(LitsError)
      })
    })

    describe('<', () => {
      it('samples', () => {
        expect(lits.run('1 < 2')).toBe(true)
        expect(lits.run('1 < 1')).toBe(false)
        expect(lits.run('2 < 1')).toBe(false)

        expect(lits.run('<(1)')).toBe(true)
        expect(lits.run('<(1, 2)')).toBe(true)
        expect(lits.run('<(1, 1)')).toBe(false)
        expect(lits.run('<(2, 1)')).toBe(false)
        expect(lits.run('<(1, 2, 1)')).toBe(false)
        expect(lits.run('<(0, 1, 2)')).toBe(true)
        expect(lits.run('<("albert", "ALBERT")')).toBe(false)
        expect(lits.run('<("ALBERT", "albert")')).toBe(true)
        expect(lits.run('<("albert", "alber")')).toBe(false)
        expect(lits.run('<("albert", "albert")')).toBe(false)
        expect(lits.run('<("alber", "albert")')).toBe(true)

        expect(lits.run('<("1")')).toBe(true)
        expect(lits.run('<("1", "2")')).toBe(true)
        expect(lits.run('<("1", "1")')).toBe(false)
        expect(lits.run('<("2", "1")')).toBe(false)
        expect(lits.run('<("1", "2", "1")')).toBe(false)

        expect(() => lits.run('1 < "a"')).toThrow(LitsError)
        expect(() => lits.run('<()')).toThrow(LitsError)
      })
    })

    describe('>=', () => {
      it('samples', () => {
        expect(lits.run('1 >= 2')).toBe(false)
        expect(lits.run('1 >= 1')).toBe(true)
        expect(lits.run('2 >= 1')).toBe(true)

        expect(lits.run('>=(1)')).toBe(true)
        expect(lits.run('>=(1, 2)')).toBe(false)
        expect(lits.run('>=(1, 1)')).toBe(true)
        expect(lits.run('>=(2, 1)')).toBe(true)
        expect(lits.run('>=(2, 1, 2)')).toBe(false)
        expect(lits.run('>=(2, 1, 1)')).toBe(true)
        expect(lits.run('>=("albert", "ALBERT")')).toBe(true)
        expect(lits.run('>=("ALBERT", "albert")')).toBe(false)
        expect(lits.run('>=("albert", "alber")')).toBe(true)
        expect(lits.run('>=("albert", "albert")')).toBe(true)
        expect(lits.run('>=("alber", "albert")')).toBe(false)

        expect(lits.run('>=("1")')).toBe(true)
        expect(lits.run('>=("1", "2")')).toBe(false)
        expect(lits.run('>=("1", "1")')).toBe(true)
        expect(lits.run('>=("2", "1")')).toBe(true)
        expect(lits.run('>=("2", "1", "2")')).toBe(false)
        expect(lits.run('>=("2", "1", "1")')).toBe(true)

        expect(() => lits.run('>=()')).toThrow(LitsError)
      })
    })

    describe('<=', () => {
      it('samples', () => {
        expect(lits.run('1 <= 2')).toBe(true)
        expect(lits.run('1 <= 1')).toBe(true)
        expect(lits.run('2 <= 1')).toBe(false)

        expect(lits.run('<=(1)')).toBe(true)
        expect(lits.run('<=(1, 2)')).toBe(true)
        expect(lits.run('<=(1, 1)')).toBe(true)
        expect(lits.run('<=(2, 1)')).toBe(false)
        expect(lits.run('<=(1, 2, 1)')).toBe(false)
        expect(lits.run('<=(1, 2, 2)')).toBe(true)
        expect(lits.run('<=("albert", "ALBERT")')).toBe(false)
        expect(lits.run('<=("ALBERT", "albert")')).toBe(true)
        expect(lits.run('<=("albert", "alber")')).toBe(false)
        expect(lits.run('<=("albert", "albert")')).toBe(true)
        expect(lits.run('<=("alber", "albert")')).toBe(true)

        expect(lits.run('<=("1")')).toBe(true)
        expect(lits.run('<=("1", "2")')).toBe(true)
        expect(lits.run('<=("1", "1")')).toBe(true)
        expect(lits.run('<=("2", "1")')).toBe(false)
        expect(lits.run('<=("1", "2", "1")')).toBe(false)
        expect(lits.run('<=("1", "2", "2")')).toBe(true)

        expect(() => lits.run('<=()')).toThrow(LitsError)
      })
    })

    describe('!', () => {
      it('samples', () => {
        expect(lits.run('!(0)')).toBe(true)
        expect(lits.run('!("")')).toBe(true)
        expect(lits.run('!("0")')).toBe(false)
        expect(lits.run('!(1)')).toBe(false)
        expect(lits.run('!(-1)')).toBe(false)
        expect(lits.run('!([])')).toBe(false)
        expect(lits.run('!(false)')).toBe(true)
        expect(lits.run('!(true)')).toBe(false)
        expect(lits.run('!(null)')).toBe(true)
        expect(() => lits.run('!(0, 1)')).toThrow(LitsError)
        expect(() => lits.run('!()')).toThrow(LitsError)
      })
    })

    describe('write!', () => {
      it('samples', () => {
        expect(lits.run('write!(1)')).toBe(1)
        expect(lits.run('write!("1")')).toBe('1')
        expect(lits.run('write!(100, [], "1")')).toBe('1')
        expect(lits.run('write!([])')).toEqual([])
        expect(lits.run('write!({})')).toEqual({})
        expect(lits.run('write!(null)')).toBe(null)
        expect(lits.run('write!(true)')).toBe(true)
        expect(lits.run('write!(false)')).toBe(false)
        expect(lits.run('write!()')).toBe(null)
      })
      it('that it does console.log', () => {
        lits.run('write!(1)')
        expect(logSpy).toHaveBeenCalledWith(1)
      })
    })

    describe('boolean', () => {
      it('samples', () => {
        expect(lits.run('boolean(0)')).toBe(false)
        expect(lits.run('boolean(1)')).toBe(true)
        expect(lits.run('boolean("Albert")')).toBe(true)
        expect(lits.run('boolean("")')).toBe(false)
        expect(lits.run('boolean(true)')).toBe(true)
        expect(lits.run('boolean(false)')).toBe(false)
        expect(lits.run('boolean(null)')).toBe(false)
        expect(lits.run('boolean([])')).toBe(true)
        expect(lits.run('boolean({})')).toBe(true)
        expect(() => lits.run('boolean()')).toThrow(LitsError)
        expect(() => lits.run('boolean(2, 3)')).toThrow(LitsError)
      })
    })

    describe('compare', () => {
      it('samples', () => {
        expect(lits.run('compare(0, 1)')).toBe(-1)
        expect(lits.run('compare(3, 1)')).toBe(1)
        expect(lits.run('compare("A", "a")')).toBe(-1)
        expect(lits.run('compare("A", "A")')).toBe(0)
      })
    })

    describe('json-stringify', () => {
      it('samples', () => {
        expect(lits.run('json-stringify({ a: 10, b: 20})')).toBe('{"a":10,"b":20}')
        expect(lits.run('json-stringify({ a: 10, b: 20}, 2)')).toBe('{\n  "a": 10,\n  "b": 20\n}')
      })
    })
    describe('json-parse', () => {
      it('samples', () => {
        expect(lits.run('json-parse("[1,2,3]")')).toEqual([1, 2, 3])
      })
    })
    describe('doc', () => {
      it('should return the doc for a function', () => {
        expect(lits.run('doc(+)')).toBeDefined()
        expect(lits.run('doc(2)')).toBe('')
        expect(lits.run(`
          function add(a, b) {
            """
            Adds two numbers together.
            Returns the sum of a and b.
            """
            
            a + b
          };
          doc(add)
        `)).toBe('Adds two numbers together.\nReturns the sum of a and b.')
        expect(lits.run(`
          function add() {
            """Escaping\\"""."""
            a + b
          };
          doc(add)
        `)).toBe('Escaping""".')
      })
    })
    describe('arity', () => {
      it('should return the arity of a function', () => {
        expect(lits.run('arity(+)')).toEqual({})
        expect(lits.run('arity(1)')).toEqual({ min: 1, max: 1 })
        expect(lits.run('arity((...x) -> x)')).toEqual({})
      })
    })
  }
})
