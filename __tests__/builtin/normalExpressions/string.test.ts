import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { LitsError } from '../../../src/errors'
import { stringUtilsModule } from '../../../src/builtin/modules/string'

describe('string functions', () => {
  for (const lits of [new Lits({}), new Lits({ debug: true })]) {
    describe('str', () => {
      it('samples', () => {
        expect(lits.run('str({})')).toBe('{}')
        expect(lits.run('str(x)', { bindings: { x: null } })).toBe('')
        expect(lits.run('str()')).toBe('')
        expect(lits.run('str("")')).toBe('')
        expect(lits.run('str("1")')).toBe('1')
        expect(lits.run('str("1", "2")')).toBe('12')
        expect(lits.run('str("1", "2", "three", "4")')).toBe('12three4')
        expect(lits.run('str(0)')).toBe('0')
        expect(lits.run('str(true)')).toBe('true')
        expect(lits.run('str("1", false)')).toBe('1false')
        expect(lits.run('str(null, "m")')).toBe('m')
        expect(lits.run('str(null)')).toBe('')
        expect(lits.run('str([])')).toBe('[]')
        expect(lits.run('str([1, 2, 3])')).toBe('[1,2,3]')
        expect(lits.run('str({a: 1})')).toBe('{"a":1}')
      })

      it('regressions', () => {
        expect(lits.run('str(")")')).toBe(')')
      })
    })

    describe('number', () => {
      it('samples', () => {
        expect(lits.run('number("123.25")')).toBe(123.25)
        expect(lits.run('number("0b1111")')).toBe(15)
        expect(lits.run('number("0Xf")')).toBe(15)
        expect(lits.run('number("0o17")')).toBe(15)
        expect(lits.run('number("-0.125")')).toBe(-0.125)
        expect(() => lits.run('number()')).toThrow(LitsError)
        expect(() => lits.run('number("987", "65")')).toThrow(LitsError)
        expect(() => lits.run('number("non parsable number")')).toThrow(LitsError)
      })
    })

    describe('lower-case', () => {
      it('samples', () => {
        expect(lits.run('lower-case("Albert!")')).toBe('albert!')
        expect(lits.run('lower-case("")')).toBe('')
        expect(() => lits.run('lower-case()')).toThrow(LitsError)
        expect(() => lits.run('lower-case("First", "Second")')).toThrow(LitsError)
      })
    })

    describe('upper-case', () => {
      it('samples', () => {
        expect(lits.run('upper-case("Albert!")')).toBe('ALBERT!')
        expect(lits.run('upper-case("")')).toBe('')
        expect(() => lits.run('upper-case()')).toThrow(LitsError)
        expect(() => lits.run('upper-case("First", "Second")')).toThrow(LitsError)
      })
    })

    describe('trim', () => {
      it('samples', () => {
        expect(lits.run('trim("  Albert!  ")')).toBe('Albert!')
        expect(lits.run('trim(" ")')).toBe('')
        expect(lits.run('trim("")')).toBe('')
        expect(() => lits.run('trim()')).toThrow(LitsError)
        expect(() => lits.run('trim("First", "Second")')).toThrow(LitsError)
      })
    })

    describe('blank?', () => {
      it('samples', () => {
        expect(lits.run('blank?("")')).toBe(true)
        expect(lits.run('blank?(" ")')).toBe(true)
        expect(lits.run('blank?("\n")')).toBe(true)
        expect(lits.run('blank?("  ")')).toBe(true)
        expect(lits.run('blank?("  a")')).toBe(false)
        expect(lits.run('blank?("a  ")')).toBe(false)
        expect(lits.run('blank?(" a ")')).toBe(false)
        expect(lits.run('blank?(" a b ")')).toBe(false)
        expect(lits.run('blank?(null)')).toBe(true)
        expect(() => lits.run('blank?(true)')).toThrow(LitsError)
        expect(() => lits.run('blank?(false)')).toThrow(LitsError)
        expect(() => lits.run('blank?(0)')).toThrow(LitsError)
        expect(() => lits.run('blank?([])')).toThrow(LitsError)
        expect(() => lits.run('blank?({})')).toThrow(LitsError)
        expect(() => lits.run('blank?()')).toThrow(LitsError)
        expect(() => lits.run('blank?("a", "b")')).toThrow(LitsError)
      })
    })
  }
})

describe('string-Utils module functions', () => {
  const imp = 'let su = import("string"); '
  for (const lits of [new Lits({ modules: [stringUtilsModule] }), new Lits({ modules: [stringUtilsModule], debug: true })]) {
    describe('trim-left', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.trim-left("  Albert!  ")`)).toBe('Albert!  ')
        expect(lits.run(`${imp}su.trim-left(" ")`)).toBe('')
        expect(lits.run(`${imp}su.trim-left("")`)).toBe('')
        expect(() => lits.run(`${imp}su.trim-left()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.trim-left("First", "Second")`)).toThrow(LitsError)
      })
    })

    describe('trim-right', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.trim-right("  Albert!  ")`)).toBe('  Albert!')
        expect(lits.run(`${imp}su.trim-right(" ")`)).toBe('')
        expect(lits.run(`${imp}su.trim-right("")`)).toBe('')
        expect(() => lits.run(`${imp}su.trim-right()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.trim-right("First", "Second")`)).toThrow(LitsError)
      })
    })

    describe('pad-left', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.pad-left("Albert", 10)`)).toBe('    Albert')
        expect(lits.run(`${imp}su.pad-left("Albert", 10, "*")`)).toBe('****Albert')
        expect(lits.run(`${imp}su.pad-left("Albert", 10, "123")`)).toBe('1231Albert')
        expect(lits.run(`${imp}su.pad-left("Albert", 5)`)).toBe('Albert')
        expect(lits.run(`${imp}su.pad-left("Albert", -1)`)).toBe('Albert')
        expect(() => lits.run(`${imp}su.pad-left()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.pad-left("First", "Second")`)).toThrow(LitsError)
      })
    })

    describe('pad-right', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.pad-right("Albert", 10)`)).toBe('Albert    ')
        expect(lits.run(`${imp}su.pad-right("Albert", 10, "*")`)).toBe('Albert****')
        expect(lits.run(`${imp}su.pad-right("Albert", 10, "123")`)).toBe('Albert1231')
        expect(lits.run(`${imp}su.pad-right("Albert", 5)`)).toBe('Albert')
        expect(lits.run(`${imp}su.pad-right("Albert", -1)`)).toBe('Albert')
        expect(() => lits.run(`${imp}su.pad-right()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.pad-right("First", "Second")`)).toThrow(LitsError)
      })
    })

    describe('split-lines', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.split-lines("Albert\\nMojir")`)).toEqual(['Albert', 'Mojir'])
        expect(lits.run(`${imp}su.split-lines("Albert\\nMojir\\n")`)).toEqual(['Albert', 'Mojir'])
        expect(lits.run(`${imp}su.split-lines("\\n\\nAlbert\\n\\n\\nMojir\\n")`)).toEqual(['Albert', 'Mojir'])
      })
    })

    describe('string-repeat', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.string-repeat("*", 10)`)).toBe('**********')
        expect(lits.run(`${imp}su.string-repeat("*", 0)`)).toBe('')
        expect(lits.run(`${imp}su.string-repeat("Hello, ", 3)`)).toBe('Hello, Hello, Hello, ')
        expect(() => lits.run(`${imp}su.string-repeat()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.string-repeat("Hello, ")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.string-repeat("Hello, ", 3, 3)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.string-repeat("Hello, ", "3")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.string-repeat(true, 1)`)).toThrow(LitsError)
      })
    })

    describe('template', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.template("Hi")`)).toBe('Hi')
        expect(lits.run(`${imp}su.template("Hi", "Carl")`)).toBe('Hi')
        expect(lits.run(`${imp}su.template("Hi, $1", "Carl")`)).toBe('Hi, Carl')
        expect(lits.run(`${imp}su.template("Hi, $$$1", "Carl")`)).toBe('Hi, $Carl')
        expect(lits.run(`${imp}su.template("Hi, $$1", "Carl")`)).toBe('Hi, $1')
        expect(lits.run(`${imp}su.template("Hi, $1", "Carl")`)).toBe('Hi, Carl')
        expect(lits.run(`${imp}su.template("Hi, $1", "Carl", "Larry")`)).toBe('Hi, Carl')
        expect(lits.run(`${imp}su.template("Hi, $1 and $2", "Carl", "Larry")`)).toBe('Hi, Carl and Larry')
        expect(lits.run(`${imp}su.template("Hi, $1 and $3", "Carl", "Larry", "Sofi")`)).toBe('Hi, Carl and Sofi')
        expect(lits.run(`${imp}su.template("$1", "Carl")`)).toBe('Carl')
        expect(lits.run(`${imp}su.template("$$1", "Carl")`)).toBe('$1')
        expect(lits.run(`${imp}su.template("$$$1", "Carl")`)).toBe('$Carl')
        expect(lits.run(`${imp}su.template("Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9", "A", "B", "C", "D", "E", "F", "G", "H", "I")`)).toBe(
          'Hi A, B, C, D, E, F, G, H and I',
        )
        expect(() =>
          lits.run(`${imp}su.template("Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9", "A", "B", "C", "D", "E", "F", "G", "H")`),
        ).toThrow()
        expect(lits.run(`${imp}su.template("Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 and $10", "A", "B", "C", "D", "E", "F", "G", "H", "I")`)).toBe(
          'Hi A, B, C, D, E, F, G, H, I and A0',
        )
        expect(lits.run(`${imp}su.template("$1", 0)`)).toBe('0')
        expect(() =>
          lits.run(`${imp}su.template("Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 $10", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J")`),
        ).toThrow()
        expect(() => lits.run(`${imp}su.template()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", true)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", false)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", null)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", undefined)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", [])`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1", object())`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(true)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(false)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(null)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(undefined)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(1)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template([]`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template(object())`)).toThrow(LitsError)
      })
      it('pluralization samples', () => {
        expect(lits.run(`${imp}su.template("", 0)`)).toBe('')
        expect(lits.run(`${imp}su.template("$1 book||||$1 books", 0)`)).toBe('0 books')
        expect(lits.run(`${imp}su.template("$1 book||||$1 books", 1)`)).toBe('1 book')
        expect(lits.run(`${imp}su.template("$1 book||||$1 books", 2)`)).toBe('2 books')
        expect(lits.run(`${imp}su.template("No books||||$1 book||||$1 books", 0)`)).toBe('No books')
        expect(lits.run(`${imp}su.template("No books||||$1 book||||$1 books", 1)`)).toBe('1 book')
        expect(lits.run(`${imp}su.template("No books||||$1 book||||$1 books", 3)`)).toBe('3 books')
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 0)`)).toBe('No books')
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 1)`)).toBe('One book')
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 2)`)).toBe(
          'Two books',
        )
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 3)`)).toBe(
          'Three books',
        )
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 4)`)).toBe('4 books')
        expect(lits.run(`${imp}su.template("No books||||One book||||Two books||||Three books||||$1 books", 14)`)).toBe(
          '14 books',
        )
        expect(() => lits.run(`${imp}su.template("No books||||$1 book||||$1 books||||$1books", -3)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1 book||||$1 books")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1 book||||$1 books", "1")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.template("$1 book||||$1 books||||$1 chairs", )`)).toThrow(LitsError)
        expect(lits.run(`${imp}su.template("$2 got $1 book||||$2 got $1 books", 1, "Carl")`)).toBe('Carl got 1 book')
        expect(lits.run(`${imp}su.template("$2 got $1 book||||$2 got $1 books", 2, "Carl")`)).toBe('Carl got 2 books')
      })
    })

    describe('to-char-code', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.to-char-code("a")`)).toBe(97)
        expect(lits.run(`${imp}su.to-char-code("abc")`)).toBe(97)
        expect(() => lits.run(`${imp}su.to-char-code()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.to-char-code("A" "B")`)).toThrow(LitsError)
      })
    })

    describe('from-char-code', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.from-char-code(97)`)).toBe('a')
        expect(() => lits.run(`${imp}su.from-char-code(9700000)`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.from-char-code()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.from-char-code(65, 66)`)).toThrow(LitsError)
      })
    })

    describe('encode-base64', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.encode-base64("Albert")`)).toBe('QWxiZXJ0')
        expect(lits.run(`${imp}su.encode-base64("Albert is a 🐻")`)).toBe('QWxiZXJ0IGlzIGEg8J+Quw==')
        expect(() => lits.run(`${imp}su.encode-base64()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.encode-base64("X" "Y")`)).toThrow(LitsError)
      })
    })

    describe('decode-base64', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.decode-base64("QWxiZXJ0")`)).toBe('Albert')
        expect(lits.run(`${imp}su.decode-base64("QWxiZXJ0IGlzIGEg8J+Quw==")`)).toBe('Albert is a 🐻')
        expect(() => lits.run(`${imp}su.decode-base64("Illegal string ~")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.decode-base64()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.decode-base64("X" "Y")`)).toThrow(LitsError)
      })
    })

    describe('encode-uri-component', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.encode-uri-component("a string")`)).toBe('a%20string')
        expect(() => lits.run(`${imp}su.encode-uri-component()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.encode-uri-component("X" "Y")`)).toThrow(LitsError)
      })
    })

    describe('decode-uri-component', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.decode-uri-component("a%20string")`)).toBe('a string')
        expect(() => lits.run(`${imp}su.decode-uri-component("a%AFc")`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.decode-uri-component()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.decode-uri-component("X" "Y")`)).toThrow(LitsError)
      })
    })

    describe('capitalize', () => {
      it('samples', () => {
        expect(lits.run(`${imp}su.capitalize("albert")`)).toBe('Albert')
        expect(lits.run(`${imp}su.capitalize("Albert")`)).toBe('Albert')
        expect(lits.run(`${imp}su.capitalize("ALBERT")`)).toBe('Albert')
        expect(lits.run(`${imp}su.capitalize("")`)).toBe('')
        expect(() => lits.run(`${imp}su.capitalize()`)).toThrow(LitsError)
        expect(() => lits.run(`${imp}su.capitalize("First", "Second")`)).toThrow(LitsError)
      })
    })
  }
})
