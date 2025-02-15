import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src'

describe('string functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('subs', () => {
      it('samples', () => {
        expect(() => lits.run('(subs "abcde")')).toThrow()
        expect(lits.run('(subs "abcde" 0)')).toBe('abcde')
        expect(lits.run('(subs "abcde" 1)')).toBe('bcde')
        expect(lits.run('(subs "abcde" 2)')).toBe('cde')
        expect(lits.run('(subs "abcde" 3)')).toBe('de')
        expect(lits.run('(subs "abcde" 4)')).toBe('e')
        expect(lits.run('(subs "abcde" 5)')).toBe('')
        expect(lits.run('(subs "abcde" 6)')).toBe('')
        expect(lits.run('(subs "abcde" 0 0)')).toBe('')
        expect(() => lits.run('(subs "abcde" 1 0)')).toThrow()
        expect(lits.run('(subs "abcde" 1 1)')).toBe('')
        expect(lits.run('(subs "abcde" 1 2)')).toBe('b')
        expect(lits.run('(subs "abcde" 1 3)')).toBe('bc')
        expect(lits.run('(subs "abcde" 1 4)')).toBe('bcd')
        expect(lits.run('(subs "abcde" 1 5)')).toBe('bcde')
        expect(lits.run('(subs "abcde" 1 6)')).toBe('bcde')
      })
    })

    describe('str', () => {
      it('samples', () => {
        // expect(lits.run('(str x)', { values: { x: null } })).toBe('')
        // expect(lits.run('(str)')).toBe('')
        // expect(lits.run('(str "")')).toBe('')
        // expect(lits.run('(str :1)')).toBe('1')
        // expect(lits.run('(str :1 :2)')).toBe('12')
        // expect(lits.run('(str :1 :2 "three" :4)')).toBe('12three4')
        // expect(lits.run('(str 0)')).toBe('0')
        // expect(lits.run('(str true)')).toBe('true')
        // expect(lits.run('(str :1 false)')).toBe('1false')
        // expect(lits.run('(str nil :m)')).toBe('m')
        // expect(lits.run('(str nil)')).toBe('')
        // expect(lits.run('(str [])')).toBe('[]')
        // expect(lits.run('(str [1 2 3])')).toBe('[1,2,3]')
        // expect(lits.run('(str {})')).toBe('{}')
        expect(lits.run('(str {:a 1})')).toBe('{"a":1}')
      })

      it('regressions', () => {
        expect(lits.run('(str ")")')).toBe(')')
      })
    })

    describe('number', () => {
      it('samples', () => {
        expect(lits.run('(number "123.25")')).toBe(123.25)
        expect(lits.run('(number "0b1111")')).toBe(15)
        expect(lits.run('(number "0Xf")')).toBe(15)
        expect(lits.run('(number "0o17")')).toBe(15)
        expect(lits.run('(number "-0.125")')).toBe(-0.125)
        expect(() => lits.run('(number)')).toThrow()
        expect(() => lits.run('(number "987" "65")')).toThrow()
        expect(() => lits.run('(number "non parsable number")')).toThrow()
      })
    })

    describe('number-to-string', () => {
      it('samples', () => {
        expect(lits.run('(number-to-string 10.25)')).toBe('10.25')
        expect(lits.run('(number-to-string -11)')).toBe('-11')
        expect(lits.run('(number-to-string 11 2)')).toBe('1011')
        expect(lits.run('(number-to-string 11 8)')).toBe('13')
        expect(lits.run('(number-to-string 11.11 10)')).toBe('11.11')
        expect(() => lits.run('(number-to-string)')).toThrow()
        expect(() => lits.run('(number-to-string -1 2)')).toThrow()
        expect(() => lits.run('(number-to-string 1.5 2)')).toThrow()
        expect(() => lits.run('(number-to-string 1.5 8)')).toThrow()
        expect(() => lits.run('(number-to-string 1.5 16)')).toThrow()
        expect(() => lits.run('(number-to-string -1 2)')).toThrow()
        expect(() => lits.run('(number-to-string -1 8)')).toThrow()
        expect(() => lits.run('(number-to-string -1 16)')).toThrow()
        expect(() => lits.run('(number-to-string 10 7)')).toThrow()
        expect(() => lits.run('(number-to-string 10 20)')).toThrow()
      })
    })

    describe('lower-case', () => {
      it('samples', () => {
        expect(lits.run('(lower-case "Albert!")')).toBe('albert!')
        expect(lits.run('(lower-case "")')).toBe('')
        expect(() => lits.run('(lower-case)')).toThrow()
        expect(() => lits.run('(lower-case "First" "Second")')).toThrow()
      })
    })

    describe('upper-case', () => {
      it('samples', () => {
        expect(lits.run('(upper-case "Albert!")')).toBe('ALBERT!')
        expect(lits.run('(upper-case "")')).toBe('')
        expect(() => lits.run('(upper-case)')).toThrow()
        expect(() => lits.run('(upper-case "First" "Second")')).toThrow()
      })
    })

    describe('trim', () => {
      it('samples', () => {
        expect(lits.run('(trim "  Albert!  ")')).toBe('Albert!')
        expect(lits.run('(trim " ")')).toBe('')
        expect(lits.run('(trim "")')).toBe('')
        expect(() => lits.run('(trim)')).toThrow()
        expect(() => lits.run('(trim "First" "Second")')).toThrow()
      })
    })

    describe('trim-left', () => {
      it('samples', () => {
        expect(lits.run('(trim-left "  Albert!  ")')).toBe('Albert!  ')
        expect(lits.run('(trim-left " ")')).toBe('')
        expect(lits.run('(trim-left "")')).toBe('')
        expect(() => lits.run('(trim-left)')).toThrow()
        expect(() => lits.run('(trim-left "First" "Second")')).toThrow()
      })
    })

    describe('trim-right', () => {
      it('samples', () => {
        expect(lits.run('(trim-right "  Albert!  ")')).toBe('  Albert!')
        expect(lits.run('(trim-right " ")')).toBe('')
        expect(lits.run('(trim-right "")')).toBe('')
        expect(() => lits.run('(trim-right)')).toThrow()
        expect(() => lits.run('(trim-right "First" "Second")')).toThrow()
      })
    })

    describe('pad-left', () => {
      it('samples', () => {
        expect(lits.run('(pad-left "Albert" 10)')).toBe('    Albert')
        expect(lits.run('(pad-left "Albert" 10 "*")')).toBe('****Albert')
        expect(lits.run('(pad-left "Albert" 10 "123")')).toBe('1231Albert')
        expect(lits.run('(pad-left "Albert" 5)')).toBe('Albert')
        expect(lits.run('(pad-left "Albert" -1)')).toBe('Albert')
        expect(() => lits.run('(pad-left)')).toThrow()
        expect(() => lits.run('(pad-left "First" "Second")')).toThrow()
      })
    })

    describe('pad-right', () => {
      it('samples', () => {
        expect(lits.run('(pad-right "Albert" 10)')).toBe('Albert    ')
        expect(lits.run('(pad-right "Albert" 10 "*")')).toBe('Albert****')
        expect(lits.run('(pad-right "Albert" 10 "123")')).toBe('Albert1231')
        expect(lits.run('(pad-right "Albert" 5)')).toBe('Albert')
        expect(lits.run('(pad-right "Albert" -1)')).toBe('Albert')
        expect(() => lits.run('(pad-right)')).toThrow()
        expect(() => lits.run('(pad-right "First" "Second")')).toThrow()
      })
    })

    describe('split', () => {
      it('samples', () => {
        expect(lits.run('(split "Albert Mojir" " ")')).toEqual(['Albert', 'Mojir'])
        expect(lits.run('(split "0123456789" "")')).toEqual(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
        expect(lits.run('(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))')).toEqual([
          '',
          'bcd',
          'fgh',
          'jklmn',
          'pqrst',
          'vw',
        ])
        expect(lits.run('(map number (split "0123456789" "" 5))')).toEqual([0, 1, 2, 3, 4])
        expect(() => lits.run('(split "0123456789")')).toThrow()
        expect(() => lits.run('(split "0123456789" :5 -1)')).toThrow()
        expect(() => lits.run('(split 23456789 :5)')).toThrow()
      })
    })

    describe('string-repeat', () => {
      it('samples', () => {
        expect(lits.run('(string-repeat "*" 10)')).toBe('**********')
        expect(lits.run('(string-repeat "*" 0)')).toBe('')
        expect(lits.run('(string-repeat "Hello, " 3)')).toBe('Hello, Hello, Hello, ')
        expect(() => lits.run('(string-repeat)')).toThrow()
        expect(() => lits.run('(string-repeat "Hello, ")')).toThrow()
        expect(() => lits.run('(string-repeat "Hello, " 3 3)')).toThrow()
        expect(() => lits.run('(string-repeat "Hello, " :3)')).toThrow()
        expect(() => lits.run('(string-repeat true, 1)')).toThrow()
      })
    })

    describe('template', () => {
      it('samples', () => {
        expect(lits.run('(template "Hi")')).toBe('Hi')
        expect(lits.run('(template "Hi" "Carl")')).toBe('Hi')
        expect(lits.run('(template "Hi, $1", "Carl")')).toBe('Hi, Carl')
        expect(lits.run('(template "Hi, $$$1", "Carl")')).toBe('Hi, $Carl')
        expect(lits.run('(template "Hi, $$1", "Carl")')).toBe('Hi, $1')
        expect(lits.run('(template "Hi, $1" "Carl")')).toBe('Hi, Carl')
        expect(lits.run('(template "Hi, $1" "Carl" "Larry")')).toBe('Hi, Carl')
        expect(lits.run('(template "Hi, $1 and $2" "Carl" "Larry")')).toBe('Hi, Carl and Larry')
        expect(lits.run('(template "Hi, $1 and $3" "Carl" "Larry" "Sofi")')).toBe('Hi, Carl and Sofi')
        expect(lits.run('(template "$1", "Carl")')).toBe('Carl')
        expect(lits.run('(template "$$1", "Carl")')).toBe('$1')
        expect(lits.run('(template "$$$1", "Carl")')).toBe('$Carl')
        expect(lits.run('(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" :A :B :C :D :E :F :G :H :I)')).toBe(
          'Hi A, B, C, D, E, F, G, H and I',
        )
        expect(() =>
          lits.run('(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" :A :B :C :D :E :F :G :H)'),
        ).toThrow()
        expect(lits.run('(template "Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 and $10" :A :B :C :D :E :F :G :H :I)')).toBe(
          'Hi A, B, C, D, E, F, G, H, I and A0',
        )
        expect(lits.run('(template "$1", 0)')).toBe('0')
        expect(() =>
          lits.run('(template "Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 $10" :A :B :C :D :E :F :G :H :I :J)'),
        ).toThrow()
        expect(() => lits.run('(template)')).toThrow()
        expect(() => lits.run('(template "$1", true)')).toThrow()
        expect(() => lits.run('(template "$1", false)')).toThrow()
        expect(() => lits.run('(template "$1", nil)')).toThrow()
        expect(() => lits.run('(template "$1", undefined)')).toThrow()
        expect(() => lits.run('(template "$1", [])')).toThrow()
        expect(() => lits.run('(template "$1", (object))')).toThrow()
        expect(() => lits.run('(template true)')).toThrow()
        expect(() => lits.run('(template false)')).toThrow()
        expect(() => lits.run('(template nil)')).toThrow()
        expect(() => lits.run('(template undefined)')).toThrow()
        expect(() => lits.run('(template 1)')).toThrow()
        expect(() => lits.run('(template []')).toThrow()
        expect(() => lits.run('(template (object))')).toThrow()
      })
      it('pluralization samples', () => {
        expect(lits.run('(template "" 0)')).toBe('')
        expect(lits.run('(template "$1 book||||$1 books" 0)')).toBe('0 books')
        expect(lits.run('(template "$1 book||||$1 books" 1)')).toBe('1 book')
        expect(lits.run('(template "$1 book||||$1 books" 2)')).toBe('2 books')
        expect(lits.run('(template "No books||||$1 book||||$1 books" 0)')).toBe('No books')
        expect(lits.run('(template "No books||||$1 book||||$1 books" 1)')).toBe('1 book')
        expect(lits.run('(template "No books||||$1 book||||$1 books" 3)')).toBe('3 books')
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 0)')).toBe('No books')
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 1)')).toBe('One book')
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 2)')).toBe(
          'Two books',
        )
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 3)')).toBe(
          'Three books',
        )
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 4)')).toBe('4 books')
        expect(lits.run('(template "No books||||One book||||Two books||||Three books||||$1 books" 14)')).toBe(
          '14 books',
        )
        expect(() => lits.run('(template "No books||||$1 book||||$1 books||||$1books" -3)')).toThrow()
        expect(() => lits.run('(template "$1 book||||$1 books")')).toThrow()
        expect(() => lits.run('(template "$1 book||||$1 books" :1)')).toThrow()
        expect(() => lits.run('(template "$1 book||||$1 books||||$1 chairs" )')).toThrow()
        expect(lits.run('(template "$2 got $1 book||||$2 got $1 books" 1 "Carl")')).toBe('Carl got 1 book')
        expect(lits.run('(template "$2 got $1 book||||$2 got $1 books" 2 "Carl")')).toBe('Carl got 2 books')
      })
    })
    describe('to-char-code', () => {
      it('samples', () => {
        expect(lits.run('(to-char-code :a)')).toBe(97)
        expect(lits.run('(to-char-code "abc")')).toBe(97)
        expect(() => lits.run('(to-char-code)')).toThrow()
        expect(() => lits.run('(to-char-code :A :B)')).toThrow()
      })
    })
    describe('from-char-code', () => {
      it('samples', () => {
        expect(lits.run('(from-char-code 97)')).toBe('a')
        expect(() => lits.run('(from-char-code 9700000)')).toThrow()
        expect(() => lits.run('(from-char-code)')).toThrow()
        expect(() => lits.run('(from-char-code 65 66)')).toThrow()
      })
    })

    describe('encode-base64', () => {
      it('samples', () => {
        expect(lits.run('(encode-base64 :Albert)')).toBe('QWxiZXJ0')
        expect(lits.run('(encode-base64 "Albert is a 🐻")')).toBe('QWxiZXJ0IGlzIGEg8J+Quw==')
        expect(() => lits.run('(encode-base64)')).toThrow()
        expect(() => lits.run('(encode-base64 :X :Y)')).toThrow()
      })
    })

    describe('decode-base64', () => {
      it('samples', () => {
        expect(lits.run('(decode-base64 "QWxiZXJ0")')).toBe('Albert')
        expect(lits.run('(decode-base64 "QWxiZXJ0IGlzIGEg8J+Quw==")')).toBe('Albert is a 🐻')
        expect(() => lits.run('(decode-base64 "Illegal string ~")')).toThrow()
        expect(() => lits.run('(decode-base64)')).toThrow()
        expect(() => lits.run('(decode-base64 :X :Y)')).toThrow()
      })
    })

    describe('encode-uri-component', () => {
      it('samples', () => {
        expect(lits.run('(encode-uri-component "a string")')).toBe('a%20string')
        expect(() => lits.run('(encode-uri-component)')).toThrow()
        expect(() => lits.run('(encode-uri-component :X :Y)')).toThrow()
      })
    })

    describe('decode-uri-component', () => {
      it('samples', () => {
        expect(lits.run('(decode-uri-component "a%20string")')).toBe('a string')
        expect(() => lits.run('(decode-uri-component "a%AFc")')).toThrow()
        expect(() => lits.run('(decode-uri-component)')).toThrow()
        expect(() => lits.run('(decode-uri-component :X :Y)')).toThrow()
      })
    })

    it('short hand strings', () => {
      expect(lits.run(':a')).toBe('a')
      expect(lits.run(':a-a')).toBe('a-a')
      expect(() => lits.run(':')).toThrow()
    })
  }
})
