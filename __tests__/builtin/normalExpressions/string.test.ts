import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe('string functions', () => {
  describe('substring', () => {
    test('samples', () => {
      expect(() => lispish.run(`(substring "abcde")`)).toThrow()
      expect(lispish.run(`(substring "abcde" 0)`)).toBe('abcde')
      expect(lispish.run(`(substring "abcde" 1)`)).toBe('bcde')
      expect(lispish.run(`(substring "abcde" 2)`)).toBe('cde')
      expect(lispish.run(`(substring "abcde" 3)`)).toBe('de')
      expect(lispish.run(`(substring "abcde" 4)`)).toBe('e')
      expect(lispish.run(`(substring "abcde" 5)`)).toBe('')
      expect(lispish.run(`(substring "abcde" 6)`)).toBe('')
      expect(lispish.run(`(substring "abcde" 0 0)`)).toBe('')
      expect(() => lispish.run(`(substring "abcde" 1 0)`)).toThrow()
      expect(lispish.run(`(substring "abcde" 1 1)`)).toBe('')
      expect(lispish.run(`(substring "abcde" 1 2)`)).toBe('b')
      expect(lispish.run(`(substring "abcde" 1 3)`)).toBe('bc')
      expect(lispish.run(`(substring "abcde" 1 4)`)).toBe('bcd')
      expect(lispish.run(`(substring "abcde" 1 5)`)).toBe('bcde')
      expect(lispish.run(`(substring "abcde" 1 6)`)).toBe('bcde')
    })
  })

  describe('stringLenght', () => {
    test('samples', () => {
      expect(lispish.run(`(string-length "")`)).toBe(0)
      expect(lispish.run(`(string-length "1")`)).toBe(1)
      expect(lispish.run(`(string-length "123")`)).toBe(3)
      expect(() => lispish.run(`(string-length '())`)).toThrow()
      expect(() => lispish.run(`(string-length '(1))`)).toThrow()
      expect(() => lispish.run(`(string-length '(1 2 3))`)).toThrow()
      expect(() => lispish.run(`(string-length)`)).toThrow()
      expect(() => lispish.run(`(string-length "" "")`)).toThrow()
      expect(() => lispish.run(`(string-length 12)`)).toThrow()
      expect(() => lispish.run(`(string-length false)`)).toThrow()
      expect(() => lispish.run(`(string-length true)`)).toThrow()
      expect(() => lispish.run(`(string-length null)`)).toThrow()
      expect(() => lispish.run(`(string-length undefined)`)).toThrow()
      expect(() => lispish.run(`(string-length (object))`)).toThrow()
    })
  })

  describe('concat', () => {
    test('samples', () => {
      expect(lispish.run(`(concat)`)).toBe('')
      expect(lispish.run(`(concat "")`)).toBe('')
      expect(lispish.run(`(concat "1")`)).toBe('1')
      expect(lispish.run(`(concat "1" "2")`)).toBe('12')
      expect(lispish.run(`(concat "1" "2" "three" "4")`)).toBe('12three4')
      expect(() => lispish.run(`(concat 0)`)).toThrow()
      expect(() => lispish.run(`(concat true)`)).toThrow()
      expect(() => lispish.run(`(concat "1" false)`)).toThrow()
      expect(() => lispish.run(`(concat null "m")`)).toThrow()
      expect(() => lispish.run(`(concat undefined)`)).toThrow()
      expect(() => lispish.run(`(concat '())`)).toThrow()
      expect(() => lispish.run(`(concat (object))`)).toThrow()
    })
  })

  describe('string>', () => {
    test('samples', () => {
      expect(lispish.run(`(string> "albert" "ALBERT")`)).toBe(true)
      expect(lispish.run(`(string> "ALBERT" "albert")`)).toBe(false)
      expect(lispish.run(`(string> "albert" "alber")`)).toBe(true)
      expect(lispish.run(`(string> "albert" "albert")`)).toBe(false)
      expect(lispish.run(`(string> "alber" "albert")`)).toBe(false)
      expect(() => lispish.run(`(string>)`)).toThrow()
      expect(() => lispish.run(`(string> "a")`)).toThrow()
      expect(() => lispish.run(`(string> "a", "A", "Q")`)).toThrow()
      expect(() => lispish.run(`(string> 2 1)`)).toThrow()
      expect(() => lispish.run(`(string> null null)`)).toThrow()
      expect(() => lispish.run(`(string> undefined undefined)`)).toThrow()
      expect(() => lispish.run(`(string> true true)`)).toThrow()
      expect(() => lispish.run(`(string> false false)`)).toThrow()
      expect(() => lispish.run(`(string> "a" true)`)).toThrow()
      expect(() => lispish.run(`(string> true "a")`)).toThrow()
      expect(() => lispish.run(`(string> '() "a")`)).toThrow()
      expect(() => lispish.run(`(string> (object) "a")`)).toThrow()
    })
  })

  describe('string<', () => {
    test('samples', () => {
      expect(lispish.run(`(string< "albert" "ALBERT")`)).toBe(false)
      expect(lispish.run(`(string< "ALBERT" "albert")`)).toBe(true)
      expect(lispish.run(`(string< "albert" "alber")`)).toBe(false)
      expect(lispish.run(`(string< "albert" "albert")`)).toBe(false)
      expect(lispish.run(`(string< "alber" "albert")`)).toBe(true)
      expect(() => lispish.run(`(string<)`)).toThrow()
      expect(() => lispish.run(`(string< "a")`)).toThrow()
      expect(() => lispish.run(`(string< "a", "A", "Q")`)).toThrow()
      expect(() => lispish.run(`(string< 2 1)`)).toThrow()
      expect(() => lispish.run(`(string< null null)`)).toThrow()
      expect(() => lispish.run(`(string< undefined undefined)`)).toThrow()
      expect(() => lispish.run(`(string< true true)`)).toThrow()
      expect(() => lispish.run(`(string< false false)`)).toThrow()
      expect(() => lispish.run(`(string< "a" true)`)).toThrow()
      expect(() => lispish.run(`(string< true "a")`)).toThrow()
      expect(() => lispish.run(`(string< '() "a")`)).toThrow()
      expect(() => lispish.run(`(string< (object) "a")`)).toThrow()
    })
  })

  describe('string>=', () => {
    test('samples', () => {
      expect(lispish.run(`(string>= "albert" "ALBERT")`)).toBe(true)
      expect(lispish.run(`(string>= "ALBERT" "albert")`)).toBe(false)
      expect(lispish.run(`(string>= "albert" "alber")`)).toBe(true)
      expect(lispish.run(`(string>= "albert" "albert")`)).toBe(true)
      expect(lispish.run(`(string>= "alber" "albert")`)).toBe(false)
      expect(() => lispish.run(`(string>=)`)).toThrow()
      expect(() => lispish.run(`(string>= "a")`)).toThrow()
      expect(() => lispish.run(`(string>= "a", "A", "Q")`)).toThrow()
      expect(() => lispish.run(`(string>= 2 1)`)).toThrow()
      expect(() => lispish.run(`(string>= null null)`)).toThrow()
      expect(() => lispish.run(`(string>= undefined undefined)`)).toThrow()
      expect(() => lispish.run(`(string>= true true)`)).toThrow()
      expect(() => lispish.run(`(string>= false false)`)).toThrow()
      expect(() => lispish.run(`(string>= "a" true)`)).toThrow()
      expect(() => lispish.run(`(string>= true "a")`)).toThrow()
      expect(() => lispish.run(`(string>= '() "a")`)).toThrow()
      expect(() => lispish.run(`(string>= (object) "a")`)).toThrow()
    })
  })

  describe('string<=', () => {
    test('samples', () => {
      expect(lispish.run(`(string<= "albert" "ALBERT")`)).toBe(false)
      expect(lispish.run(`(string<= "ALBERT" "albert")`)).toBe(true)
      expect(lispish.run(`(string<= "albert" "alber")`)).toBe(false)
      expect(lispish.run(`(string<= "albert" "albert")`)).toBe(true)
      expect(lispish.run(`(string<= "alber" "albert")`)).toBe(true)
      expect(() => lispish.run(`(string<=)`)).toThrow()
      expect(() => lispish.run(`(string<= "a")`)).toThrow()
      expect(() => lispish.run(`(string<= "a", "A", "Q")`)).toThrow()
      expect(() => lispish.run(`(string<= 2 1)`)).toThrow()
      expect(() => lispish.run(`(string<= null null)`)).toThrow()
      expect(() => lispish.run(`(string<= undefined undefined)`)).toThrow()
      expect(() => lispish.run(`(string<= true true)`)).toThrow()
      expect(() => lispish.run(`(string<= false false)`)).toThrow()
      expect(() => lispish.run(`(string<= "a" true)`)).toThrow()
      expect(() => lispish.run(`(string<= true "a")`)).toThrow()
      expect(() => lispish.run(`(string<= '() "a")`)).toThrow()
      expect(() => lispish.run(`(string<= (object) "a")`)).toThrow()
    })
  })

  describe('string-reverse', () => {
    test('samples', () => {
      expect(lispish.run(`(string-reverse "albert")`)).toBe('trebla')
      expect(lispish.run(`(string-reverse "A 1")`)).toBe('1 A')
      expect(lispish.run(`(string-reverse "")`)).toBe('')
      expect(() => lispish.run(`(string-reverse)`)).toThrow()
      expect(() => lispish.run(`(string-reverse "word1" "word2")`)).toThrow()
    })
  })

  describe('string-to-number', () => {
    test('samples', () => {
      expect(lispish.run(`(string-to-number "123.25")`)).toBe(123.25)
      expect(lispish.run(`(string-to-number "0b1111")`)).toBe(15)
      expect(lispish.run(`(string-to-number "0Xf")`)).toBe(15)
      expect(lispish.run(`(string-to-number "0o17")`)).toBe(15)
      expect(lispish.run(`(string-to-number "-0.125")`)).toBe(-0.125)
      expect(() => lispish.run(`(string-to-number)`)).toThrow()
      expect(() => lispish.run(`(string-to-number "987" "65")`)).toThrow()
      expect(() => lispish.run(`(string-to-number "non parsable number")`)).toThrow()
    })
  })

  describe('number-to-string', () => {
    test('samples', () => {
      expect(lispish.run(`(number-to-string 10.25)`)).toBe('10.25')
      expect(lispish.run(`(number-to-string -11)`)).toBe('-11')
      expect(lispish.run(`(number-to-string 11 2)`)).toBe('1011')
      expect(lispish.run(`(number-to-string 11 8)`)).toBe('13')
      expect(lispish.run(`(number-to-string 11.11 10)`)).toBe('11.11')
      expect(() => lispish.run(`(number-to-string)`)).toThrow()
      expect(() => lispish.run(`(number-to-string -1 2)`)).toThrow()
      expect(() => lispish.run(`(number-to-string 1.5 2)`)).toThrow()
      expect(() => lispish.run(`(number-to-string 1.5 8)`)).toThrow()
      expect(() => lispish.run(`(number-to-string 1.5 16)`)).toThrow()
      expect(() => lispish.run(`(number-to-string -1 2)`)).toThrow()
      expect(() => lispish.run(`(number-to-string -1 8)`)).toThrow()
      expect(() => lispish.run(`(number-to-string -1 16)`)).toThrow()
      expect(() => lispish.run(`(number-to-string 10 7)`)).toThrow()
      expect(() => lispish.run(`(number-to-string 10 20)`)).toThrow()
    })
  })

  describe('lower-case', () => {
    test('samples', () => {
      expect(lispish.run(`(lower-case "Albert!")`)).toBe('albert!')
      expect(lispish.run(`(lower-case "")`)).toBe('')
      expect(() => lispish.run(`(lower-case)`)).toThrow()
      expect(() => lispish.run(`(lower-case "First" "Second")`)).toThrow()
    })
  })

  describe('upper-case', () => {
    test('samples', () => {
      expect(lispish.run(`(upper-case "Albert!")`)).toBe('ALBERT!')
      expect(lispish.run(`(upper-case "")`)).toBe('')
      expect(() => lispish.run(`(upper-case)`)).toThrow()
      expect(() => lispish.run(`(upper-case "First" "Second")`)).toThrow()
    })
  })

  describe('trim', () => {
    test('samples', () => {
      expect(lispish.run(`(trim "  Albert!  ")`)).toBe('Albert!')
      expect(lispish.run(`(trim " ")`)).toBe('')
      expect(lispish.run(`(trim "")`)).toBe('')
      expect(() => lispish.run(`(trim)`)).toThrow()
      expect(() => lispish.run(`(trim "First" "Second")`)).toThrow()
    })
  })

  describe('trim-left', () => {
    test('samples', () => {
      expect(lispish.run(`(trim-left "  Albert!  ")`)).toBe('Albert!  ')
      expect(lispish.run(`(trim-left " ")`)).toBe('')
      expect(lispish.run(`(trim-left "")`)).toBe('')
      expect(() => lispish.run(`(trim-left)`)).toThrow()
      expect(() => lispish.run(`(trim-left "First" "Second")`)).toThrow()
    })
  })

  describe('trim-right', () => {
    test('samples', () => {
      expect(lispish.run(`(trim-right "  Albert!  ")`)).toBe('  Albert!')
      expect(lispish.run(`(trim-right " ")`)).toBe('')
      expect(lispish.run(`(trim-right "")`)).toBe('')
      expect(() => lispish.run(`(trim-right)`)).toThrow()
      expect(() => lispish.run(`(trim-right "First" "Second")`)).toThrow()
    })
  })

  describe('pad-left', () => {
    test('samples', () => {
      expect(lispish.run(`(pad-left "Albert" 10)`)).toBe('    Albert')
      expect(lispish.run(`(pad-left "Albert" 10 "*")`)).toBe('****Albert')
      expect(lispish.run(`(pad-left "Albert" 10 "123")`)).toBe('1231Albert')
      expect(lispish.run(`(pad-left "Albert" 5)`)).toBe('Albert')
      expect(lispish.run(`(pad-left "Albert" -1)`)).toBe('Albert')
      expect(() => lispish.run(`(pad-left)`)).toThrow()
      expect(() => lispish.run(`(pad-left "First" "Second")`)).toThrow()
    })
  })

  describe('pad-right', () => {
    test('samples', () => {
      expect(lispish.run(`(pad-right "Albert" 10)`)).toBe('Albert    ')
      expect(lispish.run(`(pad-right "Albert" 10 "*")`)).toBe('Albert****')
      expect(lispish.run(`(pad-right "Albert" 10 "123")`)).toBe('Albert1231')
      expect(lispish.run(`(pad-right "Albert" 5)`)).toBe('Albert')
      expect(lispish.run(`(pad-right "Albert" -1)`)).toBe('Albert')
      expect(() => lispish.run(`(pad-right)`)).toThrow()
      expect(() => lispish.run(`(pad-right "First" "Second")`)).toThrow()
    })
  })

  describe('split', () => {
    test('samples', () => {
      expect(lispish.run(`(split "Albert Mojir" " ")`)).toEqual(['Albert', 'Mojir'])
      expect(lispish.run(`(split "0123456789" "")`)).toEqual(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
      expect(lispish.run(`(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))`)).toEqual([
        '',
        'bcd',
        'fgh',
        'jklmn',
        'pqrst',
        'vw',
      ])
      expect(lispish.run(`(map #'string-to-number (split "0123456789" "" 5))`)).toEqual([0, 1, 2, 3, 4])
      expect(() => lispish.run(`(split "0123456789")`)).toThrow()
      expect(() => lispish.run(`(split "0123456789" "5" -1)`)).toThrow()
      expect(() => lispish.run(`(split 23456789 "5")`)).toThrow()
    })
  })

  describe('string-repeat', () => {
    test('samples', () => {
      expect(lispish.run(`(string-repeat "*" 10)`)).toBe('**********')
      expect(lispish.run(`(string-repeat "*" 0)`)).toBe('')
      expect(lispish.run(`(string-repeat "Hello, " 3)`)).toBe('Hello, Hello, Hello, ')
      expect(() => lispish.run(`(string-repeat)`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, ")`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, " 3 3)`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, " "3")`)).toThrow()
      expect(() => lispish.run(`(string-repeat true, 1)`)).toThrow()
    })
  })
})
