import { Lispish } from '../../../src'

let lispish: Lispish

beforeEach(() => {
  lispish = new Lispish()
})

describe(`string functions`, () => {
  describe(`substring`, () => {
    test(`samples`, () => {
      expect(() => lispish.run(`(substring "abcde")`)).toThrow()
      expect(lispish.run(`(substring "abcde" 0)`)).toBe(`abcde`)
      expect(lispish.run(`(substring "abcde" 1)`)).toBe(`bcde`)
      expect(lispish.run(`(substring "abcde" 2)`)).toBe(`cde`)
      expect(lispish.run(`(substring "abcde" 3)`)).toBe(`de`)
      expect(lispish.run(`(substring "abcde" 4)`)).toBe(`e`)
      expect(lispish.run(`(substring "abcde" 5)`)).toBe(``)
      expect(lispish.run(`(substring "abcde" 6)`)).toBe(``)
      expect(lispish.run(`(substring "abcde" 0 0)`)).toBe(``)
      expect(() => lispish.run(`(substring "abcde" 1 0)`)).toThrow()
      expect(lispish.run(`(substring "abcde" 1 1)`)).toBe(``)
      expect(lispish.run(`(substring "abcde" 1 2)`)).toBe(`b`)
      expect(lispish.run(`(substring "abcde" 1 3)`)).toBe(`bc`)
      expect(lispish.run(`(substring "abcde" 1 4)`)).toBe(`bcd`)
      expect(lispish.run(`(substring "abcde" 1 5)`)).toBe(`bcde`)
      expect(lispish.run(`(substring "abcde" 1 6)`)).toBe(`bcde`)
    })
  })

  describe(`str`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(str)`)).toBe(``)
      expect(lispish.run(`(str "")`)).toBe(``)
      expect(lispish.run(`(str "1")`)).toBe(`1`)
      expect(lispish.run(`(str "1" "2")`)).toBe(`12`)
      expect(lispish.run(`(str "1" "2" "three" "4")`)).toBe(`12three4`)
      expect(lispish.run(`(str 0)`)).toBe(`0`)
      expect(lispish.run(`(str true)`)).toBe(`true`)
      expect(lispish.run(`(str "1" false)`)).toBe(`1false`)
      expect(lispish.run(`(str null "m")`)).toBe(`nullm`)
      expect(lispish.run(`(str undefined)`)).toBe(``)
      expect(lispish.run(`(str [])`)).toBe(`[]`)
      expect(lispish.run(`(str [1 2 3])`)).toBe(`[1,2,3]`)
      expect(lispish.run(`(str {})`)).toBe(`{}`)
      expect(lispish.run(`(str {"a" 1})`)).toBe(`{"a":1}`)
    })
  })

  describe(`string>`, () => {
    test(`samples`, () => {
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
      expect(() => lispish.run(`(string> [] "a")`)).toThrow()
      expect(() => lispish.run(`(string> (object) "a")`)).toThrow()
    })
  })

  describe(`string<`, () => {
    test(`samples`, () => {
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
      expect(() => lispish.run(`(string< [] "a")`)).toThrow()
      expect(() => lispish.run(`(string< (object) "a")`)).toThrow()
    })
  })

  describe(`string>=`, () => {
    test(`samples`, () => {
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
      expect(() => lispish.run(`(string>= [] "a")`)).toThrow()
      expect(() => lispish.run(`(string>= (object) "a")`)).toThrow()
    })
  })

  describe(`string<=`, () => {
    test(`samples`, () => {
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
      expect(() => lispish.run(`(string<= [] "a")`)).toThrow()
      expect(() => lispish.run(`(string<= (object) "a")`)).toThrow()
    })
  })

  describe(`string-to-number`, () => {
    test(`samples`, () => {
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

  describe(`number-to-string`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(number-to-string 10.25)`)).toBe(`10.25`)
      expect(lispish.run(`(number-to-string -11)`)).toBe(`-11`)
      expect(lispish.run(`(number-to-string 11 2)`)).toBe(`1011`)
      expect(lispish.run(`(number-to-string 11 8)`)).toBe(`13`)
      expect(lispish.run(`(number-to-string 11.11 10)`)).toBe(`11.11`)
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

  describe(`lower-case`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(lower-case "Albert!")`)).toBe(`albert!`)
      expect(lispish.run(`(lower-case "")`)).toBe(``)
      expect(() => lispish.run(`(lower-case)`)).toThrow()
      expect(() => lispish.run(`(lower-case "First" "Second")`)).toThrow()
    })
  })

  describe(`upper-case`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(upper-case "Albert!")`)).toBe(`ALBERT!`)
      expect(lispish.run(`(upper-case "")`)).toBe(``)
      expect(() => lispish.run(`(upper-case)`)).toThrow()
      expect(() => lispish.run(`(upper-case "First" "Second")`)).toThrow()
    })
  })

  describe(`trim`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(trim "  Albert!  ")`)).toBe(`Albert!`)
      expect(lispish.run(`(trim " ")`)).toBe(``)
      expect(lispish.run(`(trim "")`)).toBe(``)
      expect(() => lispish.run(`(trim)`)).toThrow()
      expect(() => lispish.run(`(trim "First" "Second")`)).toThrow()
    })
  })

  describe(`trim-left`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(trim-left "  Albert!  ")`)).toBe(`Albert!  `)
      expect(lispish.run(`(trim-left " ")`)).toBe(``)
      expect(lispish.run(`(trim-left "")`)).toBe(``)
      expect(() => lispish.run(`(trim-left)`)).toThrow()
      expect(() => lispish.run(`(trim-left "First" "Second")`)).toThrow()
    })
  })

  describe(`trim-right`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(trim-right "  Albert!  ")`)).toBe(`  Albert!`)
      expect(lispish.run(`(trim-right " ")`)).toBe(``)
      expect(lispish.run(`(trim-right "")`)).toBe(``)
      expect(() => lispish.run(`(trim-right)`)).toThrow()
      expect(() => lispish.run(`(trim-right "First" "Second")`)).toThrow()
    })
  })

  describe(`pad-left`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(pad-left "Albert" 10)`)).toBe(`    Albert`)
      expect(lispish.run(`(pad-left "Albert" 10 "*")`)).toBe(`****Albert`)
      expect(lispish.run(`(pad-left "Albert" 10 "123")`)).toBe(`1231Albert`)
      expect(lispish.run(`(pad-left "Albert" 5)`)).toBe(`Albert`)
      expect(lispish.run(`(pad-left "Albert" -1)`)).toBe(`Albert`)
      expect(() => lispish.run(`(pad-left)`)).toThrow()
      expect(() => lispish.run(`(pad-left "First" "Second")`)).toThrow()
    })
  })

  describe(`pad-right`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(pad-right "Albert" 10)`)).toBe(`Albert    `)
      expect(lispish.run(`(pad-right "Albert" 10 "*")`)).toBe(`Albert****`)
      expect(lispish.run(`(pad-right "Albert" 10 "123")`)).toBe(`Albert1231`)
      expect(lispish.run(`(pad-right "Albert" 5)`)).toBe(`Albert`)
      expect(lispish.run(`(pad-right "Albert" -1)`)).toBe(`Albert`)
      expect(() => lispish.run(`(pad-right)`)).toThrow()
      expect(() => lispish.run(`(pad-right "First" "Second")`)).toThrow()
    })
  })

  describe(`split`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(split "Albert Mojir" " ")`)).toEqual([`Albert`, `Mojir`])
      expect(lispish.run(`(split "0123456789" "")`)).toEqual([`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`])
      expect(lispish.run(`(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))`)).toEqual([
        ``,
        `bcd`,
        `fgh`,
        `jklmn`,
        `pqrst`,
        `vw`,
      ])
      expect(lispish.run(`(map string-to-number (split "0123456789" "" 5))`)).toEqual([0, 1, 2, 3, 4])
      expect(() => lispish.run(`(split "0123456789")`)).toThrow()
      expect(() => lispish.run(`(split "0123456789" "5" -1)`)).toThrow()
      expect(() => lispish.run(`(split 23456789 "5")`)).toThrow()
    })
  })

  describe(`string-repeat`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(string-repeat "*" 10)`)).toBe(`**********`)
      expect(lispish.run(`(string-repeat "*" 0)`)).toBe(``)
      expect(lispish.run(`(string-repeat "Hello, " 3)`)).toBe(`Hello, Hello, Hello, `)
      expect(() => lispish.run(`(string-repeat)`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, ")`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, " 3 3)`)).toThrow()
      expect(() => lispish.run(`(string-repeat "Hello, " "3")`)).toThrow()
      expect(() => lispish.run(`(string-repeat true, 1)`)).toThrow()
    })
  })

  describe(`template`, () => {
    test(`samples`, () => {
      expect(lispish.run(`(template "Hi")`)).toBe(`Hi`)
      expect(lispish.run(`(template "Hi" "Carl")`)).toBe(`Hi`)
      expect(lispish.run(`(template "Hi, $1", "Carl")`)).toBe(`Hi, Carl`)
      expect(lispish.run(`(template "Hi, $1" "Carl")`)).toBe(`Hi, Carl`)
      expect(lispish.run(`(template "Hi, $1" "Carl" "Larry")`)).toBe(`Hi, Carl`)
      expect(lispish.run(`(template "Hi, $1 and $2" "Carl" "Larry")`)).toBe(`Hi, Carl and Larry`)
      expect(lispish.run(`(template "Hi, $1 and $3" "Carl" "Larry" "Sofi")`)).toBe(`Hi, Carl and Sofi`)
      expect(
        lispish.run(`(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" "A" "B" "C" "D" "E" "F" "G" "H" "I")`),
      ).toBe(`Hi A, B, C, D, E, F, G, H and I`)
      expect(() =>
        lispish.run(`(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" "A" "B" "C" "D" "E" "F" "G" "H")`),
      ).toThrow()
      expect(
        lispish.run(`(template "Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 and $10" "A" "B" "C" "D" "E" "F" "G" "H" "I")`),
      ).toBe(`Hi A, B, C, D, E, F, G, H, I and A0`)
      expect(() =>
        lispish.run(`(template "Hi $1, $2, $3, $4, $5, $6, $7, $8, $9 $10" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J")`),
      ).toThrow()
      expect(() => lispish.run(`(template)`)).toThrow()
      expect(() => lispish.run(`(template "$1", 0)`)).toThrow()
      expect(() => lispish.run(`(template "$1", true)`)).toThrow()
      expect(() => lispish.run(`(template "$1", false)`)).toThrow()
      expect(() => lispish.run(`(template "$1", null)`)).toThrow()
      expect(() => lispish.run(`(template "$1", undefined)`)).toThrow()
      expect(() => lispish.run(`(template "$1", [])`)).toThrow()
      expect(() => lispish.run(`(template "$1", (object))`)).toThrow()
      expect(() => lispish.run(`(template true)`)).toThrow()
      expect(() => lispish.run(`(template false)`)).toThrow()
      expect(() => lispish.run(`(template null)`)).toThrow()
      expect(() => lispish.run(`(template undefined)`)).toThrow()
      expect(() => lispish.run(`(template 1)`)).toThrow()
      expect(() => lispish.run(`(template []`)).toThrow()
      expect(() => lispish.run(`(template (object))`)).toThrow()
    })
    test(`Pluralization samples`, () => {
      expect(lispish.run(`(template "$1 book||||$1 books" 0)`)).toBe(`0 books`)
      expect(lispish.run(`(template "$1 book||||$1 books" 1)`)).toBe(`1 book`)
      expect(lispish.run(`(template "$1 book||||$1 books" 2)`)).toBe(`2 books`)
      expect(() => lispish.run(`(template "$1 book||||$1 books")`)).toThrow()
      expect(() => lispish.run(`(template "$1 book||||$1 books" "1")`)).toThrow()
      expect(() => lispish.run(`(template "$1 book||||$1 books||||$1 chairs" )`)).toThrow()
      expect(lispish.run(`(template "$2 got $1 book||||$2 got $1 books" 1 "Carl")`)).toBe(`Carl got 1 book`)
      expect(lispish.run(`(template "$2 got $1 book||||$2 got $1 books" 2 "Carl")`)).toBe(`Carl got 2 books`)
    })
  })
})
