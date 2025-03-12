export interface Example {
  id: string
  name: string
  description: string
  code: string
  context?: {
    values?: Record<string, unknown>
    jsFunctions?: Record<string, string>
  }
}

export const examples: Example[] = [
  {
    id: 'default',
    name: 'Simple Lisp expression',
    description: 'A super simple example.',
    code: `
10 + 20
    `.trim(),
  },
  {
    id: 'collection-accessor',
    name: 'Collection accessors',
    description: 'Syntactic sugar for accessing object, array and string elements.',
    code: `
// Access object properies with .
// Access string and array elements with []

let data := {
  numbers := [1, 2, 3],
  chars := ["a", "b", "c"],
  string := "Albert"
};

write!(data.numbers[0]);
write!(data.chars[2]);
write!(data.string[0]);

write!({a := 1, b := 2, c := 3}.b);
write!("Albert"[3]);
write!([1, 2, 3][2]);
    `.trim(),
  },
  {
    id: 'simple-context-example',
    name: 'Using context',
    description: 'Simple example using a context.',
    context: {
      values: { x: 15, y: 27 },
      jsFunctions: { plus: '(a, b) => a + b' },
    },
    code: `
  plus(x, y)
      `.trim(),
  },
  //   {
  //     id: 'phone-number-formatter',
  //     name: 'Phone number formatter',
  //     description: 'Pretty prints a US phone number.',
  //     code: `
  // (defn formatPhoneNumber [$data]
  //   (if (string? $data)
  //     (do
  //       (let [phoneNumber
  //             (if (= "+" (nth $data 0))
  //               (slice $data 2)
  //               $data)])
  //       (cond
  //         (> (count phoneNumber) 6)
  //           (str
  //             "("
  //             (slice phoneNumber 0 3)
  //             ") "
  //             (slice phoneNumber 3 6)
  //             "-"
  //             (slice phoneNumber 6))

  //         (> (count phoneNumber) 3)
  //           (str
  //             "("
  //             (slice phoneNumber 0 3)
  //             ") "
  //             (slice phoneNumber 3))

  //         (> (count phoneNumber) 0)
  //           (str "(" (slice phoneNumber 0))

  //         true
  //           phoneNumber
  //       )
  //     )
  //     ""
  //   )
  // )

  // (write! formatPhoneNumber)
  // (write! (formatPhoneNumber 123234))
  // (write! (formatPhoneNumber "123234"))
  // (write! (formatPhoneNumber "1232343456"))
  // (write! (formatPhoneNumber "+11232343456789"))
  // (write! (formatPhoneNumber "+11232343456"))
  // `.trim(),
  //   },
  //   {
  //     id: 'factorial',
  //     name: 'Factorial',
  //     description: 'A recursive implementation of the factorial function.',
  //     code: `
  // (defn factorial [x]
  //   (if (= x 1)
  //     1
  //     (* x (factorial (dec x)))
  //   )
  // )

  // (factorial 4)
  // `.trim(),
  //   },
  //   {
  //     id: 'sort',
  //     name: 'Sort',
  //     description: 'Sort an array of numbers.',
  //     code: `
  // (def l [7 39 45 0 23 1 50 100 12 -5])
  // (defn numberComparer [a b]
  //   (cond
  //     (< a b) -1
  //     (> a b) 1
  //     :else 0
  //   )
  // )

  // (sort l numberComparer)
  //     `.trim(),
  //   },
  //   {
  //     id: 'multiple-arity',
  //     name: 'Many arities',
  //     description: 'Function with multiple arities.',
  //     code: `
  // (defn foo
  //   ([] "No parameters")
  //   ([x] "One parameter")
  //   ([x y] "Two parameters")
  //   ([x y z] "Three parameters")
  //   ([x y z zz &rest rest] "Four or more parameters")
  // )

  // (write! (foo))
  // (write! (foo 1))
  // (write! (foo 1 2))
  // (write! (foo 1 2 3))
  // (write! (foo 1 2 3 4))
  // (write! (foo 1 2 3 4 5))
  // `.trim(),
  //   },
  //   {
  //     id: 'isoDateString',
  //     name: 'Is ISO date string',
  //     description: 'Check if string is formatted as an ISO date string.',
  //     code: `
  // (defn isoDateString? [$data]
  //   (let [m
  //            (match
  //              $data
  //              #"^(\\d{4})-(\\d{2})-(\\d{2})$")])
  //   (if m
  //     (do
  //       (let
  //         [
  //           year (number (m 1))
  //           month (number (m 2))
  //           day (number (m 3))
  //           leapYear
  //             (&&
  //               (zero? (mod year 4))
  //               (||
  //                 (! (zero? (mod year 100)))
  //                 (zero? (mod year 400))
  //               )
  //             )
  //         ])
  //       (! (||
  //         (|| (< year 1900) (> year 2100))
  //         (|| (< month 1) (> month 12))
  //         (|| (< day 1) (> day 31))
  //         (&&
  //           (||
  //             (= month 4)
  //             (= month 6)
  //             (= month 9)
  //             (= month 11))
  //           (> day 30)
  //         )
  //         (&&
  //           (= month 2)
  //           (||
  //             (&& leapYear (> day 29))
  //             (&& (! leapYear) (> day 28))
  //           )
  //         )
  //       ))
  //     )
  //     false
  //   )
  // )

  // (write! (isoDateString? "1978-12-21"))
  // (write! (isoDateString? "197-12-21"))
  // `.trim(),
  //   },

  //   {
  //     id: 'label-from-value',
  //     name: 'label-from-value',
  //     description: 'Find label to corresponding value in array of {label value}-objects.',
  //     code: `
  // (defn label-from-value [$array $value]
  //   (let [entry (some $array #(= $value (%1 :value)))])
  //   (if (null? entry) (str $value) (entry :label))
  // )

  // (def arr [
  //   {:label "Name" :value "name"}
  //   {:label "Age" :value "age"}
  // ])

  // (label-from-value arr :name)
  // `.trim(),
  //   },

  //   {
  //     id: 'labels-from-values',
  //     name: 'labels-from-values',
  //     description: 'Find labels to corresponding values in array of {label value}-objects.',
  //     code: `
  // (defn labels-from-values [$array $values]
  //   (for
  //     [
  //       value
  //       $values
  //       &let [
  //         label
  //         (do
  //           (let [entry
  //                  (some
  //                    $array
  //                    #(= value (%1 :value)))])
  //           (if
  //             (null? entry)
  //             (str value)
  //             (entry :label))
  //         )
  //       ]
  //       &when (boolean label)
  //     ]
  //     label
  //   )
  // )

  // (def arr [
  //   {:label "Name" :value "name"}
  //   {:label "Age" :value "age"}
  //   {:label "Email" :value "email"}
  // ])

// (labels-from-values arr [:name :age])
// `.trim(),
//   },
]
