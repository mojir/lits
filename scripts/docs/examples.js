module.exports = [
  {
    id: 'default',
    name: 'Simple Lisp expression',
    description: 'A super simple example.',
    code: `
(+ 10 20)
    `.trim(),
  },
  {
    id: 'simple-params-example',
    name: 'Params in use',
    description: 'Simple example using params.',
    params: { globals: { x: 15, y: 97 } },
    code: `
(+ x y)
    `.trim(),
  },
  {
    id: 'phone-number-formatter',
    name: 'Phone number formatter',
    description: 'Pretty prints a US phone number.',
    code: `
(defn formatPhoneNumber [$data]
  (if (string? $data)
    (let [phoneNumber (if (= "+" (nth $data 0)) (subs $data 2) $data)]
      (cond
        (> (count phoneNumber) 6)
          (str
            "("
            (subs phoneNumber 0 3)
            ") "
            (subs phoneNumber 3 6)
            "-"
            (subs phoneNumber 6))

        (> (count phoneNumber) 3)
          (str "(" (subs phoneNumber 0 3) ") " (subs phoneNumber 3))

        (> (count phoneNumber) 0)
          (str "(" (subs phoneNumber 0))

        true
          phoneNumber
      )
    )
    ""
  )
)

(write! formatPhoneNumber)
(write! (formatPhoneNumber 123234))
(write! (formatPhoneNumber "123234"))
(write! (formatPhoneNumber "1232343456"))
(write! (formatPhoneNumber "+11232343456789"))
(write! (formatPhoneNumber "+11232343456"))
`.trim(),
  },
  {
    id: 'factorial',
    name: 'Factorial',
    description: 'A recursive implementation of the factorial function.',
    code: `
(defn factorial [x]
  (if (= x 1)
    1
    (* x (factorial (dec x)))
  )
)

(factorial 4)
`.trim(),
  },
  {
    id: 'sort',
    name: 'Sort',
    description: 'Sort an array of numbers.',
    code: `
(def l [7 39 45 0 23 1 50 100 12 -5])
(defn numberComparer [a b]
  (cond
    (< a b) -1
    (> a b) 1
    true 0
  )
)

(sort numberComparer l)
    `.trim(),
  },
  {
    id: 'multiple-arity',
    name: 'Many arities',
    description: 'Function with multiple arities.',
    code: `
(defn foo
  ([] "No parameters")
  ([x] "One parameter")
  ([x y] "Two parameters")
  ([x y z] "Three parameters")
  ([x y z å & rest] "Four or more parameters")
)

(write! (foo))
(write! (foo 1))
(write! (foo 1 2))
(write! (foo 1 2 3))
(write! (foo 1 2 3 4))
(write! (foo 1 2 3 4 5))
`.trim(),
  },
  {
    id: 'translations-lib',
    name: 'Translations lib',
    description: 'A Lits take on i18n.',
    params: {
      globals: {
        TRANSLATIONS: {
          'welcome-message': 'Welcome, $1',
          'count-chairs': '$1 chair||||$1 chairs',
        },
      },
    },
    code: `
(loop [list (entries TRANSLATIONS)]
  (when (count list)
    (let [entry (first list)]
      (defns (entry 0) [& params &let [templateString (entry 1)]]
        (apply template (cons templateString params))
      )
      (recur (rest list))
    )
  )
)

(write! (welcome-message "Albert"))
(write! (count-chairs 12))
(write! (count-chairs 1))
`.trim(),
  },

  {
    id: 'isoDateString',
    name: 'Is ISO date string',
    description: 'Check if string is formatted as an ISO date string.',
    code: `
(defn isoDateString? [$data]
  (if-let [m (match (regexp "^(\\d{4})-(\\d{2})-(\\d{2})$") $data)]
    (let
      [
        year (number (m 1))
        month (number (m 2))
        day (number (m 3))
        leapYear
          (and
            (zero? (mod year 4))
            (or
              (not (zero? (mod year 100)))
              (zero? (mod year 400))
            )
          )
      ]
      (not (or
        (or (< year 1900) (> year 2100))
        (or (< month 1) (> month 12))
        (or (< day 1) (> day 31))
        (and
          (or (= month 4) (= month 6) (= month 9) (= month 11))
          (> day 30)
        )
        (and
          (= month 2)
          (or
            (and leapYear (> day 29))
            (and (not leapYear) (> day 28))
          )
        )
      ))
    )
    false
  )
)

(write! (isoDateString? "1978-12-21"))
(write! (isoDateString? "197-12-21"))
`.trim(),
  },

  {
    id: 'label-from-value',
    name: 'label-from-value',
    description: 'Find label to corresponding value in array of {label value}-objects.',
    code: `
(defn label-from-value [$array $value]
  (let [entry (some #(= $value (%1 :value)) $array)]
    (if (nil? entry) (str $value) (entry :label))
  )
)

(def arr [
  {:label "Name" :value "name"}
  {:label "Age" :value "age"}
])

(label-from-value arr :name)
`.trim(),
  },

  {
    id: 'labels-from-values',
    name: 'labels-from-values',
    description: 'Find labels to corresponding values in array of {label value}-objects.',
    code: `
(defn labels-from-values [$array $values]
  (for
    [
      value
      $values
      &let [
        label
        (let [entry (some #(= value (%1 :value)) $array)]
          (if (nil? entry) (str value) (entry :label))
        )
      ]
      &when (boolean label)
    ]
    label
  )
)

(def arr [
  {:label "Name" :value "name"}
  {:label "Age" :value "age"}
  {:label "Email" :value "email"}
])

(labels-from-values arr [:name :age])
`.trim(),
  },
]
