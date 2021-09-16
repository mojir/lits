var examples = [
  {
    id: 'default',
    name: 'Simple Lisp expression',
    description: 'A super simple example.',
    code: `
(+ 10 20)
    `.trim(),
  },
  {
    id: 'simple-context-example',
    name: 'Context in use',
    description: 'Simple example using context.',
    context: { x: 15, y: 97 },
    code: `
(+ x y)
    `.trim(),
  },
  {
    id: 'phone-number-formatter',
    name: 'Phone number formatter',
    description: 'Pretty prints a US phone number.',
    code: `
(defun formatPhoneNumber ($data)
  (if (string? $data)
    (let ((phoneNumber (if (= "+" (aref $data 0)) (substring $data 2) $data)))
      (cond
        ((> (string-length phoneNumber) 6)
          (concat
            "("
            (substring phoneNumber 0 3)
            ") "
            (substring phoneNumber 3 6)
            "-"
            (substring phoneNumber 6))
        )
        ((> (string-length phoneNumber) 3)
          (concat "(" (substring phoneNumber 0 3) ") " (substring phoneNumber 3))
        )
        ((> (string-length phoneNumber) 0)
          (concat "(" (substring phoneNumber 0))
        )
        (true
          phoneNumber
        )
      )
    )
    ""
  )
)

(write #'formatPhoneNumber)
(write (formatPhoneNumber "+11232343456"))
(write (formatPhoneNumber "1232343456"))
(write (formatPhoneNumber "123234"))
`.trim(),
  },
  {
    id: 'factorial',
    name: 'Factorial',
    description: 'A recursive implementation of the factorial function.',
    code: `
(defun factorial (x)
  (if (= x 1)
    1
    (* x (factorial (1- x)))
  )
)
  
(factorial 4) 
`.trim(),
  },
]
