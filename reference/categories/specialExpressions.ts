import type { SpecialExpressionsApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

export const specialExpressionsReference: Record<SpecialExpressionsApiName, FunctionReference<'Special expression'>> = {
  '&&': {
    title: '&&',
    category: 'Special expression',
    linkName: '-and-and',
    returns: {
      type: 'boolean',
    },
    args: {
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['expressions'] },
    ],
    description: `
Computes logical \`and\`. Evaluation of $expressions starts from left.
As soon as a \`expression\` evaluates to a falsy value, the result is returned.

If all expressions evaluate to truthy values, the value of the last expression is returned.`,
    examples: [
      '(&& 1 1)',
      '(&& (> 3 2) "string")',
      '(&& (< 3 2) "string")',
      '(&& true true true true)',
      '(&& true true 0 true)',
    ],
  },

  '||': {
    title: '||',
    category: 'Special expression',
    linkName: '-or-or',
    clojureDocs: 'or',
    returns: {
      type: 'boolean',
    },
    args: {
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['expressions'] },
    ],
    description: `
Computes logical \`or\`. Evaluation of $expressions evaluation starts from left.
As soon as a \`expression\` evaluates to a truthy value, the result is returned.

If all expressions evaluate to falsy values, the value of the last expression is returned.`,
    examples: [
      '(|| 1 1)',
      '(|| (> 3 2) "string")',
      '(|| (< 3 2) "string")',
      '(|| true true true true)',
      '(|| 1 2 3 4)',
    ],
  },
  'def': {
    title: 'def',
    category: 'Special expression',
    linkName: 'def',
    returns: {
      type: 'any',
    },
    args: {
      n: {
        type: '*name',
      },
      value: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['n', 'value'] },
    ],
    description: `Bind $value to variable $n.

If $n is already defined, an error is thrown.`,
    examples: [
      '(def a (object))',
      '(def a (object :x 10 :y true :z "A string"))',
    ],
  },
  'defs': {
    title: 'defs',
    category: 'Special expression',
    linkName: 'defs',
    clojureDocs: null,
    returns: {
      type: 'any',
    },
    args: {
      name: {
        type: '*expression',
      },
      value: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['name', 'value'] },
    ],
    description: `
Creates a variable with name set to $name evaluated and value set to $value.

If a variable with name $name is already defined, an error is thrown.`,
    examples: [
      '(defs :a :b)',
      `
(defs (str :a :1) (object :x 10 :y true :z "A string"))
a1`,
      `
(defs :a :b)
(defs a :c)
b`,
    ],
  },
  'let': {
    title: 'let',
    category: 'Special expression',
    linkName: 'let',
    returns: {
      type: 'any',
    },
    args: {
      bindings: {
        type: '*binding',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['bindings'] },
    ],
    description: `
Binds local variables.`,
    examples: [`
(let [a (+ 1 2 3 4) 
      b (* 1 2 3 4)])
(write! a b)`],
  },

  'fn': {
    title: 'fn',
    category: 'Special expression',
    linkName: 'fn',
    returns: {
      type: 'function',
    },
    args: {
      args: {
        type: '*arguments',
      },
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['args', 'expressions'] },
    ],
    description: 'Creates a function. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `
(fn [a b]
  (sqrt
    (+
      (* a a)
      (* b b))))`,
      `
(
  (fn [a b]
    (sqrt
      (+
        (* a a)
        (* b b))))
  3
  4)`,
    ],
  },
  'defn': {
    title: 'defn',
    category: 'Special expression',
    linkName: 'defn',
    returns: {
      type: 'function',
    },
    args: {
      n: {
        type: '*name',
      },
      args: {
        type: '*arguments',
      },
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['n', 'args', 'expressions'] },
    ],
    description: 'Creates a named global function. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `
(defn hyp [a b]
  (sqrt
    (+
      (* a a)
      (* b b))))
hyp`,
      `
(defn hyp [a b]
  (sqrt
    (+
      (* a a)
      (* b b))))
(hyp 3 4)`,
      `
(defn sumOfSquares [& s]
  (apply
    +
    (map
      (fn [x] (* x x))
      s)))
(sumOfSquares 1 2 3 4 5)`,
    ],
  },
  'defns': {
    title: 'defns',
    category: 'Special expression',
    linkName: 'defns',
    clojureDocs: null,
    returns: {
      type: 'function',
    },
    args: {
      name: {
        type: '*expression',
      },
      args: {
        type: '*arguments',
      },
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['name', 'args', 'expressions'] },
    ],
    description: 'Creates a named global function with its name set to $name evaluated. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `
(defns "hyp" [a b]
  (sqrt
    (+
      (* a a)
      (* b b))))
hyp`,
      `
(defns
  (str :h :y :p)
  [a b]
  (sqrt
    (+
      (* a a)
      (* b b))))
(hyp 3 4)`,
      `
(defns "sumOfSquares" [& s]
  (apply
    +
    (map
      (fn [x] (* x x))
      s)))
(sumOfSquares 1 2 3 4 5)`,
    ],
  },
  'try': {
    title: 'try',
    category: 'Special expression',
    linkName: 'try',
    clojureDocs: null,
    returns: {
      type: 'any',
    },
    args: {
      exp: {
        type: '*expression',
      },
      catch: {
        type: '*catch-expression',
      },
    },
    variants: [
      { argumentNames: ['exp', 'catch'] },
    ],
    description: 'Executes $exp. If that throws, the $catch `body` gets executed. See examples for details.',
    examples: [
      `
(try
  (/ 2 4)
  (catch error "Oops!"))`,
      `
(try
  (foo)
  (catch error "Oops!"))`,
      `
(try
  (foo)
  (catch error error))`,
    ],
  },
  'throw': {
    title: 'throw',
    category: 'Special expression',
    linkName: 'throw',
    clojureDocs: null,
    returns: {
      type: 'never',
    },
    args: {
      expr: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['expr'] },
    ],
    description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
    examples: [
      '(throw "You shall not pass!")',
      '(throw (subs "You shall not pass!" 0 3))',
    ],
  },
  'if': {
    title: 'if',
    category: 'Special expression',
    linkName: 'if',
    returns: {
      type: 'any',
    },
    args: {
      'test': {
        type: 'any',
      },
      'then-expr': {
        type: 'any',
      },
      'else-expr': {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['test', 'then-expr'] },
      { argumentNames: ['test', 'then-expr', 'else-expr'] },
    ],
    description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is truthy. If $test is falsy $else-expr is executed, if no $else-expr exists, `nil` is returned.',
    examples: [
      '(if true (write! "TRUE") (write! "FALSE"))',
      '(if false (write! "TRUE") (write! "FALSE"))',
      '(if true (write! "TRUE"))',
      '(if false (write! "TRUE"))',
    ],
  },
  'unless': {
    title: 'unless',
    category: 'Special expression',
    linkName: 'unless',
    clojureDocs: 'if-not',
    returns: {
      type: 'any',
    },
    args: {
      'test': {
        type: 'any',
      },
      'then-expr': {
        type: 'any',
      },
      'else-expr': {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['test', 'then-expr'] },
      { argumentNames: ['test', 'then-expr', 'else-expr'] },
    ],
    description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is falsy. If $test is truthy $else-expr is executed, if no $else-expr exists, `nil` is returned.',
    examples: [
      '(unless true (write! "TRUE") (write! "FALSE"))',
      '(unless false (write! "TRUE") (write! "FALSE"))',
      '(unless true (write! "TRUE"))',
      '(unless false (write! "TRUE"))',
    ],
  },
  'cond': {
    title: 'cond',
    category: 'Special expression',
    linkName: 'cond',
    returns: {
      type: 'any',
    },
    args: {
      conds: {
        type: '*conditions',
      },
    },
    variants: [
      { argumentNames: ['conds'] },
    ],
    description: 'Used for branching. $conds are tested sequentially from the top. If no branch is tested truthy, `nil` is returned.',
    examples: [
      `
(cond
  false (write! "FALSE")
  nil (write! "nil")
  :else (write! "TRUE"))`,
      `
(cond
  false (write! "FALSE")
  nil (write! "nil")
  true (write! "TRUE"))`,
      `
(cond
  false (write! "FALSE")
  nil (write! "nil"))`,
    ],
  },
  'switch': {
    title: 'switch',
    category: 'Special expression',
    linkName: 'switch',
    returns: {
      type: 'any',
    },
    args: {
      value: {
        type: 'any',
      },
      conds: {
        type: '*conditions',
      },
    },
    variants: [
      { argumentNames: ['value', 'conds'] },
    ],
    description: 'Used for branching. $conds are tested sequentially from the top against $value. If no branch is tested truthy, `nil` is returned.',
    examples: [
      `
(switch 1
  1 (write! "FALSE")
  2 (write! "nil"))`,
      `
(switch 2
  1 (write! "FALSE")
  2 (write! "nil"))`,
      `
(switch 3
  1 (write! "FALSE")
  2 (write! "nil"))`,
    ],
  },
  'comment': {
    title: 'comment',
    category: 'Special expression',
    linkName: 'comment',
    returns: {
      type: 'null',
    },
    args: {
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['expressions'] },
    ],
    description: 'All $expressions are read and must be valid `lits` but they are not eveluated. `nil` is returned.',
    examples: ['(comment (write! "Hi") (write! "Albert"))', '(comment)'],
  },
  'do': {
    title: 'do',
    category: 'Special expression',
    linkName: 'do',
    returns: {
      type: 'any',
    },
    args: {
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['expressions'] },
    ],
    description: 'Evaluates $expressions. Resulting value is the value of the last expression.',
    examples: [
      `
(do 
  (write! "Hi")
  (write! "Albert"))`,
      '(do)',
    ],
  },
  'recur': {
    title: 'recur',
    category: 'Special expression',
    linkName: 'recur',
    returns: {
      type: 'null',
    },
    args: {
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['expressions'] },
    ],
    description: 'Recursevly calls enclosing function or loop with its evaluated $expressions.',
    examples: [
      `
(defn foo [n]
  (write! n)
  (if (! (zero? n))
    (recur
      (dec n))))
(foo 3)`,
      `
(
  (fn [n]
    (write! n)
    (if (! (zero? n))
      (recur
        (dec n))))
  3)`,
      `
(
  loop [n 3]
    (write! n)
    (if
      (! (zero? n))
      (recur (dec n))))`,
    ],
  },
  'loop': {
    title: 'loop',
    category: 'Special expression',
    linkName: 'loop',
    returns: {
      type: 'any',
    },
    args: {
      bindings: {
        type: '*binding',
        rest: true,
      },
      expressions: {
        type: '*expression',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['bindings', 'expressions'] },
    ],
    description: 'Executes $expressions with initial $bindings. The $bindings will be replaced with the recur parameters for subsequent recursions.',
    examples: [
      `
(loop [n 3]
  (write! n)
  (if
    (! (zero? n))
    (recur (dec n))))`,
      `
(loop [n 3]
  (write! n)
  (if
    (! (zero? n))
    (recur (dec n))
    n))`,
    ],
  },

  'doseq': {
    title: 'doseq',
    category: 'Special expression',
    linkName: 'doseq',
    returns: {
      type: 'null',
    },
    args: {
      bindings: {
        type: '*for-binding',
        rest: true,
      },
      expr: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['vars', 'expr'] },
    ],
    description: 'Same syntax as `for`, but returns `nil`. Use for side effects. Consumes less memory than `for`.',
    examples: ['(doseq [x [1 2 4]] (write! x))'],
  },
  'for': {
    title: 'for',
    category: 'Special expression',
    linkName: 'for',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      bindings: {
        type: '*for-binding',
        rest: true,
      },
      expr: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['vars', 'expr'] },
    ],
    description: `List comprehension. Takes one or more $bindings, each followed by zero or more modifiers, and returns an array of evaluations of $expr.
  
  Collections are iterated in a nested fashion, rightmost fastest. Supported modifiers are: &let &while and &when.`,
    examples: [
      `
(for [x "Al" y [1 2]]
  (repeat y x))`,
      `
(for [x {:a 10 :b 20} y [1 2]]
  (repeat y x))`,
      `
(for [x [1 2] y [1 10]]
  (* x y))`,
      `
(for
  [x [1 2]
  &let [z (* x x x)]]
  
  z)`,
      `
(for
  [x [0 1 2 3 4 5]
  &let [y (* x 3)]
  &when (even? y)]
  
  y)`,
      `
(for
  [x [0 1 2 3 4 5]
  &let [y (* x 3)]
  &while (even? y)]
  
  y)`,
      `
(for
  [x [0 1 2 3 4 5]
  &let [y (* x 3)]
  &while (odd? y)]
  
  y)`,
      `
(for
  [x [1 2 3] y [1 2 3]
  &while (<= x y)
  z [1 2 3]]
  
  [x y z])`,
      `
(for
  [x [1 2 3] y [1 2 3] z [1 2 3]
  &while (<= x y)]
  
  [x y z])`,
    ],
  },
  'defined?': {
    title: 'defined?',
    category: 'Special expression',
    linkName: 'defined-question',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: '*name',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    description: 'Returns `true` if $n is a declared variable or a builtin function, otherwise `false`.',
    examples: [
      '(defined? foo)',
      `
(def foo :foo)
(defined? foo)`,
      '(defined? +)',
      `
(def foo nil)
(defined? foo)`,
      '(defined? if)',
    ],
  },
  '??': {
    title: '??',
    category: 'Special expression',
    linkName: '-question-question',
    returns: {
      type: 'any',
    },
    args: {
      test: {
        type: '*expression',
      },
      default: {
        type: '*expression',
      },
    },
    variants: [
      { argumentNames: ['test'] },
      { argumentNames: ['test', 'default'] },
    ],
    description: 'If $test is declared and evaluated to non `nil` value $test is result, else $default is returned. If $default is not provided, `nil` is returned.',
    examples: [
      '(?? foo)',
      `
(def foo :foo)
(?? foo)`,
      '(?? +)',
      `
(def foo nil)
(?? foo)`,
      `
(def foo nil)
(?? foo :bar)`,
      '(?? foo 1)',
      '(?? "")',
      '(?? 0)',
      '(?? 0 1)',
      '(?? 2 1)',
    ],
  },
}
