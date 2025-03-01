import type { FunctionalApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

export const functionalReference: Record<FunctionalApiName, FunctionReference<'Functional'>> = {
  apply: {
    title: 'apply',
    category: 'Functional',
    linkName: 'apply',
    returns: {
      type: 'any',
    },
    args: {
      fn: {
        type: 'function',
      },
      args: {
        type: 'array',
      },
    },
    variants: [
      { argumentNames: ['fn', 'args'] },
    ],
    description: 'Call supplied function $fn with specified arguments $args.',
    examples: [
      `
(apply + [1 2 3])`,
      `
(apply
  (fn [x y]
    (sqrt
      (+
        (* x x)
        (* y y))))
  [3 4])`,
    ],
  },
  identity: {
    title: 'identity',
    category: 'Functional',
    linkName: 'identity',
    returns: {
      type: 'any',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns $x.',
    examples: ['(identity 1)', '(identity "Albert")', '(identity {:a 1})', '(identity nil)'],
  },
  partial: {
    title: 'partial',
    category: 'Functional',
    linkName: 'partial',
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
      args: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fn', 'args'] },
    ],
    description: `Takes a function $fn and a optional number arguments $args to $fn.
It returns a function that takes the additional additional arguments.
When called, the returned function calls \`(\`$fn \`...\`$args\` ...additional_arguments)\`.`,
    examples: [
      '(partial + 100)',
      `
(def plusMany (partial + 100 1000))
(plusMany 1 10)`,
      `
(def addHundred (partial + 100))
(addHundred 10)`,
    ],
  },
  comp: {
    title: 'comp',
    category: 'Functional',
    linkName: 'comp',
    returns: {
      type: 'function',
    },
    args: {
      fns: {
        type: 'function',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fns'] },
    ],
    description: `Takes a variable number of functions and returns a function that is the composition of those.

  The returned function takes a variable number of arguments,
  applies the rightmost function to the args,
  the next function (right-to-left) to the result, etc.`,
    examples: [
      `
(def negative-quotient (comp - /))
(negative-quotient 9 3)`,
      `
(
  #((apply comp first (repeat %2 rest)) %1)
  [1 2 3 4 5 6 7]
  3)`,
      `
(def x {"bar" {"foo" 42}})
((comp "foo" "bar") x)`,
    ],
  },
  constantly: {
    title: 'constantly',
    category: 'Functional',
    linkName: 'constantly',
    returns: {
      type: 'function',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns a function that takes any number of arguments and always returns $x.',
    examples: [
      `
(def always-true (constantly true))
(always-true 9 3)`,
      `
(
  #((apply constantly first (repeat %2 rest)) %1)
  [1 2 3 4 5 6 7]
  3)`,
    ],
  },

  juxt: {
    title: 'juxt',
    category: 'Functional',
    linkName: 'juxt',
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
      fns: {
        type: 'function',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fn'] },
      { argumentNames: ['fn', 'fns'] },
    ],
    description: `Takes one or many function and returns a function that is the juxtaposition of those functions.  
The returned function takes a variable number of args,
and returns a vector containing the result of applying each function to the args (left-to-right).`,
    examples: [
      `
(
  (juxt + * min max)
  3
  4
  6)`,
      `
(
  (juxt :a :b)
  {:a 1, :b 2, :c 3, :d 4})`,
      `
(apply
  (juxt + * min max)
  (range 1 11))`,
    ],
  },
  complement: {
    title: 'complement',
    category: 'Functional',
    linkName: 'complement',
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['fn'] },
    ],
    description: 'Takes a function $fn and returns a new function that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.',
    examples: ['((complement >) 1 3)', '((complement <) 1 3)', '((complement +) 1 3)', '((complement +) 0 0)'],
  },
  every_pred: {
    title: 'every_pred',
    category: 'Functional',
    linkName: 'every_pred',
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
      fns: {
        type: 'function',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fn'] },
      { argumentNames: ['fn', 'fns'] },
    ],
    description: `
Takes a number of predicates and returns a function that returns \`true\` if all predicates
return a truthy value against all of its arguments, else it returns \`false\`.`,
    examples: [
      `
(
  (every_pred string? #(> (count %1) 3))
  "Albert"
  "Mojir")`,
      `
(
  (every_pred string? #(> (count %1) 3))
  "Albert"
  :M)`,
      `
(
  (every_pred string? #(> (count %1) 3))
  "Albert"
  [1 2 3])`,
    ],
  },
  some_pred: {
    title: 'some_pred',
    category: 'Functional',
    linkName: 'some_pred',
    clojureDocs: null,
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
      fns: {
        type: 'function',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fn'] },
      { argumentNames: ['fn', 'fns'] },
    ],
    description: 'Takes a number of `predicates` and returns a function that returns \`true\` if at least one of the `predicates` return a truthy \`true\` value against at least one of its arguments, else it returns `false`.',
    examples: [
      '((some_pred string? #(> (count %1) 3)) "Albert" "Mojir")',
      '((some_pred string? #(> (count %1) 3)) :A :M)',
      '((some_pred string? #(> (count %1) 3)) :A [1 2 3])',
      '((some_pred string? #(> (count %1) 3)) [1 2 3] [2])',
    ],
  },
  fnil: {
    title: 'fnil',
    category: 'Functional',
    linkName: 'fnil',
    returns: {
      type: 'function',
    },
    args: {
      fn: {
        type: 'function',
      },
      arg: {
        type: 'any',
      },
      args: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['fn', 'arg'] },
      { argumentNames: ['fn', 'arg', 'args'] },
    ],
    description: 'Takes a function $fn, and returns a function that calls $fn, replacing a nil argument to the corresponding argument.',
    examples: [
      '((fnil + 1 2) 0 0)',
      '((fnil + 1 2) nil 0)',
      '((fnil + 1 2) 0 nil)',
      '((fnil + 1 2) nil nil)',
      '((fnil + 1 2) nil nil 3 4)',
    ],
  },
}
