import { type FunctionalApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '../index.ts'

export const functionalReference: Record<FunctionalApiName, FunctionReference<'Functional'>> = {
  'apply': {
    title: 'apply',
    category: 'Functional',
    linkName: 'apply',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('function', 'array'),
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
apply(+, [1, 2, 3])`,
      `
apply(
  (x, y) -> sqrt(x ** 2 + y ** 2),
  [3, 4]
)`,
      `
(x, y) -> sqrt(x ** 2 + y ** 2) apply [3, 4]`,
    ],
    algebraic: true,
  },
  'identity': {
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
    examples: ['identity(1)', 'identity("Albert")', 'identity({ a := 1 })', 'identity(null)'],
    algebraic: true,
  },
  'partial': {
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
      'partial(+, 100)',
      `
let plusMany := partial(+, 100, 1000);
plusMany(1, 10)`,
      `
let addHundred := partial(+, 100);
addHundred(10)`,
    ],
    noOperatorDocumentation: true,
    algebraic: true,
  },
  'comp': {
    title: 'comp',
    category: 'Functional',
    linkName: 'comp',
    returns: {
      type: 'function',
    },
    args: {
      ...getOperatorArgs('function', 'function'),
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
let negative-quotient := comp(-, /);
negative-quotient(9, 3)`,
      `
let x := { bar := { foo := 42 } };
comp("foo", "bar")(x)`,
    ],
    algebraic: true,
  },
  'constantly': {
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
let always-true := constantly(true);
always-true(9, 3)`,
    ],
    algebraic: true,
  },

  'juxt': {
    title: 'juxt',
    category: 'Functional',
    linkName: 'juxt',
    returns: {
      type: 'function',
    },
    args: {
      ...getOperatorArgs('function', 'function'),
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
juxt(+, *, min, max)(
  3,
  4,
  6,
)`,
      `
juxt("a", "b")(
  {
    a := 1,
    b := 2,
    c := 3,
    d := 4
  }
)`,
      `
juxt(+, *, min, max) apply range(1, 11)`,
    ],
    algebraic: true,
  },
  'complement': {
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
    examples: [
      'complement(>)(1, 3)',
      'complement(<)(1, 3)',
      'complement(+)(1, 3)',
      'complement(+)(0, 0)',
    ],
    algebraic: true,
  },
  'every-pred': {
    title: 'every-pred',
    category: 'Functional',
    linkName: 'every-pred',
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
every-pred(string?, -> count($) > 3)(
  "Albert",
  "Mojir"
)`,
      `
(string? every-pred -> count($) > 3)(
  "Albert",
  "M"
)`,
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'some-pred': {
    title: 'some-pred',
    category: 'Functional',
    linkName: 'some-pred',
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
      'some-pred(string?, -> count($) > 3)("Albert", "Mojir")',
      'some-pred(string?, -> count($) > 3)("a", "M")',
      'some-pred(string?, -> count($) > 3)("a", [1, 2, 3])',
      'some-pred(string?, -> count($) > 3)([1, 2, 3], [2])',
    ],
    algebraic: true,
    noOperatorDocumentation: true,
  },
  'fnull': {
    title: 'fnull',
    category: 'Functional',
    linkName: 'fnull',
    returns: {
      type: 'function',
    },
    args: {
      ...getOperatorArgs('function', 'any'),
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
    description: 'Takes a function $fn, and returns a function that calls $fn, replacing a null argument to the corresponding argument.',
    examples: [
      'fnull(inc, 0)(1)',
      'fnull(inc, 0)(null)',
      '(inc fnull 0)(null)',
      'fnull(+, 1, 2)(null, 0)',
      'fnull(+, 1, 2)(0, null)',
      'fnull(+, 1, 2)(null, null)',
      'fnull(+, 1, 2)(null, null, 3, 4)',
    ],
    algebraic: true,
  },
}
