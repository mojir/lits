import type { FunctionReference } from '..'
import type { PredicateApiName } from '../api'

export const predicateReference: Record<PredicateApiName, FunctionReference<'Predicate'>> = {
  'boolean?': {
    title: 'boolean?',
    category: 'Predicate',
    linkName: 'boolean-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a `boolean`, otherwise `false`.',
    examples: [
      '(boolean? true)',
      '(boolean? false)',
      '(boolean? [1 2 3])',
      '(boolean? 0)',
      '(boolean? "A string")',
    ],
  },
  'null?': {
    title: 'null?',
    category: 'Predicate',
    linkName: 'null-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is `null`, otherwise `false`.',
    examples: [
      '(null? null)',
      '(null? false)',
      '(null? [1 2 3])',
      '(null? 0)',
      '(null? "A string")',
    ],
  },
  'number?': {
    title: 'number?',
    category: 'Predicate',
    linkName: 'number-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a number, otherwise `false`.',
    examples: [
      '(number? 0)',
      '(number? 2)',
      '(number? -0.12)',
      '(number? false)',
      '(number? [1 2 3])',
      '(number? "A string")',
    ],
  },
  'string?': {
    title: 'string?',
    category: 'Predicate',
    linkName: 'string-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a string, otherwise `false`.',
    examples: [
      '(string? "")',
      '(string? "A string")',
      '(string? (if true "A string" false))',
      '(string? false)',
      '(string? [1 2 3])',
      '(string? 100)',
    ],
  },
  'function?': {
    title: 'function?',
    category: 'Predicate',
    linkName: 'function-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a function, otherwise `false`.',
    examples: [
      '(function? +)',
      '(function? /)',
      '(function? (fn [x y] (+ x y)))',
      '(function? false)',
      '(function? "false")',
      '(function? [1 2 3])',
    ],
  },
  'integer?': {
    title: 'integer?',
    category: 'Predicate',
    linkName: 'integer-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is an integer, otherwise `false`.',
    examples: [
      '(integer? 0)',
      '(integer? -12)',
      '(integer? 42)',
      '(integer? 10.1)',
      '(integer? (fn [x y] (+ x y)))',
      '(integer? false)',
      '(integer? "false")',
      '(integer? [1 2 3])',
    ],
  },
  'array?': {
    title: 'array?',
    category: 'Predicate',
    linkName: 'array-question',
    clojureDocs: 'vector_q',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is an array, otherwise `false`.',
    examples: [
      '(array? [])',
      '(array? [1 2 3])',
      '(array? (object :a 10))',
      '(array? 42)',
      '(array? 10.1)',
      '(array? (fn [x y] (+ x y)))',
    ],
  },
  'object?': {
    title: 'object?',
    category: 'Predicate',
    linkName: 'object-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is an object, otherwise `false`.',
    examples: [
      '(object? (object :a 10))',
      '(object? (object))',
      '(object? 42)',
      '(object? 10.1)',
      '(object? (fn [x y] (+ x y)))',
      '(object? #"^start")',
      '(object? "false")',
      '(object? [1 2 3])',
    ],
  },
  'coll?': {
    title: 'coll?',
    category: 'Predicate',
    linkName: 'coll-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a Coll i.e. an array, an object or a string, otherwise `false`.',
    examples: [
      '(coll? [])',
      '(coll? [1 2 3])',
      '(coll? (object :a 10))',
      '(coll? "Albert")',
      '(coll? 42)',
      '(coll? 10.1)',
      '(coll? (fn [x y] (+ x y)))',
    ],
  },
  'seq?': {
    title: 'seq?',
    category: 'Predicate',
    linkName: 'seq-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a Seq i.e. an array or a string, otherwise `false`.',
    examples: [
      '(seq? [])',
      '(seq? [1 2 3])',
      '(seq? (object :a 10))',
      '(seq? "Albert")',
      '(seq? 42)',
      '(seq? 10.1)',
      '(seq? (fn [x y] (+ x y)))',
    ],
  },
  'regexp?': {
    title: 'regexp?',
    category: 'Predicate',
    linkName: 'regexp-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is a regexp, otherwise `false`.',
    examples: [
      '(regexp? (regexp "^start"))',
      '(regexp? #"^start")',
      '(regexp? -12)',
      '(regexp? (object))',
      '(regexp? 10.1)',
      '(regexp? (fn [x y] (+ x y)))',
      '(regexp? false)',
      '(regexp? "false")',
      '(regexp? [1 2 3])',
    ],
  },
  'zero?': {
    title: 'zero?',
    category: 'Predicate',
    linkName: 'zero-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is `0`, otherwise `false`.',
    examples: [
      '(zero? 0)',
      '(zero? -0.0)',
      '(zero? 1)',
      '(zero? 0.1)',
    ],
  },
  'pos?': {
    title: 'pos?',
    category: 'Predicate',
    linkName: 'pos-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is greater than `0`, otherwise `false`.',
    examples: [
      '(pos? 0)',
      '(pos? -0.0)',
      '(pos? 1)',
      '(pos? -0.1)',
    ],
  },
  'neg?': {
    title: 'neg?',
    category: 'Predicate',
    linkName: 'neg-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is less than `0`, otherwise `false`.',
    examples: [
      '(neg? 0)',
      '(neg? -0.0)',
      '(neg? 1)',
      '(neg? -0.1)',
    ],
  },
  'even?': {
    title: 'even?',
    category: 'Predicate',
    linkName: 'even-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is even, otherwise `false`.',
    examples: [
      '(even? 0)',
      '(even? -0.0)',
      '(even? -1)',
      '(even? 2.1)',
    ],
  },
  'odd?': {
    title: 'odd?',
    category: 'Predicate',
    linkName: 'odd-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is odd, otherwise `false`.',
    examples: [
      '(odd? 1.0)',
      '(odd? 1.001)',
      '(odd? -1)',
      '(odd? 2.1)',
    ],
  },
  'finite?': {
    title: 'finite?',
    category: 'Predicate',
    linkName: 'finite-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is finite, otherwise `false`.',
    examples: [
      '(finite? 1.0)',
      '(finite? (/ 1 0))',
      '(finite? (/ -1 0))',
      '(finite? (sqrt -1))',
    ],
  },
  'nan?': {
    title: 'nan?',
    category: 'Predicate',
    linkName: 'nan-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is NaN (! a number), otherwise `false`.',
    examples: [
      '(nan? 1.0)',
      '(nan? (/ 1 0))',
      '(nan? (/ -1 0))',
      '(nan? (sqrt -1))',
    ],
  },
  'negative_infinity?': {
    title: 'negative_infinity?',
    category: 'Predicate',
    linkName: 'negative_infinity-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x equals negative infinity, otherwise `false`.',
    examples: [
      '(negative_infinity? 1.0)',
      '(negative_infinity? (/ 1 0))',
      '(negative_infinity? (/ -1 0))',
      '(negative_infinity? (sqrt -1))',
    ],
  },
  'positive_infinity?': {
    title: 'positive_infinity?',
    category: 'Predicate',
    linkName: 'positive_infinity-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x equals positive infinity, otherwise `false`.',
    examples: [
      '(positive_infinity? 1.0)',
      '(positive_infinity? (/ 1 0))',
      '(positive_infinity? (/ -1 0))',
      '(positive_infinity? (sqrt -1))',
    ],
  },
  'false?': {
    title: 'false?',
    category: 'Predicate',
    linkName: 'false-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is `true`, otherwise `false`.',
    examples: [
      '(false? false)',
      '(false? true)',
      '(false? 1)',
      '(false? 0)',
    ],
  },
  'true?': {
    title: 'true?',
    category: 'Predicate',
    linkName: 'true-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is `true`, otherwise `false`.',
    examples: [
      '(true? false)',
      '(true? true)',
      '(true? 1)',
      '(true? 0)',
    ],
  },
  'empty?': {
    title: 'empty?',
    category: 'Predicate',
    linkName: 'empty-question',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: ['collection', 'string', 'null'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `true` if $x is empty or `null`, otherwise `false`.',
    examples: [
      '(empty? [])',
      '(empty? [1 2 3])',
      '(empty? {})',
      '(empty? {:a 2})',
      '(empty? "")',
      '(empty? "Albert")',
      '(empty? null)',
    ],
  },
  'not_empty?': {
    title: 'not_empty?',
    category: 'Predicate',
    linkName: 'not_empty-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: ['collection', 'string', 'null'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `false` if $x is empty or `null`, otherwise `true`.',
    examples: [
      '(not_empty? [])',
      '(not_empty? [1 2 3])',
      '(not_empty? {})',
      '(not_empty? {:a 2})',
      '(not_empty? "")',
      '(not_empty? "Albert")',
      '(not_empty? null)',
    ],
  },
}
