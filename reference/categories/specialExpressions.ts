import { type SpecialExpressionsApiName, getOperatorArgs } from '../api'
import type { CustomReference, FunctionReference } from '..'

export const specialExpressionsReference: Record<SpecialExpressionsApiName, FunctionReference<'Special expression'> | CustomReference<'Special expression'>> = {
  '&&': {
    title: '&&',
    category: 'Special expression',
    linkName: '-and-and',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
      c: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: `
Computes logical \`and\`. Evaluation of expressions starts from left.
As soon as an \`expression\` evaluates to a falsy value, the result is returned.

If all expressions evaluate to truthy values, the value of the last expression is returned.`,
    examples: [
      'true && 1',
      '&&(1, 1)',
      '&&(3 > 2, "string")',
      '&&(3 < 2, "string")',
      '&&(true, true, true, true)',
      '&&(true, true, 0, true)',
    ],
  },

  '||': {
    title: '||',
    category: 'Special expression',
    linkName: '-or-or',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('any', 'any'),
      c: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'c'] },
    ],
    description: `
  Computes logical \`or\`. Evaluation of expressions evaluation starts from left.
  As soon as a \`expression\` evaluates to a truthy value, the result is returned.

  If all expressions evaluate to falsy values, the value of the last expression is returned.`,
    examples: [
      'false || 1',
      '||(1, 1)',
      '||(3 > 2, "string")',
      '||(3 < 2, "string")',
      '||(false, false, false, true)',
      '||(1, 2, 3, 4)',
    ],
  },
  'let': {
    title: 'let',
    category: 'Special expression',
    linkName: 'let',
    customVariants: ['let s := value;'],
    details: [
      ['s', 'symbol', 'The name of the variable to bind.'],
      ['value', 'any', 'The value to bind to the variable.'],
    ],
    description: `
  Binds local variables s to \`value\`. \`value\` can be any expression. The scope of the variables is the body of the let expression.`,
    examples: [`
let a := 1 + 2 + 3 + 4;
let b := -> $ * ( $ + 1 );
write!("a", a, "b", b)`],
  },
  'function': {
    title: 'function',
    category: 'Special expression',
    linkName: 'function',
    customVariants: ['function name(args [, let-bindings]) body end;'],
    details: [
      ['name', 'symbol', 'The name of the function.'],
      ['args', 'symbol', 'The arguments of the function.'],
      ['let-bindings', 'symbol', 'Optional. The let bindings of the function.'],
      ['body', 'one or more expressions', 'The body of the function.'],
    ],
    description: 'Creates a named function. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `
function hyp (a, b)
  sqrt(a * a + b * b)
end;

hyp(3, 4)`,
      `
function sumOfSquares(...s)
  apply(
    +,
    map(s, -> $ ** 2)
  )
end;

sumOfSquares(1, 2, 3, 4, 5)`,
      `
// binding variables is sometimes useful due to dynamic scoping.
// withBindings can be exported, and used in other files and the bound variables will be available.
let a := 1;
let b := 1;
function withBindings(let x := a, let b := b)
  x + b
end;

withBindings()`,

    ],
  },
  'try': {
    title: 'try',
    category: 'Special expression',
    linkName: 'try',
    customVariants: ['try try-body catch catch-body end', 'try try-body catch(error) catch-body end'],
    details: [
      ['try-body', 'expressions', 'The expressions to try.'],
      ['error', 'symbol', 'The error variable to bind.'],
      ['catch-body', 'expression', 'The expressions to evaluate if the try-body throws an error.'],
    ],
    description: 'Executes `try-body`. If that throws, the `catch-body` gets executed. See examples for details.',
    examples: [
      `
try
  2 / 4
catch
  "Oops!"
end`,
      `
try
  foo()
catch(error)
  "Error: " ++ error.message
end`,
      `
try
  foo()
catch
  42
end`,
    ],
  },
  //   'throw': {
  //     title: 'throw',
  //     category: 'Special expression',
  //     linkName: 'throw',
  //   //     returns: {
  //       type: 'never',
  //     },
  //     args: {
  //       expr: {
  //         type: '*expression',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['expr'] },
  //     ],
  //     description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
  //     examples: [
  //       '(throw "You shall not pass!")',
  //       '(throw (slice "You shall not pass!" 0 3))',
  //     ],
  //   },
  //   'if': {
  //     title: 'if',
  //     category: 'Special expression',
  //     linkName: 'if',
  //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       'test': {
  //         type: 'any',
  //       },
  //       'then-expr': {
  //         type: 'any',
  //       },
  //       'else-expr': {
  //         type: 'any',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['test', 'then-expr'] },
  //       { argumentNames: ['test', 'then-expr', 'else-expr'] },
  //     ],
  //     description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is truthy. If $test is falsy $else-expr is executed, if no $else-expr exists, `null` is returned.',
  //     examples: [
  //       '(if true (write! "TRUE") (write! "FALSE"))',
  //       '(if false (write! "TRUE") (write! "FALSE"))',
  //       '(if true (write! "TRUE"))',
  //       '(if false (write! "TRUE"))',
  //     ],
  //   },
  //   'unless': {
  //     title: 'unless',
  //     category: 'Special expression',
  //     linkName: 'unless',
  //   //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       'test': {
  //         type: 'any',
  //       },
  //       'then-expr': {
  //         type: 'any',
  //       },
  //       'else-expr': {
  //         type: 'any',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['test', 'then-expr'] },
  //       { argumentNames: ['test', 'then-expr', 'else-expr'] },
  //     ],
  //     description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is falsy. If $test is truthy $else-expr is executed, if no $else-expr exists, `null` is returned.',
  //     examples: [
  //       '(unless true (write! "TRUE") (write! "FALSE"))',
  //       '(unless false (write! "TRUE") (write! "FALSE"))',
  //       '(unless true (write! "TRUE"))',
  //       '(unless false (write! "TRUE"))',
  //     ],
  //   },
  //   'cond': {
  //     title: 'cond',
  //     category: 'Special expression',
  //     linkName: 'cond',
  //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       conds: {
  //         type: '*conditions',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['conds'] },
  //     ],
  //     description: 'Used for branching. $conds are tested sequentially from the top. If no branch is tested truthy, `null` is returned.',
  //     examples: [
  //       `
  // (cond
  //   false (write! "FALSE")
  //   null (write! "null")
  //   :else (write! "TRUE"))`,
  //       `
  // (cond
  //   false (write! "FALSE")
  //   null (write! "null")
  //   true (write! "TRUE"))`,
  //       `
  // (cond
  //   false (write! "FALSE")
  //   null (write! "null"))`,
  //     ],
  //   },
  //   'switch': {
  //     title: 'switch',
  //     category: 'Special expression',
  //     linkName: 'switch',
  //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       value: {
  //         type: 'any',
  //       },
  //       conds: {
  //         type: '*conditions',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['value', 'conds'] },
  //     ],
  //     description: 'Used for branching. $conds are tested sequentially from the top against $value. If no branch is tested truthy, `null` is returned.',
  //     examples: [
  //       `
  // (switch 1
  //   1 (write! "FALSE")
  //   2 (write! "null"))`,
  //       `
  // (switch 2
  //   1 (write! "FALSE")
  //   2 (write! "null"))`,
  //       `
  // (switch 3
  //   1 (write! "FALSE")
  //   2 (write! "null"))`,
  //     ],
  //   },
  //   'do': {
  //     title: 'do',
  //     category: 'Special expression',
  //     linkName: 'do',
  //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       expressions: {
  //         type: '*expression',
  //         rest: true,
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['expressions'] },
  //     ],
  //     description: 'Evaluates $expressions. Resulting value is the value of the last expression.',
  //     examples: [
  //       `
  // (do
  //   (write! "Hi")
  //   (write! "Albert"))`,
  //       '(do)',
  //     ],
  //   },
  //   'recur': {
  //     title: 'recur',
  //     category: 'Special expression',
  //     linkName: 'recur',
  //     returns: {
  //       type: 'null',
  //     },
  //     args: {
  //       expressions: {
  //         type: '*expression',
  //         rest: true,
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['expressions'] },
  //     ],
  //     description: 'Recursevly calls enclosing function or loop with its evaluated $expressions.',
  //     examples: [
  //       `
  // (defn foo [n]
  //   (write! n)
  //   (if (! (zero? n))
  //     (recur
  //       (dec n))))
  // (foo 3)`,
  //       `
  // (
  //   (fun [n]
  //     (write! n)
  //     (if (! (zero? n))
  //       (recur
  //         (dec n))))
  //   3)`,
  //       `
  // (
  //   loop [n 3]
  //     (write! n)
  //     (if
  //       (! (zero? n))
  //       (recur (dec n))))`,
  //     ],
  //   },
  //   'loop': {
  //     title: 'loop',
  //     category: 'Special expression',
  //     linkName: 'loop',
  //     returns: {
  //       type: 'any',
  //     },
  //     args: {
  //       bindings: {
  //         type: '*binding',
  //         rest: true,
  //       },
  //       expressions: {
  //         type: '*expression',
  //         rest: true,
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['bindings', 'expressions'] },
  //     ],
  //     description: 'Executes $expressions with initial $bindings. The $bindings will be replaced with the recur parameters for subsequent recursions.',
  //     examples: [
  //       `
  // (loop [n 3]
  //   (write! n)
  //   (if
  //     (! (zero? n))
  //     (recur (dec n))))`,
  //       `
  // (loop [n 3]
  //   (write! n)
  //   (if
  //     (! (zero? n))
  //     (recur (dec n))
  //     n))`,
  //     ],
  //   },

  //   'doseq': {
  //     title: 'doseq',
  //     category: 'Special expression',
  //     linkName: 'doseq',
  //     returns: {
  //       type: 'null',
  //     },
  //     args: {
  //       bindings: {
  //         type: '*for-binding',
  //         rest: true,
  //       },
  //       expr: {
  //         type: '*expression',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['vars', 'expr'] },
  //     ],
  //     description: 'Same syntax as `for`, but returns `null`. Use for side effects. Consumes less memory than `for`.',
  //     examples: ['(doseq [x [1 2 4]] (write! x))'],
  //   },
  //   'for': {
  //     title: 'for',
  //     category: 'Special expression',
  //     linkName: 'for',
  //     returns: {
  //       type: 'any',
  //       array: true,
  //     },
  //     args: {
  //       bindings: {
  //         type: '*for-binding',
  //         rest: true,
  //       },
  //       expr: {
  //         type: '*expression',
  //       },
  //     },
  //     variants: [
  //       { argumentNames: ['vars', 'expr'] },
  //     ],
  //     description: `List comprehension. Takes one or more $bindings, each followed by zero or more modifiers, and returns an array of evaluations of $expr.

  //   Collections are iterated in a nested fashion, rightmost fastest. Supported modifiers are: &let &while and &when.`,
  //     examples: [
  //       `
  // (for [x "Al" y [1 2]]
  //   (repeat y x))`,
  //       `
  // (for [x {:a 10 :b 20} y [1 2]]
  //   (repeat y x))`,
  //       `
  // (for [x [1 2] y [1 10]]
  //   (* x y))`,
  //       `
  // (for
  //   [x [1 2]
  //   &let [z (* x x x)]]

  //   z)`,
  //       `
  // (for
  //   [x [0 1 2 3 4 5]
  //   &let [y (* x 3)]
  //   &when (even? y)]

  //   y)`,
  //       `
  // (for
  //   [x [0 1 2 3 4 5]
  //   &let [y (* x 3)]
  //   &while (even? y)]

  //   y)`,
  //       `
  // (for
  //   [x [0 1 2 3 4 5]
  //   &let [y (* x 3)]
  //   &while (odd? y)]

  //   y)`,
  //       `
  // (for
  //   [x [1 2 3] y [1 2 3]
  //   &while (<= x y)
  //   z [1 2 3]]

  //   [x y z])`,
  //       `
  // (for
  //   [x [1 2 3] y [1 2 3] z [1 2 3]
  //   &while (<= x y)]

//   [x y z])`,
//     ],
//   },
//   'defined?': {
//     title: 'defined?',
//     category: 'Special expression',
//     linkName: 'defined-question',
//     returns: {
//       type: 'boolean',
//     },
//     args: {
//       n: {
//         type: '*name',
//       },
//     },
//     variants: [
//       { argumentNames: ['n'] },
//     ],
//     description: 'Returns `true` if $n is a declared variable or a builtin function, otherwise `false`.',
//     examples: [
//       '(defined? foo)',
//       `
// (def foo :foo)
// (defined? foo)`,
//       '(defined? +)',
//       `
// (def foo null)
// (defined? foo)`,
//       '(defined? if)',
//     ],
//   },
//   '??': {
//     title: '??',
//     category: 'Special expression',
//     linkName: '-question-question',
//     returns: {
//       type: 'any',
//     },
//     args: {
//       test: {
//         type: '*expression',
//       },
//       default: {
//         type: '*expression',
//       },
//     },
//     variants: [
//       { argumentNames: ['test'] },
//       { argumentNames: ['test', 'default'] },
//     ],
//     description: 'If $test is declared and evaluated to non `null` value $test is result, else $default is returned. If $default is not provided, `null` is returned.',
//     examples: [
//       '(?? foo)',
//       `
// (def foo :foo)
// (?? foo)`,
//       '(?? +)',
//       `
// (def foo null)
// (?? foo)`,
//       `
// (def foo null)
// (?? foo :bar)`,
//       '(?? foo 1)',
//       '(?? "")',
//       '(?? 0)',
//       '(?? 0 1)',
//       '(?? 2 1)',
//     ],
//   },
}
