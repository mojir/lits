import { type SpecialExpressionsApiName, getOperatorArgs } from '../api'
import type { CustomReference, FunctionReference } from '..'

export const specialExpressionsReference: Record<SpecialExpressionsApiName, FunctionReference<'Special expression'> | CustomReference<'Special expression'>> = {
  'array': {
    title: 'array',
    category: 'Special expression',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['values'] },
    ],
    description: 'Makes new array from $values.',
    examples: [
      'array(1, 2, 3)',
      'array(array(null, false, true))',
      '[]',
      '[1, 2, 3]',
      '[1, 2, ...[3, 4, 5], 6]',
      '[[null, false, true]]',
      '[1, 2, 3][1]',
    ],
    noOperatorDocumentation: true,
  },
  'object': {
    title: 'object',
    category: 'Special expression',
    returns: {
      type: 'object',
    },
    args: {
      kvps: {
        type: 'any',
        rest: true,
        description: 'key - value pairs, where key is a string',
      },
    },
    variants: [
      { argumentNames: ['kvps'] },
    ],
    description: 'Constructs a new object. Object members are created from the $kvps key-value pairs. Requires an even number of arguments.',
    examples: [
      'object()',
      `
let default = {
  type: "Person",
  name: "John Doe",
  age: 42
};

{
  ...default,
  name: "Lisa"
}`,
      'object("x", 10, "y", true, "z", "A string")',
      '{}',
      '{ a: 1, b: 2 }',
    ],
    noOperatorDocumentation: true,
  },
  '&&': {
    title: '&&',
    category: 'Special expression',
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
    customVariants: ['let s = value;'],
    details: [
      ['s', 'symbol', 'The name of the variable to bind.'],
      ['value', 'any', 'The value to bind to the variable.'],
    ],
    description: `
  Binds local variables s to \`value\`. \`value\` can be any expression. The scope of the variables is the body of the let expression.`,
    examples: [`
let a = 1 + 2 + 3 + 4;
let b = -> $ * ( $ + 1 );
write!("a", a, "b", b)`],
  },
  'try': {
    title: 'try',
    category: 'Special expression',
    customVariants: ['try { try-body } catch { catch-body }', 'try { try-body } catch(error) { catch-body }'],
    details: [
      ['try-body', 'expressions', 'The expressions to try.'],
      ['error', 'symbol', 'The error variable to bind.'],
      ['catch-body', 'expression', 'The expressions to evaluate if the try-body throws an error.'],
    ],
    description: 'Executes `try-body`. If that throws, the `catch-body` gets executed. See examples for details.',
    examples: [
      `
try {
  2 / 4
} catch {
  "Oops!"
}`,
      `
try {
  foo()
} catch(error) {
  "Error: " ++ error.message
}`,
      `
try {
  foo()
} catch {
  42
}`,
    ],
  },
  'throw': {
    title: 'throw',
    category: 'Special expression',
    returns: {
      type: 'never',
    },
    args: {
      expr: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['expr'] },
    ],
    description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
    examples: [
      'try { throw("You shall not pass!") } catch(error) { "Error: " ++ error.message }',
      'try { throw(slice("You shall not pass!", 0, 3)) } catch(error) { "Error: " ++ error.message }',
    ],
  },
  'if': {
    title: 'if',
    category: 'Special expression',
    customVariants: ['if test then true-expr else false-expr', 'if test then true-expr'],
    details: [
      ['test', 'expression', 'The condition to test.'],
      ['true-expr', 'expression', 'The expression to evaluate if the test is truthy.'],
      ['false-expr', 'expression', 'The expression to evaluate if the test is falsy.'],
    ],
    description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is truthy. If $test is falsy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
    examples: [
      `
if true then {
  write!("TRUE")
} else {
  write!("FALSE")
}`,
      'if false then write!("TRUE") else write!("FALSE")',
      'if true then write!("TRUE")',
      'if false then write!("TRUE")',
    ],
  },
  'unless': {
    title: 'unless',
    category: 'Special expression',
    customVariants: ['unless test then true-expr else false-expr end', 'unless test true-expr end'],
    details: [
      ['test', 'expression', 'The condition to test.'],
      ['true-expr', 'expression', 'The expressions to evaluate if the test is falsy.'],
      ['false-expr', 'expression', 'The expressions to evaluate if the test is truthy.'],
    ],
    description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is falsy. If $test is truthy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
    examples: [
      `
unless true then {
  write!("TRUE")
} else {
  write!("FALSE")
}`,
      'unless false then write!("TRUE") else write!("FALSE")',
      'unless true then write!("TRUE")',
      'unless false then write!("TRUE")',
    ],
  },
  'cond': {
    title: 'cond',
    category: 'Special expression',
    customVariants: ['cond cond-branch cond-branch ... end'],
    details: [
      ['cond-branch', 'case test then body', 'A branch of the cond expression.'],
      ['test', 'expression', 'The condition to test.'],
      ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
    ],
    description: 'Used for branching. `cond-branches` are tested sequentially from the top. If no branch is tested truthy, `null` is returned.',
    examples: [
      `
cond
  case false then write!("FALSE")
  case true then write!("TRUE")
end`,
      `
cond
  case false then write!("FALSE")
  case null then write!("null")
end ?? write!("TRUE")`,
      `
cond
  case false then write!("FALSE")
  case null then write!("null")
end ?? write!("TRUE")`,
    ],
  },
  'switch': {
    title: 'switch',
    category: 'Special expression',
    customVariants: ['switch value switch-branch switch-branch ... end'],
    details: [
      ['value', 'any', 'The value to test.'],
      ['switch-branch', 'case test then body', 'A branch of the switch expression.'],
      ['test', 'expression', 'The condition to test.'],
      ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
    ],
    description: 'Used for branching. `switch-branches` are tested sequentially from the top against `value`. If no branch is tested truthy, `null` is returned.',
    examples: [
      `
switch 1
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
      `
switch 2
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
      `
switch 3
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
    ],
  },
  'block': {
    title: 'block',
    category: 'Special expression',
    customVariants: ['{ body }'],
    details: [
      ['body', 'expressions', 'The expressions to evaluate.'],
    ],
    description: 'Evaluates `body`. Resulting value is the value of the last expression.',
    examples: [
      `
{
  let a = 1 + 2 + 3 + 4;
  let b = -> $ * ( $ + 1 );
  b(a)
}`,
    ],
  },
  'recur': {
    title: 'recur',
    category: 'Special expression',
    customVariants: ['recur(...recur-args)'],
    description: 'Recursevly calls enclosing function or loop with its evaluated `recur-args`.',
    examples: [
      `
let foo = (n) -> {
  write!(n);
  if !(zero?(n)) then {
    recur(n - 1)
  }
};
foo(3)`,
      `
(n -> {
  write!(n);
  if !(zero?(n)) then {
    recur(n - 1)
  }
})(3)`,
      `
loop (n = 3) {
  write!(n);
  if !(zero?(n)) then {
    recur(n - 1)
  }
}`,
    ],
  },
}
