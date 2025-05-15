import { type SpecialExpressionsApiName, getOperatorArgs } from '../api'
import type { CustomReference, FunctionReference } from '..'

export const specialExpressionsReference: Record<SpecialExpressionsApiName, FunctionReference<'Special expression'> | CustomReference<'Special expression'>> = {
  'array': {
    title: 'array',
    category: 'Special expression',
    linkName: 'array',
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
    linkName: 'object',
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
let default := {
  type := "Person",
  name := "John Doe",
  age := 42
};

{
  ...default,
  name := "Lisa"
}`,
      'object("x", 10, "y", true, "z", "A string")',
      '{}',
      '{ a := 1, b := 2 }',
    ],
    noOperatorDocumentation: true,
  },
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
    customVariants: ['function name(...arg, ...let-binding) body end;'],
    details: [
      ['name', 'symbol', 'The name of the function.'],
      ['arg', '[...]arg-name [:= value]', 'Arguments of the function.'],
      ['...', 'rest-symbol', 'Optional. The rest argument of the function.'],
      ['arg-name', 'symbol', 'The name of the argument.'],
      ['value', 'any', 'Optional. The default value of the argument.'],
      ['let-binding', 'symbol', 'Optional. The let bindings of the function.'],
      ['body', 'one or more expressions', 'The body of the function.'],
    ],
    description: 'Creates a named function. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `
function hyp (a, b) {
  sqrt(a * a + b * b)
};

hyp(3, 4)`,
      `
function sumOfSquares(...s) {
  apply(
    +,
    map(s, -> $ ^ 2)
  )
};

sumOfSquares(1, 2, 3, 4, 5)`,
      `
function withOptional(a, b := 42) {
  a + b
};

write!(withOptional(1), withOptional(1, 2))`,
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
    linkName: 'throw',
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
    linkName: 'if',
    customVariants: ['if test then-body else else-body end', 'if test then-body end'],
    details: [
      ['test', 'expression', 'The condition to test.'],
      ['then-body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
      ['else-body', 'expressions', 'The expressions to evaluate if the test is falsy.'],
    ],
    description: 'Either `then-expr` or `else-expr` branch is taken. `then-expr` is selected when $test is truthy. If $test is falsy `else-expr` is executed, if no `else-expr` exists, `null` is returned.',
    examples: [
      `
if (true) {
  write!("TRUE")
} else {
  write!("FALSE")
}`,
      'if (false) { write!("TRUE") } else { write!("FALSE") }',
      'if (true) { write!("TRUE") }',
      'if (false) { write!("TRUE") }',
    ],
  },
  'unless': {
    title: 'unless',
    category: 'Special expression',
    linkName: 'unless',
    customVariants: ['unless test then-body else else-body end', 'unless test then-body end'],
    details: [
      ['test', 'expression', 'The condition to test.'],
      ['then-body', 'expressions', 'The expressions to evaluate if the test is falsy.'],
      ['else-body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
    ],
    description: 'Either `then-expr` or `else-expr` branch is taken. `then-expr` is selected when $test is falsy. If $test is truthy `else-expr` is executed, if no `else-expr` exists, `null` is returned.',
    examples: [
      `
unless (true) {
  write!("TRUE")
} else {
  write!("FALSE")
}`,
      'unless (false) { write!("TRUE") } else { write!("FALSE") }',
      'unless (true) { write!("TRUE") }',
      'unless (false) { write!("TRUE") }',
    ],
  },
  'cond': {
    title: 'cond',
    category: 'Special expression',
    linkName: 'cond',
    customVariants: ['cond cond-branch cond-branch ... end'],
    details: [
      ['cond-branch', 'case test then body', 'A branch of the cond expression.'],
      ['test', 'expression', 'The condition to test.'],
      ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
    ],
    description: 'Used for branching. `cond-branches` are tested sequentially from the top. If no branch is tested truthy, `null` is returned.',
    examples: [
      `
cond {
  case false then write!("FALSE")
  case true then write!("TRUE")
}`,
      `
cond {
  case false then write!("FALSE")
  case null then write!("null")
} ?? write!("TRUE")`,
      `
cond {
  case false then write!("FALSE")
  case null then write!("null")
} ?? write!("TRUE")`,
    ],
  },
  'switch': {
    title: 'switch',
    category: 'Special expression',
    linkName: 'switch',
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
switch (1) {
  case 1 then write!("One")
  case 2 then write!("Two")
}`,
      `
switch (2) {
  case 1 then write!("One")
  case 2 then write!("Two")
}`,
      `
switch (3) {
  case 1 then write!("One")
  case 2 then write!("Two")
}`,
    ],
  },
  'do': {
    title: 'do',
    category: 'Special expression',
    linkName: 'do',
    customVariants: ['do body end'],
    details: [
      ['body', 'expressions', 'The expressions to evaluate.'],
    ],
    description: 'Evaluates `body`. Resulting value is the value of the last expression.',
    examples: [
      `
{
  let a := 1 + 2 + 3 + 4;
  let b := -> $ * ( $ + 1 );
  b(a)
}`,
    ],
  },
  'recur': {
    title: 'recur',
    category: 'Special expression',
    linkName: 'recur',
    customVariants: ['recur(...recur-args)'],
    description: 'Recursevly calls enclosing function or loop with its evaluated `recur-args`.',
    examples: [
      `
function foo(n) {
  write!(n);
  if (!(zero?(n))) {
    recur(n - 1)
  }
};
foo(3)`,
      `
(n -> {
  write!(n);
  if (!(zero?(n))) {
    recur(n - 1)
  }
})(3)`,
      `
loop (n := 3) {
  write!(n);
  if (!(zero?(n))) {
    recur(n - 1)
  }
}`,
    ],
  },
}
