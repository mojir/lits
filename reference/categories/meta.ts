import type { FunctionReference } from '..'
import type { MetaApiName } from '../api'

export const metaReference: Record<MetaApiName, FunctionReference<'Meta'>> = {
  doc: {
    title: 'doc',
    category: 'Meta',
    returns: {
      type: 'string',
    },
    args: {
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['fun'] },
    ],
    description: 'Returns documentation string of the $fun.',
    examples: [
      'doc(+)',
      `
function add(x, y) {
  """
  Adds two numbers.
  Args:
    x: First number.
    y: Second number.
  Returns:
    Sum of x and y.
  """
  x + y;
};

doc(add)`,
    ],
  },
  arity: {
    title: 'arity',
    category: 'Meta',
    returns: {
      type: 'object',
    },
    args: {
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['fun'] },
    ],
    description: 'Returns arity of the $fun. The arity is an object with the properties: `min` and `max`. If the function has fixed arity, `min` and `max` are equal to the number of required parameters. If no restrictions apply, empty object is returned.',
    examples: [
      'arity(+)',
      'arity(defined?)',
      `
function add(x, y = 0) {
  x + y;
};

arity(add)`,
      `
function foo(k, ...x) {
  k + x;
};
  arity(foo)`,
    ],
  },
}
