import type { Any, Arr } from '../../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  FunctionLike,
  JuxtFunction,
  SomePredFunction,
} from '../../parser/types'
import { toAny } from '../../utils'
import { getArityFromFunction, getCommonArityFromFunctions, toFixedArity } from '../../utils/arity'
import { FUNCTION_SYMBOL } from '../../utils/symbols'
import type { BuiltinNormalExpressions } from '../interface'
import { assertArray } from '../../typeGuards/array'
import { asFunctionLike, assertFunctionLike } from '../../typeGuards/lits'
import { LitsError } from '../../errors'

export const functionalNormalExpression: BuiltinNormalExpressions = {
  '|>': {
    evaluate: ([value, func], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertFunctionLike(func, sourceCodeInfo)
      return executeFunction(func, [value], contextStack, sourceCodeInfo)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'Functional',
      returns: { type: 'any' },
      args: {
        a: { type: 'any' },
        b: { type: 'function' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'Takes a value $a and a function $b, and returns the result of applying $b to $a.',
      examples: [
        `
1 |> inc |> inc`,
        `range(10)
  |> map(_, -> $ ^ 2) // [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
  |> filter(_, odd?)  // [1, 9, 25, 49, 81]
  |> reduce(_, +, 0)  // 165
  |> sqrt             // 12.84523257866513
  |> round(_, 2)`,
      ],
    },
  },
  'apply': {
    evaluate: ([func, ...params]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertFunctionLike(func, sourceCodeInfo)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      assertArray(last, sourceCodeInfo)
      const applyArray = [...params.slice(0, -1), ...last]
      return executeFunction(func, applyArray, contextStack, sourceCodeInfo)
    },
    arity: { min: 2 },
    docs: {
      category: 'Functional',
      returns: { type: 'any' },
      args: {
        a: { type: 'function' },
        b: { type: 'array' },
        fun: { type: 'function' },
        args: { type: 'array' },
      },
      variants: [{ argumentNames: ['fun', 'args'] }],
      description: 'Call supplied function $fun with specified arguments $args.',
      examples: [
        `
apply(+, [1, 2, 3])`,
        `
apply(
  (x, y) -> sqrt(x ^ 2 + y ^ 2),
  [3, 4]
)`,
        `
(x, y) -> sqrt(x ^ 2 + y ^ 2) apply [3, 4]`,
      ],
    },
  },

  'identity': {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Functional',
      returns: { type: 'any' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns $x.',
      examples: ['identity(1)', 'identity("Albert")', 'identity({ a: 1 })', 'identity(null)'],
    },
  },

  'comp': {
    evaluate: (params, sourceCodeInfo): CompFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Comp',
        params,
        arity: params.length > 0 ? getArityFromFunction(params.at(-1) as FunctionLike) : { min: 1, max: 1 },
      }
    },
    arity: {},
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: {
        a: { type: 'function' },
        b: { type: 'function' },
        fns: { type: 'function', rest: true },
      },
      variants: [{ argumentNames: ['fns'] }],
      description: `Takes a variable number of functions and returns a function that is the composition of those.

  The returned function takes a variable number of arguments,
  applies the rightmost function to the args,
  the next function (right-to-left) to the result, etc.`,
      examples: [
        `
let negative-quotient = comp(-, /);
negative-quotient(9, 3)`,
        `
let x = { bar: { foo: 42 } };
comp("foo", "bar")(x)`,
      ],
    },
  },

  'constantly': {
    evaluate: ([value], sourceCodeInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Constantly',
        value: toAny(value),
        arity: {},
      }
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: { x: { type: 'any' } },
      variants: [{ argumentNames: ['x'] }],
      description: 'Returns a function that takes any number of arguments and always returns $x.',
      examples: [
        `
let always-true = constantly(true);
always-true(9, 3)`,
      ],
    },
  },

  'juxt': {
    evaluate: (params, sourceCodeInfo): JuxtFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      const arity = getCommonArityFromFunctions(params as FunctionLike[])
      if (arity === null) {
        throw new LitsError('All functions must accept the same number of arguments', sourceCodeInfo)
      }
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Juxt',
        params,
        arity,
      }
    },
    arity: { min: 1 },
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: {
        a: { type: 'function' },
        b: { type: 'function' },
        fun: { type: 'function' },
        fns: { type: 'function', rest: true },
      },
      variants: [
        { argumentNames: ['fun'] },
        { argumentNames: ['fun', 'fns'] },
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
    a: 1,
    b: 2,
    c: 3,
    d: 4
  }
)`,
        `
juxt(+, *, min, max) apply range(1, 11)`,
      ],
    },
  },

  'complement': {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      const fun = asFunctionLike(fn, sourceCodeInfo)
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Complement',
        function: fun,
        arity: getArityFromFunction(fun),
      }
    },
    arity: toFixedArity(1),
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: { fun: { type: 'function' } },
      variants: [{ argumentNames: ['fun'] }],
      description: 'Takes a function $fun and returns a new function that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.',
      examples: [
        'complement(>)(1, 3)',
        'complement(<)(1, 3)',
        'complement(+)(1, 3)',
        'complement(+)(0, 0)',
      ],
    },
  },

  'every-pred': {
    evaluate: (params, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'EveryPred',
        params,
        arity: { min: 1, max: 1 },
      }
    },
    arity: { min: 1 },
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: {
        fun: { type: 'function' },
        fns: { type: 'function', rest: true },
      },
      variants: [
        { argumentNames: ['fun'] },
        { argumentNames: ['fun', 'fns'] },
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
      hideOperatorForm: true,
    },
  },

  'some-pred': {
    evaluate: (params, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'SomePred',
        params,
        arity: { min: 1, max: 1 },
      }
    },
    arity: { min: 1 },
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: {
        fun: { type: 'function' },
        fns: { type: 'function', rest: true },
      },
      variants: [
        { argumentNames: ['fun'] },
        { argumentNames: ['fun', 'fns'] },
      ],
      description: 'Takes a number of `predicates` and returns a function that returns `true` if at least one of the `predicates` return a truthy `true` value against at least one of its arguments, else it returns `false`.',
      examples: [
        'some-pred(string?, -> count($) > 3)("Albert", "Mojir")',
        'some-pred(string?, -> count($) > 3)("a", "M")',
        'some-pred(string?, -> count($) > 3)("a", [1, 2, 3])',
        'some-pred(string?, -> count($) > 3)([1, 2, 3], [2])',
      ],
      hideOperatorForm: true,
    },
  },

  'fnull': {
    evaluate: ([fn, ...params], sourceCodeInfo): FNullFunction => {
      const fun = asFunctionLike(fn, sourceCodeInfo)
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Fnull',
        function: fun,
        params,
        arity: getArityFromFunction(fun),
      }
    },
    arity: { min: 2 },
    docs: {
      category: 'Functional',
      returns: { type: 'function' },
      args: {
        a: { type: 'function' },
        b: { type: 'any' },
        fun: { type: 'function' },
        arg: { type: 'any' },
        args: { type: 'any', rest: true },
      },
      variants: [
        { argumentNames: ['fun', 'arg'] },
        { argumentNames: ['fun', 'arg', 'args'] },
      ],
      description: 'Takes a function $fun, and returns a function that calls $fun, replacing a null argument to the corresponding argument.',
      examples: [
        'fnull(inc, 0)(1)',
        'fnull(inc, 0)(null)',
        '(inc fnull 0)(null)',
        'fnull(+, 1, 2)(null, 0)',
        'fnull(+, 1, 2)(0, null)',
        'fnull(+, 1, 2)(null, null)',
        'fnull(+, 1, 2)(null, null, 3, 4)',
      ],
    },
  },
}
