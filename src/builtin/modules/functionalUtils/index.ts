import type { Arr } from '../../../interface'
import type {
  ComplementFunction,
  EveryPredFunction,
  FNullFunction,
  FunctionLike,
  JuxtFunction,
  SomePredFunction,
} from '../../../parser/types'
import { getArityFromFunction, getCommonArityFromFunctions, toFixedArity } from '../../../utils/arity'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'
import { asFunctionLike, assertFunctionLike } from '../../../typeGuards/lits'
import { LitsError } from '../../../errors'
import type { LitsModule } from '../interface'

const functionalUtilsNormalExpression: BuiltinNormalExpressions = {
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
      seeAlso: ['comp'],
      examples: [
        `let { juxt } = import("Functional");
juxt(+, *, min, max)(
  3,
  4,
  6,
)`,
        `let { juxt } = import("Functional");
juxt("a", "b")(
  {
    a: 1,
    b: 2,
    c: 3,
    d: 4
  }
)`,
        `let { juxt } = import("Functional");
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
      seeAlso: ['comp', 'Functional.every-pred', 'Functional.some-pred'],
      examples: [
        'let { complement } = import("Functional");\ncomplement(>)(1, 3)',
        'let { complement } = import("Functional");\ncomplement(<)(1, 3)',
        'let { complement } = import("Functional");\ncomplement(+)(1, 3)',
        'let { complement } = import("Functional");\ncomplement(+)(0, 0)',
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
      seeAlso: ['Functional.some-pred', 'Functional.complement', 'Collection.every?'],
      examples: [
        `let { every-pred } = import("Functional");
every-pred(string?, -> count($) > 3)(
  "Albert",
  "Mojir"
)`,
        `let { every-pred } = import("Functional");
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
      seeAlso: ['Functional.every-pred', 'Functional.complement', 'Collection.any?'],
      examples: [
        'let { some-pred } = import("Functional");\nsome-pred(string?, -> count($) > 3)("Albert", "Mojir")',
        'let { some-pred } = import("Functional");\nsome-pred(string?, -> count($) > 3)("a", "M")',
        'let { some-pred } = import("Functional");\nsome-pred(string?, -> count($) > 3)("a", [1, 2, 3])',
        'let { some-pred } = import("Functional");\nsome-pred(string?, -> count($) > 3)([1, 2, 3], [2])',
      ],
      hideOperatorForm: true,
    },
  },

  'fnull': {
    evaluate: ([fn, ...params]: Arr, sourceCodeInfo): FNullFunction => {
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
      seeAlso: ['identity', 'constantly'],
      examples: [
        'let { fnull } = import("Functional");\nfnull(inc, 0)(1)',
        'let { fnull } = import("Functional");\nfnull(inc, 0)(null)',
        'let { fnull } = import("Functional");\n(inc fnull 0)(null)',
        'let { fnull } = import("Functional");\nfnull(+, 1, 2)(null, 0)',
        'let { fnull } = import("Functional");\nfnull(+, 1, 2)(0, null)',
        'let { fnull } = import("Functional");\nfnull(+, 1, 2)(null, null)',
        'let { fnull } = import("Functional");\nfnull(+, 1, 2)(null, null, 3, 4)',
      ],
    },
  },
}

export const functionalUtilsModule: LitsModule = {
  name: 'Functional',
  functions: functionalUtilsNormalExpression,
}
