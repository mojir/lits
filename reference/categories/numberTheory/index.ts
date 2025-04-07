import type { FunctionReference } from '../..'
import type { NumberTheoryApiName } from '../../api'
import { getOperatorArgs } from '../../api'
import { abundantReference } from './abundant'
import { arithmeticReference } from './arithmetic'
import { bellReference } from './bell'
import { bernoulliReference } from './bernoulli'
import { catalanReference } from './catalan'
import { collatzReference } from './collatz'
import { compositeReference } from './composite'
import { factorialReference } from './factorial'
import { fibonacciReference } from './fibonacci'
import { geometricReference } from './geometric'
import { golombReference } from './golomb'
import { happyReference } from './happy'
import { jugglerReference } from './juggler'
import { lookAndSayReference } from './lookAndSay'
import { lucasReference } from './lucas'
import { luckyReference } from './lucky'
import { mersenneReference } from './mersenne'
import { padovanReference } from './padovan'
import { partitionReference } from './partition'
import { pellReference } from './pell'
import { perfectReference } from './perfect'
import { perfectSquareReference } from './perfectSquare'
import { perfectCubeReference } from './perfectCube'
import { perfectPowerReference } from './perfectPower'
import { polygonalReference } from './polygonal'
import { primeReference } from './prime'
import { recamanReference } from './recaman'
import { sylvesterReference } from './sylvester'
import { thueMorseReference } from './thueMorse'
import { tribonacciReference } from './tribonacci'

type SeqKey<T extends string> = `n:${T}-seq`
type TakeWhileKey<T extends string> = `n:${T}-take-while`
type NthKey<T extends string> = `n:${T}-nth`
type PredKey<T extends string> = `n:${T}?`

type SequenceKeys<T extends string> = SeqKey<T> | TakeWhileKey<T> | NthKey<T> | PredKey<T>

export type NumberTheorySequenceReference<T extends string> = {
  [key in SequenceKeys<T>]: FunctionReference<'Number Theory'>
}

export const numberTheoryReference: Record<NumberTheoryApiName, FunctionReference<'Number Theory'>> = {
  ...abundantReference,
  ...arithmeticReference,
  ...bellReference,
  ...bernoulliReference,
  ...catalanReference,
  ...collatzReference,
  ...compositeReference,
  ...factorialReference,
  ...fibonacciReference,
  ...geometricReference,
  ...golombReference,
  ...happyReference,
  ...jugglerReference,
  ...lookAndSayReference,
  ...lucasReference,
  ...luckyReference,
  ...mersenneReference,
  ...padovanReference,
  ...partitionReference,
  ...pellReference,
  ...perfectReference,
  ...perfectSquareReference,
  ...perfectCubeReference,
  ...perfectPowerReference,
  ...polygonalReference,
  ...primeReference,
  ...recamanReference,
  ...sylvesterReference,
  ...thueMorseReference,
  ...tribonacciReference,
  'n:count-combinations': {
    title: 'n:count-combinations',
    category: 'Number Theory',
    description: 'Calculates the number of combinations of n items taken k at a time.',
    linkName: 'c-colon-count-combinations',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The total number of items.',
      },
      k: {
        type: 'integer',
        description: 'The number of items to choose.',
      },
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['n', 'k'] },
    ],
    examples: [
      'n:count-combinations(5, 3)',
      'n:binomial(10, 2)',
    ],
    aliases: ['n:binomial'],
  },
  'n:combinations': {
    title: 'n:combinations',
    category: 'Number Theory',
    description: 'Generates all possible combinations of a specified size from a collection.',
    linkName: 'c-colon-combinations',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate combinations from.',
      },
      n: {
        type: 'integer',
        description: 'The size of each combination.',
      },
      ...getOperatorArgs('array', 'integer'),
    },
    variants: [
      { argumentNames: ['set', 'n'] },
    ],
    examples: [
      'n:combinations([1, 2, 3], 2)',
      'n:combinations(["a", "b", "c"], 2)',
      'n:combinations([1, 2, 3], 0)',
      'n:combinations([1, 2, 3], 1)',
      'n:combinations([1, 2, 3], 3)',
    ],
  },
  'n:count-derangements': {
    title: 'n:count-derangements',
    category: 'Number Theory',
    description: 'Calculates the number of derangements (permutations where no element appears in its original position) of n items.',
    linkName: 'c-colon-count-derangements',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The total number of items.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-derangements(4)',
      'n:count-derangements(5)',
    ],
  },
  'n:derangements': {
    title: 'n:derangements',
    category: 'Number Theory',
    description: 'Generates all derangements (permutations where no element appears in its original position) of a set.',
    linkName: 'c-colon-derangements',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate derangements from.',
      },
    },
    variants: [
      { argumentNames: ['set'] },
    ],
    examples: [
      'n:derangements([1, 2, 3, 4])',
      'n:derangements(["a", "b", "c"])',
    ],
  },
  'n:divisors': {
    title: 'n:divisors',
    category: 'Number Theory',
    description: 'Returns the divisors of a number.',
    linkName: 'c-colon-divisors',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find divisors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:divisors(12)',
      'n:divisors(100)',
      'n:divisors(37)',
    ],
  },
  'n:count-divisors': {
    title: 'n:count-divisors',
    category: 'Number Theory',
    description: 'Returns the number of divisors of a number.',
    linkName: 'c-colon-count-divisors',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count divisors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-divisors(12)',
      'n:count-divisors(100)',
      'n:count-divisors(37)',
    ],
  },
  'n:proper-divisors': {
    title: 'n:proper-divisors',
    category: 'Number Theory',
    description: 'Returns the proper divisors of a number.',
    linkName: 'c-colon-proper-divisors',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find proper divisors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:proper-divisors(12)',
      'n:proper-divisors(100)',
      'n:proper-divisors(37)',
    ],
  },
  'n:count-proper-divisors': {
    title: 'n:count-proper-divisors',
    category: 'Number Theory',
    description: 'Returns the number of proper divisors of a number.',
    linkName: 'c-colon-count-proper-divisors',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count proper divisors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-proper-divisors(12)',
      'n:count-proper-divisors(100)',
      'n:count-proper-divisors(37)',
    ],
  },
  'n:factorial': {
    title: 'n:factorial',
    category: 'Number Theory',
    description: 'Calculates the factorial of a number.',
    linkName: 'c-colon-factorial',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the factorial for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:factorial(5)',
      'n:factorial(0)',
      'n:!(10)',
      'n:!(20)',
    ],
    aliases: ['n:!'],
  },
  'n:partitions': {
    title: 'n:partitions',
    category: 'Number Theory',
    description: 'Generates all partitions of a number.',
    linkName: 'c-colon-partitions',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to partition.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:partitions(4)',
      'n:partitions(8)',
    ],
  },
  'n:count-partitions': {
    title: 'n:count-partitions',
    category: 'Number Theory',
    description: 'Returns the number of partitions of a number.',
    linkName: 'c-colon-count-partitions',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count partitions for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-partitions(4)',
      'n:count-partitions(8)',
      'n:count-partitions(15)',
    ],
  },
  'n:permutations': {
    title: 'n:permutations',
    category: 'Number Theory',
    description: 'Generates all permutations of a collection.',
    linkName: 'c-colon-permutations',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate permutations from.',
      },
    },
    variants: [
      { argumentNames: ['set'] },
    ],
    examples: [
      'n:permutations([1, 2, 3])',
      'n:permutations(["a", "b", "c"])',
      'n:permutations([1, 2, 3, 4])',
      'n:permutations([1, 2])',
      'n:permutations([1])',
      'n:permutations([])',
    ],
  },
  'n:count-permutations': {
    title: 'n:count-permutations',
    category: 'Number Theory',
    description: 'Returns the number of permutations of n items taken k at a time.',
    linkName: 'c-colon-count-permutations',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The total number of items.',
      },
      k: {
        type: 'integer',
        description: 'The number of items to choose.',
      },
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['n', 'k'] },
    ],
    examples: [
      'n:count-permutations(5, 3)',
      'n:count-permutations(10, 2)',
      'n:count-permutations(10, 10)',
      'n:count-permutations(10, 0)',
      'n:count-permutations(10, 1)',
    ],
  },
  'n:power-set': {
    title: 'n:power-set',
    category: 'Number Theory',
    description: 'Generates the power set of a collection.',
    linkName: 'c-colon-power-set',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'any',
        array: true,
        description: 'The input collection to generate the power set from.',
      },
    },
    variants: [
      { argumentNames: ['set'] },
    ],
    examples: [
      'n:power-set(["a", "b", "c"])',
      'n:power-set([1, 2])',
      'n:power-set([1])',
      'n:power-set([])',
    ],
  },
  'n:count-power-set': {
    title: 'n:count-power-set',
    category: 'Number Theory',
    description: 'Returns the number of subsets of a set.',
    linkName: 'c-colon-count-power-set',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The size of the set.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-power-set(3)',
      'n:count-power-set(5)',
      'n:count-power-set(10)',
    ],
  },
  'n:prime-factors': {
    title: 'n:prime-factors',
    category: 'Number Theory',
    description: 'Returns the prime factors of a number.',
    linkName: 'c-colon-factors',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to factor.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:prime-factors(12)',
      'n:prime-factors(100)',
      'n:prime-factors(37)',
    ],
  },
  'n:count-prime-factors': {
    title: 'n:count-prime-factors',
    category: 'Number Theory',
    description: 'Returns the number of prime factors of a number.',
    linkName: 'c-colon-count-factors',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count prime factors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-prime-factors(12)',
      'n:count-prime-factors(100)',
      'n:count-prime-factors(37)',
    ],
  },
  'n:distinct-prime-factors': {
    title: 'n:distinct-prime-factors',
    category: 'Number Theory',
    description: 'Returns the distinct prime factors of a number.',
    linkName: 'c-colon-distinct-factors',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find distinct prime factors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:distinct-prime-factors(12)',
      'n:distinct-prime-factors(100)',
      'n:distinct-prime-factors(37)',
    ],
  },
  'n:count-distinct-prime-factors': {
    title: 'n:count-distinct-prime-factors',
    category: 'Number Theory',
    description: 'Returns the number of distinct prime factors of a number.',
    linkName: 'c-colon-count-distinct-factors',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count distinct prime factors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:count-distinct-prime-factors(12)',
      'n:count-distinct-prime-factors(100)',
      'n:count-distinct-prime-factors(37)',
    ],
  },
  'n:coprime?': {
    title: 'n:coprime?',
    category: 'Number Theory',
    description: 'Checks if two numbers are coprime (i.e., their GCD is 1).',
    linkName: 'c-colon-coprime',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'n:coprime?(12, 8)',
      'n:coprime?(12, 5)',
      'n:coprime?(37, 1)',
      'n:coprime?(0, 0)',
      'n:coprime?(0, 5)',
      'n:coprime?(5, 0)',
      'n:coprime?(1, 0)',
      'n:coprime?(0, 1)',
      'n:coprime?(1, 1)',
      'n:coprime?(2, 3)',
    ],
  },
  'n:divisible-by?': {
    title: 'n:divisible-by?',
    category: 'Number Theory',
    description: 'Checks if a number is divisible by another number.',
    linkName: 'c-colon-divisible-by',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      '12 n:divisible-by? 4',
      'n:divisible-by?(12, 4)',
      'n:divisible-by?(12, 5)',
      'n:divisible-by?(37, 1)',
      'n:divisible-by?(0, 0)',
      'n:divisible-by?(0, 5)',
      'n:divisible-by?(5, 0)',
    ],
  },
  'n:gcd': {
    title: 'n:gcd',
    category: 'Number Theory',
    description: 'Calculates the greatest common divisor (GCD) of two numbers.',
    linkName: 'c-colon-gcd',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      '12 n:gcd  8',
      'n:gcd(100, 25)',
      'n:gcd(37, 1)',
      'n:gcd(0, 0)',
      'n:gcd(0, 5)',
      'n:gcd(5, 0)',
    ],
  },
  'n:lcm': {
    title: 'n:lcm',
    category: 'Number Theory',
    description: 'Calculates the least common multiple (LCM) of two numbers.',
    linkName: 'c-colon-lcm',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      '12 n:lcm  8',
      'n:lcm(100, 25)',
      'n:lcm(37, 1)',
      'n:lcm(0, 0)',
      'n:lcm(0, 5)',
      'n:lcm(5, 0)',
    ],
  },
  'n:multinomial': {
    title: 'n:multinomial',
    category: 'Number Theory',
    description: 'Calculates the multinomial coefficient from of a list of numbers representing the sizes of each group.',
    linkName: 'c-colon-multinomial',
    returns: {
      type: 'integer',
    },
    args: {
      args: {
        type: 'integer',
        rest: true,
        description: 'The numbers representing the sizes of each group.',
      },
    },
    variants: [
      { argumentNames: ['n', 'k'] },
    ],
    examples: [
      'n:multinomial(5, 2, 3)',
      'n:multinomial(10, 2, 3, 5)',
    ],
    noOperatorDocumentation: true,
  },
  'n:amicable?': {
    title: 'n:amicable?',
    category: 'Number Theory',
    description: 'Checks if two numbers are amicable (i.e., the sum of the proper divisors of each number equals the other number).',
    linkName: 'c-colon-amicable',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'n:amicable?(220, 284)',
      'n:amicable?(1184, 1210)',
      'n:amicable?(2620, 2924)',
      'n:amicable?(5020, 5564)',
      'n:amicable?(6232, 6368)',
    ],
  },
  'n:euler-totient': {
    title: 'n:euler-totient',
    category: 'Number Theory',
    description: 'Calculates the Euler\'s totient function (φ(n)) of a number, which counts the integers up to n that are coprime to n.',
    linkName: 'c-colon-euler-totient',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the totient for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:euler-totient(1)',
      'n:euler-totient(2)',
      'n:euler-totient(10)',
      'n:euler-totient(20)',
    ],
  },
  'n:mobius': {
    title: 'n:mobius',
    category: 'Number Theory',
    description: 'Calculates the Möbius function (μ(n)) of a number, which is used in number theory.',
    linkName: 'c-colon-mobius',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Möbius function for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:mobius(1)',
      'n:mobius(2)',
      'n:mobius(3)',
      'n:mobius(4)',
      'n:mobius(6)',
      'n:mobius(12)',
      'n:mobius(30)',
    ],
  },
  'n:mertens': {
    title: 'n:mertens',
    category: 'Number Theory',
    description: 'Calculates the Mertens function (M(n)) of a number, which is the sum of the Möbius function up to n.',
    linkName: 'c-colon-mertens',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Mertens function for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:mobius(1)',
      'n:mobius(2)',
      'n:mobius(3)',
      'n:mobius(4)',
      'n:mobius(6)',
      'n:mobius(12)',
      'n:mobius(30)',
    ],
  },
  'n:sigma': {
    title: 'n:sigma',
    category: 'Number Theory',
    description: 'Calculates the sum of divisors function (σ(n)) of a number, which is the sum of all positive divisors of n.',
    linkName: 'c-colon-sigma',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the sum of divisors for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:sigma(1)',
      'n:sigma(2)',
      'n:sigma(3)',
      'n:sigma(4)',
      'n:sigma(6)',
      'n:sigma(12)',
      'n:sigma(30)',
    ],
  },
  'n:carmichael-lambda': {
    title: 'n:carmichael-lambda',
    category: 'Number Theory',
    description: 'Calculates the Carmichael function (λ(n)) of a number, which is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.',
    linkName: 'c-colon-carmichael-lambda',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Carmichael function for.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:carmichael-lambda(1)',
      'n:carmichael-lambda(2)',
      'n:carmichael-lambda(3)',
      'n:carmichael-lambda(4)',
      'n:carmichael-lambda(6)',
      'n:carmichael-lambda(12)',
      'n:carmichael-lambda(30)',
    ],
  },
  'n:cartesian-product': {
    title: 'n:cartesian-product',
    category: 'Number Theory',
    description: 'Calculates the Cartesian product of two or more sets.',
    linkName: 'c-colon-cartesian-product',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      sets: {
        type: 'any',
        array: true,
        description: 'The input collections to calculate the Cartesian product from.',
      },
    },
    variants: [
      { argumentNames: ['sets'] },
    ],
    examples: [
      'n:cartesian-product([1, 2], ["a", "b"])',
      'n:cartesian-product([1, 2], ["a", "b"], [true, false])',
      'n:cartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'n:perfect-power': {
    title: 'n:perfect-power',
    category: 'Number Theory',
    description: 'Returns a tuple of the base and exponent if the number is a perfect power, otherwise returns null.',
    linkName: 'c-colon-perfect-power',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:perfect-power(1)',
      'n:perfect-power(2)',
      'n:perfect-power(4)',
      'n:perfect-power(8)',
      'n:perfect-power(9)',
      'n:perfect-power(16)',
      'n:perfect-power(19)',
    ],
  },
  'n:mod-exp': {
    title: 'n:mod-exp',
    category: 'Number Theory',
    description: 'Calculates the modular exponentiation of a base raised to an exponent modulo a modulus.',
    linkName: 'c-colon-mod-exp',
    returns: {
      type: 'integer',
    },
    args: {
      base: {
        type: 'integer',
      },
      exponent: {
        type: 'integer',
      },
      modulus: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['base', 'exponent', 'modulus'] },
    ],
    examples: [
      'n:mod-exp(2, 3, 5)',
      'n:mod-exp(3, 4, 7)',
      'n:mod-exp(5, 6, 11)',
      'n:mod-exp(7, 8, 13)',
    ],
  },
  'n:mod-inv': {
    title: 'n:mod-inv',
    category: 'Number Theory',
    description: 'Calculates the modular multiplicative inverse of a number modulo another number.',
    linkName: 'c-colon-mod-inv',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'm'] },
    ],
    examples: [
      'n:mod-inv(3, 11)',
      'n:mod-inv(10, 17)',
      'n:mod-inv(5, 13)',
      'n:mod-inv(7, 19)',
    ],
  },
  'n:extended-gcd': {
    title: 'n:extended-gcd',
    category: 'Number Theory',
    description: 'Calculates the extended greatest common divisor (GCD) of two numbers, returning the GCD and the coefficients of Bézout\'s identity.',
    linkName: 'c-colon-extended-gcd',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'n:extended-gcd(30, 12)',
      'n:extended-gcd(56, 98)',
      'n:extended-gcd(101, 10)',
      'n:extended-gcd(17, 13)',
    ],
  },
  'n:chinese-remainder': {
    title: 'n:chinese-remainder',
    category: 'Number Theory',
    description: 'Solves a system of simultaneous congruences using the Chinese Remainder Theorem.',
    linkName: 'c-colon-chinese-remainder',
    returns: {
      type: 'integer',
    },
    args: {
      remainders: {
        type: 'integer',
        array: true,
        description: 'The remainders of the congruences.',
      },
      moduli: {
        type: 'integer',
        array: true,
        description: 'The moduli of the congruences.',
      },
    },
    variants: [
      { argumentNames: ['remainders', 'moduli'] },
    ],
    examples: [
      'n:chinese-remainder([2, 3], [3, 5])',
      'n:chinese-remainder([1, 2], [3, 4])',
      'n:chinese-remainder([0, 1], [2, 3])',
      'n:chinese-remainder([1, 2, 3], [4, 5, 6])',
    ],
  },
  'n:stirling-first': {
    title: 'n:stirling-first',
    category: 'Number Theory',
    description: 'Calculates the Stirling numbers of the first kind, which count the number of permutations of n elements with k cycles.',
    linkName: 'c-colon-stirling-first',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The number of elements.',
      },
      b: {
        type: 'integer',
        description: 'The number of cycles.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'n:stirling-first(5, 2)',
      'n:stirling-first(4, 3)',
      'n:stirling-first(6, 1)',
      'n:stirling-first(7, 4)',
      'n:stirling-first(8, 5)',
    ],
  },
  'n:stirling-second': {
    title: 'n:stirling-second',
    category: 'Number Theory',
    description: 'Calculates the Stirling numbers of the second kind, which count the number of ways to partition n elements into k non-empty subsets.',
    linkName: 'c-colon-stirling-second',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The number of elements.',
      },
      b: {
        type: 'integer',
        description: 'The number of subsets.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'n:stirling-second(5, 2)',
      'n:stirling-second(4, 3)',
      'n:stirling-second(6, 1)',
      'n:stirling-second(7, 4)',
      'n:stirling-second(8, 5)',
    ],
  },
}
