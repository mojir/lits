import type { FunctionReference } from '../..'
import { type CombinatorialApiName, getOperatorArgs } from '../../api'
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

type SeqKey<T extends string> = `c:${T}-seq`
type TakeWhileKey<T extends string> = `c:${T}-take-while`
type NthKey<T extends string> = `c:${T}-nth`
type PredKey<T extends string> = `c:${T}?`

type SequenceKeys<T extends string> = SeqKey<T> | TakeWhileKey<T> | NthKey<T> | PredKey<T>

export type CombinatorialSequenceReference<T extends string> = {
  [key in SequenceKeys<T>]: FunctionReference<'Combinatorial'>
}

export const combinatorialReference: Record<CombinatorialApiName, FunctionReference<'Combinatorial'>> = {
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
  'c:count-combinations': {
    title: 'c:count-combinations',
    category: 'Combinatorial',
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
      'c:count-combinations(5, 3)',
      'c:binomial(10, 2)',
    ],
    aliases: ['c:binomial'],
  },
  'c:combinations': {
    title: 'c:combinations',
    category: 'Combinatorial',
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
      'c:combinations([1, 2, 3], 2)',
      'c:combinations(["a", "b", "c"], 2)',
      'c:combinations([1, 2, 3], 0)',
      'c:combinations([1, 2, 3], 1)',
      'c:combinations([1, 2, 3], 3)',
    ],
  },
  'c:count-derangements': {
    title: 'c:count-derangements',
    category: 'Combinatorial',
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
      'c:count-derangements(4)',
      'c:count-derangements(5)',
    ],
  },
  'c:derangements': {
    title: 'c:derangements',
    category: 'Combinatorial',
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
      'c:derangements([1, 2, 3, 4])',
      'c:derangements(["a", "b", "c"])',
    ],
  },
  'c:divisors': {
    title: 'c:divisors',
    category: 'Combinatorial',
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
      'c:divisors(12)',
      'c:divisors(100)',
      'c:divisors(37)',
    ],
  },
  'c:count-divisors': {
    title: 'c:count-divisors',
    category: 'Combinatorial',
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
      'c:count-divisors(12)',
      'c:count-divisors(100)',
      'c:count-divisors(37)',
    ],
  },
  'c:proper-divisors': {
    title: 'c:proper-divisors',
    category: 'Combinatorial',
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
      'c:proper-divisors(12)',
      'c:proper-divisors(100)',
      'c:proper-divisors(37)',
    ],
  },
  'c:count-proper-divisors': {
    title: 'c:count-proper-divisors',
    category: 'Combinatorial',
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
      'c:count-proper-divisors(12)',
      'c:count-proper-divisors(100)',
      'c:count-proper-divisors(37)',
    ],
  },
  'c:factorial': {
    title: 'c:factorial',
    category: 'Combinatorial',
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
      'c:factorial(5)',
      'c:factorial(0)',
      'c:!(10)',
      'c:!(20)',
    ],
    aliases: ['c:!'],
  },
  'c:partitions': {
    title: 'c:partitions',
    category: 'Combinatorial',
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
      'c:partitions(4)',
      'c:partitions(8)',
    ],
  },
  'c:count-partitions': {
    title: 'c:count-partitions',
    category: 'Combinatorial',
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
      'c:count-partitions(4)',
      'c:count-partitions(8)',
      'c:count-partitions(15)',
    ],
  },
  'c:permutations': {
    title: 'c:permutations',
    category: 'Combinatorial',
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
      'c:permutations([1, 2, 3])',
      'c:permutations(["a", "b", "c"])',
      'c:permutations([1, 2, 3, 4])',
      'c:permutations([1, 2])',
      'c:permutations([1])',
      'c:permutations([])',
    ],
  },
  'c:count-permutations': {
    title: 'c:count-permutations',
    category: 'Combinatorial',
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
      'c:count-permutations(5, 3)',
      'c:count-permutations(10, 2)',
      'c:count-permutations(10, 10)',
      'c:count-permutations(10, 0)',
      'c:count-permutations(10, 1)',
    ],
  },
  'c:power-set': {
    title: 'c:power-set',
    category: 'Combinatorial',
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
      'c:power-set(["a", "b", "c"])',
      'c:power-set([1, 2])',
      'c:power-set([1])',
      'c:power-set([])',
    ],
  },
  'c:count-power-set': {
    title: 'c:count-power-set',
    category: 'Combinatorial',
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
      'c:count-power-set(3)',
      'c:count-power-set(5)',
      'c:count-power-set(10)',
    ],
  },
  'c:prime-factors': {
    title: 'c:prime-factors',
    category: 'Combinatorial',
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
      'c:prime-factors(12)',
      'c:prime-factors(100)',
      'c:prime-factors(37)',
    ],
  },
  'c:count-prime-factors': {
    title: 'c:count-prime-factors',
    category: 'Combinatorial',
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
      'c:count-prime-factors(12)',
      'c:count-prime-factors(100)',
      'c:count-prime-factors(37)',
    ],
  },
  'c:distinct-prime-factors': {
    title: 'c:distinct-prime-factors',
    category: 'Combinatorial',
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
      'c:distinct-prime-factors(12)',
      'c:distinct-prime-factors(100)',
      'c:distinct-prime-factors(37)',
    ],
  },
  'c:count-distinct-prime-factors': {
    title: 'c:count-distinct-prime-factors',
    category: 'Combinatorial',
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
      'c:count-distinct-prime-factors(12)',
      'c:count-distinct-prime-factors(100)',
      'c:count-distinct-prime-factors(37)',
    ],
  },
  'c:coprime?': {
    title: 'c:coprime?',
    category: 'Combinatorial',
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
      'c:coprime?(12, 8)',
      'c:coprime?(12, 5)',
      'c:coprime?(37, 1)',
      'c:coprime?(0, 0)',
      'c:coprime?(0, 5)',
      'c:coprime?(5, 0)',
      'c:coprime?(1, 0)',
      'c:coprime?(0, 1)',
      'c:coprime?(1, 1)',
      'c:coprime?(2, 3)',
    ],
  },
  'c:divisible-by?': {
    title: 'c:divisible-by?',
    category: 'Combinatorial',
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
      '12 c:divisible-by? 4',
      'c:divisible-by?(12, 4)',
      'c:divisible-by?(12, 5)',
      'c:divisible-by?(37, 1)',
      'c:divisible-by?(0, 0)',
      'c:divisible-by?(0, 5)',
      'c:divisible-by?(5, 0)',
    ],
  },
  'c:gcd': {
    title: 'c:gcd',
    category: 'Combinatorial',
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
      '12 c:gcd  8',
      'c:gcd(100, 25)',
      'c:gcd(37, 1)',
      'c:gcd(0, 0)',
      'c:gcd(0, 5)',
      'c:gcd(5, 0)',
    ],
  },
  'c:lcm': {
    title: 'c:lcm',
    category: 'Combinatorial',
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
      '12 c:lcm  8',
      'c:lcm(100, 25)',
      'c:lcm(37, 1)',
      'c:lcm(0, 0)',
      'c:lcm(0, 5)',
      'c:lcm(5, 0)',
    ],
  },
  'c:multinomial': {
    title: 'c:multinomial',
    category: 'Combinatorial',
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
      'c:multinomial(5, 2, 3)',
      'c:multinomial(10, 2, 3, 5)',
    ],
    noOperatorDocumentation: true,
  },
  'c:amicable?': {
    title: 'c:amicable?',
    category: 'Combinatorial',
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
      'c:amicable?(220, 284)',
      'c:amicable?(1184, 1210)',
      'c:amicable?(2620, 2924)',
      'c:amicable?(5020, 5564)',
      'c:amicable?(6232, 6368)',
    ],
  },
  'c:euler-totient': {
    title: 'c:euler-totient',
    category: 'Combinatorial',
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
      'c:euler-totient(1)',
      'c:euler-totient(2)',
      'c:euler-totient(10)',
      'c:euler-totient(20)',
    ],
  },
  'c:mobius': {
    title: 'c:mobius',
    category: 'Combinatorial',
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
      'c:mobius(1)',
      'c:mobius(2)',
      'c:mobius(3)',
      'c:mobius(4)',
      'c:mobius(6)',
      'c:mobius(12)',
      'c:mobius(30)',
    ],
  },
  'c:mertens': {
    title: 'c:mertens',
    category: 'Combinatorial',
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
      'c:mobius(1)',
      'c:mobius(2)',
      'c:mobius(3)',
      'c:mobius(4)',
      'c:mobius(6)',
      'c:mobius(12)',
      'c:mobius(30)',
    ],
  },
  'c:sigma': {
    title: 'c:sigma',
    category: 'Combinatorial',
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
      'c:sigma(1)',
      'c:sigma(2)',
      'c:sigma(3)',
      'c:sigma(4)',
      'c:sigma(6)',
      'c:sigma(12)',
      'c:sigma(30)',
    ],
  },
  'c:carmichael-lambda': {
    title: 'c:carmichael-lambda',
    category: 'Combinatorial',
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
      'c:carmichael-lambda(1)',
      'c:carmichael-lambda(2)',
      'c:carmichael-lambda(3)',
      'c:carmichael-lambda(4)',
      'c:carmichael-lambda(6)',
      'c:carmichael-lambda(12)',
      'c:carmichael-lambda(30)',
    ],
  },
  'c:cartesian-product': {
    title: 'c:cartesian-product',
    category: 'Combinatorial',
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
      'c:cartesian-product([1, 2], ["a", "b"])',
      'c:cartesian-product([1, 2], ["a", "b"], [true, false])',
      'c:cartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'c:perfect-power': {
    title: 'c:perfect-power',
    category: 'Combinatorial',
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
      'c:perfect-power(1)',
      'c:perfect-power(2)',
      'c:perfect-power(4)',
      'c:perfect-power(8)',
      'c:perfect-power(9)',
      'c:perfect-power(16)',
      'c:perfect-power(19)',
    ],
  },
  'c:mod-exp': {
    title: 'c:mod-exp',
    category: 'Combinatorial',
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
      'c:mod-exp(2, 3, 5)',
      'c:mod-exp(3, 4, 7)',
      'c:mod-exp(5, 6, 11)',
      'c:mod-exp(7, 8, 13)',
    ],
  },
  'c:mod-inv': {
    title: 'c:mod-inv',
    category: 'Combinatorial',
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
      'c:mod-inv(3, 11)',
      'c:mod-inv(10, 17)',
      'c:mod-inv(5, 13)',
      'c:mod-inv(7, 19)',
    ],
  },
  'c:extended-gcd': {
    title: 'c:extended-gcd',
    category: 'Combinatorial',
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
      'c:extended-gcd(30, 12)',
      'c:extended-gcd(56, 98)',
      'c:extended-gcd(101, 10)',
      'c:extended-gcd(17, 13)',
    ],
  },
  'c:chinese-remainder': {
    title: 'c:chinese-remainder',
    category: 'Combinatorial',
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
      'c:chinese-remainder([2, 3], [3, 5])',
      'c:chinese-remainder([1, 2], [3, 4])',
      'c:chinese-remainder([0, 1], [2, 3])',
      'c:chinese-remainder([1, 2, 3], [4, 5, 6])',
    ],
  },
  'c:stirling-first': {
    title: 'c:stirling-first',
    category: 'Combinatorial',
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
      'c:stirling-first(5, 2)',
      'c:stirling-first(4, 3)',
      'c:stirling-first(6, 1)',
      'c:stirling-first(7, 4)',
      'c:stirling-first(8, 5)',
    ],
  },
  'c:stirling-second': {
    title: 'c:stirling-second',
    category: 'Combinatorial',
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
      'c:stirling-second(5, 2)',
      'c:stirling-second(4, 3)',
      'c:stirling-second(6, 1)',
      'c:stirling-second(7, 4)',
      'c:stirling-second(8, 5)',
    ],
  },
}
