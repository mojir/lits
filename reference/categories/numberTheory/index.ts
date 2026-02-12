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
import { deficientReference } from './deficient'

type SeqKey<T extends string> = `nth.${T}-seq`
type TakeWhileKey<T extends string> = `nth.${T}-take-while`
type NthKey<T extends string> = `nth.${T}-nth`
type PredKey<T extends string> = `nth.${T}?`

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
  ...deficientReference,
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
  'nth.count-combinations': {
    title: 'nth.count-combinations',
    category: 'Number Theory',
    description: 'Calculates the number of combinations of n items taken k at a time.',
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
      'nth.count-combinations(5, 3)',
      'nth.binomial(10, 2)',
    ],
    aliases: ['nth.binomial'],
  },
  'nth.combinations': {
    title: 'nth.combinations',
    category: 'Number Theory',
    description: 'Generates all possible combinations of a specified size from a collection.',
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
      'nth.combinations([1, 2, 3], 2)',
      'nth.combinations(["a", "b", "c"], 2)',
      'nth.combinations([1, 2, 3], 0)',
      'nth.combinations([1, 2, 3], 1)',
      'nth.combinations([1, 2, 3], 3)',
    ],
  },
  'nth.count-derangements': {
    title: 'nth.count-derangements',
    category: 'Number Theory',
    description: 'Calculates the number of derangements (permutations where no element appears in its original position) of n items.',
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
      'nth.count-derangements(4)',
      'nth.count-derangements(5)',
    ],
  },
  'nth.derangements': {
    title: 'nth.derangements',
    category: 'Number Theory',
    description: 'Generates all derangements (permutations where no element appears in its original position) of a set.',
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
      'nth.derangements([1, 2, 3, 4])',
      'nth.derangements(["a", "b", "c"])',
    ],
  },
  'nth.divisors': {
    title: 'nth.divisors',
    category: 'Number Theory',
    description: 'Returns the divisors of a number.',
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
      'nth.divisors(12)',
      'nth.divisors(100)',
      'nth.divisors(37)',
    ],
  },
  'nth.count-divisors': {
    title: 'nth.count-divisors',
    category: 'Number Theory',
    description: 'Returns the number of divisors of a number.',
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
      'nth.count-divisors(12)',
      'nth.count-divisors(100)',
      'nth.count-divisors(37)',
    ],
  },
  'nth.proper-divisors': {
    title: 'nth.proper-divisors',
    category: 'Number Theory',
    description: 'Returns the proper divisors of a number.',
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
      'nth.proper-divisors(12)',
      'nth.proper-divisors(100)',
      'nth.proper-divisors(37)',
    ],
  },
  'nth.count-proper-divisors': {
    title: 'nth.count-proper-divisors',
    category: 'Number Theory',
    description: 'Returns the number of proper divisors of a number.',
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
      'nth.count-proper-divisors(12)',
      'nth.count-proper-divisors(100)',
      'nth.count-proper-divisors(37)',
    ],
  },
  'nth.factorial': {
    title: 'nth.factorial',
    category: 'Number Theory',
    description: 'Calculates the factorial of a number.',
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
      'nth.factorial(5)',
      'nth.factorial(0)',
      'nth.!(10)',
      'nth.!(20)',
    ],
    aliases: ['nth.!'],
  },
  'nth.partitions': {
    title: 'nth.partitions',
    category: 'Number Theory',
    description: 'Generates all partitions of a number.',
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
      'nth.partitions(4)',
      'nth.partitions(8)',
    ],
  },
  'nth.count-partitions': {
    title: 'nth.count-partitions',
    category: 'Number Theory',
    description: 'Returns the number of partitions of a number.',
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
      'nth.count-partitions(4)',
      'nth.count-partitions(8)',
      'nth.count-partitions(15)',
    ],
  },
  'nth.permutations': {
    title: 'nth.permutations',
    category: 'Number Theory',
    description: 'Generates all permutations of a collection.',
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
      'nth.permutations([1, 2, 3])',
      'nth.permutations(["a", "b", "c"])',
      'nth.permutations([1, 2, 3, 4])',
      'nth.permutations([1, 2])',
      'nth.permutations([1])',
      'nth.permutations([])',
    ],
  },
  'nth.count-permutations': {
    title: 'nth.count-permutations',
    category: 'Number Theory',
    description: 'Returns the number of permutations of n items taken k at a time.',
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
      'nth.count-permutations(5, 3)',
      'nth.count-permutations(10, 2)',
      'nth.count-permutations(10, 10)',
      'nth.count-permutations(10, 0)',
      'nth.count-permutations(10, 1)',
    ],
  },
  'nth.power-set': {
    title: 'nth.power-set',
    category: 'Number Theory',
    description: 'Generates the power set of a collection.',
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
      'nth.power-set(["a", "b", "c"])',
      'nth.power-set([1, 2])',
      'nth.power-set([1])',
      'nth.power-set([])',
    ],
  },
  'nth.count-power-set': {
    title: 'nth.count-power-set',
    category: 'Number Theory',
    description: 'Returns the number of subsets of a set.',
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
      'nth.count-power-set(3)',
      'nth.count-power-set(5)',
      'nth.count-power-set(10)',
    ],
  },
  'nth.prime-factors': {
    title: 'nth.prime-factors',
    category: 'Number Theory',
    description: 'Returns the prime factors of a number.',
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
      'nth.prime-factors(12)',
      'nth.prime-factors(100)',
      'nth.prime-factors(37)',
    ],
  },
  'nth.count-prime-factors': {
    title: 'nth.count-prime-factors',
    category: 'Number Theory',
    description: 'Returns the number of prime factors of a number.',
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
      'nth.count-prime-factors(12)',
      'nth.count-prime-factors(100)',
      'nth.count-prime-factors(37)',
    ],
  },
  'nth.distinct-prime-factors': {
    title: 'nth.distinct-prime-factors',
    category: 'Number Theory',
    description: 'Returns the distinct prime factors of a number.',
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
      'nth.distinct-prime-factors(12)',
      'nth.distinct-prime-factors(100)',
      'nth.distinct-prime-factors(37)',
    ],
  },
  'nth.count-distinct-prime-factors': {
    title: 'nth.count-distinct-prime-factors',
    category: 'Number Theory',
    description: 'Returns the number of distinct prime factors of a number.',
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
      'nth.count-distinct-prime-factors(12)',
      'nth.count-distinct-prime-factors(100)',
      'nth.count-distinct-prime-factors(37)',
    ],
  },
  'nth.coprime?': {
    title: 'nth.coprime?',
    category: 'Number Theory',
    description: 'Checks if two numbers are coprime (i.e., their GCD is 1).',
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
      'nth.coprime?(12, 8)',
      'nth.coprime?(12, 5)',
      'nth.coprime?(37, 1)',
      'nth.coprime?(0, 0)',
      'nth.coprime?(0, 5)',
      'nth.coprime?(5, 0)',
      'nth.coprime?(1, 0)',
      'nth.coprime?(0, 1)',
      'nth.coprime?(1, 1)',
      'nth.coprime?(2, 3)',
    ],
  },
  'nth.divisible-by?': {
    title: 'nth.divisible-by?',
    category: 'Number Theory',
    description: 'Checks if a number is divisible by another number.',
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
      'nth.divisible-by?(12, 4)',
      'nth.divisible-by?(12, 5)',
      'nth.divisible-by?(37, 1)',
      'nth.divisible-by?(0, 0)',
      'nth.divisible-by?(0, 5)',
      'nth.divisible-by?(5, 0)',
    ],
  },
  'nth.gcd': {
    title: 'nth.gcd',
    category: 'Number Theory',
    description: 'Calculates the greatest common divisor (GCD) of two numbers.',
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
      'nth.gcd(100, 25)',
      'nth.gcd(37, 1)',
      'nth.gcd(0, 0)',
      'nth.gcd(0, 5)',
      'nth.gcd(5, 0)',
    ],
  },
  'nth.lcm': {
    title: 'nth.lcm',
    category: 'Number Theory',
    description: 'Calculates the least common multiple (LCM) of two numbers.',
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
      'nth.lcm(100, 25)',
      'nth.lcm(37, 1)',
      'nth.lcm(0, 5)',
      'nth.lcm(5, 0)',
    ],
  },
  'nth.multinomial': {
    title: 'nth.multinomial',
    category: 'Number Theory',
    description: 'Calculates the multinomial coefficient from of a list of numbers representing the sizes of each group.',
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
      { argumentNames: ['args'] },
    ],
    examples: [
      'nth.multinomial(5, 2, 3)',
      'nth.multinomial(10, 2, 3, 5)',
    ],
    noOperatorDocumentation: true,
  },
  'nth.amicable?': {
    title: 'nth.amicable?',
    category: 'Number Theory',
    description: 'Checks if two numbers are amicable (i.e., the sum of the proper divisors of each number equals the other number).',
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
      'nth.amicable?(220, 284)',
      'nth.amicable?(1184, 1210)',
      'nth.amicable?(2620, 2924)',
      'nth.amicable?(5020, 5564)',
      'nth.amicable?(6232, 6368)',
    ],
  },
  'nth.euler-totient': {
    title: 'nth.euler-totient',
    category: 'Number Theory',
    description: 'Calculates the Euler\'s totient function (φ(n)) of a number, which counts the integers up to n that are coprime to n.',
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
      'nth.euler-totient(1)',
      'nth.euler-totient(2)',
      'nth.euler-totient(10)',
      'nth.euler-totient(20)',
    ],
  },
  'nth.mobius': {
    title: 'nth.mobius',
    category: 'Number Theory',
    description: 'Calculates the Möbius function (μ(n)) of a number, which is used in number theory.',
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
      'nth.mobius(1)',
      'nth.mobius(2)',
      'nth.mobius(3)',
      'nth.mobius(4)',
      'nth.mobius(6)',
      'nth.mobius(12)',
      'nth.mobius(30)',
    ],
    aliases: ['nth.möbius'],
  },
  'nth.mertens': {
    title: 'nth.mertens',
    category: 'Number Theory',
    description: 'Calculates the Mertens function (M(n)) of a number, which is the sum of the Möbius function up to n.',
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
      'nth.mobius(1)',
      'nth.mobius(2)',
      'nth.mobius(3)',
      'nth.mobius(4)',
      'nth.mobius(6)',
      'nth.mobius(12)',
      'nth.mobius(30)',
    ],
  },
  'nth.sigma': {
    title: 'nth.sigma',
    category: 'Number Theory',
    description: 'Calculates the sum of divisors function (σ(n)) of a number, which is the sum of all positive divisors of n.',
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
      'nth.sigma(1)',
      'nth.sigma(2)',
      'nth.sigma(3)',
      'nth.sigma(4)',
      'nth.sigma(6)',
      'nth.sigma(12)',
      'nth.sigma(30)',
    ],
  },
  'nth.carmichael-lambda': {
    title: 'nth.carmichael-lambda',
    category: 'Number Theory',
    description: 'Calculates the Carmichael function (λ(n)) of a number, which is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.',
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
      'nth.carmichael-lambda(1)',
      'nth.carmichael-lambda(2)',
      'nth.carmichael-lambda(3)',
      'nth.carmichael-lambda(4)',
      'nth.carmichael-lambda(6)',
      'nth.carmichael-lambda(12)',
      'nth.carmichael-lambda(30)',
    ],
  },
  'nth.cartesian-product': {
    title: 'nth.cartesian-product',
    category: 'Number Theory',
    description: 'Calculates the Cartesian product of two or more sets.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      sets: {
        type: 'array',
        array: true,
        description: 'The input collections to calculate the Cartesian product from.',
      },
      ...getOperatorArgs('array', 'array'),
    },
    variants: [
      { argumentNames: ['sets'] },
    ],
    examples: [
      'nth.cartesian-product([1, 2], ["a", "b"])',
      'nth.cartesian-product([1, 2], ["a", "b"], [true, false])',
      'nth.cartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'nth.perfect-power': {
    title: 'nth.perfect-power',
    category: 'Number Theory',
    description: 'Returns a tuple of the base and exponent if the number is a perfect power, otherwise returns null.',
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
      'nth.perfect-power(1)',
      'nth.perfect-power(2)',
      'nth.perfect-power(4)',
      'nth.perfect-power(8)',
      'nth.perfect-power(9)',
      'nth.perfect-power(16)',
      'nth.perfect-power(19)',
    ],
  },
  'nth.mod-exp': {
    title: 'nth.mod-exp',
    category: 'Number Theory',
    description: 'Calculates the modular exponentiation of a base raised to an exponent modulo a modulus.',
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
      'nth.mod-exp(2, 3, 5)',
      'nth.mod-exp(3, 4, 7)',
      'nth.mod-exp(5, 6, 11)',
      'nth.mod-exp(7, 8, 13)',
    ],
  },
  'nth.mod-inv': {
    title: 'nth.mod-inv',
    category: 'Number Theory',
    description: 'Calculates the modular multiplicative inverse of a number modulo another number.',
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
      'nth.mod-inv(3, 11)',
      'nth.mod-inv(10, 17)',
      'nth.mod-inv(5, 13)',
      'nth.mod-inv(7, 19)',
    ],
  },
  'nth.extended-gcd': {
    title: 'nth.extended-gcd',
    category: 'Number Theory',
    description: 'Calculates the extended greatest common divisor (GCD) of two numbers, returning the GCD and the coefficients of Bézout\'s identity.',
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
      'nth.extended-gcd(30, 12)',
      'nth.extended-gcd(56, 98)',
      'nth.extended-gcd(101, 10)',
      'nth.extended-gcd(17, 13)',
    ],
  },
  'nth.chinese-remainder': {
    title: 'nth.chinese-remainder',
    category: 'Number Theory',
    description: 'Solves a system of simultaneous congruences using the Chinese Remainder Theorem.',
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
      ...getOperatorArgs('array', 'array'),
    },
    variants: [
      { argumentNames: ['remainders', 'moduli'] },
    ],
    examples: [
      'nth.chinese-remainder([2, 3], [3, 5])',
      'nth.chinese-remainder([1, 2], [3, 4])',
      'nth.chinese-remainder([0, 1], [2, 3])',
      'nth.chinese-remainder([1, 2, 3], [4, 5, 7])',
    ],
  },
  'nth.stirling-first': {
    title: 'nth.stirling-first',
    category: 'Number Theory',
    description: 'Calculates the Stirling numbers of the first kind, which count the number of permutations of n elements with k cycles.',
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
      'nth.stirling-first(5, 2)',
      'nth.stirling-first(4, 3)',
      'nth.stirling-first(6, 1)',
      'nth.stirling-first(7, 4)',
      'nth.stirling-first(8, 5)',
    ],
  },
  'nth.stirling-second': {
    title: 'nth.stirling-second',
    category: 'Number Theory',
    description: 'Calculates the Stirling numbers of the second kind, which count the number of ways to partition n elements into k non-empty subsets.',
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
      'nth.stirling-second(5, 2)',
      'nth.stirling-second(4, 3)',
      'nth.stirling-second(6, 1)',
      'nth.stirling-second(7, 4)',
      'nth.stirling-second(8, 5)',
    ],
  },
}
