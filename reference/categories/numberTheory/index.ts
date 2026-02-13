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

type SeqKey<T extends string> = `TEMP-nth.${T}-seq`
type TakeWhileKey<T extends string> = `TEMP-nth.${T}-take-while`
type NthKey<T extends string> = `TEMP-nth.${T}-nth`
type PredKey<T extends string> = `TEMP-nth.${T}?`

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
  'TEMP-nth.count-combinations': {
    title: 'TEMP-nth.count-combinations',
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
      'let { count-combinations } = import("TEMP-nth");\ncount-combinations(5, 3)',
      'let { binomial } = import("TEMP-nth");\nbinomial(10, 2)',
    ],
    aliases: ['TEMP-nth.binomial'],
  },
  'TEMP-nth.combinations': {
    title: 'TEMP-nth.combinations',
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
      'let { combinations } = import("TEMP-nth");\ncombinations([1, 2, 3], 2)',
      'let { combinations } = import("TEMP-nth");\ncombinations(["a", "b", "c"], 2)',
      'let { combinations } = import("TEMP-nth");\ncombinations([1, 2, 3], 0)',
      'let { combinations } = import("TEMP-nth");\ncombinations([1, 2, 3], 1)',
      'let { combinations } = import("TEMP-nth");\ncombinations([1, 2, 3], 3)',
    ],
  },
  'TEMP-nth.count-derangements': {
    title: 'TEMP-nth.count-derangements',
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
      'let { count-derangements } = import("TEMP-nth");\ncount-derangements(4)',
      'let { count-derangements } = import("TEMP-nth");\ncount-derangements(5)',
    ],
  },
  'TEMP-nth.derangements': {
    title: 'TEMP-nth.derangements',
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
      'let { derangements } = import("TEMP-nth");\nderangements([1, 2, 3, 4])',
      'let { derangements } = import("TEMP-nth");\nderangements(["a", "b", "c"])',
    ],
  },
  'TEMP-nth.divisors': {
    title: 'TEMP-nth.divisors',
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
      'let { divisors } = import("TEMP-nth");\ndivisors(12)',
      'let { divisors } = import("TEMP-nth");\ndivisors(100)',
      'let { divisors } = import("TEMP-nth");\ndivisors(37)',
    ],
  },
  'TEMP-nth.count-divisors': {
    title: 'TEMP-nth.count-divisors',
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
      'let { count-divisors } = import("TEMP-nth");\ncount-divisors(12)',
      'let { count-divisors } = import("TEMP-nth");\ncount-divisors(100)',
      'let { count-divisors } = import("TEMP-nth");\ncount-divisors(37)',
    ],
  },
  'TEMP-nth.proper-divisors': {
    title: 'TEMP-nth.proper-divisors',
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
      'let { proper-divisors } = import("TEMP-nth");\nproper-divisors(12)',
      'let { proper-divisors } = import("TEMP-nth");\nproper-divisors(100)',
      'let { proper-divisors } = import("TEMP-nth");\nproper-divisors(37)',
    ],
  },
  'TEMP-nth.count-proper-divisors': {
    title: 'TEMP-nth.count-proper-divisors',
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
      'let { count-proper-divisors } = import("TEMP-nth");\ncount-proper-divisors(12)',
      'let { count-proper-divisors } = import("TEMP-nth");\ncount-proper-divisors(100)',
      'let { count-proper-divisors } = import("TEMP-nth");\ncount-proper-divisors(37)',
    ],
  },
  'TEMP-nth.factorial': {
    title: 'TEMP-nth.factorial',
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
      'let { factorial } = import("TEMP-nth");\nfactorial(5)',
      'let { factorial } = import("TEMP-nth");\nfactorial(0)',
      'let { factorial } = import("TEMP-nth");\nfactorial(10)',
      'let { factorial } = import("TEMP-nth");\nfactorial(20)',
    ],
    aliases: ['TEMP-nth.!'],
  },
  'TEMP-nth.partitions': {
    title: 'TEMP-nth.partitions',
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
      'let { partitions } = import("TEMP-nth");\npartitions(4)',
      'let { partitions } = import("TEMP-nth");\npartitions(8)',
    ],
  },
  'TEMP-nth.count-partitions': {
    title: 'TEMP-nth.count-partitions',
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
      'let { count-partitions } = import("TEMP-nth");\ncount-partitions(4)',
      'let { count-partitions } = import("TEMP-nth");\ncount-partitions(8)',
      'let { count-partitions } = import("TEMP-nth");\ncount-partitions(15)',
    ],
  },
  'TEMP-nth.permutations': {
    title: 'TEMP-nth.permutations',
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
      'let { permutations } = import("TEMP-nth");\npermutations([1, 2, 3])',
      'let { permutations } = import("TEMP-nth");\npermutations(["a", "b", "c"])',
      'let { permutations } = import("TEMP-nth");\npermutations([1, 2, 3, 4])',
      'let { permutations } = import("TEMP-nth");\npermutations([1, 2])',
      'let { permutations } = import("TEMP-nth");\npermutations([1])',
      'let { permutations } = import("TEMP-nth");\npermutations([])',
    ],
  },
  'TEMP-nth.count-permutations': {
    title: 'TEMP-nth.count-permutations',
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
      'let { count-permutations } = import("TEMP-nth");\ncount-permutations(5, 3)',
      'let { count-permutations } = import("TEMP-nth");\ncount-permutations(10, 2)',
      'let { count-permutations } = import("TEMP-nth");\ncount-permutations(10, 10)',
      'let { count-permutations } = import("TEMP-nth");\ncount-permutations(10, 0)',
      'let { count-permutations } = import("TEMP-nth");\ncount-permutations(10, 1)',
    ],
  },
  'TEMP-nth.power-set': {
    title: 'TEMP-nth.power-set',
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
      'let { power-set } = import("TEMP-nth");\npower-set(["a", "b", "c"])',
      'let { power-set } = import("TEMP-nth");\npower-set([1, 2])',
      'let { power-set } = import("TEMP-nth");\npower-set([1])',
      'let { power-set } = import("TEMP-nth");\npower-set([])',
    ],
  },
  'TEMP-nth.count-power-set': {
    title: 'TEMP-nth.count-power-set',
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
      'let { count-power-set } = import("TEMP-nth");\ncount-power-set(3)',
      'let { count-power-set } = import("TEMP-nth");\ncount-power-set(5)',
      'let { count-power-set } = import("TEMP-nth");\ncount-power-set(10)',
    ],
  },
  'TEMP-nth.prime-factors': {
    title: 'TEMP-nth.prime-factors',
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
      'let { prime-factors } = import("TEMP-nth");\nprime-factors(12)',
      'let { prime-factors } = import("TEMP-nth");\nprime-factors(100)',
      'let { prime-factors } = import("TEMP-nth");\nprime-factors(37)',
    ],
  },
  'TEMP-nth.count-prime-factors': {
    title: 'TEMP-nth.count-prime-factors',
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
      'let { count-prime-factors } = import("TEMP-nth");\ncount-prime-factors(12)',
      'let { count-prime-factors } = import("TEMP-nth");\ncount-prime-factors(100)',
      'let { count-prime-factors } = import("TEMP-nth");\ncount-prime-factors(37)',
    ],
  },
  'TEMP-nth.distinct-prime-factors': {
    title: 'TEMP-nth.distinct-prime-factors',
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
      'let { distinct-prime-factors } = import("TEMP-nth");\ndistinct-prime-factors(12)',
      'let { distinct-prime-factors } = import("TEMP-nth");\ndistinct-prime-factors(100)',
      'let { distinct-prime-factors } = import("TEMP-nth");\ndistinct-prime-factors(37)',
    ],
  },
  'TEMP-nth.count-distinct-prime-factors': {
    title: 'TEMP-nth.count-distinct-prime-factors',
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
      'let { count-distinct-prime-factors } = import("TEMP-nth");\ncount-distinct-prime-factors(12)',
      'let { count-distinct-prime-factors } = import("TEMP-nth");\ncount-distinct-prime-factors(100)',
      'let { count-distinct-prime-factors } = import("TEMP-nth");\ncount-distinct-prime-factors(37)',
    ],
  },
  'TEMP-nth.coprime?': {
    title: 'TEMP-nth.coprime?',
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
      'let { coprime? } = import("TEMP-nth");\ncoprime?(12, 8)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(12, 5)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(37, 1)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(0, 0)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(0, 5)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(5, 0)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(1, 0)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(0, 1)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(1, 1)',
      'let { coprime? } = import("TEMP-nth");\ncoprime?(2, 3)',
    ],
  },
  'TEMP-nth.divisible-by?': {
    title: 'TEMP-nth.divisible-by?',
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
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(12, 4)',
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(12, 5)',
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(37, 1)',
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(0, 0)',
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(0, 5)',
      'let { divisible-by? } = import("TEMP-nth");\ndivisible-by?(5, 0)',
    ],
  },
  'TEMP-nth.gcd': {
    title: 'TEMP-nth.gcd',
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
      'let { gcd } = import("TEMP-nth");\ngcd(100, 25)',
      'let { gcd } = import("TEMP-nth");\ngcd(37, 1)',
      'let { gcd } = import("TEMP-nth");\ngcd(0, 0)',
      'let { gcd } = import("TEMP-nth");\ngcd(0, 5)',
      'let { gcd } = import("TEMP-nth");\ngcd(5, 0)',
    ],
  },
  'TEMP-nth.lcm': {
    title: 'TEMP-nth.lcm',
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
      'let { lcm } = import("TEMP-nth");\nlcm(100, 25)',
      'let { lcm } = import("TEMP-nth");\nlcm(37, 1)',
      'let { lcm } = import("TEMP-nth");\nlcm(0, 5)',
      'let { lcm } = import("TEMP-nth");\nlcm(5, 0)',
    ],
  },
  'TEMP-nth.multinomial': {
    title: 'TEMP-nth.multinomial',
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
      'let { multinomial } = import("TEMP-nth");\nmultinomial(5, 2, 3)',
      'let { multinomial } = import("TEMP-nth");\nmultinomial(10, 2, 3, 5)',
    ],
    noOperatorDocumentation: true,
  },
  'TEMP-nth.amicable?': {
    title: 'TEMP-nth.amicable?',
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
      'let { amicable? } = import("TEMP-nth");\namicable?(220, 284)',
      'let { amicable? } = import("TEMP-nth");\namicable?(1184, 1210)',
      'let { amicable? } = import("TEMP-nth");\namicable?(2620, 2924)',
      'let { amicable? } = import("TEMP-nth");\namicable?(5020, 5564)',
      'let { amicable? } = import("TEMP-nth");\namicable?(6232, 6368)',
    ],
  },
  'TEMP-nth.euler-totient': {
    title: 'TEMP-nth.euler-totient',
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
      'let { euler-totient } = import("TEMP-nth");\neuler-totient(1)',
      'let { euler-totient } = import("TEMP-nth");\neuler-totient(2)',
      'let { euler-totient } = import("TEMP-nth");\neuler-totient(10)',
      'let { euler-totient } = import("TEMP-nth");\neuler-totient(20)',
    ],
  },
  'TEMP-nth.mobius': {
    title: 'TEMP-nth.mobius',
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
      'let { mobius } = import("TEMP-nth");\nmobius(1)',
      'let { mobius } = import("TEMP-nth");\nmobius(2)',
      'let { mobius } = import("TEMP-nth");\nmobius(3)',
      'let { mobius } = import("TEMP-nth");\nmobius(4)',
      'let { mobius } = import("TEMP-nth");\nmobius(6)',
      'let { mobius } = import("TEMP-nth");\nmobius(12)',
      'let { mobius } = import("TEMP-nth");\nmobius(30)',
    ],
    aliases: ['TEMP-nth.möbius'],
  },
  'TEMP-nth.mertens': {
    title: 'TEMP-nth.mertens',
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
      'let { mobius } = import("TEMP-nth");\nmobius(1)',
      'let { mobius } = import("TEMP-nth");\nmobius(2)',
      'let { mobius } = import("TEMP-nth");\nmobius(3)',
      'let { mobius } = import("TEMP-nth");\nmobius(4)',
      'let { mobius } = import("TEMP-nth");\nmobius(6)',
      'let { mobius } = import("TEMP-nth");\nmobius(12)',
      'let { mobius } = import("TEMP-nth");\nmobius(30)',
    ],
  },
  'TEMP-nth.sigma': {
    title: 'TEMP-nth.sigma',
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
      'let { sigma } = import("TEMP-nth");\nsigma(1)',
      'let { sigma } = import("TEMP-nth");\nsigma(2)',
      'let { sigma } = import("TEMP-nth");\nsigma(3)',
      'let { sigma } = import("TEMP-nth");\nsigma(4)',
      'let { sigma } = import("TEMP-nth");\nsigma(6)',
      'let { sigma } = import("TEMP-nth");\nsigma(12)',
      'let { sigma } = import("TEMP-nth");\nsigma(30)',
    ],
  },
  'TEMP-nth.carmichael-lambda': {
    title: 'TEMP-nth.carmichael-lambda',
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
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(1)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(2)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(3)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(4)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(6)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(12)',
      'let { carmichael-lambda } = import("TEMP-nth");\ncarmichael-lambda(30)',
    ],
  },
  'TEMP-nth.cartesian-product': {
    title: 'TEMP-nth.cartesian-product',
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
      'let { cartesian-product } = import("TEMP-nth");\ncartesian-product([1, 2], ["a", "b"])',
      'let { cartesian-product } = import("TEMP-nth");\ncartesian-product([1, 2], ["a", "b"], [true, false])',
      'let { cartesian-product } = import("TEMP-nth");\ncartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'TEMP-nth.perfect-power': {
    title: 'TEMP-nth.perfect-power',
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
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(1)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(2)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(4)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(8)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(9)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(16)',
      'let { perfect-power } = import("TEMP-nth");\nperfect-power(19)',
    ],
  },
  'TEMP-nth.mod-exp': {
    title: 'TEMP-nth.mod-exp',
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
      'let { mod-exp } = import("TEMP-nth");\nmod-exp(2, 3, 5)',
      'let { mod-exp } = import("TEMP-nth");\nmod-exp(3, 4, 7)',
      'let { mod-exp } = import("TEMP-nth");\nmod-exp(5, 6, 11)',
      'let { mod-exp } = import("TEMP-nth");\nmod-exp(7, 8, 13)',
    ],
  },
  'TEMP-nth.mod-inv': {
    title: 'TEMP-nth.mod-inv',
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
      'let { mod-inv } = import("TEMP-nth");\nmod-inv(3, 11)',
      'let { mod-inv } = import("TEMP-nth");\nmod-inv(10, 17)',
      'let { mod-inv } = import("TEMP-nth");\nmod-inv(5, 13)',
      'let { mod-inv } = import("TEMP-nth");\nmod-inv(7, 19)',
    ],
  },
  'TEMP-nth.extended-gcd': {
    title: 'TEMP-nth.extended-gcd',
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
      'let { extended-gcd } = import("TEMP-nth");\nextended-gcd(30, 12)',
      'let { extended-gcd } = import("TEMP-nth");\nextended-gcd(56, 98)',
      'let { extended-gcd } = import("TEMP-nth");\nextended-gcd(101, 10)',
      'let { extended-gcd } = import("TEMP-nth");\nextended-gcd(17, 13)',
    ],
  },
  'TEMP-nth.chinese-remainder': {
    title: 'TEMP-nth.chinese-remainder',
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
      'let { chinese-remainder } = import("TEMP-nth");\nchinese-remainder([2, 3], [3, 5])',
      'let { chinese-remainder } = import("TEMP-nth");\nchinese-remainder([1, 2], [3, 4])',
      'let { chinese-remainder } = import("TEMP-nth");\nchinese-remainder([0, 1], [2, 3])',
      'let { chinese-remainder } = import("TEMP-nth");\nchinese-remainder([1, 2, 3], [4, 5, 7])',
    ],
  },
  'TEMP-nth.stirling-first': {
    title: 'TEMP-nth.stirling-first',
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
      'let { stirling-first } = import("TEMP-nth");\nstirling-first(5, 2)',
      'let { stirling-first } = import("TEMP-nth");\nstirling-first(4, 3)',
      'let { stirling-first } = import("TEMP-nth");\nstirling-first(6, 1)',
      'let { stirling-first } = import("TEMP-nth");\nstirling-first(7, 4)',
      'let { stirling-first } = import("TEMP-nth");\nstirling-first(8, 5)',
    ],
  },
  'TEMP-nth.stirling-second': {
    title: 'TEMP-nth.stirling-second',
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
      'let { stirling-second } = import("TEMP-nth");\nstirling-second(5, 2)',
      'let { stirling-second } = import("TEMP-nth");\nstirling-second(4, 3)',
      'let { stirling-second } = import("TEMP-nth");\nstirling-second(6, 1)',
      'let { stirling-second } = import("TEMP-nth");\nstirling-second(7, 4)',
      'let { stirling-second } = import("TEMP-nth");\nstirling-second(8, 5)',
    ],
  },
}
