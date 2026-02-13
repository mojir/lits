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

type SeqKey<T extends string> = `Number-Theory.${T}-seq`
type TakeWhileKey<T extends string> = `Number-Theory.${T}-take-while`
type NthKey<T extends string> = `Number-Theory.${T}-nth`
type PredKey<T extends string> = `Number-Theory.${T}?`

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
  'Number-Theory.count-combinations': {
    title: 'Number-Theory.count-combinations',
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
      'let { count-combinations } = import("Number-Theory");\ncount-combinations(5, 3)',
      'let { count-combinations } = import("Number-Theory");\ncount-combinations(10, 2)',
    ],
  },
  'Number-Theory.combinations': {
    title: 'Number-Theory.combinations',
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
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 2)',
      'let { combinations } = import("Number-Theory");\ncombinations(["a", "b", "c"], 2)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 0)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 1)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 3)',
    ],
  },
  'Number-Theory.count-derangements': {
    title: 'Number-Theory.count-derangements',
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
      'let { count-derangements } = import("Number-Theory");\ncount-derangements(4)',
      'let { count-derangements } = import("Number-Theory");\ncount-derangements(5)',
    ],
  },
  'Number-Theory.derangements': {
    title: 'Number-Theory.derangements',
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
      'let { derangements } = import("Number-Theory");\nderangements([1, 2, 3, 4])',
      'let { derangements } = import("Number-Theory");\nderangements(["a", "b", "c"])',
    ],
  },
  'Number-Theory.divisors': {
    title: 'Number-Theory.divisors',
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
      'let { divisors } = import("Number-Theory");\ndivisors(12)',
      'let { divisors } = import("Number-Theory");\ndivisors(100)',
      'let { divisors } = import("Number-Theory");\ndivisors(37)',
    ],
  },
  'Number-Theory.count-divisors': {
    title: 'Number-Theory.count-divisors',
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
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(12)',
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(100)',
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(37)',
    ],
  },
  'Number-Theory.proper-divisors': {
    title: 'Number-Theory.proper-divisors',
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
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(12)',
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(100)',
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(37)',
    ],
  },
  'Number-Theory.count-proper-divisors': {
    title: 'Number-Theory.count-proper-divisors',
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
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(12)',
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(100)',
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(37)',
    ],
  },
  'Number-Theory.factorial': {
    title: 'Number-Theory.factorial',
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
      'let { factorial } = import("Number-Theory");\nfactorial(5)',
      'let { factorial } = import("Number-Theory");\nfactorial(0)',
      'let { factorial } = import("Number-Theory");\nfactorial(10)',
      'let { factorial } = import("Number-Theory");\nfactorial(20)',
    ],
  },
  'Number-Theory.partitions': {
    title: 'Number-Theory.partitions',
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
      'let { partitions } = import("Number-Theory");\npartitions(4)',
      'let { partitions } = import("Number-Theory");\npartitions(8)',
    ],
  },
  'Number-Theory.count-partitions': {
    title: 'Number-Theory.count-partitions',
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
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(4)',
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(8)',
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(15)',
    ],
  },
  'Number-Theory.permutations': {
    title: 'Number-Theory.permutations',
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
      'let { permutations } = import("Number-Theory");\npermutations([1, 2, 3])',
      'let { permutations } = import("Number-Theory");\npermutations(["a", "b", "c"])',
      'let { permutations } = import("Number-Theory");\npermutations([1, 2, 3, 4])',
      'let { permutations } = import("Number-Theory");\npermutations([1, 2])',
      'let { permutations } = import("Number-Theory");\npermutations([1])',
      'let { permutations } = import("Number-Theory");\npermutations([])',
    ],
  },
  'Number-Theory.count-permutations': {
    title: 'Number-Theory.count-permutations',
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
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(5, 3)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 2)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 10)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 0)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 1)',
    ],
  },
  'Number-Theory.power-set': {
    title: 'Number-Theory.power-set',
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
      'let { power-set } = import("Number-Theory");\npower-set(["a", "b", "c"])',
      'let { power-set } = import("Number-Theory");\npower-set([1, 2])',
      'let { power-set } = import("Number-Theory");\npower-set([1])',
      'let { power-set } = import("Number-Theory");\npower-set([])',
    ],
  },
  'Number-Theory.count-power-set': {
    title: 'Number-Theory.count-power-set',
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
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(3)',
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(5)',
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(10)',
    ],
  },
  'Number-Theory.prime-factors': {
    title: 'Number-Theory.prime-factors',
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
      'let { prime-factors } = import("Number-Theory");\nprime-factors(12)',
      'let { prime-factors } = import("Number-Theory");\nprime-factors(100)',
      'let { prime-factors } = import("Number-Theory");\nprime-factors(37)',
    ],
  },
  'Number-Theory.count-prime-factors': {
    title: 'Number-Theory.count-prime-factors',
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
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(12)',
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(100)',
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(37)',
    ],
  },
  'Number-Theory.distinct-prime-factors': {
    title: 'Number-Theory.distinct-prime-factors',
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
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(12)',
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(100)',
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(37)',
    ],
  },
  'Number-Theory.count-distinct-prime-factors': {
    title: 'Number-Theory.count-distinct-prime-factors',
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
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(12)',
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(100)',
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(37)',
    ],
  },
  'Number-Theory.coprime?': {
    title: 'Number-Theory.coprime?',
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
      'let { coprime? } = import("Number-Theory");\ncoprime?(12, 8)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(12, 5)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(37, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 5)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(5, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(1, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(1, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(2, 3)',
    ],
  },
  'Number-Theory.divisible-by?': {
    title: 'Number-Theory.divisible-by?',
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
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(12, 4)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(12, 5)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(37, 1)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(0, 0)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(0, 5)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(5, 0)',
    ],
  },
  'Number-Theory.gcd': {
    title: 'Number-Theory.gcd',
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
      'let { gcd } = import("Number-Theory");\ngcd(100, 25)',
      'let { gcd } = import("Number-Theory");\ngcd(37, 1)',
      'let { gcd } = import("Number-Theory");\ngcd(0, 0)',
      'let { gcd } = import("Number-Theory");\ngcd(0, 5)',
      'let { gcd } = import("Number-Theory");\ngcd(5, 0)',
    ],
  },
  'Number-Theory.lcm': {
    title: 'Number-Theory.lcm',
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
      'let { lcm } = import("Number-Theory");\nlcm(100, 25)',
      'let { lcm } = import("Number-Theory");\nlcm(37, 1)',
      'let { lcm } = import("Number-Theory");\nlcm(0, 5)',
      'let { lcm } = import("Number-Theory");\nlcm(5, 0)',
    ],
  },
  'Number-Theory.multinomial': {
    title: 'Number-Theory.multinomial',
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
      'let { multinomial } = import("Number-Theory");\nmultinomial(5, 2, 3)',
      'let { multinomial } = import("Number-Theory");\nmultinomial(10, 2, 3, 5)',
    ],
    noOperatorDocumentation: true,
  },
  'Number-Theory.amicable?': {
    title: 'Number-Theory.amicable?',
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
      'let { amicable? } = import("Number-Theory");\namicable?(220, 284)',
      'let { amicable? } = import("Number-Theory");\namicable?(1184, 1210)',
      'let { amicable? } = import("Number-Theory");\namicable?(2620, 2924)',
      'let { amicable? } = import("Number-Theory");\namicable?(5020, 5564)',
      'let { amicable? } = import("Number-Theory");\namicable?(6232, 6368)',
    ],
  },
  'Number-Theory.euler-totient': {
    title: 'Number-Theory.euler-totient',
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
      'let { euler-totient } = import("Number-Theory");\neuler-totient(1)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(2)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(10)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(20)',
    ],
  },
  'Number-Theory.mobius': {
    title: 'Number-Theory.mobius',
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
      'let { mobius } = import("Number-Theory");\nmobius(1)',
      'let { mobius } = import("Number-Theory");\nmobius(2)',
      'let { mobius } = import("Number-Theory");\nmobius(3)',
      'let { mobius } = import("Number-Theory");\nmobius(4)',
      'let { mobius } = import("Number-Theory");\nmobius(6)',
      'let { mobius } = import("Number-Theory");\nmobius(12)',
      'let { mobius } = import("Number-Theory");\nmobius(30)',
    ],
  },
  'Number-Theory.mertens': {
    title: 'Number-Theory.mertens',
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
      'let { mobius } = import("Number-Theory");\nmobius(1)',
      'let { mobius } = import("Number-Theory");\nmobius(2)',
      'let { mobius } = import("Number-Theory");\nmobius(3)',
      'let { mobius } = import("Number-Theory");\nmobius(4)',
      'let { mobius } = import("Number-Theory");\nmobius(6)',
      'let { mobius } = import("Number-Theory");\nmobius(12)',
      'let { mobius } = import("Number-Theory");\nmobius(30)',
    ],
  },
  'Number-Theory.sigma': {
    title: 'Number-Theory.sigma',
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
      'let { sigma } = import("Number-Theory");\nsigma(1)',
      'let { sigma } = import("Number-Theory");\nsigma(2)',
      'let { sigma } = import("Number-Theory");\nsigma(3)',
      'let { sigma } = import("Number-Theory");\nsigma(4)',
      'let { sigma } = import("Number-Theory");\nsigma(6)',
      'let { sigma } = import("Number-Theory");\nsigma(12)',
      'let { sigma } = import("Number-Theory");\nsigma(30)',
    ],
  },
  'Number-Theory.carmichael-lambda': {
    title: 'Number-Theory.carmichael-lambda',
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
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(1)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(2)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(3)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(4)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(6)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(12)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(30)',
    ],
  },
  'Number-Theory.cartesian-product': {
    title: 'Number-Theory.cartesian-product',
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
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2], ["a", "b"])',
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2], ["a", "b"], [true, false])',
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'Number-Theory.perfect-power': {
    title: 'Number-Theory.perfect-power',
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
      'let { perfect-power } = import("Number-Theory");\nperfect-power(1)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(2)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(4)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(8)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(9)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(16)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(19)',
    ],
  },
  'Number-Theory.mod-exp': {
    title: 'Number-Theory.mod-exp',
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
      'let { mod-exp } = import("Number-Theory");\nmod-exp(2, 3, 5)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(3, 4, 7)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(5, 6, 11)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(7, 8, 13)',
    ],
  },
  'Number-Theory.mod-inv': {
    title: 'Number-Theory.mod-inv',
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
      'let { mod-inv } = import("Number-Theory");\nmod-inv(3, 11)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(10, 17)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(5, 13)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(7, 19)',
    ],
  },
  'Number-Theory.extended-gcd': {
    title: 'Number-Theory.extended-gcd',
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
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(30, 12)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(56, 98)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(101, 10)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(17, 13)',
    ],
  },
  'Number-Theory.chinese-remainder': {
    title: 'Number-Theory.chinese-remainder',
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
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([2, 3], [3, 5])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([1, 2], [3, 4])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([0, 1], [2, 3])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([1, 2, 3], [4, 5, 7])',
    ],
  },
  'Number-Theory.stirling-first': {
    title: 'Number-Theory.stirling-first',
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
      'let { stirling-first } = import("Number-Theory");\nstirling-first(5, 2)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(4, 3)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(6, 1)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(7, 4)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(8, 5)',
    ],
  },
  'Number-Theory.stirling-second': {
    title: 'Number-Theory.stirling-second',
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
      'let { stirling-second } = import("Number-Theory");\nstirling-second(5, 2)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(4, 3)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(6, 1)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(7, 4)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(8, 5)',
    ],
  },
}
