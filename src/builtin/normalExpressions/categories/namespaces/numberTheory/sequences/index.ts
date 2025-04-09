import { LitsError } from '../../../../../../errors'
import type { SourceCodeInfo } from '../../../../../../tokenizer/token'
import { assertFunctionLike } from '../../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../../typeGuards/number'
import { assertString } from '../../../../../../typeGuards/string'
import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../../../../../interface'
import { abundantSequence } from './abundant'
import { arithmeticNormalExpressions } from './arithmetic'
import { bellNumbers } from './bell'
import { bernoulliNormalExpressions } from './bernoulli'
import { catalanNumbers } from './catalan'
import { collatzSequence } from './collatz'
import { compositeSequence } from './composite'
import { deficientSequence } from './deficient'
import { factorialNumbers } from './factorial'
import { fibonacciNumbers } from './fibonacci'
import { geometricNormalExpressions } from './geometric'
import { golombSequence } from './golomb'
import { happySequence } from './happy'
import { jugglerSequence } from './juggler'
import { lookAndSaySequence } from './lookAndSay'
import { lucasNumbers } from './lucas'
import { luckySequence } from './lucky'
import { mersenneNumbers } from './mersenne'
import { padovanSequence } from './padovan'
import { partitionNumbers } from './partition'
import { pellNumbers } from './pell'
import { perfectNumbers } from './perfect'
import { perfectCubeSequence } from './perfectCube'
import { perfectPowerSequence } from './perfectPower'
import { perfectSquareSequence } from './perfectSquare'
import { poligonalNormalExpressions } from './poligonal'
import { primeSequence } from './prime'
import { recamanSequence } from './recaman'
import { sylvesterNumbers } from './sylvester'
import { thueMorseSequence } from './thueMorse'
import { tribonacciNumbers } from './tribonacci'

type SeqKey<T extends string> = `nth:${T}-seq`
type TakeWhileKey<T extends string> = `nth:${T}-take-while`
type NthKey<T extends string> = `nth:${T}-nth`
type PredKey<T extends string> = `nth:${T}?`

type SeqFunction<Type extends number | string> = (length: number, sourceCodeInfo: SourceCodeInfo | undefined) => Type[]
type TakeWhileFunction<Type extends number | string> = (pred: (value: Type, index: number) => boolean, sourceCodeInfo: SourceCodeInfo | undefined) => Type[]
type PredFunction<Type extends number | string> = (n: Type, sourceCodeInfo: SourceCodeInfo | undefined) => boolean

export type SequenceKeys<T extends string> = SeqKey<T> | TakeWhileKey<T> | NthKey<T> | PredKey<T>

export type SequenceDefinition<T extends string, Type extends number | string = number> = {
  [key in Exclude<SequenceKeys<T>, NthKey<T>>]: key extends SeqKey<T>
    ? SeqFunction<Type>
    : key extends TakeWhileKey<T>
      ? TakeWhileFunction<Type>
      : PredFunction<Type>
} & {
  maxLength?: number
} & (Type extends string ? {
  string: true
} : {
  string?: never
}) & {
  noNth?: true
}

export type SequenceNormalExpressions<T extends string, Type extends string | number = number> = {
  [key in SequenceKeys<T>]: key extends SeqKey<T>
    ? BuiltinNormalExpression<Type[]>
    : key extends TakeWhileKey<T>
      ? BuiltinNormalExpression<Type[]>
      : key extends NthKey<T>
        ? BuiltinNormalExpression<Type>
        : BuiltinNormalExpression<boolean>
}

export const sequenceNormalExpressions: BuiltinNormalExpressions = {}

addSequence(abundantSequence)
addSequence(collatzSequence)
addSequence(compositeSequence)
addSequence(deficientSequence)
addSequence(golombSequence)
addSequence(happySequence)
addSequence(jugglerSequence)
addSequence(lookAndSaySequence)
addSequence(luckySequence)
addSequence(padovanSequence)
addSequence(perfectSquareSequence)
addSequence(perfectCubeSequence)
addSequence(perfectPowerSequence)
addSequence(primeSequence)
addSequence(recamanSequence)
addSequence(thueMorseSequence)
addNormalExpressions(getFiniteNumberSequence('tribonacci', tribonacciNumbers))
addNormalExpressions(getFiniteNumberSequence('catalan', catalanNumbers))
addNormalExpressions(getFiniteNumberSequence('factorial', factorialNumbers))
addNormalExpressions(getFiniteNumberSequence('fibonacci', fibonacciNumbers))
addNormalExpressions(getFiniteNumberSequence('lucas', lucasNumbers))
addNormalExpressions(getFiniteNumberSequence('mersenne', mersenneNumbers))
addNormalExpressions(getFiniteNumberSequence('partition', partitionNumbers))
addNormalExpressions(getFiniteNumberSequence('pell', pellNumbers))
addNormalExpressions(getFiniteNumberSequence('perfect', perfectNumbers))
addNormalExpressions(getFiniteNumberSequence('sylvester', sylvesterNumbers))
addNormalExpressions(getFiniteNumberSequence('bell', bellNumbers))
addNormalExpressions(arithmeticNormalExpressions)
addNormalExpressions(bernoulliNormalExpressions)
addNormalExpressions(geometricNormalExpressions)
addNormalExpressions(poligonalNormalExpressions)

function addNormalExpressions(normalExpressions: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(normalExpressions)) {
    /* v8 ignore next 3 */
    if (sequenceNormalExpressions[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    sequenceNormalExpressions[key] = value
  }
}

function getFiniteNumberSequence<T extends string>(name: T, sequence: number[]): SequenceNormalExpressions<T> {
  return {
    [`nth:${name}-seq`]: createSeqNormalExpression(length => sequence.slice(0, length), sequence.length),
    [`nth:${name}-take-while`]: createTakeWhileNormalExpression((takeWhile) => {
      let i = 0
      for (i = 0; ; i += 1) {
        if (i >= sequence.length) {
          break
        }
        if (!takeWhile(sequence[i]!, i)) {
          break
        }
      }
      return sequence.slice(0, i)
    }, sequence.length),
    [`nth:${name}-nth`]: createNthNormalExpression(() => sequence, sequence.length),
    [`nth:${name}?`]: createNumberPredNormalExpression(n => sequence.includes(n)),
  } as unknown as SequenceNormalExpressions<T>
}

function addSequence<Type extends number | string>(sequence: SequenceDefinition<string, Type>) {
  for (const [key, value] of Object.entries(sequence)) {
    /* v8 ignore next 3 */
    if (sequenceNormalExpressions[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    if (key.endsWith('seq')) {
      sequenceNormalExpressions[key] = createSeqNormalExpression(value as SeqFunction<Type>, sequence.maxLength)
      if (!sequence.noNth) {
        sequenceNormalExpressions[key.replace(/seq$/, 'nth')] = createNthNormalExpression(value as SeqFunction<Type>, sequence.maxLength)
      }
    }
    else if (key.endsWith('take-while')) {
      sequenceNormalExpressions[key] = createTakeWhileNormalExpression(value as TakeWhileFunction<Type>, sequence.maxLength)
    }
    else if (key.endsWith('?')) {
      if (sequence.string) {
        sequenceNormalExpressions[key] = createStringPredNormalExpression(value as PredFunction<string>)
      }
      else {
        sequenceNormalExpressions[key] = createNumberPredNormalExpression(value as PredFunction<number>)
      }
    }
  }
}

function createSeqNormalExpression<Type extends number | string>(
  seqFunction: SeqFunction<Type>,
  maxLength: number | undefined,
): BuiltinNormalExpression<Type[]> {
  return {
    evaluate: (params, sourceCodeInfo) => {
      const length = params[0] ?? maxLength
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true, lte: maxLength })
      if (typeof length !== 'number') {
        throw new LitsError('Length must be a number', sourceCodeInfo)
      }
      const result = seqFunction(length, sourceCodeInfo)
      if (typeof result[0] === 'number') {
        if (result.some(n => (n as number) > Number.MAX_SAFE_INTEGER)) {
          throw new LitsError('Result exceeds maximum safe integer', sourceCodeInfo)
        }
      }
      return result
    },
    paramCount: typeof maxLength === 'number' ? { max: 1 } : 1,
  }
}

function createTakeWhileNormalExpression<Type extends number | string>(
  takeWhileFunction: TakeWhileFunction<Type>,
  maxLength: number | undefined,
): BuiltinNormalExpression<Type[]> {
  return {
    evaluate: (params, sourceCodeInfo, contextStack, { executeFunction }) => {
      const fn = params[0]
      assertFunctionLike(fn, sourceCodeInfo)
      const result = takeWhileFunction((value, index) => !!executeFunction(fn, [value, index], contextStack), sourceCodeInfo)
      if (typeof result[0] === 'number') {
        if (result.some(n => (n as number) > Number.MAX_SAFE_INTEGER)) {
          throw new LitsError('Result exceeds maximum safe integer', sourceCodeInfo)
        }
      }
      return result
    },
    paramCount: typeof maxLength === 'number' ? { max: 1 } : 1,
  }
}

function createNthNormalExpression<Type extends number | string>(
  seqFunction: SeqFunction<Type>,
  maxLength: number | undefined,
): BuiltinNormalExpression<Type> {
  return {
    evaluate: (params, sourceCodeInfo) => {
      const n = params[0]
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: maxLength })
      const sequence = seqFunction(n, sourceCodeInfo)
      if (typeof sequence[0] === 'number') {
        if (sequence.some(val => (val as number) > Number.MAX_SAFE_INTEGER)) {
          throw new LitsError('Result exceeds maximum safe integer', sourceCodeInfo)
        }
      }
      return sequence[n - 1]!
    },
    paramCount: 1,
  }
}

function createNumberPredNormalExpression(
  predFunction: PredFunction<number>,
): BuiltinNormalExpression<boolean> {
  return {
    evaluate: (params, sourceCodeInfo) => {
      const value = params[0]
      assertNumber(value, sourceCodeInfo)
      return predFunction(value, sourceCodeInfo)
    },
    paramCount: 1,
  }
}

function createStringPredNormalExpression(
  predFunction: PredFunction<string>,
): BuiltinNormalExpression<boolean> {
  return {
    evaluate: (params, sourceCodeInfo) => {
      const value = params[0]
      assertString(value, sourceCodeInfo)
      return predFunction(value, sourceCodeInfo)
    },
    paramCount: 1,
  }
}
