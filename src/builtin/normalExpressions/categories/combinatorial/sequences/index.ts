import type { SourceCodeInfo } from '../../../../../tokenizer/token'
import { assertLitsFunction } from '../../../../../typeGuards/litsFunction'
import { assertNumber } from '../../../../../typeGuards/number'
import { assertString } from '../../../../../typeGuards/string'
import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../../../../interface'
import { arithmeticNormalExpressions } from './arithmetic'
import { bellSequence } from './bell'
import { catalanSequence } from './catalan'
import { factorialSequence } from './factorial'
import { fibonacciSequence } from './fibonacci'
import { geometricNormalExpressions } from './geometric'
import { lookAndSaySequence } from './lookAndSay'
import { happySequence } from './happy'
import { lucasSequence } from './lucas'
import { mersenneSequence } from './mersenne'
import { padovanSequence } from './padovan'
import { partitionSequence } from './partition'
import { pellSequence } from './pell'
import { perfectSequence } from './perfect'
import { poligonalNormalExpressions } from './poligonal'
import { primeSequence } from './prime'
import { recamanSequence } from './recaman'
import { thueMorseSequence } from './thueMorse'
import { tribonacciSequence } from './tribonacci'
import { luckySequence } from './lucky'

type SeqKey<T extends string> = `c:${T}-seq`
type TakeWhileKey<T extends string> = `c:${T}-take-while`
type NthKey<T extends string> = `c:${T}-nth`
type PredKey<T extends string> = `c:${T}?`

type SeqFunction<Type extends number | string> = (length: number, sourceCodeInfo: SourceCodeInfo | undefined) => Type[]
type TakeWhileFunction<Type extends number | string> = (pred: (value: Type, index: number) => boolean, sourceCodeInfo: SourceCodeInfo | undefined) => Type[]
type NthFunction<Type extends number | string> = (n: number, sourceCodeInfo: SourceCodeInfo | undefined) => Type
type PredFunction<Type extends number | string> = (n: Type, sourceCodeInfo: SourceCodeInfo | undefined) => boolean

export type SequenceKeys<T extends string> = SeqKey<T> | TakeWhileKey<T> | NthKey<T> | PredKey<T>

export type SequenceDefinition<T extends string, Type extends number | string = number> = {
  [key in SequenceKeys<T>]: key extends SeqKey<T>
    ? SeqFunction<Type>
    : key extends TakeWhileKey<T>
      ? TakeWhileFunction<Type>
      : key extends NthKey<T>
        ? NthFunction<Type>
        : PredFunction<Type>
} & {
  maxLength?: number
} & (Type extends string ? {
  string: true
} : {
  string?: never
})

export type SequenceNormalExpressions<T extends string> = {
  [key in SequenceKeys<T>]: key extends SeqKey<T>
    ? BuiltinNormalExpression<number[]>
    : key extends TakeWhileKey<T>
      ? BuiltinNormalExpression<number[]>
      : key extends NthKey<T>
        ? BuiltinNormalExpression<number>
        : BuiltinNormalExpression<boolean>
}

export const sequenceNormalExpression: BuiltinNormalExpressions = {}

addSequence(bellSequence)
addSequence(catalanSequence)
addSequence(factorialSequence)
addSequence(fibonacciSequence)
addSequence(happySequence)
addSequence(lookAndSaySequence)
addSequence(lucasSequence)
addSequence(luckySequence)
addSequence(mersenneSequence)
addSequence(padovanSequence)
addSequence(partitionSequence)
addSequence(pellSequence)
addSequence(perfectSequence)
addSequence(primeSequence)
addSequence(recamanSequence)
addSequence(thueMorseSequence)
addSequence(tribonacciSequence)
addNormalExpressions(arithmeticNormalExpressions)
addNormalExpressions(geometricNormalExpressions)
addNormalExpressions(poligonalNormalExpressions)

function addNormalExpressions(normalExpressions: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(normalExpressions)) {
    if (sequenceNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    sequenceNormalExpression[key] = value
  }
}

function addSequence<Type extends number | string>(sequence: SequenceDefinition<string, Type>) {
  for (const [key, value] of Object.entries(sequence)) {
    if (sequenceNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    if (key.endsWith('seq')) {
      sequenceNormalExpression[key] = createSeqNormalExpression(value as SeqFunction<Type>, sequence.maxLength)
    }
    else if (key.endsWith('take-while')) {
      sequenceNormalExpression[key] = createTakeWhileNormalExpression(value as TakeWhileFunction<Type>, sequence.maxLength)
    }
    else if (key.endsWith('nth')) {
      sequenceNormalExpression[key] = createNthNormalExpression(value as NthFunction<Type>, sequence.maxLength)
    }
    else if (key.endsWith('?')) {
      if (sequence.string) {
        sequenceNormalExpression[key] = createStringPredNormalExpression(value as PredFunction<string>)
      }
      else {
        sequenceNormalExpression[key] = createNumberPredNormalExpression(value as PredFunction<number>)
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
      return seqFunction(length, sourceCodeInfo)
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
      assertLitsFunction(fn, sourceCodeInfo)
      return takeWhileFunction((value, index) => !!executeFunction(fn, [value, index], contextStack), sourceCodeInfo)
    },
    paramCount: typeof maxLength === 'number' ? { max: 1 } : 1,
  }
}

function createNthNormalExpression<Type extends number | string>(
  nthFunction: NthFunction<Type>,
  maxLength: number | undefined,
): BuiltinNormalExpression<Type> {
  return {
    evaluate: (params, sourceCodeInfo) => {
      const n = params[0]
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: maxLength })
      return nthFunction(n, sourceCodeInfo)
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
