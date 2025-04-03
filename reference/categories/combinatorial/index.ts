import type { FunctionReference } from '../..'
import type { CombinatorialApiName } from '../../api'
import { arithmeticReference } from './arithmetic'
import { bellReference } from './bell'
import { catalanReference } from './catalan'
import { factorialReference } from './factorial'
import { fibonacciReference } from './fibonacci'
import { geometricReference } from './geometric'
import { happyReference } from './happy'
import { lookAndSayReference } from './lookAndSay'
import { lucasReference } from './lucas'
import { luckyReference } from './lucky'
import { mersenneReference } from './mersenne'
import { padovanReference } from './padovan'
import { partitionReference } from './partition'
import { pellReference } from './pell'
import { perfectReference } from './perfect'
import { polygonalReference } from './polygonal'
import { primeReference } from './prime'
import { recamanReference } from './recaman'
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
  ...arithmeticReference,
  ...bellReference,
  ...catalanReference,
  ...factorialReference,
  ...fibonacciReference,
  ...geometricReference,
  ...happyReference,
  ...lookAndSayReference,
  ...lucasReference,
  ...luckyReference,
  ...mersenneReference,
  ...padovanReference,
  ...partitionReference,
  ...pellReference,
  ...perfectReference,
  ...polygonalReference,
  ...primeReference,
  ...recamanReference,
  ...thueMorseReference,
  ...tribonacciReference,
}
