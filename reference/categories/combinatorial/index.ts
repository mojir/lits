import type { FunctionReference } from '../..'
import type { CombinatorialApiName } from '../../api'
import { catalanReference } from './catalan'
import { lookAndSayReference } from './lookAndSay'
import { thueMorseReference } from './thueMorse'
import { recamanReference } from './recaman'
import { pellReference } from './pell'
import { bellReference } from './bell'
import { padovanReference } from './padovan'

type SeqKey<T extends string> = `c:${T}-seq`
type TakeWhileKey<T extends string> = `c:${T}-take-while`
type NthKey<T extends string> = `c:${T}-nth`
type PredKey<T extends string> = `c:${T}?`

type SequenceKeys<T extends string> = SeqKey<T> | TakeWhileKey<T> | NthKey<T> | PredKey<T>

export type CombinatorialSequenceReference<T extends string> = {
  [key in SequenceKeys<T>]: FunctionReference<'Combinatorial'>
}

export const combinatorialReference: Record<CombinatorialApiName, FunctionReference<'Combinatorial'>> = {
  ...bellReference,
  ...catalanReference,
  ...lookAndSayReference,
  ...padovanReference,
  ...pellReference,
  ...recamanReference,
  ...thueMorseReference,
}
